use convert_case::{Case, Casing};
use proc_macro2::Ident;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse::Parse,
    punctuated::Punctuated,
    spanned::Spanned,
    token::{And, Comma},
    AngleBracketedGenericArguments, FnArg, ImplItem, ImplItemMethod, ItemImpl, Pat, PathArguments,
    ReturnType, Token, Type, TypePath,
};

#[derive(Debug, Clone)]
enum FFISafeType {
    Vec(AngleBracketedGenericArguments),
    // TODO: Slice
    String,
    Box(AngleBracketedGenericArguments),
    Opaque(Ident),
    // TODO: &str
    // TODO: Anything that isn't listed should be repr_c::Box
}

#[derive(Debug, Clone)]
struct FFIType {
    native_type: TypePath,
    ffi_safe_type: FFISafeType,
}

impl TryFrom<TypePath> for FFIType {
    type Error = syn::Error;

    fn try_from(path: TypePath) -> Result<Self, Self::Error> {
        let last_segment = path
            .path
            .segments
            .last()
            .ok_or_else(|| syn::Error::new(path.span(), "unexpected empty path"))?;

        let ffi_safe_type = match last_segment.ident.to_string().as_str() {
            "Vec" => {
                let PathArguments::AngleBracketed(ref args) = last_segment.arguments else {
                    return Err(syn::Error::new(last_segment.arguments.span(), "invalid path arguments"));
                };

                Ok::<_, syn::Error>(FFISafeType::Vec(args.clone()))
            }
            "Box" => {
                let PathArguments::AngleBracketed(ref args) = last_segment.arguments else {
                    return Err(syn::Error::new(last_segment.arguments.span(), "invalid path arguments"));
                };

                Ok(FFISafeType::Box(args.clone()))
            }
            "String" => Ok(FFISafeType::String),
            // Anything that isn't listed should be repr_c::Box
            _ => Ok(FFISafeType::Opaque(last_segment.ident.clone())),
        }?;

        Ok(FFIType {
            native_type: path,
            ffi_safe_type,
        })
    }
}

impl TryFrom<Box<Type>> for FFIType {
    type Error = syn::Error;

    fn try_from(ty: Box<Type>) -> Result<Self, Self::Error> {
        let Type::Path(path) = *ty else {
            return Err(syn::Error::new(ty.span(), "unexpected non-path value"));
        };

        FFIType::try_from(path)
    }
}

impl ToTokens for FFISafeType {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            FFISafeType::Vec(args) => quote! { safer_ffi::prelude::repr_c::Vec #args },
            FFISafeType::Box(args) => quote! { safer_ffi::prelude::repr_c::Box #args },
            FFISafeType::String => quote! { safer_ffi::prelude::repr_c::String },
            FFISafeType::Opaque(ident) => quote! { #ident },
        }
        .to_tokens(tokens)
    }
}

#[derive(Debug)]
struct ReturnFFIType(Option<FFIType>);

impl ToTokens for ReturnFFIType {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self.0.as_ref().map(|v| &v.ffi_safe_type) {
            Some(ffi_type) => quote! { -> #ffi_type },
            None => quote! {},
        }
        .to_tokens(tokens)
    }
}

#[derive(Debug, Clone)]
struct FFIArgument {
    name: Box<Pat>,
    ffi_type: FFIType,
    mutable: Option<Token![mut]>,
    reference: Option<Token![&]>,
    is_receiver: bool,
}

impl ToTokens for FFIArgument {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let name = &self.name;
        let ffi_type = &self.ffi_type.ffi_safe_type;
        let reference = &self.reference;
        let mutable = &self.mutable;

        quote! {
           #name: #reference #mutable #ffi_type
        }
        .to_tokens(tokens)
    }
}

#[derive(Debug)]
#[non_exhaustive]
struct FFIArgumentConversion {
    ffi_arg: FFIArgument,
}

impl FFIArgumentConversion {
    fn new(ffi_arg: FFIArgument) -> FFIArgumentConversion {
        FFIArgumentConversion { ffi_arg }
    }
}

impl ToTokens for FFIArgumentConversion {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let name = &self.ffi_arg.name;
        let orig = &self.ffi_arg.ffi_type.native_type;
        let reference = &self.ffi_arg.reference;
        let mutable = &self.ffi_arg.mutable;

        quote! {
            let #name: #reference #mutable #orig = #name.into();
        }
        .to_tokens(tokens)
    }
}

fn arg_to_ffi(module_name: &TypePath, arg: FnArg) -> syn::Result<FFIArgument> {
    match arg {
        FnArg::Receiver(rec) => {
            if rec.lifetime().is_some() {
                return Err(syn::Error::new(
                    rec.lifetime().span(),
                    "named lifetime's are not supported for receivers",
                ));
            }

            Ok(FFIArgument {
                name: Pat::Verbatim(quote! { this }.to_token_stream()).into(),
                ffi_type: FFIType::try_from(module_name.clone())?,
                reference: rec.reference.map(|_| And::default()),
                is_receiver: true,
                mutable: rec.mutability,
            })
        }

        FnArg::Typed(pat_type) => Ok(FFIArgument {
            name: pat_type.pat,
            ffi_type: FFIType::try_from(pat_type.ty.clone())?,
            reference: matches!(*pat_type.ty.clone(), Type::Reference(_)).then_some(And::default()),
            is_receiver: false,
            mutable: match *pat_type.ty {
                Type::Reference(reference) => reference.mutability,
                _ => None,
            },
        }),
    }
}

#[derive(Debug)]
struct FFIFunction {
    module_name: TypePath,
    function_name: Ident,
    parameters: Punctuated<FFIArgument, Comma>,
    output: ReturnFFIType,
}

impl FFIFunction {
    fn new(module_name: TypePath, method: ImplItemMethod) -> syn::Result<Self> {
        // TODO: Support async
        if method.sig.asyncness.is_some() {
            return Err(syn::Error::new(method.span(), "async is not supported"));
        }

        let parameters =
            method
                .sig
                .inputs
                .into_iter()
                .try_fold(Punctuated::new(), |mut inputs, arg| {
                    inputs.push(arg_to_ffi(&module_name, arg)?);
                    Ok::<_, syn::Error>(inputs)
                })?;

        let output = match method.sig.output {
            ReturnType::Default => ReturnFFIType(None),
            ReturnType::Type(_, otype) => ReturnFFIType(Some(FFIType::try_from(otype)?)),
        };

        Ok(Self {
            module_name,
            function_name: method.sig.ident,
            parameters,
            output,
        })
    }

    fn ffi_funcation_name(&self) -> Ident {
        format_ident!(
            "{}_{}",
            self.module_name
                .to_token_stream()
                .to_string()
                .to_case(Case::Snake),
            self.function_name
        )
    }
}

impl ToTokens for FFIFunction {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let ffi_function_name = self.ffi_funcation_name();
        let ffi_function_inputs = &self.parameters;
        let ffi_function_output = &self.output;
        let module_name = &self.module_name;
        let function_name = &self.function_name;

        let type_conversions = self
            .parameters
            .iter()
            .filter(|a| !a.is_receiver)
            .map(|arg| FFIArgumentConversion::new(arg.clone()))
            .collect::<Vec<_>>();

        let input_names: Punctuated<Box<Pat>, Comma> =
            Punctuated::from_iter(self.parameters.iter().map(|arg| arg.name.clone()));

        // TODO: Switch to safer_ffi::export_ffi
        quote! {
            #[no_mangle]
            pub fn #ffi_function_name(#ffi_function_inputs) #ffi_function_output {
                #(#type_conversions)*

                let res = #module_name::#function_name(#input_names);

                res.into()
            }
        }
        .to_tokens(tokens);
    }
}

#[derive(Debug)]
struct FFIModule {
    functions: Vec<FFIFunction>,
}

impl FFIModule {
    pub fn new(impl_block: ItemImpl) -> syn::Result<Self> {
        let Type::Path(path) = *impl_block.self_ty.clone() else {
            return Err(syn::Error::new(impl_block.span(), "impl block must be for a struct"));
        };

        // Find functions
        let functions = impl_block
            .items
            .into_iter()
            .filter_map(|item| match item {
                ImplItem::Method(method) => method
                    .attrs
                    .iter()
                    .any(|attr| {
                        attr.path
                            .segments
                            .iter()
                            .any(|segment| segment.ident == "safer_ffi_gen_func")
                    })
                    .then_some(FFIFunction::new(path.clone(), method)),
                _ => None,
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(FFIModule { functions })
    }
}

impl ToTokens for FFIModule {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.functions.iter().for_each(|f| f.to_tokens(tokens))
    }
}

impl Parse for FFIModule {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let impl_block = input.parse::<ItemImpl>()?;
        FFIModule::new(impl_block)
    }
}

#[proc_macro_attribute]
pub fn safer_ffi_gen(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut item = proc_macro2::TokenStream::from(item);

    let output = syn::parse2::<FFIModule>(item.clone()).unwrap();

    // Add the output to the input
    output.to_tokens(&mut item);

    proc_macro::TokenStream::from(item)
}

#[proc_macro_attribute]
// TODO: This isn't necessary, but it is easier for now with debugging to avoid
// having to filter out recursive safer_ffi_gen inside an impl
pub fn safer_ffi_gen_func(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}
