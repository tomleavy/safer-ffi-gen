use convert_case::{Case, Casing};
use proc_macro2::Ident;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse::Parse, punctuated::Punctuated, spanned::Spanned, token::Comma,
    AngleBracketedGenericArguments, FnArg, ImplItem, ImplItemMethod, ItemImpl, Pat, PathArguments,
    ReturnType, Type,
};

#[derive(Debug)]
enum FFIType {
    Vec(AngleBracketedGenericArguments),
    // TODO: Slice
    String,
    Box(AngleBracketedGenericArguments),
    Opaque(Ident),
    // TODO: &str
    // TODO: Anything that isn't listed should be repr_c::Box
}

impl FFIType {
    fn new(ty: Box<Type>) -> Result<Self, syn::Error> {
        let Type::Path(path) = *ty else {
            return Err(syn::Error::new(ty.span(), "unexpected non-path value"));
        };

        let last_segment = path
            .path
            .segments
            .last()
            .ok_or_else(|| syn::Error::new(path.span(), "unexpected empty path"))?;

        match last_segment.ident.to_string().as_str() {
            "Vec" => {
                let PathArguments::AngleBracketed(ref args) = last_segment.arguments else {
                    return Err(syn::Error::new(last_segment.arguments.span(), "invalid path arguments"));
                };
                Ok(FFIType::Vec(args.clone()))
            }
            "Box" => {
                let PathArguments::AngleBracketed(ref args) = last_segment.arguments else {
                    return Err(syn::Error::new(last_segment.arguments.span(), "invalid path arguments"));
                };
                Ok(FFIType::Box(args.clone()))
            }
            "String" => Ok(FFIType::String),
            // Anything that isn't listed should be repr_c::Box
            _ => Ok(FFIType::Opaque(last_segment.ident.clone())),
        }
    }
}

impl ToTokens for FFIType {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            FFIType::Vec(args) => quote! { safer_ffi::prelude::repr_c::Vec #args },
            FFIType::Box(args) => quote! { safer_ffi::prelude::repr_c::Box #args },
            FFIType::String => quote! { safer_ffi::prelude::repr_c::String },
            FFIType::Opaque(ident) => quote! { safer_ffi::prelude::repr_c::Box<#ident> },
        }
        .to_tokens(tokens)
    }
}

#[derive(Debug)]
struct ReturnFFIType(Option<FFIType>);

impl ToTokens for ReturnFFIType {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match &self.0 {
            Some(ffi_type) => quote! { -> #ffi_type },
            None => quote! {},
        }
        .to_tokens(tokens)
    }
}

#[derive(Debug)]
struct FFIArgument {
    name: Box<Pat>,
    ffi_type: FFIType,
}

impl ToTokens for FFIArgument {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let name = &self.name;
        let ffi_type = &self.ffi_type;

        quote! {
           #name: #ffi_type
        }
        .to_tokens(tokens)
    }
}

fn arg_to_ffi(arg: FnArg) -> syn::Result<FFIArgument> {
    match arg {
        // TODO: Receiver conversion, which requires knowing the type name
        FnArg::Receiver(rec) => Err(syn::Error::new(
            rec.span(),
            "only non-receiver methods supported",
        )),

        FnArg::Typed(pat_type) => Ok(FFIArgument {
            name: pat_type.pat,
            ffi_type: FFIType::new(pat_type.ty)?,
        }),
    }
}

#[derive(Debug)]
struct FFIFunction {
    name: String,
    parameters: Punctuated<FFIArgument, Comma>,
    output: ReturnFFIType,
}

impl FFIFunction {
    fn new(module_name: &str, method: ImplItemMethod) -> syn::Result<Self> {
        // TODO: Support async
        if method.sig.asyncness.is_some() {
            return Err(syn::Error::new(method.span(), "async is not supported"));
        }

        let inputs =
            method
                .sig
                .inputs
                .into_iter()
                .try_fold(Punctuated::new(), |mut inputs, arg| {
                    inputs.push(arg_to_ffi(arg)?);
                    Ok::<_, syn::Error>(inputs)
                })?;

        let output = match method.sig.output {
            ReturnType::Default => ReturnFFIType(None),
            ReturnType::Type(_, otype) => ReturnFFIType(Some(FFIType::new(otype)?)),
        };

        Ok(Self {
            name: format!("{}_{}", module_name.to_case(Case::Snake), method.sig.ident),
            parameters: inputs,
            output,
        })
    }
}

impl ToTokens for FFIFunction {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let ident = format_ident!("{}", self.name);
        let inputs = &self.parameters;
        let output = &self.output;

        // TODO: Switch to safer_ffi::export_ffi
        quote! {
            #[no_mangle]
            pub fn #ident(#inputs) #output {
                // TODO: Convert the inputs, call original function, convert output
                println!("Hello world!");
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

        let module_name = path
            .path
            .get_ident()
            .ok_or_else(|| syn::Error::new(path.span(), "multiple path segments are not allowed"))?
            .to_string();

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
                    .then_some(FFIFunction::new(&module_name, method)),
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
