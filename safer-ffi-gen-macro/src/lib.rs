use convert_case::{Case, Casing};
use proc_macro2::{Ident, Span};
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse::Parse, punctuated::Punctuated, spanned::Spanned, token::Comma, FnArg, ImplItem,
    ImplItemMethod, ItemImpl, Pat, ReturnType, Type, TypePath, TypeReference,
};

#[derive(Debug, Clone)]
struct FFIType {
    native_type: Type,
}

impl FFIType {
    fn new(native_type: Type) -> Self {
        Self { native_type }
    }
}

#[derive(Debug)]
struct ReturnFFIType(Option<FFIType>);

impl ToTokens for ReturnFFIType {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self.0.as_ref().map(|v| &v.native_type) {
            Some(ty) => quote! { -> <#ty as ::safer_ffi_gen::FfiType>::Safe },
            None => quote! {},
        }
        .to_tokens(tokens)
    }
}

#[derive(Debug, Clone)]
struct FFIArgument {
    name: Ident,
    ffi_type: FFIType,
}

impl ToTokens for FFIArgument {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let name = &self.name;
        let ty = &self.ffi_type.native_type;

        quote! {
           #name: <#ty as ::safer_ffi_gen::FfiType>::Safe
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

        quote! {
            let #name = ::safer_ffi_gen::FfiType::from_safe(#name);
        }
        .to_tokens(tokens)
    }
}

fn arg_to_ffi(module_name: &TypePath, arg: FnArg) -> syn::Result<FFIArgument> {
    match arg {
        FnArg::Receiver(rec) => {
            let ty = Type::Path(module_name.clone());
            let ty = match rec.reference {
                Some((and_token, lifetime)) => Type::Reference(TypeReference {
                    and_token,
                    lifetime,
                    mutability: rec.mutability,
                    elem: ty.into(),
                }),
                None => ty,
            };

            Ok(FFIArgument {
                name: Ident::new("this", Span::call_site()),
                ffi_type: FFIType::new(ty),
            })
        }
        FnArg::Typed(pat_type) => {
            let Pat::Ident(ident) = &*pat_type.pat else {
                return Err(syn::Error::new_spanned(&*pat_type.pat, "Expected an identifier"));
            };

            Ok(FFIArgument {
                name: ident.ident.clone(),
                ffi_type: FFIType::new((*pat_type.ty).clone()),
            })
        }
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

        let parameters = method
            .sig
            .inputs
            .into_iter()
            .map(|arg| arg_to_ffi(&module_name, arg))
            .collect::<Result<_, _>>()?;

        let output = match method.sig.output {
            ReturnType::Default => ReturnFFIType(None),
            ReturnType::Type(_, otype) => ReturnFFIType(Some(FFIType::new(*otype))),
        };

        Ok(Self {
            module_name,
            function_name: method.sig.ident,
            parameters,
            output,
        })
    }

    fn ffi_function_name(&self) -> Ident {
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
        let ffi_function_name = self.ffi_function_name();
        let ffi_function_inputs = &self.parameters;
        let ffi_function_output = &self.output;
        let module_name = &self.module_name;
        let function_name = &self.function_name;

        let type_conversions = self
            .parameters
            .iter()
            .cloned()
            .map(FFIArgumentConversion::new);

        let input_names: Punctuated<Ident, Comma> =
            Punctuated::from_iter(self.parameters.iter().map(|arg| arg.name.clone()));

        quote! {
            #[::safer_ffi_gen::safer_ffi::ffi_export]
            pub fn #ffi_function_name(#ffi_function_inputs) #ffi_function_output {
                #(#type_conversions)*

                let res = #module_name::#function_name(#input_names);

                ::safer_ffi_gen::FfiType::into_safe(res)
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

#[proc_macro_attribute]
pub fn ffi_type(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    process_ffi_type(args.into(), input.into())
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

fn process_ffi_type(
    args: proc_macro2::TokenStream,
    input: proc_macro2::TokenStream,
) -> Result<proc_macro2::TokenStream, syn::Error> {
    let args_span = args.span();

    match &*syn::parse2::<Ident>(args)?.to_string() {
        "opaque" => Ok(()),
        s => Err(syn::Error::new(
            args_span,
            format!("Expected `opaque`, found {s}"),
        )),
    }?;

    let ty_def = syn::parse2::<syn::ItemStruct>(input)?;
    let ty = &ty_def.ident;
    let ty_visibility = &ty_def.vis;
    let drop_ident = Ident::new(
        &format!("{}_free", ty.to_string().to_case(Case::Snake)),
        Span::call_site(),
    );

    Ok(quote! {
        #[::safer_ffi_gen::safer_ffi::derive_ReprC]
        #[ReprC::opaque]
        #ty_def

        impl ::safer_ffi_gen::FfiType for #ty {
            type Safe = ::safer_ffi_gen::safer_ffi::boxed::Box<#ty>;

            fn into_safe(self) -> Self::Safe {
                ::safer_ffi_gen::safer_ffi::boxed::Box::new(self)
            }

            fn from_safe(x: Self::Safe) -> Self {
                *x.into()
            }
        }

        #[::safer_ffi_gen::safer_ffi::ffi_export]
        #ty_visibility fn #drop_ident(x: <#ty as ::safer_ffi_gen::FfiType>::Safe) {
            ::core::mem::drop(x);
        }
    })
}
