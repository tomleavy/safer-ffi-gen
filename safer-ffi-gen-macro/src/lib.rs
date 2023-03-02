use convert_case::{Case, Casing};
use proc_macro2::{Ident, Span};
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse::Parse, punctuated::Punctuated, spanned::Spanned, token::Comma, FnArg, GenericArgument,
    ImplItem, ImplItemMethod, ItemImpl, Pat, PathArguments, PathSegment, ReturnType, Type,
    TypePath, TypeReference,
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

impl ReturnFFIType {
    fn last_path_segment(&self) -> Option<&PathSegment> {
        self.0
            .as_ref()
            .and_then(|ffi_type| match ffi_type.native_type {
                Type::Path(ref path) => path.path.segments.last(),
                _ => None,
            })
            .filter(|last_path| last_path.ident.to_string().as_str() == "Result")
    }

    fn is_result(&self) -> bool {
        self.last_path_segment().is_some()
    }

    fn out_arg(&self) -> Option<FFIArgument> {
        self.last_path_segment().and_then(|ffi_type| {
            let name = Ident::new("out", Span::call_site());

            let PathArguments::AngleBracketed(ref args) = ffi_type.arguments else {
                return None;
            };

            let Some(GenericArgument::Type(out_type)) = args.args.first() else {
                    return None;
                };

            if let Type::Tuple(tuple) = out_type {
                if tuple.elems.is_empty() {
                    return None;
                }
            }

            Some(FFIArgument {
                name,
                ffi_type: FFIType::new(out_type.clone()),
                is_out: true,
            })
        })
    }
}

impl ToTokens for ReturnFFIType {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        if self.is_result() {
            return quote! { -> ::std::os::raw::c_int }.to_tokens(tokens);
        }

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
    is_out: bool,
}

impl ToTokens for FFIArgument {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let name = &self.name;
        let ty = &self.ffi_type.native_type;

        if self.is_out {
            quote! {
                #name: ::safer_ffi::prelude::Out<'_, <#ty as ::safer_ffi_gen::FfiType>::Safe>
            }
        } else {
            quote! {
               #name: <#ty as ::safer_ffi_gen::FfiType>::Safe
            }
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
                is_out: false,
            })
        }
        FnArg::Typed(pat_type) => {
            let Pat::Ident(ident) = &*pat_type.pat else {
                return Err(syn::Error::new_spanned(&*pat_type.pat, "Expected an identifier"));
            };

            Ok(FFIArgument {
                name: ident.ident.clone(),
                ffi_type: FFIType::new((*pat_type.ty).clone()),
                is_out: false,
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

        let mut parameters = method
            .sig
            .inputs
            .into_iter()
            .map(|arg| arg_to_ffi(&module_name, arg))
            .collect::<Result<Punctuated<FFIArgument, Comma>, _>>()?;

        let output = match method.sig.output {
            ReturnType::Default => ReturnFFIType(None),
            ReturnType::Type(_, otype) => ReturnFFIType(Some(FFIType::new(*otype))),
        };

        if let Some(out_arg) = output.out_arg() {
            parameters.push(out_arg);
        }

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
            .filter(|arg| !arg.is_out)
            .cloned()
            .map(FFIArgumentConversion::new);

        let input_names: Punctuated<Ident, Comma> = Punctuated::from_iter(
            self.parameters
                .iter()
                .filter(|arg| !arg.is_out)
                .map(|arg| arg.name.clone()),
        );

        let result_handling = if self.output.is_result() {
            let handle_success = if self.output.out_arg().is_some() {
                quote! {
                    out.write(::safer_ffi_gen::FfiType::into_safe(output));
                    0
                }
            } else {
                quote! {0}
            };

            let handle_error = quote! {
                #module_name::set_last_error(err);
                -1
            };

            quote! {
                match res {
                    Ok(output) => {
                        #handle_success
                    }
                    Err(err) => {
                        #handle_error
                    },
                }
            }
        } else {
            quote! {
                ::safer_ffi_gen::FfiType::into_safe(res)
            }
        };

        quote! {
            #[::safer_ffi_gen::safer_ffi::ffi_export]
            pub fn #ffi_function_name(#ffi_function_inputs) #ffi_function_output {
                #(#type_conversions)*

                let res = #module_name::#function_name(#input_names);

                #result_handling
            }
        }
        .to_tokens(tokens)
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

    let last_error_ident = Ident::new(
        &format!("{}_last_error", ty.to_string().to_case(Case::Snake)),
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

        #[::safer_ffi_gen::safer_ffi::ffi_export]
        #ty_visibility fn #last_error_ident() -> Option<::safer_ffi::prelude::repr_c::String> {
            #ty::LAST_ERROR.with(|prev| {
                (*prev.borrow())
                .as_ref()
                .map(|err| ::safer_ffi::prelude::repr_c::String::from(err.to_string()))
            })
        }

        impl #ty {
            thread_local! {
                static LAST_ERROR: ::std::cell::RefCell<Option<Box<dyn std::error::Error>>> = ::std::cell::RefCell::new(None);
            }

            pub(crate) fn set_last_error<E: std::error::Error + 'static>(err: E) {
                Self::LAST_ERROR.with(|prev| *prev.borrow_mut() = Some(Box::new(err)))
            }
        }
    })
}
