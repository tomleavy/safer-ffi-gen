use convert_case::{Case, Casing};
use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::{
    fold::Fold, parse::Parse, punctuated::Punctuated, token::Comma, Attribute, AttributeArgs,
    FnArg, GenericArgument, ImplItem, ImplItemMethod, ItemImpl, Meta, NestedMeta, Pat,
    PathArguments, PathSegment, ReturnType, Type, TypePath, TypeReference,
};

const EXPORT_MARKER: &str = "safer_ffi_gen_func";

#[derive(Debug, Clone)]
struct FFIType {
    native_type: Type,
}

impl FFIType {
    fn new(native_type: Type, self_ty: Type) -> Self {
        Self {
            native_type: resolve_self(native_type, self_ty),
        }
    }

    fn new_no_self(native_type: Type) -> Self {
        Self { native_type }
    }
}

struct SelfResolver(Type);

impl Fold for SelfResolver {
    fn fold_type(&mut self, i: Type) -> Type {
        match &i {
            Type::Path(p) => p
                .path
                .segments
                .first()
                .filter(|segment| p.path.segments.len() == 1 && segment.ident == "Self")
                .map(|_| self.0.clone())
                .unwrap_or_else(|| syn::fold::fold_type(self, i)),
            _ => syn::fold::fold_type(self, i),
        }
    }
}

fn resolve_self(ty: Type, self_ty: Type) -> Type {
    SelfResolver(self_ty).fold_type(ty)
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
                ffi_type: FFIType::new_no_self(out_type.clone()),
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
                ffi_type: FFIType::new(ty, Type::Path(module_name.clone())),
                is_out: false,
            })
        }
        FnArg::Typed(pat_type) => {
            let Pat::Ident(ident) = &*pat_type.pat else {
                return Err(syn::Error::new_spanned(&*pat_type.pat, "Expected an identifier"));
            };

            Ok(FFIArgument {
                name: ident.ident.clone(),
                ffi_type: FFIType::new((*pat_type.ty).clone(), Type::Path(module_name.clone())),
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
    is_async: bool,
}

impl FFIFunction {
    fn new(module_name: TypePath, method: ImplItemMethod) -> syn::Result<Self> {
        let mut parameters = method
            .sig
            .inputs
            .into_iter()
            .map(|arg| arg_to_ffi(&module_name, arg))
            .collect::<Result<Punctuated<FFIArgument, Comma>, _>>()?;

        let output = match method.sig.output {
            ReturnType::Default => ReturnFFIType(None),
            ReturnType::Type(_, otype) => {
                ReturnFFIType(Some(FFIType::new(*otype, Type::Path(module_name.clone()))))
            }
        };

        if let Some(out_arg) = output.out_arg() {
            parameters.push(out_arg);
        }

        Ok(Self {
            module_name,
            function_name: method.sig.ident,
            parameters,
            output,
            is_async: method.sig.asyncness.is_some(),
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

    fn convert_output_with_result(&self) -> TokenStream {
        let module_name = &self.module_name;

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
    }

    fn convert_output_no_result(&self) -> TokenStream {
        quote! {
            ::safer_ffi_gen::FfiType::into_safe(res)
        }
    }

    fn convert_output(&self) -> TokenStream {
        if self.output.is_result() {
            self.convert_output_with_result()
        } else {
            self.convert_output_no_result()
        }
    }

    fn method_impl_async_blocking(
        &self,
        module_name: &TypePath,
        function_name: &Ident,
        input_names: &Punctuated<Ident, Comma>,
    ) -> TokenStream {
        quote! {
            let res = safer_ffi_gen::BLOCKING_ASYNC_RUNTIME
                .block_on(#module_name::#function_name(#input_names));
        }
    }

    fn method_impl_sync(
        &self,
        module_name: &TypePath,
        function_name: &Ident,
        input_names: &Punctuated<Ident, Comma>,
    ) -> TokenStream {
        quote! {
            let res = #module_name::#function_name(#input_names);
        }
    }

    fn method_impl(&self) -> TokenStream {
        let module_name = &self.module_name;
        let function_name = &self.function_name;

        let input_names: Punctuated<Ident, Comma> = Punctuated::from_iter(
            self.parameters
                .iter()
                .filter(|arg| !arg.is_out)
                .map(|arg| arg.name.clone()),
        );

        if self.is_async {
            self.method_impl_async_blocking(module_name, function_name, &input_names)
        } else {
            self.method_impl_sync(module_name, function_name, &input_names)
        }
    }
}

impl ToTokens for FFIFunction {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let ffi_function_name = self.ffi_function_name();
        let ffi_function_inputs = &self.parameters;
        let ffi_function_output = &self.output;

        let type_conversions = self
            .parameters
            .iter()
            .filter(|arg| !arg.is_out)
            .cloned()
            .map(FFIArgumentConversion::new);

        let method_impl = self.method_impl();
        let convert_output = self.convert_output();

        quote! {
            #[::safer_ffi_gen::safer_ffi::ffi_export]
            pub fn #ffi_function_name(#ffi_function_inputs) #ffi_function_output {
                #(#type_conversions)*

                #method_impl
                #convert_output
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
        if let Some((_, trait_, _)) = impl_block.trait_ {
            return Err(syn::Error::new_spanned(
                trait_,
                "safer_ffi_gen does not support trait implementations",
            ));
        }

        let Type::Path(path) = &*impl_block.self_ty else {
            return Err(syn::Error::new_spanned(
                &impl_block.self_ty,
                "impl block must be for a type path",
            ));
        };

        // Find functions
        let functions = impl_block
            .items
            .into_iter()
            .filter_map(|item| match item {
                ImplItem::Method(method) => exported(&method.attrs)
                    .is_some()
                    .then(|| FFIFunction::new(path.clone(), method)),
                ImplItem::Const(syn::ImplItemConst { attrs, .. })
                | ImplItem::Type(syn::ImplItemType { attrs, .. })
                | ImplItem::Macro(syn::ImplItemMacro { attrs, .. }) => {
                    exported(&attrs).map(|marker| {
                        Err(syn::Error::new_spanned(
                            marker,
                            "Only functions can be exported by safer_ffi_gen",
                        ))
                    })
                }
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

fn exported(attrs: &[Attribute]) -> Option<&Ident> {
    attrs.iter().find_map(|attr| {
        attr.path
            .get_ident()
            .filter(|&ident| ident == EXPORT_MARKER)
    })
}

#[proc_macro_attribute]
pub fn safer_ffi_gen(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut result = proc_macro2::TokenStream::from(item.clone());

    let output = syn::parse_macro_input!(item as FFIModule);

    // Add the output to the input
    output.to_tokens(&mut result);

    result.into()
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
    let args = syn::parse_macro_input!(args as AttributeArgs);
    let ty_def = syn::parse_macro_input!(input as syn::ItemStruct);

    process_ffi_type(args, ty_def)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

fn process_ffi_type(
    args: AttributeArgs,
    ty_def: syn::ItemStruct,
) -> Result<proc_macro2::TokenStream, syn::Error> {
    let args = args
        .into_iter()
        .try_fold(FfiTypeArgs::default(), |mut acc, arg| match arg {
            NestedMeta::Meta(Meta::Path(p)) => {
                match &*p.get_ident().map(ToString::to_string).unwrap_or_default() {
                    "opaque" => {
                        acc.opaque = true;
                        Ok(acc)
                    }
                    "clone" => {
                        acc.clone = true;
                        Ok(acc)
                    }
                    _ => Err(syn::Error::new_spanned(p, "Unknown argument")),
                }
            }
            _ => Err(syn::Error::new_spanned(arg, "Unknown argument")),
        })?;

    args.opaque.then_some(()).ok_or_else(|| {
        syn::Error::new(
            Span::call_site(),
            "`opaque` must be specified as argument to `ffi_type`",
        )
    })?;

    let ty = &ty_def.ident;
    let ty_visibility = &ty_def.vis;
    let ty_prefix = ty.to_string().to_case(Case::Snake);
    let drop_ident = Ident::new(&format!("{ty_prefix}_free"), Span::call_site());

    let clone_fn = args.clone.then(|| {
        let clone_ident = Ident::new(&format!("{ty_prefix}_clone"), Span::call_site());

        quote! {
            #[::safer_ffi_gen::safer_ffi::ffi_export]
            #ty_visibility fn #clone_ident(
                x: <&#ty as ::safer_ffi_gen::FfiType>::Safe,
            ) -> <#ty as ::safer_ffi_gen::FfiType>::Safe {
                ::safer_ffi_gen::safer_ffi::boxed::Box::new(::std::clone::Clone::clone(x))
            }
        }
    });

    let last_error_ident = Ident::new(&format!("{ty_prefix}_last_error"), Span::call_site());

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

        #clone_fn

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

#[derive(Debug, Default)]
struct FfiTypeArgs {
    opaque: bool,
    clone: bool,
}
