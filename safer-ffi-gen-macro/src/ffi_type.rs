use crate::{has_only_lifetime_parameters, Error, ErrorReason};
use heck::ToSnakeCase;
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{AttributeArgs, Generics, Ident, ItemStruct, Meta, NestedMeta, Visibility};

pub fn process_ffi_type(args: AttributeArgs, type_def: ItemStruct) -> Result<FfiTypeOutput, Error> {
    let has_only_lifetime_params = has_only_lifetime_parameters(&type_def.generics);

    let args = args
        .iter()
        .try_fold(FfiTypeArgs::default(), |mut acc, arg| match arg {
            NestedMeta::Meta(Meta::Path(p)) => {
                match &*p.get_ident().map(ToString::to_string).unwrap_or_default() {
                    "opaque" => {
                        acc.opaque = true;
                        Ok(acc)
                    }
                    "clone" if has_only_lifetime_params => {
                        acc.clone = true;
                        Ok(acc)
                    }
                    "clone" => Err(ErrorReason::CloneOnGenericType.spanned(p)),
                    _ => Err(ErrorReason::UnknownArg.spanned(p)),
                }
            }
            _ => Err(ErrorReason::UnknownArg.spanned(arg)),
        })?;

    args.opaque
        .then_some(())
        .ok_or_else(|| ErrorReason::OpaqueRequired.with_span(Span::call_site()))?;

    Ok(FfiTypeOutput {
        type_def,
        clone: args.clone,
    })
}

#[derive(Debug, Default)]
struct FfiTypeArgs {
    opaque: bool,
    clone: bool,
}

#[derive(Debug)]
pub struct FfiTypeOutput {
    type_def: ItemStruct,
    clone: bool,
}

impl ToTokens for FfiTypeOutput {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let has_only_lifetime_params = has_only_lifetime_parameters(&self.type_def.generics);
        let (impl_generics, type_generics, where_clause) = self.type_def.generics.split_for_impl();
        let drop_fn = has_only_lifetime_params.then(|| {
            export_drop(
                &self.type_def.ident,
                &self.type_def.vis,
                &self.type_def.generics,
            )
        });
        let clone_fn = self.clone.then(|| {
            export_clone(
                &self.type_def.ident,
                &self.type_def.vis,
                &self.type_def.generics,
            )
        });
        let type_def = &self.type_def;
        let ty = &self.type_def.ident;

        let out = quote! {
            #[::safer_ffi_gen::safer_ffi::derive_ReprC]
            #[ReprC::opaque]
            #type_def

            impl #impl_generics ::safer_ffi_gen::FfiType for #ty #type_generics #where_clause {
                type Safe = ::safer_ffi_gen::safer_ffi::boxed::Box<#ty #type_generics>;

                fn into_safe(self) -> Self::Safe {
                    ::safer_ffi_gen::safer_ffi::boxed::Box::new(self)
                }

                fn from_safe(x: Self::Safe) -> Self {
                    *x.into()
                }
            }

            #drop_fn

            #clone_fn
        };
        out.to_tokens(tokens);
    }
}

fn export_clone(ty: &Ident, type_visibility: &Visibility, generics: &Generics) -> TokenStream {
    let prefix = fn_prefix(ty);
    let clone_ident = Ident::new(&format!("{prefix}_clone"), Span::call_site());
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    quote! {
        #[::safer_ffi_gen::safer_ffi::ffi_export]
        #type_visibility fn #clone_ident #impl_generics(
            x: <&#ty #type_generics as ::safer_ffi_gen::FfiType>::Safe,
        ) -> <#ty #type_generics as ::safer_ffi_gen::FfiType>::Safe #where_clause {
            ::safer_ffi_gen::safer_ffi::boxed::Box::new(::std::clone::Clone::clone(x))
        }
    }
}

fn export_drop(ty: &Ident, type_visibility: &Visibility, generics: &Generics) -> TokenStream {
    let prefix = fn_prefix(ty);
    let drop_ident = Ident::new(&format!("{prefix}_free"), Span::call_site());
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    quote! {
        #[::safer_ffi_gen::safer_ffi::ffi_export]
        #type_visibility fn #drop_ident #impl_generics(
            x: <#ty #type_generics as ::safer_ffi_gen::FfiType>::Safe,
        ) #where_clause {
            ::core::mem::drop(x);
        }
    }
}

fn fn_prefix(ty: &Ident) -> String {
    ty.to_string().to_snake_case()
}

#[cfg(test)]
mod tests {
    use crate::{process_ffi_type, Error, ErrorReason};
    use assert2::assert;
    use syn::parse_quote;

    #[test]
    fn unknown_arg_to_ffi_type_fails() {
        let res = process_ffi_type(
            vec![parse_quote! { foobar }],
            parse_quote! { struct Foo {} },
        );
        assert!(let Err(Error { reason: ErrorReason::UnknownArg, .. }) = res);
    }

    #[test]
    fn clone_for_type_generic_over_type_fails() {
        let res = process_ffi_type(
            vec![parse_quote! { clone }],
            parse_quote! { struct Foo<T> { x: T } },
        );
        assert!(let Err(Error { reason: ErrorReason::CloneOnGenericType, .. }) = res);
    }

    #[test]
    fn clone_for_type_generic_over_lifetime_fails() {
        let out = process_ffi_type(
            vec![parse_quote! { opaque }, parse_quote! { clone }],
            parse_quote! { struct Foo<'a> { s: &'a str } },
        )
        .unwrap();
        assert!(out.clone);
    }
}
