use crate::{has_only_lifetime_parameters, Error, ErrorReason};
use heck::ToSnakeCase;
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    punctuated::Punctuated, spanned::Spanned, Attribute, Generics, Ident, ItemStruct, Meta, Token,
    Visibility,
};

pub fn process_ffi_type(
    args: Punctuated<Meta, Token![,]>,
    type_def: ItemStruct,
) -> Result<FfiTypeOutput, Error> {
    let has_only_lifetime_params = has_only_lifetime_parameters(&type_def.generics);

    let args = args
        .iter()
        .try_fold(FfiTypeArgs::default(), |mut acc, arg| match arg {
            Meta::Path(p) => {
                if p.is_ident("opaque") {
                    acc.repr = Some(WithSpan::new(FfiRepr::Opaque, p.span()));
                    Ok(acc)
                } else if p.is_ident("clone") {
                    if has_only_lifetime_params {
                        acc.clone = Some(p.span());
                        Ok(acc)
                    } else {
                        Err(ErrorReason::CloneOnGenericType.spanned(p))
                    }
                } else {
                    Err(ErrorReason::UnknownArg.spanned(p))
                }
            }
            _ => Err(ErrorReason::UnknownArg.spanned(arg)),
        })?;

    let ty_repr = type_repr(&type_def.attrs).map(|r| r.map(FfiRepr::from));

    let repr = match (ty_repr, args.repr) {
        (Some(a), Some(_)) => Err(ErrorReason::IncompatibleRepr.with_span(a.span)),
        (Some(a), None) | (None, Some(a)) => Ok(a.item),
        (None, None) => Err(ErrorReason::MissingRepr.with_span(Span::call_site())),
    }?;

    Ok(FfiTypeOutput {
        repr,
        type_def,
        clone: args.clone.is_some(),
    })
}

#[derive(Debug, Default)]
struct FfiTypeArgs {
    repr: Option<WithSpan<FfiRepr>>,
    clone: Option<Span>,
}

#[derive(Debug)]
pub struct FfiTypeOutput {
    repr: FfiRepr,
    type_def: ItemStruct,
    clone: bool,
}

impl ToTokens for FfiTypeOutput {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let has_only_lifetime_params = has_only_lifetime_parameters(&self.type_def.generics);
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
                self.repr,
            )
        });
        let type_def = &self.type_def;

        let ty_repr = match self.repr {
            FfiRepr::C => quote! { #[repr(C)] },
            FfiRepr::Opaque => quote! { #[ReprC::opaque] },
            FfiRepr::Transparent => quote! { #[repr(transparent)] },
        };

        let ffi_type_impl = impl_ffi_type(&self.type_def.ident, &self.type_def.generics, self.repr);

        let out = quote! {
            #[::safer_ffi_gen::safer_ffi::derive_ReprC]
            #ty_repr
            #type_def

            #ffi_type_impl

            #drop_fn

            #clone_fn
        };
        out.to_tokens(tokens);
    }
}

fn impl_ffi_type(ty: &Ident, generics: &Generics, repr: FfiRepr) -> TokenStream {
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    match repr {
        FfiRepr::C | FfiRepr::Transparent => quote! {
            impl #impl_generics ::safer_ffi_gen::FfiType for #ty #type_generics #where_clause {
                type Safe = #ty #type_generics;

                fn into_safe(self) -> Self::Safe {
                    self
                }

                fn from_safe(x: Self::Safe) -> Self {
                    x
                }
            }
        },
        FfiRepr::Opaque => quote! {
            impl #impl_generics ::safer_ffi_gen::FfiType for #ty #type_generics #where_clause {
                type Safe = ::safer_ffi_gen::safer_ffi::boxed::Box<#ty #type_generics>;

                fn into_safe(self) -> Self::Safe {
                    ::safer_ffi_gen::safer_ffi::boxed::Box::new(self)
                }

                fn from_safe(x: Self::Safe) -> Self {
                    *x.into()
                }
            }
        },
    }
}

fn export_clone(
    ty: &Ident,
    type_visibility: &Visibility,
    generics: &Generics,
    repr: FfiRepr,
) -> TokenStream {
    let prefix = fn_prefix(ty);
    let clone_ident = Ident::new(&format!("{prefix}_clone"), Span::call_site());
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();
    let return_value = match repr {
        FfiRepr::Opaque => quote! {
            ::safer_ffi_gen::safer_ffi::boxed::Box::new(::std::clone::Clone::clone(x))
        },
        FfiRepr::C | FfiRepr::Transparent => quote! {
            ::std::clone::Clone::clone(x)
        },
    };

    quote! {
        #[::safer_ffi_gen::safer_ffi::ffi_export]
        #type_visibility fn #clone_ident #impl_generics(
            x: <&#ty #type_generics as ::safer_ffi_gen::FfiType>::Safe,
        ) -> <#ty #type_generics as ::safer_ffi_gen::FfiType>::Safe #where_clause {
            #return_value
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

fn type_repr(attrs: &[Attribute]) -> Option<WithSpan<TypeRepr>> {
    attrs
        .iter()
        .find(|attr| attr.path().is_ident("repr"))
        .and_then(|attr| {
            attr.parse_args_with(Punctuated::<Meta, Token![,]>::parse_terminated)
                .ok()
        })
        .and_then(|args| {
            args.into_iter().find_map(|meta| match meta {
                Meta::Path(p) => {
                    if p.is_ident("C") {
                        Some(WithSpan::new(TypeRepr::C, p.span()))
                    } else if p.is_ident("transparent") {
                        Some(WithSpan::new(TypeRepr::Transparent, p.span()))
                    } else {
                        None
                    }
                }
                _ => None,
            })
        })
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum TypeRepr {
    C,
    Transparent,
}

#[derive(Clone, Copy, Debug)]
enum FfiRepr {
    Opaque,
    C,
    Transparent,
}

impl From<TypeRepr> for FfiRepr {
    fn from(r: TypeRepr) -> FfiRepr {
        match r {
            TypeRepr::C => FfiRepr::C,
            TypeRepr::Transparent => FfiRepr::Transparent,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct WithSpan<T> {
    item: T,
    span: Span,
}

impl<T> WithSpan<T> {
    fn new(item: T, span: Span) -> Self {
        Self { item, span }
    }

    fn map<F, U>(self, f: F) -> WithSpan<U>
    where
        F: FnOnce(T) -> U,
    {
        WithSpan {
            item: f(self.item),
            span: self.span,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ffi_type::{type_repr, TypeRepr},
        process_ffi_type, Error, ErrorReason,
    };
    use assert2::assert;
    use syn::{parse_quote, ItemStruct};

    #[test]
    fn unknown_arg_to_ffi_type_fails() {
        let res = process_ffi_type(parse_quote! { foobar }, parse_quote! { struct Foo {} });
        assert!(let Err(Error { reason: ErrorReason::UnknownArg, .. }) = res);
    }

    #[test]
    fn clone_for_type_generic_over_type_fails() {
        let res = process_ffi_type(
            parse_quote! { clone },
            parse_quote! { struct Foo<T> { x: T } },
        );
        assert!(let Err(Error { reason: ErrorReason::CloneOnGenericType, .. }) = res);
    }

    #[test]
    fn clone_for_type_generic_over_lifetime_fails() {
        let out = process_ffi_type(
            parse_quote! { opaque, clone },
            parse_quote! { struct Foo<'a> { s: &'a str } },
        )
        .unwrap();
        assert!(out.clone);
    }

    #[test]
    fn repr_c_can_be_detected() {
        let ty: ItemStruct = parse_quote! {
            #[repr(C)]
            struct Foo(i32);
        };
        assert!(type_repr(&ty.attrs).unwrap().item == TypeRepr::C);
    }

    #[test]
    fn repr_transparent_can_be_detected() {
        let ty: ItemStruct = parse_quote! {
            #[repr(transparent)]
            struct Foo(i32);
        };
        assert!(type_repr(&ty.attrs).unwrap().item == TypeRepr::Transparent);
    }
}
