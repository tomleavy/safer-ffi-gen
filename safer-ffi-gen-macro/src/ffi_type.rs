use crate::{
    utils::{
        add_lifetime_constraint_to_type, has_only_lifetime_parameters, is_cfg, make_generic_type,
        new_ident,
    },
    Error, ErrorReason,
};
use heck::ToSnakeCase;
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    punctuated::Punctuated, spanned::Spanned, Attribute, Fields, Generics, Ident, Item, ItemEnum,
    ItemStruct, Lifetime, LifetimeParam, Meta, Token, Type, Visibility,
};

pub fn process_ffi_type(
    args: Punctuated<Meta, Token![,]>,
    def: Item,
) -> Result<TokenStream, Error> {
    match def {
        Item::Struct(type_def) => {
            process_ffi_struct(args, type_def).map(ToTokens::into_token_stream)
        }
        Item::Enum(type_def) => process_ffi_enum(args, type_def).map(ToTokens::into_token_stream),
        _ => Err(ErrorReason::UnsupportedItemType.spanned(def)),
    }
}

fn process_ffi_struct(
    args: Punctuated<Meta, Token![,]>,
    type_def: ItemStruct,
) -> Result<FfiStructOutput, Error> {
    let args = FfiTypeArgs::parse(&args, &type_def.generics)?;
    let opaque = is_opaque(&args, &type_def.attrs)?;

    Ok(FfiStructOutput {
        opaque,
        type_def,
        clone: args.clone.is_some(),
    })
}

fn process_ffi_enum(
    args: Punctuated<Meta, Token![,]>,
    type_def: ItemEnum,
) -> Result<FfiEnumOutput, Error> {
    let args = FfiTypeArgs::parse(&args, &type_def.generics)?;
    let opaque = is_opaque(&args, &type_def.attrs)?;
    let repr = repr_type(&type_def.attrs)?;

    Ok(FfiEnumOutput {
        opaque,
        type_def,
        repr,
        clone: args.clone.is_some(),
    })
}

#[derive(Debug, Default)]
struct FfiTypeArgs {
    opaque: Option<Span>,
    clone: Option<Span>,
}

impl FfiTypeArgs {
    fn parse(args: &Punctuated<Meta, Token![,]>, generics: &Generics) -> Result<Self, Error> {
        let has_only_lifetime_params = has_only_lifetime_parameters(generics);

        let args = args
            .iter()
            .try_fold(FfiTypeArgs::default(), |mut acc, arg| match arg {
                Meta::Path(p) => {
                    if p.is_ident("opaque") {
                        acc.opaque = Some(p.span());
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

        Ok(args)
    }
}

#[derive(Debug)]
struct FfiStructOutput {
    opaque: bool,
    type_def: ItemStruct,
    clone: bool,
}

impl ToTokens for FfiStructOutput {
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
                self.opaque,
            )
        });

        let slice_access = (has_only_lifetime_params && self.opaque).then(|| {
            export_slice_access(
                &self.type_def.ident,
                &self.type_def.vis,
                &self.type_def.generics,
            )
        });

        let vec_access = (has_only_lifetime_params && self.opaque).then(|| {
            export_vec_access(
                &self.type_def.ident,
                &self.type_def.vis,
                &self.type_def.generics,
            )
        });

        let type_def = &self.type_def;
        let ty_repr = self.opaque.then(|| quote! { #[repr(opaque)] });

        let ffi_type_impl =
            impl_ffi_type(&self.type_def.ident, &self.type_def.generics, self.opaque);

        let out = quote! {
            #[::safer_ffi_gen::safer_ffi::derive_ReprC]
            #ty_repr
            #type_def

            #ffi_type_impl

            #drop_fn

            #clone_fn

            #slice_access

            #vec_access
        };
        out.to_tokens(tokens);
    }
}

#[derive(Debug)]
pub struct FfiEnumOutput {
    opaque: bool,
    type_def: ItemEnum,
    repr: Option<Type>,
    clone: bool,
}

impl ToTokens for FfiEnumOutput {
    fn to_tokens(&self, tokens: &mut TokenStream) {
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
                self.opaque,
            )
        });

        let slice_access = (has_only_lifetime_params && self.opaque).then(|| {
            export_slice_access(
                &self.type_def.ident,
                &self.type_def.vis,
                &self.type_def.generics,
            )
        });

        let vec_access = (has_only_lifetime_params && self.opaque).then(|| {
            export_vec_access(
                &self.type_def.ident,
                &self.type_def.vis,
                &self.type_def.generics,
            )
        });

        let type_def = &self.type_def;

        let ffi_type_impl =
            impl_ffi_type(&self.type_def.ident, &self.type_def.generics, self.opaque);

        let repr_c_derive = (!self.opaque).then(|| {
            quote! {
                #[::safer_ffi_gen::safer_ffi::derive_ReprC]
            }
        });

        let repr_c_impl = self.opaque.then(|| impl_c_repr_for_enum(&self.type_def));

        let discriminant = (has_only_lifetime_params && self.opaque)
            .then(|| generate_enum_discriminant(&self.type_def, self.repr.as_ref()));

        let variant_accessors = (has_only_lifetime_params && self.opaque)
            .then(|| generate_enum_accessors(&self.type_def));

        let out = quote! {
            #repr_c_derive
            #type_def

            #repr_c_impl

            #ffi_type_impl

            #drop_fn

            #clone_fn

            #slice_access

            #vec_access

            #discriminant

            #variant_accessors
        };
        out.to_tokens(tokens);
    }
}

fn impl_ffi_type(ty: &Ident, generics: &Generics, opaque: bool) -> TokenStream {
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    if opaque {
        quote! {
            impl #impl_generics ::safer_ffi_gen::FfiType for #ty #type_generics #where_clause {
                type Safe = ::safer_ffi_gen::safer_ffi::boxed::Box_<#ty #type_generics>;

                fn into_safe(self) -> Self::Safe {
                    ::safer_ffi_gen::safer_ffi::boxed::Box_::new(self)
                }

                fn from_safe(x: Self::Safe) -> Self {
                    *x.into()
                }
            }
        }
    } else {
        quote! {
            impl #impl_generics ::safer_ffi_gen::FfiType for #ty #type_generics #where_clause {
                type Safe = #ty #type_generics;

                fn into_safe(self) -> Self::Safe {
                    self
                }

                fn from_safe(x: Self::Safe) -> Self {
                    x
                }
            }
        }
    }
}

fn impl_c_repr_for_enum(ty_def: &ItemEnum) -> TokenStream {
    let ty = &ty_def.ident;
    let (impl_generics, type_generics, where_clause) = ty_def.generics.split_for_impl();

    quote! {
        unsafe impl #impl_generics ::safer_ffi_gen::safer_ffi::layout::ReprC for #ty #type_generics #where_clause {
            type CLayout = ::safer_ffi_gen::private::OpaqueLayout<Self>;

            fn is_valid(layout: &Self::CLayout) -> bool {
                unreachable!("This is never called according to safer-ffi `Opaque` implementation")
            }
        }
    }
}

fn generate_enum_discriminant(ty_def: &ItemEnum, repr: Option<&Type>) -> TokenStream {
    let ty = &ty_def.ident;
    let discriminant_ty = new_ident(format!("{ty}Discriminant"));
    let (impl_generics, type_generics, where_clause) = ty_def.generics.split_for_impl();

    let arms = ty_def.variants.iter().map(|v| {
        let name = &v.ident;
        let cfg_attrs = v.attrs.iter().filter(|&a| is_cfg(a));
        quote! {
            #(#cfg_attrs)*
            #ty::#name { .. } => #discriminant_ty::#name
        }
    });

    let repr = repr
        .map(|ty| quote! { #[repr(#ty)] })
        .unwrap_or_else(|| quote! { #[repr(i32)] });

    let variant_defs = ty_def.variants.iter().map(|v| {
        let name = &v.ident;

        let assignment = v
            .discriminant
            .as_ref()
            .map(|(eq, value)| quote! { #eq #value });

        // Ideally the discriminant variants would inherit the cfg attributes from the original
        // variants but safer-ffi does not support such enums.
        quote! {
            #name #assignment
        }
    });

    let function = new_ident(format!("{}_discriminant", ty.to_string().to_snake_case()));

    quote! {
        #[::safer_ffi_gen::safer_ffi::derive_ReprC]
        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        #repr
        pub enum #discriminant_ty {
            #(#variant_defs,)*
        }

        #[::safer_ffi_gen::safer_ffi::ffi_export]
        pub fn #function #impl_generics(x: &#ty #type_generics) -> #discriminant_ty #where_clause {
            match *x {
                #(#arms,)*
            }
        }
    }
}

fn generate_enum_accessors(ty_def: &ItemEnum) -> TokenStream {
    let ty = &ty_def.ident;
    let ty_prefix = ty.to_string().to_snake_case();
    let (impl_generics, type_generics, where_clause) = ty_def.generics.split_for_impl();

    ty_def
        .variants
        .iter()
        .filter_map(|v| match &v.fields {
            Fields::Unnamed(fields) => match fields.unnamed.first() {
                Some(field) if fields.unnamed.len() == 1 => {
                    let variant = &v.ident;

                    let function = new_ident(
                        format!("{ty_prefix}_to_{}", v.ident.to_string().to_snake_case()));

                    let field_ty = &field.ty;

                    let cfg_attrs = v.attrs.iter().filter(|&a| is_cfg(a));

                    Some(quote! {
                        #(#cfg_attrs)*
                        #[::safer_ffi_gen::safer_ffi::ffi_export]
                        pub fn #function #impl_generics(x: &#ty #type_generics) -> ::core::option::Option<&#field_ty> #where_clause {
                            match x {
                                #ty::#variant(data) => ::core::option::Option::Some(data),
                                #[allow(unreachable_patterns)]
                                _ => ::core::option::Option::None,
                            }
                        }
                    })
                }
                _ => None,
            },
            _ => None,
        })
        .collect()
}

fn export_clone(
    ty: &Ident,
    type_visibility: &Visibility,
    generics: &Generics,
    opaque: bool,
) -> TokenStream {
    let prefix = fn_prefix(ty);
    let clone_ident = new_ident(format!("{prefix}_clone"));
    let (_, type_generics, _) = generics.split_for_impl();
    let lifetime = Lifetime::new("'__safer_ffi_gen_lifetime", Span::call_site());
    let mut generics_with_extra_lifetime = generics.clone();

    generics_with_extra_lifetime
        .params
        .push(LifetimeParam::new(lifetime.clone()).into());

    generics_with_extra_lifetime
        .make_where_clause()
        .predicates
        .push(
            add_lifetime_constraint_to_type(
                make_generic_type(ty.clone(), generics),
                lifetime.clone(),
            )
            .into(),
        );

    let (impl_generics, _, where_clause) = generics_with_extra_lifetime.split_for_impl();

    let return_value = if opaque {
        quote! {
            ::safer_ffi_gen::safer_ffi::boxed::Box_::new(::core::clone::Clone::clone(x))
        }
    } else {
        quote! {
            ::core::clone::Clone::clone(x)
        }
    };

    quote! {
        #[::safer_ffi_gen::safer_ffi::ffi_export]
        #type_visibility fn #clone_ident #impl_generics(
            x: <&#lifetime #ty #type_generics as ::safer_ffi_gen::FfiType>::Safe,
        ) -> <#ty #type_generics as ::safer_ffi_gen::FfiType>::Safe #where_clause {
            #return_value
        }
    }
}

fn export_drop(ty: &Ident, type_visibility: &Visibility, generics: &Generics) -> TokenStream {
    let prefix = fn_prefix(ty);
    let drop_ident = new_ident(format!("{prefix}_free"));
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    quote! {
        #[::safer_ffi_gen::safer_ffi::ffi_export]
        #type_visibility fn #drop_ident #impl_generics(
            _x: <#ty #type_generics as ::safer_ffi_gen::FfiType>::Safe,
        ) #where_clause {
        }
    }
}

fn export_slice_access(
    ty: &Ident,
    type_visibility: &Visibility,
    generics: &Generics,
) -> TokenStream {
    let prefix = fn_prefix(ty);
    let get_ident = new_ident(format!("{prefix}_slice_get"));
    let get_mut_ident = new_ident(format!("{prefix}_slice_get_mut"));
    let (_, type_generics, _) = generics.split_for_impl();
    let lifetime = Lifetime::new("'__safer_ffi_gen_lifetime", Span::call_site());
    let mut generics_with_extra_lifetime = generics.clone();

    generics_with_extra_lifetime
        .params
        .push(LifetimeParam::new(lifetime.clone()).into());

    generics_with_extra_lifetime
        .make_where_clause()
        .predicates
        .push(
            add_lifetime_constraint_to_type(
                make_generic_type(ty.clone(), generics),
                lifetime.clone(),
            )
            .into(),
        );

    let (impl_generics, _, where_clause) = generics_with_extra_lifetime.split_for_impl();

    quote! {
        #[::safer_ffi_gen::safer_ffi::ffi_export]
        #type_visibility fn #get_ident #impl_generics(
            items: ::safer_ffi_gen::safer_ffi::slice::slice_ref<#lifetime, #ty #type_generics>,
            index: usize,
        ) -> & #lifetime #ty #type_generics #where_clause {
            &items.as_slice()[index]
        }

        #[::safer_ffi_gen::safer_ffi::ffi_export]
        #type_visibility fn #get_mut_ident #impl_generics(
            items: ::safer_ffi_gen::safer_ffi::slice::slice_mut<#lifetime, #ty #type_generics>,
            index: usize,
        ) -> & #lifetime mut #ty #type_generics #where_clause {
            &mut items.as_slice()[index]
        }
    }
}

fn export_vec_access(ty: &Ident, type_visibility: &Visibility, generics: &Generics) -> TokenStream {
    let prefix = fn_prefix(ty);
    let vec_new_ident = new_ident(format!("{prefix}_vec_new"));
    let vec_free_ident = new_ident(format!("{prefix}_vec_free"));
    let vec_push_ident = new_ident(format!("{prefix}_vec_push"));
    let vec_as_slice_ident = new_ident(format!("{prefix}_vec_as_slice"));
    let vec_as_slice_mut_ident = new_ident(format!("{prefix}_vec_as_slice_mut"));
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();
    let lifetime = Lifetime::new("'__safer_ffi_gen_lifetime", Span::call_site());

    let mut generics_with_extra_lifetime = generics.clone();

    generics_with_extra_lifetime
        .params
        .push(LifetimeParam::new(lifetime.clone()).into());

    generics_with_extra_lifetime
        .make_where_clause()
        .predicates
        .push(
            add_lifetime_constraint_to_type(
                make_generic_type(ty.clone(), generics),
                lifetime.clone(),
            )
            .into(),
        );

    let (impl_generics_with_extra_lifetime, _, where_clause_with_extra_lifetime) =
        generics_with_extra_lifetime.split_for_impl();

    quote! {
        #[::safer_ffi_gen::safer_ffi::ffi_export]
        #type_visibility fn #vec_new_ident #impl_generics() -> ::safer_ffi_gen::safer_ffi::vec::Vec<#ty #type_generics> #where_clause {
            ::safer_ffi_gen::safer_ffi::vec::Vec::EMPTY
        }

        #[::safer_ffi_gen::safer_ffi::ffi_export]
        #type_visibility fn #vec_free_ident #impl_generics(
            _v: ::safer_ffi_gen::safer_ffi::vec::Vec<#ty #type_generics>,
        ) #where_clause {
        }

        #[::safer_ffi_gen::safer_ffi::ffi_export]
        #type_visibility fn #vec_push_ident #impl_generics_with_extra_lifetime(
            v: &#lifetime mut ::safer_ffi_gen::safer_ffi::vec::Vec<#ty #type_generics>,
            item: <#ty #type_generics as ::safer_ffi_gen::FfiType>::Safe,
        ) #where_clause_with_extra_lifetime {
            v.with_rust_mut(|v| v.push(<#ty #type_generics as ::safer_ffi_gen::FfiType>::from_safe(item)));
        }

        #[::safer_ffi_gen::safer_ffi::ffi_export]
        #type_visibility fn #vec_as_slice_ident #impl_generics_with_extra_lifetime(
            v: &#lifetime ::safer_ffi_gen::safer_ffi::vec::Vec<#ty #type_generics>,
        ) -> ::safer_ffi_gen::safer_ffi::slice::slice_ref<#lifetime, #ty #type_generics>
        #where_clause_with_extra_lifetime
        {
            v.as_ref()
        }

        #[::safer_ffi_gen::safer_ffi::ffi_export]
        #type_visibility fn #vec_as_slice_mut_ident #impl_generics_with_extra_lifetime(
            v: &#lifetime mut ::safer_ffi_gen::safer_ffi::vec::Vec<#ty #type_generics>,
        ) -> ::safer_ffi_gen::safer_ffi::slice::slice_mut<#lifetime, #ty #type_generics>
        #where_clause_with_extra_lifetime
        {
            v.as_mut()
        }
    }
}

fn fn_prefix(ty: &Ident) -> String {
    ty.to_string().to_snake_case()
}

fn has_type_repr(attrs: &[Attribute]) -> Result<bool, Error> {
    repr_type(attrs).map(|ty| ty.is_some())
}

fn repr_type(attrs: &[Attribute]) -> Result<Option<Type>, Error> {
    attrs
        .iter()
        .find_map(|attr| attr.path().is_ident("repr").then(|| attr.parse_args()))
        .transpose()
        .map_err(|e| ErrorReason::BadReprType.with_span(e.span()))
}

fn is_opaque(args: &FfiTypeArgs, attrs: &[Attribute]) -> Result<bool, Error> {
    let opaque = match (has_type_repr(attrs)?, args.opaque) {
        (_, Some(_)) => Ok(true),
        (true, None) => Ok(false),
        (false, None) => Err(ErrorReason::MissingRepr.with_span(Span::call_site())),
    }?;

    Ok(opaque)
}

#[cfg(test)]
mod tests {
    use crate::{
        ffi_type::{has_type_repr, process_ffi_struct},
        Error, ErrorReason,
    };
    use assert2::assert;
    use syn::{parse_quote, ItemStruct};

    #[test]
    fn unknown_arg_to_ffi_type_fails() {
        let res = process_ffi_struct(parse_quote! { foobar }, parse_quote! { struct Foo {} });
        assert!(let Err(Error { reason: ErrorReason::UnknownArg, .. }) = res);
    }

    #[test]
    fn clone_for_type_generic_over_type_fails() {
        let res = process_ffi_struct(
            parse_quote! { clone },
            parse_quote! { struct Foo<T> { x: T } },
        );
        assert!(let Err(Error { reason: ErrorReason::CloneOnGenericType, .. }) = res);
    }

    #[test]
    fn clone_for_type_generic_over_lifetime_fails() {
        let out = process_ffi_struct(
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
        assert!(has_type_repr(&ty.attrs).unwrap());
    }

    #[test]
    fn repr_transparent_can_be_detected() {
        let ty: ItemStruct = parse_quote! {
            #[repr(transparent)]
            struct Foo(i32);
        };
        assert!(has_type_repr(&ty.attrs).unwrap());
    }
}
