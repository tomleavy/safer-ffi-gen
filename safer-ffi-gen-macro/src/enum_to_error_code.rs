use crate::{
    utils::{attr_is, is_cfg, new_ident},
    Error, ErrorReason,
};
use proc_macro2::TokenStream;
use quote::quote;
use syn::{Attribute, ItemEnum};

pub fn impl_enum_to_error_code(enumeration: ItemEnum) -> Result<TokenStream, Error> {
    let cfgs = enumeration
        .attrs
        .iter()
        .filter(|&attr| is_cfg(attr))
        .collect::<Vec<_>>();

    let ty = &enumeration.ident;
    let (impl_generics, ty_generics, where_clause) = enumeration.generics.split_for_impl();
    let discriminant_type = new_ident(format!("{ty}Code"));

    let non_exhaustive = if enumeration.attrs.iter().any(is_non_exhaustive) {
        Some(quote! { #[non_exhaustive] })
    } else {
        None
    };

    let discriminant_variants = enumeration
        .variants
        .iter()
        .enumerate()
        .map(|(i, v)| {
            let i =
                index_to_error_code(i).ok_or_else(|| ErrorReason::TooManyVariants.spanned(ty))?;
            let cfgs = v.attrs.iter().filter(|&attr| is_cfg(attr));
            let variant = &v.ident;

            Ok(quote! {
                #(#cfgs)*
                #variant = #i
            })
        })
        .collect::<Result<Vec<_>, _>>()?;

    let branches = enumeration.variants.iter().map(|v| {
        let cfgs = v.attrs.iter().filter(|&attr| is_cfg(attr));
        let variant = &v.ident;

        quote! {
            #(#cfgs)*
            #ty::#variant { .. } => #discriminant_type::#variant as i32
        }
    });

    Ok(quote! {
        #(#cfgs)*
        impl #impl_generics ::core::convert::From<&#ty #ty_generics> for i32 #where_clause {
            fn from(x: &#ty #ty_generics) -> i32 {
                match *x {
                    #(#branches,)*
                }
            }
        }

        #(#cfgs)*
        #[::safer_ffi_gen::safer_ffi::derive_ReprC]
        #[derive(Clone, Copy, Debug, Eq, PartialEq)]
        #[repr(i32)]
        #non_exhaustive
        pub enum #discriminant_type {
            #(#discriminant_variants,)*
        }
    })
}

fn is_non_exhaustive(attr: &Attribute) -> bool {
    attr_is(attr, "non_exhaustive")
}

fn index_to_error_code(i: usize) -> Option<i32> {
    i32::try_from(i.checked_add(1)?).ok()?.checked_neg()
}
