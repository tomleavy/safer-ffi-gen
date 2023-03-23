use crate::{type_path_last_ident, FfiModule};
use heck::ToSnakeCase;
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    Ident, TypePath,
};

#[derive(Debug)]
pub struct SpecializationDecl {
    alias: Ident,
    target: TypePath,
}

impl Parse for SpecializationDecl {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let alias = Ident::parse(input)?;
        let _ = syn::token::Eq::parse(input)?;
        let target = TypePath::parse(input)?;
        Ok(Self { alias, target })
    }
}

impl ToTokens for SpecializationDecl {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let macro_path = if self.target.path.segments.len() < 2
            || self.target.path.segments[0].ident == "crate"
        {
            specialization_macro_ident(&self.target).into()
        } else {
            syn::Path {
                leading_colon: self.target.path.leading_colon,
                segments: [
                    self.target.path.segments[0].clone(),
                    specialization_macro_ident(&self.target).into(),
                ]
                .into_iter()
                .collect(),
            }
        };

        let alias = &self.alias;
        let target = &self.target;

        quote! {
            #macro_path! { #alias = #target }
        }
        .to_tokens(tokens)
    }
}

pub fn specialization_macro_ident(type_path: &TypePath) -> Ident {
    Ident::new(
        &format!(
            "__safer_ffi_gen_specialize_{}",
            type_path_last_ident(type_path).to_string().to_snake_case(),
        ),
        Span::call_site(),
    )
}

#[derive(Debug)]
pub struct Specialization {
    mini_impl_block: FfiModule,
}

impl Parse for Specialization {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let alias = Ident::parse(input)?;
        let _ = syn::token::Comma::parse(input)?;
        let target = TypePath::parse(input)?;
        let _ = syn::token::Comma::parse(input)?;
        let mut mini_impl_block = FfiModule::parse(input)?;
        mini_impl_block.specialize(&alias, &target);
        Ok(Self { mini_impl_block })
    }
}

impl ToTokens for Specialization {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.mini_impl_block.to_tokens(tokens);
    }
}
