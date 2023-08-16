use crate::utils::{parent_path, specialization_macro_name};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    Ident, Path,
};

#[derive(Debug)]
pub struct Specialization {
    alias: Ident,
    target: Path,
}

impl Parse for Specialization {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let alias = Ident::parse(input)?;
        let _ = syn::token::Eq::parse(input)?;
        let target = Path::parse(input)?;
        Ok(Self { alias, target })
    }
}

impl ToTokens for Specialization {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let prefix = parent_path(&self.target)
            .filter(|p| p.segments.first().unwrap().ident != "crate")
            .map(|p| {
                let leading_colon = p.leading_colon;
                let p = p.segments.first().unwrap();
                quote! { #leading_colon #p:: }
            });

        let macro_id =
            specialization_macro_name(self.target.segments.last().unwrap().ident.to_string());

        let alias = &self.alias;
        let target = &self.target;

        quote! {
            #prefix #macro_id! { #alias = #target }
        }
        .to_tokens(tokens)
    }
}
