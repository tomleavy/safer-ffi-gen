use crate::{
    has_only_lifetime_parameters, specialization::specialization_macro_ident, Error, ErrorReason,
    FfiSignature,
};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use std::mem::take;
use syn::{
    parse::{Parse, ParseStream},
    Attribute, GenericArgument, GenericParam, Generics, Ident, ImplItem, ImplItemConst,
    ImplItemMacro, ImplItemMethod, ImplItemType, ItemImpl, PathArguments, Signature, Type,
    TypePath,
};

const EXPORT_MARKER: &str = "safer_ffi_gen";

#[derive(Debug)]
pub struct FfiModule {
    type_path: TypePath,
    generics: Generics,
    functions: Vec<FfiSignature>,
}

impl FfiModule {
    pub fn new(impl_block: ItemImpl) -> Result<Self, Error> {
        if let Some((_, trait_, _)) = impl_block.trait_ {
            return Err(ErrorReason::TraitImplBlock.spanned(trait_));
        }

        let Type::Path(path) = &*impl_block.self_ty else {
            return Err(ErrorReason::ImplBlockMustBeForTypePath.spanned(impl_block.self_ty));
        };

        // Find functions
        let functions = impl_block
            .items
            .into_iter()
            .filter_map(|item| match item {
                ImplItem::Method(method) => exported(&method.attrs)
                    .is_some()
                    .then(|| FfiSignature::parse((*impl_block.self_ty).clone(), method.sig)),
                ImplItem::Const(ImplItemConst { attrs, .. })
                | ImplItem::Type(ImplItemType { attrs, .. })
                | ImplItem::Macro(ImplItemMacro { attrs, .. }) => exported(&attrs)
                    .map(|marker| Err(ErrorReason::OnylFunctionsCanBeExported.spanned(marker))),
                _ => None,
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(FfiModule {
            type_path: path.clone(),
            generics: impl_block.generics,
            functions,
        })
    }

    pub fn specialize(&mut self, alias: &Ident, target: &TypePath) {
        let replacements = type_parameters(&self.type_path)
            .cloned()
            .zip(type_parameters(target).cloned())
            .collect();

        self.functions
            .iter_mut()
            .for_each(|f| f.replace_types(&replacements));

        self.type_path = TypePath {
            qself: None,
            path: alias.clone().into(),
        };

        self.generics.params = take(&mut self.generics.params)
            .into_iter()
            .filter(|p| matches!(p, GenericParam::Lifetime(_)))
            .collect();
    }

    fn write_functions(&self, tokens: &mut TokenStream) {
        self.functions.iter().for_each(|f| {
            let mut f = f.clone();
            f.add_generics(&self.generics);
            f.prefix_with_type(&self.type_path);
            f.to_tokens(tokens);
        });
    }

    fn write_specialization_macro(&self, tokens: &mut TokenStream) {
        let macro_name = specialization_macro_ident(&self.type_path);
        let type_path = &self.type_path;
        let functions = self.functions.iter().cloned().map(Signature::from);
        let (impl_generics, _, where_clause) = self.generics.split_for_impl();

        let mini_impl_block = quote! {
            impl #impl_generics  #type_path #where_clause {
                #(
                    #[safer_ffi_gen]
                    #functions {}
                )*
            }
        };

        quote! {
            macro_rules! #macro_name {
                ($alias:ident = $ty:ty) => {
                    ::safer_ffi_gen::__specialize!($alias, $ty, #mini_impl_block);
                };
            }
        }
        .to_tokens(tokens)
    }
}

impl ToTokens for FfiModule {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if has_only_lifetime_parameters(&self.generics) {
            self.write_functions(tokens)
        } else {
            self.write_specialization_macro(tokens)
        }
    }
}

impl Parse for FfiModule {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let impl_block = ItemImpl::parse(input)?;
        FfiModule::new(impl_block).map_err(Into::into)
    }
}

fn exported(attrs: &[Attribute]) -> Option<&Ident> {
    attrs.iter().find_map(|attr| {
        attr.path
            .get_ident()
            .filter(|&ident| ident == EXPORT_MARKER)
    })
}

fn type_parameters(type_path: &TypePath) -> impl Iterator<Item = &Type> {
    type_path
        .path
        .segments
        .last()
        .and_then(|segment| match &segment.arguments {
            PathArguments::AngleBracketed(args) => {
                Some(args.args.iter().filter_map(|arg| match arg {
                    GenericArgument::Type(ty) => Some(ty),
                    _ => None,
                }))
            }
            _ => None,
        })
        .into_iter()
        .flatten()
}

#[derive(Debug)]
pub struct FfiModuleInput {
    impl_block: ItemImpl,
}

impl Parse for FfiModuleInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut impl_block = ItemImpl::parse(input)?;
        impl_block
            .items
            .iter_mut()
            .flat_map(|item| match item {
                ImplItem::Const(ImplItemConst { attrs, .. })
                | ImplItem::Macro(ImplItemMacro { attrs, .. })
                | ImplItem::Method(ImplItemMethod { attrs, .. })
                | ImplItem::Type(ImplItemType { attrs, .. }) => Some(attrs),
                _ => None,
            })
            .for_each(|attrs| {
                attrs.retain(|attr| attr.path.get_ident().map_or(true, |id| id != EXPORT_MARKER))
            });

        Ok(Self { impl_block })
    }
}

impl ToTokens for FfiModuleInput {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.impl_block.to_tokens(tokens);
    }
}
