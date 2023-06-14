use crate::{
    has_only_lifetime_parameters, specialization::specialization_macro_ident,
    type_path_constructor, type_path_last_ident, Error, ErrorReason, FfiSignature,
};
use heck::ToSnakeCase;
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use std::{collections::HashSet, mem::take};
use syn::{
    parse::{Parse, ParseStream},
    GenericArgument, GenericParam, Generics, Ident, ImplItem, ItemImpl, PathArguments, Signature,
    Type, TypePath,
};

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
                ImplItem::Fn(f) => Some(FfiSignature::parse((*impl_block.self_ty).clone(), f.sig)),
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
        let mut alias_prefix = target.clone();
        alias_prefix.path.segments.pop();

        let alias_mod = export_module_name(target);

        let replacements = type_parameters(&self.type_path)
            .cloned()
            .zip(type_parameters(target).cloned())
            .collect();

        self.functions
            .iter_mut()
            .for_each(|f| f.replace_types(&replacements));

        let alias_replacements = self
            .all_named_types()
            .into_iter()
            .enumerate()
            .map(|(i, ty)| {
                let mut alias_path = alias_prefix.clone();
                alias_path.path.segments.push(alias_mod.clone().into());
                alias_path.path.segments.push(type_alias(i).into());
                (ty, alias_path)
            })
            .collect();

        self.functions
            .iter_mut()
            .for_each(|f| f.replace_type_constructors(&alias_replacements));

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
        let export_module = export_module_name(&self.type_path);
        let exported_types = self.all_exported_types();
        let (aliases, types): (Vec<_>, Vec<_>) = exported_types.iter().map(|(a, t)| (a, t)).unzip();

        quote! {
            pub mod #export_module {
                #(pub use #types as #aliases;)*
            }
        }
        .to_tokens(tokens);

        let macro_name = specialization_macro_ident(&self.type_path);
        let type_path = &self.type_path;

        let functions = self.functions.iter().cloned().map(Signature::from);

        let (impl_generics, _, where_clause) = self.generics.split_for_impl();

        let mini_impl_block = quote! {
            impl #impl_generics #type_path #where_clause {
                #(
                    #functions {}
                )*
            }
        };

        quote! {
            #[macro_export]
            macro_rules! #macro_name {
                ($alias:ident = $ty:ty) => {
                    ::safer_ffi_gen::__specialize!($alias, $ty, #mini_impl_block);
                };
            }
        }
        .to_tokens(tokens)
    }

    fn all_exported_types(&self) -> Vec<(Ident, TypePath)> {
        self.all_named_types()
            .into_iter()
            .enumerate()
            .map(|(i, ty)| (type_alias(i), type_alias_target(ty)))
            .collect()
    }

    fn all_named_types(&self) -> Vec<TypePath> {
        let type_params = type_parameters(&self.type_path)
            .filter_map(|ty| match ty {
                Type::Path(p) => Some(p),
                _ => None,
            })
            .collect::<HashSet<_>>();

        let mut all_named_types = self
            .functions
            .iter()
            .flat_map(|f| f.all_types())
            .flat_map(extract_named_types)
            .filter(|ty| !type_params.contains(ty))
            .filter(type_must_be_exported)
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>();

        all_named_types.sort_by_cached_key(|ty| ty.to_token_stream().to_string());
        all_named_types
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

fn extract_named_types(ty: &Type) -> impl Iterator<Item = TypePath> {
    let mut extractor = TypePathExtractor::default();
    syn::visit::visit_type(&mut extractor, ty);
    extractor.paths.into_iter()
}

#[derive(Debug, Default)]
struct TypePathExtractor {
    paths: Vec<TypePath>,
}

impl<'a> syn::visit::Visit<'a> for TypePathExtractor {
    fn visit_type_path(&mut self, path: &'a TypePath) {
        let p = type_path_constructor(path);
        self.paths.push(p);
        syn::visit::visit_type_path(self, path);
    }
}

fn export_module_name(type_path: &TypePath) -> Ident {
    Ident::new(
        &format!(
            "__safer_ffi_gen_types_{}",
            type_path_last_ident(type_path).to_string().to_snake_case(),
        ),
        Span::call_site(),
    )
}

fn type_must_be_exported(ty: &TypePath) -> bool {
    // More types will need to be added here eventually (in theory,
    // most types in the Rust prelude)
    !ty.path.is_ident("i32")
}

fn type_alias_target(mut ty: TypePath) -> TypePath {
    match &ty.path.leading_colon {
        Some(_) => ty,
        None => match &*ty
            .path
            .segments
            .first()
            .map(|segment| segment.ident.to_string())
            .unwrap_or_default()
        {
            "crate" => ty,
            _ => {
                ty.path
                    .segments
                    .insert(0, Ident::new("super", Span::call_site()).into());
                ty
            }
        },
    }
}

fn type_alias(i: usize) -> Ident {
    Ident::new(&format!("Type{i}"), Span::call_site())
}

#[cfg(test)]
mod tests {
    use crate::ffi_module::extract_named_types;
    use assert2::assert;
    use std::{collections::HashSet, hash::Hash};
    use syn::{parse_quote, TypePath};

    fn make_set<I>(items: I) -> HashSet<I::Item>
    where
        I: IntoIterator,
        I::Item: Eq + Hash,
    {
        items.into_iter().collect()
    }

    #[test]
    fn extracting_types_from_single_type_path_works() {
        let actual = extract_named_types(&parse_quote! { foo::Bar });
        let expected: TypePath = parse_quote! { foo::Bar };
        assert!(make_set(actual) == make_set([expected]));
    }

    #[test]
    fn extracting_types_from_generic_type_works() {
        let actual = extract_named_types(&parse_quote! { Vec<i32> });
        let expected: [TypePath; 2] = [parse_quote! { Vec }, parse_quote! { i32 }];
        assert!(make_set(actual) == make_set(expected));
    }
}
