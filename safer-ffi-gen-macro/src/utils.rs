use heck::ToSnakeCase;
use proc_macro2::Span;
use quote::ToTokens;
use std::collections::{HashMap, HashSet};
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    AngleBracketedGenericArguments, Attribute, Expr, ExprPath, GenericArgument, GenericParam,
    Generics, Ident, Lifetime, Path, PathArguments, PathSegment, PredicateType, ReturnType, Type,
    TypeParamBound, TypePath,
};

pub fn has_only_lifetime_parameters(generics: &Generics) -> bool {
    generics
        .params
        .iter()
        .all(|p| matches!(p, GenericParam::Lifetime(_)))
}

pub fn attr_is(attr: &Attribute, id: &str) -> bool {
    attr.path().get_ident().map_or(false, |i| i == id)
}

pub fn is_cfg(attr: &Attribute) -> bool {
    attr_is(attr, "cfg")
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TypeCtor {
    pub path: Path,
    pub lifetime_arity: usize,
    pub arity: usize,
}

impl From<Path> for TypeCtor {
    fn from(mut path: Path) -> Self {
        let (arity, lifetime_arity) = path
            .segments
            .last()
            .and_then(|segment| match &segment.arguments {
                PathArguments::AngleBracketed(args) => Some(&args.args),
                _ => None,
            })
            .into_iter()
            .flatten()
            .fold((0, 0), |(arity, lifetime_arity), arg| {
                (
                    arity + matches!(arg, GenericArgument::Type(_)) as usize,
                    lifetime_arity + matches!(arg, GenericArgument::Lifetime(_)) as usize,
                )
            });

        if let Some(segment) = path.segments.last_mut() {
            segment.arguments = PathArguments::None;
        }

        Self {
            path,
            arity,
            lifetime_arity,
        }
    }
}

impl Parse for TypeCtor {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Path::parse(input).map(Into::into)
    }
}

#[derive(Debug, Default)]
pub struct TypeCtorExtractor {
    ctors: Vec<TypeCtor>,
}

impl TypeCtorExtractor {
    pub fn unique_type_ctors(&self) -> Vec<TypeCtor> {
        let mut ctors = self
            .ctors
            .iter()
            .collect::<HashSet<_>>()
            .into_iter()
            .cloned()
            .collect::<Vec<_>>();

        ctors.sort_by_cached_key(|ty| {
            (
                ty.path.to_token_stream().to_string(),
                ty.arity,
                ty.lifetime_arity,
            )
        });

        ctors
    }
}

impl<'a> syn::visit::Visit<'a> for TypeCtorExtractor {
    fn visit_type_path(&mut self, ty: &'a TypePath) {
        if ty.qself.is_none() {
            self.ctors.push(ty.path.clone().into());
            syn::visit::visit_type_path(self, ty);
        }
    }
}

#[derive(Debug)]
pub struct TypeReplacer<'a> {
    pub replacements: &'a HashMap<Type, Type>,
}

impl syn::visit_mut::VisitMut for TypeReplacer<'_> {
    fn visit_type_mut(&mut self, ty: &mut Type) {
        if let Some(new_ty) = self.replacements.get(ty).cloned() {
            *ty = new_ty;
        }
        syn::visit_mut::visit_type_mut(self, ty);
    }
}

#[derive(Debug)]
pub struct PathReplacer<'a> {
    pub replacements: &'a HashMap<Path, Path>,
}

impl syn::visit_mut::VisitMut for PathReplacer<'_> {
    fn visit_path_mut(&mut self, p: &mut Path) {
        let path_without_args = remove_path_args(p.clone());
        if let Some(mut new_path) = self.replacements.get(&path_without_args).cloned() {
            let args = p
                .segments
                .last_mut()
                .map_or_else(Default::default, |p| std::mem::take(&mut p.arguments));

            if let Some(segment) = new_path.segments.last_mut() {
                segment.arguments = args;
            }

            *p = new_path;
        }
        syn::visit_mut::visit_path_mut(self, p);
    }
}

#[derive(Debug)]
pub struct LifetimeInserter {
    pub lifetime: Lifetime,
}

impl LifetimeInserter {
    pub fn new(lifetime: Lifetime) -> Self {
        Self { lifetime }
    }
}

impl syn::visit_mut::VisitMut for LifetimeInserter {
    fn visit_lifetime_mut(&mut self, lifetime: &mut Lifetime) {
        if is_placeholder_lifetime(lifetime) {
            *lifetime = self.lifetime.clone();
        }
    }

    fn visit_type_reference_mut(&mut self, ty: &mut syn::TypeReference) {
        if ty.lifetime.is_none() {
            ty.lifetime = Some(self.lifetime.clone());
        }
        syn::visit_mut::visit_type_reference_mut(self, ty);
    }
}

#[derive(Debug, Default)]
struct HasImplicitLifetime {
    found: bool,
}

impl<'a> syn::visit::Visit<'a> for HasImplicitLifetime {
    fn visit_lifetime(&mut self, lifetime: &'a Lifetime) {
        self.found = self.found || is_placeholder_lifetime(lifetime);
    }

    fn visit_type_reference(&mut self, ty: &'a syn::TypeReference) {
        self.found = self.found
            || ty
                .lifetime
                .as_ref()
                .filter(|lt| !is_placeholder_lifetime(lt))
                .is_none();

        if !self.found {
            syn::visit::visit_type_reference(self, ty);
        }
    }
}

pub fn type_has_implicit_lifetime(ty: &Type) -> bool {
    let mut finder = HasImplicitLifetime::default();
    syn::visit::visit_type(&mut finder, ty);
    finder.found
}

pub fn return_type_has_implicit_lifetime(ty: &ReturnType) -> bool {
    match ty {
        ReturnType::Default => false,
        ReturnType::Type(_, ty) => type_has_implicit_lifetime(ty),
    }
}

pub fn remove_path_args(mut p: Path) -> Path {
    if let Some(p) = p.segments.last_mut() {
        p.arguments = PathArguments::None;
    }
    p
}

pub fn parent_path(p: &Path) -> Option<Path> {
    let mut p = p.clone();
    p.segments.pop();
    p.segments.pop_punct();
    Some(p).filter(|p| !p.segments.is_empty())
}

pub fn new_ident<S>(s: S) -> Ident
where
    S: AsRef<str>,
{
    Ident::new(s.as_ref(), Span::call_site())
}

pub fn string_to_path(segments: &str) -> syn::Path {
    let s = segments.strip_prefix("::");
    let leading_colon = s.map(|_| Default::default());
    let segments = s.unwrap_or(segments);
    syn::Path {
        leading_colon,
        segments: string_to_path_segments(segments).collect(),
    }
}

fn string_to_path_segments(segments: &str) -> impl Iterator<Item = PathSegment> + '_ {
    segments
        .split("::")
        .map(|s| PathSegment::from(new_ident(s)))
}

pub fn specialization_macro_name<S>(s: S) -> Ident
where
    S: AsRef<str>,
{
    new_ident(format!(
        "__safer_ffi_gen_specialize_{}",
        s.as_ref().to_snake_case(),
    ))
}

pub fn is_placeholder_lifetime(lifetime: &Lifetime) -> bool {
    lifetime.ident == "'_"
}

pub fn generics_to_path_arguments(generics: &Generics) -> PathArguments {
    let args = generics
        .params
        .iter()
        .map(generic_param_to_generic_arg)
        .collect::<Punctuated<_, _>>();
    if args.is_empty() {
        PathArguments::None
    } else {
        PathArguments::AngleBracketed(AngleBracketedGenericArguments {
            colon2_token: None,
            lt_token: Default::default(),
            args,
            gt_token: Default::default(),
        })
    }
}

pub fn generic_param_to_generic_arg(p: &GenericParam) -> GenericArgument {
    match p {
        GenericParam::Const(p) => GenericArgument::Const(Expr::Path(ExprPath {
            attrs: Vec::new(),
            qself: None,
            path: p.ident.clone().into(),
        })),
        GenericParam::Lifetime(p) => GenericArgument::Lifetime(p.lifetime.clone()),
        GenericParam::Type(p) => GenericArgument::Type(Type::Path(TypePath {
            qself: None,
            path: p.ident.clone().into(),
        })),
    }
}

pub fn add_lifetime_constraint_to_type(ty: Type, lifetime: Lifetime) -> PredicateType {
    PredicateType {
        lifetimes: None,
        bounded_ty: ty,
        colon_token: Default::default(),
        bounds: [TypeParamBound::Lifetime(lifetime.clone())]
            .into_iter()
            .collect(),
    }
}

pub fn make_generic_type(name: Ident, generics: &Generics) -> Type {
    TypePath {
        qself: None,
        path: Path {
            leading_colon: None,
            segments: [PathSegment {
                ident: name,
                arguments: generics_to_path_arguments(generics),
            }]
            .into_iter()
            .collect(),
        },
    }
    .into()
}

#[cfg(test)]
mod tests {
    use crate::{
        test_utils::Pretty,
        utils::{
            add_lifetime_constraint_to_type, generic_param_to_generic_arg, is_cfg,
            make_generic_type, string_to_path, TypeCtor, TypeCtorExtractor,
        },
    };
    use assert2::check;
    use syn::{parse_quote, Attribute, GenericArgument, GenericParam, Path, Type, WherePredicate};

    #[test]
    fn is_cfg_returns_true_for_cfg_attribute() {
        let attr: Attribute = parse_quote! { #[cfg(test)] };
        check!(is_cfg(&attr));
    }

    #[test]
    fn is_cfg_returns_false_for_non_cfg_attribute() {
        let attr: Attribute = parse_quote! { #[test] };
        check!(!is_cfg(&attr));
    }

    #[test]
    fn absolute_path_can_be_created_from_string() {
        let actual = string_to_path("::foo::bar::baz");
        let expected: Path = parse_quote! { ::foo::bar::baz };
        check!(Pretty(actual) == Pretty(expected));
    }

    #[test]
    fn relative_path_can_be_created_from_string() {
        let actual = string_to_path("foo::bar::baz");
        let expected: Path = parse_quote! { foo::bar::baz };
        check!(Pretty(actual) == Pretty(expected));
    }

    #[test]
    fn type_constructor_has_arity_zero_when_there_are_no_generic_arguments() {
        let actual: TypeCtor = parse_quote! { Foo };
        let expected = TypeCtor {
            path: parse_quote! { Foo },
            arity: 0,
            lifetime_arity: 0,
        };

        check!(actual == expected);
    }

    #[test]
    fn type_constructor_has_arity_one_when_there_is_one_type_argument() {
        let actual: TypeCtor = parse_quote! { Foo<T> };
        let expected = TypeCtor {
            path: parse_quote! { Foo },
            arity: 1,
            lifetime_arity: 0,
        };

        check!(actual == expected);
    }

    #[test]
    fn type_constructor_has_lifetime_arity_one_when_there_is_one_lifetime_argument() {
        let actual: TypeCtor = parse_quote! { Foo<'a> };
        let expected = TypeCtor {
            path: parse_quote! { Foo },
            arity: 0,
            lifetime_arity: 1,
        };

        check!(actual == expected);
    }

    #[test]
    fn type_constructor_has_correct_arities() {
        let actual: TypeCtor = parse_quote! { Foo<'a, 'b, T, U, V> };
        let expected = TypeCtor {
            path: parse_quote! { Foo },
            arity: 3,
            lifetime_arity: 2,
        };

        check!(actual == expected);
    }

    #[test]
    fn type_ctors_can_be_extracted() {
        let ty: Type = parse_quote! { Foo<bar::Bar, i32, Baz<i32>> };
        let mut extractor = TypeCtorExtractor::default();

        let expected = [
            TypeCtor {
                path: string_to_path("Baz"),
                lifetime_arity: 0,
                arity: 1,
            },
            TypeCtor {
                path: string_to_path("Foo"),
                lifetime_arity: 0,
                arity: 3,
            },
            TypeCtor {
                path: string_to_path("bar::Bar"),
                lifetime_arity: 0,
                arity: 0,
            },
            TypeCtor {
                path: string_to_path("i32"),
                lifetime_arity: 0,
                arity: 0,
            },
        ];

        syn::visit::visit_type(&mut extractor, &ty);
        let actual = extractor.unique_type_ctors();
        check!(actual == expected);
    }

    #[test]
    fn lifetime_param_can_be_mapped_to_arg() {
        let param: GenericParam = parse_quote! { 'a };
        let expected: GenericArgument = parse_quote! { 'a };
        let actual = generic_param_to_generic_arg(&param);
        check!(actual == expected);
    }

    #[test]
    fn type_param_can_be_mapped_to_arg() {
        let param: GenericParam = parse_quote! { T };
        let expected: GenericArgument = parse_quote! { T };
        let actual = generic_param_to_generic_arg(&param);
        check!(actual == expected);
    }

    #[test]
    fn lifetime_constraint_can_be_added_to_type() {
        let expected: WherePredicate = parse_quote! { T: 'a };
        let actual = WherePredicate::Type(add_lifetime_constraint_to_type(
            parse_quote! { T },
            parse_quote! { 'a },
        ));
        check!(actual == expected);
    }

    #[test]
    fn generic_type_can_be_built() {
        let expected: Type = parse_quote! { Foo<'a, T> };
        let actual = make_generic_type(parse_quote! { Foo }, &parse_quote! { <'a, T> });
        check!(actual == expected);
    }
}
