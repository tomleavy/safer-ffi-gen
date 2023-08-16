use crate::{
    utils::{
        has_only_lifetime_parameters, is_cfg, new_ident, parent_path, specialization_macro_name,
        string_to_path, PathReplacer, TypeCtor, TypeCtorExtractor, TypeReplacer,
    },
    Error, ErrorReason,
};
use heck::ToUpperCamelCase;
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use std::collections::{HashMap, HashSet};
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    visit::Visit,
    Attribute, FnArg, GenericArgument, GenericParam, Generics, Ident, ItemFn, Lifetime, Pat,
    PatIdent, PatType, Path, PathArguments, QSelf, ReturnType, Type, TypeParam, TypePath,
    TypeTuple, Visibility, WherePredicate,
};

const TYPE_ALIAS_PREFIX: &str = "__SaferFfiGenAlias";

/// Macro arguments
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FfiSignatureOptions {
    /// Name to expose the function as for FFI
    ffi_name: Option<Ident>,
}

impl Parse for FfiSignatureOptions {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.is_empty() {
            return Ok(FfiSignatureOptions { ffi_name: None });
        }

        let name = Ident::parse(input)?;

        (name == "ffi_name")
            .then_some(())
            .ok_or_else(|| ErrorReason::UnknownArg.spanned(name))?;

        let _ = syn::token::Eq::parse(input)?;
        let ffi_name = Some(Ident::parse(input)?);
        Ok(FfiSignatureOptions { ffi_name })
    }
}

#[derive(Clone, Debug)]
struct Signature {
    name: Ident,
    is_async: bool,
    generics: Generics,
    params: Vec<(Ident, Type)>,
    return_type: Type,
}

impl Signature {
    fn types_mut(&mut self) -> impl Iterator<Item = &mut Type> {
        self.params
            .iter_mut()
            .map(|(_, ty)| ty)
            .chain([&mut self.return_type])
    }

    fn all_type_ctors(&self) -> Vec<TypeCtor> {
        let mut extractor = TypeCtorExtractor::default();

        self.params
            .iter()
            .map(|(_, ty)| ty)
            .chain([&self.return_type])
            .for_each(|ty| extractor.visit_type(ty));

        let type_params = self
            .generics
            .type_params()
            .map(|p| &p.ident)
            .collect::<HashSet<_>>();

        let mut ctors = extractor.unique_type_ctors();
        ctors.retain(|ctor| {
            let is_type_param = ctor
                .path
                .get_ident()
                .map_or(false, |ctor| type_params.contains(ctor));

            // `Result` must not be aliased as it will be turned into an output parameter
            let is_result = ctor.arity == 2
                && ctor.lifetime_arity == 0
                && ctor
                    .path
                    .segments
                    .last()
                    .map_or(false, |segment| segment.ident == "Result");

            !is_type_param && !is_result
        });
        ctors
    }
}

impl ToTokens for Signature {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        syn::Signature::from(self.clone()).to_tokens(tokens)
    }
}

impl From<Signature> for syn::Signature {
    fn from(sig: Signature) -> Self {
        Self {
            constness: None,
            asyncness: sig.is_async.then(Default::default),
            unsafety: None,
            abi: None,
            fn_token: Default::default(),
            ident: sig.name,
            generics: sig.generics,
            paren_token: Default::default(),
            inputs: sig
                .params
                .into_iter()
                .map(|(ident, ty)| {
                    FnArg::from(PatType {
                        attrs: Vec::new(),
                        pat: Box::new(Pat::Ident(PatIdent {
                            attrs: Vec::new(),
                            by_ref: None,
                            mutability: None,
                            ident,
                            subpat: None,
                        })),
                        colon_token: Default::default(),
                        ty: Box::new(ty),
                    })
                })
                .collect(),
            variadic: None,
            output: into_non_unit_type(sig.return_type).map_or(ReturnType::Default, |ty| {
                ReturnType::Type(Default::default(), Box::new(ty))
            }),
        }
    }
}

impl TryFrom<syn::Signature> for Signature {
    type Error = Error;

    fn try_from(sig: syn::Signature) -> Result<Self, Self::Error> {
        let params = sig
            .inputs
            .into_iter()
            .map(|input| match input {
                FnArg::Typed(PatType { pat, ty, .. }) => match *pat {
                    Pat::Ident(pat) => Ok((pat.ident, *ty)),
                    pat => Err(ErrorReason::UnsupportedParamPattern.spanned(pat)),
                },
                FnArg::Receiver(..) => Err(ErrorReason::UnexpectedReceiverType.spanned(input)),
            })
            .collect::<Result<Vec<_>, _>>()?;

        let return_type = match sig.output {
            ReturnType::Default => unit_type(),
            ReturnType::Type(_, ty) => *ty,
        };

        Ok(Signature {
            name: sig.ident,
            is_async: sig.asyncness.is_some(),
            generics: sig.generics,
            params,
            return_type,
        })
    }
}

impl Parse for Signature {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(syn::Signature::parse(input)?.try_into()?)
    }
}

#[derive(Clone, Debug)]
pub struct Export {
    ffi_name: Ident,
    attrs: Vec<Attribute>,
    visibility: Visibility,
    original_fn_prefix: Option<Path>,
    signature: Signature,
}

impl ToTokens for Export {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let attrs = &self.attrs;
        let visibility = &self.visibility;
        let name = &self.ffi_name;
        let param_names = self
            .signature
            .params
            .iter()
            .map(|(name, _)| name)
            .collect::<Vec<_>>();

        let param_types = self
            .signature
            .params
            .iter()
            .map(|(_, ty)| make_type_ffi_safe(ty.clone()));

        // Map Result type to output parameter and int return type
        let MappedReturnType {
            return_type,
            out_param_type,
        } = MappedReturnType::new(&self.signature.return_type);

        let return_type = into_non_unit_type(return_type).map(|ty| {
            if result_ok_type(&self.signature.return_type).is_some() {
                quote! { -> #ty }
            } else {
                quote! { -> <#ty as ::safer_ffi_gen::FfiType>::Safe }
            }
        });

        let out_param = out_param_type.map(|ty| {
            let name = output_param_name();
            quote! {
                #name: ::safer_ffi_gen::safer_ffi::prelude::Out<'_, <#ty as ::safer_ffi_gen::FfiType>::Safe>
            }
        });

        let (impl_generics, _, where_clause) = self.signature.generics.split_for_impl();
        let original_fn = &self.signature.name;

        let prefix = self
            .original_fn_prefix
            .as_ref()
            .map(|prefix| quote! { #prefix:: });

        let call =
            quote! { #prefix #original_fn(#(::safer_ffi_gen::FfiType::from_safe(#param_names),)*) };
        let call = if self.signature.is_async {
            quote! {
                ::safer_ffi_gen::block_on(#call)
            }
        } else {
            call
        };

        let return_value = return_value_name();
        let return_expression = make_return_expression(&self.signature.return_type);

        quote! {
            #(#attrs)*
            #[::safer_ffi_gen::safer_ffi::ffi_export]
            // Code generated by safer-ffi may trigger this warning
            #[allow(unreachable_code)]
            #visibility fn #name #impl_generics(
                #(#param_names: #param_types,)*
                #out_param
            ) #return_type #where_clause {
                let #return_value = #call;
                #return_expression
            }
        }
        .to_tokens(tokens)
    }
}

/// Build expression to be returned from the FFI wrapper function
fn make_return_expression(return_type: &Type) -> TokenStream {
    let return_value = return_value_name();

    match result_ok_type(return_type) {
        Some(ok_type) => {
            let set_output_param = (!type_is_unit(ok_type)).then(|| {
                let out_param = output_param_name();
                quote! {
                    let ret = ::safer_ffi_gen::FfiType::into_safe(ret);
                    #out_param.write(ret);
                }
            });

            quote! {
                match #return_value {
                    Ok(ret) => {
                        #set_output_param
                        0
                    }
                    Err(e) => {
                        let code = ::safer_ffi_gen::error_code!(e);
                        ::safer_ffi_gen::set_last_error!(e);
                        code
                    }
                }
            }
        }
        None => quote! {
            ::safer_ffi_gen::FfiType::into_safe(#return_value)
        },
    }
}

#[derive(Clone, Debug)]
pub struct Specialize {
    attrs: Vec<Attribute>,
    signature: Signature,
}

impl ToTokens for Specialize {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let attrs = &self.attrs;
        let macro_name = specialization_macro_name(self.signature.name.to_string());
        let types_to_alias = self.signature.all_type_ctors();

        // Alias all types as they may not be accessible from the module where the specialization
        // happens.
        let aliases = types_to_alias
            .iter()
            .enumerate()
            .map(|(i, _)| make_alias(&self.signature.name, i))
            .collect::<Vec<_>>();

        let alias_defs = types_to_alias.iter().zip(&aliases).map(|(target, alias)| {
            let def = define_type_alias(alias, target);
            quote! {
                #(#attrs)*
                #def
            }
        });

        let replacements = types_to_alias
            .iter()
            .zip(&aliases)
            .map(|(target, alias)| (target.path.clone(), alias.clone().into()))
            .collect::<HashMap<_, _>>();

        let mut path_replacer = PathReplacer {
            replacements: &replacements,
        };

        let mut signature = self.signature.clone();

        signature
            .types_mut()
            .for_each(|ty| syn::visit_mut::visit_type_mut(&mut path_replacer, ty));

        quote! {
            #(#alias_defs)*

            #(#attrs)*
            #[macro_export]
            macro_rules! #macro_name {
                ($alias:ident = $target:expr) => {
                    ::safer_ffi_gen::__specialize_function!($alias, $target, #signature);
                }
            }
        }
        .to_tokens(tokens);
    }
}

#[derive(Clone, Debug)]
pub enum FfiSignature {
    Export(Export),
    Specialize(Specialize),
}

impl FfiSignature {
    pub fn new(options: FfiSignatureOptions, function: ItemFn) -> Result<FfiSignature, Error> {
        let has_only_lifetime_params = has_only_lifetime_parameters(&function.sig.generics);
        let attrs = function.attrs.into_iter().filter(is_cfg).collect();
        let signature = function.sig.try_into()?;

        match (has_only_lifetime_params, options.ffi_name) {
            (true, Some(ffi_name)) => Ok(Self::Export(Export {
                ffi_name,
                attrs,
                visibility: function.vis,
                original_fn_prefix: None,
                signature,
            })),
            (false, None) => Ok(Self::Specialize(Specialize { attrs, signature })),
            (true, None) => Err(ErrorReason::MissingFfiName.with_span(Span::call_site())),
            (false, Some(ffi_name)) => Err(ErrorReason::UnexpectedFfiName.spanned(ffi_name)),
        }
    }
}

impl ToTokens for FfiSignature {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            FfiSignature::Export(f) => f.to_tokens(tokens),
            FfiSignature::Specialize(f) => f.to_tokens(tokens),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
struct MappedReturnType {
    return_type: Type,
    out_param_type: Option<Type>,
}

impl MappedReturnType {
    fn new(original_return_type: &Type) -> Self {
        let (out_param_type, return_type) = match result_ok_type(original_return_type) {
            Some(ok_type) => (
                into_non_unit_type(ok_type.clone()),
                Type::Path(TypePath {
                    qself: None,
                    path: string_to_path("::core::ffi::c_int"),
                }),
            ),
            None => (None, original_return_type.clone()),
        };

        Self {
            return_type,
            out_param_type,
        }
    }
}

fn unit_type() -> Type {
    Type::Tuple(TypeTuple {
        paren_token: Default::default(),
        elems: Default::default(),
    })
}

fn result_ok_type(ty: &Type) -> Option<&Type> {
    let Type::Path(path) = ty else {
        return None;
    };

    let result = path
        .path
        .segments
        .last()
        .filter(|segment| segment.ident == "Result")?;

    let PathArguments::AngleBracketed(args) = &result.arguments else {
        return None;
    };

    args.args.iter().find_map(|arg| match arg {
        GenericArgument::Type(ty) => Some(ty),
        _ => None,
    })
}

fn type_is_unit(ty: &Type) -> bool {
    matches!(ty, Type::Tuple(tuple) if tuple.elems.is_empty())
}

fn into_non_unit_type(ty: Type) -> Option<Type> {
    Some(ty).filter(|ty| !type_is_unit(ty))
}

fn make_type_ffi_safe(ty: Type) -> Type {
    const TARGET_PATH: &str = "::safer_ffi_gen::FfiType::Safe";

    Type::Path(TypePath {
        qself: Some(QSelf {
            lt_token: Default::default(),
            ty: Box::new(ty),
            position: TARGET_PATH.split("::").count() - 2,
            as_token: Some(Default::default()),
            gt_token: Default::default(),
        }),
        path: string_to_path(TARGET_PATH),
    })
}

fn output_param_name() -> Ident {
    const OUTPUT_PARAM_NAME: &str = "__safer_ffi_gen_out";
    new_ident(OUTPUT_PARAM_NAME)
}

fn return_value_name() -> Ident {
    const RETURN_VALUE_NAME: &str = "__safer_ffi_gen_ret";
    new_ident(RETURN_VALUE_NAME)
}

fn make_alias(function: &Ident, index: usize) -> Ident {
    let function = function.to_string().to_upper_camel_case();
    new_ident(format!("{TYPE_ALIAS_PREFIX}{function}Alias{index}"))
}

fn make_lifetime(i: usize) -> Lifetime {
    Lifetime::new(&format!("'__safer_ffi_gen_lifetime{i}"), Span::call_site())
}

fn make_type_param(i: usize) -> Ident {
    new_ident(format!("__SaferFfiGenType{i}"))
}

fn define_type_alias(alias: &Ident, target: &TypeCtor) -> TokenStream {
    let lifetime_args = (0..target.lifetime_arity).map(make_lifetime);
    let type_args = (0..target.arity).map(make_type_param);

    let args = lifetime_args
        .map(|arg| quote! { #arg })
        .chain(type_args.map(|arg| quote! { #arg }))
        .collect::<Punctuated<_, syn::token::Comma>>();

    let args = if args.is_empty() {
        None
    } else {
        Some(quote! {
            <#args>
        })
    };

    let target = &target.path;

    quote! {
        pub type #alias #args = #target #args;
    }
}

#[derive(Debug)]
pub struct FunctionSpecialization {
    alias: Ident,
    target: Path,
    signature: Signature,
}

impl Parse for FunctionSpecialization {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let alias = Ident::parse(input)?;
        let _ = syn::token::Comma::parse(input)?;
        let target = Path::parse(input)?;
        let _ = syn::token::Comma::parse(input)?;
        let signature = Signature::parse(input)?;
        Ok(Self {
            alias,
            target,
            signature,
        })
    }
}

impl ToTokens for FunctionSpecialization {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut signature = self.signature.clone();
        let target_parent_path = parent_path(&self.target);

        // All types were aliased and the aliases are defined where the function is defined, so
        // prepend the function path to the aliases
        if let Some(parent) = target_parent_path.clone() {
            let mut prefixer = PathPrefixer { prefix: parent };

            signature
                .types_mut()
                .for_each(|ty| syn::visit_mut::visit_type_mut(&mut prefixer, ty))
        }

        // Get types the function is instantiated with
        let generic_arguments = self
            .target
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
            .cloned()
            .collect::<Vec<_>>();

        // Extract type parameters from function signature
        let (generic_types, other_generic_params) = std::mem::take(&mut signature.generics.params)
            .into_iter()
            .fold(
                (Vec::new(), Punctuated::new()),
                |(mut generic_types, mut other_generic_params), p| {
                    match p {
                        GenericParam::Type(TypeParam { ident, .. }) => {
                            generic_types.push(Type::Path(TypePath {
                                qself: None,
                                path: ident.into(),
                            }))
                        }
                        _ => other_generic_params.push(p),
                    }
                    (generic_types, other_generic_params)
                },
            );

        // Remove type parameters from function signature
        signature.generics.params = other_generic_params;

        // Keep only lifetime constraints in `where` clause
        if let Some(where_clause) = signature.generics.where_clause.as_mut() {
            where_clause.predicates = std::mem::take(&mut where_clause.predicates)
                .into_iter()
                .filter(|predicate| matches!(predicate, WherePredicate::Lifetime(_)))
                .collect();
        }

        // Replace type parameters with type arguments the function is instantiated with
        let mut type_replacer = TypeReplacer {
            replacements: &generic_types.into_iter().zip(generic_arguments).collect(),
        };

        signature.types_mut().for_each(|ty| {
            syn::visit_mut::VisitMut::visit_type_mut(&mut type_replacer, ty);
        });

        let exported = Export {
            ffi_name: self.alias.clone(),
            attrs: Vec::new(),
            visibility: Visibility::Public(Default::default()),
            original_fn_prefix: target_parent_path,
            signature,
        };

        exported.to_tokens(tokens);
    }
}

#[derive(Debug)]
struct PathPrefixer {
    prefix: Path,
}

impl syn::visit_mut::VisitMut for PathPrefixer {
    fn visit_path_mut(&mut self, p: &mut Path) {
        if let Some(segment) = p
            .segments
            .first()
            .filter(|segment| segment.ident.to_string().starts_with(TYPE_ALIAS_PREFIX))
        {
            let mut new_path = self.prefix.clone();
            new_path.segments.push(segment.clone());
            *p = new_path;
        }
        syn::visit_mut::visit_path_mut(self, p);
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ffi_signature::{
            make_alias, output_param_name, result_ok_type, string_to_path, type_is_unit,
            FfiSignature, FfiSignatureOptions, MappedReturnType, PathPrefixer, Signature,
        },
        test_utils::Pretty,
        utils::{new_ident, TypeCtor},
    };
    use assert2::assert;
    use quote::ToTokens;
    use syn::{parse_quote, ItemFn, Type};

    fn signature_options(ffi_name: &str) -> FfiSignatureOptions {
        FfiSignatureOptions {
            ffi_name: Some(new_ident(ffi_name)),
        }
    }

    #[test]
    fn signature_options_can_be_parsed() {
        let actual_options: FfiSignatureOptions = parse_quote! { ffi_name = foo };
        let expected_options = signature_options("foo");
        assert!(actual_options == expected_options);
    }

    #[test]
    fn empty_signature_options_can_be_parsed() {
        let actual_options: FfiSignatureOptions = parse_quote! {};
        let expected_options = FfiSignatureOptions { ffi_name: None };
        assert!(actual_options == expected_options);
    }

    #[test]
    fn unit_type_is_recognized() {
        let ty: Type = parse_quote! { () };
        assert!(type_is_unit(&ty));
    }

    #[test]
    fn non_unit_type_is_recognized() {
        let ty: Type = parse_quote! { Foo };
        assert!(!type_is_unit(&ty));
    }

    #[test]
    fn extracting_ok_type_from_result_works() {
        let expected_ok_type: Type = parse_quote! { Success };
        let result_type: Type = parse_quote! { Result<#expected_ok_type, Failure> };
        let ok_type = result_ok_type(&result_type);
        assert!(ok_type == Some(&expected_ok_type));
    }

    #[test]
    fn extracting_ok_type_from_qualified_result_works() {
        let expected_ok_type: Type = parse_quote! { Success };
        let result_type: Type = parse_quote! { ::core::result::Result<#expected_ok_type, Failure> };
        let ok_type = result_ok_type(&result_type);
        assert!(ok_type == Some(&expected_ok_type));
    }

    #[test]
    fn extracting_ok_type_from_non_result_returns_none() {
        let ty: Type = parse_quote! { Foo };
        assert!(result_ok_type(&ty) == None);
    }

    #[test]
    fn unit_return_type_is_mapped_to_itself() {
        let ty: Type = parse_quote! { () };
        let expected = MappedReturnType {
            return_type: ty.clone(),
            out_param_type: None,
        };
        let actual = MappedReturnType::new(&ty);
        assert!(actual == expected);
    }

    #[test]
    fn non_result_type_is_mapped_to_itself() {
        let ty: Type = parse_quote! { i32 };
        let expected = MappedReturnType {
            return_type: ty.clone(),
            out_param_type: None,
        };
        let actual = MappedReturnType::new(&ty);
        assert!(actual == expected);
    }

    #[test]
    fn result_type_is_mapped_to_output_param_and_error_code() {
        let ty: Type = parse_quote! { Result<Foo, Error> };
        let expected = MappedReturnType {
            return_type: parse_quote! { ::core::ffi::c_int },
            out_param_type: Some(parse_quote! { Foo }),
        };
        let actual = MappedReturnType::new(&ty);
        assert!(actual == expected);
    }

    #[test]
    fn unit_ok_type_is_only_mapped_to_error_code() {
        let ty: Type = parse_quote! { Result<(), Error> };
        let expected = MappedReturnType {
            return_type: parse_quote! { ::core::ffi::c_int },
            out_param_type: None,
        };
        let actual = MappedReturnType::new(&ty);
        assert!(actual == expected);
    }

    #[test]
    fn making_result_an_output_parameter_works() {
        let wrapper = FfiSignature::new(
            signature_options("foo"),
            parse_quote! {
                fn bar() -> Result<Bar, Error> {}
            },
        )
        .unwrap()
        .into_token_stream();

        let output_param = output_param_name();

        let expected_signature: syn::Signature = parse_quote! {
            fn foo(
                #output_param: ::safer_ffi_gen::safer_ffi::prelude::Out<'_, <Bar as ::safer_ffi_gen::FfiType>::Safe>
            ) -> ::core::ffi::c_int
        };

        let actual_signature = syn::parse2::<ItemFn>(wrapper).unwrap().sig;
        assert!(Pretty(actual_signature) == Pretty(expected_signature));
    }

    #[test]
    fn unit_ok_type_does_not_cause_an_output_parameter() {
        let wrapper = FfiSignature::new(
            signature_options("foo"),
            parse_quote! { fn bar() -> Result<(), Error> {} },
        )
        .unwrap()
        .into_token_stream();

        let expected_signature: syn::Signature = parse_quote! { fn foo() -> ::core::ffi::c_int };
        let actual_signature = syn::parse2::<ItemFn>(wrapper).unwrap().sig;
        assert!(Pretty(actual_signature) == Pretty(expected_signature));
    }

    #[test]
    fn signature_type_ctors_can_be_extracted() {
        let signature: Signature = parse_quote! {
            fn foo<'a>(a: Foo, b: &Vec<i32>, c: ::alloc::vec::Vec<u8>, d: Baz<'a>) -> Bar
        };

        let expected = [
            TypeCtor {
                path: string_to_path("::alloc::vec::Vec"),
                lifetime_arity: 0,
                arity: 1,
            },
            TypeCtor {
                path: string_to_path("Bar"),
                lifetime_arity: 0,
                arity: 0,
            },
            TypeCtor {
                path: string_to_path("Baz"),
                lifetime_arity: 1,
                arity: 0,
            },
            TypeCtor {
                path: string_to_path("Foo"),
                lifetime_arity: 0,
                arity: 0,
            },
            TypeCtor {
                path: string_to_path("Vec"),
                lifetime_arity: 0,
                arity: 1,
            },
            TypeCtor {
                path: string_to_path("i32"),
                lifetime_arity: 0,
                arity: 0,
            },
            TypeCtor {
                path: string_to_path("u8"),
                lifetime_arity: 0,
                arity: 0,
            },
        ];

        let actual = signature.all_type_ctors();
        assert!(actual == expected);
    }

    #[test]
    fn extracting_type_ctors_ignores_type_parameters() {
        let signature: Signature = parse_quote! {
            fn foo<T>(a: T, b: Vec<T>)
        };

        let expected = [TypeCtor {
            path: string_to_path("Vec"),
            lifetime_arity: 0,
            arity: 1,
        }];

        let actual = signature.all_type_ctors();
        assert!(actual == expected);
    }

    #[test]
    fn extracting_type_ctors_ignores_result_type() {
        let signature: Signature = parse_quote! {
            fn foo() -> Result<i32, ()>
        };

        let expected = [TypeCtor {
            path: string_to_path("i32"),
            lifetime_arity: 0,
            arity: 0,
        }];

        let actual = signature.all_type_ctors();
        assert!(actual == expected);
    }

    fn prefix_type(prefix: &str, ty: &mut Type) {
        let mut prefixer = PathPrefixer {
            prefix: string_to_path(prefix),
        };
        syn::visit_mut::visit_type_mut(&mut prefixer, ty);
    }

    #[test]
    fn simple_type_can_be_prefixed() {
        let id = make_alias(&new_ident("Baz"), 33);
        let mut actual: Type = parse_quote! { #id };
        prefix_type("foo::bar", &mut actual);
        let expected: Type = parse_quote! { foo::bar::#id };
        assert!(Pretty(actual) == Pretty(expected));
    }

    #[test]
    fn absolute_path_can_be_prepended() {
        let id = make_alias(&new_ident("Baz"), 33);
        let mut actual: Type = parse_quote! { #id };
        prefix_type("::foo::bar", &mut actual);
        let expected: Type = parse_quote! { ::foo::bar::#id };
        assert!(Pretty(actual) == Pretty(expected));
    }

    #[test]
    fn nested_type_can_be_prefixed() {
        let baz = make_alias(&new_ident("alpha"), 33);
        let quux = make_alias(&new_ident("alpha"), 34);
        let mut actual: Type = parse_quote! { #baz<#quux> };
        prefix_type("foo::bar", &mut actual);
        let expected: Type = parse_quote! { foo::bar::#baz<foo::bar::#quux> };
        assert!(Pretty(actual) == Pretty(expected));
    }

    #[test]
    fn only_recognized_identifiers_are_prefixed() {
        let id = make_alias(&new_ident("Bar"), 33);
        let mut actual: Type = parse_quote! { Foo<#id, Baz> };
        prefix_type("foo::bar", &mut actual);
        let expected: Type = parse_quote! { Foo<foo::bar::#id, Baz> };
        assert!(Pretty(actual) == Pretty(expected));
    }
}
