use crate::{
    replace_type_path_constructor, type_path_constructor, type_path_last_ident, Error, ErrorReason,
};
use heck::ToSnakeCase;
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use std::{collections::HashMap, fmt::Display};
use syn::{
    token::SelfType, visit_mut::VisitMut, AngleBracketedGenericArguments, FnArg, GenericArgument,
    GenericParam, Generics, Ident, Lifetime, LifetimeDef, Pat, PatIdent, PatType, PathArguments,
    PathSegment, PredicateLifetime, QSelf, Receiver, ReturnType, Signature, Type, TypePath,
    TypeReference, WhereClause, WherePredicate,
};

#[derive(Clone, Debug)]
pub struct FfiSignature {
    self_type: Type,
    is_async: bool,
    name: Ident,
    lifetime_params: Vec<LifetimeDef>,
    lifetime_predicates: Vec<PredicateLifetime>,
    params: Vec<(Ident, Type)>,
    return_type: ReturnType,
    export_prefix: Option<String>,
}

impl FfiSignature {
    pub fn parse(self_type: Type, signature: Signature) -> Result<FfiSignature, Error> {
        let lifetime_params = signature
            .generics
            .params
            .iter()
            .map(|p| match p {
                GenericParam::Lifetime(l) => Ok(l.clone()),
                _ => Err(ErrorReason::GenericFunction.spanned(p)),
            })
            .collect::<Result<Vec<_>, _>>()?;

        let lifetime_predicates =
            filter_lifetime_predicates(signature.generics.where_clause.as_ref())
                .cloned()
                .collect();

        let params = signature
            .inputs
            .into_iter()
            .map(|input| match input {
                FnArg::Receiver(Receiver {
                    reference,
                    mutability,
                    ..
                }) => Ok((
                    self_param_name(),
                    reference
                        .map(|(and_token, lifetime)| {
                            Type::Reference(TypeReference {
                                and_token,
                                lifetime,
                                mutability,
                                elem: Box::new(self_type.clone()),
                            })
                        })
                        .unwrap_or_else(|| self_type.clone()),
                )),
                FnArg::Typed(PatType { pat, ty, .. }) => match *pat {
                    Pat::Ident(pat) => Ok((pat.ident, *ty)),
                    pat => Err(ErrorReason::UnsupportedParamPattern.spanned(pat)),
                },
            })
            .collect::<Result<Vec<_>, _>>()?;

        let mut sig = FfiSignature {
            self_type: self_type.clone(),
            is_async: signature.asyncness.is_some(),
            name: signature.ident,
            lifetime_params,
            lifetime_predicates,
            params,
            return_type: signature.output,
            export_prefix: None,
        };

        sig.replace_types(&self_type_replacement(self_type));
        Ok(sig)
    }

    pub fn prefix_with_type(&mut self, type_path: &TypePath) {
        self.prefix_name(type_path_last_ident(type_path).to_string().to_snake_case());
    }

    fn prefix_name<T: Display>(&mut self, prefix: T) {
        self.export_prefix = Some(prefix.to_string());
    }

    pub fn add_generics(&mut self, generics: &Generics) {
        self.lifetime_params.extend(generics.lifetimes().cloned());
        self.lifetime_predicates
            .extend(filter_lifetime_predicates(generics.where_clause.as_ref()).cloned());
    }

    pub fn replace_types(&mut self, replacements: &HashMap<Type, Type>) {
        self.edit_types(&mut TypeReplacer { replacements });
    }

    pub fn replace_type_constructors(&mut self, replacements: &HashMap<TypePath, TypePath>) {
        self.edit_types(&mut TypePathReplacer { replacements });
    }

    pub fn all_types(&self) -> impl Iterator<Item = &Type> {
        std::iter::once(&self.self_type)
            .chain(self.params.iter().map(|(_, ty)| ty))
            .chain(match &self.return_type {
                ReturnType::Default => None,
                ReturnType::Type(_, ty) => Some(&**ty),
            })
    }

    pub fn edit_types<V: VisitMut>(&mut self, visitor: &mut V) {
        visitor.visit_type_mut(&mut self.self_type);

        self.params
            .iter_mut()
            .for_each(|(_, ty)| visitor.visit_type_mut(ty));

        if let ReturnType::Type(_, ty) = &mut self.return_type {
            visitor.visit_type_mut(ty);
        }
    }

    fn make_result_output_parameter(&mut self) {
        let ReturnType::Type(arrow, return_type) = &self.return_type else {
            return;
        };

        let Some(ok_type) = result_ok_type(return_type) else {
            return;
        };

        if !type_is_unit(ok_type) {
            self.params
                .push((output_param_name(), wrap_output_param_type(ok_type.clone())));
        }

        self.return_type = ReturnType::Type(
            *arrow,
            Box::new(Type::Path(TypePath {
                qself: None,
                path: string_to_path(true, "std::ffi::c_int"),
            })),
        );
    }

    fn make_types_ffi_safe(&mut self) {
        let output_param = output_param_name();

        self.params
            .iter_mut()
            .filter(|(name, _)| *name != output_param)
            .for_each(|(_, ty)| {
                *ty = make_type_ffi_safe(ty.clone());
            });

        if let ReturnType::Type(_, ty) = &mut self.return_type {
            *ty = Box::new(make_type_ffi_safe((**ty).clone()));
        }
    }

    fn export(&mut self) {
        self.is_async = false;
        self.make_result_output_parameter();
        self.make_types_ffi_safe();
    }
}

impl ToTokens for FfiSignature {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut adapted_sig = self.clone();
        adapted_sig.export();
        let signature = Signature::from(adapted_sig);

        let arg_conversions = self.params.iter().map(|(name, _)| {
            quote! {
                let #name = ::safer_ffi_gen::FfiType::from_safe(#name);
            }
        });

        let fn_name = &self.name;
        let inputs = self.params.iter().map(|(name, _)| name);
        let return_value = return_value_name();
        let self_type = &self.self_type;

        let call = quote! {
            <#self_type>::#fn_name(#(#inputs,)*)
        };

        let call = if self.is_async {
            quote! {
                ::safer_ffi_gen::BLOCKING_ASYNC_RUNTIME.block_on(#call)
            }
        } else {
            call
        };

        let return_expression = match &self.return_type {
            ReturnType::Default => None,
            ReturnType::Type(_, ty) => Some(match result_ok_type(ty) {
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
                                ::safer_ffi_gen::set_last_error(e);
                                -1
                            }
                        }
                    }
                }
                None => quote! {
                    ::safer_ffi_gen::FfiType::into_safe(#return_value)
                },
            }),
        };

        quote! {
            #[::safer_ffi_gen::safer_ffi::ffi_export]
            pub #signature {
                #(#arg_conversions)*

                let #return_value = #call;

                #return_expression
            }
        }
        .to_tokens(tokens);
    }
}

impl From<FfiSignature> for Signature {
    fn from(signature: FfiSignature) -> Self {
        let has_lifetime = !signature.lifetime_params.is_empty();

        Signature {
            constness: None,
            asyncness: signature.is_async.then(Default::default),
            unsafety: None,
            abi: None,
            fn_token: Default::default(),
            ident: Ident::new(
                &format!(
                    "{}{}",
                    signature
                        .export_prefix
                        .as_ref()
                        .map(|p| format!("{p}_"))
                        .unwrap_or_default(),
                    signature.name
                ),
                Span::call_site(),
            ),
            generics: Generics {
                lt_token: has_lifetime.then(Default::default),
                params: signature
                    .lifetime_params
                    .into_iter()
                    .map(GenericParam::Lifetime)
                    .collect(),
                gt_token: has_lifetime.then(Default::default),
                where_clause: (!signature.lifetime_predicates.is_empty()).then(|| WhereClause {
                    where_token: Default::default(),
                    predicates: signature
                        .lifetime_predicates
                        .into_iter()
                        .map(WherePredicate::Lifetime)
                        .collect(),
                }),
            },
            paren_token: Default::default(),
            inputs: signature
                .params
                .into_iter()
                .map(|(name, ty)| {
                    FnArg::Typed(PatType {
                        attrs: Vec::new(),
                        pat: Box::new(
                            PatIdent {
                                attrs: Vec::new(),
                                by_ref: None,
                                mutability: None,
                                ident: name,
                                subpat: None,
                            }
                            .into(),
                        ),
                        colon_token: Default::default(),
                        ty: Box::new(ty),
                    })
                })
                .collect(),
            variadic: None,
            output: signature.return_type,
        }
    }
}

fn filter_lifetime_predicates(
    where_clause: Option<&WhereClause>,
) -> impl Iterator<Item = &PredicateLifetime> {
    where_clause
        .into_iter()
        .flat_map(|w| &w.predicates)
        .filter_map(|pred| match pred {
            WherePredicate::Lifetime(l) => Some(l),
            _ => None,
        })
}

fn literally_self_type() -> Type {
    Type::Path(TypePath {
        qself: None,
        path: SelfType {
            span: Span::call_site(),
        }
        .into(),
    })
}

fn self_type_replacement(self_type: Type) -> HashMap<Type, Type> {
    [(literally_self_type(), self_type)].into_iter().collect()
}

#[derive(Debug)]
struct TypeReplacer<'a> {
    replacements: &'a HashMap<Type, Type>,
}

impl VisitMut for TypeReplacer<'_> {
    fn visit_type_mut(&mut self, ty: &mut Type) {
        if let Some(new_ty) = self.replacements.get(ty).cloned() {
            *ty = new_ty;
        }
        syn::visit_mut::visit_type_mut(self, ty);
    }
}

#[derive(Debug)]
struct TypePathReplacer<'a> {
    replacements: &'a HashMap<TypePath, TypePath>,
}

impl VisitMut for TypePathReplacer<'_> {
    fn visit_type_path_mut(&mut self, ty: &mut TypePath) {
        let ty_ctor = type_path_constructor(ty);
        if let Some(new_ty_ctor) = self.replacements.get(&ty_ctor) {
            replace_type_path_constructor(ty, new_ty_ctor);
        }
        syn::visit_mut::visit_type_path_mut(self, ty);
    }
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

fn make_type_ffi_safe(ty: Type) -> Type {
    const TARGET_PATH: &str = "safer_ffi_gen::FfiType::Safe";

    Type::Path(TypePath {
        qself: Some(QSelf {
            lt_token: Default::default(),
            ty: Box::new(ty),
            position: TARGET_PATH.split("::").count() - 1,
            as_token: Some(Default::default()),
            gt_token: Default::default(),
        }),
        path: string_to_path(true, TARGET_PATH),
    })
}

fn string_to_path(leading_colons: bool, segments: &str) -> syn::Path {
    syn::Path {
        leading_colon: leading_colons.then(Default::default),
        segments: string_to_path_segments(segments).collect(),
    }
}

fn string_to_path_segments(segments: &str) -> impl Iterator<Item = PathSegment> + '_ {
    segments
        .split("::")
        .map(|s| PathSegment::from(Ident::new(s, Span::call_site())))
}

fn wrap_output_param_type(ty: Type) -> Type {
    Type::Path(TypePath {
        qself: None,
        path: syn::Path {
            leading_colon: Some(Default::default()),
            segments: string_to_path_segments("safer_ffi_gen::safer_ffi::prelude")
                .chain(Some(PathSegment {
                    ident: Ident::new("Out", Span::call_site()),
                    arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                        colon2_token: None,
                        lt_token: Default::default(),
                        args: [
                            GenericArgument::Lifetime(Lifetime::new("'_", Span::call_site())),
                            GenericArgument::Type(make_type_ffi_safe(ty)),
                        ]
                        .into_iter()
                        .collect(),
                        gt_token: Default::default(),
                    }),
                }))
                .collect(),
        },
    })
}

fn self_param_name() -> Ident {
    const SELF_PARAM_NAME: &str = "__safer_ffi_gen_self";
    Ident::new(SELF_PARAM_NAME, Span::call_site())
}

fn output_param_name() -> Ident {
    const OUTPUT_PARAM_NAME: &str = "__safer_ffi_gen_out";
    Ident::new(OUTPUT_PARAM_NAME, Span::call_site())
}

fn return_value_name() -> Ident {
    const RETURN_VALUE_NAME: &str = "__safer_ffi_gen_ret";
    Ident::new(RETURN_VALUE_NAME, Span::call_site())
}

#[cfg(test)]
mod tests {
    use crate::{
        ffi_signature::{
            output_param_name, result_ok_type, type_is_unit, FfiSignature, TypeReplacer,
        },
        test_utils::Pretty,
    };
    use assert2::assert;
    use std::collections::HashSet;
    use syn::{parse_quote, visit_mut::VisitMut, Signature, Type};

    fn replace_types(mut ty: Type, replacements: &[(Type, Type)]) -> Type {
        TypeReplacer {
            replacements: &replacements.iter().cloned().collect(),
        }
        .visit_type_mut(&mut ty);
        ty
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
        let result_type: Type = parse_quote! { ::std::result::Result<#expected_ok_type, Failure> };
        let ok_type = result_ok_type(&result_type);
        assert!(ok_type == Some(&expected_ok_type));
    }

    #[test]
    fn extracting_ok_type_from_non_result_returns_none() {
        let ty: Type = parse_quote! { Foo };
        assert!(result_ok_type(&ty) == None);
    }

    #[test]
    fn replacing_single_type_works() {
        let old_ty: Type = parse_quote! { Foo };
        let new_ty: Type = parse_quote! { Bar };
        let ty = replace_types(old_ty.clone(), &[(old_ty, new_ty.clone())]);
        assert!(ty == new_ty);
    }

    #[test]
    fn replacing_multiple_types_works() {
        let (foo, bar): (Type, Type) = (parse_quote! { Foo }, parse_quote! { Bar });
        let (baz, quux): (Type, Type) = (parse_quote! { Baz }, parse_quote! { Quux });
        let ty: Type = parse_quote! { (#foo, #bar) };
        let expected: Type = parse_quote! { (#baz, #quux) };
        let ty = replace_types(ty, &[(foo, baz), (bar, quux)]);
        assert!(ty == expected);
    }

    #[test]
    fn replacing_type_in_type_param_works() {
        let old_type: Type = parse_quote! { Foo };
        let new_type: Type = parse_quote! { Bar };
        let ty: Type = parse_quote! { Vec<Foo> };
        let expected: Type = parse_quote! { Vec<Bar> };
        let ty = replace_types(ty, &[(old_type, new_type)]);
        assert!(ty == expected);
    }

    #[test]
    fn prefixing_function_name_works() {
        let mut signature =
            FfiSignature::parse(parse_quote! { FooBar }, parse_quote! { fn baz() }).unwrap();

        signature.prefix_with_type(&parse_quote! { ::test::FooBar });
        let expected_signature: Signature = parse_quote! { fn foo_bar_baz() };
        let actual_signature = Signature::from(signature);
        assert!(actual_signature == expected_signature);
    }

    #[test]
    fn making_result_an_output_parameter_works() {
        let mut signature = FfiSignature::parse(
            parse_quote! { Foo },
            parse_quote! { fn bar() -> Result<Bar, Error> },
        )
        .unwrap();

        signature.make_result_output_parameter();
        let output_param = output_param_name();

        let expected_signature: Signature = parse_quote! {
            fn bar(
                #output_param: ::safer_ffi_gen::safer_ffi::prelude::Out<'_, <Bar as ::safer_ffi_gen::FfiType>::Safe>
            ) -> ::std::ffi::c_int
        };

        let actual_signature = Signature::from(signature);
        assert!(Pretty(actual_signature) == Pretty(expected_signature));
    }

    #[test]
    fn unit_ok_type_does_not_cause_an_output_parameter() {
        let mut signature = FfiSignature::parse(
            parse_quote! { Foo },
            parse_quote! { fn bar() -> Result<(), Error> },
        )
        .unwrap();

        signature.make_result_output_parameter();
        let expected_signature: Signature = parse_quote! { fn bar() -> ::std::ffi::c_int };
        let actual_signature = Signature::from(signature);
        assert!(actual_signature == expected_signature);
    }

    #[test]
    fn extracting_all_types_works() {
        let signature = FfiSignature::parse(
            parse_quote! { Foo },
            parse_quote! { fn bar(x: Bar) -> Result<i32, ()> },
        )
        .unwrap();

        let expected_types: HashSet<Type> = [
            parse_quote! { Foo },
            parse_quote! { Bar },
            parse_quote! { Result<i32, ()> },
        ]
        .into_iter()
        .collect();

        let actual_types = signature.all_types().cloned().collect::<HashSet<_>>();
        assert!(actual_types == expected_types);
    }
}
