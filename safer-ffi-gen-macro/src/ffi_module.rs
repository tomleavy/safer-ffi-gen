use crate::{
    utils::{
        has_only_lifetime_parameters, is_cfg, is_placeholder_lifetime, new_ident, parent_path,
        return_type_has_implicit_lifetime, specialization_macro_name, string_to_path,
        LifetimeInserter, TypeReplacer,
    },
    Error, ErrorReason,
};
use heck::ToSnakeCase;
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Attribute, FnArg, GenericParam, Generics, Ident, ImplItem, ImplItemFn, ItemImpl, Lifetime,
    LifetimeParam, Pat, PatIdent, PatType, Path, Receiver, Signature, Type, TypePath,
    TypeReference, Visibility, WhereClause,
};

#[derive(Debug)]
pub struct FfiModule {
    self_type: TypePath,
    generics: Generics,
    functions: Vec<ImplItemFn>,
}

impl FfiModule {
    pub fn new(impl_block: ItemImpl) -> Result<Self, Error> {
        // Trait impl blocks are not supported
        if let Some((_, trait_, _)) = impl_block.trait_ {
            return Err(ErrorReason::TraitImplBlock.spanned(trait_));
        }

        // Only type paths are supported
        let Type::Path(self_type) = *impl_block.self_ty else {
            return Err(ErrorReason::ImplBlockMustBeForTypePath.spanned(impl_block.self_ty));
        };

        // Find public functions not explicitly ignored
        let functions = impl_block
            .items
            .into_iter()
            .filter_map(|item| match item {
                ImplItem::Fn(f) => {
                    let exported = matches!(f.vis, Visibility::Public(_))
                        && !f.attrs.iter().any(|attr| {
                            attr.to_token_stream()
                                .to_string()
                                .contains("safer_ffi_gen_ignore")
                        });

                    exported.then_some(f)
                }
                _ => None,
            })
            .collect::<Vec<_>>();

        Ok(FfiModule {
            self_type,
            generics: impl_block.generics,
            functions,
        })
    }
}

impl Parse for FfiModule {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let impl_block = ItemImpl::parse(input)?;
        FfiModule::new(impl_block).map_err(Into::into)
    }
}

impl ToTokens for FfiModule {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let type_prefix = type_to_string(&self.self_type);

        let free_functions = self
            .functions
            .iter()
            .map(|f| {
                let mut free_function = into_free_function(
                    f.sig.clone(),
                    Type::Path(self.self_type.clone()),
                    self.generics.clone(),
                );

                let call = forward_call(&free_function, &Type::Path(self.self_type.clone()));
                let new_name = free_function_wrapper_name(&self.self_type, &f.sig.ident);
                free_function.ident = new_name;

                // If the function is generic only over lifetimes, it can be exposed through FFI.
                // If not, it will need to be specialized, i.e. instantiated with concrete types.
                let ffi_attribute = if has_only_lifetime_parameters(&free_function.generics) {
                    let ffi_name = new_ident(format!("{type_prefix}_{}", f.sig.ident));
                    quote! {
                        #[::safer_ffi_gen::safer_ffi_gen(ffi_name = #ffi_name)]
                    }
                } else {
                    quote! {
                        #[::safer_ffi_gen::safer_ffi_gen]
                    }
                };

                (ffi_attribute, free_function, call)
            })
            .collect::<Vec<_>>();

        // Attributes on the original method
        let attrs = self.functions.iter().map(|f| &f.attrs);

        // FFI attribute added by this code
        let ffi_attributes = free_functions.iter().map(|(attr, ..)| attr);

        let signatures = free_functions
            .iter()
            .map(|(_, sig, ..)| sig)
            .collect::<Vec<_>>();

        let calls = free_functions.iter().map(|(_, _, call)| call);

        quote! {
            #(
                #(#attrs)*
                #ffi_attributes
                pub #signatures {
                    #calls
                }
            )*
        }
        .to_tokens(tokens);

        // Generate specialization macro if `Self` is generic over types
        if !has_only_lifetime_parameters(&self.generics) {
            let macro_name = specialization_macro_name(&type_prefix);

            let functions = self
                .functions
                .iter()
                .filter(|f| has_only_lifetime_parameters(&f.sig.generics))
                .map(|f| FunctionToSpecialize {
                    attributes: f
                        .attrs
                        .iter()
                        .filter(|attr| is_cfg(attr))
                        .cloned()
                        .collect(),
                    name: f.sig.ident.clone(),
                });

            quote! {
                #[macro_export]
                macro_rules! #macro_name {
                    ($alias:ident = $ty:ty) => {
                        ::safer_ffi_gen::__specialize_impl!($alias, $ty, #(#functions,)*);
                    };
                }
            }
            .to_tokens(tokens)
        }
    }
}

/// Convert method into free function
fn into_free_function(
    mut signature: Signature,
    self_type: Type,
    mut generics: Generics,
) -> Signature {
    // Handle lifetime elision rules that allow implicitly eliding lifetimes in return type if they
    // are tied to `self`'s lifetime.
    if return_type_has_implicit_lifetime(&signature.output) {
        let self_lifetime = signature.inputs.iter_mut().find_map(|input| {
            let FnArg::Receiver(Receiver { ty, .. }) = input else {
                return None;
            };

            let Type::Reference(TypeReference { lifetime, .. }) = &mut **ty else {
                return None;
            };

            if lifetime
                .as_ref()
                .filter(|&lt| !is_placeholder_lifetime(lt))
                .is_none()
            {
                // Set an explicit lifetime if one isn't used
                *lifetime = Some(make_self_lifetime());
            }

            lifetime.clone()
        });

        if let Some(self_lifetime) = self_lifetime {
            if self_lifetime == make_self_lifetime() {
                // If the lifetime was generated by this code, it needs to be declared as a generic
                // parameter
                signature
                    .generics
                    .params
                    .push(GenericParam::Lifetime(LifetimeParam::new(
                        self_lifetime.clone(),
                    )));
            }

            // Insert `self` lifetime wherever there's an implicit lifetime in the return type
            syn::visit_mut::visit_return_type_mut(
                &mut LifetimeInserter::new(self_lifetime),
                &mut signature.output,
            );
        }
    }

    // Convert `self` parameter to regular parameter
    signature.inputs.iter_mut().for_each(|input| match input {
        FnArg::Receiver(Receiver { attrs, ty, .. }) => {
            *input = FnArg::Typed(PatType {
                attrs: std::mem::take(attrs),
                pat: Box::new(Pat::Ident(PatIdent {
                    attrs: Vec::new(),
                    by_ref: None,
                    mutability: None,
                    ident: self_param_name(),
                    subpat: None,
                })),
                colon_token: Default::default(),
                ty: ty.clone(),
            });
        }
        FnArg::Typed(p) => {
            if let Pat::Ident(p) = &mut *p.pat {
                // If the parameter is `mut p: T`, remove `mut` to avoid warning as the parameter is
                // only forwarded to the original method
                if p.mutability.is_some() && p.by_ref.is_none() {
                    p.mutability = None;
                }
            }
        }
    });

    // Merge `Self` and method generic parameters
    generics.params.extend(signature.generics.params);
    signature.generics.params = generics.params;

    if !signature.generics.params.is_empty() {
        signature.generics.lt_token = Some(Default::default());
        signature.generics.gt_token = Some(Default::default());
    }

    let mut where_clause = generics.where_clause.unwrap_or(WhereClause {
        where_token: Default::default(),
        predicates: Default::default(),
    });

    // Merge `where` clauses
    where_clause.predicates.extend(
        signature
            .generics
            .where_clause
            .map(|clause| clause.predicates)
            .unwrap_or_default(),
    );

    signature.generics.where_clause =
        Some(where_clause).filter(|clause| !clause.predicates.is_empty());

    // Replace `Self` type with actual type
    let mut replacer = TypeReplacer {
        replacements: &[(literal_self_type(), self_type)].into_iter().collect(),
    };

    syn::visit_mut::visit_signature_mut(&mut replacer, &mut signature);
    signature
}

fn make_self_lifetime() -> Lifetime {
    Lifetime::new("'__safer_ffi_gen_self", Span::call_site())
}

/// Return code to forward free function call to original method
fn forward_call(signature: &Signature, self_type: &Type) -> TokenStream {
    let function = &signature.ident;

    let params = signature.inputs.iter().filter_map(|input| match input {
        FnArg::Typed(p) => match &*p.pat {
            Pat::Ident(p) => Some(&p.ident),
            _ => None,
        },
        _ => None,
    });

    let await_suffix = signature.asyncness.is_some().then(|| quote! { .await });

    quote! {
        <#self_type>::#function(#(#params),*) #await_suffix
    }
}

fn literal_self_type() -> Type {
    Type::Path(TypePath {
        qself: None,
        path: string_to_path("Self"),
    })
}

fn self_param_name() -> Ident {
    new_ident("__safer_ffi_gen_self")
}

fn free_function_wrapper_name(self_type: &TypePath, function: &Ident) -> Ident {
    let type_prefix = type_to_string(self_type);
    new_ident(format!("ffi_{type_prefix}_{function}"))
}

fn type_to_string(ty: &TypePath) -> String {
    ty.path
        .segments
        .last()
        .unwrap()
        .ident
        .to_string()
        .to_snake_case()
}

#[derive(Debug)]
pub struct ImplSpecialization {
    alias: Ident,
    target: Path,
    functions: Vec<FunctionToSpecialize>,
}

impl Parse for ImplSpecialization {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let alias = Ident::parse(input)?;
        let _ = syn::token::Comma::parse(input)?;
        let target = Path::parse(input)?;
        let _ = syn::token::Comma::parse(input)?;
        let functions = Punctuated::<_, syn::token::Comma>::parse_terminated(input)?
            .into_iter()
            .collect();
        Ok(Self {
            alias,
            target,
            functions,
        })
    }
}

impl ToTokens for ImplSpecialization {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let target_parent = parent_path(&self.target);
        let target_prefix = target_parent.as_ref().map(|p| quote! { #p:: });

        // Macros are found at the crate root, with the exception of macro-expanded macros in the
        // current crate
        let macro_prefix = target_parent
            .as_ref()
            .filter(|p| p.segments.first().unwrap().ident != "crate")
            .map(|p| {
                let leading_colon = &p.leading_colon;
                let p = p.segments.first().unwrap();
                quote! { #leading_colon #p:: }
            });

        let self_type = TypePath {
            qself: None,
            path: self.target.segments.last().unwrap().ident.clone().into(),
        };

        // Generate calls to each free function specialization macro
        let macro_calls = self.functions.iter().map(|function| {
            let f = &function.name;
            let free_function_wrapper = free_function_wrapper_name(&self_type, f);
            let macro_id = specialization_macro_name(free_function_wrapper.to_string());

            let function_alias =
                new_ident(format!("{}_{f}", self.alias.to_string().to_snake_case()));

            let generic_params = &self.target.segments.last().unwrap().arguments;
            let attrs = &function.attributes;

            quote! {
                #(#attrs)*
                #macro_prefix #macro_id! {
                    #function_alias = #target_prefix #free_function_wrapper :: #generic_params
                }
            }
        });

        quote! { #(#macro_calls)* }.to_tokens(tokens);
    }
}

#[derive(Clone, Debug)]
struct FunctionToSpecialize {
    attributes: Vec<Attribute>,
    name: Ident,
}

impl ToTokens for FunctionToSpecialize {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let attributes = &self.attributes;
        let name = &self.name;

        quote! {
            #(#attributes)* #name
        }
        .to_tokens(tokens);
    }
}

impl Parse for FunctionToSpecialize {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attributes = Attribute::parse_outer(input)?;
        let name = Ident::parse(input)?;
        Ok(Self { attributes, name })
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ffi_module::{forward_call, into_free_function, self_param_name, FfiModule},
        test_utils::Pretty,
    };
    use assert2::assert;
    use quote::quote;
    use syn::{parse_quote, Generics, Signature};

    fn extract_generics(signature: Signature) -> Generics {
        signature.generics
    }

    #[test]
    fn associated_function_can_be_converted_to_free_function() {
        let original: Signature = parse_quote! { fn foo(i: i32) };
        let actual = into_free_function(original, parse_quote! { Foo }, Default::default());
        let expected: Signature = parse_quote! { fn foo(i: i32) };
        assert!(Pretty(actual) == Pretty(expected));
    }

    #[test]
    fn self_method_can_be_converted_to_free_function() {
        let original: Signature = parse_quote! { fn foo(self, i: i32) };
        let actual = into_free_function(original, parse_quote! { Foo }, Default::default());
        let this = self_param_name();
        let expected: Signature = parse_quote! { fn foo(#this: Foo, i: i32) };
        assert!(Pretty(actual) == Pretty(expected));
    }

    #[test]
    fn by_ref_self_method_can_be_converted_to_free_function() {
        let original: Signature = parse_quote! { fn foo(&self, i: i32) };
        let actual = into_free_function(original, parse_quote! { Foo }, Default::default());
        let this = self_param_name();
        let expected: Signature = parse_quote! { fn foo(#this: &Foo, i: i32) };
        assert!(Pretty(actual) == Pretty(expected));
    }

    #[test]
    fn by_ref_mut_self_method_can_be_converted_to_free_function() {
        let original: Signature = parse_quote! { fn foo(&mut self, i: i32) };
        let actual = into_free_function(original, parse_quote! { Foo }, Default::default());
        let this = self_param_name();
        let expected: Signature = parse_quote! { fn foo(#this: &mut Foo, i: i32) };
        assert!(Pretty(actual) == Pretty(expected));
    }

    #[test]
    fn method_using_self_type_can_be_converted_to_free_function() {
        let original: Signature = parse_quote! { fn foo(self, other: Self) };
        let actual = into_free_function(original, parse_quote! { Foo }, Default::default());
        let this = self_param_name();
        let expected: Signature = parse_quote! { fn foo(#this: Foo, other: Foo) };
        assert!(Pretty(actual) == Pretty(expected));
    }

    #[test]
    fn method_using_self_type_in_other_type_can_be_converted_to_free_function() {
        let original: Signature = parse_quote! { fn foo(self, others: Vec<Self>) };
        let actual = into_free_function(original, parse_quote! { Foo }, Default::default());
        let this = self_param_name();
        let expected: Signature = parse_quote! { fn foo(#this: Foo, others: Vec<Foo>) };
        assert!(Pretty(actual) == Pretty(expected));
    }

    #[test]
    fn self_in_where_clause_is_replaced_when_converting_to_free_function() {
        let original: Signature = parse_quote! { fn foo() where Self: Sized };
        let actual = into_free_function(original, parse_quote! { Foo }, Default::default());
        let expected: Signature = parse_quote! { fn foo() where Foo: Sized };
        assert!(Pretty(actual) == Pretty(expected));
    }

    #[test]
    fn self_type_generics_are_added_to_method_converted_to_free_function() {
        let original: Signature = parse_quote! { fn foo() where Self: Sized };
        let actual = into_free_function(
            original,
            parse_quote! { Foo<T> },
            extract_generics(parse_quote! { fn f<T>() where T: Clone }),
        );
        let expected: Signature = parse_quote! { fn foo<T>() where T: Clone, Foo<T>: Sized };
        assert!(Pretty(actual) == Pretty(expected));
    }

    #[test]
    fn call_without_parameters_can_be_forwarded() {
        let actual = forward_call(&parse_quote! { fn foo() }, &parse_quote! { Foo });
        let expected = quote! { <Foo>::foo() };
        assert!(actual.to_string() == expected.to_string());
    }

    #[test]
    fn call_with_parameters_can_be_forwarded() {
        let actual = forward_call(
            &parse_quote! { fn foo(this: &Foo, i: i32) },
            &parse_quote! { Foo },
        );
        let expected = quote! { <Foo>::foo(this, i) };
        assert!(actual.to_string() == expected.to_string());
    }

    #[test]
    fn only_pub_functions_are_considered() {
        let module = FfiModule::new(parse_quote! {
            impl Foo {
                pub fn foo() {}
                pub(crate) fn bar() {}
                fn baz() {}
            }
        })
        .unwrap();

        assert!(module.functions.len() == 1);
        assert!(module.functions[0].sig.ident == "foo");
    }

    #[test]
    fn functions_can_be_ignored() {
        let module = FfiModule::new(parse_quote! {
            impl Foo {
                pub fn foo() {}

                #[safer_ffi_gen_ignore]
                pub fn bar() {}

                #[cfg_attr(feature = "ffi", safer_ffi_gen_ignore)]
                pub fn baz() {}
            }
        })
        .unwrap();

        assert!(module.functions.len() == 1);
        assert!(module.functions[0].sig.ident == "foo");
    }
}
