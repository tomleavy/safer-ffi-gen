use proc_macro2::{Ident, Span};
use quote::ToTokens;
use syn::{
    parse_macro_input, punctuated::Punctuated, spanned::Spanned, GenericParam, Generics, Meta,
    PathArguments, Token, TypePath,
};

mod enum_to_error_code;
mod error;
mod ffi_module;
mod ffi_signature;
mod ffi_type;
mod specialization;
#[cfg(test)]
mod test_utils;

use enum_to_error_code::impl_enum_to_error_code;
use error::{Error, ErrorReason};
use ffi_module::FfiModule;
use ffi_signature::FfiSignature;
use ffi_type::process_ffi_type;
use specialization::{Specialization, SpecializationDecl};

#[proc_macro_attribute]
pub fn safer_ffi_gen(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut input = item.clone();
    let output = parse_macro_input!(item as FfiModule);
    input.extend([proc_macro::TokenStream::from(output.to_token_stream())]);
    input
}

/// Marker to ignore functions in `impl` block
#[proc_macro_attribute]
pub fn safer_ffi_gen_ignore(
    _: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
pub fn ffi_type(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args with Punctuated::<Meta, Token![,]>::parse_terminated);
    let ty_def = parse_macro_input!(input as syn::Item);

    process_ffi_type(args, ty_def)
        .map(ToTokens::into_token_stream)
        .map_err(Into::into)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro_attribute]
pub fn enum_to_error_code(
    _: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut input = item.clone();
    let enum_def = parse_macro_input!(item as syn::ItemEnum);
    let generated = impl_enum_to_error_code(enum_def)
        .map_err(Into::into)
        .unwrap_or_else(syn::Error::into_compile_error);
    input.extend(proc_macro::TokenStream::from(generated));
    input
}

#[proc_macro]
pub fn specialize(args: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let decl = parse_macro_input!(args as SpecializationDecl);
    decl.to_token_stream().into()
}

#[doc(hidden)]
#[proc_macro]
pub fn __specialize(args: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let specialization = parse_macro_input!(args as Specialization);
    specialization.to_token_stream().into()
}

fn type_path_last_ident(p: &TypePath) -> &Ident {
    &p.path
        .segments
        .last()
        .expect("Path has at least one segment")
        .ident
}

fn has_only_lifetime_parameters(generics: &Generics) -> bool {
    non_lifetime_parameter(generics).is_none()
}

fn non_lifetime_parameter(generics: &Generics) -> Option<Span> {
    generics.params.iter().find_map(|p| match p {
        GenericParam::Const(p) => Some(p.span()),
        GenericParam::Type(p) => Some(p.span()),
        GenericParam::Lifetime(_) => None,
    })
}

fn type_path_constructor(path: &TypePath) -> TypePath {
    let mut p = match &path.qself {
        Some(qself) => TypePath {
            qself: None,
            path: syn::Path {
                leading_colon: path.path.leading_colon,
                segments: path
                    .path
                    .segments
                    .iter()
                    .take(qself.position)
                    .cloned()
                    .collect(),
            },
        },
        None => path.clone(),
    };

    if let Some(segment) = p.path.segments.last_mut() {
        segment.arguments = PathArguments::None;
    }

    p
}

fn replace_type_path_constructor(path: &mut TypePath, replacement: &TypePath) {
    let ctor_end = match &mut path.qself {
        Some(qself) => {
            let end = qself.position;
            qself.position = replacement.path.segments.len();
            end
        }
        None => path.path.segments.len(),
    };
    let args = path.path.segments[ctor_end - 1].arguments.clone();
    let tail = path
        .path
        .segments
        .iter()
        .skip(ctor_end)
        .cloned()
        .collect::<Vec<_>>();
    path.path.leading_colon = replacement.path.leading_colon;
    path.path.segments = replacement.path.segments.iter().cloned().collect();
    path.path.segments.last_mut().unwrap().arguments = args;
    path.path.segments.extend(tail);
}

#[cfg(test)]
mod tests {
    use crate::{replace_type_path_constructor, type_path_constructor};
    use assert2::assert;
    use syn::{parse_quote, TypePath};

    #[test]
    fn type_path_constructor_works_for_simple_type() {
        let expected: TypePath = parse_quote! { Foo };
        let actual = type_path_constructor(&parse_quote! { Foo });
        assert!(actual == expected);
    }

    #[test]
    fn type_path_constructor_works_for_generic_type() {
        let expected: TypePath = parse_quote! { Foo };
        let actual = type_path_constructor(&parse_quote! { Foo<T> });
        assert!(actual == expected);
    }

    #[test]
    fn type_path_constructor_works_for_associated_type() {
        let expected: TypePath = parse_quote! { bar::Bar };
        let actual = type_path_constructor(&parse_quote! { <Foo as bar::Bar>::Output });
        assert!(actual == expected);
    }

    #[test]
    fn type_path_constructor_preserves_module() {
        let expected: TypePath = parse_quote! { foo::Foo };
        let actual = type_path_constructor(&parse_quote! { foo::Foo });
        assert!(actual == expected);
    }

    #[test]
    fn replacing_type_path_constructor_works() {
        let mut ty: TypePath = parse_quote! { Foo };
        replace_type_path_constructor(&mut ty, &parse_quote! { Bar });
        let expected = parse_quote! { Bar };
        assert!(ty == expected);
    }

    #[test]
    fn replacing_type_path_constructor_works_for_generic_type() {
        let mut ty: TypePath = parse_quote! { Foo<T> };
        replace_type_path_constructor(&mut ty, &parse_quote! { Bar });
        let expected = parse_quote! { Bar<T> };
        assert!(ty == expected);
    }

    #[test]
    fn replacing_type_path_constructor_works_for_associated_type() {
        let mut ty: TypePath = parse_quote! { <Foo as bar::Bar>::Output };
        replace_type_path_constructor(&mut ty, &parse_quote! { baz::Baz });
        let expected = parse_quote! { <Foo as baz::Baz>::Output };
        assert!(ty == expected);
    }
}
