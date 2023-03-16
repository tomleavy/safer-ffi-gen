use proc_macro2::{Ident, Span};
use quote::ToTokens;
use syn::{parse_macro_input, spanned::Spanned, AttributeArgs, GenericParam, Generics, TypePath};

mod error;
mod ffi_module;
mod ffi_signature;
mod ffi_type;
mod specialization;
#[cfg(test)]
mod test_utils;

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
    let mut result = proc_macro2::TokenStream::from(item.clone());

    let output = parse_macro_input!(item as FfiModule);

    // Add the output to the input
    output.to_tokens(&mut result);

    result.into()
}

#[proc_macro_attribute]
// TODO: This isn't necessary, but it is easier for now with debugging to avoid
// having to filter out recursive safer_ffi_gen inside an impl
pub fn safer_ffi_gen_func(
    _attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    item
}

#[proc_macro_attribute]
pub fn ffi_type(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as AttributeArgs);
    let ty_def = parse_macro_input!(input as syn::ItemStruct);

    process_ffi_type(args, ty_def)
        .map(ToTokens::into_token_stream)
        .map_err(Into::into)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
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
