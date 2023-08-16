use quote::ToTokens;
use syn::{parse_macro_input, punctuated::Punctuated, Item, Meta, Token};

mod enum_to_error_code;
mod error;
mod ffi_module;
mod ffi_signature;
mod ffi_type;
mod specialization;
#[cfg(test)]
mod test_utils;
mod utils;

use enum_to_error_code::impl_enum_to_error_code;
use error::{Error, ErrorReason};
use ffi_module::{FfiModule, ImplSpecialization};
use ffi_signature::{FfiSignature, FfiSignatureOptions, FunctionSpecialization};
use ffi_type::process_ffi_type;
use specialization::Specialization;

#[proc_macro_attribute]
pub fn safer_ffi_gen(
    attr: proc_macro::TokenStream,
    mut item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = {
        let item = item.clone();
        parse_macro_input!(item as Item)
    };

    let output = match input {
        Item::Fn(f) => {
            let options = parse_macro_input!(attr as FfiSignatureOptions);
            FfiSignature::new(options, f).map(ToTokens::into_token_stream)
        }
        Item::Impl(impl_block) => FfiModule::new(impl_block).map(ToTokens::into_token_stream),
        _ => Err(ErrorReason::UnsupportedItemType.spanned(input)),
    };

    let output = output
        .map_err(Into::into)
        .unwrap_or_else(syn::Error::into_compile_error);

    item.extend([proc_macro::TokenStream::from(output)]);
    item
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
    let decl = parse_macro_input!(args as Specialization);
    decl.to_token_stream().into()
}

#[doc(hidden)]
#[proc_macro]
pub fn __specialize_impl(args: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let specialization = parse_macro_input!(args as ImplSpecialization);
    specialization.to_token_stream().into()
}

#[doc(hidden)]
#[proc_macro]
pub fn __specialize_function(args: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let specialization = parse_macro_input!(args as FunctionSpecialization);
    specialization.to_token_stream().into()
}
