use proc_macro::TokenStream;
use quote::ToTokens;
use safer_ffi_gen_core::FFIModule;

#[proc_macro_attribute]
pub fn safer_ffi_gen(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut item = proc_macro2::TokenStream::from(item);

    let output = syn::parse2::<FFIModule>(item.clone()).unwrap();

    // Add the output to the input
    output.to_tokens(&mut item);

    proc_macro::TokenStream::from(item)
}

#[proc_macro_attribute]
// TODO: This isn't necessary, but it is easier for now with debugging to avoid
// having to filter out recursive safer_ffi_gen inside an impl
pub fn safer_ffi_gen_func(_attr: TokenStream, item: TokenStream) -> TokenStream {
    item
}
