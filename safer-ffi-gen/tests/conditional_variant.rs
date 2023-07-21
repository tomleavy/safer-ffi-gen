use safer_ffi_gen::ffi_type;

#[ffi_type(opaque)]
pub enum Quux {
    A,
    #[cfg(not(debug_assertions))]
    B,
    #[cfg(debug_assertions)]
    C,
}
