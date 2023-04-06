use alloc::string::{String, ToString};

pub fn set_last_error<E>(_: E)
where
    E: ToString,
{
}

#[safer_ffi::ffi_export]
pub fn last_error() -> safer_ffi::String {
    String::from("Error message is not recorded in no_std").into()
}
