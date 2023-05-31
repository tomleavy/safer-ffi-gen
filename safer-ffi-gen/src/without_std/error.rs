use alloc::string::{String, ToString};

static mut LAST_ERROR: String = String::new();

pub fn set_last_error<E>(e: E)
where
    E: ToString,
{
    unsafe {
        LAST_ERROR = e.to_string();
    }
}

#[safer_ffi::ffi_export]
pub fn last_error() -> safer_ffi::String {
    unsafe { LAST_ERROR.clone().into() }
}
