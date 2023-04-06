use std::cell::RefCell;

thread_local! {
    static LAST_ERROR: RefCell<Option<Box<dyn std::error::Error>>> = RefCell::new(None);
}

pub fn set_last_error<E>(e: E)
where
    E: Into<Box<dyn std::error::Error>>,
{
    LAST_ERROR.with(|last_error| *last_error.borrow_mut() = Some(e.into()));
}

#[safer_ffi::ffi_export]
pub fn last_error() -> safer_ffi::String {
    LAST_ERROR
        .with(|e| {
            e.borrow()
                .as_ref()
                .map(ToString::to_string)
                .unwrap_or_default()
        })
        .into()
}
