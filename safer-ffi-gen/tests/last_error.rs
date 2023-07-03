use safer_ffi_gen::{ffi_type, safer_ffi_gen};
use std::fmt::{self, Display};

#[ffi_type(opaque)]
pub struct Foo {
    _i: i32,
}

#[safer_ffi_gen]
impl Foo {
    pub fn new(i: i32) -> Foo {
        Self { _i: i }
    }

    pub fn fails(&self) -> Result<(), SomeError> {
        Err(SomeError)
    }

    pub fn fails_too(&self) -> Result<(), OtherError> {
        Err(OtherError)
    }
}

#[derive(Debug)]
pub struct SomeError;

#[derive(Debug)]
pub struct OtherError;

impl Display for OtherError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("other error")
    }
}

impl std::error::Error for OtherError {}

#[test]
fn error_description_can_be_retrieved() {
    let x = foo_new(0);
    foo_fails_too(&x);
    assert_eq!(&*safer_ffi_gen::last_error(), format!("{}", OtherError));
}

#[test]
fn error_debug_description_can_be_retrieved() {
    let x = foo_new(0);
    foo_fails(&x);
    assert_eq!(&*safer_ffi_gen::last_error(), format!("{:?}", SomeError));
}
