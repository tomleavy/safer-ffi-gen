use safer_ffi_gen::{ffi_type, safer_ffi_gen};

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

    pub fn succeeds(&self) -> Result<(), SomeError> {
        Ok(())
    }
}

pub struct SomeError;

impl From<&SomeError> for i32 {
    fn from(_: &SomeError) -> i32 {
        33
    }
}

pub struct OtherError;

#[test]
fn error_can_be_converted_to_int() {
    let x = foo_new(0);
    assert_eq!(foo_fails(&x), 33);
}

#[test]
fn error_not_convertible_to_int_is_mapped_to_negative() {
    let x = foo_new(0);
    assert_eq!(foo_fails_too(&x), -1);
}

#[test]
fn zero_is_returned_on_success() {
    let x = foo_new(33);
    assert_eq!(foo_succeeds(&x), 0);
}
