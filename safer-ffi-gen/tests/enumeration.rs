use safer_ffi_gen::{ffi_type, safer_ffi_gen};

#[ffi_type]
#[repr(u8)]
#[derive(Clone, Copy)]
pub enum Foo {
    A,
    B,
}

#[safer_ffi_gen]
impl Foo {
    pub fn discriminant(&self) -> u8 {
        *self as u8
    }
}

#[test]
fn c_like_enumeration_is_supported() {
    assert_eq!(foo_discriminant(&Foo::A), 0);
    assert_eq!(foo_discriminant(&Foo::B), 1);
}
