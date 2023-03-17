use safer_ffi_gen::safer_ffi_gen;

struct Foo;

#[safer_ffi_gen]
impl Foo {
    #[safer_ffi_gen]
    pub fn identity(bytes: &[u8]) -> &[u8] {
        bytes
    }
}

#[test]
fn slice_can_be_passed_and_returned() {
    assert_eq!(foo_identity(b"foo"[..].into()).as_slice(), b"foo");
}
