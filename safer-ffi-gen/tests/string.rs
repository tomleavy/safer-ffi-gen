use safer_ffi_gen::safer_ffi_gen;

struct Foo;

#[safer_ffi_gen]
impl Foo {
    #[safer_ffi_gen]
    pub fn identity_str(s: &str) -> &str {
        s
    }

    #[safer_ffi_gen]
    pub fn append(mut prefix: String, suffix: &str) -> String {
        prefix.push_str(suffix);
        prefix
    }
}

#[test]
fn string_slice_can_be_passed_and_returned() {
    assert_eq!(foo_identity_str("foo".into()).as_str(), "foo");
}

#[test]
fn string_can_be_passed_and_returned() {
    assert_eq!(
        &*foo_append(String::from("foo").into(), "bar".into()),
        "foobar"
    );
}
