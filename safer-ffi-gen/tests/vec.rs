use safer_ffi_gen::safer_ffi_gen;

struct Foo;

#[safer_ffi_gen]
impl Foo {
    pub fn append(mut v: Vec<u8>, x: u8) -> Vec<u8> {
        v.push(x);
        v
    }
}

#[test]
fn vec_can_be_passed_and_returned() {
    assert_eq!(&*foo_append(vec![0, 1].into(), 2), &[0, 1, 2]);
}
