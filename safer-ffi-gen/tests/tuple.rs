use safer_ffi_gen::{ffi_type, safer_ffi_gen};

#[ffi_type(opaque)]
pub struct Foo {
    i: i32,
}

#[safer_ffi_gen]
impl Foo {
    pub fn new_pair(i: i32, j: i32) -> (Foo, Foo) {
        (Self { i }, Self { i: j })
    }

    pub fn get(&self) -> i32 {
        self.i
    }
}

#[test]
fn pair_can_be_returned() {
    let pair = foo_new_pair(33, 34);
    assert_eq!(foo_get(&pair._0), 33);
    assert_eq!(foo_get(&pair._1), 34);
}
