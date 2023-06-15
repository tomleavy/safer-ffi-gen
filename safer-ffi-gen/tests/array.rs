use safer_ffi_gen::{ffi_type, safer_ffi_gen};

#[ffi_type(opaque)]
pub struct Foo {
    i: i32,
}

#[safer_ffi_gen]
impl Foo {
    pub fn new_many(i: i32) -> [Foo; 2] {
        [Self { i }, Self { i }]
    }

    pub fn get(&self) -> i32 {
        self.i
    }

    pub fn take_many(_foos: [Foo; 2]) {}
}

#[test]
fn array_can_be_returned() {
    let a = foo_new_many(33);
    assert_eq!(foo_get(&a[0]), 33);
    assert_eq!(foo_get(&a[1]), 33);
}

#[test]
fn array_can_be_passed_in() {
    let foos = foo_new_many(33);
    foo_take_many(foos);
}
