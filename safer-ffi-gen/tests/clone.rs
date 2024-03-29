use safer_ffi_gen::{ffi_type, safer_ffi_gen};

#[ffi_type(opaque, clone)]
#[derive(Clone)]
pub struct Foo {
    i: i32,
}

#[safer_ffi_gen]
impl Foo {
    pub fn new(i: i32) -> Foo {
        Self { i }
    }

    pub fn get(&self) -> i32 {
        self.i
    }

    pub fn inc(&mut self) {
        self.i += 1;
    }
}

#[test]
fn cloning_works() {
    let a = foo_new(33);
    let b = foo_clone(&a);
    assert_eq!(foo_get(&a), foo_get(&b));
}

#[test]
fn cloning_returns_new_copy() {
    let a = foo_new(33);
    let mut b = foo_clone(&a);
    foo_inc(&mut b);
    assert_eq!(foo_get(&a), 33);
    assert_eq!(foo_get(&b), 34);
}
