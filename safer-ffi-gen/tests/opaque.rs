use safer_ffi_gen::{ffi_type, safer_ffi_gen, safer_ffi_gen_func};

#[ffi_type(opaque)]
pub struct Foo {
    i: i32,
}

#[safer_ffi_gen]
impl Foo {
    #[safer_ffi_gen_func]
    pub fn new(i: i32) -> Foo {
        Self { i }
    }

    #[safer_ffi_gen_func]
    pub fn get(&self) -> i32 {
        self.i
    }

    #[safer_ffi_gen_func]
    pub fn inc(&mut self) {
        self.i += 1;
    }
}

#[test]
fn opaque_type_can_be_created() {
    let _ = foo_new(33);
}

#[test]
fn by_ref_method_works() {
    let x = foo_new(33);
    assert_eq!(foo_get(&x), 33);
}

#[test]
fn by_ref_mut_method_works() {
    let mut x = foo_new(33);
    foo_inc(&mut x);
    assert_eq!(foo_get(&x), 34);
}
