use safer_ffi_gen::{ffi_type, safer_ffi_gen};

#[ffi_type]
#[repr(C)]
pub struct Foo {
    x: i32,
}

#[safer_ffi_gen]
impl Foo {
    #[safer_ffi_gen]
    pub fn new(x: i32) -> Self {
        Self { x }
    }

    #[safer_ffi_gen]
    pub fn get(&self) -> i32 {
        self.x
    }

    #[safer_ffi_gen]
    pub fn set(&mut self, x: i32) {
        self.x = x;
    }
}

#[test]
fn repr_c_works() {
    let mut a = foo_new(33);
    assert_eq!(foo_get(&a), 33);
    foo_set(&mut a, 34);
    assert_eq!(foo_get(&a), 34);
}
