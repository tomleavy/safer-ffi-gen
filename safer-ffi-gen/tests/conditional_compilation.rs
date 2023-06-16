use safer_ffi_gen::{ffi_type, safer_ffi_gen};

#[ffi_type(opaque)]
pub struct Foo {
    i: i32,
}

#[safer_ffi_gen]
impl Foo {
    pub fn new(i: i32) -> Foo {
        Self { i }
    }

    #[cfg(debug_assertions)]
    pub fn get(&self) -> i32 {
        self.i + 1
    }

    #[cfg(not(debug_assertions))]
    pub fn get(&self) -> i32 {
        self.i + 2
    }
}

#[test]
fn conditional_compilation_works() {
    let a = foo_new(33);
    let _ = foo_get(&a);
}
