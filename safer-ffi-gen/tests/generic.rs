use safer_ffi_gen::{ffi_type, safer_ffi_gen, safer_ffi_gen_func};

#[ffi_type(opaque)]
pub struct Foo<T> {
    x: T,
}

#[safer_ffi_gen]
impl<T> Foo<T> {
    #[safer_ffi_gen_func]
    pub fn new(x: T) -> Self {
        Self { x }
    }

    #[safer_ffi_gen_func]
    pub fn get(&self) -> &T {
        &self.x
    }
}

safer_ffi_gen::specialize! { FooI32 = Foo<i32> }

#[test]
fn specialization_works() {
    let x = foo_i32_new(33);
    assert_eq!(*foo_i32_get(&x), 33);
}
