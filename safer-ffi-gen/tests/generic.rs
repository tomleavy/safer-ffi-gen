use safer_ffi_gen::{ffi_type, safer_ffi_gen};

#[ffi_type(opaque)]
pub struct Foo<T> {
    x: T,
}

#[safer_ffi_gen]
impl<T> Foo<T> {
    pub fn new(x: T) -> Self {
        Self { x }
    }

    pub fn get(&self) -> &T {
        &self.x
    }
}

safer_ffi_gen::specialize! { FooI32 = Foo<i32> }
safer_ffi_gen::specialize! { FooI64 = Foo<i64> }

#[test]
fn specialization_works() {
    let x = foo_i32_new(33);
    assert_eq!(*foo_i32_get(&x), 33);

    let x = foo_i64_new(33);
    assert_eq!(*foo_i64_get(&x), 33);
}
