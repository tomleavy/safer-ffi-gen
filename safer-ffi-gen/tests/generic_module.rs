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

mod ffi {
    safer_ffi_gen::specialize! { FooI32 = crate::Foo<i32> }
}

#[test]
fn specialization_works() {
    let x = ffi::foo_i32_new(33);
    assert_eq!(*ffi::foo_i32_get(&x), 33);
}
