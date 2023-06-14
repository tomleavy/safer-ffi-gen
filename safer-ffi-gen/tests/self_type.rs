use safer_ffi_gen::{ffi_type, safer_ffi_gen};

#[ffi_type(opaque)]
pub struct Foo {
    pub i: i32,
}

#[safer_ffi_gen]
impl Foo {
    pub fn take_self_by_value(_x: Self) {}

    pub fn take_self_by_ref(_x: &Self) {}

    pub fn take_wrapped_self(_x: Vec<&Self>) {}

    pub fn return_self() -> Self {
        Self { i: 33 }
    }
}

#[test]
fn self_type_works() {
    foo_take_self_by_ref(&foo_return_self());
    foo_take_self_by_value(foo_return_self());
    foo_take_wrapped_self(vec![&*foo_return_self()].into());
}
