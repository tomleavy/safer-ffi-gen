use safer_ffi_gen::{safer_ffi_gen, safer_ffi_gen_func};

pub struct Foo;

#[safer_ffi_gen]
impl Foo {
    #[safer_ffi_gen_func]
    pub fn foo() {}
}

#[test]
fn unit_return_type_works() {
    foo_foo();
}
