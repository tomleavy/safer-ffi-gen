use safer_ffi_gen::{ffi_type, safer_ffi_gen};

#[ffi_type(clone, opaque)]
#[derive(Clone, Debug)]
pub enum Foo {
    A,
    B(i32),
    C(Bar),
}

#[safer_ffi_gen]
impl Foo {
    pub fn get_int(&self) -> i32 {
        match *self {
            Foo::B(i) => i,
            _ => 0,
        }
    }
}

#[ffi_type(clone, opaque)]
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Bar {
    x: i32,
}

#[test]
fn enumeration_with_data_is_supported() {
    assert_eq!(foo_get_int(&Foo::B(33)), 33);
}

#[test]
fn discriminant_is_generated() {
    assert_eq!(foo_discriminant(&Foo::B(33)), FooDiscriminant::B);
}

#[test]
fn variant_accessors_are_generated() {
    assert_eq!(foo_to_b(&Foo::A), None);
    assert_eq!(foo_to_b(&Foo::B(33)), Some(&33));
    assert_eq!(foo_to_c(&Foo::C(Bar { x: 34 })), Some(&Bar { x: 34 }));
}
