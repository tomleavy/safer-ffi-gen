use safer_ffi_gen::{ffi_type, safer_ffi_gen};

#[ffi_type(opaque)]
pub struct Foo<'a> {
    bar: &'a Bar,
}

#[safer_ffi_gen]
impl<'a> Foo<'a> {
    pub fn get(&self) -> i32 {
        self.bar.i
    }

    pub fn as_bar(&self) -> &Bar {
        self.bar
    }
}

#[ffi_type(opaque)]
pub struct Bar {
    i: i32,
}

#[safer_ffi_gen]
impl Bar {
    pub fn new(i: i32) -> Self {
        Bar { i }
    }

    pub fn as_foo(&self) -> Foo<'_> {
        Foo { bar: self }
    }
}

#[test]
fn type_generic_over_lifetime_is_supported() {
    let a = bar_new(33);
    let b = bar_as_foo(&a);
    assert_eq!(foo_get(&b), 33);

    let a2 = foo_as_bar(&b);
    let b2 = bar_as_foo(a2);
    assert_eq!(foo_get(&b2), 33);
}
