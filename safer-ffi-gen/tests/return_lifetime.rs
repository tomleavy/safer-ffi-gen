// This test checks that methods that rely on the lifetime deduction rule that links implicit
// lifetimes in the return type to the lifetime of `self` are appropriately converted to free
// functions (that set explicit lifetimes).

use safer_ffi_gen::{ffi_type, safer_ffi_gen};

#[ffi_type(opaque)]
pub struct Foo {
    _x: i32,
}

#[safer_ffi_gen]
impl Foo {
    pub fn foo(&self, _s: &str) -> &str {
        ""
    }
}
