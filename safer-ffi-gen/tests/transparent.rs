use safer_ffi_gen::ffi_type;

#[ffi_type]
#[repr(transparent)]
pub struct Foo(i32);
