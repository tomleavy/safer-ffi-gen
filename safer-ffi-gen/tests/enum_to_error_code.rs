use safer_ffi_gen::enum_to_error_code;

#[enum_to_error_code]
#[non_exhaustive]
pub enum FooError {
    A,
    B(i64),
}

#[test]
fn enum_to_error_code_works() {
    assert_eq!(i32::from(&FooError::A), -1);
    assert_eq!(i32::from(&FooError::B(33)), -2);
    assert_eq!(FooErrorCode::A as i32, -1);
    assert_eq!(FooErrorCode::B as i32, -2);
}
