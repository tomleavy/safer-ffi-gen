#[cfg(feature = "std")]
mod with_std;

#[cfg(not(feature = "std"))]
mod without_std;

#[cfg(feature = "std")]
pub use with_std::{last_error, set_last_error, WrapDebug, WrapError, WrapNotDebug, WrapNotError};

#[macro_export]
macro_rules! error_code {
    ($e:expr) => {{
        #[allow(unused_imports)]
        use $crate::private::{WrapIntoInt, WrapNotIntoInt};

        let e = &$e;
        e.safer_ffi_gen_wrap_into_int()
    }};
}

pub trait WrapIntoInt {
    fn safer_ffi_gen_wrap_into_int(&self) -> i32;
}

impl<T> WrapIntoInt for T
where
    for<'a> &'a T: Into<i32>,
{
    fn safer_ffi_gen_wrap_into_int(&self) -> i32 {
        self.into()
    }
}

pub trait WrapNotIntoInt {
    fn safer_ffi_gen_wrap_into_int(&self) -> i32 {
        -1
    }
}

impl<T> WrapNotIntoInt for &T {}

#[cfg(test)]
mod tests {
    #[test]
    fn error_code_uses_int_conversion_when_it_exists() {
        struct Foo;

        impl From<&Foo> for i32 {
            fn from(_: &Foo) -> Self {
                33
            }
        }

        assert_eq!(crate::error_code!(Foo), 33);
    }

    #[test]
    fn error_code_returns_negative_when_int_conversion_does_not_exist() {
        struct Foo;

        assert_eq!(crate::error_code!(Foo), -1);
    }
}
