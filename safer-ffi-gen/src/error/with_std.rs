use std::{cell::RefCell, fmt::Debug};

thread_local! {
    static LAST_ERROR: RefCell<Option<Box<dyn std::error::Error>>> = RefCell::new(None);
}

pub fn set_last_error<E>(e: E)
where
    E: Into<Box<dyn std::error::Error>>,
{
    LAST_ERROR.with(|last_error| *last_error.borrow_mut() = Some(e.into()));
}

#[safer_ffi::ffi_export]
pub fn last_error() -> safer_ffi::String {
    LAST_ERROR
        .with(|e| {
            e.borrow()
                .as_ref()
                .map(ToString::to_string)
                .unwrap_or_default()
        })
        .into()
}

#[macro_export]
macro_rules! set_last_error {
    ($e:expr) => {{
        #[allow(unused_imports)]
        use $crate::private::{WrapDebug, WrapError, WrapNotDebug, WrapNotError};

        let e = $e;
        let e = e
            .safer_ffi_gen_wrap_error()
            .unwrap_or_else(|e| e.safer_ffi_gen_wrap_debug());
        $crate::private::set_last_error(e);
    }};
}

pub trait WrapError: Sized {
    fn safer_ffi_gen_wrap_error(self) -> Result<Box<dyn std::error::Error>, Self>;
}

impl<T: std::error::Error + 'static> WrapError for T {
    fn safer_ffi_gen_wrap_error(self) -> Result<Box<dyn std::error::Error>, Self> {
        Ok(Box::new(self))
    }
}

pub trait WrapNotError {
    fn safer_ffi_gen_wrap_error(&self) -> Result<Box<dyn std::error::Error>, &Self> {
        Err(self)
    }
}

impl<T> WrapNotError for T {}

pub trait WrapDebug {
    fn safer_ffi_gen_wrap_debug(self) -> Box<dyn std::error::Error>;
}

impl<T: Debug> WrapDebug for T {
    fn safer_ffi_gen_wrap_debug(self) -> Box<dyn std::error::Error> {
        format!("{self:?}").into()
    }
}

pub trait WrapNotDebug {
    fn safer_ffi_gen_wrap_debug(&self) -> Box<dyn std::error::Error> {
        std::any::type_name::<Self>().into()
    }
}

impl<T> WrapNotDebug for &T {}

#[cfg(test)]
mod tests {
    #[test]
    fn value_of_type_implementing_error_can_be_set_as_last_error() {
        use std::fmt::{self, Display};

        const MESSAGE: &str = "just a test";

        #[derive(Debug)]
        struct TestError;

        impl Display for TestError {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str(MESSAGE)
            }
        }

        impl std::error::Error for TestError {}

        crate::set_last_error!(TestError);
        assert_eq!(&*crate::last_error(), MESSAGE);
    }

    #[test]
    fn value_of_type_implementing_debug_can_be_set_as_last_error() {
        crate::set_last_error!(33);
        assert_eq!(&*crate::last_error(), "33");
    }

    #[test]
    fn value_of_type_not_implementing_debug_can_be_set_as_last_error() {
        struct Foo;

        crate::set_last_error!(Foo);
        assert!(crate::last_error().contains("Foo"));
    }
}
