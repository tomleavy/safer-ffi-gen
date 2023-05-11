#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

use alloc::{boxed::Box, string::String, vec::Vec};
use safer_ffi::layout::ReprC;

pub use safer_ffi;
pub use safer_ffi_gen_macro::*;

#[cfg_attr(feature = "std", path = "with_std/error.rs")]
#[cfg_attr(not(feature = "std"), path = "without_std/error.rs")]
mod error;

mod async_util;

pub use async_util::*;
pub use error::{last_error, set_last_error};

pub trait FfiType {
    type Safe;

    fn into_safe(self) -> Self::Safe;
    fn from_safe(x: Self::Safe) -> Self;
}

macro_rules! impl_ffi_type_as_identity {
    ($ty:ty) => {
        impl FfiType for $ty {
            type Safe = $ty;

            fn into_safe(self) -> $ty {
                self
            }

            fn from_safe(x: $ty) -> $ty {
                x
            }
        }
    };
}

impl_ffi_type_as_identity!(i8);
impl_ffi_type_as_identity!(u8);
impl_ffi_type_as_identity!(i16);
impl_ffi_type_as_identity!(u16);
impl_ffi_type_as_identity!(i32);
impl_ffi_type_as_identity!(u32);
impl_ffi_type_as_identity!(i64);
impl_ffi_type_as_identity!(u64);
impl_ffi_type_as_identity!(bool);
impl_ffi_type_as_identity!(());

impl<'a> FfiType for &'a str {
    type Safe = safer_ffi::string::str_ref<'a>;

    fn into_safe(self) -> Self::Safe {
        self.into()
    }

    fn from_safe(x: Self::Safe) -> Self {
        x.as_str()
    }
}

impl FfiType for String {
    type Safe = safer_ffi::String;

    fn into_safe(self) -> Self::Safe {
        self.into()
    }

    fn from_safe(x: Self::Safe) -> Self {
        x.into()
    }
}

impl<'a, T: ReprC + 'a> FfiType for &'a [T] {
    type Safe = safer_ffi::slice::slice_ref<'a, T>;

    fn into_safe(self) -> Self::Safe {
        self.into()
    }

    fn from_safe(x: Self::Safe) -> Self {
        x.as_slice()
    }
}

impl<'a, T: ReprC + 'a> FfiType for &'a mut [T] {
    type Safe = safer_ffi::slice::slice_mut<'a, T>;

    fn into_safe(self) -> Self::Safe {
        self.into()
    }

    fn from_safe(x: Self::Safe) -> Self {
        x.as_slice()
    }
}

impl<T: ReprC> FfiType for Vec<T> {
    type Safe = safer_ffi::Vec<T>;

    fn into_safe(self) -> Self::Safe {
        self.into()
    }

    fn from_safe(x: Self::Safe) -> Self {
        x.into()
    }
}

impl<T: ReprC> FfiType for Box<T> {
    type Safe = safer_ffi::boxed::Box<T>;

    fn into_safe(self) -> Self::Safe {
        self.into()
    }

    fn from_safe(x: Self::Safe) -> Self {
        x.into()
    }
}

impl<T> FfiType for Option<T>
where
    Option<T>: ReprC,
{
    type Safe = Option<T>;

    fn into_safe(self) -> Self::Safe {
        self
    }

    fn from_safe(x: Self::Safe) -> Self {
        x
    }
}

impl<'a, T: ReprC> FfiType for &'a T {
    type Safe = &'a T;

    fn into_safe(self) -> Self::Safe {
        self
    }

    fn from_safe(x: Self::Safe) -> Self {
        x
    }
}

impl<'a, T: ReprC> FfiType for &'a mut T {
    type Safe = &'a mut T;

    fn into_safe(self) -> Self::Safe {
        self
    }

    fn from_safe(x: Self::Safe) -> Self {
        x
    }
}

impl<T: FfiType, U: FfiType> FfiType for (T, U) {
    type Safe = safer_ffi::Tuple2<T::Safe, U::Safe>;

    fn into_safe(self) -> Self::Safe {
        safer_ffi::Tuple2 {
            _0: self.0.into_safe(),
            _1: self.1.into_safe(),
        }
    }

    fn from_safe(x: Self::Safe) -> Self {
        (T::from_safe(x._0), U::from_safe(x._1))
    }
}

impl<const N: usize, T: ReprC> FfiType for [T; N] {
    type Safe = Self;

    fn into_safe(self) -> Self::Safe {
        self
    }

    fn from_safe(x: Self::Safe) -> Self {
        x
    }
}
