#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

use alloc::{boxed::Box, string::String, vec, vec::Vec};
use safer_ffi::layout::ReprC;

pub use safer_ffi;
pub use safer_ffi_gen_macro::*;

mod async_util;
mod error;
mod opaque;

pub use async_util::*;

#[cfg(feature = "std")]
pub use error::last_error;

/// Private definitions used by macros. These are always considered unstable and can have breaking
/// changes between semver-compatible releases of this crate. Do not use.
#[doc(hidden)]
pub mod private {
    #[cfg(feature = "std")]
    pub use crate::error::{set_last_error, WrapDebug, WrapError, WrapNotDebug, WrapNotError};

    pub use crate::{
        error::{WrapIntoInt, WrapNotIntoInt},
        opaque::OpaqueLayout,
    };
}

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
impl_ffi_type_as_identity!(isize);
impl_ffi_type_as_identity!(usize);
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
    T: FfiType,
    Option<T::Safe>: ReprC,
{
    type Safe = Option<T::Safe>;

    fn into_safe(self) -> Self::Safe {
        self.map(T::into_safe)
    }

    fn from_safe(x: Self::Safe) -> Self {
        x.map(T::from_safe)
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

#[safer_ffi::ffi_export]
pub fn vec_u8_free(_v: safer_ffi::vec::Vec<u8>) {}

#[safer_ffi::ffi_export]
pub fn vec_u8_new(length: usize) -> safer_ffi::vec::Vec<u8> {
    vec![0; length].into()
}

#[safer_ffi::ffi_export]
pub fn vec_u8_from_slice(bytes: safer_ffi::slice::slice_ref<'_, u8>) -> safer_ffi::vec::Vec<u8> {
    bytes.to_vec().into()
}
