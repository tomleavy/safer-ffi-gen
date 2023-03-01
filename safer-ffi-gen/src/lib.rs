pub use safer_ffi;
pub use safer_ffi_gen_macro::*;

pub trait FfiType {
    type Foreign;

    fn to_foreign(self) -> Self::Foreign;
    fn from_foreign(x: Self::Foreign) -> Self;
}

macro_rules! impl_ffi_type_as_identity {
    ($ty:ty) => {
        impl FfiType for $ty {
            type Foreign = $ty;

            fn to_foreign(self) -> $ty {
                self
            }

            fn from_foreign(x: $ty) -> $ty {
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

impl FfiType for bool {
    type Foreign = u8;

    fn to_foreign(self) -> Self::Foreign {
        self as _
    }

    fn from_foreign(x: Self::Foreign) -> Self {
        x != 0
    }
}

impl FfiType for String {
    type Foreign = safer_ffi::String;

    fn to_foreign(self) -> Self::Foreign {
        self.into()
    }

    fn from_foreign(x: Self::Foreign) -> Self {
        x.into()
    }
}

impl<T> FfiType for Vec<T> {
    type Foreign = safer_ffi::Vec<T>;

    fn to_foreign(self) -> Self::Foreign {
        self.into()
    }

    fn from_foreign(x: Self::Foreign) -> Self {
        x.into()
    }
}

impl<T> FfiType for Box<T> {
    type Foreign = safer_ffi::boxed::Box<T>;

    fn to_foreign(self) -> Self::Foreign {
        self.into()
    }

    fn from_foreign(x: Self::Foreign) -> Self {
        x.into()
    }
}

impl<'a, T> FfiType for &'a T
where
    T: FfiType<Foreign = T>,
{
    type Foreign = &'a T;

    fn to_foreign(self) -> Self::Foreign {
        self
    }

    fn from_foreign(x: Self::Foreign) -> Self {
        x
    }
}

impl<'a, T> FfiType for &'a mut T
where
    T: FfiType<Foreign = T>,
{
    type Foreign = &'a mut T;

    fn to_foreign(self) -> Self::Foreign {
        self
    }

    fn from_foreign(x: Self::Foreign) -> Self {
        x
    }
}
