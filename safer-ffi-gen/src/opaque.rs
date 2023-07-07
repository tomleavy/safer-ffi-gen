use core::marker::PhantomData;
use safer_ffi::layout::CType;

#[cfg(feature = "headers")]
use safer_ffi::{
    headers::{languages::HeaderLanguage, Definer},
    layout::ReprC,
};

#[cfg(feature = "headers")]
use std::io;

pub struct OpaqueLayout<T>(PhantomData<T>);

impl<T> Clone for OpaqueLayout<T> {
    fn clone(&self) -> Self {
        OpaqueLayout(PhantomData)
    }
}

impl<T> Copy for OpaqueLayout<T> {}

unsafe impl<T> CType for OpaqueLayout<T> {
    type OPAQUE_KIND = safer_ffi::layout::OpaqueKind::Opaque;

    #[cfg(feature = "headers")]
    fn short_name() -> String {
        let s = <<safer_ffi::layout::Opaque<T> as ReprC>::CLayout as CType>::short_name();
        match s.strip_prefix("Opaque_") {
            Some(s) => s.into(),
            None => s,
        }
    }

    #[cfg(feature = "headers")]
    fn define_self__impl(
        language: &dyn HeaderLanguage,
        definer: &mut dyn Definer,
    ) -> io::Result<()> {
        language.emit_opaque_type(
            definer,
            &[&format!(
                "The layout of {} is subject to change",
                core::any::type_name::<T>()
            )],
            &PhantomData::<Self>,
        )
    }
}
