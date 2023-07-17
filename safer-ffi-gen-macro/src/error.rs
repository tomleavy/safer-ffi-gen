use proc_macro2::Span;
use quote::ToTokens;
use syn::spanned::Spanned;
use thiserror::Error;

#[derive(Debug, Error)]
#[error("{reason}")]
pub struct Error {
    pub reason: ErrorReason,
    pub span: Span,
}

impl Error {
    pub fn new(span: Span, reason: ErrorReason) -> Self {
        Self { reason, span }
    }

    pub fn new_spanned<T>(token: T, reason: ErrorReason) -> Self
    where
        T: ToTokens,
    {
        Self {
            reason,
            span: token.span(),
        }
    }
}

impl From<Error> for syn::Error {
    fn from(e: Error) -> Self {
        syn::Error::new(e.span, e.reason)
    }
}

#[derive(Clone, Debug, Eq, Error, PartialEq)]
pub enum ErrorReason {
    #[error("Argument is unknown")]
    UnknownArg,
    #[error(
        "`clone` is not supported on generic types with type or const parameters; it must be \
specified with `safer_ffi_gen::specialize` instead"
    )]
    CloneOnGenericType,
    #[error("Trait implementation blocks are not supported")]
    TraitImplBlock,
    #[error("Implementation block must be for a type path (e.g. `some_module::some_type`")]
    ImplBlockMustBeForTypePath,
    #[error("Exported functions can only be generic over lifetimes")]
    GenericFunction,
    #[error("Unsupported parameter pattern")]
    UnsupportedParamPattern,
    #[error("Missing type representation")]
    MissingRepr,
    #[error("Invalid type in `repr`")]
    BadReprType,
    #[error("Too many variants")]
    TooManyVariants,
    #[error("Unsupported item type (only structs and enums are supported)")]
    UnsupportedItemType,
}

impl ErrorReason {
    pub fn with_span(self, span: Span) -> Error {
        Error::new(span, self)
    }

    pub fn spanned<T>(self, token: T) -> Error
    where
        T: ToTokens,
    {
        Error::new_spanned(token, self)
    }
}
