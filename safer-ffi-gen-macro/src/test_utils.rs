use quote::ToTokens;
use std::fmt::{self, Debug};

/// Helper to show syntax items as tokens, making it easier to inspect and spot differences
#[derive(Eq, PartialEq)]
pub struct Pretty<T>(pub T);

impl<T> Debug for Pretty<T>
where
    T: ToTokens,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0.to_token_stream().to_string())
    }
}
