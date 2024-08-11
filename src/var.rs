use std::fmt::{self, Display, Formatter};

use crate::{span::Spanned, Span};

#[derive(Clone, Debug, Eq, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub struct UnresolvedVar {
    pub name: String,
    #[cfg_attr(feature = "serde", serde(skip))]
    pub span: Span,
}

impl Spanned for UnresolvedVar {
    fn span(&self) -> &Span {
        &self.span
    }

    fn span_mut(&mut self) -> &mut Span {
        &mut self.span
    }
}

impl Display for UnresolvedVar {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.name.fmt(f)
    }
}

impl UnresolvedVar {
    pub fn new(name: impl ToString, span: Span) -> Self {
        Self {
            name: name.to_string(),
            span,
        }
    }
}
