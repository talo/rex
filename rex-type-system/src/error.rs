use crate::types::{Kind, Type};
use rex_lexer::span::Span;
use std::{collections::BTreeSet, fmt, sync::Arc};

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum TypeError {
    UnboundVariable(Span, String),
    CannotUnify(Span, String, Arc<Type>, Arc<Type>),
    IncompatibleCandidates(Span, Arc<Type>, BTreeSet<Arc<Type>>),
    OccursCheckFailed(Span, String),
    TupleLengthMismatch(Span, String),
    DictKeysMismatch(Span, String, Vec<String>),
    KindMismatch(Span, String, Option<Kind>, Option<Kind>),
}

impl TypeError {
    pub fn span(&self) -> &Span {
        match self {
            Self::UnboundVariable(span, ..) => span,
            Self::CannotUnify(span, ..) => span,
            Self::IncompatibleCandidates(span, ..) => span,
            Self::OccursCheckFailed(span, ..) => span,
            Self::TupleLengthMismatch(span, ..) => span,
            Self::DictKeysMismatch(span, ..) => span,
            Self::KindMismatch(span, ..) => span,
        }
    }
    pub fn path(&self) -> &str {
        match self {
            Self::UnboundVariable(..) => "",
            Self::CannotUnify(_, path, ..) => path,
            Self::IncompatibleCandidates(..) => "",
            Self::OccursCheckFailed(_, path, ..) => path,
            Self::TupleLengthMismatch(_, path, ..) => path,
            Self::DictKeysMismatch(_, path, ..) => path,
            Self::KindMismatch(_, path, ..) => path,
        }
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}: ", self.span())?;
        if !self.path().is_empty() {
            write!(f, "{}: ", self.path())?;
        }
        match self {
            Self::UnboundVariable(_, name) => {
                write!(f, "Unbound variable: {}", name)
            }
            Self::CannotUnify(_, _, t1, t2) => {
                write!(f, "Cannot unify {} with {}", t1, t2)
            }
            Self::IncompatibleCandidates(_, t1, t2_possibilities) => {
                writeln!(f, "Cannot unify")?;
                writeln!(f, "    {}", t1)?;
                write!(f, "    with incompatible candidates")?;
                for t2 in t2_possibilities.iter() {
                    write!(f, "\n    {}", t2)?;
                }
                Ok(())
            }
            Self::OccursCheckFailed(_, _) => {
                write!(f, "Occurs check failed")
            }
            Self::TupleLengthMismatch(_, _) => {
                write!(f, "Tuple lengths do not match")
            }
            Self::DictKeysMismatch(_, _, missing_keys) => {
                write!(f, "Missing and/or extra keys:")?;
                for (i, key) in missing_keys.iter().enumerate() {
                    if i == 0 {
                        write!(f, " ")?;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", key)?;
                }
                Ok(())
            }
            Self::KindMismatch(_, _, k1, k2) => {
                write!(
                    f,
                    "Kinds {} and {} do not match",
                    k1.map(|k| k.to_string())
                        .unwrap_or_else(|| "<invalid>".to_string()),
                    k2.map(|k| k.to_string())
                        .unwrap_or_else(|| "<invalid>".to_string())
                )
            }
        }
    }
}

impl fmt::Debug for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        fmt::Display::fmt(self, f)
    }
}

impl std::error::Error for TypeError {}
