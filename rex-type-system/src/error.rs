use crate::types::Type;
use rex_lexer::span::Span;
use std::{collections::BTreeSet, fmt, sync::Arc};

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum TypeError {
    UnboundVariable(Span, String),
    CannotUnify(Span, Arc<Type>, Arc<Type>),
    IncompatibleCandidates(Span, Arc<Type>, BTreeSet<Arc<Type>>),
    OccursCheckFailed(Span),
    Other(Span, String),
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::UnboundVariable(span, name) => {
                write!(f, "{}: Unbound variable: {}", span, name)
            }
            Self::CannotUnify(span, t1, t2) => {
                write!(f, "{}: Cannot unify {} with {}", span, t1, t2)
            }
            Self::IncompatibleCandidates(span, t1, t2_possibilities) => {
                write!(f, "{}: Cannot unify\n", span)?;
                write!(f, "    {}\n", t1)?;
                write!(f, "with incompatible candidates")?;
                for t2 in t2_possibilities.iter() {
                    write!(f, "\n    {}", t2)?;
                }
                Ok(())
            }
            Self::OccursCheckFailed(span) => {
                write!(f, "{}: Occurs check failed", span)
            }
            Self::Other(span, msg) => {
                write!(f, "{}: {}", span, msg)
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
