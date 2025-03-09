use std::fmt;
use std::rc::Rc;

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Kind {
    Star,                      // *
    Kfun(Rc<Kind>, Rc<Kind>),  // k1 -> k2
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        fmt_kind(f, self, false)
    }
}

fn fmt_kind(f: &mut fmt::Formatter<'_>, kind: &Kind, paren: bool) -> Result<(), fmt::Error> {
    match kind {
        Kind::Star => write!(f, "*"),
        Kind::Kfun(l, r) => {
            if paren {
                write!(f, "(")?;
            }
            fmt_kind(f, l, true)?;
            write!(f, " -> ")?;
            fmt_kind(f, r, false)?;
            if paren {
                write!(f, ")")?;
            }
            Ok(())

        }
    }
}
