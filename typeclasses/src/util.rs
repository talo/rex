use std::fmt;

pub fn fmt_paren<I, P>(f: &mut fmt::Formatter<'_>, at: P, level: P, inner: I) -> Result<(), fmt::Error>
    where I: FnOnce(&mut fmt::Formatter<'_>) -> Result<(), fmt::Error>,
          P: PartialOrd
{
    if at >= level {
        write!(f, "(")?;
    }
    inner(f)?;
    if at >= level {
        write!(f, ")")?;
    }
    Ok(())
}
