use std::rc::Rc;
use std::fmt;
use crate::extras::Id;
use crate::kind::Kind;
use crate::util::fmt_paren;

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Type {
    TVar(Rc<Tyvar>),
    TCon(Rc<Tycon>),
    TAp(Rc<Type>, Rc<Type>),
    TGen(u64),
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Tyvar(pub Id, pub Rc<Kind>);

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Tycon(pub Id, pub Rc<Kind>);

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        fmt_type(f, self, TypePrec::Min)
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Clone, Copy)]
enum TypePrec {
    Min,
    // Atomic,
    // Arrow,
    Pair,
    Ap,
}

fn fmt_type(
    f: &mut fmt::Formatter<'_>,
    t: &Type,
    at: TypePrec,
) -> Result<(), fmt::Error> {
    match t {
        Type::TVar(v) => write!(f, "{}", v.0.0)?,
        Type::TCon(c) => write!(f, "{}", c.0.0)?,
        Type::TAp(l, r) => {
            if let Type::TAp(l2, r2) = &**l {
                if let Type::TCon(tc) = &**l2 {
                    if tc.0.0 == "->" {
                        fmt_paren(f, at, TypePrec::Ap, |f| {
                            fmt_type(f, r2, TypePrec::Ap)?;
                            write!(f, " -> ")?;
                            fmt_type(f, r, TypePrec::Min)?;
                            Ok(())
                        })?;
                        return Ok(())
                    }
                    if tc.0.0 == "(,)" {
                        fmt_paren(f, at, TypePrec::Pair, |f| {
                            fmt_type(f, r2, TypePrec::Pair)?;
                            write!(f, " , ")?;
                            fmt_type(f, r, TypePrec::Min)?;
                            Ok(())
                        })?;
                        return Ok(())
                    }
                }
            }

            if let Type::TCon(tc) = &**l {
                if tc.0.0 == "[]" {
                    write!(f, "[")?;
                    fmt_type(f, r, TypePrec::Min)?;
                    write!(f, "]")?;
                    return Ok(())
                }
            }

            fmt_paren(f, at, TypePrec::Ap, |f| {
                write!(f, "TAp ")?;
                fmt_type(f, l, TypePrec::Ap)?;
                write!(f, " ")?;
                fmt_type(f, r, TypePrec::Min)?;
                Ok(())
            })?;

        }
        Type::TGen(i) => write!(f, "{}", i)?,
    }
    Ok(())
}

impl fmt::Display for Tyvar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "Tyvar {} {}", self.0, self.1)
    }
}

impl fmt::Display for Tycon {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "Tycon {} {}", self.0, self.1)
    }
}
