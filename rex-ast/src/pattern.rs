use std::fmt::{self, Display, Formatter};

use crate::ast::Var;

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub enum Pattern {
    Var(Var),
    Tuple(Vec<Pattern>),
    List(ListPattern),
    Data(DataPattern),
}

impl Display for Pattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(x) => x.fmt(f),
            Self::Tuple(xs) => {
                '('.fmt(f)?;
                for (i, x) in xs.iter().enumerate() {
                    x.fmt(f)?;
                    if i < xs.len() - 1 || xs.len() == 1 {
                        ", ".fmt(f)?;
                    }
                }
                ')'.fmt(f)
            }
            Self::List(x) => x.fmt(f),
            Self::Data(x) => x.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub enum ListPattern {
    Var(Var),                                 // xs
    Elems(Vec<Pattern>),                      // [x, y, z]
    HeadTail(Box<Pattern>, Box<ListPattern>), // x:xs
}

impl Display for ListPattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(x) => x.fmt(f),
            Self::Elems(xs) => {
                '['.fmt(f)?;
                for (i, x) in xs.iter().enumerate() {
                    x.fmt(f)?;
                    if i < xs.len() - 1 {
                        ", ".fmt(f)?;
                    }
                }
                ']'.fmt(f)
            }
            Self::HeadTail(x, xs) => {
                x.fmt(f)?;
                ':'.fmt(f)?;
                xs.fmt(f)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub enum DataPattern {
    Constructor(String, Option<Vec<DictPattern>>), // Foo { x, y, w = z:zs }
}

impl Display for DataPattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Constructor(x, xs) => {
                x.fmt(f)?;
                if let Some(xs) = xs {
                    '{'.fmt(f)?;
                    for (i, x) in xs.iter().enumerate() {
                        x.fmt(f)?;
                        if i < xs.len() - 1 {
                            ", ".fmt(f)?;
                        }
                    }
                    '}'.fmt(f)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub enum DictPattern {
    Key(Var),                       // w
    KeyMatch(String, Box<Pattern>), // w = x:xs
}

impl Display for DictPattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Key(key) => key.fmt(f),
            Self::KeyMatch(key, x) => {
                key.fmt(f)?;
                " = ".fmt(f)?;
                x.fmt(f)
            }
        }
    }
}
