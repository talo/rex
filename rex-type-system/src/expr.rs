use std::fmt::{self, Display, Formatter};

use rex_lexer::span::Span;

#[derive(Clone, Debug, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Expr {
    Bool(bool, Option<Span>),                           // true
    Uint(u64, Option<Span>),                            // 69
    Int(i64, Option<Span>),                             // -420
    Float(f64, Option<Span>),                           // 3.14
    String(String, Option<Span>),                       // "hello"
    Tuple(Vec<Expr>, Option<Span>),                     // (e1, e2, e3)
    List(Vec<Expr>, Option<Span>),                      // [e1, e2, e3]
    Dict(Vec<(String, Expr)>, Option<Span>),            // {k1 = v1, k2 = v2}
    Var(String, Option<Span>),                          // x
    App(Box<Expr>, Box<Expr>, Option<Span>),            // f x
    Lam(String, Box<Expr>, Option<Span>),               // λx → e
    Let(String, Box<Expr>, Box<Expr>, Option<Span>),    // let x = e1 in e2
    Ite(Box<Expr>, Box<Expr>, Box<Expr>, Option<Span>), // if e1 then e2 else e3
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool(x, _span) => x.fmt(f),
            Self::Uint(x, _span) => x.fmt(f),
            Self::Int(x, _span) => x.fmt(f),
            Self::Float(x, _span) => x.fmt(f),
            Self::String(x, _span) => x.fmt(f),
            Self::List(xs, _span) => {
                '['.fmt(f)?;
                for (i, x) in xs.iter().enumerate() {
                    x.fmt(f)?;
                    if i + 1 < xs.len() {
                        ", ".fmt(f)?;
                    }
                }
                ']'.fmt(f)
            }
            Self::Tuple(xs, _span) => {
                '('.fmt(f)?;
                for (i, x) in xs.iter().enumerate() {
                    x.fmt(f)?;
                    if i + 1 < xs.len() {
                        ", ".fmt(f)?;
                    }
                }
                ')'.fmt(f)
            }
            Self::Dict(kvs, _span) => {
                '{'.fmt(f)?;
                for (i, (k, v)) in kvs.iter().enumerate() {
                    k.fmt(f)?;
                    " = ".fmt(f)?;
                    v.fmt(f)?;
                    if i + 1 < kvs.len() {
                        ", ".fmt(f)?;
                    }
                }
                '}'.fmt(f)
            }

            Self::Var(name, _span) => match name.as_ref() {
                "+" | "-" | "*" | "/" | "==" | ">=" | ">" | "<=" | "<" | "++" | "." => {
                    '('.fmt(f)?;
                    name.fmt(f)?;
                    ')'.fmt(f)
                }
                _ => name.fmt(f),
            },
            Self::App(g, x, _span) => {
                g.fmt(f)?;
                ' '.fmt(f)?;
                match x.as_ref() {
                    Self::Bool(..)
                    | Self::Uint(..)
                    | Self::Int(..)
                    | Self::Float(..)
                    | Self::String(..)
                    | Self::List(..)
                    | Self::Tuple(..)
                    | Self::Dict(..)
                    | Self::Var(..) => x.fmt(f),
                    _ => {
                        '('.fmt(f)?;
                        x.fmt(f)?;
                        ')'.fmt(f)
                    }
                }
            }
            Self::Lam(param, body, _span) => {
                'λ'.fmt(f)?;
                param.fmt(f)?;
                " → ".fmt(f)?;
                body.fmt(f)
            }
            Self::Let(var, def, body, _span) => {
                "let ".fmt(f)?;
                var.fmt(f)?;
                " = ".fmt(f)?;
                def.fmt(f)?;
                " in ".fmt(f)?;
                body.fmt(f)
            }
            Self::Ite(cond, then, r#else, _span) => {
                "if ".fmt(f)?;
                cond.fmt(f)?;
                " then ".fmt(f)?;
                then.fmt(f)?;
                " else ".fmt(f)?;
                r#else.fmt(f)
            }
        }
    }
}
