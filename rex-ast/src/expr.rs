use std::{
    collections::BTreeMap,
    fmt::{self, Display, Formatter},
};

use rex_lexer::span::{Position, Span};
use rpds::HashTrieMapSync;

use chrono::{DateTime, Utc};
use uuid::Uuid;

pub type Scope = HashTrieMapSync<String, Expr>;

#[derive(Clone, Debug, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub struct Var {
    pub span: Span,
    pub name: String,
}

impl Var {
    pub fn new(name: impl ToString) -> Self {
        Self {
            span: Span::default(),
            name: name.to_string(),
        }
    }

    pub fn with_span(span: Span, name: impl ToString) -> Self {
        Self {
            span,
            name: name.to_string(),
        }
    }

    pub fn reset_span(&mut self) {
        self.span = Span::default();
    }
}

impl Display for Var {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.name.as_ref() {
            "+" | "-" | "*" | "/" | "==" | ">=" | ">" | "<=" | "<" | "++" | "." => {
                '('.fmt(f)?;
                self.name.fmt(f)?;
                ')'.fmt(f)
            }
            _ => self.name.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Expr {
    Bool(Span, bool),     // true
    Uint(Span, u64),      // 69
    Int(Span, i64),       // -420
    Float(Span, f64),     // 3.14
    String(Span, String), // "hello"
    Uuid(Span, Uuid),
    DateTime(Span, DateTime<Utc>),

    Tuple(Span, Vec<Expr>),                 // (e1, e2, e3)
    List(Span, Vec<Expr>),                  // [e1, e2, e3]
    Dict(Span, BTreeMap<String, Expr>),     // {k1 = v1, k2 = v2}
    Named(Span, String, Option<Box<Expr>>), //  MyVariant1 {k1 = v1, k2 = v2}
    Promise(Span, Uuid),
    Var(Var),                                   // x
    App(Span, Box<Expr>, Box<Expr>),            // f x
    Lam(Span, Scope, Var, Box<Expr>),           // λx → e
    Let(Span, Var, Box<Expr>, Box<Expr>),       // let x = e1 in e2
    Ite(Span, Box<Expr>, Box<Expr>, Box<Expr>), // if e1 then e2 else e3

    // NOTE(loong): this cannot actually be expressed in code. It is the result
    // of an application to a multi-argument function from the ftable. We can
    // probably simplify evaluation massively by having the FtableFn calls be
    // responsible for capturing arguments and returning curried functions.
    // Right now this is handled by the engine and it causes some confusion.
    Curry(Span, Var, Vec<Expr>), // f x y z {- for currying external functions -}
}

impl Expr {
    pub fn span(&self) -> &Span {
        match self {
            Self::Bool(span, ..)
            | Self::Uint(span, ..)
            | Self::Int(span, ..)
            | Self::Float(span, ..)
            | Self::String(span, ..)
            | Self::Uuid(span, ..)
            | Self::DateTime(span, ..)
            | Self::Tuple(span, ..)
            | Self::List(span, ..)
            | Self::Dict(span, ..)
            | Self::Named(span, ..)
            | Self::Promise(span, ..)
            | Self::Var(Var { span, .. })
            | Self::App(span, ..)
            | Self::Lam(span, ..)
            | Self::Let(span, ..)
            | Self::Ite(span, ..)
            | Self::Curry(span, ..) => span,
        }
    }

    pub fn span_mut(&mut self) -> &mut Span {
        match self {
            Self::Bool(span, ..)
            | Self::Uint(span, ..)
            | Self::Int(span, ..)
            | Self::Float(span, ..)
            | Self::String(span, ..)
            | Self::Uuid(span, ..)
            | Self::DateTime(span, ..)
            | Self::Tuple(span, ..)
            | Self::List(span, ..)
            | Self::Dict(span, ..)
            | Self::Named(span, ..)
            | Self::Promise(span, ..)
            | Self::Var(Var { span, .. })
            | Self::App(span, ..)
            | Self::Lam(span, ..)
            | Self::Let(span, ..)
            | Self::Ite(span, ..)
            | Self::Curry(span, ..) => span,
        }
    }

    pub fn set_span_begin_end(&mut self, begin: Position, end: Position) {
        self.span_mut().begin = begin;
        self.span_mut().end = end;
    }

    pub fn set_span_begin(&mut self, begin: Position) {
        self.span_mut().begin = begin;
    }

    pub fn set_span_end(&mut self, end: Position) {
        self.span_mut().end = end;
    }

    pub fn reset_spans(&mut self) {
        match self {
            Self::Bool(span, ..) => *span = Span::default(),
            Self::Uint(span, ..) => *span = Span::default(),
            Self::Int(span, ..) => *span = Span::default(),
            Self::Float(span, ..) => *span = Span::default(),
            Self::String(span, ..) => *span = Span::default(),
            Self::Uuid(span, ..) => *span = Span::default(),
            Self::DateTime(span, ..) => *span = Span::default(),
            Self::Tuple(span, elems) => {
                *span = Span::default();
                for elem in elems {
                    elem.reset_spans();
                }
            }
            Self::List(span, elems) => {
                *span = Span::default();
                for elem in elems {
                    elem.reset_spans();
                }
            }
            Self::Dict(span, kvs) => {
                *span = Span::default();
                for (_k, v) in kvs {
                    v.reset_spans();
                }
            }
            Self::Named(span, _name, inner) => {
                *span = Span::default();
                if let Some(inner) = inner {
                    inner.reset_spans();
                }
            }
            Self::Promise(span, ..) => {
                *span = Span::default();
            }
            Self::Var(var) => var.reset_span(),
            Self::App(span, g, x) => {
                *span = Span::default();
                g.reset_spans();
                x.reset_spans();
            }
            Self::Lam(span, _scope, param, body) => {
                *span = Span::default();
                param.reset_span();
                body.reset_spans();
            }
            Self::Let(span, var, def, body) => {
                *span = Span::default();
                var.reset_span();
                def.reset_spans();
                body.reset_spans();
            }
            Self::Ite(span, cond, then, r#else) => {
                *span = Span::default();
                cond.reset_spans();
                then.reset_spans();
                r#else.reset_spans();
            }
            Self::Curry(span, g, args) => {
                *span = Span::default();
                g.reset_span();
                for arg in args {
                    arg.reset_spans();
                }
            }
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool(_span, x) => x.fmt(f),
            Self::Uint(_span, x) => x.fmt(f),
            Self::Int(_span, x) => x.fmt(f),
            Self::Float(_span, x) => x.fmt(f),
            Self::String(_span, x) => write!(f, "{:?}", x),
            Self::Uuid(_span, x) => x.fmt(f),
            Self::DateTime(_span, x) => x.fmt(f),
            Self::List(_span, xs) => {
                '['.fmt(f)?;
                for (i, x) in xs.iter().enumerate() {
                    x.fmt(f)?;
                    if i + 1 < xs.len() {
                        ", ".fmt(f)?;
                    }
                }
                ']'.fmt(f)
            }
            Self::Tuple(_span, xs) => {
                '('.fmt(f)?;
                for (i, x) in xs.iter().enumerate() {
                    x.fmt(f)?;
                    if i + 1 < xs.len() {
                        ", ".fmt(f)?;
                    }
                }
                ')'.fmt(f)
            }
            Self::Dict(_span, kvs) => {
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
            Self::Named(_span, name, inner) => {
                name.fmt(f)?;
                if let Some(inner) = inner {
                    '('.fmt(f)?;
                    inner.fmt(f)?;
                    ')'.fmt(f)?;
                }
                Ok(())
            }
            Self::Promise(_span, uuid) => {
                write!(f, "Promise({}", uuid)
            }
            Self::Var(var) => var.fmt(f),
            Self::App(_span, g, x) => {
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
            Self::Lam(_span, _scope, param, body) => {
                'λ'.fmt(f)?;
                param.fmt(f)?;
                " → ".fmt(f)?;
                body.fmt(f)
            }
            Self::Let(_span, var, def, body) => {
                "let ".fmt(f)?;
                var.fmt(f)?;
                " = ".fmt(f)?;
                def.fmt(f)?;
                " in ".fmt(f)?;
                body.fmt(f)
            }
            Self::Ite(_span, cond, then, r#else) => {
                "if ".fmt(f)?;
                cond.fmt(f)?;
                " then ".fmt(f)?;
                then.fmt(f)?;
                " else ".fmt(f)?;
                r#else.fmt(f)
            }
            Self::Curry(_span, g, args) => {
                g.fmt(f)?;
                for arg in args {
                    ' '.fmt(f)?;
                    match arg {
                        Self::Bool(..)
                        | Self::Uint(..)
                        | Self::Int(..)
                        | Self::Float(..)
                        | Self::String(..)
                        | Self::List(..)
                        | Self::Tuple(..)
                        | Self::Dict(..)
                        | Self::Var(..) => {
                            arg.fmt(f)?;
                        }
                        _ => {
                            '('.fmt(f)?;
                            arg.fmt(f)?;
                            ')'.fmt(f)?;
                        }
                    }
                }
                Ok(())
            }
        }
    }
}
