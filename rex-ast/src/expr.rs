use std::fmt::{self, Display, Formatter};

use rex_lexer::span::{Position, Span};

use crate::id::{Id, IdDispenser};

#[derive(Clone, Debug, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub struct Var {
    pub id: Id,
    pub span: Span,
    pub name: String,
}

impl Var {
    pub fn new(id: Id, span: Span, name: impl ToString) -> Self {
        Self {
            id,
            span,
            name: name.to_string(),
        }
    }

    pub fn next(id_dispenser: &mut IdDispenser, name: impl ToString) -> Self {
        Self::new(id_dispenser.next(), Span::default(), name)
    }

    pub fn next_with_span(id_dispenser: &mut IdDispenser, span: Span, name: impl ToString) -> Self {
        Self::new(id_dispenser.next(), span, name)
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
    Bool(Id, Span, bool),     // true
    Uint(Id, Span, u64),      // 69
    Int(Id, Span, i64),       // -420
    Float(Id, Span, f64),     // 3.14
    String(Id, Span, String), // "hello"

    Tuple(Id, Span, Vec<Expr>),          // (e1, e2, e3)
    List(Id, Span, Vec<Expr>),           // [e1, e2, e3]
    Dict(Id, Span, Vec<(String, Expr)>), // {k1 = v1, k2 = v2}

    Var(Var),                                       // x
    App(Id, Span, Box<Expr>, Box<Expr>),            // f x
    Lam(Id, Span, Var, Box<Expr>),                  // λx → e
    Let(Id, Span, Var, Box<Expr>, Box<Expr>),       // let x = e1 in e2
    Ite(Id, Span, Box<Expr>, Box<Expr>, Box<Expr>), // if e1 then e2 else e3
}

impl Expr {
    pub fn id(&self) -> &Id {
        match self {
            Self::Bool(id, _, _)
            | Self::Uint(id, _, _)
            | Self::Int(id, _, _)
            | Self::Float(id, _, _)
            | Self::String(id, _, _)
            | Self::Tuple(id, _, _)
            | Self::List(id, _, _)
            | Self::Dict(id, _, _)
            | Self::Var(Var { id, .. })
            | Self::App(id, _, _, _)
            | Self::Lam(id, _, _, _)
            | Self::Let(id, _, _, _, _)
            | Self::Ite(id, _, _, _, _) => id,
        }
    }

    pub fn span(&self) -> &Span {
        match self {
            Self::Bool(_, span, _)
            | Self::Uint(_, span, _)
            | Self::Int(_, span, _)
            | Self::Float(_, span, _)
            | Self::String(_, span, _)
            | Self::Tuple(_, span, _)
            | Self::List(_, span, _)
            | Self::Dict(_, span, _)
            | Self::Var(Var { span, .. })
            | Self::App(_, span, _, _)
            | Self::Lam(_, span, _, _)
            | Self::Let(_, span, _, _, _)
            | Self::Ite(_, span, _, _, _) => span,
        }
    }

    pub fn span_mut(&mut self) -> &mut Span {
        match self {
            Self::Bool(_, span, _)
            | Self::Uint(_, span, _)
            | Self::Int(_, span, _)
            | Self::Float(_, span, _)
            | Self::String(_, span, _)
            | Self::Tuple(_, span, _)
            | Self::List(_, span, _)
            | Self::Dict(_, span, _)
            | Self::Var(Var { span, .. })
            | Self::App(_, span, _, _)
            | Self::Lam(_, span, _, _)
            | Self::Let(_, span, _, _, _)
            | Self::Ite(_, span, _, _, _) => span,
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
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool(_id, _span, x) => x.fmt(f),
            Self::Uint(_id, _span, x) => x.fmt(f),
            Self::Int(_id, _span, x) => x.fmt(f),
            Self::Float(_id, _span, x) => x.fmt(f),
            Self::String(_id, _span, x) => x.fmt(f),
            Self::List(_id, _span, xs) => {
                '['.fmt(f)?;
                for (i, x) in xs.iter().enumerate() {
                    x.fmt(f)?;
                    if i + 1 < xs.len() {
                        ", ".fmt(f)?;
                    }
                }
                ']'.fmt(f)
            }
            Self::Tuple(_id, _span, xs) => {
                '('.fmt(f)?;
                for (i, x) in xs.iter().enumerate() {
                    x.fmt(f)?;
                    if i + 1 < xs.len() {
                        ", ".fmt(f)?;
                    }
                }
                ')'.fmt(f)
            }
            Self::Dict(_id, _span, kvs) => {
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

            Self::Var(var) => var.fmt(f),
            Self::App(_id, _span, g, x) => {
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
            Self::Lam(_id, _span, param, body) => {
                'λ'.fmt(f)?;
                param.fmt(f)?;
                " → ".fmt(f)?;
                body.fmt(f)
            }
            Self::Let(_id, _span, var, def, body) => {
                "let ".fmt(f)?;
                var.fmt(f)?;
                " = ".fmt(f)?;
                def.fmt(f)?;
                " in ".fmt(f)?;
                body.fmt(f)
            }
            Self::Ite(_id, _span, cond, then, r#else) => {
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
