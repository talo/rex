use std::{
    collections::BTreeMap,
    fmt::{self, Display, Formatter},
};

use rex_lexer::span::{Position, Span};
use rpds::HashTrieMapSync;

use crate::id::Id;

pub type Scope = HashTrieMapSync<String, Expr>;

#[derive(Clone, Debug, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub struct Var {
    pub id: Id,
    pub span: Span,
    pub name: String,
}

impl Var {
    pub fn new(name: impl ToString) -> Self {
        Self {
            id: Id::new(),
            span: Span::default(),
            name: name.to_string(),
        }
    }

    pub fn with_span(span: Span, name: impl ToString) -> Self {
        Self {
            id: Id::new(),
            span,
            name: name.to_string(),
        }
    }

    pub fn with_id(id: Id, span: Span, name: impl ToString) -> Self {
        Self {
            id,
            span,
            name: name.to_string(),
        }
    }

    pub fn reset_id(&mut self) {
        self.id = Id::default();
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
    Bool(Id, Span, bool),     // true
    Uint(Id, Span, u64),      // 69
    Int(Id, Span, i64),       // -420
    Float(Id, Span, f64),     // 3.14
    String(Id, Span, String), // "hello"

    Tuple(Id, Span, Vec<Expr>),             // (e1, e2, e3)
    List(Id, Span, Vec<Expr>),              // [e1, e2, e3]
    Dict(Id, Span, BTreeMap<String, Expr>), // {k1 = v1, k2 = v2}
    Named(Id, Span, String, Box<Expr>), //  MyVariant1 {k1 = v1, k2 = v2}
    Var(Var),                                       // x
    App(Id, Span, Box<Expr>, Box<Expr>),            // f x
    Lam(Id, Span, Scope, Var, Box<Expr>),           // λx → e
    Let(Id, Span, Var, Box<Expr>, Box<Expr>),       // let x = e1 in e2
    Ite(Id, Span, Box<Expr>, Box<Expr>, Box<Expr>), // if e1 then e2 else e3

    // NOTE(loong): this cannot actually be expressed in code. It is the result
    // of an application to a multi-argument function from the ftable. We can
    // probably simplify evaluation massively by having the FtableFn calls be
    // responsible for capturing arguments and returning curried functions.
    // Right now this is handled by the engine and it causes some confusion.
    Curry(Id, Span, Var, Vec<Expr>), // f x y z {- for currying external functions -}
}

impl Expr {
    pub fn id(&self) -> &Id {
        match self {
            Self::Bool(id, ..)
            | Self::Uint(id, ..)
            | Self::Int(id, ..)
            | Self::Float(id, ..)
            | Self::String(id, ..)
            | Self::Tuple(id, ..)
            | Self::List(id, ..)
            | Self::Dict(id, ..)
            | Self::Named(id, ..)
            | Self::Var(Var { id, .. })
            | Self::App(id, ..)
            | Self::Lam(id, ..)
            | Self::Let(id, ..)
            | Self::Ite(id, ..)
            | Self::Curry(id, ..) => id,
        }
    }

    pub fn id_mut(&mut self) -> &mut Id {
        match self {
            Self::Bool(id, ..)
            | Self::Uint(id, ..)
            | Self::Int(id, ..)
            | Self::Float(id, ..)
            | Self::String(id, ..)
            | Self::Tuple(id, ..)
            | Self::List(id, ..)
            | Self::Dict(id, ..)
            | Self::Named(id, ..)
            | Self::Var(Var { id, .. })
            | Self::App(id, ..)
            | Self::Lam(id, ..)
            | Self::Let(id, ..)
            | Self::Ite(id, ..)
            | Self::Curry(id, ..) => id,
        }
    }

    pub fn span(&self) -> &Span {
        match self {
            Self::Bool(_, span, ..)
            | Self::Uint(_, span, ..)
            | Self::Int(_, span, ..)
            | Self::Float(_, span, ..)
            | Self::String(_, span, ..)
            | Self::Tuple(_, span, ..)
            | Self::List(_, span, ..)
            | Self::Dict(_, span, ..)
            | Self::Named(_, span, ..)
            | Self::Var(Var { span, .. })
            | Self::App(_, span, ..)
            | Self::Lam(_, span, ..)
            | Self::Let(_, span, ..)
            | Self::Ite(_, span, ..)
            | Self::Curry(_, span, ..) => span,
        }
    }

    pub fn span_mut(&mut self) -> &mut Span {
        match self {
            Self::Bool(_, span, ..)
            | Self::Uint(_, span, ..)
            | Self::Int(_, span, ..)
            | Self::Float(_, span, ..)
            | Self::String(_, span, ..)
            | Self::Tuple(_, span, ..)
            | Self::List(_, span, ..)
            | Self::Dict(_, span, ..)
            | Self::Named(_, span, ..)
            | Self::Var(Var { span, .. })
            | Self::App(_, span, ..)
            | Self::Lam(_, span, ..)
            | Self::Let(_, span, ..)
            | Self::Ite(_, span, ..)
            | Self::Curry(_, span, ..) => span,
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

    pub fn reset_ids(&mut self) {
        match self {
            Self::Bool(id, ..) => *id = Id::default(),
            Self::Uint(id, ..) => *id = Id::default(),
            Self::Int(id, ..) => *id = Id::default(),
            Self::Float(id, ..) => *id = Id::default(),
            Self::String(id, ..) => *id = Id::default(),
            Self::Tuple(id, _span, elems) => {
                *id = Id::default();
                for elem in elems {
                    elem.reset_ids();
                }
            }
            Self::List(id, _span, elems) => {
                *id = Id::default();
                for elem in elems {
                    elem.reset_ids();
                }
            }
            Self::Dict(id, _span, kvs) => {
                *id = Id::default();
                for (_k, v) in kvs {
                    v.reset_ids();
                }
            }
            Self::Named(id, _span, _name, inner) => {
                *id = Id::default();
                inner.reset_ids();
            }
            Self::Var(var) => var.reset_id(),
            Self::App(id, _span, g, x) => {
                *id = Id::default();
                g.reset_ids();
                x.reset_ids();
            }
            Self::Lam(id, _span, _scope, param, body) => {
                *id = Id::default();
                param.reset_id();
                body.reset_ids();
            }
            Self::Let(id, _span, var, def, body) => {
                *id = Id::default();
                var.reset_id();
                def.reset_ids();
                body.reset_ids();
            }
            Self::Ite(id, _span, cond, then, r#else) => {
                *id = Id::default();
                cond.reset_ids();
                then.reset_ids();
                r#else.reset_ids();
            }
            Self::Curry(id, _span, g, args) => {
                *id = Id::default();
                g.reset_id();
                for arg in args {
                    arg.reset_ids();
                }
            }
        }
    }

    pub fn reset_spans(&mut self) {
        match self {
            Self::Bool(_, span, ..) => *span = Span::default(),
            Self::Uint(_, span, ..) => *span = Span::default(),
            Self::Int(_, span, ..) => *span = Span::default(),
            Self::Float(_, span, ..) => *span = Span::default(),
            Self::String(_, span, ..) => *span = Span::default(),
            Self::Tuple(_, span, elems) => {
                *span = Span::default();
                for elem in elems {
                    elem.reset_spans();
                }
            }
            Self::List(_, span, elems) => {
                *span = Span::default();
                for elem in elems {
                    elem.reset_spans();
                }
            }
            Self::Dict(_, span, kvs) => {
                *span = Span::default();
                for (_k, v) in kvs {
                    v.reset_spans();
                }
            }
            Self::Named(_, span, _name, inner) => {
                *span = Span::default();
                inner.reset_spans();
            }
            Self::Var(var) => var.reset_span(),
            Self::App(_, span, g, x) => {
                *span = Span::default();
                g.reset_spans();
                x.reset_spans();
            }
            Self::Lam(_, span, _scope, param, body) => {
                *span = Span::default();
                param.reset_span();
                body.reset_spans();
            }
            Self::Let(_, span, var, def, body) => {
                *span = Span::default();
                var.reset_span();
                def.reset_spans();
                body.reset_spans();
            }
            Self::Ite(_, span, cond, then, r#else) => {
                *span = Span::default();
                cond.reset_spans();
                then.reset_spans();
                r#else.reset_spans();
            }
            Self::Curry(_, span, g, args) => {
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
            Self::Named(_id, _span, name, inner) => {
                name.fmt(f)?;
                '('.fmt(f)?;
                inner.fmt(f)?;
                ')'.fmt(f)
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
            Self::Lam(_id, _span, _scope, param, body) => {
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
            Self::Curry(_id, _span, g, args) => {
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
