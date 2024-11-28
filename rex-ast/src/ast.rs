use std::fmt::{self, Display, Formatter};

use rex_lexer::{
    span::{Span, Spanned},
    Token,
};

use crate::id::Id;

#[derive(Clone, Debug, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub enum AST {
    // Comment
    Comment(Span, Vec<Token>),

    // Literal expressions
    Null(Span),
    Bool(Span, bool),
    Uint(Span, u64),
    Int(Span, i64),
    Float(Span, f64),
    String(Span, String),
    List(Span, Vec<AST>),
    Tuple(Span, Vec<AST>),
    Dict(Span, Vec<(Var, AST)>),

    // Expressions
    Var(Var),
    Call(Call),
    Lambda(Lambda),
    LetIn(LetIn),
    IfThenElse(IfThenElse),

    // Constructor
    Ctor(Ctor),
}

impl From<Var> for AST {
    fn from(var: Var) -> Self {
        AST::Var(var)
    }
}

impl From<Call> for AST {
    fn from(call: Call) -> Self {
        AST::Call(call)
    }
}

impl From<Lambda> for AST {
    fn from(lam: Lambda) -> Self {
        AST::Lambda(lam)
    }
}

impl From<LetIn> for AST {
    fn from(let_in: LetIn) -> Self {
        AST::LetIn(let_in)
    }
}

impl From<Ctor> for AST {
    fn from(cons: Ctor) -> Self {
        AST::Ctor(cons)
    }
}

impl Display for AST {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            // Comment
            Self::Comment(.., comment) => {
                "{{- ".fmt(f)?;
                for n in comment {
                    n.fmt(f)?;
                }
                " -}}".fmt(f)
            }

            // Literal expressions
            Self::Null(..) => "null".fmt(f),
            Self::Bool(.., x) => x.fmt(f),
            Self::Uint(.., x) => x.fmt(f),
            Self::Int(.., x) => x.fmt(f),
            Self::Float(.., x) => x.fmt(f),
            Self::String(.., x) => x.fmt(f),
            Self::List(.., xs) => {
                '['.fmt(f)?;
                for (i, x) in xs.iter().enumerate() {
                    x.fmt(f)?;
                    if i + 1 < xs.len() {
                        ", ".fmt(f)?;
                    }
                }
                ']'.fmt(f)
            }
            Self::Tuple(.., xs) => {
                '('.fmt(f)?;
                for (i, x) in xs.iter().enumerate() {
                    x.fmt(f)?;
                    if i + 1 < xs.len() {
                        ", ".fmt(f)?;
                    }
                }
                ')'.fmt(f)
            }
            Self::Dict(.., xs) => {
                '{'.fmt(f)?;
                for (i, (k, v)) in xs.iter().enumerate() {
                    k.fmt(f)?;
                    " = ".fmt(f)?;
                    v.fmt(f)?;
                    if i + 1 < xs.len() {
                        ", ".fmt(f)?;
                    }
                }
                '}'.fmt(f)
            }

            // Expressions
            Self::Var(ast) => ast.fmt(f),
            Self::Call(ast) => ast.fmt(f),
            Self::Lambda(ast) => ast.fmt(f),
            Self::LetIn(ast) => ast.fmt(f),
            Self::IfThenElse(ast) => ast.fmt(f),

            // Constructor
            Self::Ctor(ast) => ast.fmt(f),
        }
    }
}

#[derive(Clone, Debug, Eq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub struct Var {
    #[serde(skip)]
    pub span: Span,
    pub id: Id,

    pub name: String,
}

impl Var {
    pub fn new(span: Span, id: Id, name: impl ToString) -> Self {
        Self {
            span,
            id,
            name: name.to_string(),
        }
    }
}

impl PartialEq<Var> for Var {
    fn eq(&self, other: &Var) -> bool {
        self.id == other.id && self.name == other.name
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

#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub struct Call {
    #[serde(skip)]
    pub span: Span,
    pub id: Id,

    pub base: Box<AST>,
    pub arg: Box<AST>,
}

impl Call {
    pub fn new(span: Span, id: Id, base: impl Into<AST>, arg: impl Into<AST>) -> Self {
        Self {
            span,
            id,

            base: Box::new(base.into()),
            arg: Box::new(arg.into()),
        }
    }
}

impl PartialEq<Call> for Call {
    fn eq(&self, other: &Call) -> bool {
        self.id == other.id && self.base == other.base && self.arg == other.arg
    }
}

impl Display for Call {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.base.fmt(f)?;
        ' '.fmt(f)?;
        match self.arg.as_ref() {
            AST::Null(..)
            | AST::Bool(..)
            | AST::Uint(..)
            | AST::Int(..)
            | AST::Float(..)
            | AST::String(..)
            | AST::List(..)
            | AST::Tuple(..)
            | AST::Dict(..)
            | AST::Var(..) => self.arg.fmt(f),
            _ => {
                '('.fmt(f)?;
                self.arg.fmt(f)?;
                ')'.fmt(f)
            }
        }
    }
}

#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub struct Lambda {
    #[serde(skip)]
    pub span: Span,
    pub id: Id,

    pub var: Var,
    pub body: Box<AST>,
}

impl Lambda {
    pub fn new(span: Span, id: Id, var: Var, body: impl Into<AST>) -> Self {
        Self {
            span,
            id,

            var,
            body: Box::new(body.into()),
        }
    }
}

impl PartialEq<Lambda> for Lambda {
    fn eq(&self, other: &Lambda) -> bool {
        self.id == other.id && self.var == other.var && self.body == other.body
    }
}

impl Display for Lambda {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        'λ'.fmt(f)?;
        self.var.fmt(f)?;
        " → ".fmt(f)?;
        self.body.fmt(f)
    }
}

#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub struct LetIn {
    #[serde(skip)]
    pub span: Span,
    pub id: Id,

    pub var: Var,
    pub def: Box<AST>,
    pub body: Box<AST>,
}

impl LetIn {
    pub fn new(span: Span, id: Id, var: Var, def: impl Into<AST>, body: impl Into<AST>) -> Self {
        Self {
            span,
            id,

            var,
            def: Box::new(def.into()),
            body: Box::new(body.into()),
        }
    }
}

impl PartialEq<LetIn> for LetIn {
    fn eq(&self, other: &LetIn) -> bool {
        self.id == other.id
            && self.var == other.var
            && self.def == other.def
            && self.body == other.body
    }
}

impl Display for LetIn {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        "let ".fmt(f)?;
        self.var.fmt(f)?;
        " = ".fmt(f)?;
        self.def.fmt(f)?;
        " in ".fmt(f)?;
        self.body.fmt(f)
    }
}

#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub struct IfThenElse {
    #[serde(skip)]
    pub span: Span,
    pub id: Id,

    pub cond: Box<AST>,
    pub then: Box<AST>,
    pub r#else: Box<AST>,
}

impl IfThenElse {
    pub fn new(
        span: Span,
        id: Id,
        cond: impl Into<AST>,
        then: impl Into<AST>,
        r#else: impl Into<AST>,
    ) -> Self {
        Self {
            span,
            id,

            cond: Box::new(cond.into()),
            then: Box::new(then.into()),
            r#else: Box::new(r#else.into()),
        }
    }
}

impl PartialEq<IfThenElse> for IfThenElse {
    fn eq(&self, other: &IfThenElse) -> bool {
        self.id == other.id
            && self.cond == other.cond
            && self.then == other.then
            && self.r#else == other.r#else
    }
}

impl Display for IfThenElse {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        "if ".fmt(f)?;
        self.cond.fmt(f)?;
        " then ".fmt(f)?;
        self.then.fmt(f)?;
        " else ".fmt(f)?;
        self.r#else.fmt(f)
    }
}

#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub struct Ctor {
    #[serde(skip)]
    pub span: Span,
    pub id: Id,

    pub name: String,
    pub fields: Fields,
}

impl Ctor {
    pub fn new(span: Span, id: Id, name: String, fields: impl Into<Fields>) -> Self {
        Self {
            span,
            id,

            name,
            fields: fields.into(),
        }
    }
}

impl PartialEq<Ctor> for Ctor {
    fn eq(&self, other: &Ctor) -> bool {
        self.id == other.id && self.name == other.name && self.fields == other.fields
    }
}

impl Display for Ctor {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.name.fmt(f)?;
        ' '.fmt(f)?;
        self.fields.fmt(f)
    }
}

#[derive(Clone, Debug, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Fields {
    Named(NamedFields),
    Unnamed(UnnamedFields),
}

impl From<NamedFields> for Fields {
    fn from(fields: NamedFields) -> Self {
        Fields::Named(fields)
    }
}

impl From<UnnamedFields> for Fields {
    fn from(fields: UnnamedFields) -> Self {
        Fields::Unnamed(fields)
    }
}

impl Display for Fields {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Named(named_fields) => named_fields.fmt(f),
            Self::Unnamed(unnamed_fields) => unnamed_fields.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub struct NamedFields {
    #[serde(skip)]
    pub span: Span,
    pub fields: Vec<(Var, AST)>,
}

impl NamedFields {
    pub fn new(span: Span, fields: impl Into<Vec<(Var, AST)>>) -> Self {
        Self {
            span,
            fields: fields.into(),
        }
    }
}

impl Display for NamedFields {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        "{ ".fmt(f)?;
        for (i, (k, v)) in self.fields.iter().enumerate() {
            k.fmt(f)?;
            " = ".fmt(f)?;
            v.fmt(f)?;
            if i + 1 < self.fields.len() {
                ", ".fmt(f)?;
            }
        }
        '}'.fmt(f)
    }
}

#[derive(Clone, Debug, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub struct UnnamedFields {
    #[serde(skip)]
    pub span: Span,
    pub fields: Vec<AST>,
}

impl UnnamedFields {
    pub fn new(span: Span, fields: impl Into<Vec<AST>>) -> Self {
        Self {
            span,
            fields: fields.into(),
        }
    }
}

impl Display for UnnamedFields {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (i, v) in self.fields.iter().enumerate() {
            v.fmt(f)?;
            if i + 1 < self.fields.len() {
                ' '.fmt(f)?;
            }
        }
        Ok(())
    }
}

impl Spanned for AST {
    fn span(&self) -> &Span {
        match self {
            AST::Comment(span, ..) => span,

            AST::Null(span, ..) => span,
            AST::Bool(span, ..) => span,
            AST::Uint(span, ..) => span,
            AST::Int(span, ..) => span,
            AST::Float(span, ..) => span,
            AST::String(span, ..) => span,
            AST::List(span, ..) => span,
            AST::Tuple(span, ..) => span,
            AST::Dict(span, ..) => span,

            AST::Var(var) => &var.span,
            AST::Call(call) => &call.span,
            AST::Lambda(lam) => &lam.span,
            AST::LetIn(let_in) => &let_in.span,
            AST::IfThenElse(ite) => &ite.span,
            AST::Ctor(cons) => &cons.span,
        }
    }

    fn span_mut(&mut self) -> &mut Span {
        match self {
            AST::Comment(span, ..) => span,

            AST::Null(span, ..) => span,
            AST::Bool(span, ..) => span,
            AST::Uint(span, ..) => span,
            AST::Int(span, ..) => span,
            AST::Float(span, ..) => span,
            AST::String(span, ..) => span,
            AST::List(span, ..) => span,
            AST::Tuple(span, ..) => span,
            AST::Dict(span, ..) => span,

            AST::Var(var) => &mut var.span,
            AST::Call(call) => &mut call.span,
            AST::Lambda(lam) => &mut lam.span,
            AST::LetIn(let_in) => &mut let_in.span,
            AST::IfThenElse(ite) => &mut ite.span,
            AST::Ctor(cons) => &mut cons.span,
        }
    }
}
