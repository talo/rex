use std::{
    collections::{BTreeMap, HashMap},
    fmt::{self, Display, Formatter},
};

use rex_ast::id::Id;

pub type TypeEnv = HashMap<String, Type>;

pub type ExprTypeEnv = HashMap<Id, Type>;

#[derive(Clone, Debug, Eq, Hash, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Type {
    Var(Id),
    ForAll(Id, Box<Type>, Vec<Id>),

    ADT(ADT),
    Arrow(Box<Type>, Box<Type>),
    Result(Box<Type>, Box<Type>),
    Option(Box<Type>),
    List(Box<Type>),
    Dict(BTreeMap<String, Type>),
    Tuple(Vec<Type>),

    Bool,
    Uint,
    Int,
    Float,
    String,
}

impl Type {
    pub fn num_params(&self) -> usize {
        match self {
            Type::Arrow(_, b) => 1 + b.num_params(),
            _ => 0,
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub struct ADT {
    pub name: String,
    pub variants: Vec<ADTVariant>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, serde::Deserialize, serde::Serialize)]
#[serde(rename_all = "lowercase")]
pub struct ADTVariant {
    pub name: String,
    pub t: Option<Box<Type>>,
}

impl Display for ADT {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.name.fmt(f)?;
        if !self.variants.is_empty() {
            " ∈ ".fmt(f)?;
            for (i, v) in self.variants.iter().enumerate() {
                v.fmt(f)?;
                if i + 1 < self.variants.len() {
                    " | ".fmt(f)?;
                }
            }
        }
        Ok(())
    }
}

impl Display for ADTVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.name.fmt(f)?;
        if let Some(t) = &self.t {
            " (".fmt(f)?;
            t.fmt(f)?;
            ')'.fmt(f)?;
        }
        Ok(())
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Type::Bool => "bool".fmt(f),
            Type::Uint => "uint".fmt(f),
            Type::Int => "int".fmt(f),
            Type::Float => "float".fmt(f),
            Type::String => "string".fmt(f),
            Type::Option(x) => {
                "Option (".fmt(f)?;
                x.fmt(f)?;
                ')'.fmt(f)
            }
            Type::Result(a, b) => {
                "Result (".fmt(f)?;
                a.fmt(f)?;
                ") (".fmt(f)?;
                b.fmt(f)?;
                ')'.fmt(f)
            }
            Type::Tuple(xs) => {
                '('.fmt(f)?;
                for (i, x) in xs.iter().enumerate() {
                    x.fmt(f)?;
                    if i + 1 < xs.len() {
                        ", ".fmt(f)?;
                    }
                }
                ')'.fmt(f)
            }
            Type::Dict(xs) => {
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
            Type::Arrow(a, b) => {
                a.fmt(f)?;
                " → ".fmt(f)?;
                b.fmt(f)
            }
            Type::Var(x) => {
                'τ'.fmt(f)?;
                x.fmt(f)
            }
            Type::ForAll(x, t, deps) => {
                "∀τ".fmt(f)?;
                x.fmt(f)?;
                ". ".fmt(f)?;
                t.fmt(f)?;
                if !deps.is_empty() {
                    " where {".fmt(f)?;
                    for (i, dep) in deps.iter().enumerate() {
                        dep.fmt(f)?;
                        if i + 1 < deps.len() {
                            ", ".fmt(f)?;
                        }
                    }
                    '}'.fmt(f)?;
                }
                Ok(())
            }
            Type::List(x) => {
                '['.fmt(f)?;
                x.fmt(f)?;
                ']'.fmt(f)
            }
            Type::ADT(x) => x.fmt(f),
        }
    }
}
