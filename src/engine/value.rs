use std::fmt::Display;

use ouroboros::Type;

use crate::resolver::{Id, Lambda};

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    U64(u64),
    I64(i64),
    F64(f64),
    String(String),
    List(Vec<Value>),
    Function(Function),
    Lambda(Lambda),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Bool(x) => x.fmt(f),
            Value::U64(x) => x.fmt(f),
            Value::I64(x) => x.fmt(f),
            Value::F64(x) => x.fmt(f),
            Value::String(x) => x.fmt(f),
            Value::List(xs) => {
                write!(f, "[")?;
                for (i, x) in xs.iter().enumerate() {
                    x.fmt(f)?;
                    if i < xs.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Value::Function(function) => {
                write!(
                    f,
                    "({}: ",
                    match function.name.as_str() {
                        "+" => "(+)",
                        "-" => "(-)",
                        "*" => "(*)",
                        "/" => "(/)",
                        "." => "(.)",
                        x => x,
                    }
                )?;
                for (i, x) in function.params.iter().enumerate() {
                    x.fmt(f)?;
                    if i < function.params.len() - 1 {
                        write!(f, " -> ")?;
                    }
                }
                write!(f, ")")
            }
            Value::Lambda(lam) => {
                write!(f, "(λ: ")?;
                for (i, x) in lam.params.iter().enumerate() {
                    x.name.fmt(f)?;
                    if i < lam.params.len() - 1 {
                        write!(f, " -> ")?;
                    }
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub id: Id,
    pub name: String,
    pub params: Vec<Type>,
    pub ret: Type,
}
