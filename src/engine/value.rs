use std::{collections::BTreeMap, fmt::Display};

use ouroboros::Type;
use serde::ser::Error;

use crate::{
    resolver::{Id, Lambda, Variable, IR},
    span::Span,
};

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub enum Value {
    Null,
    Bool(bool),
    U64(u64),
    I64(i64),
    F64(f64),
    String(String),
    List(Vec<Value>),
    Record(BTreeMap<String, serde_json::Value>),
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
            Value::Record(xs) => {
                write!(f, "{{")?;
                for (i, (field_name, x)) in xs.iter().enumerate() {
                    field_name.fmt(f)?;
                    write!(f, ": ")?;
                    x.fmt(f)?;
                    if i < xs.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")
            }
            Value::Function(function) => {
                write!(
                    f,
                    "({}: ",
                    match function.name.as_str() {
                        "++" => "(++)",
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
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub struct Function {
    pub id: Id,
    pub name: String,
    pub params: Vec<Type>,
    pub ret: Type,
}

impl From<serde_json::Value> for Value {
    fn from(value: serde_json::Value) -> Self {
        match value {
            serde_json::Value::Null => Value::Null,
            serde_json::Value::Bool(x) => Value::Bool(x),
            serde_json::Value::Number(x) => {
                if x.is_i64() {
                    Value::I64(x.as_i64().unwrap())
                } else if x.is_u64() {
                    Value::U64(x.as_u64().unwrap())
                } else {
                    Value::F64(x.as_f64().unwrap())
                }
            }
            serde_json::Value::String(x) => Value::String(x),
            serde_json::Value::Array(xs) => Value::List(xs.into_iter().map(Value::from).collect()),
            serde_json::Value::Object(xs) => Value::Record(xs.into_iter().collect()),
        }
    }
}

impl TryInto<serde_json::Value> for Value {
    type Error = serde_json::Error;

    fn try_into(self) -> Result<serde_json::Value, Self::Error> {
        match self {
            Value::Null => Ok(serde_json::Value::Null),
            Value::Bool(x) => Ok(serde_json::Value::Bool(x)),
            Value::U64(x) => Ok(serde_json::Value::Number(serde_json::Number::from(x))),
            Value::I64(x) => Ok(serde_json::Value::Number(serde_json::Number::from(x))),
            Value::F64(x) => Ok(serde_json::Value::Number(
                serde_json::Number::from_f64(x).unwrap(),
            )),
            Value::String(x) => Ok(serde_json::Value::String(x)),
            Value::List(xs) => Ok(serde_json::Value::Array(
                xs.into_iter()
                    .map(|x| x.try_into())
                    .collect::<Result<_, _>>()?,
            )),
            Value::Record(xs) => Ok(serde_json::Value::Object(xs.into_iter().collect())),
            Value::Function(_) => Err(serde_json::Error::custom(
                "cannot serialize function to JSON",
            )),
            Value::Lambda(_) => Err(serde_json::Error::custom("cannot serialize lambda to JSON")),
        }
    }
}

pub fn value_to_ir(val: Value, span: Span) -> IR {
    match val {
        Value::Null => IR::Null(span),
        Value::Bool(x) => IR::Bool(x, span),
        Value::U64(x) => IR::Uint(x, span),
        Value::I64(x) => IR::Int(x, span),
        Value::F64(x) => IR::Float(x, span),
        Value::String(x) => IR::String(x, span),
        Value::List(xs) => IR::List(
            xs.into_iter()
                .map(|x| value_to_ir(x, span.clone()))
                .collect(),
            span,
        ),
        Value::Record(xs) => IR::Record(xs, span),
        Value::Function(f) => IR::Variable(Variable {
            id: f.id,
            name: f.name,
            span,
        }),
        Value::Lambda(lam) => IR::Lambda(lam),
    }
}
