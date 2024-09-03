use std::{
    collections::BTreeMap,
    fmt::{self, Display, Formatter},
};

use rex_ast::{
    ast::Lambda,
    id::Id,
    types::{ADTVariantFields, Type, ADT},
};

use crate::Context;

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub enum Value {
    // Basics
    Null,
    Bool(bool),
    Uint(u64),
    Int(i64),
    Float(f64),
    String(String),
    List(Vec<Value>),
    Tuple(Vec<Value>),
    Dict(BTreeMap<String, Value>),

    // Vars
    Id(Id),

    // Functions
    Function(Function),
    Closure(Closure),

    // Data
    Data(Data),
}

impl Value {
    pub fn implements(&self, t: &Type) -> bool {
        match (self, t) {
            (Self::Null, Type::Null) => true,
            (Self::Bool(_), Type::Bool) => true,
            (Self::Uint(_), Type::Uint) => true,
            (Self::Int(_), Type::Int) => true,
            (Self::Float(_), Type::Float) => true,
            (Self::String(_), Type::String) => true,
            (Self::List(xs), Type::List(t)) => xs.iter().all(|x| x.implements(t)),
            (Self::Tuple(xs), Type::Tuple(ts)) => {
                xs.len() == ts.len() && xs.iter().zip(ts).all(|(x, t)| x.implements(t))
            }
            (Self::Dict(xs), Type::Dict(ts)) => {
                xs.len() == ts.len()
                    && xs
                        .iter()
                        .all(|(k, v)| ts.get(k).map_or(false, |t| v.implements(t)))
            }

            // FIXE: Until type inference is implemented, we cannot know if a
            // value is compatible with a generic type. For now, we just assume
            // that it is
            (_, Type::Generic(_)) => true,

            (Self::Function(function), t) => function.implements(t),
            (Self::Closure(closure), t) => closure.implements(t),
            (Self::Data(data), Type::ADT(adt)) => data.implements(adt),
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            // Basics
            Self::Null => "null".fmt(f),
            Self::Bool(x) => x.fmt(f),
            Self::Uint(x) => x.fmt(f),
            Self::Int(x) => x.fmt(f),
            Self::Float(x) => x.fmt(f),
            Self::String(x) => x.fmt(f),
            Self::List(xs) => {
                '['.fmt(f)?;
                for (i, x) in xs.iter().enumerate() {
                    x.fmt(f)?;
                    if i + 1 < xs.len() {
                        ", ".fmt(f)?;
                    }
                }
                ']'.fmt(f)
            }
            Self::Tuple(xs) => {
                '('.fmt(f)?;
                for (i, x) in xs.iter().enumerate() {
                    x.fmt(f)?;
                    if i + 1 < xs.len() {
                        ", ".fmt(f)?;
                    }
                }
                ')'.fmt(f)
            }
            Self::Dict(xs) => {
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

            // Vars
            Self::Id(id) => id.fmt(f),

            // Functions
            Self::Function(function) => function.fmt(f),
            Self::Closure(closure) => closure.fmt(f),

            // Data
            Self::Data(data) => data.fmt(f),
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

impl Function {
    pub fn implements(&self, t: &Type) -> bool {
        match t {
            Type::Arrow(a, b) => match self.params.len() {
                0 => false,
                1 => a.as_ref() == &self.params[0] && b.as_ref() == &self.ret,
                _ => {
                    a.as_ref() == &self.params[0]
                        && Function {
                            id: self.id,
                            name: self.name.clone(),
                            params: self.params.iter().skip(1).cloned().collect(),
                            ret: self.ret.clone(),
                        }
                        .implements(b.as_ref())
                }
            },
            t if self.params.is_empty() => t == &self.params[0],
            _ => false,
        }
    }
}

impl Display for Function {
    fn fmt(&self, _f: &mut Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub enum FunctionLike {
    Function(Function),
    Lambda(Lambda),
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub struct Closure {
    pub captured_ctx: Context,
    pub captured_args: Vec<Value>,
    pub body: FunctionLike,
}

impl Closure {
    pub fn implements(&self, t: &Type) -> bool {
        match &self.body {
            FunctionLike::Function(function) => Function {
                id: function.id,
                name: function.name.clone(),
                params: function
                    .params
                    .iter()
                    .skip(self.captured_args.len())
                    .cloned()
                    .collect(),
                ret: function.ret.clone(),
            }
            .implements(t),
            FunctionLike::Lambda(_lam) => match t {
                // FIXME: Until we implement type inference, we cannot know if
                // this lambda is actually implements the arrow type (and thus,
                // this check gets deferred until the lambda continue to
                // evaluate)
                Type::Arrow(..) => true,
                _ => false,
            },
        }
    }
}

impl Display for Closure {
    fn fmt(&self, _f: &mut Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

#[derive(Clone, Debug)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub struct Data {
    pub name: String,
    pub fields: Option<DataFields>,
}

impl Data {
    pub fn new(name: String, fields: impl Into<DataFields>) -> Self {
        Self {
            name,
            fields: Some(fields.into()),
        }
    }

    pub fn implements(&self, adt: &ADT) -> bool {
        if !adt.generics.is_empty() {
            // FIXME: We do not currently support generic ADTs
            return false;
        }
        for variant in &adt.variants {
            if self.name != variant.name {
                continue;
            }
            match (&self.fields, &variant.fields) {
                (None, None) => return true,
                (
                    Some(DataFields::Named(named_fields)),
                    Some(ADTVariantFields::Named(named_variant_fields)),
                ) => {
                    if named_fields.fields.len() != named_variant_fields.fields.len() {
                        continue;
                    }
                    if named_fields
                        .fields
                        .iter()
                        .zip(named_variant_fields.fields.iter())
                        .all(|((k1, v), (k2, t))| k1 == k2 && v.implements(t))
                    {
                        return true;
                    }
                }
                (
                    Some(DataFields::Unnamed(unnamed_fields)),
                    Some(ADTVariantFields::Unnamed(unnamed_variant_fields)),
                ) => {
                    if unnamed_fields.fields.len() != unnamed_variant_fields.fields.len() {
                        continue;
                    }
                    if unnamed_fields
                        .fields
                        .iter()
                        .zip(unnamed_variant_fields.fields.iter())
                        .all(|(v, t)| v.implements(t))
                    {
                        return true;
                    }
                }
                _ => continue,
            }
        }
        false
    }
}

impl PartialEq<Data> for Data {
    fn eq(&self, other: &Data) -> bool {
        self.name == other.name && self.fields == other.fields
    }
}

impl Display for Data {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.name.fmt(f)?;
        if let Some(fields) = &self.fields {
            ' '.fmt(f)?;
            fields.fmt(f)?;
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub enum DataFields {
    Named(NamedDataFields),
    Unnamed(UnnamedDataFields),
}

impl From<NamedDataFields> for DataFields {
    fn from(fields: NamedDataFields) -> Self {
        DataFields::Named(fields)
    }
}

impl From<UnnamedDataFields> for DataFields {
    fn from(fields: UnnamedDataFields) -> Self {
        DataFields::Unnamed(fields)
    }
}

impl Display for DataFields {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Named(named_fields) => named_fields.fmt(f),
            Self::Unnamed(unnamed_fields) => unnamed_fields.fmt(f),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub struct NamedDataFields {
    pub fields: BTreeMap<String, Value>,
}

impl NamedDataFields {
    pub fn new(fields: impl Into<BTreeMap<String, Value>>) -> Self {
        Self {
            fields: fields.into(),
        }
    }
}

impl Display for NamedDataFields {
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

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub struct UnnamedDataFields {
    pub fields: Vec<Value>,
}

impl UnnamedDataFields {
    pub fn new(fields: impl Into<Vec<Value>>) -> Self {
        Self {
            fields: fields.into(),
        }
    }
}

impl Display for UnnamedDataFields {
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
