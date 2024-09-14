use std::{
    collections::BTreeMap,
    fmt::{self, Display, Formatter},
};

#[macro_export]
macro_rules! a {
    () => {
        $crate::types::Type::Generic($crate::types::Generic {
            name: "a".to_string(),
        })
    };
}

#[macro_export]
macro_rules! b {
    () => {
        $crate::types::Type::Generic($crate::types::Generic {
            name: "b".to_string(),
        })
    };
}

#[macro_export]
macro_rules! c {
    () => {
        $crate::types::Type::Generic($crate::types::Generic {
            name: "c".to_string(),
        })
    };
}

#[macro_export]
macro_rules! list {
    ($a:expr) => {
        $crate::types::Type::List(Box::new($a))
    };
}

#[macro_export]
macro_rules! tuple {
    ($($a:expr),*) => {
        $crate::types::Type::Tuple(vec![$($a),*])
    };
}

#[macro_export]
macro_rules! dict {
    ($($key:expr => $value:expr),*) => {
        $crate::types::Type::Dict({
            let mut map = ::std::collections::BTreeMap::new();
            $(map.insert($key.to_string(), $value);)*
            map
        })
    };
}

#[macro_export]
macro_rules! adt_variant_with_named_fields {
    ($name:ident { $($key:ident = $value:expr),* }) => {{
        let mut fields = ::std::collections::BTreeMap::new();
        $(fields.insert(stringify!($key).to_string(), $value);)*
        $crate::types::ADTVariant {
            name: stringify!($name).to_string(),
            fields: Some($crate::types::ADTVariantFields::Named(fields)),
        }
    }};
}

#[macro_export]
macro_rules! adt {
    ($name:ident = | $($v:ident $vs:tt)|*) => {{
        adt!{ $name = $($v $vs)|* }
    }};
    ($name:ident = $($v:ident $vs:tt)|*) => {{
        $crate::types::ADT {
            name: stringify!($name).to_string(),
            generics: vec![],
            variants: vec![$($crate::adt_variant_with_named_fields!{ $v $vs }),*],
        }
    }};
    ($name:ident = | $($v:ident)|*) => {{
        adt!{ $name = $($v)|* }
    }};
    ($name:ident = $($v:ident)|*) => {{
        $crate::types::ADT {
            name: stringify!($name).to_string(),
            generics: vec![],
            variants: vec![$($crate::types::ADTVariant{ name: stringify!($v).to_string(), fields: None }),*],
        }
    }};
}

#[macro_export]
macro_rules! arrow {
    ($a:expr => $b:expr) => {
        $crate::types::Type::Arrow(Box::new($a), Box::new($b))
    };
}

pub trait TypeInfo {
    fn t() -> Type;
}

impl TypeInfo for () {
    fn t() -> Type {
        Type::Null
    }
}

impl TypeInfo for bool {
    fn t() -> Type {
        Type::Bool
    }
}

impl TypeInfo for u8 {
    fn t() -> Type {
        Type::Uint
    }
}

impl TypeInfo for u16 {
    fn t() -> Type {
        Type::Uint
    }
}

impl TypeInfo for u32 {
    fn t() -> Type {
        Type::Uint
    }
}

impl TypeInfo for u64 {
    fn t() -> Type {
        Type::Uint
    }
}

impl TypeInfo for i8 {
    fn t() -> Type {
        Type::Int
    }
}

impl TypeInfo for i16 {
    fn t() -> Type {
        Type::Int
    }
}

impl TypeInfo for i32 {
    fn t() -> Type {
        Type::Int
    }
}

impl TypeInfo for i64 {
    fn t() -> Type {
        Type::Int
    }
}

impl TypeInfo for f32 {
    fn t() -> Type {
        Type::Float
    }
}

impl TypeInfo for f64 {
    fn t() -> Type {
        Type::Float
    }
}

impl TypeInfo for String {
    fn t() -> Type {
        Type::String
    }
}

impl<T: TypeInfo> TypeInfo for Option<T> {
    fn t() -> Type {
        Type::Option(Box::new(T::t()))
    }
}

impl<T: TypeInfo, E: TypeInfo> TypeInfo for Result<T, E> {
    fn t() -> Type {
        Type::Result(Box::new(T::t()), Box::new(E::t()))
    }
}

impl<T0: TypeInfo> TypeInfo for (T0,) {
    fn t() -> Type {
        Type::Tuple(vec![T0::t()])
    }
}

impl<T0: TypeInfo, T1: TypeInfo> TypeInfo for (T0, T1) {
    fn t() -> Type {
        Type::Tuple(vec![T0::t(), T1::t()])
    }
}

impl<T0: TypeInfo, T1: TypeInfo, T2: TypeInfo> TypeInfo for (T0, T1, T2) {
    fn t() -> Type {
        Type::Tuple(vec![T0::t(), T1::t(), T2::t()])
    }
}

impl<T0: TypeInfo, T1: TypeInfo, T2: TypeInfo, T3: TypeInfo> TypeInfo for (T0, T1, T2, T3) {
    fn t() -> Type {
        Type::Tuple(vec![T0::t(), T1::t(), T2::t(), T3::t()])
    }
}

impl<T: TypeInfo, const N: usize> TypeInfo for [T; N] {
    fn t() -> Type {
        Type::List(Box::new(T::t()))
    }
}

impl<T: TypeInfo> TypeInfo for &[T] {
    fn t() -> Type {
        Type::List(Box::new(T::t()))
    }
}

impl<T: TypeInfo> TypeInfo for Vec<T> {
    fn t() -> Type {
        Type::List(Box::new(T::t()))
    }
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub enum Type {
    Null,
    Bool,
    Uint,
    Int,
    Float,
    String,
    Ptr(Box<Type>),
    Option(Box<Type>),
    Result(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Dict(BTreeMap<String, Type>),
    Arrow(Box<Type>, Box<Type>),
    Generic(Generic),
    List(Box<Type>),
    Symbol(String),
    Alias(Alias),
    ADT(ADT),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Type::Null => "null".fmt(f),
            Type::Bool => "bool".fmt(f),
            Type::Uint => "uint".fmt(f),
            Type::Int => "int".fmt(f),
            Type::Float => "float".fmt(f),
            Type::String => "string".fmt(f),
            Type::Ptr(t) => {
                "Ptr<".fmt(f)?;
                t.fmt(f)?;
                '>'.fmt(f)
            }
            Type::Option(x) => {
                "Option<".fmt(f)?;
                x.fmt(f)?;
                '>'.fmt(f)
            }
            Type::Result(a, b) => {
                "Result<".fmt(f)?;
                a.fmt(f)?;
                ", ".fmt(f)?;
                b.fmt(f)?;
                '>'.fmt(f)
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
                " -> ".fmt(f)?;
                b.fmt(f)
            }
            Type::Generic(x) => x.fmt(f),
            Type::List(x) => {
                '['.fmt(f)?;
                x.fmt(f)?;
                ']'.fmt(f)
            }
            Type::Symbol(x) => {
                '$'.fmt(f)?;
                x.fmt(f)
            }
            Type::Alias(x) => x.fmt(f),
            Type::ADT(x) => x.fmt(f),
        }
    }
}

impl From<Generic> for Type {
    fn from(x: Generic) -> Self {
        Type::Generic(x)
    }
}

impl From<Alias> for Type {
    fn from(x: Alias) -> Self {
        Type::Alias(x)
    }
}

impl From<ADT> for Type {
    fn from(x: ADT) -> Self {
        Type::ADT(x)
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub struct Generic {
    pub name: String,
}

impl Display for Generic {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.name.fmt(f)
    }
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub struct Alias {
    pub name: String,
    pub t: Box<Type>,
}

impl Alias {
    pub fn new(name: impl ToString, t: impl Into<Box<Type>>) -> Self {
        Self {
            name: name.to_string(),
            t: t.into(),
        }
    }
}

impl Display for Alias {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.name.fmt(f)?;
        '('.fmt(f)?;
        self.t.fmt(f)?;
        ')'.fmt(f)
    }
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub struct ADT {
    pub name: String,
    pub generics: Vec<Generic>,
    pub variants: Vec<ADTVariant>,
}

impl Display for ADT {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.name.fmt(f)?;
        if !self.generics.is_empty() {
            ' '.fmt(f)?;
            for (i, x) in self.generics.iter().enumerate() {
                x.fmt(f)?;
                if i + 1 < self.generics.len() {
                    ' '.fmt(f)?;
                }
            }
        }
        " = ".fmt(f)?;
        for (i, x) in self.variants.iter().enumerate() {
            x.fmt(f)?;
            if i + 1 < self.variants.len() {
                " | ".fmt(f)?;
            }
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
pub struct ADTVariant {
    pub name: String,
    pub fields: Option<ADTVariantFields>,
}

impl Display for ADTVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.name.fmt(f)?;
        if let Some(fields) = &self.fields {
            match fields {
                ADTVariantFields::Named(fields) => {
                    ' '.fmt(f)?;
                    '{'.fmt(f)?;
                    for (i, (k, v)) in fields.iter().enumerate() {
                        k.fmt(f)?;
                        " = ".fmt(f)?;
                        v.fmt(f)?;
                        if i + 1 < fields.len() {
                            ", ".fmt(f)?;
                        }
                    }
                    '}'.fmt(f)
                }
                ADTVariantFields::Unnamed(fields) => {
                    ' '.fmt(f)?;
                    for (i, x) in fields.iter().enumerate() {
                        '('.fmt(f)?;
                        x.fmt(f)?;
                        ')'.fmt(f)?;
                        if i + 1 < fields.len() {
                            ' '.fmt(f)?;
                        }
                    }
                    Ok(())
                }
            }
        } else {
            Ok(())
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub enum ADTVariantFields {
    Named(BTreeMap<String, Type>),
    Unnamed(Vec<Type>),
}
