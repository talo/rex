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
            fields: Some($crate::types::ADTVariantFields::Named($crate::types::ADTVariantNamedFields {
                fields,
            })),
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
            variants: vec![$(adt_variant_with_named_fields!{ $v $vs }),*],
        }
    }};
}

#[macro_export]
macro_rules! arrow {
    ($a:expr => $b:expr) => {
        $crate::types::Type::Arrow(Box::new($a), Box::new($b))
    };
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
    Option(Box<Type>),
    Result(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Dict(BTreeMap<String, Type>),
    Arrow(Box<Type>, Box<Type>),
    Generic(Generic),
    List(Box<Type>),
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
            Type::ADT(x) => x.fmt(f),
        }
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
                    for (i, (k, v)) in fields.fields.iter().enumerate() {
                        k.fmt(f)?;
                        " = ".fmt(f)?;
                        v.fmt(f)?;
                        if i + 1 < fields.fields.len() {
                            ", ".fmt(f)?;
                        }
                    }
                    '}'.fmt(f)
                }
                ADTVariantFields::Unnamed(fields) => {
                    ' '.fmt(f)?;
                    for (i, x) in fields.fields.iter().enumerate() {
                        '('.fmt(f)?;
                        x.fmt(f)?;
                        ')'.fmt(f)?;
                        if i + 1 < fields.fields.len() {
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
    Named(ADTVariantNamedFields),
    Unnamed(ADTVariantUnnamedFields),
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub struct ADTVariantNamedFields {
    pub fields: BTreeMap<String, Type>,
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub struct ADTVariantUnnamedFields {
    pub fields: Vec<Type>,
}
