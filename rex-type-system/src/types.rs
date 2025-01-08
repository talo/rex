use std::collections::HashMap;

use rex_ast::id::Id;

// Change from single type to vec of types
pub type TypeEnv = HashMap<String, Type>;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Var(Id),
    ForAll(Id, Box<Type>),

    ADT(ADT),
    Arrow(Box<Type>, Box<Type>),
    Result(Box<Type>, Box<Type>),
    Option(Box<Type>),
    List(Box<Type>),
    Dict(HashMap<String, Type>),
    Tuple(Vec<Type>),

    Bool,
    Uint,
    Int,
    Float,
    String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ADT {
    pub name: String,
    pub variants: Vec<ADTVariant>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ADTVariant {
    pub name: String,
    pub t: Option<Box<Type>>,
}
