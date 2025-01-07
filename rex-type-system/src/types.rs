use std::collections::HashMap;

use rex_ast::id::Id;

// Change from single type to vec of types
pub type TypeEnv = HashMap<String, Type>;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Var(Id),
    ForAll(Id, Box<Type>),
    Arrow(Box<Type>, Box<Type>),
    List(Box<Type>),
    Tuple(Vec<Type>),
    Int,
    Bool,
}
