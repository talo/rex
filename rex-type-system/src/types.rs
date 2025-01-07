use std::collections::HashMap;

use rex_ast::id::Id;

// Change from single type to vec of types
pub type TypeEnv = HashMap<String, Vec<Type>>;

// Helper to add overloaded type
pub fn add_overload(env: &mut TypeEnv, name: &str, ty: Type) {
    env.entry(name.to_string())
        .or_insert_with(Vec::new)
        .push(ty);
}

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
