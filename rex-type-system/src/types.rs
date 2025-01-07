use rex_ast::id::Id;

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
