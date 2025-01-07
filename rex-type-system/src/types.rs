pub type TypeId = u64;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Var(TypeId),
    Arrow(Box<Type>, Box<Type>),
    Int,
    Bool,
}
