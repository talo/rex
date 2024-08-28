use rex_ast::{ast::Var, types::Type};

use crate::value::{Function, Value};

#[derive(Clone, Debug, thiserror::Error)]
pub enum Error {
    #[error("unresolved variable {var:?}")]
    UnexpectedVar { var: Var },

    #[error("expected callable, got `{got}`")]
    ExpectedCallable { got: Value },

    #[error("expected num, got `{got}`")]
    ExpectedNum { got: Value },

    #[error("expected cmp, got `{got}`")]
    ExpectedCmp { got: Value },

    #[error("expected concat, got `{got}`")]
    ExpectedConcat { got: Value },

    #[error("expected indexable, got `{got}`")]
    ExpectedIndexable { got: Value },

    #[error("expected {expected} arguments, got {got} arguments")]
    ExpectedArguments { expected: usize, got: usize },

    #[error("expected {expected}, got `{got}`")]
    UnexpectedType { expected: Type, got: Value },

    #[error("var not found `{var}`")]
    VarNotFound { var: Var },

    #[error("field not found `{field}`")]
    FieldNotFound { field: String },

    #[error("function not found `{function}`")]
    FunctionNotFound { function: Function },

    #[error("bad type")]
    BadType,

    #[error("index out of bounds")]
    IndexOutOfBounds,

    #[error("key not found")]
    KeyNotFound,
}
