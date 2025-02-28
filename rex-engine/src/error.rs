use rex_ast::expr::{Expr, Var};
use rex_type_system::types::Type;

// TODO(loong): re-implement traces so that developers can get meaningful
// errors when something goes wrong.
#[derive(Clone, Debug, thiserror::Error)]
pub enum Error {
    #[error("unimplemented")]
    Unimplemented,
    #[error("index out of bounds {index}")]
    IndexOutOfBounds { index: usize },
    #[error("variable not found {var}")]
    VarNotFound { var: Var },
    #[error("expected {expected}, got {got}")]
    ExpectedTypeGotValue { expected: Type, got: Expr },
    #[error("missing argument {argument}")]
    MissingArgument { argument: usize },
    #[error("{0}")]
    ParseIntError(#[from] std::num::ParseIntError),
    #[error("{0}")]
    ParseFloatError(#[from] std::num::ParseFloatError),
    #[error("cannot overload a parametrically polymorphic function")]
    ParametricOverload {
        name: String,
        prev_insts: Vec<Type>,
        curr_inst: Type,
    },
}
