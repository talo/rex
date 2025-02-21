use rex_ast::expr::Expr;
use rex_type_system::types::Type;

// TODO(loong): re-implement traces so that developers can get meaningful
// errors when something goes wrong.
#[derive(Clone, Debug, thiserror::Error)]
pub enum Error {
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
