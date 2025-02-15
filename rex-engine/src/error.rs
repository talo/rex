use rex_ast::expr::Expr;
use rex_type_system::types::Type;

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
}
