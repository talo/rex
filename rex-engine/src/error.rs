use rex_ast::expr::{Expr, Var};
use rex_lexer::span::Span;
use rex_parser::error::ParserErr;
use rex_type_system::types::Type;
use serde_json::Value;

// TODO(loong): re-implement traces so that developers can get meaningful
// errors when something goes wrong.
#[derive(Clone, Debug, thiserror::Error)]
pub enum Error {
    #[error("unexpected token {0}")]
    UnexpectedToken(Span),
    #[error("parsing failed: {0:?}")]
    Parser(Vec<ParserErr>),
    #[error("type inference failed: {0:?}")]
    TypeInference(String),
    #[error("variable not found {var}")]
    VarNotFound { var: Var },
    #[error("expected {expected}, got {got}")]
    ExpectedTypeGotValue { expected: Type, got: Expr },
    #[error("expected {expected}, got {got}")]
    ExpectedTypeGotJSON { expected: Type, got: Value },
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
    #[error("{0}")]
    RegexCompilationError(#[from] regex::Error),
}
