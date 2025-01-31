use rex_type_system::types::Type;

use crate::eval::Value;

#[derive(Clone, Debug, thiserror::Error)]
pub enum Error {
    #[error("expected {expected}, got {got}")]
    ExpectedTypeGotValue { expected: Type, got: Value },
    #[error("missing argument {argument}")]
    MissingArgument { argument: usize },
}
