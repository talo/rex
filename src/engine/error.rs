#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum Error {
    #[error("custom error: {message}")]
    Custom { message: String },
    #[error("type error: expected {expected}, got {got}")]
    Type { expected: String, got: String },
    #[error("variable not found: {name}")]
    VarNotFound { name: String },
    #[error("division by zero")]
    DivByZero,
}
