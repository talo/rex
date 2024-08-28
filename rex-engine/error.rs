#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
#[cfg_attr(
    feature = "serde",
    derive(serde::Deserialize, serde::Serialize),
    serde(rename_all = "lowercase")
)]
pub enum Error {
    #[error("{0}")]
    Custom(String),
    #[error("type error: expected {expected}, got {got}")]
    Type { expected: String, got: String },
    #[error("variable not found: {name}")]
    VarNotFound { name: String },
    #[error("field not found: {name}")]
    FieldNotFound { name: String },
    #[error("division by zero")]
    DivByZero,
    #[error("index out of bounds: {index}")]
    IndexOutOfBounds { index: u64 },
}
