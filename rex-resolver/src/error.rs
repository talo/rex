#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("use of undefined variable {0}")]
    UseOfUndefined(String),
}
