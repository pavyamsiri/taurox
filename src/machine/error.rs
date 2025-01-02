use crate::{compiler::DecodeError, value::error::RuntimeError};
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum VMError {
    #[error("Decode error {0}")]
    Decode(#[from] DecodeError),
    #[error("Runtime error {0}")]
    Runtime(#[from] RuntimeError),
    #[error("Only have {actual} out of expected {expected}")]
    MissingStackOperands { expected: usize, actual: usize },
}
