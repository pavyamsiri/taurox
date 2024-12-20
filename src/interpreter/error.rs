use crate::lexer::Span;
use compact_str::CompactString;
use thiserror::Error;

use super::value::LoxValue;

#[derive(Debug, Error, Clone)]
pub enum RuntimeErrorKind {
    #[error("Non-Number {{Unary}}: {0}")]
    NonNumeric(LoxValue),
    #[error("Non-Numbers {{Binary}}: [{0} , {1}]")]
    NonNumerics(LoxValue, LoxValue),
    #[error("Non-Numbers/Non-Strings {{Binary}}: [{0} , {1}]")]
    NonAddable(LoxValue, LoxValue),
    #[error("Invalid Access: {0}")]
    InvalidAccess(CompactString),
    #[error("Invalid Callee: {0}")]
    InvalidCallee(LoxValue),
    #[error("Invalid Argument Count: {actual} of {expected}")]
    InvalidArgumentCount { actual: usize, expected: usize },
}

#[derive(Debug, Error, Clone)]
#[error("{kind}")]
pub struct RuntimeError {
    pub kind: RuntimeErrorKind,
    pub span: Span,
}
