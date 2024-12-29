use super::LoxValue;
use crate::lexer::Span;
use crate::string::IdentName;
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum RuntimeErrorKind {
    #[error("Non-Number {{Unary}}: {0}")]
    NonNumeric(LoxValue),
    #[error("Non-Numbers {{Binary}}: [{0} , {1}]")]
    NonNumerics(LoxValue, LoxValue),
    #[error("Non-Numbers/Non-Strings {{Binary}}: [{0} , {1}]")]
    NonAddable(LoxValue, LoxValue),
    #[error("Invalid Access: {0}")]
    InvalidAccess(IdentName),
    #[error("Invalid Callee: {0}")]
    InvalidCallee(LoxValue),
    #[error("Invalid Instance: {0}")]
    InvalidInstance(LoxValue),
    #[error("Undefined property access: {object}")]
    UndefinedProperty { object: LoxValue, name: IdentName },
    #[error("Invalid Argument Count: {actual} of {expected}")]
    InvalidArgumentCount { actual: usize, expected: usize },
    #[error("Super class must be a class: {0}")]
    InvalidSuperClass(IdentName),
}

#[derive(Debug, Error, Clone)]
#[error("{kind}")]
pub struct RuntimeError {
    pub kind: RuntimeErrorKind,
    pub span: Span,
}

impl RuntimeError {
    pub fn code(&self) -> &'static str {
        match self.kind {
            RuntimeErrorKind::NonNumeric(_) => "RT001",
            RuntimeErrorKind::NonNumerics(_, _) => "RT002",
            RuntimeErrorKind::NonAddable(_, _) => "RT003",
            RuntimeErrorKind::InvalidAccess(_) => "RT004",
            RuntimeErrorKind::InvalidCallee(_) => "RT005",
            RuntimeErrorKind::InvalidArgumentCount { .. } => "RT006",
            RuntimeErrorKind::InvalidInstance(_) => "RT007",
            RuntimeErrorKind::UndefinedProperty { .. } => "RT008",
            RuntimeErrorKind::InvalidSuperClass(_) => "RT009",
        }
    }
}
