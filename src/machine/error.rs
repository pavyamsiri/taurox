use crate::{compiler::DecodeError, lexer::Span, string::IdentName};
use thiserror::Error;

use super::value::VMValue;

#[derive(Debug, Error, Clone)]
pub enum VMError {
    #[error("Decode error {0}")]
    Decode(#[from] DecodeError),
    #[error("Runtime error {0}")]
    Runtime(#[from] VMRuntimeError),
    #[error("Only have {actual} out of expected {expected}")]
    MissingStackOperands { expected: usize, actual: usize },
    #[error("Invalid stack slot {slot} for stack of size {size}")]
    InvalidSlot { slot: usize, size: usize },
}

#[derive(Debug, Error, Clone)]
pub enum VMRuntimeErrorKind {
    #[error("Non-Number {{Unary}}: {0}")]
    NonNumeric(VMValue),
    #[error("Non-Numbers {{Binary}}: [{0} , {1}]")]
    NonNumerics(VMValue, VMValue),
    #[error("Non-Numbers/Non-Strings {{Binary}}: [{0} , {1}]")]
    NonAddable(VMValue, VMValue),
    #[error("Invalid Access: {0}")]
    InvalidAccess(IdentName),
    #[error("Invalid Callee: {0}")]
    InvalidCallee(VMValue),
    #[error("Invalid Instance Set: {0}")]
    InvalidInstanceSet(VMValue),
    #[error("Invalid Instance Get: {0}")]
    InvalidInstanceGet(VMValue),
    #[error("Undefined property access: {object}")]
    UndefinedProperty { object: VMValue, name: IdentName },
    #[error("Invalid Argument Count: {actual} of {expected}")]
    InvalidArgumentCount { actual: usize, expected: usize },
    #[error("Super class must be a class: {0}")]
    InvalidSuperClass(IdentName),
}

#[derive(Debug, Error, Clone)]
#[error("{kind}")]
pub struct VMRuntimeError {
    pub kind: VMRuntimeErrorKind,
    pub span: Span,
}

impl VMRuntimeError {
    pub fn code(&self) -> &'static str {
        type Kind = VMRuntimeErrorKind;
        match self.kind {
            Kind::NonNumeric(_) => "RT001",
            Kind::NonNumerics(_, _) => "RT002",
            Kind::NonAddable(_, _) => "RT003",
            Kind::InvalidAccess(_) => "RT004",
            Kind::InvalidCallee(_) => "RT005",
            Kind::InvalidArgumentCount { .. } => "RT006",
            Kind::InvalidInstanceSet(_) => "RT007",
            Kind::UndefinedProperty { .. } => "RT008",
            Kind::InvalidSuperClass(_) => "RT009",
            Kind::InvalidInstanceGet(_) => "RT010",
        }
    }
}
