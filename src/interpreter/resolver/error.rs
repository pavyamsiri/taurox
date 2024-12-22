use crate::lexer::Span;
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum ResolutionErrorKind {
    #[error("Reading a local variable in its own initializer.")]
    SelfReferentialInitializer,
}

#[derive(Debug, Error, Clone)]
#[error("{kind}")]
pub struct ResolutionError {
    pub kind: ResolutionErrorKind,
    pub span: Span,
}
