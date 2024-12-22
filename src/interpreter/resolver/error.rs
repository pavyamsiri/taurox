use crate::lexer::Span;
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum ResolutionErrorKind {
    #[error("Reading a local variable in its own initializer.")]
    SelfReferentialInitializer,
    #[error("Shadowing a local.")]
    ShadowLocal,
    #[error("Returning in a non-function scope.")]
    NonFunctionReturn,
}

#[derive(Debug, Error, Clone)]
#[error("{kind}")]
pub struct ResolutionError {
    pub kind: ResolutionErrorKind,
    pub span: Span,
}
