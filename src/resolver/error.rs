use crate::{lexer::Span, string::Ident};
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum ResolutionErrorKind {
    #[error("Reading a local variable in its own initializer.")]
    SelfReferentialInitializer,
    #[error("Shadowing a local.")]
    ShadowLocal { old: Ident, new: Ident },
    #[error("Returning in a non-function scope.")]
    NonFunctionReturn,
}

#[derive(Debug, Error, Clone)]
#[error("{kind}")]
pub struct ResolutionError {
    pub kind: ResolutionErrorKind,
    pub span: Span,
}

impl ResolutionError {
    pub fn code(&self) -> &'static str {
        match self.kind {
            ResolutionErrorKind::SelfReferentialInitializer => "RA001",
            ResolutionErrorKind::ShadowLocal { .. } => "RA002",
            ResolutionErrorKind::NonFunctionReturn => "RA003",
        }
    }
}
