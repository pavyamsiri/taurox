use crate::{lexer::Span, string::Ident};
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum ResolutionErrorKind {
    #[error("Reading a local variable in its own initializer.")]
    SelfReferentialInitializer {
        destination: Ident,
        reference: Ident,
    },
    #[error("Classes can not self-inherit.")]
    SelfReferentialInheritance {
        destination: Ident,
        reference: Ident,
    },
    #[error("Shadowing a local.")]
    ShadowLocal { old: Ident, new: Ident },
    #[error("Returning in a non-function scope.")]
    NonFunctionReturn,
    #[error("Can't access `this` outside of a class.")]
    NonClassThis,
    #[error("Can't access `super` outside of a class.")]
    NonClassSuper,
    #[error("Constructors are not allowed to explicitly return values.")]
    ReturnInConstructor,
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
            ResolutionErrorKind::SelfReferentialInitializer { .. } => "RA001",
            ResolutionErrorKind::ShadowLocal { .. } => "RA002",
            ResolutionErrorKind::NonFunctionReturn => "RA003",
            ResolutionErrorKind::NonClassThis => "RA004",
            ResolutionErrorKind::ReturnInConstructor => "RA005",
            ResolutionErrorKind::SelfReferentialInheritance { .. } => "RA006",
            ResolutionErrorKind::NonClassSuper => "RA007",
        }
    }
}
