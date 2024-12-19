use super::token::Span;
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum LexicalErrorKind {
    #[error("Unrecognized character {0}")]
    Unrecognized(char),
    #[error("Unterminated string literal")]
    UnclosedString,
}

#[derive(Debug, Error, Clone)]
#[error("{kind}")]
pub struct LexicalError {
    #[source]
    pub kind: LexicalErrorKind,
    pub span: Span,
}
