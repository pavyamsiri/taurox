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

impl LexicalError {
    pub const fn code(&self) -> &'static str {
        match self.kind {
            LexicalErrorKind::Unrecognized(_) => "L001",
            LexicalErrorKind::UnclosedString => "L002",
        }
    }
}
