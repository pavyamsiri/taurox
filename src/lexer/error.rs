use crate::token::Span;
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum LexicalErrorKind {
    #[error("Unrecognized character {0}")]
    Unrecognized(char),
    #[error("Unterminated string literal")]
    UnclosedString,
}

#[derive(Debug, Error, Clone)]
#[error("[line {line}] {kind}")]
pub struct LexicalError {
    #[source]
    pub kind: LexicalErrorKind,
    pub span: Span,
    pub line: u32,
}
