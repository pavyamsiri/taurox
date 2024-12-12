use super::statement::{Declaration, Statement};
use crate::lexer::{LexicalError, TokenKind};
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum ParserErrorKind {
    #[error("Expected {expected:?} but got token {actual:?}.")]
    UnexpectedToken {
        actual: TokenKind,
        expected: TokenKind,
    },
    #[error("Expected an operator but got token {0:?}.")]
    NonOperator(TokenKind),
    #[error("Expected a block but got {0:?}.")]
    NonBlock(Statement),
    #[error("Expected an left hand side to expression but got token {0:?}.")]
    NonExpression(TokenKind),
    #[error("Expected a non-EOF token.")]
    UnexpectedEof,
    #[error("Expected a statement but got {0}.")]
    InvalidStatement(TokenKind),
    #[error("Expected a non-declaration but got {0:?}.")]
    InvalidNonDeclaration(Declaration),
    #[error("Encountered an invalid l-value {0}.")]
    InvalidLValue(TokenKind),
    #[error("Encountered a lexer error {0}.")]
    LexicalError(#[from] LexicalError),
}

#[derive(Debug, Error, Clone)]
#[error("[line {line}] {kind}")]
pub struct ParserError {
    #[source]
    pub kind: ParserErrorKind,
    pub line: u32,
}
