use super::statement::{Declaration, Statement};
use crate::lexer::{LexicalError, Span, Token, TokenKind};
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum ParserError {
    #[error("Expected {expected:?} but got token {actual:?}.")]
    UnexpectedToken { actual: Token, expected: TokenKind },
    #[error("Expected an operator but got token {0:?}.")]
    NonOperator(Token),
    #[error("Expected a block but got {0:?}.")]
    NonBlock(Statement),
    #[error("Expected an left hand side to expression but got token {0:?}.")]
    NonExpression(Token),
    #[error("Expected a non-EOF token.")]
    UnexpectedEof(Span),
    #[error("Expected a statement but got {0:?}.")]
    InvalidStatement(Token),
    #[error("Expected a non-declaration but got {0:?}.")]
    InvalidNonDeclaration(Declaration),
    #[error("Encountered an invalid l-value {0:?}.")]
    InvalidLValue(Token),
    #[error("Encountered a lexer error {0}.")]
    LexicalError(#[from] LexicalError),
}
