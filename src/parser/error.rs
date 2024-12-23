use super::statement::{Declaration, Statement};
use crate::lexer::{LexicalError, Span, Token, TokenKind};
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum GeneralParserError {
    #[error("Expected {expected:?} but got token {actual:?}.")]
    UnexpectedToken { actual: Token, expected: TokenKind },
    #[error("Expected a non-EOF token.")]
    UnexpectedEof(Span),
    #[error(transparent)]
    LexicalError(#[from] LexicalError),
}

impl GeneralParserError {
    pub fn code(&self) -> &'static str {
        match self {
            Self::UnexpectedToken { .. } => "PG001",
            Self::UnexpectedEof(_) => "PG002",
            Self::LexicalError(_) => "PG003",
        }
    }
}

#[derive(Debug, Error, Clone)]
pub enum ExpressionParserError {
    #[error("Expected an left hand side to expression but got token {0:?}.")]
    NonExpression(Token),
    #[error("Encountered an invalid l-value {0:?}.")]
    InvalidLValue(Token),
}

impl ExpressionParserError {
    pub fn code(&self) -> &'static str {
        match self {
            Self::NonExpression(_) => "PE002",
            Self::InvalidLValue(_) => "PE003",
        }
    }
}

#[derive(Debug, Error, Clone)]
pub enum GeneralExpressionParserError {
    #[error(transparent)]
    Inner(#[from] ExpressionParserError),
    #[error(transparent)]
    General(#[from] GeneralParserError),
}

impl GeneralExpressionParserError {
    pub fn code(&self) -> &'static str {
        match self {
            GeneralExpressionParserError::Inner(error) => error.code(),
            GeneralExpressionParserError::General(error) => error.code(),
        }
    }
}

impl From<LexicalError> for GeneralExpressionParserError {
    fn from(value: LexicalError) -> Self {
        Self::General(GeneralParserError::LexicalError(value))
    }
}

#[derive(Debug, Error, Clone)]
pub enum StatementParserError {
    #[error("Expected a block but got {0:?}.")]
    NonBlock(Statement),
    #[error("Expected a statement but got {0:?}.")]
    InvalidStatement(Token),
    #[error("Expected a non-declaration but got {0:?}.")]
    InvalidNonDeclaration(Declaration),
}

#[derive(Debug, Error, Clone)]
pub enum ParserError {
    #[error(transparent)]
    Expression(#[from] ExpressionParserError),
    // Statement parser errors
    #[error(transparent)]
    Statement(#[from] StatementParserError),
    // Generic parser errors
    #[error(transparent)]
    General(#[from] GeneralParserError),
}

impl From<GeneralExpressionParserError> for ParserError {
    fn from(value: GeneralExpressionParserError) -> Self {
        match value {
            GeneralExpressionParserError::Inner(error) => Self::Expression(error),
            GeneralExpressionParserError::General(error) => Self::General(error),
        }
    }
}

impl From<LexicalError> for ParserError {
    fn from(value: LexicalError) -> Self {
        Self::General(GeneralParserError::LexicalError(value))
    }
}
