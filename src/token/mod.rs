pub mod formatter;

use std::collections::HashMap;
use std::fmt::Display;
use std::ops::Range;
use std::sync::LazyLock;

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SpanIndex(u32);

impl From<SpanIndex> for usize {
    fn from(value: SpanIndex) -> Self {
        value.0 as usize
    }
}

impl From<usize> for SpanIndex {
    fn from(value: usize) -> Self {
        Self(value as u32)
    }
}

impl std::ops::Add<SpanLength> for SpanIndex {
    type Output = Self;

    fn add(self, rhs: SpanLength) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl std::ops::Add<usize> for SpanIndex {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        Self(self.0 + rhs as u32)
    }
}

impl std::ops::Sub<SpanIndex> for SpanIndex {
    type Output = SpanLength;

    fn sub(self, rhs: SpanIndex) -> Self::Output {
        SpanLength(self.0 - rhs.0)
    }
}

impl std::ops::Sub<SpanIndex> for usize {
    type Output = SpanLength;

    fn sub(self, rhs: SpanIndex) -> Self::Output {
        SpanLength(self as u32 - rhs.0)
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SpanLength(u32);

impl From<SpanLength> for usize {
    fn from(value: SpanLength) -> Self {
        value.0 as usize
    }
}

impl From<usize> for SpanLength {
    fn from(value: usize) -> Self {
        Self(value as u32)
    }
}

impl std::ops::Sub<usize> for SpanLength {
    type Output = SpanLength;

    fn sub(self, rhs: usize) -> Self::Output {
        SpanLength(self.0 - rhs as u32)
    }
}

/// The hashmap for keywords
pub static KEYWORD_HASHMAP: LazyLock<HashMap<&'static str, TokenKind>> = LazyLock::new(|| {
    let mut map = HashMap::new();
    map.insert("and", TokenKind::KeywordAnd);
    map.insert("class", TokenKind::KeywordClass);
    map.insert("else", TokenKind::KeywordElse);
    map.insert("false", TokenKind::KeywordFalse);
    map.insert("for", TokenKind::KeywordFor);
    map.insert("fun", TokenKind::KeywordFun);
    map.insert("if", TokenKind::KeywordIf);
    map.insert("nil", TokenKind::KeywordNil);
    map.insert("or", TokenKind::KeywordOr);
    map.insert("print", TokenKind::KeywordPrint);
    map.insert("return", TokenKind::KeywordReturn);
    map.insert("super", TokenKind::KeywordSuper);
    map.insert("this", TokenKind::KeywordThis);
    map.insert("true", TokenKind::KeywordTrue);
    map.insert("var", TokenKind::KeywordVar);
    map.insert("while", TokenKind::KeywordWhile);
    map
});

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    /// The byte position of the start of the token.
    pub start: SpanIndex,
    /// The length of the token in bytes.
    pub length: SpanLength,
}

impl Span {
    pub fn range(&self) -> Range<usize> {
        self.start.into()..(self.start + self.length).into()
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenKind {
    // Parentheses
    LeftParenthesis,
    RightParenthesis,
    // Braces
    LeftBrace,
    RightBrace,
    // Miscellaneous
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,
    // Operators
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Slash,

    // Literals
    NumericLiteral,
    StringLiteral,
    Ident,

    // Identifiers
    KeywordAnd,
    KeywordClass,
    KeywordElse,
    KeywordFalse,
    KeywordFor,
    KeywordFun,
    KeywordIf,
    KeywordNil,
    KeywordOr,
    KeywordPrint,
    KeywordReturn,
    KeywordSuper,
    KeywordThis,
    KeywordTrue,
    KeywordVar,
    KeywordWhile,

    // End of file.
    Eof,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::LeftParenthesis => write!(f, "LEFT_PAREN"),
            TokenKind::RightParenthesis => write!(f, "RIGHT_PAREN"),
            TokenKind::Eof => write!(f, "EOF"),
            TokenKind::LeftBrace => write!(f, "LEFT_BRACE"),
            TokenKind::RightBrace => write!(f, "RIGHT_BRACE"),
            TokenKind::Comma => write!(f, "COMMA"),
            TokenKind::Dot => write!(f, "DOT"),
            TokenKind::Minus => write!(f, "MINUS"),
            TokenKind::Plus => write!(f, "PLUS"),
            TokenKind::Semicolon => write!(f, "SEMICOLON"),
            TokenKind::Star => write!(f, "STAR"),
            TokenKind::Bang => write!(f, "BANG"),
            TokenKind::BangEqual => write!(f, "BANG_EQUAL"),
            TokenKind::Equal => write!(f, "EQUAL"),
            TokenKind::EqualEqual => write!(f, "EQUAL_EQUAL"),
            TokenKind::LessThan => write!(f, "LESS"),
            TokenKind::LessThanEqual => write!(f, "LESS_EQUAL"),
            TokenKind::GreaterThan => write!(f, "GREATER"),
            TokenKind::GreaterThanEqual => write!(f, "GREATER_EQUAL"),
            TokenKind::Slash => write!(f, "SLASH"),
            TokenKind::StringLiteral => write!(f, "STRING"),
            TokenKind::NumericLiteral => write!(f, "NUMBER"),
            TokenKind::Ident => write!(f, "IDENTIFIER"),
            TokenKind::KeywordAnd => write!(f, "AND"),
            TokenKind::KeywordClass => write!(f, "CLASS"),
            TokenKind::KeywordElse => write!(f, "ELSE"),
            TokenKind::KeywordFalse => write!(f, "FALSE"),
            TokenKind::KeywordFor => write!(f, "FOR"),
            TokenKind::KeywordFun => write!(f, "FUN"),
            TokenKind::KeywordIf => write!(f, "IF"),
            TokenKind::KeywordNil => write!(f, "NIL"),
            TokenKind::KeywordOr => write!(f, "OR"),
            TokenKind::KeywordPrint => write!(f, "PRINT"),
            TokenKind::KeywordReturn => write!(f, "RETURN"),
            TokenKind::KeywordSuper => write!(f, "SUPER"),
            TokenKind::KeywordThis => write!(f, "THIS"),
            TokenKind::KeywordTrue => write!(f, "TRUE"),
            TokenKind::KeywordVar => write!(f, "VAR"),
            TokenKind::KeywordWhile => write!(f, "WHILE"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}
