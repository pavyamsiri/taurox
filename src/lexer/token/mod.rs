mod span;

pub use span::{SpanIndex, SpanLength};
use std::collections::HashMap;
use std::fmt::Display;
use std::ops::Range;
use std::sync::LazyLock;

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

    pub fn end(&self) -> SpanIndex {
        self.start + self.length
    }

    pub fn split_left(&self, offset: SpanIndex) -> Span {
        if offset > self.end() {
            self.clone()
        } else {
            Span {
                start: self.start,
                length: SpanLength::new(offset.to_usize() as u32),
            }
        }
    }

    pub fn split_right(&self, offset: SpanIndex) -> Span {
        let new_start = self.start + offset.to_usize();
        if offset > self.end() {
            Span {
                start: new_start,
                length: 0.into(),
            }
        } else {
            let new_length = self.length - offset.to_usize();
            Span {
                start: new_start,
                length: new_length,
            }
        }
    }

    pub fn merge(&self, other: &Span) -> Span {
        let start = self.start.min(other.start);
        let end = self.end().max(other.end());
        let length = end - start;
        Span { start, length }
    }

    pub fn expand(&self, size: usize) -> Span {
        let start = self.start.to_usize().saturating_sub(size);
        let end = self.end().to_usize().saturating_add(size);
        let length = end - start;
        Span {
            start: SpanIndex::new(start as u32),
            length: SpanLength::new(length as u32),
        }
    }

    pub fn right_expand(&self, size: usize) -> Span {
        let start = self.start.to_usize();
        let end = self.end().to_usize().saturating_add(size);
        let length = end - start;
        Span {
            start: SpanIndex::new(start as u32),
            length: SpanLength::new(length as u32),
        }
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

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}
