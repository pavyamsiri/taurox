use super::{
    token::{Token, TokenKind},
    LineBreaks,
};
use crate::lexer::{Lexer, LexicalError, LexicalErrorKind};
use owo_colors::colors::*;
use owo_colors::OwoColorize;

/// Interface for creating new token formatters.
pub trait TokenFormatter {
    /// Formats a token into a string.
    fn format(&self, token: &Token) -> String;
    /// Formats a lexer error into a string.
    fn format_lexical_error(&self, error: &LexicalError) -> String;
}

pub trait ToFormatter<F>
where
    F: TokenFormatter,
{
    fn create_formatter(&self) -> F;
}

pub struct BasicFormatter<'src> {
    text: &'src str,
    line_breaks: LineBreaks,
}

impl<'src> ToFormatter<BasicFormatter<'src>> for Lexer<'src> {
    fn create_formatter(&self) -> BasicFormatter<'src> {
        BasicFormatter {
            text: self.get_source(),
            line_breaks: self.get_line_breaks(),
        }
    }
}

impl<'src> TokenFormatter for BasicFormatter<'src> {
    fn format(&self, token: &Token) -> String {
        match token.kind {
            TokenKind::LeftParenthesis => "LEFT_PAREN ( null".into(),
            TokenKind::RightParenthesis => "RIGHT_PAREN ) null".into(),
            TokenKind::LeftBrace => "LEFT_BRACE { null".into(),
            TokenKind::RightBrace => "RIGHT_BRACE } null".into(),
            TokenKind::Comma => "COMMA , null".into(),
            TokenKind::Dot => "DOT . null".into(),
            TokenKind::Minus => "MINUS - null".into(),
            TokenKind::Plus => "PLUS + null".into(),
            TokenKind::Semicolon => "SEMICOLON ; null".into(),
            TokenKind::Star => "STAR * null".into(),
            TokenKind::Bang => "BANG ! null".into(),
            TokenKind::BangEqual => "BANG_EQUAL != null".into(),
            TokenKind::Equal => "EQUAL = null".into(),
            TokenKind::EqualEqual => "EQUAL_EQUAL == null".into(),
            TokenKind::LessThan => "LESS < null".into(),
            TokenKind::LessThanEqual => "LESS_EQUAL <= null".into(),
            TokenKind::GreaterThan => "GREATER > null".into(),
            TokenKind::GreaterThanEqual => "GREATER_EQUAL >= null".into(),
            TokenKind::Slash => "SLASH / null".into(),
            TokenKind::Eof => "EOF  null".into(),
            TokenKind::NumericLiteral => {
                let lexeme = &self.text[token.span.range()];
                let value: f64 = lexeme
                    .parse()
                    .expect("Numeric literals are guaranteed to be parseable into f64.");
                format!("NUMBER {lexeme} {value:?}")
            }
            TokenKind::StringLiteral => {
                let lexeme = &self.text[token.span.range()];
                let value = &lexeme[1..lexeme.len() - 1];
                format!("STRING {lexeme} {value}")
            }
            TokenKind::Ident => {
                let lexeme = &self.text[token.span.range()];
                format!("IDENTIFIER {lexeme} null")
            }
            TokenKind::KeywordAnd => "AND and null".into(),
            TokenKind::KeywordClass => "CLASS class null".into(),
            TokenKind::KeywordElse => "ELSE else null".into(),
            TokenKind::KeywordFalse => "FALSE false null".into(),
            TokenKind::KeywordFor => "FOR for null".into(),
            TokenKind::KeywordFun => "FUN fun null".into(),
            TokenKind::KeywordIf => "IF if null".into(),
            TokenKind::KeywordNil => "NIL nil null".into(),
            TokenKind::KeywordOr => "OR or null".into(),
            TokenKind::KeywordPrint => "PRINT print null".into(),
            TokenKind::KeywordReturn => "RETURN return null".into(),
            TokenKind::KeywordSuper => "SUPER super null".into(),
            TokenKind::KeywordThis => "THIS this null".into(),
            TokenKind::KeywordTrue => "TRUE true null".into(),
            TokenKind::KeywordVar => "VAR var null".into(),
            TokenKind::KeywordWhile => "WHILE while null".into(),
        }
    }

    fn format_lexical_error(&self, error: &LexicalError) -> String {
        let line = self.line_breaks.get_line_from_span(error.span);
        match error.kind {
            LexicalErrorKind::Unrecognized(c) => {
                format!("[line {}] Error: Unexpected character: {c}", line)
            }
            LexicalErrorKind::UnclosedString => {
                format!("[line {}] Error: Unterminated string.", line)
            }
        }
    }
}

pub struct DebugFormatter;

impl<'src> ToFormatter<DebugFormatter> for Lexer<'src> {
    fn create_formatter(&self) -> DebugFormatter {
        DebugFormatter {}
    }
}

impl TokenFormatter for DebugFormatter {
    fn format(&self, token: &Token) -> String {
        format!("{token:?}")
    }

    fn format_lexical_error(&self, error: &LexicalError) -> String {
        format!("{error:?}")
    }
}

pub struct LineFormatter<'src> {
    text: &'src str,
    line_breaks: LineBreaks,
}

impl<'src> ToFormatter<LineFormatter<'src>> for Lexer<'src> {
    fn create_formatter(&self) -> LineFormatter<'src> {
        LineFormatter {
            text: self.get_source(),
            line_breaks: self.get_line_breaks(),
        }
    }
}

impl<'src> TokenFormatter for LineFormatter<'src> {
    fn format(&self, token: &Token) -> String {
        let line = self.line_breaks.get_line_from_span(token.span);
        let value = match token.kind {
            TokenKind::LeftParenthesis => "LEFT_PAREN ( null".into(),
            TokenKind::RightParenthesis => "RIGHT_PAREN ) null".into(),
            TokenKind::LeftBrace => "LEFT_BRACE { null".into(),
            TokenKind::RightBrace => "RIGHT_BRACE } null".into(),
            TokenKind::Comma => "COMMA , null".into(),
            TokenKind::Dot => "DOT . null".into(),
            TokenKind::Minus => "MINUS - null".into(),
            TokenKind::Plus => "PLUS + null".into(),
            TokenKind::Semicolon => "SEMICOLON ; null".into(),
            TokenKind::Star => "STAR * null".into(),
            TokenKind::Bang => "BANG ! null".into(),
            TokenKind::BangEqual => "BANG_EQUAL != null".into(),
            TokenKind::Equal => "EQUAL = null".into(),
            TokenKind::EqualEqual => "EQUAL_EQUAL == null".into(),
            TokenKind::LessThan => "LESS < null".into(),
            TokenKind::LessThanEqual => "LESS_EQUAL <= null".into(),
            TokenKind::GreaterThan => "GREATER > null".into(),
            TokenKind::GreaterThanEqual => "GREATER_EQUAL >= null".into(),
            TokenKind::Slash => "SLASH / null".into(),
            TokenKind::Eof => "EOF  null".into(),
            TokenKind::NumericLiteral => {
                let lexeme = &self.text[token.span.range()];
                let value: f64 = lexeme
                    .parse()
                    .expect("Numeric literals are guaranteed to be parseable into f64.");
                format!("NUMBER {lexeme} {value:?}")
            }
            TokenKind::StringLiteral => {
                let lexeme = &self.text[token.span.range()];
                let value = &lexeme[1..lexeme.len() - 1];
                format!("STRING {lexeme} {value}")
            }
            TokenKind::Ident => {
                let lexeme = &self.text[token.span.range()];
                format!("IDENTIFIER {lexeme} null")
            }
            TokenKind::KeywordAnd => "AND and null".into(),
            TokenKind::KeywordClass => "CLASS class null".into(),
            TokenKind::KeywordElse => "ELSE else null".into(),
            TokenKind::KeywordFalse => "FALSE false null".into(),
            TokenKind::KeywordFor => "FOR for null".into(),
            TokenKind::KeywordFun => "FUN fun null".into(),
            TokenKind::KeywordIf => "IF if null".into(),
            TokenKind::KeywordNil => "NIL nil null".into(),
            TokenKind::KeywordOr => "OR or null".into(),
            TokenKind::KeywordPrint => "PRINT print null".into(),
            TokenKind::KeywordReturn => "RETURN return null".into(),
            TokenKind::KeywordSuper => "SUPER super null".into(),
            TokenKind::KeywordThis => "THIS this null".into(),
            TokenKind::KeywordTrue => "TRUE true null".into(),
            TokenKind::KeywordVar => "VAR var null".into(),
            TokenKind::KeywordWhile => "WHILE while null".into(),
        };
        format!("({line}) {value}")
    }

    fn format_lexical_error(&self, error: &LexicalError) -> String {
        let line = self.line_breaks.get_line_from_span(error.span);
        match error.kind {
            LexicalErrorKind::Unrecognized(c) => {
                format!("({}) ERROR UNEXPECTED_CHAR {c}", line)
            }
            LexicalErrorKind::UnclosedString => {
                format!("({}) ERROR UNTERMINATED_STRING null", line)
            }
        }
    }
}

pub struct PrettyFormatter<'src> {
    text: &'src str,
    line_breaks: LineBreaks,
}

impl<'src> ToFormatter<PrettyFormatter<'src>> for Lexer<'src> {
    fn create_formatter(&self) -> PrettyFormatter<'src> {
        PrettyFormatter {
            text: self.get_source(),
            line_breaks: self.get_line_breaks(),
        }
    }
}

impl<'src> TokenFormatter for PrettyFormatter<'src> {
    fn format(&self, token: &Token) -> String {
        let line = self.line_breaks.get_line_from_span(token.span);
        let value = match token.kind {
            TokenKind::LeftParenthesis => "LEFT_PAREN ( null".into(),
            TokenKind::RightParenthesis => "RIGHT_PAREN ) null".into(),
            TokenKind::LeftBrace => "LEFT_BRACE { null".into(),
            TokenKind::RightBrace => "RIGHT_BRACE } null".into(),
            TokenKind::Comma => "COMMA , null".into(),
            TokenKind::Dot => "DOT . null".into(),
            TokenKind::Minus => "MINUS - null".into(),
            TokenKind::Plus => "PLUS + null".into(),
            TokenKind::Semicolon => "SEMICOLON ; null".into(),
            TokenKind::Star => "STAR * null".into(),
            TokenKind::Bang => "BANG ! null".into(),
            TokenKind::BangEqual => "BANG_EQUAL != null".into(),
            TokenKind::Equal => "EQUAL = null".into(),
            TokenKind::EqualEqual => "EQUAL_EQUAL == null".into(),
            TokenKind::LessThan => "LESS < null".into(),
            TokenKind::LessThanEqual => "LESS_EQUAL <= null".into(),
            TokenKind::GreaterThan => "GREATER > null".into(),
            TokenKind::GreaterThanEqual => "GREATER_EQUAL >= null".into(),
            TokenKind::Slash => "SLASH / null".into(),
            TokenKind::Eof => "EOF  null".into(),
            TokenKind::NumericLiteral => {
                let lexeme = &self.text[token.span.range()];
                let value: f64 = lexeme
                    .parse()
                    .expect("Numeric literals are guaranteed to be parseable into f64.");
                format!("NUMBER {lexeme} {value:?}")
            }
            TokenKind::StringLiteral => {
                let lexeme = &self.text[token.span.range()];
                let value = &lexeme[1..lexeme.len() - 1];
                format!("STRING {lexeme} {value}")
            }
            TokenKind::Ident => {
                let lexeme = &self.text[token.span.range()];
                format!("IDENTIFIER {lexeme} null")
            }
            TokenKind::KeywordAnd => "AND and null".into(),
            TokenKind::KeywordClass => "CLASS class null".into(),
            TokenKind::KeywordElse => "ELSE else null".into(),
            TokenKind::KeywordFalse => "FALSE false null".into(),
            TokenKind::KeywordFor => "FOR for null".into(),
            TokenKind::KeywordFun => "FUN fun null".into(),
            TokenKind::KeywordIf => "IF if null".into(),
            TokenKind::KeywordNil => "NIL nil null".into(),
            TokenKind::KeywordOr => "OR or null".into(),
            TokenKind::KeywordPrint => "PRINT print null".into(),
            TokenKind::KeywordReturn => "RETURN return null".into(),
            TokenKind::KeywordSuper => "SUPER super null".into(),
            TokenKind::KeywordThis => "THIS this null".into(),
            TokenKind::KeywordTrue => "TRUE true null".into(),
            TokenKind::KeywordVar => "VAR var null".into(),
            TokenKind::KeywordWhile => "WHILE while null".into(),
        };
        format!("({line}) {value}")
    }

    fn format_lexical_error(&self, error: &LexicalError) -> String {
        let line = self.line_breaks.get_line_from_span(error.span);
        match error.kind {
            LexicalErrorKind::Unrecognized(_) => {
                let width = self.line_breaks.get_max_line().ilog10() as usize;
                let (start_span, end_span) = self.line_breaks.get_line_range_from_span(error.span);
                let length = error.span.length.to_usize();
                let start = &self.text[start_span];
                let e = &self.text[error.span.range()];
                let end = &self.text[end_span];
                format!(
                    "{:width$} {}    {start}{}{end}\n{}{}{}{}",
                    line.fg::<BrightBlue>(),
                    "|".fg::<BrightBlue>(),
                    e.fg::<BrightMagenta>(),
                    " ".repeat(width + 2),
                    "|".fg::<BrightBlue>(),
                    " ".repeat(start.len() + 4),
                    "^".repeat(length).fg::<BrightYellow>(),
                )
            }
            LexicalErrorKind::UnclosedString => {
                format!("({}) ERROR UNTERMINATED_STRING null", line)
            }
        }
    }
}
