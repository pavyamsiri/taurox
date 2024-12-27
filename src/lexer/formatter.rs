use super::{
    token::{SpanIndex, Token, TokenKind},
    LineBreaks,
};
use crate::lexer::{LexicalError, LexicalErrorKind};
use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use std::{fmt::Write, path::Path};

const WRITE_FMT_MSG: &'static str =
    "Encountered an error while attempting to write format string to buffer.";
const ARIADNE_MSG: &'static str = "Ariadne produces valid utf-8 strings";
const ARIADNE_WRITE_MSG: &'static str = "Write into buffer should not fail.";

fn format_token_in_place_basic(text: &str, buffer: &mut String, token: &Token) {
    match token.kind {
        TokenKind::LeftParenthesis => buffer.push_str("LEFT_PAREN ( null"),
        TokenKind::RightParenthesis => buffer.push_str("RIGHT_PAREN ) null"),
        TokenKind::LeftBrace => buffer.push_str("LEFT_BRACE { null"),
        TokenKind::RightBrace => buffer.push_str("RIGHT_BRACE } null"),
        TokenKind::Comma => buffer.push_str("COMMA , null"),
        TokenKind::Dot => buffer.push_str("DOT . null"),
        TokenKind::Minus => buffer.push_str("MINUS - null"),
        TokenKind::Plus => buffer.push_str("PLUS + null"),
        TokenKind::Semicolon => buffer.push_str("SEMICOLON ; null"),
        TokenKind::Star => buffer.push_str("STAR * null"),
        TokenKind::Bang => buffer.push_str("BANG ! null"),
        TokenKind::BangEqual => buffer.push_str("BANG_EQUAL != null"),
        TokenKind::Equal => buffer.push_str("EQUAL = null"),
        TokenKind::EqualEqual => buffer.push_str("EQUAL_EQUAL == null"),
        TokenKind::LessThan => buffer.push_str("LESS < null"),
        TokenKind::LessThanEqual => buffer.push_str("LESS_EQUAL <= null"),
        TokenKind::GreaterThan => buffer.push_str("GREATER > null"),
        TokenKind::GreaterThanEqual => buffer.push_str("GREATER_EQUAL >= null"),
        TokenKind::Slash => buffer.push_str("SLASH / null"),
        TokenKind::Eof => buffer.push_str("EOF  null"),
        TokenKind::KeywordAnd => buffer.push_str("AND and null"),
        TokenKind::KeywordClass => buffer.push_str("CLASS class null"),
        TokenKind::KeywordElse => buffer.push_str("ELSE else null"),
        TokenKind::KeywordFalse => buffer.push_str("FALSE false null"),
        TokenKind::KeywordFor => buffer.push_str("FOR for null"),
        TokenKind::KeywordFun => buffer.push_str("FUN fun null"),
        TokenKind::KeywordIf => buffer.push_str("IF if null"),
        TokenKind::KeywordNil => buffer.push_str("NIL nil null"),
        TokenKind::KeywordOr => buffer.push_str("OR or null"),
        TokenKind::KeywordPrint => buffer.push_str("PRINT print null"),
        TokenKind::KeywordReturn => buffer.push_str("RETURN return null"),
        TokenKind::KeywordSuper => buffer.push_str("SUPER super null"),
        TokenKind::KeywordThis => buffer.push_str("THIS this null"),
        TokenKind::KeywordTrue => buffer.push_str("TRUE true null"),
        TokenKind::KeywordVar => buffer.push_str("VAR var null"),
        TokenKind::KeywordWhile => buffer.push_str("WHILE while null"),
        TokenKind::NumericLiteral => {
            let lexeme = &text[token.span.range()];
            let value: f64 = lexeme
                .parse()
                .expect("Numeric literals are guaranteed to be parseable into f64.");
            write!(buffer, "NUMBER {lexeme} {value:?}").expect(&WRITE_FMT_MSG);
        }
        TokenKind::StringLiteral => {
            let lexeme = &text[token.span.range()];
            let value = &lexeme[1..lexeme.len() - 1];
            write!(buffer, "STRING {lexeme} {value}").expect(&WRITE_FMT_MSG);
        }
        TokenKind::Ident => {
            let lexeme = &text[token.span.range()];
            write!(buffer, "IDENTIFIER {lexeme} null").expect(&WRITE_FMT_MSG);
        }
    }
}
/// Interface for creating new token formatters.
pub trait TokenFormatter {
    /// Formats a token into a string.
    fn format(&self, token: &Token) -> String {
        let mut buffer = String::new();
        self.format_in_place(&mut buffer, token);
        buffer
    }
    /// Formats a lexer error into a string.
    fn format_error(&self, error: &LexicalError) -> String {
        let mut buffer = String::new();
        self.format_error_in_place(&mut buffer, error);
        buffer
    }

    /// Formats a token into a string.
    fn format_in_place(&self, buffer: &mut String, token: &Token);
    /// Formats a lexer error into a string.
    fn format_error_in_place(&self, buffer: &mut String, error: &LexicalError);
}

pub struct BasicFormatter<'src> {
    text: &'src str,
    line_breaks: LineBreaks,
}

impl<'src> BasicFormatter<'src> {
    pub fn new(text: &'src str) -> Self {
        let line_breaks = LineBreaks::new(text);
        Self { text, line_breaks }
    }
}

impl<'src> TokenFormatter for BasicFormatter<'src> {
    fn format_in_place(&self, buffer: &mut String, token: &Token) {
        format_token_in_place_basic(&self.text, buffer, token);
    }

    fn format_error_in_place(&self, buffer: &mut String, error: &LexicalError) {
        let line = self.line_breaks.get_line_from_span(error.span);
        match error.kind {
            LexicalErrorKind::Unrecognized(c) => {
                buffer
                    .write_fmt(format_args!(
                        "[line {}] Error: Unexpected character: {c}",
                        line
                    ))
                    .expect(&WRITE_FMT_MSG);
            }
            LexicalErrorKind::UnclosedString => {
                buffer
                    .write_fmt(format_args!("[line {}] Error: Unterminated string.", line))
                    .expect(&WRITE_FMT_MSG);
            }
        }
    }
}

pub struct DebugFormatter;

impl TokenFormatter for DebugFormatter {
    fn format_in_place(&self, buffer: &mut String, token: &Token) {
        write!(buffer, "{token:?}").expect(&WRITE_FMT_MSG);
    }

    fn format_error_in_place(&self, buffer: &mut String, error: &LexicalError) {
        write!(buffer, "{error:?}").expect(&WRITE_FMT_MSG);
    }
}

pub struct LineFormatter<'src> {
    text: &'src str,
    line_breaks: LineBreaks,
}

impl<'src> LineFormatter<'src> {
    pub fn new(text: &'src str) -> Self {
        let line_breaks = LineBreaks::new(text);
        Self { text, line_breaks }
    }

    pub fn get_line_breaks(&self) -> &LineBreaks {
        &self.line_breaks
    }
}

impl<'src> TokenFormatter for LineFormatter<'src> {
    fn format_in_place(&self, buffer: &mut String, token: &Token) {
        let line = self.line_breaks.get_line_from_span(token.span);
        write!(buffer, "({line}) ").expect(&WRITE_FMT_MSG);
        format_token_in_place_basic(&self.text, buffer, token);
    }

    fn format_error_in_place(&self, buffer: &mut String, error: &LexicalError) {
        let line = self.line_breaks.get_line_from_span(error.span);
        match error.kind {
            LexicalErrorKind::Unrecognized(c) => {
                write!(buffer, "({}) ERROR UNEXPECTED_CHAR {c}", line).expect(&WRITE_FMT_MSG)
            }
            LexicalErrorKind::UnclosedString => {
                write!(buffer, "({}) ERROR UNTERMINATED_STRING null", line).expect(&WRITE_FMT_MSG)
            }
        }
    }
}

pub struct PrettyFormatter<'src> {
    text: &'src str,
    line_breaks: LineBreaks,
    path: &'src Path,
}

impl<'src> PrettyFormatter<'src> {
    pub fn new(text: &'src str, path: &'src Path) -> Self {
        let line_breaks = LineBreaks::new(text);
        Self {
            text,
            line_breaks,
            path,
        }
    }

    pub fn get_line_breaks(&self) -> &LineBreaks {
        &self.line_breaks
    }

    pub fn get_path(&self) -> &'src Path {
        self.path
    }

    pub fn get_text(&self) -> &'src str {
        self.text
    }
}

impl<'src> TokenFormatter for PrettyFormatter<'src> {
    fn format_in_place(&self, buffer: &mut String, token: &Token) {
        format_token_in_place_basic(&self.text, buffer, token);
    }

    fn format_error_in_place(&self, buffer: &mut String, error: &LexicalError) {
        let path = self
            .path
            .to_str()
            .expect("Non-UTF8 paths are not supported!");
        let mut output = std::io::Cursor::new(Vec::new());
        match error.kind {
            LexicalErrorKind::Unrecognized(c) => {
                Report::build(ReportKind::Error, (path, error.span.range()))
                    .with_code(error.code())
                    .with_message("Encountered an unrecognized character during lexing")
                    .with_label(
                        Label::new((path, error.span.range()))
                            .with_message(format!(
                                "Unrecognized character {}",
                                c.fg(Color::BrightRed)
                            ))
                            .with_color(Color::BrightRed),
                    )
                    .finish()
                    .write((path, Source::from(self.text)), &mut output)
                    .expect(&ARIADNE_WRITE_MSG);
            }
            LexicalErrorKind::UnclosedString => {
                Report::build(ReportKind::Error, (path, error.span.range()))
                    .with_code(error.code())
                    .with_message("Encountered non-terminated string during lexing")
                    .with_label(
                        Label::new((path, error.span.split_left(SpanIndex::new(1)).range()))
                            .with_message(format!("String starts here..."))
                            .with_color(Color::BrightRed),
                    )
                    .with_label(
                        Label::new((path, error.span.split_right(SpanIndex::new(1)).range()))
                            .with_message(format!("and is not closed"))
                            .with_color(Color::BrightCyan),
                    )
                    .finish()
                    .write((path, Source::from(self.text)), &mut output)
                    .expect(&ARIADNE_WRITE_MSG);
            }
        }
        buffer.push_str(&String::from_utf8(output.into_inner()).expect(&ARIADNE_MSG));
    }
}
