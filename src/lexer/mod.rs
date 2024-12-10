mod state;

use crate::token::{Span, SpanIndex, Token};
use state::{LexerState, LexerStateTransition};
use std::str::Chars;
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

#[derive(Debug, Clone, Copy)]
pub struct SourceChar {
    value: char,
    offset: SpanIndex,
    line: u32,
}

impl SourceChar {
    pub fn next_offset(&self) -> SpanIndex {
        self.offset + self.value.len_utf8()
    }
}

#[derive(Debug)]
enum LookAhead {
    None,
    Single(SourceChar),
    Double(SourceChar, SourceChar),
}

#[derive(Debug)]
pub struct Lexer<'src> {
    source: &'src str,
    chars: Chars<'src>,
    state: LexerState,
    offset: SpanIndex,
    lookahead: LookAhead,
    line: u32,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            chars: source.chars(),
            state: LexerState::default(),
            lookahead: LookAhead::None,
            offset: 0.into(),
            line: 1,
        }
    }

    pub fn get_source(&self) -> &'src str {
        self.source
    }

    pub fn get_lexeme(&self, span: &Span) -> Option<&'src str> {
        let start: usize = span.start.into();
        match start <= self.source.len() {
            true => Some(&self.source[span.range()]),
            false => None,
        }
    }
}

impl<'src> Lexer<'src> {
    fn next_char(&mut self) -> Option<SourceChar> {
        match self.lookahead {
            LookAhead::None => {
                if let Some(c) = self.chars.next() {
                    if c == '\n' {
                        self.line += 1;
                    }

                    let old_location = self.offset;
                    self.offset = self.offset + c.len_utf8();
                    Some(SourceChar {
                        value: c,
                        offset: old_location,
                        line: self.line,
                    })
                } else {
                    None
                }
            }
            LookAhead::Single(lookahead) => {
                self.lookahead = LookAhead::None;
                Some(lookahead)
            }
            LookAhead::Double(first, second) => {
                self.lookahead = LookAhead::Single(second);
                Some(first)
            }
        }
    }

    fn put_back_char(&mut self, c: SourceChar) {
        self.lookahead = LookAhead::Single(c);
    }

    fn put_back_two_chars(&mut self, first: SourceChar, second: SourceChar) {
        self.lookahead = LookAhead::Double(first, second);
    }

    pub fn next_token(&mut self) -> Result<Token, LexicalError> {
        loop {
            let next_char = self.next_char();
            let transition = self.state.execute(self.source, &next_char);

            match transition {
                LexerStateTransition::Stay => {}
                LexerStateTransition::ChangeState(new_state) => {
                    self.state = new_state;
                }
                LexerStateTransition::ChangeStateAndEmit {
                    new_state,
                    token_or_error,
                } => {
                    self.state = new_state;
                    return token_or_error;
                }
                LexerStateTransition::ChangeStateAndEmitAndPutBack {
                    new_state,
                    token_or_error,
                    put_back,
                } => {
                    self.state = new_state;
                    self.put_back_char(put_back);

                    return token_or_error;
                }
                LexerStateTransition::ChangeStateAndEmitAndPutBackTwo {
                    new_state,
                    token_or_error,
                    put_back,
                } => {
                    self.state = new_state;
                    self.put_back_two_chars(put_back.0, put_back.1);

                    return token_or_error;
                }
            }
        }
    }
}
