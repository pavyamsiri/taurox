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
pub struct Lexer<'src> {
    source: &'src str,
    chars: Chars<'src>,
    state: LexerState,
    offset: SpanIndex,
    lookahead: Option<SourceChar>,
    line: u32,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            chars: source.chars(),
            state: LexerState::default(),
            lookahead: None,
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
        if self.lookahead.is_some() {
            self.lookahead.take()
        } else {
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
    }

    fn put_back_char(&mut self, c: SourceChar) {
        self.lookahead = Some(c);
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
            }
        }
    }
}
