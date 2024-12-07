mod state;

use crate::token::{Span, SpanIndex, Token};
use state::{LexerState, LexerStateTransition};
use std::str::Chars;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum LexicalErrorKind {
    #[error("Unexpected character: {0}")]
    Unrecognized(char),
    #[error("Unterminated string literal.")]
    UnclosedString,
}

#[derive(Debug, Error)]
#[error("Error: {kind}")]
pub struct LexicalError {
    #[source]
    pub kind: LexicalErrorKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct Lexer<'src> {
    source: &'src str,
    chars: Chars<'src>,
    state: LexerState,
    offset: SpanIndex,
    lookahead: Option<(SpanIndex, char)>,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            chars: source.chars(),
            state: LexerState::default(),
            lookahead: None,
            offset: 0.into(),
        }
    }
}

impl<'src> Lexer<'src> {
    fn next_char(&mut self) -> Option<(SpanIndex, char)> {
        if self.lookahead.is_some() {
            self.lookahead.take()
        } else {
            if let Some(c) = self.chars.next() {
                let old_location = self.offset;
                self.offset = self.offset + c.len_utf8();
                Some((old_location, c))
            } else {
                None
            }
        }
    }

    fn put_back_char(&mut self, c: char) {
        self.lookahead = Some((self.offset, c));
    }

    pub fn next_token(&mut self) -> Result<Token, LexicalError> {
        loop {
            let next_char = self.next_char();
            let transition = self.state.execute(self.source, next_char);

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
