mod state;

use crate::token::{Span, Token, TokenKind};
use state::LexerState;
use std::str::CharIndices;
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
    chars: CharIndices<'src>,
    state: LexerState,
    lookahead: Option<(usize, char)>,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            chars: source.char_indices(),
            state: LexerState::default(),
            lookahead: None,
        }
    }
}

impl<'src> Lexer<'src> {
    fn next_char(&mut self) -> Option<(usize, char)> {
        if self.lookahead.is_some() {
            self.lookahead
        } else {
            self.chars.next()
        }
    }

    pub fn next_token(&mut self) -> Result<Token, LexicalError> {
        loop {
            let next_char = self.next_char();
            let transition = self.state.execute(self.source, next_char);
            self.state = transition.new_state;

            if let Some(r) = transition.token_or_error {
                return r;
            }
        }
    }
}
