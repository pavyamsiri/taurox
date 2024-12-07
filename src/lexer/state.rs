use crate::token::{Span, SpanIndex, Token, TokenKind, KEYWORD_HASHMAP};

use super::{LexicalError, LexicalErrorKind};

pub struct LexerStateTransition {
    pub new_state: LexerState,
    pub token_or_error: Option<Result<Token, LexicalError>>,
}

pub trait LexerStateExecutor {
    fn execute(&self, source: &str, next_char: Option<(usize, char)>) -> LexerStateTransition;
}

#[derive(Debug)]
pub enum LexerState {
    Normal(NormalState),
    Ident(IdentState),
    String(StringState),
}

impl LexerState {
    pub fn execute(&self, source: &str, next_char: Option<(usize, char)>) -> LexerStateTransition {
        match self {
            LexerState::Normal(s) => s.execute(source, next_char),
            LexerState::Ident(s) => s.execute(source, next_char),
            LexerState::String(s) => s.execute(source, next_char),
        }
    }
}

impl std::default::Default for LexerState {
    fn default() -> Self {
        Self::Normal(NormalState { location: 0 })
    }
}

#[derive(Debug)]
struct NormalState {
    location: SpanIndex,
}

impl NormalState {
    pub fn increment(&self) -> NormalState {
        NormalState {
            location: self.location + 1,
        }
    }
}

impl LexerStateExecutor for NormalState {
    fn execute(&self, source: &str, next_char: Option<(usize, char)>) -> LexerStateTransition {
        let _ = source;
        let Some((start, c)) = next_char else {
            return LexerStateTransition {
                new_state: LexerState::Normal(self.increment()),
                token_or_error: Some(Ok(Token {
                    kind: TokenKind::Eof,
                    span: Span {
                        start: self.location,
                        length: 1,
                    },
                })),
            };
        };

        let start = start as SpanIndex;

        let just = move |kind: TokenKind| LexerStateTransition {
            new_state: LexerState::Normal(self.increment()),
            token_or_error: Some(Ok(Token {
                kind,
                span: Span { start, length: 1 },
            })),
        };

        match c {
            // Single character tokens
            '(' => just(TokenKind::LeftParenthesis),
            ')' => just(TokenKind::RightParenthesis),
            '{' => just(TokenKind::LeftBrace),
            '}' => just(TokenKind::RightBrace),
            ',' => just(TokenKind::Comma),
            '.' => just(TokenKind::Dot),
            '-' => just(TokenKind::Minus),
            '+' => just(TokenKind::Plus),
            ';' => just(TokenKind::Semicolon),
            '*' => just(TokenKind::Star),
            // Identifier/keyword token
            'a'..='z' | 'A'..='Z' | '_' => LexerStateTransition {
                new_state: LexerState::Ident(IdentState { start }),
                token_or_error: None,
            },
            // // String literal
            // '"' => State::String(super::StringState {
            //     start: current_char.offset,
            // }),
            _ => {
                if c.is_ascii_whitespace() {
                    LexerStateTransition {
                        new_state: LexerState::Normal(self.increment()),
                        token_or_error: None,
                    }
                } else {
                    LexerStateTransition {
                        new_state: LexerState::Normal(self.increment()),
                        token_or_error: Some(Err(LexicalError {
                            kind: LexicalErrorKind::Unrecognized(c),
                            span: Span {
                                start,
                                length: c.len_utf8() as SpanIndex,
                            },
                        })),
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
struct IdentState {
    start: SpanIndex,
}

impl IdentState {
    fn lex_ident_or_keyword(&self, source: &str, offset: SpanIndex) -> Token {
        let span = Span {
            start: self.start,
            length: offset - self.start,
        };
        let lexeme = &source[span.range()];
        Token {
            kind: KEYWORD_HASHMAP
                .get(lexeme)
                .cloned()
                .unwrap_or(TokenKind::Ident),
            span,
        }
    }
}

impl LexerStateExecutor for IdentState {
    fn execute(&self, source: &str, next_char: Option<(usize, char)>) -> LexerStateTransition {
        let Some((offset, c)) = next_char else {
            let token = self.lex_ident_or_keyword(source, source.len() as SpanIndex);
            return LexerStateTransition {
                new_state: LexerState::Normal(NormalState {
                    location: source.len() as SpanIndex,
                }),
                token_or_error: Some(Ok(token)),
            };
        };
        let offset = offset as SpanIndex;

        if c.is_ascii_alphanumeric() || c == '_' {
            LexerStateTransition {
                new_state: LexerState::Ident(self.clone()),
                token_or_error: None,
            }
        } else {
            let token = self.lex_ident_or_keyword(source, offset);
            LexerStateTransition {
                new_state: LexerState::Normal(NormalState {
                    location: offset + 1,
                }),
                token_or_error: Some(Ok(token)),
            }
        }
    }
}

#[derive(Debug, Clone)]
struct StringState {
    start: SpanIndex,
}

impl LexerStateExecutor for StringState {
    fn execute(&self, source: &str, next_char: Option<(usize, char)>) -> LexerStateTransition {
        let Some((offset, c)) = next_char else {
            return LexerStateTransition {
                new_state: LexerState::Normal(NormalState {
                    location: source.len() as SpanIndex,
                }),
                token_or_error: Some(Err(LexicalError {
                    kind: LexicalErrorKind::UnclosedString,
                    span: Span {
                        start: self.start,
                        length: source.len() as SpanIndex - self.start,
                    },
                })),
            };
        };

        let offset = offset as SpanIndex;

        if c == '"' {
            LexerStateTransition {
                new_state: LexerState::Normal(NormalState {
                    location: offset + 1,
                }),
                token_or_error: Some(Ok(Token {
                    kind: TokenKind::StringLiteral,
                    span: Span {
                        start: self.start,
                        length: offset - self.start,
                    },
                })),
            }
        } else {
            LexerStateTransition {
                new_state: LexerState::String(self.clone()),
                token_or_error: None,
            }
        }
    }
}
