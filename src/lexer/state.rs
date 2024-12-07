use crate::token::{Span, SpanIndex, Token, TokenKind, KEYWORD_HASHMAP};

use super::{LexicalError, LexicalErrorKind};

#[derive(Debug)]
pub enum LexerStateTransition {
    Stay,
    ChangeState(LexerState),
    ChangeStateAndEmit {
        new_state: LexerState,
        token_or_error: Result<Token, LexicalError>,
    },
    ChangeStateAndEmitAndPutBack {
        new_state: LexerState,
        token_or_error: Result<Token, LexicalError>,
        put_back: char,
    },
}

pub trait LexerStateExecutor {
    fn execute(&self, source: &str, next_char: Option<(SpanIndex, char)>) -> LexerStateTransition;
}

#[derive(Debug)]
pub struct DoubleCharacterTokenSpec {
    first: char,
    second: char,
    single_token: TokenKind,
    double_token: TokenKind,
}

const BANG_SPEC: DoubleCharacterTokenSpec = DoubleCharacterTokenSpec {
    first: '!',
    second: '=',
    single_token: TokenKind::Bang,
    double_token: TokenKind::BangEqual,
};
const EQUAL_SPEC: DoubleCharacterTokenSpec = DoubleCharacterTokenSpec {
    first: '=',
    second: '=',
    single_token: TokenKind::Equal,
    double_token: TokenKind::EqualEqual,
};
const LESS_THAN_SPEC: DoubleCharacterTokenSpec = DoubleCharacterTokenSpec {
    first: '<',
    second: '=',
    single_token: TokenKind::LessThan,
    double_token: TokenKind::LessThanEqual,
};
const GREATER_THAN_SPEC: DoubleCharacterTokenSpec = DoubleCharacterTokenSpec {
    first: '>',
    second: '=',
    single_token: TokenKind::GreaterThan,
    double_token: TokenKind::GreaterThanEqual,
};

#[derive(Debug)]
pub enum LexerState {
    Normal(NormalState),
    Ident(IdentState),
    String(StringState),
    // Integer part of number
    Integer(IntegerState),
    // Period after integer
    Period(PeriodState),
    // Decimal part of number
    Decimal(DecimalState),
    // Double characters
    DoubleCharacter(DoubleCharacterState),
    // Slash and comment
    Slash(SlashState),
    Comment(CommentState),
}

impl LexerState {
    pub fn execute(
        &self,
        source: &str,
        next_char: Option<(SpanIndex, char)>,
    ) -> LexerStateTransition {
        match self {
            LexerState::Normal(s) => s.execute(source, next_char),
            LexerState::Ident(s) => s.execute(source, next_char),
            LexerState::String(s) => s.execute(source, next_char),
            LexerState::Integer(s) => s.execute(source, next_char),
            LexerState::Period(s) => s.execute(source, next_char),
            LexerState::Decimal(s) => s.execute(source, next_char),
            LexerState::DoubleCharacter(s) => s.execute(source, next_char),
            LexerState::Slash(s) => s.execute(source, next_char),
            LexerState::Comment(s) => s.execute(source, next_char),
        }
    }
}

impl std::default::Default for LexerState {
    fn default() -> Self {
        Self::Normal(NormalState { location: 0.into() })
    }
}

#[derive(Debug)]
pub struct NormalState {
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
    fn execute(&self, source: &str, next_char: Option<(SpanIndex, char)>) -> LexerStateTransition {
        let _ = source;
        let Some((start, c)) = next_char else {
            return LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(self.increment()),
                token_or_error: Ok(Token {
                    kind: TokenKind::Eof,
                    span: Span {
                        start: self.location,
                        length: source.len() - self.location,
                    },
                }),
            };
        };

        let just = move |kind: TokenKind, length: usize| LexerStateTransition::ChangeStateAndEmit {
            new_state: LexerState::Normal(self.increment()),
            token_or_error: Ok(Token {
                kind,
                span: Span {
                    start,
                    length: length.into(),
                },
            }),
        };

        match c {
            // Single character tokens
            '(' => just(TokenKind::LeftParenthesis, '('.len_utf8()),
            ')' => just(TokenKind::RightParenthesis, ')'.len_utf8()),
            '{' => just(TokenKind::LeftBrace, '{'.len_utf8()),
            '}' => just(TokenKind::RightBrace, '}'.len_utf8()),
            ',' => just(TokenKind::Comma, ','.len_utf8()),
            '.' => just(TokenKind::Dot, '.'.len_utf8()),
            '-' => just(TokenKind::Minus, '-'.len_utf8()),
            '+' => just(TokenKind::Plus, '+'.len_utf8()),
            ';' => just(TokenKind::Semicolon, ';'.len_utf8()),
            '*' => just(TokenKind::Star, '*'.len_utf8()),
            // Double character tokens
            '!' => LexerStateTransition::ChangeState(LexerState::DoubleCharacter(
                DoubleCharacterState::new_bang_state(start),
            )),
            '=' => LexerStateTransition::ChangeState(LexerState::DoubleCharacter(
                DoubleCharacterState::new_equal_state(start),
            )),
            '<' => LexerStateTransition::ChangeState(LexerState::DoubleCharacter(
                DoubleCharacterState::new_less_than_state(start),
            )),
            '>' => LexerStateTransition::ChangeState(LexerState::DoubleCharacter(
                DoubleCharacterState::new_greater_than_state(start),
            )),
            // Slash/comment
            '/' => LexerStateTransition::ChangeState(LexerState::Slash(SlashState { start })),
            // Identifier/keyword token
            'a'..='z' | 'A'..='Z' | '_' => {
                LexerStateTransition::ChangeState(LexerState::Ident(IdentState { start }))
            }
            // String literal
            '"' => LexerStateTransition::ChangeState(LexerState::String(StringState { start })),
            // Numeric literal
            '0'..='9' => {
                LexerStateTransition::ChangeState(LexerState::Integer(IntegerState { start }))
            }
            _ => {
                if c.is_ascii_whitespace() {
                    LexerStateTransition::ChangeState(LexerState::Normal(self.increment()))
                } else {
                    LexerStateTransition::ChangeStateAndEmit {
                        new_state: LexerState::Normal(self.increment()),
                        token_or_error: Err(LexicalError {
                            kind: LexicalErrorKind::Unrecognized(c),
                            span: Span {
                                start,
                                length: c.len_utf8().into(),
                            },
                        }),
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
pub struct IdentState {
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
    fn execute(&self, source: &str, next_char: Option<(SpanIndex, char)>) -> LexerStateTransition {
        let Some((offset, c)) = next_char else {
            let token = self.lex_ident_or_keyword(source, source.len().into());
            return LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(NormalState {
                    location: source.len().into(),
                }),
                token_or_error: Ok(token),
            };
        };

        if c.is_ascii_alphanumeric() || c == '_' {
            LexerStateTransition::Stay
        } else {
            let token = self.lex_ident_or_keyword(source, offset);
            LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(NormalState {
                    location: offset + c.len_utf8(),
                }),
                token_or_error: Ok(token),
            }
        }
    }
}

#[derive(Debug)]
pub struct StringState {
    start: SpanIndex,
}

impl LexerStateExecutor for StringState {
    fn execute(&self, source: &str, next_char: Option<(SpanIndex, char)>) -> LexerStateTransition {
        let Some((offset, c)) = next_char else {
            return LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(NormalState {
                    location: source.len().into(),
                }),
                token_or_error: Err(LexicalError {
                    kind: LexicalErrorKind::UnclosedString,
                    span: Span {
                        start: self.start,
                        length: source.len() - self.start,
                    },
                }),
            };
        };

        if c == '"' {
            LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(NormalState {
                    location: offset + c.len_utf8(),
                }),
                token_or_error: Ok(Token {
                    kind: TokenKind::StringLiteral,
                    span: Span {
                        start: self.start,
                        length: offset - self.start,
                    },
                }),
            }
        } else {
            LexerStateTransition::Stay
        }
    }
}

#[derive(Debug)]
pub struct IntegerState {
    start: SpanIndex,
}

impl LexerStateExecutor for IntegerState {
    fn execute(&self, source: &str, next_char: Option<(SpanIndex, char)>) -> LexerStateTransition {
        let Some((offset, c)) = next_char else {
            return LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(NormalState {
                    location: source.len().into(),
                }),
                token_or_error: Ok(Token {
                    kind: TokenKind::NumericLiteral,
                    span: Span {
                        start: self.start,
                        length: source.len() - self.start,
                    },
                }),
            };
        };

        if c.is_ascii_digit() {
            LexerStateTransition::Stay
        } else if c == '.' {
            LexerStateTransition::ChangeState(LexerState::Period(PeriodState { start: self.start }))
        } else {
            LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(NormalState {
                    location: offset + c.len_utf8(),
                }),
                token_or_error: Ok(Token {
                    kind: TokenKind::NumericLiteral,
                    span: Span {
                        start: self.start,
                        length: offset - self.start,
                    },
                }),
            }
        }
    }
}

#[derive(Debug)]
pub struct PeriodState {
    start: SpanIndex,
}

impl LexerStateExecutor for PeriodState {
    fn execute(&self, source: &str, next_char: Option<(SpanIndex, char)>) -> LexerStateTransition {
        let Some((offset, c)) = next_char else {
            return LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(NormalState {
                    location: source.len().into(),
                }),
                token_or_error: Ok(Token {
                    kind: TokenKind::NumericLiteral,
                    span: Span {
                        start: self.start,
                        length: source.len() - self.start,
                    },
                }),
            };
        };

        if c.is_ascii_digit() {
            LexerStateTransition::ChangeState(LexerState::Decimal(DecimalState {
                start: self.start,
            }))
        } else {
            LexerStateTransition::ChangeStateAndEmitAndPutBack {
                new_state: LexerState::Normal(NormalState { location: offset }),
                token_or_error: Ok(Token {
                    kind: TokenKind::NumericLiteral,
                    span: Span {
                        start: self.start,
                        length: offset - self.start - '.'.len_utf8(),
                    },
                }),
                put_back: '.',
            }
        }
    }
}

#[derive(Debug)]
pub struct DecimalState {
    start: SpanIndex,
}

impl LexerStateExecutor for DecimalState {
    fn execute(&self, source: &str, next_char: Option<(SpanIndex, char)>) -> LexerStateTransition {
        let Some((offset, c)) = next_char else {
            return LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(NormalState {
                    location: source.len().into(),
                }),
                token_or_error: Ok(Token {
                    kind: TokenKind::NumericLiteral,
                    span: Span {
                        start: self.start,
                        length: source.len() - self.start,
                    },
                }),
            };
        };

        if c.is_ascii_digit() {
            LexerStateTransition::Stay
        } else {
            LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(NormalState {
                    location: offset + c.len_utf8(),
                }),
                token_or_error: Ok(Token {
                    kind: TokenKind::NumericLiteral,
                    span: Span {
                        start: self.start,
                        length: offset - self.start,
                    },
                }),
            }
        }
    }
}

#[derive(Debug)]
pub struct DoubleCharacterState {
    start: SpanIndex,
    spec: DoubleCharacterTokenSpec,
}

impl DoubleCharacterState {
    pub fn new_bang_state(start: SpanIndex) -> Self {
        Self {
            start,
            spec: BANG_SPEC,
        }
    }

    pub fn new_equal_state(start: SpanIndex) -> Self {
        Self {
            start,
            spec: EQUAL_SPEC,
        }
    }
    pub fn new_less_than_state(start: SpanIndex) -> Self {
        Self {
            start,
            spec: LESS_THAN_SPEC,
        }
    }
    pub fn new_greater_than_state(start: SpanIndex) -> Self {
        Self {
            start,
            spec: GREATER_THAN_SPEC,
        }
    }
}

impl LexerStateExecutor for DoubleCharacterState {
    fn execute(&self, source: &str, next_char: Option<(SpanIndex, char)>) -> LexerStateTransition {
        let Some((offset, c)) = next_char else {
            return LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(NormalState {
                    location: source.len().into(),
                }),
                token_or_error: Ok(Token {
                    kind: self.spec.single_token,
                    span: Span {
                        start: self.start,
                        length: self.spec.first.len_utf8().into(),
                    },
                }),
            };
        };

        if c == self.spec.second {
            LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(NormalState {
                    location: offset + c.len_utf8(),
                }),
                token_or_error: Ok(Token {
                    kind: self.spec.double_token,
                    span: Span {
                        start: self.start,
                        length: offset - self.start,
                    },
                }),
            }
        } else {
            LexerStateTransition::ChangeStateAndEmitAndPutBack {
                new_state: LexerState::Normal(NormalState { location: offset }),
                token_or_error: Ok(Token {
                    kind: self.spec.double_token,
                    span: Span {
                        start: self.start,
                        length: offset - self.start,
                    },
                }),
                put_back: c,
            }
        }
    }
}

#[derive(Debug)]
pub struct SlashState {
    start: SpanIndex,
}

impl LexerStateExecutor for SlashState {
    fn execute(&self, source: &str, next_char: Option<(SpanIndex, char)>) -> LexerStateTransition {
        let Some((offset, c)) = next_char else {
            return LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(NormalState {
                    location: source.len().into(),
                }),
                token_or_error: Ok(Token {
                    kind: TokenKind::Slash,
                    span: Span {
                        start: self.start,
                        length: '/'.len_utf8().into(),
                    },
                }),
            };
        };

        if c == '/' {
            LexerStateTransition::ChangeState(LexerState::Comment(CommentState {
                start: self.start,
            }))
        } else {
            LexerStateTransition::ChangeStateAndEmitAndPutBack {
                new_state: LexerState::Normal(NormalState {
                    location: offset + '/'.len_utf8(),
                }),
                token_or_error: Ok(Token {
                    kind: TokenKind::Slash,
                    span: Span {
                        start: self.start,
                        length: '/'.len_utf8().into(),
                    },
                }),
                put_back: c,
            }
        }
    }
}

#[derive(Debug)]
pub struct CommentState {
    start: SpanIndex,
}

impl LexerStateExecutor for CommentState {
    fn execute(&self, source: &str, next_char: Option<(SpanIndex, char)>) -> LexerStateTransition {
        let Some((offset, c)) = next_char else {
            return LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(NormalState {
                    location: source.len().into(),
                }),
                token_or_error: Ok(Token {
                    kind: TokenKind::Eof,
                    span: Span {
                        start: self.start,
                        length: source.len() - self.start,
                    },
                }),
            };
        };

        if c != '\n' {
            LexerStateTransition::Stay
        } else {
            LexerStateTransition::ChangeState(LexerState::Normal(NormalState {
                location: offset + c.len_utf8(),
            }))
        }
    }
}
