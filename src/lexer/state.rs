use crate::token::{Span, SpanIndex, Token, TokenKind, KEYWORD_HASHMAP};

use super::cursor::SourceChar;
use super::error::{LexicalError, LexicalErrorKind};

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
        put_back: SourceChar,
    },
    ChangeStateAndEmitAndPutBackTwo {
        new_state: LexerState,
        token_or_error: Result<Token, LexicalError>,
        put_back: (SourceChar, SourceChar),
    },
}

pub trait LexerStateExecutor {
    fn execute(&self, source: &str, next_char: &Option<SourceChar>) -> LexerStateTransition;
}

// States
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

#[derive(Debug)]
pub struct NormalState {
    location: SpanIndex,
    line: u32,
}

#[derive(Debug)]
pub struct IdentState {
    start: SpanIndex,
    line: u32,
}

#[derive(Debug)]
pub struct StringState {
    start: SpanIndex,
    line: u32,
}

#[derive(Debug)]
pub struct IntegerState {
    start: SpanIndex,
    line: u32,
}

#[derive(Debug)]
pub struct PeriodState {
    start: SpanIndex,
    period: SourceChar,
    line: u32,
}

#[derive(Debug)]
pub struct DecimalState {
    start: SpanIndex,
    line: u32,
}

#[derive(Debug)]
pub struct SlashState {
    start: SpanIndex,
    line: u32,
}

#[derive(Debug)]
pub struct CommentState {
    start: SpanIndex,
    line: u32,
}

// Double character state
#[derive(Debug)]
pub struct DoubleCharacterState {
    start: SpanIndex,
    spec: DoubleCharacterTokenSpec,
    line: u32,
}

#[derive(Debug)]
pub struct DoubleCharacterTokenSpec {
    first: char,
    second: char,
    single_token: TokenKind,
    double_token: TokenKind,
}

impl LexerState {
    pub fn execute(&self, source: &str, next_char: &Option<SourceChar>) -> LexerStateTransition {
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
        Self::Normal(NormalState {
            location: 0.into(),
            line: 1,
        })
    }
}

impl NormalState {
    pub fn increment(&self, line: u32) -> NormalState {
        NormalState {
            location: self.location + 1,
            line,
        }
    }
}

impl LexerStateExecutor for NormalState {
    fn execute(&self, source: &str, next_char: &Option<SourceChar>) -> LexerStateTransition {
        let _ = source;
        let Some(next_char) = next_char else {
            return LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(self.increment(self.line)),
                token_or_error: Ok(Token {
                    kind: TokenKind::Eof,
                    span: Span {
                        start: self.location,
                        length: source.len() + 1 - self.location,
                    },
                    line: self.line,
                }),
            };
        };

        let just = move |kind: TokenKind, length: usize| LexerStateTransition::ChangeStateAndEmit {
            new_state: LexerState::Normal(self.increment(next_char.line)),
            token_or_error: Ok(Token {
                kind,
                span: Span {
                    start: next_char.offset,
                    length: length.into(),
                },
                line: next_char.line,
            }),
        };

        match next_char.value {
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
                DoubleCharacterState::new_bang_state(next_char.offset, next_char.line),
            )),
            '=' => LexerStateTransition::ChangeState(LexerState::DoubleCharacter(
                DoubleCharacterState::new_equal_state(next_char.offset, next_char.line),
            )),
            '<' => LexerStateTransition::ChangeState(LexerState::DoubleCharacter(
                DoubleCharacterState::new_less_than_state(next_char.offset, next_char.line),
            )),
            '>' => LexerStateTransition::ChangeState(LexerState::DoubleCharacter(
                DoubleCharacterState::new_greater_than_state(next_char.offset, next_char.line),
            )),
            // Slash/comment
            '/' => LexerStateTransition::ChangeState(LexerState::Slash(SlashState {
                start: next_char.offset,
                line: next_char.line,
            })),
            // Identifier/keyword token
            'a'..='z' | 'A'..='Z' | '_' => {
                LexerStateTransition::ChangeState(LexerState::Ident(IdentState {
                    start: next_char.offset,
                    line: next_char.line,
                }))
            }
            // String literal
            '"' => LexerStateTransition::ChangeState(LexerState::String(StringState {
                start: next_char.offset,
                line: next_char.line,
            })),
            // Numeric literal
            '0'..='9' => LexerStateTransition::ChangeState(LexerState::Integer(IntegerState {
                start: next_char.offset,
                line: next_char.line,
            })),
            c => {
                if c.is_ascii_whitespace() {
                    LexerStateTransition::ChangeState(LexerState::Normal(
                        self.increment(next_char.line),
                    ))
                } else {
                    LexerStateTransition::ChangeStateAndEmit {
                        new_state: LexerState::Normal(self.increment(next_char.line)),
                        token_or_error: Err(LexicalError {
                            kind: LexicalErrorKind::Unrecognized(c),
                            span: Span {
                                start: next_char.offset,
                                length: c.len_utf8().into(),
                            },
                            line: next_char.line,
                        }),
                    }
                }
            }
        }
    }
}

impl IdentState {
    fn lex_ident_or_keyword(&self, source: &str, offset: SpanIndex, line: u32) -> Token {
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
            line,
        }
    }
}

impl LexerStateExecutor for IdentState {
    fn execute(&self, source: &str, next_char: &Option<SourceChar>) -> LexerStateTransition {
        let Some(next_char) = next_char else {
            let token = self.lex_ident_or_keyword(source, source.len().into(), self.line);
            return LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(NormalState {
                    location: source.len().into(),
                    line: self.line,
                }),
                token_or_error: Ok(token),
            };
        };

        if next_char.value.is_ascii_alphanumeric() || next_char.value == '_' {
            LexerStateTransition::Stay
        } else {
            let token = self.lex_ident_or_keyword(source, next_char.offset, self.line);
            LexerStateTransition::ChangeStateAndEmitAndPutBack {
                new_state: LexerState::Normal(NormalState {
                    location: next_char.offset,
                    line: next_char.line,
                }),
                token_or_error: Ok(token),
                put_back: next_char.clone(),
            }
        }
    }
}

impl LexerStateExecutor for StringState {
    fn execute(&self, source: &str, next_char: &Option<SourceChar>) -> LexerStateTransition {
        let Some(next_char) = next_char else {
            return LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(NormalState {
                    location: source.len().into(),
                    line: self.line,
                }),
                token_or_error: Err(LexicalError {
                    kind: LexicalErrorKind::UnclosedString,
                    span: Span {
                        start: self.start,
                        length: source.len() - self.start,
                    },
                    line: self.line,
                }),
            };
        };

        if next_char.value == '"' {
            LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(NormalState {
                    location: next_char.next_offset(),
                    line: next_char.line,
                }),
                token_or_error: Ok(Token {
                    kind: TokenKind::StringLiteral,
                    span: Span {
                        start: self.start,
                        length: (next_char.offset - self.start) + 1,
                    },
                    line: next_char.line,
                }),
            }
        } else {
            LexerStateTransition::Stay
        }
    }
}

impl LexerStateExecutor for IntegerState {
    fn execute(&self, source: &str, next_char: &Option<SourceChar>) -> LexerStateTransition {
        let Some(next_char) = next_char else {
            return LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(NormalState {
                    location: source.len().into(),
                    line: self.line,
                }),
                token_or_error: Ok(Token {
                    kind: TokenKind::NumericLiteral,
                    span: Span {
                        start: self.start,
                        length: source.len() - self.start,
                    },
                    line: self.line,
                }),
            };
        };

        if next_char.value.is_ascii_digit() {
            LexerStateTransition::Stay
        } else if next_char.value == '.' {
            LexerStateTransition::ChangeState(LexerState::Period(PeriodState {
                start: self.start,
                period: next_char.clone(),
                line: self.line,
            }))
        } else {
            LexerStateTransition::ChangeStateAndEmitAndPutBack {
                new_state: LexerState::Normal(NormalState {
                    location: next_char.offset,
                    line: next_char.line,
                }),
                token_or_error: Ok(Token {
                    kind: TokenKind::NumericLiteral,
                    span: Span {
                        start: self.start,
                        length: next_char.offset - self.start,
                    },
                    line: self.line,
                }),
                put_back: next_char.clone(),
            }
        }
    }
}

impl LexerStateExecutor for PeriodState {
    fn execute(&self, source: &str, next_char: &Option<SourceChar>) -> LexerStateTransition {
        let Some(next_char) = next_char else {
            return LexerStateTransition::ChangeStateAndEmitAndPutBack {
                new_state: LexerState::Normal(NormalState {
                    location: (source.len() - '.'.len_utf8()).into(),
                    line: self.line,
                }),
                token_or_error: Ok(Token {
                    kind: TokenKind::NumericLiteral,
                    span: Span {
                        start: self.start,
                        length: source.len() - self.start,
                    },
                    line: self.line,
                }),
                put_back: self.period,
            };
        };

        if next_char.value.is_ascii_digit() {
            LexerStateTransition::ChangeState(LexerState::Decimal(DecimalState {
                start: self.start,
                line: self.line,
            }))
        } else {
            let current_offset: usize = next_char.offset.into();
            LexerStateTransition::ChangeStateAndEmitAndPutBackTwo {
                new_state: LexerState::Normal(NormalState {
                    location: (current_offset - '.'.len_utf8()).into(),
                    line: self.line,
                }),
                token_or_error: Ok(Token {
                    kind: TokenKind::NumericLiteral,
                    span: Span {
                        start: self.start,
                        length: next_char.offset - self.start - '.'.len_utf8(),
                    },
                    line: self.line,
                }),
                put_back: (
                    SourceChar {
                        value: '.',
                        offset: current_offset.into(),
                        line: self.line,
                    },
                    next_char.clone(),
                ),
            }
        }
    }
}

impl LexerStateExecutor for DecimalState {
    fn execute(&self, source: &str, next_char: &Option<SourceChar>) -> LexerStateTransition {
        let Some(next_char) = next_char else {
            return LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(NormalState {
                    location: source.len().into(),
                    line: self.line,
                }),
                token_or_error: Ok(Token {
                    kind: TokenKind::NumericLiteral,
                    span: Span {
                        start: self.start,
                        length: source.len() - self.start,
                    },
                    line: self.line,
                }),
            };
        };

        if next_char.value.is_ascii_digit() {
            LexerStateTransition::Stay
        } else {
            LexerStateTransition::ChangeStateAndEmitAndPutBack {
                new_state: LexerState::Normal(NormalState {
                    location: next_char.next_offset(),
                    line: next_char.line,
                }),
                token_or_error: Ok(Token {
                    kind: TokenKind::NumericLiteral,
                    span: Span {
                        start: self.start,
                        length: next_char.offset - self.start,
                    },
                    line: self.line,
                }),
                put_back: next_char.clone(),
            }
        }
    }
}

impl DoubleCharacterState {
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

    pub fn new_bang_state(start: SpanIndex, line: u32) -> Self {
        Self {
            start,
            spec: DoubleCharacterState::BANG_SPEC,
            line,
        }
    }

    pub fn new_equal_state(start: SpanIndex, line: u32) -> Self {
        Self {
            start,
            spec: DoubleCharacterState::EQUAL_SPEC,
            line,
        }
    }
    pub fn new_less_than_state(start: SpanIndex, line: u32) -> Self {
        Self {
            start,
            spec: DoubleCharacterState::LESS_THAN_SPEC,
            line,
        }
    }
    pub fn new_greater_than_state(start: SpanIndex, line: u32) -> Self {
        Self {
            start,
            spec: DoubleCharacterState::GREATER_THAN_SPEC,
            line,
        }
    }
}

impl LexerStateExecutor for DoubleCharacterState {
    fn execute(&self, source: &str, next_char: &Option<SourceChar>) -> LexerStateTransition {
        let Some(next_char) = next_char else {
            return LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(NormalState {
                    location: source.len().into(),
                    line: self.line,
                }),
                token_or_error: Ok(Token {
                    kind: self.spec.single_token,
                    span: Span {
                        start: self.start,
                        length: self.spec.first.len_utf8().into(),
                    },
                    line: self.line,
                }),
            };
        };

        if next_char.value == self.spec.second {
            LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(NormalState {
                    location: next_char.next_offset(),
                    line: self.line,
                }),
                token_or_error: Ok(Token {
                    kind: self.spec.double_token,
                    span: Span {
                        start: self.start,
                        length: next_char.offset - self.start,
                    },
                    line: self.line,
                }),
            }
        } else {
            LexerStateTransition::ChangeStateAndEmitAndPutBack {
                new_state: LexerState::Normal(NormalState {
                    location: next_char.offset,
                    line: self.line,
                }),
                token_or_error: Ok(Token {
                    kind: self.spec.single_token,
                    span: Span {
                        start: self.start,
                        length: next_char.offset - self.start,
                    },
                    line: self.line,
                }),
                put_back: next_char.clone(),
            }
        }
    }
}

impl LexerStateExecutor for SlashState {
    fn execute(&self, source: &str, next_char: &Option<SourceChar>) -> LexerStateTransition {
        let Some(next_char) = next_char else {
            return LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(NormalState {
                    location: source.len().into(),
                    line: self.line,
                }),
                token_or_error: Ok(Token {
                    kind: TokenKind::Slash,
                    span: Span {
                        start: self.start,
                        length: '/'.len_utf8().into(),
                    },
                    line: self.line,
                }),
            };
        };

        if next_char.value == '/' {
            LexerStateTransition::ChangeState(LexerState::Comment(CommentState {
                start: self.start,
                line: self.line,
            }))
        } else {
            LexerStateTransition::ChangeStateAndEmitAndPutBack {
                new_state: LexerState::Normal(NormalState {
                    location: next_char.offset + '/'.len_utf8(),
                    line: self.line,
                }),
                token_or_error: Ok(Token {
                    kind: TokenKind::Slash,
                    span: Span {
                        start: self.start,
                        length: '/'.len_utf8().into(),
                    },
                    line: self.line,
                }),
                put_back: next_char.clone(),
            }
        }
    }
}

impl LexerStateExecutor for CommentState {
    fn execute(&self, source: &str, next_char: &Option<SourceChar>) -> LexerStateTransition {
        let Some(next_char) = next_char else {
            return LexerStateTransition::ChangeStateAndEmit {
                new_state: LexerState::Normal(NormalState {
                    location: source.len().into(),
                    line: self.line,
                }),
                token_or_error: Ok(Token {
                    kind: TokenKind::Eof,
                    span: Span {
                        start: self.start,
                        length: source.len() - self.start,
                    },
                    line: self.line,
                }),
            };
        };

        if next_char.value != '\n' {
            LexerStateTransition::Stay
        } else {
            LexerStateTransition::ChangeState(LexerState::Normal(NormalState {
                location: next_char.next_offset(),
                line: self.line,
            }))
        }
    }
}
