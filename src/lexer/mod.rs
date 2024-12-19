mod cursor;
mod error;
pub mod formatter;
mod state;
mod token;

use cursor::SourceChar;
pub use error::{LexicalError, LexicalErrorKind};
use state::{LexerState, LexerStateTransition};
use std::{ops::Range, rc::Rc, str::Chars};
use token::{Span, SpanIndex};
pub use token::{Token, TokenKind};

#[derive(Debug, Clone)]
pub struct LineBreaks {
    line_breaks: Rc<[Range<SpanIndex>]>,
    max_offset: SpanIndex,
}

impl LineBreaks {
    fn new(text: &str) -> Self {
        let line_breaks = if !text.is_empty() {
            let mut line_breaks = Vec::new();
            let mut cursor: SpanIndex = 0.into();
            for (offset, byte) in text.bytes().enumerate() {
                let offset = (offset + 1).into();
                if byte == b'\n' {
                    line_breaks.push(cursor..offset);
                    cursor = offset;
                }
            }
            if !text.ends_with("\n") {
                line_breaks.push(cursor..(text.len() + 1).into());
            }
            line_breaks
        } else {
            vec![0.into()..1.into()]
        };
        Self {
            line_breaks: line_breaks.into(),
            max_offset: text.len().into(),
        }
    }

    pub fn get_max_line(&self) -> u32 {
        (self.line_breaks.len() + 1) as u32
    }

    pub fn get_ranges(&self, range: Range<usize>) -> &[Range<SpanIndex>] {
        &self.line_breaks[range]
    }

    pub fn get_line(&self, offset: SpanIndex) -> u32 {
        self.line_breaks
            .binary_search_by(|r| {
                if offset < r.start {
                    std::cmp::Ordering::Greater
                } else if offset >= r.end {
                    std::cmp::Ordering::Less
                } else {
                    std::cmp::Ordering::Equal
                }
            })
            .map(|v| (v + 1) as u32)
            .unwrap_or(self.get_max_line())
    }

    pub fn get_line_from_span(&self, span: Span) -> u32 {
        let offset = span.start;
        self.get_line(offset)
    }

    pub fn get_line_range_from_span(
        &self,
        span: Span,
    ) -> ((Range<usize>, u32), (Range<usize>, u32)) {
        let start_offset = span.start;
        let end_offset = span.end();

        // A closure for binary search that returns the appropriate line range
        let binary_search = |offset: SpanIndex| -> Range<SpanIndex> {
            match self.line_breaks.binary_search_by(|r| {
                if offset < r.start {
                    std::cmp::Ordering::Greater
                } else if offset >= r.end {
                    std::cmp::Ordering::Less
                } else {
                    std::cmp::Ordering::Equal
                }
            }) {
                Ok(index) => self.line_breaks[index].clone(),
                Err(_) => end_offset..self.max_offset,
            }
        };

        let start_range = binary_search(start_offset);
        let end_range = binary_search(end_offset);

        // Fix up last range to not include EOF
        let end_range = if end_range.end >= self.max_offset {
            end_range.start..self.max_offset
        } else {
            end_range
        };

        (
            (
                start_range.start.into()..span.start.into(),
                self.get_line(start_offset),
            ),
            (
                span.end().into()..end_range.end.into(),
                self.get_line(end_offset),
            ),
        )
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
    line_breaks: LineBreaks,
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
            line_breaks: LineBreaks::new(source),
        }
    }

    pub fn get_source(&self) -> &'src str {
        self.source
    }

    pub fn get_line_breaks(&self) -> LineBreaks {
        self.line_breaks.clone()
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
