use crate::token::SpanIndex;

#[derive(Debug, Clone, Copy)]
pub struct SourceChar {
    pub value: char,
    pub offset: SpanIndex,
    pub line: u32,
}

impl SourceChar {
    pub fn next_offset(&self) -> SpanIndex {
        self.offset + self.value.len_utf8()
    }
}
