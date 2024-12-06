use std::str::CharIndices;

#[derive(Debug)]
enum LexerState {
    Normal,
    Ident { start: u32 },
    String { start: u32 },
}

#[derive(Debug)]
pub struct Lexer<'a> {
    text: CharIndices<'a>,
    state: LexerState,
    lookahead: Option<(usize, char)>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            text: src.char_indices(),
            state: LexerState::Normal,
            lookahead: None,
        }
    }
}
