mod opcode;

use crate::lexer::{LineBreaks, Span};
use crate::resolver::ResolvedProgram;
use crate::string::IdentName;
pub use opcode::{ConstRef, ConstantPool, LoxConstant, Opcode};
use std::fmt::Write;
use std::sync::Arc;

const WRITE_FMT_MSG: &'static str =
    "Encountered an error while attempting to write format string to buffer.";

pub struct IncompleteChunk<'src> {
    name: IdentName,
    text: &'src str,
    line_breaks: LineBreaks,
    data: Vec<u8>,
    spans: Vec<Span>,
    constants: ConstantPool,
}

impl<'src> IncompleteChunk<'src> {
    pub fn new(name: IdentName, text: &'src str) -> Self {
        let line_breaks = LineBreaks::new(text);
        Self {
            name,
            text,
            line_breaks,
            data: Vec::new(),
            spans: Vec::new(),
            constants: ConstantPool::new(),
        }
    }

    pub fn emit_u8(&mut self, byte: u8) {
        self.data.push(byte);
    }

    pub fn emit_u32(&mut self, value: u32) {
        // Little endian
        let [first, second, third, fourth] = value.to_le_bytes();
        self.data.push(first);
        self.data.push(second);
        self.data.push(third);
        self.data.push(fourth);
    }

    pub fn emit_return(&mut self, span: Span) {
        self.spans.push(span);
        Opcode::Return.encode(self);
    }

    pub fn emit_multiply(&mut self, span: Span) {
        self.spans.push(span);
        Opcode::Multiply.encode(self);
    }

    pub fn emit_divide(&mut self, span: Span) {
        self.spans.push(span);
        Opcode::Divide.encode(self);
    }

    pub fn emit_add(&mut self, span: Span) {
        self.spans.push(span);
        Opcode::Add.encode(self);
    }

    pub fn emit_subtract(&mut self, span: Span) {
        self.spans.push(span);
        Opcode::Subtract.encode(self);
    }

    pub fn emit_negate(&mut self, span: Span) {
        self.spans.push(span);
        Opcode::Negate.encode(self);
    }

    pub fn emit_constant(&mut self, span: Span, value: LoxConstant) {
        let handle = self.constants.push_constant(value);
        self.spans.push(span);
        Opcode::Const(handle).encode(self);
    }

    pub fn finish(self) -> Chunk<'src> {
        Chunk {
            name: self.name,
            data: self.data.into(),
            spans: self.spans.into(),
            constants: self.constants,
            text: &self.text,
            line_breaks: self.line_breaks,
        }
    }
}

pub struct Chunk<'src> {
    name: IdentName,
    text: &'src str,
    line_breaks: LineBreaks,
    data: Arc<[u8]>,
    spans: Arc<[Span]>,
    constants: ConstantPool,
}

struct OpcodeIterator<'a, 'src> {
    text: &'src str,
    line_breaks: &'a LineBreaks,
    data: &'a [u8],
    index: usize,
}

impl<'a, 'src> std::iter::Iterator for OpcodeIterator<'a, 'src> {
    type Item = (usize, Opcode);

    fn next(&mut self) -> Option<Self::Item> {
        let value = Opcode::decode(&self.data);
        if let Some((opcode, rest)) = value {
            // Should not underflow because `rest.len()` is at most `self.data.len()`.
            let offset = self.index;
            self.index += self.data.len() - rest.len();
            self.data = rest;
            Some((offset, opcode))
        } else {
            None
        }
    }
}

impl<'src> Chunk<'src> {
    fn iter<'a>(&'a self) -> OpcodeIterator<'a, 'src> {
        OpcodeIterator {
            data: &self.data,
            index: 0,
            text: &self.text,
            line_breaks: &self.line_breaks,
        }
    }

    pub fn decode_at(&self, index: usize) -> Option<(Opcode, usize)> {
        let slice = &self.data[index..];
        let (opcode, rest) = Opcode::decode(&self.data[index..])?;
        let offset = slice.len() - rest.len() + index;
        Some((opcode, offset))
    }

    pub fn get_constant(&self, handle: ConstRef) -> Option<&LoxConstant> {
        self.constants.get(handle)
    }

    pub fn disassemble(&self) -> String {
        const INDENT: &'static str = "  ";
        let max_line = self.line_breaks.get_max_line();
        let num_digits = 4usize.max((max_line.checked_ilog10().unwrap_or(0) + 1) as usize);

        let mut buffer = String::new();
        write!(buffer, "Chunk <{}>:\n", self.name).expect(WRITE_FMT_MSG);
        let mut previous_line_number: Option<u32> = None;
        for (instruction_index, (offset, opcode)) in self.iter().enumerate() {
            let span = self
                .spans
                .get(instruction_index)
                .expect("# of spans == # of opcodes");
            let line_number = self.line_breaks.get_line_from_span(*span);
            write!(buffer, "{INDENT}{offset:04x}:").expect(WRITE_FMT_MSG);

            let same = match previous_line_number {
                Some(previous) => previous == line_number,
                None => false,
            };
            if same {
                write!(buffer, "{:>width$}| ", " ", width = num_digits).expect(WRITE_FMT_MSG);
            } else {
                write!(buffer, "{:>width$}{line_number} ", "L", width = num_digits)
                    .expect(WRITE_FMT_MSG);
            }
            opcode.format(&mut buffer, self);
            buffer.push('\n');

            previous_line_number = Some(line_number);
        }
        buffer.push('\n');
        buffer
    }
}

pub struct Compiler {}

impl Compiler {
    pub fn compile<'src>(program: &ResolvedProgram, text: &'src str) -> Chunk<'src> {
        let dummy_span = Span {
            start: 0.into(),
            length: 0.into(),
        };
        let mut chunk = IncompleteChunk::new("TEST".into(), text);
        chunk.emit_constant(dummy_span, LoxConstant::Number(4512.0084));
        chunk.emit_negate(dummy_span);
        chunk.emit_return(dummy_span);
        chunk.finish()
    }
}
