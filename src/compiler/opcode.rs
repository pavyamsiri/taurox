use super::{Chunk, IncompleteChunk};
use crate::lexer::Span;
use std::fmt::Write;

const WRITE_FMT_MSG: &'static str =
    "Encountered an error while attempting to write format string to buffer.";

#[derive(Debug, Clone, Copy)]
pub enum LoxConstant {
    Number(f64),
}

impl std::fmt::Display for LoxConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxConstant::Number(value) => write!(f, "{value}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ConstRef(u32);

pub struct ConstantPool {
    data: Vec<LoxConstant>,
}

impl ConstantPool {
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    pub fn get(&self, handle: ConstRef) -> Option<&LoxConstant> {
        self.data.get(handle.0 as usize)
    }

    pub fn push_constant(&mut self, value: LoxConstant) -> ConstRef {
        self.data.push(value);
        ConstRef((self.data.len() - 1) as u32)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    Return,
    Const(ConstRef),
}

impl Opcode {
    pub fn decode(data: &[u8]) -> Option<(Opcode, &[u8])> {
        let (first, rest) = data.split_first()?;
        let (opcode, rest) = match first {
            0xDE => (Opcode::Return, rest),
            0xAD => {
                let (handle_bytes, rest) = rest.split_at(4);
                let [first, second, third, fourth] = handle_bytes else {
                    return None;
                };
                let handle = u32::from_le_bytes([*first, *second, *third, *fourth]);
                (Opcode::Const(ConstRef(handle)), rest)
            }
            _ => {
                return None;
            }
        };
        Some((opcode, rest))
    }

    pub fn encode(&self, chunk: &mut IncompleteChunk) {
        match self {
            Opcode::Return => {
                chunk.emit_u8(0xDE);
            }
            Opcode::Const(handle) => {
                chunk.emit_u8(0xAD);
                chunk.emit_u32(handle.0);
            }
        }
    }

    pub fn format(&self, buffer: &mut String, chunk: &Chunk) {
        match self {
            Opcode::Return => buffer.push_str("ret"),
            Opcode::Const(handle) => {
                buffer.push_str("ldc ");
                let value = chunk
                    .get_constant(*handle)
                    .expect("OP_CONSTANT has an invalid constant reference.");
                write!(buffer, "{:<width$}${} = {value}", " ", handle.0, width = 4)
                    .expect(WRITE_FMT_MSG);
            }
        }
    }
}
