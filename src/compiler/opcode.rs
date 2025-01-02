use thiserror::Error;

use super::{Chunk, IncompleteChunk};
use crate::value::LoxValue;
use std::fmt::Write;

const WRITE_FMT_MSG: &'static str =
    "Encountered an error while attempting to write format string to buffer.";

#[derive(Debug, Error, Clone)]
pub enum DecodeError {
    #[error("Encountered invalid opcode {value}.")]
    InvalidOpcode { value: u8 },
    #[error("Incomplete operand for opcode {opcode:?}.")]
    IncompleteOperand { opcode: u8 },
}

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

impl From<&LoxConstant> for LoxValue {
    fn from(value: &LoxConstant) -> Self {
        match value {
            LoxConstant::Number(value) => Self::Number(*value),
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
    Multiply,
    Divide,
    Add,
    Subtract,
    Negate,
}

impl Opcode {
    // Opcode table
    const C_RETURN: u8 = 0x01;
    const C_CONST: u8 = 0x02;
    const C_MULTIPLY: u8 = 0x03;
    const C_DIVIDE: u8 = 0x04;
    const C_ADD: u8 = 0x05;
    const C_SUBTRACT: u8 = 0x06;
    const C_NEGATE: u8 = 0x07;

    pub fn decode(data: &[u8]) -> Result<Option<(Opcode, &[u8])>, DecodeError> {
        let Some((first, rest)) = data.split_first() else {
            return Ok(None);
        };
        let (opcode, rest) = match *first {
            Opcode::C_RETURN => (Opcode::Return, rest),
            Opcode::C_MULTIPLY => (Opcode::Multiply, rest),
            Opcode::C_DIVIDE => (Opcode::Divide, rest),
            Opcode::C_ADD => (Opcode::Add, rest),
            Opcode::C_SUBTRACT => (Opcode::Subtract, rest),
            Opcode::C_NEGATE => (Opcode::Negate, rest),
            Opcode::C_CONST => {
                let (handle_bytes, rest) = rest.split_at(4);
                let [first, second, third, fourth] = handle_bytes else {
                    return Err(DecodeError::IncompleteOperand {
                        opcode: Opcode::C_CONST,
                    });
                };
                let handle = u32::from_le_bytes([*first, *second, *third, *fourth]);
                (Opcode::Const(ConstRef(handle)), rest)
            }
            opcode => {
                return Err(DecodeError::InvalidOpcode { value: opcode });
            }
        };
        Ok(Some((opcode, rest)))
    }

    pub fn encode(&self, chunk: &mut IncompleteChunk) {
        match self {
            Opcode::Return => {
                chunk.emit_u8(Opcode::C_RETURN);
            }
            Opcode::Const(handle) => {
                chunk.emit_u8(Opcode::C_CONST);
                chunk.emit_u32(handle.0);
            }
            Opcode::Multiply => chunk.emit_u8(Opcode::C_MULTIPLY),
            Opcode::Divide => chunk.emit_u8(Opcode::C_DIVIDE),
            Opcode::Add => chunk.emit_u8(Opcode::C_ADD),
            Opcode::Subtract => chunk.emit_u8(Opcode::C_SUBTRACT),
            Opcode::Negate => chunk.emit_u8(Opcode::C_NEGATE),
        }
    }

    pub fn format(&self, buffer: &mut String, chunk: &Chunk) {
        match self {
            Opcode::Return => buffer.push_str("ret"),
            Opcode::Multiply => buffer.push_str("mul"),
            Opcode::Divide => buffer.push_str("div"),
            Opcode::Add => buffer.push_str("add"),
            Opcode::Subtract => buffer.push_str("sub"),
            Opcode::Negate => buffer.push_str("neg"),
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
