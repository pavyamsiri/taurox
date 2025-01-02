use thiserror::Error;

use super::{Chunk, ConstRef, IncompleteChunk};
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
pub enum Opcode {
    Return,
    Const(ConstRef),
    Multiply,
    Divide,
    Add,
    Subtract,
    Negate,
    Not,
    Equals,
    LessThan,
    GreaterThan,
    Print,
    Pop,
    DefineGlobal(ConstRef),
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
    const C_NOT: u8 = 0x08;
    const C_EQUAL: u8 = 0x09;
    const C_LESS_THAN: u8 = 0x0A;
    const C_GREATER_THAN: u8 = 0x0B;
    const C_PRINT: u8 = 0x0C;
    const C_POP: u8 = 0x0D;
    const C_DEFINE_GLOBAL: u8 = 0x0E;

    pub fn decode_at(data: &[u8], index: usize) -> Result<Option<(Opcode, usize)>, DecodeError> {
        let Some(first) = data.get(index) else {
            return Ok(None);
        };

        let parse_u32 = |code: u8| -> Result<u32, DecodeError> {
            let handle_bytes = &data[index + 1..(index + 5)];
            let [first, second, third, fourth] = handle_bytes else {
                return Err(DecodeError::IncompleteOperand { opcode: code });
            };
            Ok(u32::from_le_bytes([*first, *second, *third, *fourth]))
        };

        let (opcode, rest) = match *first {
            Opcode::C_CONST => {
                let handle = parse_u32(Opcode::C_CONST)?;
                (Opcode::Const(ConstRef(handle)), index + 5)
            }
            Opcode::C_DEFINE_GLOBAL => {
                let handle = parse_u32(Opcode::C_CONST)?;
                (Opcode::DefineGlobal(ConstRef(handle)), index + 5)
            }
            Opcode::C_RETURN => (Opcode::Return, index + 1),
            Opcode::C_MULTIPLY => (Opcode::Multiply, index + 1),
            Opcode::C_DIVIDE => (Opcode::Divide, index + 1),
            Opcode::C_ADD => (Opcode::Add, index + 1),
            Opcode::C_SUBTRACT => (Opcode::Subtract, index + 1),
            Opcode::C_NEGATE => (Opcode::Negate, index + 1),
            Opcode::C_NOT => (Opcode::Not, index + 1),
            Opcode::C_EQUAL => (Opcode::Equals, index + 1),
            Opcode::C_LESS_THAN => (Opcode::LessThan, index + 1),
            Opcode::C_GREATER_THAN => (Opcode::GreaterThan, index + 1),
            Opcode::C_PRINT => (Opcode::Print, index + 1),
            Opcode::C_POP => (Opcode::Pop, index + 1),
            opcode => {
                return Err(DecodeError::InvalidOpcode { value: opcode });
            }
        };
        Ok(Some((opcode, rest)))
    }

    pub fn encode(&self, chunk: &mut IncompleteChunk) {
        match self {
            Opcode::Const(handle) => {
                chunk.emit_u8(Opcode::C_CONST);
                chunk.emit_u32(handle.0);
            }
            Opcode::DefineGlobal(handle) => {
                chunk.emit_u8(Opcode::C_DEFINE_GLOBAL);
                chunk.emit_u32(handle.0);
            }
            Opcode::Return => chunk.emit_u8(Opcode::C_RETURN),
            Opcode::Multiply => chunk.emit_u8(Opcode::C_MULTIPLY),
            Opcode::Divide => chunk.emit_u8(Opcode::C_DIVIDE),
            Opcode::Add => chunk.emit_u8(Opcode::C_ADD),
            Opcode::Subtract => chunk.emit_u8(Opcode::C_SUBTRACT),
            Opcode::Negate => chunk.emit_u8(Opcode::C_NEGATE),
            Opcode::Not => chunk.emit_u8(Opcode::C_NOT),
            Opcode::Equals => chunk.emit_u8(Opcode::C_EQUAL),
            Opcode::LessThan => chunk.emit_u8(Opcode::C_LESS_THAN),
            Opcode::GreaterThan => chunk.emit_u8(Opcode::C_GREATER_THAN),
            Opcode::Print => chunk.emit_u8(Opcode::C_PRINT),
            Opcode::Pop => chunk.emit_u8(Opcode::C_POP),
        }
    }

    pub fn format(&self, buffer: &mut String, chunk: &Chunk) {
        match self {
            Opcode::Const(handle) => {
                buffer.push_str("ldc");
                write!(buffer, " {:<width$}${} = ", " ", handle.0, width = 4).expect(WRITE_FMT_MSG);
                chunk.constants.format_constant(*handle, buffer);
            }
            Opcode::DefineGlobal(handle) => {
                buffer.push_str("dgl");
                write!(buffer, " {:<width$}${} = ", " ", handle.0, width = 4).expect(WRITE_FMT_MSG);
                chunk.constants.format_constant(*handle, buffer);
            }
            Opcode::Return => buffer.push_str("ret"),
            Opcode::Multiply => buffer.push_str("mul"),
            Opcode::Divide => buffer.push_str("div"),
            Opcode::Add => buffer.push_str("add"),
            Opcode::Subtract => buffer.push_str("sub"),
            Opcode::Negate => buffer.push_str("neg"),
            Opcode::Not => buffer.push_str("not"),
            Opcode::Equals => buffer.push_str("eq"),
            Opcode::LessThan => buffer.push_str("lt"),
            Opcode::GreaterThan => buffer.push_str("gt"),
            Opcode::Print => buffer.push_str("print"),
            Opcode::Pop => buffer.push_str("pop"),
        }
    }
}
