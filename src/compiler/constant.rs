use super::Chunk;
use crate::string::{InternSymbol, StringInterner};
use std::fmt::Write;

const WRITE_FMT_MSG: &'static str =
    "Encountered an error while attempting to write format string to buffer.";

#[derive(Debug, Clone)]
pub struct FunctionConstant {
    // TODO(pavyamsiri): Seems a bit dumb to copy a chunk around, each chunk should be allocated into a pool.
    pub name: InternSymbol,
    pub chunk: Chunk,
    pub arity: u32,
}

#[derive(Debug, Clone, Copy)]
pub struct ConstRef(pub u32);

#[derive(Debug, Clone)]
pub enum LoxConstant {
    Number(f64),
    Nil,
    Bool(bool),
    String(InternSymbol),
    Function(FunctionConstant),
}

impl std::cmp::PartialEq for LoxConstant {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LoxConstant::Nil, LoxConstant::Nil) => true,
            (LoxConstant::Number(lhs), LoxConstant::Number(rhs)) => lhs == rhs,
            (LoxConstant::Bool(lhs), LoxConstant::Bool(rhs)) => lhs == rhs,
            (LoxConstant::String(lhs), LoxConstant::String(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl std::fmt::Display for LoxConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxConstant::Number(value) => write!(f, "{value}"),
            LoxConstant::Nil => write!(f, "nil"),
            LoxConstant::Bool(value) => write!(f, "{value}"),
            LoxConstant::String(_) => write!(f, "Interned String"),
            LoxConstant::Function(_) => write!(f, "Function"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConstantPool {
    data: Vec<LoxConstant>,
    interned_strings: StringInterner,
}

impl ConstantPool {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            interned_strings: StringInterner::new(),
        }
    }

    pub fn get(&self, handle: ConstRef) -> Option<&LoxConstant> {
        self.data.get(handle.0 as usize)
    }

    pub fn get_string_through_ref(&self, handle: ConstRef) -> Option<&str> {
        let value = self.get(handle)?;
        match value {
            LoxConstant::String(handle) => self.get_string(*handle),
            _ => None,
        }
    }

    pub fn get_string(&self, handle: InternSymbol) -> Option<&str> {
        self.interned_strings.get_string(handle)
    }

    pub fn push_constant(&mut self, value: LoxConstant) -> ConstRef {
        // Find if there is a constant that has the same value
        if let Some(old_handle) = self
            .data
            .iter()
            .enumerate()
            .find(|&(_, v)| v == &value)
            .and_then(|(index, _)| Some(index))
        {
            ConstRef(old_handle as u32)
        } else {
            self.data.push(value);
            ConstRef((self.data.len() - 1) as u32)
        }
    }

    pub fn push_str(&mut self, text: &str) -> ConstRef {
        let handle = self.interned_strings.intern(text);
        let value = LoxConstant::String(handle);
        self.push_constant(value)
    }

    pub fn format_constant(&self, handle: ConstRef, buffer: &mut String) {
        let Some(value) = self.get(handle) else {
            buffer.push_str("INVALID_CONSTANT_HANDLE");
            return;
        };
        match value {
            LoxConstant::String(handle) => {
                let value = self.get_string(*handle).unwrap_or("INVALID_STRING_HANDLE");
                write!(buffer, "\"{value}\"").expect(WRITE_FMT_MSG);
            }
            value => {
                write!(buffer, "{value}").expect(WRITE_FMT_MSG);
            }
        }
    }
}
