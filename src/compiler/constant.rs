use crate::{machine::value::VMValue, value::LoxValue};
use std::fmt::Write;
use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
};

const WRITE_FMT_MSG: &'static str =
    "Encountered an error while attempting to write format string to buffer.";

#[derive(Debug, Clone, Copy)]
pub struct ConstRef(pub u32);

#[derive(Debug, Clone, Copy)]
pub struct InternStringHandle {
    start: u32,
    length: u32,
}

#[derive(Debug, Clone, Copy)]
pub enum LoxConstant {
    Number(f64),
    Nil,
    Bool(bool),
    String(InternStringHandle),
}

impl std::fmt::Display for LoxConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxConstant::Number(value) => write!(f, "{value}"),
            LoxConstant::Nil => write!(f, "nil"),
            LoxConstant::Bool(value) => write!(f, "{value}"),
            LoxConstant::String(_) => write!(f, "Interned String"),
        }
    }
}

impl From<&LoxConstant> for LoxValue {
    fn from(value: &LoxConstant) -> Self {
        match value {
            LoxConstant::Number(value) => Self::Number(*value),
            LoxConstant::Nil => Self::Nil,
            LoxConstant::Bool(value) => Self::Bool(*value),
            LoxConstant::String(_) => todo!(),
        }
    }
}

impl From<&LoxConstant> for VMValue {
    fn from(value: &LoxConstant) -> Self {
        match value {
            LoxConstant::Number(value) => Self::Number(*value),
            LoxConstant::Nil => Self::Nil,
            LoxConstant::Bool(value) => Self::Bool(*value),
            LoxConstant::String(_) => todo!(),
        }
    }
}

#[derive(Debug)]
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

    pub fn get_string(&self, handle: InternStringHandle) -> Option<&str> {
        self.interned_strings.get_string(handle)
    }

    pub fn push_constant(&mut self, value: LoxConstant) -> ConstRef {
        self.data.push(value);
        ConstRef((self.data.len() - 1) as u32)
    }

    pub fn push_str(&mut self, text: &str) -> ConstRef {
        let handle = self.interned_strings.intern(text);
        self.data.push(LoxConstant::String(handle));
        ConstRef((self.data.len() - 1) as u32)
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

#[derive(Debug)]
struct StringInterner {
    handles: HashMap<u64, InternStringHandle>,
    buffer: String,
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            handles: HashMap::new(),
            buffer: String::new(),
        }
    }

    fn hash_string(text: &str) -> u64 {
        let mut hasher = std::hash::DefaultHasher::new();
        text.hash(&mut hasher);
        hasher.finish()
    }

    pub fn intern(&mut self, text: &str) -> InternStringHandle {
        let key = Self::hash_string(text);
        if let Some(handle) = self.handles.get(&key) {
            *handle
        } else {
            // Intern string
            let start = self.buffer.len() as u32;
            let length = text.len() as u32;
            let handle = InternStringHandle { start, length };
            self.buffer.push_str(text);
            self.handles.insert(key, handle);
            handle
        }
    }

    pub fn get_string(&self, handle: InternStringHandle) -> Option<&str> {
        let start = (handle.start) as usize;
        let end = start + (handle.length as usize);
        self.buffer.get(start..end)
    }
}
