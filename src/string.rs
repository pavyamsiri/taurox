use crate::lexer::Span;
use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
    rc::Rc,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    pub name: IdentName,
    pub span: Span,
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

pub type IdentName = Rc<str>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InternSymbol {
    start: u32,
    length: u32,
}

#[derive(Debug)]
pub struct StringInterner {
    handles: HashMap<u64, InternSymbol>,
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

    pub fn intern(&mut self, text: &str) -> InternSymbol {
        let key = Self::hash_string(text);
        if let Some(handle) = self.handles.get(&key) {
            *handle
        } else {
            // Intern string
            let start = self.buffer.len() as u32;
            let length = text.len() as u32;
            let handle = InternSymbol { start, length };
            self.buffer.push_str(text);
            self.handles.insert(key, handle);
            handle
        }
    }

    pub fn get_string(&self, handle: InternSymbol) -> Option<&str> {
        let start = (handle.start) as usize;
        let end = start + (handle.length as usize);
        self.buffer.get(start..end)
    }
}
