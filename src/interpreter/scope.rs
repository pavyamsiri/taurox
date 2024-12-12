use compact_str::{CompactString, ToCompactString};
use std::collections::HashMap;

pub enum Liveness {
    Undefined,
    Defined,
}

pub struct Scope {
    levels: Vec<HashMap<CompactString, Liveness>>,
}

impl Scope {
    pub fn enter_scope(&mut self) {
        self.levels.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.levels.pop();
    }

    pub fn declare(&mut self, name: &str) {
        if let Some(current_level) = self.levels.last_mut() {
            current_level.insert(name.to_compact_string(), Liveness::Undefined);
        }
    }

    pub fn define(&mut self, name: &str) {
        if let Some(current_level) = self.levels.last_mut() {
            current_level.insert(name.to_compact_string(), Liveness::Defined);
        }
    }

    pub fn get(&self, key: &CompactString) -> Option<&Liveness> {
        self.levels.last().and_then(|level| level.get(key))
    }
}
