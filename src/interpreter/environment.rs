use super::{
    native::NativeClock,
    value::{LoxValue, NativeFunction},
};
use compact_str::{CompactString, ToCompactString};
use std::{collections::HashMap, sync::Arc};

#[derive(Debug)]
pub struct Environment {
    globals: HashMap<CompactString, LoxValue>,
    scopes: Vec<HashMap<CompactString, LoxValue>>,
}

impl std::fmt::Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Globals:")?;
        for (key, value) in &self.globals {
            writeln!(f, "  {}: {}", key, value)?;
        }

        writeln!(f, "Scopes:")?;
        for (i, scope) in self.scopes.iter().enumerate() {
            writeln!(f, "  Scope {}:", i)?;
            for (key, value) in scope {
                writeln!(f, "    {}: {}", key, value)?;
            }
        }

        Ok(())
    }
}

impl Environment {
    pub fn new() -> Self {
        let mut globals = HashMap::new();

        // Inject native functions here
        {
            let clock = NativeClock;

            globals.insert(
                clock.get_name().to_compact_string(),
                LoxValue::NativeFunction(Arc::new(clock)),
            );
        }

        Self {
            globals,
            scopes: Vec::new(),
        }
    }

    fn get_global(&self, name: &str) -> Option<&LoxValue> {
        self.globals.get(name)
    }

    fn declare_global(&mut self, name: &str, value: LoxValue) {
        self.globals.insert(name.to_compact_string(), value);
    }

    fn assign_global(&mut self, name: &str, value: LoxValue) -> Result<(), ()> {
        if self.globals.contains_key(name) {
            self.globals.insert(name.to_compact_string(), value);
            Ok(())
        } else {
            Err(())
        }
    }

    pub fn access(&self, name: &str) -> Option<&LoxValue> {
        // Go from the innermost scope to the outermost scope
        for scope in self.scopes.iter().rev() {
            // Found the variable
            if scope.contains_key(name) {
                return scope.get(name);
            }
        }

        // Now return from globals
        self.get_global(name)
    }

    pub fn assign(&mut self, name: &str, value: LoxValue) -> Result<(), ()> {
        // Go from the innermost scope to the outermost scope
        for scope in self.scopes.iter_mut().rev() {
            // Found the variable
            if scope.contains_key(name) {
                scope.insert(name.to_compact_string(), value);
                return Ok(());
            }
        }

        // Check global
        self.assign_global(name, value)
    }

    pub fn declare(&mut self, name: &str, value: LoxValue) {
        // Add to outermost scope
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_compact_string(), value);
        } else {
            self.declare_global(name, value);
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }
}
