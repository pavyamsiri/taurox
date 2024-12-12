use super::{
    native::NativeClock,
    value::{LoxValue, NativeFunction},
};
use compact_str::{CompactString, ToCompactString};
use std::{collections::HashMap, sync::Arc};

#[derive(Debug, Clone)]
pub struct Locals {
    scopes: Vec<HashMap<CompactString, LoxValue>>,
}

impl Locals {
    pub fn new() -> Self {
        Self { scopes: Vec::new() }
    }

    pub fn new_enclosed(&self, closure: Locals) -> Self {
        let mut new_scopes = self.scopes.clone();

        new_scopes.extend(closure.scopes);

        Self { scopes: new_scopes }
    }

    pub fn access(&self, name: &str) -> Option<&LoxValue> {
        // Go from the innermost scope to the outermost scope
        for scope in self.scopes.iter().rev() {
            // Found the variable
            if scope.contains_key(name) {
                return scope.get(name);
            }
        }

        None
    }

    pub fn assign(&mut self, name: &str, value: LoxValue) -> Result<(), LoxValue> {
        // Go from the innermost scope to the outermost scope
        for scope in self.scopes.iter_mut().rev() {
            // Found the variable
            if scope.contains_key(name) {
                scope.insert(name.to_compact_string(), value);
                return Ok(());
            }
        }
        Err(value)
    }

    pub fn declare(&mut self, name: &str, value: LoxValue) -> Result<(), LoxValue> {
        // Add to innermost scope
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_compact_string(), value);
            Ok(())
        } else {
            Err(value)
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    globals: HashMap<CompactString, LoxValue>,
    locals: Locals,
}

impl std::fmt::Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Globals:")?;
        for (key, value) in &self.globals {
            writeln!(f, "  {}: {}", key, value)?;
        }

        writeln!(f, "Scopes:")?;
        for (i, scope) in self.locals.scopes.iter().enumerate() {
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
            locals: Locals::new(),
        }
    }

    pub fn new_enclosed(&self, closure: Locals) -> Self {
        Self {
            globals: self.globals.clone(),
            locals: self.locals.new_enclosed(closure),
        }
    }

    pub fn copy_locals(&self) -> Locals {
        self.locals.clone()
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
        if let Some(value) = self.locals.access(name) {
            Some(value)
        } else {
            // Now return from globals
            self.get_global(name)
        }
    }

    pub fn assign(&mut self, name: &str, value: LoxValue) -> Result<(), ()> {
        // Go from the innermost scope to the outermost scope
        match self.locals.assign(name, value) {
            Ok(()) => return Ok(()),
            Err(value) => {
                // Check global
                self.assign_global(name, value)
            }
        }
    }

    pub fn declare(&mut self, name: &str, value: LoxValue) {
        // Go from the innermost scope to the outermost scope
        match self.locals.declare(name, value) {
            Ok(()) => {}
            Err(value) => {
                // Check global
                self.declare_global(name, value)
            }
        }
    }

    pub fn enter_scope(&mut self) {
        self.locals.enter_scope();
    }

    pub fn exit_scope(&mut self) {
        self.locals.exit_scope();
    }
}
