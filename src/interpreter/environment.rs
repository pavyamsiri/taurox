use super::{
    native::NativeClock,
    value::{LoxValue, NativeFunction},
};
use compact_str::{CompactString, ToCompactString};
use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

#[derive(Debug, Clone)]
pub struct SharedEnvironment {
    inner: Arc<Mutex<Environment>>,
}

#[derive(Debug)]
struct Environment {
    values: HashMap<CompactString, LoxValue>,
    parent: Option<SharedEnvironment>,
}

impl SharedEnvironment {
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
            inner: Arc::new(Mutex::new(Environment {
                values: globals,
                parent: None,
            })),
        }
    }

    pub fn new_scope(&self) -> Self {
        Self {
            inner: Arc::new(Mutex::new(Environment {
                values: HashMap::new(),
                parent: Some(self.clone()),
            })),
        }
    }

    pub fn access(&self, name: &str) -> Option<LoxValue> {
        let inner = self.inner.lock().unwrap();
        inner.access(name)
    }

    pub fn assign(&mut self, name: &str, value: LoxValue) -> Result<(), ()> {
        let mut inner = self.inner.lock().unwrap();
        inner.assign(name, value)
    }

    pub fn declare(&mut self, name: &str, value: LoxValue) {
        let mut inner = self.inner.lock().unwrap();
        inner.declare(name, value)
    }
}

impl Environment {
    pub fn access(&self, name: &str) -> Option<LoxValue> {
        if let Some(value) = self.values.get(name) {
            Some(value.clone())
        } else if let Some(parent) = self.parent.clone() {
            parent.access(name)
        } else {
            None
        }
    }

    pub fn assign(&mut self, name: &str, value: LoxValue) -> Result<(), ()> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_compact_string(), value);
            Ok(())
        } else if let Some(mut parent) = self.parent.clone() {
            parent.assign(name, value)
        } else {
            Err(())
        }
    }

    pub fn declare(&mut self, name: &str, value: LoxValue) {
        self.values.insert(name.to_compact_string(), value);
    }
}
