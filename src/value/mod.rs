pub mod error;
pub mod formatter;

use super::environment::SharedEnvironment;
use crate::{parser::statement::Statement, string::Ident};
use compact_str::{CompactString, CompactStringExt};
use error::{RuntimeError, RuntimeErrorKind};
use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

pub trait NativeFunction: std::fmt::Debug + Send + Sync {
    fn get_name(&self) -> &'static str;
    fn get_parameters(&self) -> &'static [&'static str];
    fn call(&self, environment: &mut SharedEnvironment) -> Result<LoxValue, RuntimeError>;
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Ident,
    pub parameters: Vec<Ident>,
    pub body: Vec<Statement>,
    pub closure: SharedEnvironment,
    pub is_constructor: bool,
}

impl Function {
    pub fn bind(&self, value: Arc<Instance>) -> Function {
        let mut bound_function = self.clone();
        let mut new_closure = bound_function.closure.new_scope();
        new_closure.declare("this", LoxValue::Instance(value));
        bound_function.closure = new_closure;
        bound_function
    }
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: Ident,
    pub methods: Vec<Arc<Function>>,
    pub super_class: Option<Arc<Class>>,
}

impl Class {
    pub fn find_method(&self, name: &str) -> Option<&Function> {
        if let Some(method) = self.methods.iter().find(|m| &(*m.name.name) == name) {
            return Some(method);
        }

        if let Some(ref super_class) = self.super_class {
            super_class.find_method(name)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub class: Arc<Class>,
    pub fields: Arc<Mutex<HashMap<CompactString, LoxValue>>>,
}

#[derive(Debug, Clone)]
pub enum LoxValue {
    Number(f64),
    String(CompactString),
    Nil,
    Bool(bool),
    NativeFunction(Arc<dyn NativeFunction>),
    Function(Arc<Function>),
    Class(Arc<Class>),
    Instance(Arc<Instance>),
}

impl std::fmt::Display for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(v) => write!(f, "{v}"),
            Self::String(v) => write!(f, "{v}"),
            Self::Nil => write!(f, "nil"),
            Self::Bool(v) => write!(f, "{v}"),
            Self::NativeFunction(fun) => {
                write!(f, "<native fn `{}`>", fun.get_name())
            }
            Self::Function(func) => {
                let name = &func.name;
                write!(f, "<fn {name}>")
            }
            Self::Class(class) => {
                let name = &class.name;
                write!(f, "{name}")
            }
            Self::Instance(instance) => {
                let class = &instance.class.name;
                write!(f, "{class} instance")
            }
        }
    }
}

// Unary operators
impl LoxValue {
    pub fn is_truthy(&self) -> bool {
        match self {
            LoxValue::Bool(false) | LoxValue::Nil => false,
            _ => true,
        }
    }

    pub fn logical_not(&self) -> bool {
        !self.is_truthy()
    }

    pub fn numeric_negate(&self) -> Result<LoxValue, RuntimeErrorKind> {
        match self {
            LoxValue::Number(v) => Ok(LoxValue::Number(-v)),
            v => Err(RuntimeErrorKind::NonNumeric(v.clone())),
        }
    }
}

impl LoxValue {
    // Arithmetic + string concatenation
    pub fn add(&self, other: &LoxValue) -> Result<LoxValue, RuntimeErrorKind> {
        match (self, other) {
            (LoxValue::Number(lhs), LoxValue::Number(rhs)) => Ok(LoxValue::Number(lhs + rhs)),
            (LoxValue::String(lhs), LoxValue::String(rhs)) => {
                Ok(LoxValue::String([lhs, rhs].concat_compact()))
            }
            (lhs, rhs) => Err(RuntimeErrorKind::NonAddable(lhs.clone(), rhs.clone())),
        }
    }

    pub fn subtract(&self, other: &LoxValue) -> Result<LoxValue, RuntimeErrorKind> {
        match (self, other) {
            (LoxValue::Number(lhs), LoxValue::Number(rhs)) => Ok(LoxValue::Number(lhs - rhs)),
            (lhs, rhs) => Err(RuntimeErrorKind::NonNumerics(lhs.clone(), rhs.clone())),
        }
    }

    pub fn multiply(&self, other: &LoxValue) -> Result<LoxValue, RuntimeErrorKind> {
        match (self, other) {
            (LoxValue::Number(lhs), LoxValue::Number(rhs)) => Ok(LoxValue::Number(lhs * rhs)),
            (lhs, rhs) => Err(RuntimeErrorKind::NonNumerics(lhs.clone(), rhs.clone())),
        }
    }

    pub fn divide(&self, other: &LoxValue) -> Result<LoxValue, RuntimeErrorKind> {
        match (self, other) {
            (LoxValue::Number(lhs), LoxValue::Number(rhs)) => Ok(LoxValue::Number(lhs / rhs)),
            (lhs, rhs) => Err(RuntimeErrorKind::NonNumerics(lhs.clone(), rhs.clone())),
        }
    }

    // Comparison
    pub fn less_than(&self, other: &LoxValue) -> Result<LoxValue, RuntimeErrorKind> {
        match (self, other) {
            (LoxValue::Number(lhs), LoxValue::Number(rhs)) => Ok(LoxValue::Bool(lhs < rhs)),
            (lhs, rhs) => Err(RuntimeErrorKind::NonNumerics(lhs.clone(), rhs.clone())),
        }
    }

    pub fn less_than_or_equal(&self, other: &LoxValue) -> Result<LoxValue, RuntimeErrorKind> {
        match (self, other) {
            (LoxValue::Number(lhs), LoxValue::Number(rhs)) => Ok(LoxValue::Bool(lhs <= rhs)),
            (lhs, rhs) => Err(RuntimeErrorKind::NonNumerics(lhs.clone(), rhs.clone())),
        }
    }

    pub fn greater_than(&self, other: &LoxValue) -> Result<LoxValue, RuntimeErrorKind> {
        match (self, other) {
            (LoxValue::Number(lhs), LoxValue::Number(rhs)) => Ok(LoxValue::Bool(lhs > rhs)),
            (lhs, rhs) => Err(RuntimeErrorKind::NonNumerics(lhs.clone(), rhs.clone())),
        }
    }

    pub fn greater_than_or_equal(&self, other: &LoxValue) -> Result<LoxValue, RuntimeErrorKind> {
        match (self, other) {
            (LoxValue::Number(lhs), LoxValue::Number(rhs)) => Ok(LoxValue::Bool(lhs >= rhs)),
            (lhs, rhs) => Err(RuntimeErrorKind::NonNumerics(lhs.clone(), rhs.clone())),
        }
    }

    // Equality
    pub fn is_equal(&self, other: &LoxValue) -> bool {
        match (self, other) {
            (LoxValue::Number(lhs), LoxValue::Number(rhs)) => lhs == rhs,
            (LoxValue::String(lhs), LoxValue::String(rhs)) => lhs == rhs,
            (LoxValue::Nil, LoxValue::Nil) => true,
            (LoxValue::Bool(lhs), LoxValue::Bool(rhs)) => lhs == rhs,
            _ => false,
        }
    }

    pub fn is_not_equal(&self, other: &LoxValue) -> bool {
        !self.is_equal(other)
    }
}
