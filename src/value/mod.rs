pub mod error;
pub mod formatter;

use super::environment::SharedEnvironment;
use crate::{parser::statement::Statement, string::Ident};
use compact_str::{CompactString, CompactStringExt};
use error::{RuntimeError, RuntimeErrorKind};
use std::{collections::HashMap, sync::Arc};

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

#[derive(Debug, Clone)]
pub enum LoxValue {
    Number(f64),
    String(CompactString),
    Nil,
    Bool(bool),
    NativeFunction(Arc<dyn NativeFunction>),
    Function(Function),
    Class {
        name: Ident,
        methods: Vec<Function>,
    },
    Instance {
        class: Ident,
        fields: HashMap<CompactString, LoxValue>,
    },
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
            Self::Function(Function { name, .. }) => {
                write!(f, "<fn {name}>")
            }
            Self::Class { name, .. } => {
                write!(f, "{name}")
            }
            Self::Instance { class, .. } => {
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
