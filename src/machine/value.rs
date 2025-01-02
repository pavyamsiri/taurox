use super::{error::VMRuntimeErrorKind, garbage::VMStringRef};

#[derive(Debug, Clone)]
pub enum VMValue {
    Number(f64),
    Nil,
    Bool(bool),
    String(VMStringRef),
}

impl std::fmt::Display for VMValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(v) => write!(f, "{v}"),
            Self::Nil => write!(f, "nil"),
            Self::Bool(v) => write!(f, "{v}"),
            Self::String(v) => write!(f, "<string ref {v:?}>"),
        }
    }
}

// Unary operators
impl VMValue {
    pub fn is_truthy(&self) -> bool {
        match self {
            VMValue::Bool(false) | VMValue::Nil => false,
            _ => true,
        }
    }

    pub fn logical_not(&self) -> bool {
        !self.is_truthy()
    }

    pub fn numeric_negate(&self) -> Result<VMValue, VMRuntimeErrorKind> {
        match self {
            VMValue::Number(v) => Ok(VMValue::Number(-v)),
            v => Err(VMRuntimeErrorKind::NonNumeric(v.clone())),
        }
    }
}

// Binary
impl VMValue {
    // Arithmetic + string concatenation
    pub fn subtract(&self, other: &VMValue) -> Result<VMValue, VMRuntimeErrorKind> {
        match (self, other) {
            (VMValue::Number(lhs), VMValue::Number(rhs)) => Ok(VMValue::Number(lhs - rhs)),
            (lhs, rhs) => Err(VMRuntimeErrorKind::NonNumerics(lhs.clone(), rhs.clone())),
        }
    }

    pub fn multiply(&self, other: &VMValue) -> Result<VMValue, VMRuntimeErrorKind> {
        match (self, other) {
            (VMValue::Number(lhs), VMValue::Number(rhs)) => Ok(VMValue::Number(lhs * rhs)),
            (lhs, rhs) => Err(VMRuntimeErrorKind::NonNumerics(lhs.clone(), rhs.clone())),
        }
    }

    pub fn divide(&self, other: &VMValue) -> Result<VMValue, VMRuntimeErrorKind> {
        match (self, other) {
            (VMValue::Number(lhs), VMValue::Number(rhs)) => Ok(VMValue::Number(lhs / rhs)),
            (lhs, rhs) => Err(VMRuntimeErrorKind::NonNumerics(lhs.clone(), rhs.clone())),
        }
    }

    // Comparison
    pub fn less_than(&self, other: &VMValue) -> Result<VMValue, VMRuntimeErrorKind> {
        match (self, other) {
            (VMValue::Number(lhs), VMValue::Number(rhs)) => Ok(VMValue::Bool(lhs < rhs)),
            (lhs, rhs) => Err(VMRuntimeErrorKind::NonNumerics(lhs.clone(), rhs.clone())),
        }
    }

    pub fn greater_than(&self, other: &VMValue) -> Result<VMValue, VMRuntimeErrorKind> {
        match (self, other) {
            (VMValue::Number(lhs), VMValue::Number(rhs)) => Ok(VMValue::Bool(lhs > rhs)),
            (lhs, rhs) => Err(VMRuntimeErrorKind::NonNumerics(lhs.clone(), rhs.clone())),
        }
    }
}
