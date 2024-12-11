use crate::expression::{
    BinaryOperator, ExpressionTreeAtom, ExpressionTreeNode, ExpressionTreeNodeRef,
    ExpressionTreeWithRoot, UnaryOperator,
};
use compact_str::{CompactString, CompactStringExt};
use std::collections::HashMap;
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum RuntimeError {
    #[error("Expected a number but got {0:?}.")]
    NonNumeric(LoxValue),
    #[error("Expected two numbers but got {0:?} and {1:?}")]
    NonNumerics(LoxValue, LoxValue),
    #[error("Expected two numbers or two strings but got {0:?} and {1:?}.")]
    NonAddable(LoxValue, LoxValue),
}

#[derive(Debug, Clone)]
pub enum LoxValue {
    Number(f64),
    String(CompactString),
    Nil,
    Bool(bool),
}

impl std::fmt::Display for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxValue::Number(v) => write!(f, "{v:?}"),
            LoxValue::String(v) => write!(f, "{v}"),
            LoxValue::Nil => write!(f, "nil"),
            LoxValue::Bool(v) => write!(f, "{v}"),
        }
    }
}

// Unary operators
impl LoxValue {
    fn is_truthy(&self) -> bool {
        match self {
            LoxValue::Bool(false) | LoxValue::Nil => false,
            _ => true,
        }
    }

    pub fn logical_not(&self) -> bool {
        !self.is_truthy()
    }

    pub fn numeric_negate(&self) -> Result<LoxValue, RuntimeError> {
        match self {
            LoxValue::Number(v) => Ok(LoxValue::Number(-v)),
            v => Err(RuntimeError::NonNumeric(v.clone())),
        }
    }
}

impl LoxValue {
    // Arithmetic + string concatenation
    pub fn add(&self, other: &LoxValue) -> Result<LoxValue, RuntimeError> {
        match (self, other) {
            (LoxValue::Number(lhs), LoxValue::Number(rhs)) => Ok(LoxValue::Number(lhs + rhs)),
            (LoxValue::String(lhs), LoxValue::String(rhs)) => {
                Ok(LoxValue::String([lhs, rhs].concat_compact()))
            }
            (lhs, rhs) => Err(RuntimeError::NonAddable(lhs.clone(), rhs.clone())),
        }
    }

    pub fn subtract(&self, other: &LoxValue) -> Result<LoxValue, RuntimeError> {
        match (self, other) {
            (LoxValue::Number(lhs), LoxValue::Number(rhs)) => Ok(LoxValue::Number(lhs - rhs)),
            (lhs, rhs) => Err(RuntimeError::NonNumerics(lhs.clone(), rhs.clone())),
        }
    }

    pub fn multiply(&self, other: &LoxValue) -> Result<LoxValue, RuntimeError> {
        match (self, other) {
            (LoxValue::Number(lhs), LoxValue::Number(rhs)) => Ok(LoxValue::Number(lhs * rhs)),
            (lhs, rhs) => Err(RuntimeError::NonNumerics(lhs.clone(), rhs.clone())),
        }
    }

    pub fn divide(&self, other: &LoxValue) -> Result<LoxValue, RuntimeError> {
        match (self, other) {
            (LoxValue::Number(lhs), LoxValue::Number(rhs)) => Ok(LoxValue::Number(lhs / rhs)),
            (lhs, rhs) => Err(RuntimeError::NonNumerics(lhs.clone(), rhs.clone())),
        }
    }

    // Comparison
    pub fn less_than(&self, other: &LoxValue) -> Result<LoxValue, RuntimeError> {
        match (self, other) {
            (LoxValue::Number(lhs), LoxValue::Number(rhs)) => Ok(LoxValue::Bool(lhs < rhs)),
            (lhs, rhs) => Err(RuntimeError::NonNumerics(lhs.clone(), rhs.clone())),
        }
    }

    pub fn less_than_or_equal(&self, other: &LoxValue) -> Result<LoxValue, RuntimeError> {
        match (self, other) {
            (LoxValue::Number(lhs), LoxValue::Number(rhs)) => Ok(LoxValue::Bool(lhs <= rhs)),
            (lhs, rhs) => Err(RuntimeError::NonNumerics(lhs.clone(), rhs.clone())),
        }
    }

    pub fn greater_than(&self, other: &LoxValue) -> Result<LoxValue, RuntimeError> {
        match (self, other) {
            (LoxValue::Number(lhs), LoxValue::Number(rhs)) => Ok(LoxValue::Bool(lhs > rhs)),
            (lhs, rhs) => Err(RuntimeError::NonNumerics(lhs.clone(), rhs.clone())),
        }
    }

    pub fn greater_than_or_equal(&self, other: &LoxValue) -> Result<LoxValue, RuntimeError> {
        match (self, other) {
            (LoxValue::Number(lhs), LoxValue::Number(rhs)) => Ok(LoxValue::Bool(lhs >= rhs)),
            (lhs, rhs) => Err(RuntimeError::NonNumerics(lhs.clone(), rhs.clone())),
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
pub struct ExpressionEvaluator {
    scope: HashMap<CompactString, LoxValue>,
}

impl ExpressionEvaluator {
    pub fn new() -> Self {
        Self {
            scope: HashMap::new(),
        }
    }

    fn evaluate_node(
        &mut self,
        tree: &ExpressionTreeWithRoot,
        node: &ExpressionTreeNodeRef,
    ) -> Result<LoxValue, RuntimeError> {
        let current_node = tree
            .get_node(node)
            .expect("Node ref came from the tree itself so it must exist.");

        let result = match current_node {
            ExpressionTreeNode::Atom(ExpressionTreeAtom::Nil) => LoxValue::Nil,
            ExpressionTreeNode::Atom(ExpressionTreeAtom::Bool(value)) => LoxValue::Bool(*value),
            ExpressionTreeNode::Atom(ExpressionTreeAtom::Number(value)) => LoxValue::Number(*value),
            ExpressionTreeNode::Atom(ExpressionTreeAtom::Identifier(value)) => {
                self.scope.get(value).cloned().unwrap_or(LoxValue::Nil)
            }
            ExpressionTreeNode::Atom(ExpressionTreeAtom::StringLiteral(value)) => {
                LoxValue::String(value.clone())
            }
            ExpressionTreeNode::Unary { operator, rhs } => {
                let rhs = self.evaluate_node(tree, rhs)?;
                self.evaluate_unary(operator, &rhs)?
            }
            ExpressionTreeNode::Binary { operator, lhs, rhs } => {
                let lhs = self.evaluate_node(tree, lhs)?;
                let rhs = self.evaluate_node(tree, rhs)?;
                self.evaluate_binary(operator, &lhs, &rhs)?
            }
            ExpressionTreeNode::Group { inner } => self.evaluate_node(tree, inner)?,
        };
        Ok(result)
    }

    pub fn evaluate(&mut self, tree: &ExpressionTreeWithRoot) -> Result<LoxValue, RuntimeError> {
        self.evaluate_node(tree, &tree.get_root())
    }

    fn evaluate_unary(
        &self,
        operator: &UnaryOperator,
        rhs: &LoxValue,
    ) -> Result<LoxValue, RuntimeError> {
        match operator {
            UnaryOperator::Bang => Ok(LoxValue::Bool(rhs.logical_not())),
            UnaryOperator::Minus => rhs.numeric_negate(),
        }
    }

    fn evaluate_binary(
        &self,
        operator: &BinaryOperator,
        lhs: &LoxValue,
        rhs: &LoxValue,
    ) -> Result<LoxValue, RuntimeError> {
        match operator {
            BinaryOperator::Add => lhs.add(rhs),
            BinaryOperator::Subtract => lhs.subtract(rhs),
            BinaryOperator::Multiply => lhs.multiply(rhs),
            BinaryOperator::Divide => lhs.divide(rhs),
            BinaryOperator::LessThan => lhs.less_than(rhs),
            BinaryOperator::LessThanEqual => lhs.less_than_or_equal(rhs),
            BinaryOperator::GreaterThan => lhs.greater_than(rhs),
            BinaryOperator::GreaterThanEqual => lhs.greater_than_or_equal(rhs),
            BinaryOperator::EqualEqual => Ok(LoxValue::Bool(lhs.is_equal(rhs))),
            BinaryOperator::BangEqual => Ok(LoxValue::Bool(lhs.is_not_equal(rhs))),
        }
    }
}
