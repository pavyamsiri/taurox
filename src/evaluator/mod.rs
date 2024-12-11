pub mod formatter;

use std::sync::Arc;

use crate::{
    expression::{
        BinaryAssignmentOperator, BinaryOperator, BinaryShortCircuitOperator, ExpressionTreeAtom,
        ExpressionTreeAtomKind, ExpressionTreeNode, ExpressionTreeNodeRef, ExpressionTreeWithRoot,
        UnaryOperator,
    },
    interpreter::Environment,
};
use compact_str::{CompactString, CompactStringExt};
use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum RuntimeErrorKind {
    #[error("Non-Number {{Unary}}: {0}")]
    NonNumeric(LoxValue),
    #[error("Non-Numbers {{Binary}}: [{0} , {1}]")]
    NonNumerics(LoxValue, LoxValue),
    #[error("Non-Numbers/Non-Strings {{Binary}}: [{0} , {1}]")]
    NonAddable(LoxValue, LoxValue),
    #[error("Invalid Access: {0}")]
    InvalidAccess(CompactString),
}

#[derive(Debug, Error, Clone)]
#[error("({line}) {kind}")]
pub struct RuntimeError {
    #[source]
    kind: RuntimeErrorKind,
    line: u32,
}

pub trait NativeFunction: std::fmt::Debug + Send + Sync {
    fn get_name(&self) -> &'static str;
    fn call(&self, arguments: &[LoxValue]) -> Result<LoxValue, RuntimeError>;
}

#[derive(Debug, Clone)]
pub enum LoxValue {
    Number(f64),
    String(CompactString),
    Nil,
    Bool(bool),
    NativeFunction(Arc<dyn NativeFunction>),
}

impl std::fmt::Display for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxValue::Number(v) => write!(f, "{v}"),
            LoxValue::String(v) => write!(f, "{v}"),
            LoxValue::Nil => write!(f, "nil"),
            LoxValue::Bool(v) => write!(f, "{v}"),
            LoxValue::NativeFunction(fun) => {
                write!(f, "<native fn `{}`>", fun.get_name())
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
pub struct ExpressionEvaluator;

impl ExpressionEvaluator {
    fn evaluate_expression_atom(
        atom: &ExpressionTreeAtom,
        environment: &mut Environment,
    ) -> Result<LoxValue, RuntimeErrorKind> {
        let result = match &atom.kind {
            ExpressionTreeAtomKind::Number(v) => LoxValue::Number(*v),
            ExpressionTreeAtomKind::Bool(v) => LoxValue::Bool(*v),
            ExpressionTreeAtomKind::Nil => LoxValue::Nil,
            ExpressionTreeAtomKind::StringLiteral(ref v) => LoxValue::String(v.clone()),
            ExpressionTreeAtomKind::Identifier(ref name) => environment
                .access(name)
                .ok_or(RuntimeErrorKind::InvalidAccess(name.clone()))?
                .clone(),
            ExpressionTreeAtomKind::NativeFunction(fun) => LoxValue::NativeFunction(fun.clone()),
        };
        Ok(result)
    }

    fn evaluate_expression_node(
        tree: &ExpressionTreeWithRoot,
        node: &ExpressionTreeNodeRef,
        environment: &mut Environment,
    ) -> Result<LoxValue, RuntimeError> {
        let current_node = tree
            .get_node(node)
            .expect("Node ref came from the tree so it must exist.");
        let line = tree
            .get_line(node)
            .expect("Node ref came from the tree so it must exist.");

        let result = match current_node {
            ExpressionTreeNode::Atom(atom) => Self::evaluate_expression_atom(atom, environment)
                .map_err(|kind| RuntimeError { kind, line })?,
            ExpressionTreeNode::Unary { operator, rhs } => {
                let rhs = Self::evaluate_expression_node(tree, rhs, environment)?;
                Self::evaluate_unary(operator, &rhs).map_err(|kind| RuntimeError { kind, line })?
            }
            ExpressionTreeNode::Group { inner } => {
                Self::evaluate_expression_node(tree, inner, environment)?
            }
            ExpressionTreeNode::Binary { operator, lhs, rhs } => {
                let lhs = Self::evaluate_expression_node(tree, lhs, environment)?;
                let rhs = Self::evaluate_expression_node(tree, rhs, environment)?;
                Self::evaluate_binary(operator, &lhs, &rhs)
                    .map_err(|kind| RuntimeError { kind, line })?
            }
            ExpressionTreeNode::BinaryAssignment {
                operator: BinaryAssignmentOperator::Assign,
                lhs,
                rhs,
            } => {
                let rhs = Self::evaluate_expression_node(tree, rhs, environment)?;
                let _ = environment
                    .assign(lhs, rhs.clone())
                    .map_err(|_| RuntimeError {
                        kind: RuntimeErrorKind::InvalidAccess(lhs.clone()),
                        line,
                    })?;
                rhs
            }
            ExpressionTreeNode::BinaryShortCircuit { operator, lhs, rhs } => {
                Self::evaluate_binary_short_circuit(operator, lhs, rhs, tree, environment)?
            }
        };
        Ok(result)
    }

    pub fn evaluate_expression(
        tree: &ExpressionTreeWithRoot,
        environment: &mut Environment,
    ) -> Result<LoxValue, RuntimeError> {
        Self::evaluate_expression_node(tree, &tree.get_root_ref(), environment)
    }

    fn evaluate_unary(
        operator: &UnaryOperator,
        rhs: &LoxValue,
    ) -> Result<LoxValue, RuntimeErrorKind> {
        match operator {
            UnaryOperator::Bang => Ok(LoxValue::Bool(rhs.logical_not())),
            UnaryOperator::Minus => rhs.numeric_negate(),
        }
    }

    fn evaluate_binary(
        operator: &BinaryOperator,
        lhs: &LoxValue,
        rhs: &LoxValue,
    ) -> Result<LoxValue, RuntimeErrorKind> {
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

    fn evaluate_binary_short_circuit(
        operator: &BinaryShortCircuitOperator,
        lhs: &ExpressionTreeNodeRef,
        rhs: &ExpressionTreeNodeRef,
        tree: &ExpressionTreeWithRoot,
        environment: &mut Environment,
    ) -> Result<LoxValue, RuntimeError> {
        let lhs = { Self::evaluate_expression_node(tree, lhs, environment)? };

        match operator {
            BinaryShortCircuitOperator::And => {
                if !lhs.is_truthy() {
                    Ok(lhs)
                } else {
                    let rhs = Self::evaluate_expression_node(tree, rhs, environment)?;
                    Ok(rhs)
                }
            }
            BinaryShortCircuitOperator::Or => {
                if lhs.is_truthy() {
                    Ok(lhs)
                } else {
                    let rhs = Self::evaluate_expression_node(tree, rhs, environment)?;
                    Ok(rhs)
                }
            }
        }
    }
}
