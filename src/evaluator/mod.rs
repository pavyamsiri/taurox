pub mod formatter;

use crate::{
    expression::{
        BinaryAssignmentOperator, BinaryOperator, ExpressionTreeAtom, ExpressionTreeAtomKind,
        ExpressionTreeNode, ExpressionTreeNodeRef, ExpressionTreeWithRoot, UnaryOperator,
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
            LoxValue::Number(v) => write!(f, "{v}"),
            LoxValue::String(v) => write!(f, "{v}"),
            LoxValue::Nil => write!(f, "nil"),
            LoxValue::Bool(v) => write!(f, "{v}"),
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
            ExpressionTreeNode::Atom(ExpressionTreeAtom {
                kind: ExpressionTreeAtomKind::Nil,
                ..
            }) => LoxValue::Nil,
            ExpressionTreeNode::Atom(ExpressionTreeAtom {
                kind: ExpressionTreeAtomKind::Bool(value),
                ..
            }) => LoxValue::Bool(*value),
            ExpressionTreeNode::Atom(ExpressionTreeAtom {
                kind: ExpressionTreeAtomKind::Number(value),
                ..
            }) => LoxValue::Number(*value),
            ExpressionTreeNode::Atom(ExpressionTreeAtom {
                kind: ExpressionTreeAtomKind::Identifier(name),
                ..
            }) => environment
                .access(name)
                .ok_or(RuntimeError {
                    kind: RuntimeErrorKind::InvalidAccess(name.clone()),
                    line,
                })?
                .clone(),
            ExpressionTreeNode::Atom(ExpressionTreeAtom {
                kind: ExpressionTreeAtomKind::StringLiteral(value),
                ..
            }) => LoxValue::String(value.clone()),
            ExpressionTreeNode::Unary { operator, rhs } => {
                let rhs = ExpressionEvaluator::evaluate_expression_node(tree, rhs, environment)?;
                ExpressionEvaluator::evaluate_unary(operator, &rhs)
                    .map_err(|kind| RuntimeError { kind, line })?
            }
            ExpressionTreeNode::Binary { operator, lhs, rhs } => {
                let lhs = ExpressionEvaluator::evaluate_expression_node(tree, lhs, environment)?;
                let rhs = ExpressionEvaluator::evaluate_expression_node(tree, rhs, environment)?;
                ExpressionEvaluator::evaluate_binary(operator, &lhs, &rhs)
                    .map_err(|kind| RuntimeError { kind, line })?
            }
            ExpressionTreeNode::Group { inner } => {
                ExpressionEvaluator::evaluate_expression_node(tree, inner, environment)?
            }
            ExpressionTreeNode::BinaryAssignment {
                operator: BinaryAssignmentOperator::Assign,
                lhs,
                rhs,
            } => {
                let rhs = ExpressionEvaluator::evaluate_expression_node(tree, rhs, environment)?;
                let _ = environment
                    .assign(lhs, rhs.clone())
                    .map_err(|_| RuntimeError {
                        kind: RuntimeErrorKind::InvalidAccess(lhs.clone()),
                        line,
                    })?;
                rhs
            }
        };
        Ok(result)
    }

    pub fn evaluate_expression(
        tree: &ExpressionTreeWithRoot,
        environment: &mut Environment,
    ) -> Result<LoxValue, RuntimeError> {
        ExpressionEvaluator::evaluate_expression_node(tree, &tree.get_root_ref(), environment)
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
}
