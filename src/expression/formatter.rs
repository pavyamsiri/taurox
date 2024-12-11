use crate::parser::{ParserError, ParserErrorKind};

use super::{
    BinaryAssignmentOperator, BinaryOperator, BinaryShortCircuitOperator, ExpressionTreeAtom,
    ExpressionTreeAtomKind, ExpressionTreeNode, ExpressionTreeNodeRef, ExpressionTreeWithRoot,
    UnaryOperator,
};

pub trait ExpressionFormatter {
    fn format(&self, tree: &ExpressionTreeWithRoot) -> String;
    fn format_error(&self, error: &ParserError) -> String;
}

pub struct DebugFormatter;

impl ExpressionFormatter for DebugFormatter {
    fn format(&self, tree: &ExpressionTreeWithRoot) -> String {
        format!("{tree:?}")
    }

    fn format_error(&self, error: &ParserError) -> String {
        format!("{error:?}")
    }
}

pub struct SExpressionFormatter;

impl SExpressionFormatter {
    fn format_node(tree: &ExpressionTreeWithRoot, node: &ExpressionTreeNodeRef) -> String {
        let current_node = &tree.nodes[node.0 as usize];

        match current_node {
            ExpressionTreeNode::Atom(ExpressionTreeAtom {
                kind: ExpressionTreeAtomKind::Nil,
                ..
            }) => "nil".into(),
            ExpressionTreeNode::Atom(ExpressionTreeAtom {
                kind: ExpressionTreeAtomKind::Bool(value),
                ..
            }) => format!("{value}"),
            ExpressionTreeNode::Atom(ExpressionTreeAtom {
                kind: ExpressionTreeAtomKind::Number(value),
                ..
            }) => format!("{value:?}"),
            ExpressionTreeNode::Atom(ExpressionTreeAtom {
                kind: ExpressionTreeAtomKind::Identifier(value),
                ..
            }) => format!("{value}"),
            ExpressionTreeNode::Atom(ExpressionTreeAtom {
                kind: ExpressionTreeAtomKind::StringLiteral(value),
                ..
            }) => format!("{value}"),
            ExpressionTreeNode::Unary { operator, rhs } => {
                format!(
                    "({} {})",
                    SExpressionFormatter::format_unary_operator(operator),
                    SExpressionFormatter::format_node(tree, &rhs),
                )
            }
            ExpressionTreeNode::Binary { operator, lhs, rhs } => {
                format!(
                    "({} {} {})",
                    SExpressionFormatter::format_binary_operator(operator),
                    SExpressionFormatter::format_node(tree, &lhs),
                    SExpressionFormatter::format_node(tree, &rhs),
                )
            }
            ExpressionTreeNode::BinaryAssignment { operator, lhs, rhs } => {
                format!(
                    "({} {lhs} {})",
                    SExpressionFormatter::format_binary_assignment_operator(operator),
                    SExpressionFormatter::format_node(tree, &rhs)
                )
            }
            ExpressionTreeNode::BinaryShortCircuit { operator, lhs, rhs } => {
                format!(
                    "({} {} {})",
                    SExpressionFormatter::format_binary_short_circuit_operator(operator),
                    SExpressionFormatter::format_node(tree, &lhs),
                    SExpressionFormatter::format_node(tree, &rhs),
                )
            }
            ExpressionTreeNode::Group { inner } => {
                format!(
                    "(group {})",
                    SExpressionFormatter::format_node(tree, &inner)
                )
            }
        }
    }

    fn format_unary_operator(operator: &UnaryOperator) -> String {
        match operator {
            UnaryOperator::Bang => "!",
            UnaryOperator::Minus => "-",
        }
        .into()
    }
    fn format_binary_operator(operator: &BinaryOperator) -> String {
        match operator {
            BinaryOperator::Add => "+",
            BinaryOperator::Subtract => "-",
            BinaryOperator::Multiply => "*",
            BinaryOperator::Divide => "/",
            BinaryOperator::LessThan => "<",
            BinaryOperator::LessThanEqual => "<=",
            BinaryOperator::GreaterThan => ">",
            BinaryOperator::GreaterThanEqual => ">=",
            BinaryOperator::EqualEqual => "==",
            BinaryOperator::BangEqual => "!=",
        }
        .into()
    }

    fn format_binary_assignment_operator(operator: &BinaryAssignmentOperator) -> String {
        match operator {
            BinaryAssignmentOperator::Assign => "=",
        }
        .into()
    }

    fn format_binary_short_circuit_operator(operator: &BinaryShortCircuitOperator) -> String {
        match operator {
            BinaryShortCircuitOperator::And => "and",
            BinaryShortCircuitOperator::Or => "or",
        }
        .into()
    }
}

impl ExpressionFormatter for SExpressionFormatter {
    fn format(&self, tree: &ExpressionTreeWithRoot) -> String {
        SExpressionFormatter::format_node(tree, &tree.root)
    }

    fn format_error(&self, error: &ParserError) -> String {
        let line = error.line;
        match error.kind {
            ParserErrorKind::UnexpectedToken { actual, expected } => {
                format!("({line}) Unexpected: A = {actual} E = {expected}")
            }
            ParserErrorKind::NonOperator(token) => {
                format!("({line}) Non-operator: {token}")
            }
            ParserErrorKind::NonExpression(token) => {
                format!("({line}) Non-Expression: {token}")
            }
            ParserErrorKind::LexicalError(ref error) => {
                format!("({line}) Lexical error: {error:?}")
            }
            ParserErrorKind::UnexpectedEof => {
                format!("({line}) Unexpected EOF")
            }
            ParserErrorKind::InvalidStatement(token) => {
                format!("({line}) Invalid statement token: {token}")
            }
            ParserErrorKind::InvalidLValue(token) => {
                format!("({line}) Invalid l-value: {token}")
            }
        }
    }
}
