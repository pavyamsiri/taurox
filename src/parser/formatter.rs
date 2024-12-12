use crate::parser::{ParserError, ParserErrorKind};

use super::expression::{
    Expression, ExpressionAtom, ExpressionAtomKind, ExpressionNode, ExpressionNodeRef,
    InfixOperator, InfixShortCircuitOperator, PrefixOperator,
};

pub trait ExpressionFormatter {
    fn format(&self, tree: &Expression) -> String;
    fn format_error(&self, error: &ParserError) -> String;
}

pub struct DebugFormatter;

impl ExpressionFormatter for DebugFormatter {
    fn format(&self, tree: &Expression) -> String {
        format!("{tree:?}")
    }

    fn format_error(&self, error: &ParserError) -> String {
        format!("{error:?}")
    }
}

pub struct SExpressionFormatter;

impl SExpressionFormatter {
    fn format_atom(atom: &ExpressionAtom) -> String {
        match atom.kind {
            ExpressionAtomKind::Number(v) => format!("{v:?}"),
            ExpressionAtomKind::Bool(v) => format!("{v}"),
            ExpressionAtomKind::Nil => "nil".into(),
            ExpressionAtomKind::Identifier(ref name) => format!("{name}"),
            ExpressionAtomKind::StringLiteral(ref v) => format!("{v}"),
        }
    }

    fn format_node(tree: &Expression, node: &ExpressionNodeRef) -> String {
        let current_node = &tree
            .get_node(node)
            .expect("Caller should make sure the ref is valid.");

        match current_node {
            ExpressionNode::Atom(atom) => Self::format_atom(atom),
            ExpressionNode::Prefix { operator, rhs } => {
                format!(
                    "({} {})",
                    SExpressionFormatter::format_unary_operator(operator),
                    SExpressionFormatter::format_node(tree, &rhs),
                )
            }
            ExpressionNode::Infix { operator, lhs, rhs } => {
                format!(
                    "({} {} {})",
                    SExpressionFormatter::format_binary_operator(operator),
                    SExpressionFormatter::format_node(tree, &lhs),
                    SExpressionFormatter::format_node(tree, &rhs),
                )
            }
            ExpressionNode::InfixAssignment { lhs, rhs } => {
                format!(
                    "(= {lhs} {})",
                    SExpressionFormatter::format_node(tree, &rhs)
                )
            }
            ExpressionNode::InfixShortCircuit { operator, lhs, rhs } => {
                format!(
                    "({} {} {})",
                    SExpressionFormatter::format_binary_short_circuit_operator(operator),
                    SExpressionFormatter::format_node(tree, &lhs),
                    SExpressionFormatter::format_node(tree, &rhs),
                )
            }
            ExpressionNode::Group { inner } => {
                format!(
                    "(group {})",
                    SExpressionFormatter::format_node(tree, &inner)
                )
            }
            ExpressionNode::Call { callee, arguments } => {
                let mut buffer =
                    format!("(call {}", SExpressionFormatter::format_node(tree, callee));

                for argument in arguments.iter() {
                    buffer.push_str(&format!(
                        " {}",
                        SExpressionFormatter::format_node(tree, argument)
                    ));
                }
                buffer.push(')');

                buffer
            }
        }
    }

    fn format_unary_operator(operator: &PrefixOperator) -> String {
        match operator {
            PrefixOperator::Bang => "!",
            PrefixOperator::Minus => "-",
        }
        .into()
    }
    fn format_binary_operator(operator: &InfixOperator) -> String {
        match operator {
            InfixOperator::Add => "+",
            InfixOperator::Subtract => "-",
            InfixOperator::Multiply => "*",
            InfixOperator::Divide => "/",
            InfixOperator::LessThan => "<",
            InfixOperator::LessThanEqual => "<=",
            InfixOperator::GreaterThan => ">",
            InfixOperator::GreaterThanEqual => ">=",
            InfixOperator::EqualEqual => "==",
            InfixOperator::BangEqual => "!=",
        }
        .into()
    }

    fn format_binary_short_circuit_operator(operator: &InfixShortCircuitOperator) -> String {
        match operator {
            InfixShortCircuitOperator::And => "and",
            InfixShortCircuitOperator::Or => "or",
        }
        .into()
    }
}

impl ExpressionFormatter for SExpressionFormatter {
    fn format(&self, tree: &Expression) -> String {
        SExpressionFormatter::format_node(tree, &tree.get_root_ref())
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
            ParserErrorKind::InvalidNonDeclaration(ref declaration) => {
                format!("({line}) Invalid non-declaration: {declaration:?}")
            }
            ParserErrorKind::NonBlock(ref statement) => {
                format!("({line}) Invalid block: {statement:?}")
            }
        }
    }
}
