use super::{
    expression::{
        Expression, ExpressionAtom, ExpressionAtomKind, ExpressionNode, ExpressionNodeRef,
        InfixOperator, InfixShortCircuitOperator, PrefixOperator,
    },
    Parser,
};
use crate::{
    lexer::{
        formatter::{
            LineFormatter as LineTokenFormatter, ToFormatter as ToTokenFormatter, TokenFormatter,
        },
        LineBreaks, Token,
    },
    parser::ParserError,
};

pub trait ExpressionFormatter {
    fn format(&self, tree: &Expression) -> String;
    fn format_error(&self, error: &ParserError) -> String;
}

pub trait ToFormatter<F>
where
    F: ExpressionFormatter,
{
    fn create_formatter(&self) -> F;
}

pub struct DebugFormatter;

impl<'src> ToFormatter<DebugFormatter> for Parser<'src> {
    fn create_formatter(&self) -> DebugFormatter {
        DebugFormatter {}
    }
}

impl ExpressionFormatter for DebugFormatter {
    fn format(&self, tree: &Expression) -> String {
        format!("{tree:?}")
    }

    fn format_error(&self, error: &ParserError) -> String {
        format!("{error:?}")
    }
}

pub struct SExpressionFormatter<'src> {
    text: &'src str,
    line_breaks: LineBreaks,
    lexer_formatter: LineTokenFormatter<'src>,
}

impl<'src> ToFormatter<SExpressionFormatter<'src>> for Parser<'src> {
    fn create_formatter(&self) -> SExpressionFormatter<'src> {
        SExpressionFormatter {
            text: &self.lexer.get_source(),
            line_breaks: self.lexer.get_line_breaks(),
            lexer_formatter: self.lexer.create_formatter(),
        }
    }
}

impl<'src> SExpressionFormatter<'src> {
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
            .get_node(*node)
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

impl<'src> ExpressionFormatter for SExpressionFormatter<'src> {
    fn format(&self, tree: &Expression) -> String {
        SExpressionFormatter::format_node(tree, &tree.get_root_ref())
    }

    fn format_error(&self, error: &ParserError) -> String {
        match error {
            ParserError::UnexpectedToken {
                actual: Token { kind, span },
                expected,
            } => {
                let line = self.line_breaks.get_line_from_span(*span);
                format!("({line}) Unexpected: A = {kind} E = {expected}")
            }
            ParserError::NonOperator(Token { kind, span }) => {
                let line = self.line_breaks.get_line_from_span(*span);
                format!("({line}) Non-operator: {kind}")
            }
            ParserError::NonExpression(Token { kind, span }) => {
                let line = self.line_breaks.get_line_from_span(*span);
                format!("({line}) Non-Expression: {kind}")
            }
            ParserError::LexicalError(ref error) => {
                self.lexer_formatter.format_lexical_error(error)
            }
            ParserError::UnexpectedEof(span) => {
                let line = self.line_breaks.get_line_from_span(*span);
                format!("({line}) Unexpected EOF")
            }
            ParserError::InvalidStatement(Token { kind, span }) => {
                let line = self.line_breaks.get_line_from_span(*span);
                format!("({line}) Invalid statement token: {kind}")
            }
            ParserError::InvalidLValue(Token { kind, span }) => {
                let line = self.line_breaks.get_line_from_span(*span);
                format!("({line}) Invalid l-value: {kind}")
            }
            ParserError::InvalidNonDeclaration(ref declaration) => {
                let line = std::u32::MAX;
                format!("({line}) Invalid non-declaration: {declaration:?}")
            }
            ParserError::NonBlock(ref statement) => {
                let line = std::u32::MAX;
                format!("({line}) Invalid block: {statement:?}")
            }
        }
    }
}
