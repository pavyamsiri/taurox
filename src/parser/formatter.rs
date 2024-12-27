use super::{
    error::{ExpressionParserError, GeneralExpressionParserError, GeneralParserError},
    expression::{
        Expression, ExpressionAtom, ExpressionAtomKind, ExpressionNode, ExpressionNodeRef,
        InfixOperator, InfixShortCircuitOperator, PrefixOperator,
    },
};
use crate::lexer::{
    formatter::{
        LineFormatter as LineTokenFormatter, PrettyFormatter as PrettyTokenFormatter,
        TokenFormatter,
    },
    Token,
};
use ariadne::{Color, Label, Report, ReportKind, Source};
use std::path::Path;

const ARIADNE_MSG: &'static str = "Ariadne produces valid utf-8 strings";
const ARIADNE_WRITE_MSG: &'static str = "Write into buffer should not fail.";

pub trait ExpressionFormatter {
    fn format(&self, tree: &Expression) -> String;
    fn format_error(&self, error: &GeneralExpressionParserError) -> String;
}

pub struct DebugFormatter;

impl ExpressionFormatter for DebugFormatter {
    fn format(&self, tree: &Expression) -> String {
        format!("{tree:?}")
    }

    fn format_error(&self, error: &GeneralExpressionParserError) -> String {
        format!("{error:?}")
    }
}

pub struct SExpressionFormatter<'src> {
    token_formatter: LineTokenFormatter<'src>,
}

impl<'src> SExpressionFormatter<'src> {
    pub fn new(text: &'src str) -> Self {
        Self {
            token_formatter: LineTokenFormatter::new(text),
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
                    "(= {} {})",
                    lhs.get_name(),
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

    fn format_error(&self, error: &GeneralExpressionParserError) -> String {
        match error {
            GeneralExpressionParserError::Inner(expr) => match expr {
                ExpressionParserError::NonExpression(Token { kind, span }) => {
                    let line = self
                        .token_formatter
                        .get_line_breaks()
                        .get_line_from_span(*span);
                    format!("({line}) Non-Expression: {kind}")
                }
                ExpressionParserError::InvalidLValue(Token { kind, span }) => {
                    let line = self
                        .token_formatter
                        .get_line_breaks()
                        .get_line_from_span(*span);
                    format!("({line}) Invalid l-value: {kind}")
                }
            },
            GeneralExpressionParserError::General(gen) => match gen {
                GeneralParserError::UnexpectedToken {
                    actual: Token { kind, span },
                    expected,
                } => {
                    let line = self
                        .token_formatter
                        .get_line_breaks()
                        .get_line_from_span(*span);
                    format!("({line}) Unexpected: A = {kind} E = {expected}")
                }
                GeneralParserError::UnexpectedEof(span) => {
                    let line = self
                        .token_formatter
                        .get_line_breaks()
                        .get_line_from_span(*span);
                    format!("({line}) Unexpected EOF")
                }
                GeneralParserError::LexicalError(err) => {
                    self.token_formatter.format_lexical_error(err)
                }
            },
        }
    }
}

pub struct PrettyFormatter<'src> {
    token_formatter: PrettyTokenFormatter<'src>,
}

impl<'src> PrettyFormatter<'src> {
    pub fn new(text: &'src str, path: &'src Path) -> Self {
        Self {
            token_formatter: PrettyTokenFormatter::new(text, path),
        }
    }
}

impl<'src> ExpressionFormatter for PrettyFormatter<'src> {
    fn format(&self, tree: &Expression) -> String {
        SExpressionFormatter::format_node(tree, &tree.get_root_ref())
    }

    fn format_error(&self, error: &GeneralExpressionParserError) -> String {
        let text = self.token_formatter.get_text();
        let path = &self.token_formatter.get_path().to_string_lossy();
        let mut output = std::io::Cursor::new(Vec::new());
        match error {
            GeneralExpressionParserError::Inner(expr) => match expr {
                ExpressionParserError::NonExpression(Token { kind, span }) => {
                    Report::build(ReportKind::Error, (path, span.range()))
                        .with_code(error.code())
                        .with_message("Expected a valid expression operator or atom token")
                        .with_label(
                            Label::new((path, span.range()))
                                .with_message(format!("Not an expression token {}", kind))
                                .with_color(Color::BrightRed),
                        )
                        .finish()
                        .write((path, Source::from(text)), &mut output)
                        .expect(ARIADNE_WRITE_MSG);
                    String::from_utf8(output.into_inner()).expect(ARIADNE_MSG)
                }
                ExpressionParserError::InvalidLValue(Token { kind, span }) => {
                    Report::build(ReportKind::Error, (path, span.range()))
                        .with_code(error.code())
                        .with_message("Expected an l-value")
                        .with_label(
                            Label::new((path, span.range()))
                                .with_message(format!("Not an l-value {}", kind))
                                .with_color(Color::BrightRed),
                        )
                        .finish()
                        .write((path, Source::from(text)), &mut output)
                        .expect(ARIADNE_WRITE_MSG);
                    String::from_utf8(output.into_inner()).expect(ARIADNE_MSG)
                }
            },
            GeneralExpressionParserError::General(gen) => match gen {
                GeneralParserError::UnexpectedToken {
                    actual: Token { kind, span },
                    expected,
                } => {
                    Report::build(ReportKind::Error, (path, span.range()))
                        .with_code(error.code())
                        .with_message("Expected a different token")
                        .with_label(
                            Label::new((path, span.range()))
                                .with_message(format!("Expected {} but got {}", expected, kind))
                                .with_color(Color::BrightRed),
                        )
                        .finish()
                        .write((path, Source::from(text)), &mut output)
                        .expect(ARIADNE_WRITE_MSG);
                    String::from_utf8(output.into_inner()).expect(ARIADNE_MSG)
                }
                GeneralParserError::UnexpectedEof(span) => {
                    Report::build(ReportKind::Error, (path, span.range()))
                        .with_code(error.code())
                        .with_message("Unexpected EOF")
                        .with_label(
                            Label::new((path, span.range()))
                                .with_message("File ends here...")
                                .with_color(Color::BrightRed),
                        )
                        .finish()
                        .write((path, Source::from(text)), &mut output)
                        .expect(ARIADNE_WRITE_MSG);
                    String::from_utf8(output.into_inner()).expect(ARIADNE_MSG)
                }
                GeneralParserError::LexicalError(err) => {
                    self.token_formatter.format_lexical_error(err)
                }
            },
        }
    }
}
