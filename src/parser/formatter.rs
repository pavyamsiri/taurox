use super::{
    error::{
        ExpressionParserError, GeneralExpressionParserError, GeneralParserError,
        StatementParserError,
    },
    expression::{
        Expression, ExpressionAtom, ExpressionAtomKind, ExpressionNode, ExpressionNodeRef,
        InfixOperator, InfixShortCircuitOperator, PrefixOperator,
    },
    ParserError,
};
use crate::lexer::{
    formatter::{
        LineFormatter as LineTokenFormatter, PrettyFormatter as PrettyTokenFormatter,
        TokenFormatter,
    },
    LineBreaks, Token,
};
use ariadne::{Color, Label, Report, ReportKind, Source};
use std::{fmt::Write, path::Path};

const WRITE_FMT_MSG: &'static str =
    "Encountered an error while attempting to write format string to buffer.";
const ARIADNE_MSG: &'static str = "Ariadne produces valid utf-8 strings";
const ARIADNE_WRITE_MSG: &'static str = "Write into buffer should not fail.";

pub trait ExpressionFormatter {
    fn format(&self, tree: &Expression) -> String {
        let mut buffer = String::new();
        self.format_in_place(&mut buffer, tree);
        buffer
    }
    fn format_error(&self, error: &GeneralExpressionParserError) -> String {
        let mut buffer = String::new();
        self.format_error_in_place(&mut buffer, error);
        buffer
    }
    fn format_in_place(&self, buffer: &mut String, tree: &Expression);
    fn format_error_in_place(&self, buffer: &mut String, error: &GeneralExpressionParserError);
}

pub struct DebugExpressionFormatter;

impl ExpressionFormatter for DebugExpressionFormatter {
    fn format_in_place(&self, buffer: &mut String, tree: &Expression) {
        write!(buffer, "{tree:?}").expect(&WRITE_FMT_MSG);
    }

    fn format_error_in_place(&self, buffer: &mut String, error: &GeneralExpressionParserError) {
        write!(buffer, "{error:?}").expect(&WRITE_FMT_MSG);
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

    pub fn get_line_breaks(&self) -> &LineBreaks {
        self.token_formatter.get_line_breaks()
    }
}

impl<'src> SExpressionFormatter<'src> {
    fn format_atom(buffer: &mut String, atom: &ExpressionAtom) {
        match atom.kind {
            ExpressionAtomKind::Number(v) => write!(buffer, "{v:?}").expect(&WRITE_FMT_MSG),
            ExpressionAtomKind::Bool(v) => write!(buffer, "{v}").expect(&WRITE_FMT_MSG),
            ExpressionAtomKind::Nil => buffer.push_str("nil"),
            ExpressionAtomKind::Identifier(ref name) => {
                write!(buffer, "{name}").expect(&WRITE_FMT_MSG)
            }
            ExpressionAtomKind::StringLiteral(ref v) => {
                write!(buffer, "{v}").expect(&WRITE_FMT_MSG)
            }
        }
    }

    fn format_node(buffer: &mut String, tree: &Expression, node: &ExpressionNodeRef) {
        let current_node = &tree
            .get_node(*node)
            .expect("Caller should make sure the ref is valid.");

        match current_node {
            ExpressionNode::Atom(atom) => Self::format_atom(buffer, atom),
            ExpressionNode::Prefix { operator, rhs } => {
                buffer.push('(');
                SExpressionFormatter::format_unary_operator(buffer, operator);
                buffer.push(' ');
                SExpressionFormatter::format_node(buffer, tree, &rhs);
                buffer.push(')');
            }
            ExpressionNode::Infix {
                operator,
                ref lhs,
                ref rhs,
            } => {
                buffer.push('(');
                SExpressionFormatter::format_binary_operator(buffer, operator);
                buffer.push(' ');
                SExpressionFormatter::format_node(buffer, tree, lhs);
                buffer.push(' ');
                SExpressionFormatter::format_node(buffer, tree, rhs);
                buffer.push(')');
            }
            ExpressionNode::InfixAssignment { lhs, ref rhs } => {
                write!(buffer, "(= {} ", lhs.get_name()).expect(&WRITE_FMT_MSG);
                SExpressionFormatter::format_node(buffer, tree, rhs);
                buffer.push(')');
            }
            ExpressionNode::InfixShortCircuit {
                operator,
                ref lhs,
                ref rhs,
            } => {
                buffer.push('(');
                SExpressionFormatter::format_binary_short_circuit_operator(buffer, operator);
                buffer.push(' ');
                SExpressionFormatter::format_node(buffer, tree, lhs);
                buffer.push(' ');
                SExpressionFormatter::format_node(buffer, tree, rhs);
                buffer.push(')');
            }
            ExpressionNode::Group { ref inner } => {
                buffer.push_str("(group ");
                SExpressionFormatter::format_node(buffer, tree, inner);
                buffer.push(')');
            }
            ExpressionNode::Call {
                ref callee,
                arguments,
            } => {
                buffer.push_str("(call ");
                SExpressionFormatter::format_node(buffer, tree, callee);

                for argument in arguments.iter() {
                    buffer.push(' ');
                    SExpressionFormatter::format_node(buffer, tree, argument);
                }
                buffer.push(')');
            }
        }
    }

    fn format_unary_operator(buffer: &mut String, operator: &PrefixOperator) {
        match operator {
            PrefixOperator::Bang => buffer.push('!'),
            PrefixOperator::Minus => buffer.push('-'),
        }
    }
    fn format_binary_operator(buffer: &mut String, operator: &InfixOperator) {
        match operator {
            InfixOperator::Add => buffer.push('+'),
            InfixOperator::Subtract => buffer.push('-'),
            InfixOperator::Multiply => buffer.push('*'),
            InfixOperator::Divide => buffer.push('/'),
            InfixOperator::LessThan => buffer.push('<'),
            InfixOperator::LessThanEqual => buffer.push_str("<="),
            InfixOperator::GreaterThan => buffer.push('>'),
            InfixOperator::GreaterThanEqual => buffer.push_str(">="),
            InfixOperator::EqualEqual => buffer.push_str("=="),
            InfixOperator::BangEqual => buffer.push_str("!="),
        }
    }

    fn format_binary_short_circuit_operator(
        buffer: &mut String,
        operator: &InfixShortCircuitOperator,
    ) {
        match operator {
            InfixShortCircuitOperator::And => buffer.push_str("and"),
            InfixShortCircuitOperator::Or => buffer.push_str("or"),
        }
    }
}

impl<'src> ExpressionFormatter for SExpressionFormatter<'src> {
    fn format_in_place(&self, buffer: &mut String, tree: &Expression) {
        SExpressionFormatter::format_node(buffer, tree, &tree.get_root_ref())
    }

    fn format_error_in_place(&self, buffer: &mut String, error: &GeneralExpressionParserError) {
        match error {
            GeneralExpressionParserError::Inner(expr) => match expr {
                ExpressionParserError::NonExpression(Token { kind, span }) => {
                    let line = self
                        .token_formatter
                        .get_line_breaks()
                        .get_line_from_span(*span);
                    write!(buffer, "({line}) Non-Expression: {kind}").expect(&WRITE_FMT_MSG);
                }
                ExpressionParserError::InvalidLValue(Token { kind, span }) => {
                    let line = self
                        .token_formatter
                        .get_line_breaks()
                        .get_line_from_span(*span);
                    write!(buffer, "({line}) Invalid l-value: {kind}").expect(&WRITE_FMT_MSG);
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
                    write!(buffer, "({line}) Unexpected: A = {kind} E = {expected}")
                        .expect(&WRITE_FMT_MSG);
                }
                GeneralParserError::UnexpectedEof(span) => {
                    let line = self
                        .token_formatter
                        .get_line_breaks()
                        .get_line_from_span(*span);
                    write!(buffer, "({line}) Unexpected EOF").expect(&WRITE_FMT_MSG);
                }
                GeneralParserError::LexicalError(err) => {
                    self.token_formatter.format_error_in_place(buffer, err)
                }
            },
        }
    }
}

pub struct PrettyExpressionFormatter<'src> {
    token_formatter: PrettyTokenFormatter<'src>,
}

impl<'src> PrettyExpressionFormatter<'src> {
    pub fn new(text: &'src str, path: &'src Path) -> Self {
        Self {
            token_formatter: PrettyTokenFormatter::new(text, path),
        }
    }
}

impl<'src> ExpressionFormatter for PrettyExpressionFormatter<'src> {
    fn format_in_place(&self, buffer: &mut String, tree: &Expression) {
        SExpressionFormatter::format_node(buffer, tree, &tree.get_root_ref())
    }

    fn format_error_in_place(&self, buffer: &mut String, error: &GeneralExpressionParserError) {
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
                }
                GeneralParserError::LexicalError(err) => {
                    self.token_formatter.format_error_in_place(buffer, err)
                }
            },
        }
        buffer.push_str(&String::from_utf8(output.into_inner()).expect(ARIADNE_MSG));
    }
}

pub trait ParserFormatter {
    fn format_error(&self, error: &ParserError) -> String;
}

pub struct DebugParserFormatter;

impl ParserFormatter for DebugParserFormatter {
    fn format_error(&self, error: &ParserError) -> String {
        format!("{error}")
    }
}

pub struct BasicParserFormatter<'src> {
    expr_formatter: SExpressionFormatter<'src>,
}

impl<'src> BasicParserFormatter<'src> {
    pub fn new(text: &'src str) -> Self {
        Self {
            expr_formatter: SExpressionFormatter::new(text),
        }
    }

    fn format_expression_parser_error(&self, buffer: &mut String, error: &ExpressionParserError) {
        let error: GeneralExpressionParserError = (error.clone()).into();
        self.expr_formatter.format_error_in_place(buffer, &error);
    }

    fn format_general_parser_error(&self, buffer: &mut String, error: &GeneralParserError) {
        let error: GeneralExpressionParserError = (error.clone()).into();
        self.expr_formatter.format_error_in_place(buffer, &error);
    }

    fn format_statement_parser_error(&self, buffer: &mut String, error: &StatementParserError) {
        match error {
            StatementParserError::NonBlock(statement) => {
                let line = 0;
                buffer
                    .write_fmt(format_args!("({line}) Non-Block: {statement:?}"))
                    .expect(&WRITE_FMT_MSG);
            }
            StatementParserError::InvalidStatement(Token { kind, span }) => {
                let line = self
                    .expr_formatter
                    .get_line_breaks()
                    .get_line_from_span(*span);
                buffer
                    .write_fmt(format_args!("({line}) Invalid statement token: {kind:?}"))
                    .expect(&WRITE_FMT_MSG);
            }
            StatementParserError::InvalidNonDeclaration(decl) => {
                let line = 0;
                buffer
                    .write_fmt(format_args!("({line}) Invalid non declaration: {decl:?}"))
                    .expect(&WRITE_FMT_MSG);
            }
        }
    }
}

impl<'src> ParserFormatter for BasicParserFormatter<'src> {
    fn format_error(&self, error: &ParserError) -> String {
        let mut buffer = String::new();
        match error {
            ParserError::Expression(e) => self.format_expression_parser_error(&mut buffer, e),
            ParserError::General(e) => self.format_general_parser_error(&mut buffer, e),
            ParserError::Statement(e) => self.format_statement_parser_error(&mut buffer, e),
        }
        buffer
    }
}
