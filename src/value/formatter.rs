use super::error::{RuntimeError, RuntimeErrorKind};
use super::LoxValue;
use crate::lexer::LineBreaks;
use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};
use std::fmt::Write;
use std::path::Path;

const WRITE_FMT_MSG: &'static str =
    "Encountered an error while attempting to write format string to buffer.";
const ARIADNE_MSG: &'static str = "Ariadne produces valid utf-8 strings";
const ARIADNE_WRITE_MSG: &'static str = "Write into buffer should not fail.";

pub trait ValueFormatter {
    fn format(&self, value: &LoxValue) -> String {
        let mut buffer = String::new();
        self.format_in_place(&mut buffer, value);
        buffer
    }
    fn format_error(&self, error: &RuntimeError) -> String {
        let mut buffer = String::new();
        self.format_error_in_place(&mut buffer, error);
        buffer
    }
    fn format_in_place(&self, buffer: &mut String, value: &LoxValue);
    fn format_error_in_place(&self, buffer: &mut String, error: &RuntimeError);
}

pub struct DebugValueFormatter;

impl ValueFormatter for DebugValueFormatter {
    fn format_in_place(&self, buffer: &mut String, value: &LoxValue) {
        write!(buffer, "{value:?}").expect(&WRITE_FMT_MSG);
    }

    fn format_error_in_place(&self, buffer: &mut String, error: &RuntimeError) {
        write!(buffer, "{error:?}").expect(&WRITE_FMT_MSG);
    }
}

pub struct BasicValueFormatter {
    line_breaks: LineBreaks,
}

impl BasicValueFormatter {
    pub fn new(text: &str) -> Self {
        let line_breaks = LineBreaks::new(text);
        Self { line_breaks }
    }
}

impl BasicValueFormatter {
    fn format_verbose(value: &LoxValue) -> String {
        match value {
            LoxValue::Number(v) => format!("Number({v})"),
            LoxValue::String(v) => format!("String(\"{v}\")"),
            LoxValue::Nil => format!("Nil"),
            LoxValue::Bool(v) => format!("Bool({v})"),
            LoxValue::NativeFunction(fun) => format!("NativeFunction({})", fun.get_name()),
            LoxValue::Function(ref fun) => {
                let name = &fun.name.name;
                format!("Function({name})")
            }
            LoxValue::Class(class) => {
                let name = &class.name.name;
                format!("Class({name})")
            }
            LoxValue::Instance(instance) => {
                let class = &instance.class.name;
                format!("Instance({class})")
            }
        }
    }
}

impl ValueFormatter for BasicValueFormatter {
    fn format_in_place(&self, buffer: &mut String, value: &LoxValue) {
        write!(buffer, "{value}").expect(&WRITE_FMT_MSG);
    }

    fn format_error_in_place(&self, buffer: &mut String, error: &RuntimeError) {
        let line = self.line_breaks.get_line_from_span(error.span);
        match &error.kind {
            RuntimeErrorKind::NonNumeric(ref v) => {
                write!(
                    buffer,
                    "({line}) Non-Number {{Unary}}: {}",
                    Self::format_verbose(v)
                )
                .expect(&WRITE_FMT_MSG);
            }
            RuntimeErrorKind::NonNumerics(ref lhs, ref rhs) => write!(
                buffer,
                "({line}) Non-Numbers {{Binary}}: [{}, {}]",
                Self::format_verbose(lhs),
                Self::format_verbose(rhs)
            )
            .expect(&WRITE_FMT_MSG),
            RuntimeErrorKind::NonAddable(ref lhs, ref rhs) => write!(
                buffer,
                "({line}) Non-Numbers/Non-Strings {{Binary}}: [{}, {}]",
                Self::format_verbose(lhs),
                Self::format_verbose(rhs)
            )
            .expect(&WRITE_FMT_MSG),
            RuntimeErrorKind::InvalidAccess(ref name) => {
                write!(buffer, "({line}) Invalid Access: {name}").expect(&WRITE_FMT_MSG);
            }
            RuntimeErrorKind::InvalidCallee(ref callee) => {
                write!(
                    buffer,
                    "({line}) Invalid Callee: {}",
                    Self::format_verbose(callee)
                )
                .expect(&WRITE_FMT_MSG);
            }
            RuntimeErrorKind::InvalidArgumentCount { actual, expected } => {
                write!(
                    buffer,
                    "({line}) Invalid Argument Count: {actual} of {expected}"
                )
                .expect(&WRITE_FMT_MSG);
            }
            RuntimeErrorKind::InvalidInstance(ref object) => {
                write!(buffer, "({line}) Invalid Instance: {object}").expect(&WRITE_FMT_MSG);
            }
            RuntimeErrorKind::UndefinedProperty {
                ref object,
                ref name,
            } => {
                write!(
                    buffer,
                    "({line}) Undefined Property Access: {name} of {object}"
                )
                .expect(&WRITE_FMT_MSG);
            }
            RuntimeErrorKind::InvalidSuperClass(name) => {
                write!(buffer, "({line}) Invalid Super Class: {name}").expect(&WRITE_FMT_MSG);
            }
        }
    }
}

pub struct PrettyValueFormatter<'src> {
    text: &'src str,
    path: &'src Path,
}

impl<'src> PrettyValueFormatter<'src> {
    pub fn new(text: &'src str, path: &'src Path) -> Self {
        Self { text, path }
    }
}

impl<'src> ValueFormatter for PrettyValueFormatter<'src> {
    fn format_in_place(&self, buffer: &mut String, value: &LoxValue) {
        write!(buffer, "{value}").expect(&WRITE_FMT_MSG);
    }

    fn format_error_in_place(&self, buffer: &mut String, error: &RuntimeError) {
        let path = self
            .path
            .to_str()
            .expect("Non-UTF8 paths are not supported!");
        let mut output = std::io::Cursor::new(Vec::new());
        let span = error.span;
        match &error.clone().kind {
            RuntimeErrorKind::NonNumeric(v) => {
                Report::build(ReportKind::Error, (path, span.range()))
                    .with_code(error.code())
                    .with_message("Expected a single numeric operand")
                    .with_label(
                        Label::new((path, span.range()))
                            .with_message(format!(
                                "Type is {} instead of numeric",
                                BasicValueFormatter::format_verbose(&v).fg(Color::BrightRed)
                            ))
                            .with_color(Color::BrightRed),
                    )
                    .finish()
                    .write((path, Source::from(self.text)), &mut output)
                    .expect(ARIADNE_WRITE_MSG);
            }
            RuntimeErrorKind::NonNumerics(lhs, rhs) => {
                Report::build(ReportKind::Error, (path, span.range()))
                    .with_code(error.code())
                    .with_message("Expected both operands to be numeric")
                    .with_label(
                        Label::new((path, span.range()))
                            .with_message(format!(
                                "One or both of {} and {} is not numeric",
                                BasicValueFormatter::format_verbose(&lhs).fg(Color::BrightRed),
                                BasicValueFormatter::format_verbose(&rhs).fg(Color::BrightRed),
                            ))
                            .with_color(Color::BrightRed),
                    )
                    .finish()
                    .write((path, Source::from(self.text)), &mut output)
                    .expect(ARIADNE_WRITE_MSG);
            }
            RuntimeErrorKind::NonAddable(lhs, rhs) => {
                let mut colors = ColorGenerator::new();
                Report::build(ReportKind::Error, (path, span.range()))
                    .with_code(error.code())
                    .with_message("Expected both operands to either be numeric or strings")
                    .with_label(
                        Label::new((path, span.range()))
                            .with_message(format!(
                                "{} and {} are not the same type and/or are not numeric/string.",
                                BasicValueFormatter::format_verbose(&lhs).fg(colors.next()),
                                BasicValueFormatter::format_verbose(&rhs).fg(colors.next()),
                            ))
                            .with_color(Color::BrightRed),
                    )
                    .finish()
                    .write((path, Source::from(self.text)), &mut output)
                    .expect(ARIADNE_WRITE_MSG);
            }
            RuntimeErrorKind::InvalidAccess(name) => {
                Report::build(ReportKind::Error, (path, span.range()))
                    .with_code(error.code())
                    .with_message("Attempted to access a value that has not been yet defined")
                    .with_label(
                        Label::new((path, span.range()))
                            .with_message(format!(
                                "{} has not been yet defined.",
                                name.fg(Color::BrightRed),
                            ))
                            .with_color(Color::BrightRed),
                    )
                    .finish()
                    .write((path, Source::from(self.text)), &mut output)
                    .expect(ARIADNE_WRITE_MSG);
            }
            RuntimeErrorKind::InvalidCallee(callee) => {
                Report::build(ReportKind::Error, (path, span.range()))
                    .with_code(error.code())
                    .with_message("Attempted to call a value that is not callable")
                    .with_label(
                        Label::new((path, span.range()))
                            .with_message(format!(
                                "{} is not callable.",
                                BasicValueFormatter::format_verbose(&callee).fg(Color::BrightRed),
                            ))
                            .with_color(Color::BrightRed),
                    )
                    .finish()
                    .write((path, Source::from(self.text)), &mut output)
                    .expect(ARIADNE_WRITE_MSG);
            }
            RuntimeErrorKind::InvalidArgumentCount { actual, expected } => {
                Report::build(ReportKind::Error, (path, span.range()))
                    .with_code(error.code())
                    .with_message("Attempted to call a callable with the number of arguments")
                    .with_label(
                        Label::new((path, span.range()))
                            .with_message(format!(
                                "Callable has {} parameters but got {} arguments instead.",
                                expected.fg(Color::BrightCyan),
                                actual.fg(Color::BrightRed),
                            ))
                            .with_color(Color::BrightRed),
                    )
                    .finish()
                    .write((path, Source::from(self.text)), &mut output)
                    .expect(ARIADNE_WRITE_MSG);
            }
            RuntimeErrorKind::InvalidInstance(_) => {
                Report::build(ReportKind::Error, (path, span.range()))
                    .with_code(error.code())
                    .with_message("Attempted to access a property on a non-instance value")
                    .with_label(
                        Label::new((path, span.range()))
                            .with_message("This is not a callable...")
                            .with_color(Color::BrightRed),
                    )
                    .finish()
                    .write((path, Source::from(self.text)), &mut output)
                    .expect(ARIADNE_WRITE_MSG);
            }
            RuntimeErrorKind::UndefinedProperty { name, .. } => {
                Report::build(ReportKind::Error, (path, span.range()))
                    .with_code(error.code())
                    .with_message("Attempted to access an undefined property of an instance")
                    .with_label(
                        Label::new((path, span.range()))
                            .with_message(format!(
                                "The `{}` property is not defined on the instance",
                                name.fg(Color::BrightYellow)
                            ))
                            .with_color(Color::BrightRed),
                    )
                    .finish()
                    .write((path, Source::from(self.text)), &mut output)
                    .expect(ARIADNE_WRITE_MSG);
            }
            RuntimeErrorKind::InvalidSuperClass(name) => {
                Report::build(ReportKind::Error, (path, span.range()))
                    .with_code(error.code())
                    .with_message("Attempted to subclass from a non-class value")
                    .with_label(
                        Label::new((path, span.range()))
                            .with_message(format!(
                                "The `{}` variable is not a class",
                                name.fg(Color::BrightYellow)
                            ))
                            .with_color(Color::BrightRed),
                    )
                    .finish()
                    .write((path, Source::from(self.text)), &mut output)
                    .expect(ARIADNE_WRITE_MSG);
            }
        }
        buffer.push_str(&String::from_utf8(output.into_inner()).expect(ARIADNE_MSG));
    }
}

pub struct NystromValueFormatter {
    line_breaks: LineBreaks,
}

impl NystromValueFormatter {
    pub fn new(text: &str) -> Self {
        let line_breaks = LineBreaks::new(text);
        Self { line_breaks }
    }
}

impl ValueFormatter for NystromValueFormatter {
    fn format_in_place(&self, buffer: &mut String, value: &LoxValue) {
        write!(buffer, "{value:?}").expect(&WRITE_FMT_MSG);
    }

    fn format_error_in_place(&self, buffer: &mut String, error: &RuntimeError) {
        let line = self.line_breaks.get_line_from_span(error.span);
        match &error.kind {
            RuntimeErrorKind::NonNumeric(_) => {
                write!(buffer, "({line}) [Runtime] Operand must be a number.")
                    .expect(&WRITE_FMT_MSG)
            }
            RuntimeErrorKind::NonNumerics(_, _) => {
                write!(buffer, "({line}) [Runtime] Operands must be numbers.")
                    .expect(&WRITE_FMT_MSG)
            }
            RuntimeErrorKind::NonAddable(_, _) => write!(
                buffer,
                "({line}) [Runtime] Operands must be two numbers or two strings."
            )
            .expect(&WRITE_FMT_MSG),
            RuntimeErrorKind::InvalidAccess(name) => {
                write!(buffer, "({line}) [Runtime] Undefined variable '{name}'.")
                    .expect(&WRITE_FMT_MSG)
            }
            RuntimeErrorKind::InvalidCallee(_) => write!(buffer, "").expect(&WRITE_FMT_MSG),
            RuntimeErrorKind::InvalidInstance(_) => write!(buffer, "").expect(&WRITE_FMT_MSG),
            RuntimeErrorKind::UndefinedProperty { .. } => write!(buffer, "").expect(&WRITE_FMT_MSG),
            RuntimeErrorKind::InvalidArgumentCount { .. } => {
                write!(buffer, "").expect(&WRITE_FMT_MSG)
            }
            RuntimeErrorKind::InvalidSuperClass(_) => write!(buffer, "").expect(&WRITE_FMT_MSG),
        }
    }
}
