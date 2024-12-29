use super::error::{RuntimeError, RuntimeErrorKind};
use super::LoxValue;
use crate::lexer::LineBreaks;
use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};
use std::path::Path;

const ARIADNE_MSG: &'static str = "Ariadne produces valid utf-8 strings";
const ARIADNE_WRITE_MSG: &'static str = "Write into buffer should not fail.";

pub trait ValueFormatter {
    fn format(&self, value: &LoxValue) -> String;
    fn format_error(&self, error: &RuntimeError) -> String;
}

pub struct DebugFormatter;

impl ValueFormatter for DebugFormatter {
    fn format(&self, value: &LoxValue) -> String {
        format!("{value:?}")
    }

    fn format_error(&self, error: &RuntimeError) -> String {
        format!("{error:?}")
    }
}

pub struct BasicFormatter {
    line_breaks: LineBreaks,
}

impl BasicFormatter {
    pub fn new(text: &str) -> Self {
        let line_breaks = LineBreaks::new(text);
        Self { line_breaks }
    }
}

impl BasicFormatter {
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

impl ValueFormatter for BasicFormatter {
    fn format(&self, value: &LoxValue) -> String {
        format!("{value}")
    }

    fn format_error(&self, error: &RuntimeError) -> String {
        let line = self.line_breaks.get_line_from_span(error.span);
        match &error.kind {
            RuntimeErrorKind::NonNumeric(ref v) => {
                format!("({line}) Non-Number {{Unary}}: {}", Self::format_verbose(v))
            }
            RuntimeErrorKind::NonNumerics(ref lhs, ref rhs) => format!(
                "({line}) Non-Numbers {{Binary}}: [{}, {}]",
                Self::format_verbose(lhs),
                Self::format_verbose(rhs)
            ),
            RuntimeErrorKind::NonAddable(ref lhs, ref rhs) => format!(
                "({line}) Non-Numbers/Non-Strings {{Binary}}: [{}, {}]",
                Self::format_verbose(lhs),
                Self::format_verbose(rhs)
            ),
            RuntimeErrorKind::InvalidAccess(ref name) => {
                format!("({line}) Invalid Access: {name}",)
            }
            RuntimeErrorKind::InvalidCallee(ref callee) => {
                format!("({line}) Invalid Callee: {}", Self::format_verbose(callee))
            }
            RuntimeErrorKind::InvalidArgumentCount { actual, expected } => {
                format!("({line}) Invalid Argument Count: {actual} of {expected}")
            }
            RuntimeErrorKind::InvalidInstance(ref object) => {
                format!("({line}) Invalid Instance: {object}")
            }
            RuntimeErrorKind::UndefinedProperty {
                ref object,
                ref name,
            } => {
                format!("({line}) Undefined Property Access: {name} of {object}")
            }
            RuntimeErrorKind::InvalidSuperClass(name) => {
                format!("({line}) Invalid Super Class: {name}")
            }
        }
    }
}

pub struct PrettyFormatter<'src> {
    text: &'src str,
    path: &'src Path,
}

impl<'src> PrettyFormatter<'src> {
    pub fn new(text: &'src str, path: &'src Path) -> Self {
        Self { text, path }
    }
}

impl<'src> ValueFormatter for PrettyFormatter<'src> {
    fn format(&self, value: &LoxValue) -> String {
        format!("{value}")
    }

    fn format_error(&self, error: &RuntimeError) -> String {
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
                                BasicFormatter::format_verbose(&v).fg(Color::BrightRed)
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
                                BasicFormatter::format_verbose(&lhs).fg(Color::BrightRed),
                                BasicFormatter::format_verbose(&rhs).fg(Color::BrightRed),
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
                                BasicFormatter::format_verbose(&lhs).fg(colors.next()),
                                BasicFormatter::format_verbose(&rhs).fg(colors.next()),
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
                                BasicFormatter::format_verbose(&callee).fg(Color::BrightRed),
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
        String::from_utf8(output.into_inner()).expect(ARIADNE_MSG)
    }
}
