use super::{
    error::{RuntimeError, RuntimeErrorKind},
    value::LoxValue,
};
use crate::{lexer::LineBreaks, parser::Parser};
use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};
use std::path::Path;

const ARIADNE_MSG: &'static str = "Ariadne produces valid utf-8 strings";
const ARIADNE_WRITE_MSG: &'static str = "Write into buffer should not fail.";

pub trait ValueFormatter {
    fn format(&self, value: &LoxValue) -> String;
    fn format_error(&self, error: &RuntimeError) -> String;
}

pub trait ToFormatter<F>
where
    F: ValueFormatter,
{
    fn create_formatter(&self) -> F;
}

pub struct DebugFormatter;

impl<'src> ToFormatter<DebugFormatter> for Parser<'src> {
    fn create_formatter(&self) -> DebugFormatter {
        DebugFormatter {}
    }
}

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

impl<'src> ToFormatter<BasicFormatter> for Parser<'src> {
    fn create_formatter(&self) -> BasicFormatter {
        BasicFormatter {
            line_breaks: self.get_line_breaks(),
        }
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
            LoxValue::Function { name, .. } => format!("Function({name})"),
        }
    }
}

impl ValueFormatter for BasicFormatter {
    fn format(&self, value: &LoxValue) -> String {
        format!("{value}")
    }

    fn format_error(&self, error: &RuntimeError) -> String {
        let line = self.line_breaks.get_line_from_span(error.span);
        match error.kind {
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
        }
    }
}

pub struct PrettyFormatter<'src> {
    text: &'src str,
    path: &'src Path,
}

impl<'src> ToFormatter<PrettyFormatter<'src>> for Parser<'src> {
    fn create_formatter(&self) -> PrettyFormatter<'src> {
        PrettyFormatter {
            text: &self.get_source(),
            path: &self.get_path(),
        }
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
        match &error.kind {
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
                String::from_utf8(output.into_inner()).expect(ARIADNE_MSG)
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
                String::from_utf8(output.into_inner()).expect(ARIADNE_MSG)
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
                String::from_utf8(output.into_inner()).expect(ARIADNE_MSG)
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
                String::from_utf8(output.into_inner()).expect(ARIADNE_MSG)
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
                String::from_utf8(output.into_inner()).expect(ARIADNE_MSG)
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
                String::from_utf8(output.into_inner()).expect(ARIADNE_MSG)
            }
        }
    }
}
