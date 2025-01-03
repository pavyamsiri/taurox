use super::{ResolutionError, ResolutionErrorKind};
use crate::lexer::LineBreaks;
use ariadne::{Color, Label, Report, ReportKind, Source};
use std::{fmt::Write, path::Path};

const WRITE_FMT_MSG: &'static str =
    "Encountered an error while attempting to write format string to buffer.";
const ARIADNE_MSG: &'static str = "Ariadne produces valid utf-8 strings";
const ARIADNE_WRITE_MSG: &'static str = "Write into buffer should not fail.";

pub trait ResolverFormatter {
    fn format_error(&self, error: &ResolutionError) -> String {
        let mut buffer = String::new();
        self.format_error_in_place(&mut buffer, error);
        buffer
    }
    fn format_error_in_place(&self, buffer: &mut String, error: &ResolutionError);
}

pub struct DebugResolverFormatter;

impl ResolverFormatter for DebugResolverFormatter {
    fn format_error_in_place(&self, buffer: &mut String, error: &ResolutionError) {
        write!(buffer, "{error:?}").expect(&WRITE_FMT_MSG);
    }
}

pub struct BasicResolverFormatter {
    line_breaks: LineBreaks,
}

impl BasicResolverFormatter {
    pub fn new(text: &str) -> Self {
        Self {
            line_breaks: LineBreaks::new(text),
        }
    }
}

impl ResolverFormatter for BasicResolverFormatter {
    fn format_error_in_place(&self, buffer: &mut String, error: &ResolutionError) {
        let line = self.line_breaks.get_line_from_span(error.span);
        write!(buffer, "({line}) ").expect(&WRITE_FMT_MSG);
        match &error.kind {
            ResolutionErrorKind::SelfReferentialInitializer { destination, .. } => write!(
                buffer,
                "Referencing {} in its own initializer",
                destination.name
            )
            .expect(&WRITE_FMT_MSG),
            ResolutionErrorKind::ShadowLocal { old, .. } => {
                write!(buffer, "Shadowing the local {}", old.name).expect(&WRITE_FMT_MSG)
            }
            ResolutionErrorKind::NonFunctionReturn => {
                buffer.push_str("Returning in a non-function context")
            }
            ResolutionErrorKind::NonClassThis => {
                buffer.push_str("Accessing `this` in a non-class context")
            }
            ResolutionErrorKind::ReturnInConstructor => {
                buffer.push_str("Constructors can not have explicit returns")
            }
            ResolutionErrorKind::SelfReferentialInheritance { destination, .. } => write!(
                buffer,
                "The class {} can not inherit from itself.",
                destination.name
            )
            .expect(&WRITE_FMT_MSG),
            ResolutionErrorKind::NonClassSuper => {
                buffer.push_str("Accessing `super` in a non-class context")
            }
            ResolutionErrorKind::NonSubClassSuper => {
                buffer.push_str("Accessing `super` in a non-subclass context")
            }
        }
    }
}

pub struct PrettyResolverFormatter<'src> {
    text: &'src str,
    path: &'src Path,
}

impl<'src> PrettyResolverFormatter<'src> {
    pub fn new(text: &'src str, path: &'src Path) -> Self {
        Self { text, path }
    }
}

impl<'src> ResolverFormatter for PrettyResolverFormatter<'src> {
    fn format_error_in_place(&self, buffer: &mut String, error: &ResolutionError) {
        let text = self.text;
        let path = &self.path.to_string_lossy();
        let mut output = std::io::Cursor::new(Vec::new());
        let span = error.span;
        match &error.kind {
            ResolutionErrorKind::SelfReferentialInitializer {
                destination,
                reference,
            } => {
                let dest_span = destination.span;
                let reference_span = reference.span;
                Report::build(ReportKind::Error, (path, span.range()))
                    .with_code(error.code())
                    .with_message("Attempted to reference a variable in its own initializer")
                    .with_label(
                        Label::new((path, dest_span.range()))
                            .with_message("The variable declared here...")
                            .with_color(Color::BrightRed),
                    )
                    .with_label(
                        Label::new((path, reference_span.range()))
                            .with_message("is used its own initializer, creating a self-reference")
                            .with_color(Color::BrightRed),
                    )
                    .with_label(Label::new((path, span.range())).with_color(Color::BrightYellow))
                    .finish()
                    .write((path, Source::from(text)), &mut output)
                    .expect(ARIADNE_WRITE_MSG);
            }
            ResolutionErrorKind::SelfReferentialInheritance {
                destination,
                reference,
            } => {
                let dest_span = destination.span;
                let reference_span = reference.span;
                Report::build(ReportKind::Error, (path, span.range()))
                    .with_code(error.code())
                    .with_message("Attempted to inherit a class from itself")
                    .with_label(
                        Label::new((path, dest_span.range()))
                            .with_message("The class here...")
                            .with_color(Color::BrightRed),
                    )
                    .with_label(
                        Label::new((path, reference_span.range()))
                            .with_message("can not inherit from itself")
                            .with_color(Color::BrightRed),
                    )
                    .with_label(Label::new((path, span.range())).with_color(Color::BrightYellow))
                    .finish()
                    .write((path, Source::from(text)), &mut output)
                    .expect(ARIADNE_WRITE_MSG);
            }
            ResolutionErrorKind::ShadowLocal { old, new } => {
                let old_span = old.span;
                let new_span = new.span;
                Report::build(ReportKind::Error, (path, span.range()))
                    .with_code(error.code())
                    .with_message("Shadowed a local variable in a scope")
                    .with_label(
                        Label::new((path, old_span.range()))
                            .with_message("Variable is first declared here...")
                            .with_color(Color::BrightRed),
                    )
                    .with_label(
                        Label::new((path, new_span.range()))
                            .with_message("but is declared again over here")
                            .with_color(Color::BrightRed),
                    )
                    .with_label(Label::new((path, span.range())).with_color(Color::BrightYellow))
                    .finish()
                    .write((path, Source::from(text)), &mut output)
                    .expect(ARIADNE_WRITE_MSG);
            }
            ResolutionErrorKind::NonFunctionReturn => {
                Report::build(ReportKind::Error, (path, span.range()))
                    .with_code(error.code())
                    .with_message("Returning from a non-function context")
                    .with_label(
                        Label::new((path, span.range()))
                            .with_message("Returning from here is an invalid operation")
                            .with_color(Color::BrightRed),
                    )
                    .finish()
                    .write((path, Source::from(text)), &mut output)
                    .expect(ARIADNE_WRITE_MSG);
            }
            ResolutionErrorKind::NonClassThis => {
                Report::build(ReportKind::Error, (path, span.range()))
                    .with_code(error.code())
                    .with_message("Accessing `this` in a non-class context")
                    .with_label(
                        Label::new((path, span.range()))
                            .with_message("`this` can't be used here")
                            .with_color(Color::BrightRed),
                    )
                    .finish()
                    .write((path, Source::from(text)), &mut output)
                    .expect(ARIADNE_WRITE_MSG);
            }
            ResolutionErrorKind::ReturnInConstructor => {
                Report::build(ReportKind::Error, (path, span.range()))
                    .with_code(error.code())
                    .with_message("Returning in a constructor")
                    .with_label(
                        Label::new((path, span.range()))
                            .with_message("Returning values are not allowed in constructors")
                            .with_color(Color::BrightRed),
                    )
                    .finish()
                    .write((path, Source::from(text)), &mut output)
                    .expect(ARIADNE_WRITE_MSG);
            }
            ResolutionErrorKind::NonClassSuper => {
                Report::build(ReportKind::Error, (path, span.range()))
                    .with_code(error.code())
                    .with_message("Accessing `super` in a non-class context")
                    .with_label(
                        Label::new((path, span.range()))
                            .with_message("`super` can't be used here")
                            .with_color(Color::BrightRed),
                    )
                    .finish()
                    .write((path, Source::from(text)), &mut output)
                    .expect(ARIADNE_WRITE_MSG);
            }
            ResolutionErrorKind::NonSubClassSuper => {
                Report::build(ReportKind::Error, (path, span.range()))
                    .with_code(error.code())
                    .with_message("Accessing `super` in a non-subclass context")
                    .with_label(
                        Label::new((path, span.range()))
                            .with_message("`super` can't be used here")
                            .with_color(Color::BrightRed),
                    )
                    .finish()
                    .write((path, Source::from(text)), &mut output)
                    .expect(ARIADNE_WRITE_MSG);
            }
        }
        buffer.push_str(&String::from_utf8(output.into_inner()).expect(ARIADNE_MSG));
    }
}

pub struct NystromResolverFormatter {
    line_breaks: LineBreaks,
}

impl NystromResolverFormatter {
    pub fn new(text: &str) -> Self {
        Self {
            line_breaks: LineBreaks::new(text),
        }
    }
}

impl ResolverFormatter for NystromResolverFormatter {
    fn format_error_in_place(&self, buffer: &mut String, error: &ResolutionError) {
        let line = self.line_breaks.get_line_from_span(error.span);
        match &error.kind {
            ResolutionErrorKind::SelfReferentialInitializer { destination, .. } => {
                write!(buffer, "({line}) [Compiler] Error at '{}': Can't read local variable in its own initializer.", destination.name).expect(&WRITE_FMT_MSG)
            }
            ResolutionErrorKind::SelfReferentialInheritance { destination, .. } => write!(
                buffer,
                "({line}) [Compiler] Error at '{}': A class can't inherit from itself.",
                destination.name
            )
            .expect(&WRITE_FMT_MSG),
            ResolutionErrorKind::ShadowLocal { new, .. } => 
            {
                let line = self.line_breaks.get_line_from_span(new.span);
                write!(buffer, "({line}) [Compiler] Error at '{}': Already a variable with this name in this scope.", new.name).expect(&WRITE_FMT_MSG);}
            ResolutionErrorKind::NonFunctionReturn => write!(
                buffer,
                "({line}) [Compiler] Error at 'return': Can't return from top-level code."
            )
            .expect(&WRITE_FMT_MSG),
            ResolutionErrorKind::NonClassThis => {
                write!(buffer, "({line}) [Compiler] Error at 'this': Can't use 'this' outside of a class.").expect(&WRITE_FMT_MSG)
            }
            ResolutionErrorKind::NonClassSuper => {
                write!(buffer, "({line}) [Compiler] Error at 'super': Can't use 'super' outside of a class.").expect(&WRITE_FMT_MSG)
            }
            ResolutionErrorKind::NonSubClassSuper => {
                write!(buffer, "({line}) [Compiler] Error at 'super': Can't use 'super' in a class with no superclass.").expect(&WRITE_FMT_MSG)
            }
            ResolutionErrorKind::ReturnInConstructor => write!(
                buffer,
                "({line}) [Compiler] Error at 'return': Can't return a value from an initializer."
            )
            .expect(&WRITE_FMT_MSG),
        }
    }
}
