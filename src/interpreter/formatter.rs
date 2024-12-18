use super::{
    error::{RuntimeError, RuntimeErrorKind},
    value::LoxValue,
};

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

pub struct BasicFormatter;

impl BasicFormatter {
    fn format_verbose(&self, value: &LoxValue) -> String {
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
        let line = error.line;
        match error.kind {
            RuntimeErrorKind::NonNumeric(ref v) => {
                format!("({line}) Non-Number {{Unary}}: {}", self.format_verbose(v))
            }
            RuntimeErrorKind::NonNumerics(ref lhs, ref rhs) => format!(
                "({line}) Non-Numbers {{Binary}}: [{}, {}]",
                self.format_verbose(lhs),
                self.format_verbose(rhs)
            ),
            RuntimeErrorKind::NonAddable(ref lhs, ref rhs) => format!(
                "({line}) Non-Numbers/Non-Strings {{Binary}}: [{}, {}]",
                self.format_verbose(lhs),
                self.format_verbose(rhs)
            ),
            RuntimeErrorKind::InvalidAccess(ref name) => {
                format!("({line}) Invalid Access: {name}",)
            }
            RuntimeErrorKind::InvalidCallee(ref callee) => {
                format!("({line}) Invalid Callee: {}", self.format_verbose(callee))
            }
            RuntimeErrorKind::InvalidArgumentCount { actual, expected } => {
                format!("({line}) Invalid Argument Count: {actual} of {expected}")
            }
        }
    }
}
