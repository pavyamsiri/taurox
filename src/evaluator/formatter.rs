use super::{LoxValue, RuntimeError, RuntimeErrorKind};

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
            LoxValue::Number(v) => format!("{v})"),
            LoxValue::String(v) => format!("String(\"{v}\")"),
            LoxValue::Nil => format!("Nil"),
            LoxValue::Bool(v) => format!("Bool({v})"),
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
        }
    }
}
