pub mod context;
pub mod environment;
pub mod error;
pub mod formatter;
mod native;
pub mod resolver;
mod tree;
mod value;

use std::collections::HashMap;

use crate::{
    lexer::Span,
    parser::{expression::Expression, statement::Statement},
};
use environment::SharedEnvironment;
use error::RuntimeError;
pub use tree::{TreeWalkInterpreter, TreeWalkStatementInterpreter};
use value::LoxValue;

#[derive(Debug)]
pub enum ProgramState {
    Run,
    Return(LoxValue),
    Terminate,
}

pub trait SystemContext {
    fn writeln(&mut self, text: &str);
}

pub trait Interpreter<C> {
    fn step(&mut self, context: &mut C) -> Result<ProgramState, RuntimeError>;
}

pub trait StatementInterpreter<C> {
    fn interpret_statement(
        &self,
        statement: &Statement,
        environment: &mut SharedEnvironment,
        context: &mut C,
        resolution: &HashMap<Span, usize>,
    ) -> Result<ProgramState, RuntimeError>;

    fn evaluate(
        &self,
        expr: &Expression,
        environment: &mut SharedEnvironment,
        context: &mut C,
        resolution: &HashMap<Span, usize>,
    ) -> Result<LoxValue, RuntimeError>;

    fn create() -> Self;
}
