pub mod context;
mod tree;

use std::collections::HashMap;

use crate::environment::SharedEnvironment;
use crate::value::error::RuntimeError;
use crate::value::LoxValue;
use crate::{
    lexer::Span,
    parser::{expression::Expression, statement::Statement},
};
pub use tree::{TreeWalkInterpreter, TreeWalkStatementInterpreter};

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
