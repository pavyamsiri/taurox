pub mod environment;
pub mod error;
pub mod formatter;
mod native;
mod tree;
mod value;

use crate::parser::{expression::Expression, statement::Statement};
use environment::Environment;
use error::RuntimeError;
pub use tree::{TreeWalkInterpreter, TreeWalkStatementInterpreter};
use value::LoxValue;

#[derive(Debug)]
pub enum ProgramState {
    Run,
    Return(LoxValue),
    Terminate,
}

pub trait Interpreter {
    fn step(&mut self) -> Result<ProgramState, RuntimeError>;
}

pub trait StatementInterpreter {
    fn interpret_statement(
        &self,
        statement: &Statement,
        environment: &mut Environment,
    ) -> Result<ProgramState, RuntimeError>;

    fn evaluate(
        &self,
        expr: &Expression,
        environment: &mut Environment,
    ) -> Result<LoxValue, RuntimeError>;

    fn create() -> Self;
}
