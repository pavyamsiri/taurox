pub mod environment;
pub mod error;
pub mod expression;
pub mod formatter;
mod native;
mod tree;
mod value;

use crate::parser::statement::Statement;
use environment::Environment;
use error::RuntimeError;
pub use tree::{TreeWalkInterpreter, TreeWalkStatementInterpreter};

#[derive(Debug)]
pub enum ProgramState {
    Run,
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

    fn create() -> Self;
}
