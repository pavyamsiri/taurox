pub mod environment;
pub mod error;
pub mod expression;
pub mod formatter;
mod native;
mod tree;
mod value;

use error::RuntimeError;
pub use tree::TreeWalkInterpreter;

#[derive(Debug)]
pub enum ProgramState {
    Run,
    Terminate,
}

pub trait Interpreter {
    fn step(&mut self) -> Result<ProgramState, RuntimeError>;
}
