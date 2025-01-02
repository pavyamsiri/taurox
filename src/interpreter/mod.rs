pub mod context;
mod tree;

use crate::value::LoxValue;
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
