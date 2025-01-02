pub mod context;
mod tree;

pub use tree::{TreeWalkInterpreter, TreeWalkStatementInterpreter};

pub trait SystemContext {
    fn writeln(&mut self, text: &str);
}
