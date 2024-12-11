use std::collections::HashMap;

use compact_str::{CompactString, ToCompactString};

use crate::{
    evaluator::{ExpressionEvaluator, LoxValue, RuntimeError},
    parser::Program,
    statement::Statement,
};

#[derive(Debug)]
pub enum ProgramState {
    Run,
    Write(CompactString),
    Terminate,
}

pub trait Interpreter {
    fn step(&mut self) -> Result<ProgramState, RuntimeError>;
}

pub struct TreeWalkInterpreter {
    program: Program,
    environment: HashMap<CompactString, LoxValue>,
    counter: usize,
}

impl TreeWalkInterpreter {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            environment: HashMap::new(),
            counter: 0,
        }
    }
}

impl Interpreter for TreeWalkInterpreter {
    fn step(&mut self) -> Result<ProgramState, RuntimeError> {
        let Some(statement) = self.program.get_statement(self.counter) else {
            return Ok(ProgramState::Terminate);
        };

        let mut state = ProgramState::Run;

        match statement {
            Statement::Expression(ref expr) => {
                let result = ExpressionEvaluator::evaluate_expression(expr)?;
                eprintln!("SIDE EFFECTLESS: {result:?}");
            }
            Statement::Print(expr) => {
                let result = ExpressionEvaluator::evaluate_expression(expr)?;
                state = ProgramState::Write(format!("{result}").to_compact_string())
            }
        }
        self.counter += 1;

        Ok(state)
    }
}
