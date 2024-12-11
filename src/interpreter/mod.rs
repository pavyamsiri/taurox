use std::collections::HashMap;

use compact_str::{CompactString, ToCompactString};

use crate::{
    evaluator::{ExpressionEvaluator, LoxValue, RuntimeError},
    parser::Program,
    statement::{Declaration, NonDeclaration, Statement},
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
    environment: HashMap<CompactString, Option<LoxValue>>,
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
            Statement::Declaration(Declaration::Variable { name, initial }) => {
                let initial = if let Some(expr) = initial {
                    Some(ExpressionEvaluator::evaluate_expression(
                        expr,
                        &self.environment,
                    )?)
                } else {
                    None
                };
                self.environment.insert(name.clone(), initial);
            }
            Statement::NonDeclaration(NonDeclaration::Expression(ref expr)) => {
                let result = ExpressionEvaluator::evaluate_expression(expr, &self.environment)?;
                eprintln!("SIDE EFFECTLESS: {result:?}");
            }
            Statement::NonDeclaration(NonDeclaration::Print(ref expr)) => {
                let result = ExpressionEvaluator::evaluate_expression(expr, &self.environment)?;
                state = ProgramState::Write(format!("{result}").to_compact_string())
            }
        }
        self.counter += 1;

        Ok(state)
    }
}
