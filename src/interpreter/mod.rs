use std::collections::HashMap;

use compact_str::{CompactString, ToCompactString};

use crate::{
    evaluator::{ExpressionEvaluator, LoxValue, RuntimeError},
    expression::ExpressionTreeWithRoot,
    parser::Program,
    statement::{Declaration, NonDeclaration, Statement},
};

#[derive(Debug)]
pub enum ProgramState {
    Run,
    Terminate,
}

#[derive(Debug)]
pub struct Environment {
    globals: HashMap<CompactString, LoxValue>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
        }
    }

    pub fn get_global(&self, name: &str) -> Option<&LoxValue> {
        self.globals.get(name)
    }

    pub fn set_global(&mut self, name: &str, value: LoxValue) {
        self.globals.insert(name.to_compact_string(), value);
    }
}

pub trait Interpreter {
    fn step(&mut self) -> Result<ProgramState, RuntimeError>;
}

pub struct TreeWalkInterpreter {
    program: Program,
    environment: Environment,
    counter: usize,
}

impl TreeWalkInterpreter {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            environment: Environment::new(),
            counter: 0,
        }
    }

    // Visitors

    fn handle_statement(
        statement: &Statement,
        environment: &mut Environment,
    ) -> Result<ProgramState, RuntimeError> {
        let state = match statement {
            Statement::Declaration(Declaration::Variable { name, initial }) => {
                TreeWalkInterpreter::interpret_variable_declaration(
                    environment,
                    name,
                    initial.as_ref(),
                )?
            }
            Statement::NonDeclaration(NonDeclaration::Expression(ref expr)) => {
                TreeWalkInterpreter::interpret_expression_statement(environment, expr)?
            }
            Statement::NonDeclaration(NonDeclaration::Print(ref expr)) => {
                TreeWalkInterpreter::interpret_print_statement(environment, expr)?
            }
            Statement::NonDeclaration(NonDeclaration::Block(statements)) => {
                TreeWalkInterpreter::interpret_block_statement(environment, statements)?
            }
        };
        Ok(state)
    }

    fn interpret_variable_declaration(
        environment: &mut Environment,
        name: &str,
        initial: Option<&ExpressionTreeWithRoot>,
    ) -> Result<ProgramState, RuntimeError> {
        let initial = if let Some(expr) = initial {
            ExpressionEvaluator::evaluate_expression(expr, environment)?
        } else {
            LoxValue::Nil
        };
        environment.set_global(name, initial);
        Ok(ProgramState::Run)
    }

    fn interpret_print_statement(
        environment: &mut Environment,
        expr: &ExpressionTreeWithRoot,
    ) -> Result<ProgramState, RuntimeError> {
        let result = ExpressionEvaluator::evaluate_expression(expr, environment)?;
        println!("{result}");
        Ok(ProgramState::Run)
    }

    fn interpret_expression_statement(
        environment: &mut Environment,
        expr: &ExpressionTreeWithRoot,
    ) -> Result<ProgramState, RuntimeError> {
        let result = ExpressionEvaluator::evaluate_expression(expr, environment)?;
        eprintln!("SIDE EFFECTLESS: {result:?}");
        Ok(ProgramState::Run)
    }

    fn interpret_block_statement(
        environment: &mut Environment,
        statements: &Vec<Statement>,
    ) -> Result<ProgramState, RuntimeError> {
        // TODO(pavyamsiri): This write out program state doesn't make sense for block statements
        let mut state = ProgramState::Run;
        for statement in statements {
            state = TreeWalkInterpreter::handle_statement(statement, environment)?;
        }
        Ok(state)
    }
}

impl Interpreter for TreeWalkInterpreter {
    fn step(&mut self) -> Result<ProgramState, RuntimeError> {
        if let Some(statement) = { self.program.get_statement(self.counter) } {
            let state = TreeWalkInterpreter::handle_statement(statement, &mut self.environment)?;
            self.counter += 1;

            Ok(state)
        } else {
            Ok(ProgramState::Terminate)
        }
    }
}
