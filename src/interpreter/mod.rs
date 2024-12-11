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
    scopes: Vec<HashMap<CompactString, LoxValue>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            scopes: Vec::new(),
        }
    }

    fn get_global(&self, name: &str) -> Option<&LoxValue> {
        self.globals.get(name)
    }

    fn declare_global(&mut self, name: &str, value: LoxValue) {
        self.globals.insert(name.to_compact_string(), value);
    }

    fn assign_global(&mut self, name: &str, value: LoxValue) -> Result<(), ()> {
        if self.globals.contains_key(name) {
            self.globals.insert(name.to_compact_string(), value);
            Ok(())
        } else {
            Err(())
        }
    }

    pub fn access(&self, name: &str) -> Option<&LoxValue> {
        // Go from the innermost scope to the outermost scope
        for scope in self.scopes.iter().rev() {
            // Found the variable
            if scope.contains_key(name) {
                return scope.get(name);
            }
        }

        // Now return from globals
        self.get_global(name)
    }

    pub fn assign(&mut self, name: &str, value: LoxValue) -> Result<(), ()> {
        // Go from the innermost scope to the outermost scope
        for scope in self.scopes.iter_mut().rev() {
            // Found the variable
            if scope.contains_key(name) {
                scope.insert(name.to_compact_string(), value);
                return Ok(());
            }
        }

        // Check global
        self.assign_global(name, value)
    }

    pub fn declare(&mut self, name: &str, value: LoxValue) {
        // Add to outermost scope
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_compact_string(), value);
        } else {
            self.declare_global(name, value);
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
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
            Statement::NonDeclaration(NonDeclaration::If {
                condition,
                success,
                failure,
            }) => TreeWalkInterpreter::interpret_if_statement(
                environment,
                condition,
                success,
                failure.as_ref().as_ref(),
            )?,
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
        environment.declare(name, initial);
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
        let _ = ExpressionEvaluator::evaluate_expression(expr, environment)?;
        Ok(ProgramState::Run)
    }

    fn interpret_if_statement(
        environment: &mut Environment,
        condition: &ExpressionTreeWithRoot,
        success: &Statement,
        failure: Option<&Statement>,
    ) -> Result<ProgramState, RuntimeError> {
        if ExpressionEvaluator::evaluate_expression(condition, environment)?.is_truthy() {
            Self::handle_statement(success, environment)?;
        } else {
            if let Some(failure) = failure {
                Self::handle_statement(failure, environment)?;
            }
        }
        Ok(ProgramState::Run)
    }
    fn interpret_block_statement(
        environment: &mut Environment,
        statements: &Vec<Statement>,
    ) -> Result<ProgramState, RuntimeError> {
        let mut state = ProgramState::Run;
        environment.enter_scope();
        for statement in statements {
            state = TreeWalkInterpreter::handle_statement(statement, environment)?;
        }
        environment.exit_scope();
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
