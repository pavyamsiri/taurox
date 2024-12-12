use compact_str::CompactString;

use crate::parser::{
    expression::Expression,
    statement::{Declaration, Initializer, NonDeclaration, Statement},
    Program,
};

use super::{
    environment::Environment, error::RuntimeError, expression::ExpressionEvaluator,
    value::LoxValue, Interpreter, ProgramState, StatementInterpreter,
};

pub struct TreeWalkInterpreter<S> {
    program: Program,
    environment: Environment,
    counter: usize,
    interpreter: S,
}

impl<S> Interpreter for TreeWalkInterpreter<S>
where
    S: StatementInterpreter,
{
    fn step(&mut self) -> Result<ProgramState, RuntimeError> {
        if let Some(statement) = { self.program.get_statement(self.counter) } {
            let state = self
                .interpreter
                .interpret_statement(statement, &mut self.environment)?;
            self.counter += 1;

            Ok(state)
        } else {
            Ok(ProgramState::Terminate)
        }
    }
}

impl<S> TreeWalkInterpreter<S>
where
    S: StatementInterpreter,
{
    pub fn new(program: Program) -> Self {
        Self {
            program,
            environment: Environment::new(),
            counter: 0,
            interpreter: S::create(),
        }
    }

    // Visitors
}

pub struct TreeWalkStatementInterpreter;

impl StatementInterpreter for TreeWalkStatementInterpreter {
    fn interpret_statement(
        &self,
        statement: &Statement,
        environment: &mut Environment,
    ) -> Result<ProgramState, RuntimeError> {
        let state = match statement {
            Statement::Declaration(Declaration::Variable { name, initial }) => {
                self.interpret_variable_declaration(environment, name, initial.as_ref())?
            }
            Statement::Declaration(Declaration::Function {
                name,
                parameters,
                body,
            }) => {
                self.interpret_function_declaration(environment, name, &parameters, body.as_ref())?
            }
            Statement::NonDeclaration(statement) => {
                self.interpret_non_declaration(statement, environment)?
            }
        };
        Ok(state)
    }

    fn create() -> Self {
        Self {}
    }
}

impl TreeWalkStatementInterpreter {
    fn interpret_non_declaration(
        &self,
        statement: &NonDeclaration,
        environment: &mut Environment,
    ) -> Result<ProgramState, RuntimeError> {
        let state = match statement {
            NonDeclaration::Expression(expr) => {
                self.interpret_expression_statement(environment, expr)?
            }
            NonDeclaration::Print(expr) => self.interpret_print_statement(environment, expr)?,
            NonDeclaration::Block(statements) => {
                self.interpret_block_statement(environment, statements)?
            }
            NonDeclaration::If {
                condition,
                success,
                failure,
            } => self.interpret_if_statement(
                environment,
                condition,
                success,
                failure.as_ref().as_ref(),
            )?,
            NonDeclaration::While { condition, body } => {
                self.interpret_while_statement(environment, condition, body.as_ref())?
            }
            NonDeclaration::For {
                initializer,
                condition,
                increment,
                body,
            } => self.interpret_for_statement(
                environment,
                initializer.as_ref(),
                condition.as_ref(),
                increment.as_ref(),
                body,
            )?,
        };
        Ok(state)
    }

    fn interpret_variable_declaration(
        &self,
        environment: &mut Environment,
        name: &str,
        initial: Option<&Expression>,
    ) -> Result<ProgramState, RuntimeError> {
        let initial = if let Some(expr) = initial {
            ExpressionEvaluator::evaluate_expression(expr, environment)?
        } else {
            LoxValue::Nil
        };
        environment.declare(name, initial);
        Ok(ProgramState::Run)
    }

    fn interpret_function_declaration(
        &self,
        environment: &mut Environment,
        name: &str,
        parameters: &[CompactString],
        body: &[Statement],
    ) -> Result<ProgramState, RuntimeError> {
        let _ = environment;
        let _ = name;
        let _ = parameters;
        let _ = body;
        todo!();
    }

    fn interpret_print_statement(
        &self,
        environment: &mut Environment,
        expr: &Expression,
    ) -> Result<ProgramState, RuntimeError> {
        let result = ExpressionEvaluator::evaluate_expression(expr, environment)?;
        println!("{result}");
        Ok(ProgramState::Run)
    }

    fn interpret_expression_statement(
        &self,
        environment: &mut Environment,
        expr: &Expression,
    ) -> Result<ProgramState, RuntimeError> {
        let _ = ExpressionEvaluator::evaluate_expression(expr, environment)?;
        Ok(ProgramState::Run)
    }

    fn interpret_if_statement(
        &self,
        environment: &mut Environment,
        condition: &Expression,
        success: &Statement,
        failure: Option<&Statement>,
    ) -> Result<ProgramState, RuntimeError> {
        if ExpressionEvaluator::evaluate_expression(condition, environment)?.is_truthy() {
            self.interpret_statement(success, environment)?;
        } else {
            if let Some(failure) = failure {
                self.interpret_statement(failure, environment)?;
            }
        }
        Ok(ProgramState::Run)
    }

    fn interpret_while_statement(
        &self,
        environment: &mut Environment,
        condition: &Expression,
        body: &Statement,
    ) -> Result<ProgramState, RuntimeError> {
        while ExpressionEvaluator::evaluate_expression(condition, environment)?.is_truthy() {
            self.interpret_statement(body, environment)?;
        }

        Ok(ProgramState::Run)
    }

    fn interpret_block_statement(
        &self,
        environment: &mut Environment,
        statements: &Vec<Statement>,
    ) -> Result<ProgramState, RuntimeError> {
        let mut state = ProgramState::Run;
        environment.enter_scope();
        for statement in statements {
            state = self.interpret_statement(statement, environment)?;
        }
        environment.exit_scope();
        Ok(state)
    }

    fn interpret_for_statement(
        &self,
        environment: &mut Environment,
        initializer: Option<&Initializer>,
        condition: Option<&Expression>,
        increment: Option<&Expression>,
        body: &NonDeclaration,
    ) -> Result<ProgramState, RuntimeError> {
        // Run the initializer
        // NOTE(pavyamsiri): I don't really understanding how for loops are scoped.
        environment.enter_scope();
        match initializer {
            Some(Initializer::VarDecl { name, initial }) => {
                self.interpret_variable_declaration(environment, name, initial.as_ref())?;
            }
            Some(Initializer::Expression(expr)) => {
                ExpressionEvaluator::evaluate_expression(expr, environment)?;
            }
            _ => {}
        };

        loop {
            let flag = match condition {
                Some(condition) => {
                    ExpressionEvaluator::evaluate_expression(condition, environment)?.is_truthy()
                }
                None => true,
            };

            if !flag {
                break;
            }

            self.interpret_non_declaration(body, environment)?;
            match increment {
                Some(increment) => {
                    ExpressionEvaluator::evaluate_expression(increment, environment)?;
                }
                None => {}
            }
        }
        environment.exit_scope();
        Ok(ProgramState::Run)
    }
}
