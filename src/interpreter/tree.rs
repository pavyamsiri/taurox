use super::{
    environment::Environment,
    error::{RuntimeError, RuntimeErrorKind},
    value::LoxValue,
    Interpreter, ProgramState, StatementInterpreter,
};
use crate::parser::{
    expression::{
        Expression, ExpressionAtom, ExpressionAtomKind, ExpressionNode, ExpressionNodeRef,
        InfixOperator, InfixShortCircuitOperator, PrefixOperator,
    },
    statement::{Declaration, Initializer, NonDeclaration, Statement},
    Program,
};
use compact_str::ToCompactString;

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
}

pub struct TreeWalkStatementInterpreter;

impl StatementInterpreter for TreeWalkStatementInterpreter {
    fn create() -> Self {
        Self {}
    }

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
            }) => self.interpret_function_declaration(
                environment,
                name,
                &parameters.iter().map(|s| s.as_ref()).collect::<Vec<&str>>(),
                body.as_ref(),
            )?,
            Statement::NonDeclaration(statement) => {
                self.interpret_non_declaration(statement, environment)?
            }
        };
        Ok(state)
    }

    fn evaluate(
        &self,
        expr: &Expression,
        environment: &mut Environment,
    ) -> Result<LoxValue, RuntimeError> {
        self.evaluate_expression_node(expr, expr.get_root_ref(), environment)
    }
}

// Statement interpreter
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
            NonDeclaration::Return { value } => {
                self.interpret_return_statement(environment, value.as_ref())?
            }
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
            self.evaluate(expr, environment)?
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
        parameters: &[&str],
        body: &[Statement],
    ) -> Result<ProgramState, RuntimeError> {
        environment.declare(
            name,
            LoxValue::Function {
                name: name.into(),
                parameters: parameters
                    .to_vec()
                    .iter()
                    .map(|&s| s.to_compact_string())
                    .collect(),
                body: body.to_vec(),
            },
        );
        Ok(ProgramState::Run)
    }

    fn interpret_print_statement(
        &self,
        environment: &mut Environment,
        expr: &Expression,
    ) -> Result<ProgramState, RuntimeError> {
        let result = self.evaluate(expr, environment)?;
        println!("{result}");
        Ok(ProgramState::Run)
    }

    fn interpret_expression_statement(
        &self,
        environment: &mut Environment,
        expr: &Expression,
    ) -> Result<ProgramState, RuntimeError> {
        let _ = self.evaluate(expr, environment)?;
        Ok(ProgramState::Run)
    }

    fn interpret_if_statement(
        &self,
        environment: &mut Environment,
        condition: &Expression,
        success: &Statement,
        failure: Option<&Statement>,
    ) -> Result<ProgramState, RuntimeError> {
        let mut state = ProgramState::Run;
        if self.evaluate(condition, environment)?.is_truthy() {
            state = self.interpret_statement(success, environment)?;
        } else {
            if let Some(failure) = failure {
                state = self.interpret_statement(failure, environment)?;
            }
        }
        Ok(state)
    }

    fn interpret_while_statement(
        &self,
        environment: &mut Environment,
        condition: &Expression,
        body: &Statement,
    ) -> Result<ProgramState, RuntimeError> {
        while self.evaluate(condition, environment)?.is_truthy() {
            match self.interpret_statement(body, environment)? {
                ProgramState::Run => {}
                s => {
                    return Ok(s);
                }
            }
        }

        Ok(ProgramState::Run)
    }

    fn interpret_return_statement(
        &self,
        environment: &mut Environment,
        value: Option<&Expression>,
    ) -> Result<ProgramState, RuntimeError> {
        let value = if let Some(expr) = value {
            self.evaluate(expr, environment)?
        } else {
            LoxValue::Nil
        };

        Ok(ProgramState::Return(value))
    }

    fn interpret_block_statement(
        &self,
        environment: &mut Environment,
        statements: &Vec<Statement>,
    ) -> Result<ProgramState, RuntimeError> {
        let mut state = ProgramState::Run;
        environment.enter_scope();
        for statement in statements {
            match self.interpret_statement(statement, environment)? {
                ProgramState::Run => {}
                s => {
                    state = s;
                    break;
                }
            }
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
                self.evaluate(expr, environment)?;
            }
            _ => {}
        };

        loop {
            let flag = match condition {
                Some(condition) => self.evaluate(condition, environment)?.is_truthy(),
                None => true,
            };

            if !flag {
                break;
            }

            self.interpret_non_declaration(body, environment)?;
            match increment {
                Some(increment) => {
                    self.evaluate(increment, environment)?;
                }
                None => {}
            }
        }
        environment.exit_scope();
        Ok(ProgramState::Run)
    }
}

// Expression evaluator
impl TreeWalkStatementInterpreter {
    fn evaluate_expression_node(
        &self,
        expr: &Expression,
        node: ExpressionNodeRef,
        environment: &mut Environment,
    ) -> Result<LoxValue, RuntimeError> {
        let current_node = expr
            .get_node(node)
            .expect("Node ref came from the tree so it must exist.");
        let line = expr
            .get_line(node)
            .expect("Node ref came from the tree so it must exist.");

        let result = match current_node {
            ExpressionNode::Atom(atom) => self
                .evaluate_atom(atom, environment)
                .map_err(|kind| RuntimeError { kind, line })?,
            ExpressionNode::Prefix { operator, rhs } => {
                let rhs = self.evaluate_expression_node(expr, *rhs, environment)?;
                self.evaluate_prefix(*operator, &rhs)
                    .map_err(|kind| RuntimeError { kind, line })?
            }
            ExpressionNode::Group { inner } => {
                self.evaluate_expression_node(expr, *inner, environment)?
            }
            ExpressionNode::Infix { operator, lhs, rhs } => {
                let lhs = self.evaluate_expression_node(expr, *lhs, environment)?;
                let rhs = self.evaluate_expression_node(expr, *rhs, environment)?;
                self.evaluate_infix(*operator, &lhs, &rhs)
                    .map_err(|kind| RuntimeError { kind, line })?
            }
            ExpressionNode::InfixAssignment { lhs, rhs } => {
                let rhs = self.evaluate_expression_node(expr, *rhs, environment)?;
                let _ = environment
                    .assign(lhs, rhs.clone())
                    .map_err(|_| RuntimeError {
                        kind: RuntimeErrorKind::InvalidAccess(lhs.clone()),
                        line,
                    })?;
                rhs
            }
            ExpressionNode::InfixShortCircuit { operator, lhs, rhs } => {
                self.evaluate_infix_short_circuit(*operator, *lhs, *rhs, expr, environment)?
            }
            ExpressionNode::Call { callee, arguments } => {
                self.evaluate_call(*callee, arguments, expr, environment, line)?
            }
        };
        Ok(result)
    }
    // Atoms
    fn evaluate_atom(
        &self,
        atom: &ExpressionAtom,
        environment: &mut Environment,
    ) -> Result<LoxValue, RuntimeErrorKind> {
        let result = match &atom.kind {
            ExpressionAtomKind::Number(v) => LoxValue::Number(*v),
            ExpressionAtomKind::Bool(v) => LoxValue::Bool(*v),
            ExpressionAtomKind::Nil => LoxValue::Nil,
            ExpressionAtomKind::StringLiteral(ref v) => LoxValue::String(v.clone()),
            ExpressionAtomKind::Identifier(ref name) => environment
                .access(name)
                .ok_or(RuntimeErrorKind::InvalidAccess(name.clone()))?
                .clone(),
        };
        Ok(result)
    }

    fn evaluate_prefix(
        &self,
        operator: PrefixOperator,
        rhs: &LoxValue,
    ) -> Result<LoxValue, RuntimeErrorKind> {
        type Operator = PrefixOperator;
        match operator {
            Operator::Bang => Ok(LoxValue::Bool(rhs.logical_not())),
            Operator::Minus => rhs.numeric_negate(),
        }
    }

    fn evaluate_infix(
        &self,
        operator: InfixOperator,
        lhs: &LoxValue,
        rhs: &LoxValue,
    ) -> Result<LoxValue, RuntimeErrorKind> {
        type Operator = InfixOperator;
        match operator {
            Operator::Add => lhs.add(rhs),
            Operator::Subtract => lhs.subtract(rhs),
            Operator::Multiply => lhs.multiply(rhs),
            Operator::Divide => lhs.divide(rhs),
            Operator::LessThan => lhs.less_than(rhs),
            Operator::LessThanEqual => lhs.less_than_or_equal(rhs),
            Operator::GreaterThan => lhs.greater_than(rhs),
            Operator::GreaterThanEqual => lhs.greater_than_or_equal(rhs),
            Operator::EqualEqual => Ok(LoxValue::Bool(lhs.is_equal(rhs))),
            Operator::BangEqual => Ok(LoxValue::Bool(lhs.is_not_equal(rhs))),
        }
    }

    fn evaluate_infix_short_circuit(
        &self,
        operator: InfixShortCircuitOperator,
        lhs: ExpressionNodeRef,
        rhs: ExpressionNodeRef,
        expr: &Expression,
        environment: &mut Environment,
    ) -> Result<LoxValue, RuntimeError> {
        type Operator = InfixShortCircuitOperator;
        let lhs = { self.evaluate_expression_node(expr, lhs, environment)? };

        match operator {
            Operator::And => {
                if !lhs.is_truthy() {
                    Ok(lhs)
                } else {
                    let rhs = self.evaluate_expression_node(expr, rhs, environment)?;
                    Ok(rhs)
                }
            }
            Operator::Or => {
                if lhs.is_truthy() {
                    Ok(lhs)
                } else {
                    let rhs = self.evaluate_expression_node(expr, rhs, environment)?;
                    Ok(rhs)
                }
            }
        }
    }

    fn evaluate_call(
        &self,
        callee: ExpressionNodeRef,
        arguments: &[ExpressionNodeRef],
        expr: &Expression,
        environment: &mut Environment,
        line: u32,
    ) -> Result<LoxValue, RuntimeError> {
        let callee = self.evaluate_expression_node(expr, callee, environment)?;
        let result = match callee {
            LoxValue::NativeFunction(fun) => {
                // Set up scope
                environment.enter_scope();

                // Check that the argument list is the same length as the parameter list.
                if arguments.len() != fun.get_parameters().len() {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::InvalidArgumentCount {
                            actual: arguments.len(),
                            expected: fun.get_parameters().len(),
                        },
                        line,
                    });
                }

                // Define the arguments in the function scope
                for (name, argument) in fun.get_parameters().iter().zip(arguments.iter()) {
                    let argument = self.evaluate_expression_node(expr, *argument, environment)?;
                    environment.declare(name, argument);
                }

                let result = fun.call(environment)?;

                environment.exit_scope();
                result
            }
            LoxValue::Function {
                name: _,
                parameters,
                body,
            } => {
                // Set up scope
                environment.enter_scope();

                // Check that the argument list is the same length as the parameter list.
                if arguments.len() != parameters.len() {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::InvalidArgumentCount {
                            actual: arguments.len(),
                            expected: parameters.len(),
                        },
                        line,
                    });
                }

                // Define the arguments in the function scope
                for (name, argument) in parameters.iter().zip(arguments.iter()) {
                    let argument = self.evaluate_expression_node(expr, *argument, environment)?;
                    environment.declare(name, argument);
                }

                let mut result = LoxValue::Nil;
                for statement in body {
                    match self.interpret_statement(&statement, environment)? {
                        ProgramState::Run => {}
                        ProgramState::Return(v) => {
                            result = v;
                            break;
                        }
                        ProgramState::Terminate => {
                            panic!("Terminating during function?");
                        }
                    }
                }
                // Exit scope
                environment.exit_scope();
                result
            }
            v => {
                return Err(RuntimeError {
                    kind: RuntimeErrorKind::InvalidCallee(v),
                    line,
                });
            }
        };
        Ok(result)
    }
}
