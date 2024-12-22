use super::{
    environment::SharedEnvironment,
    error::{RuntimeError, RuntimeErrorKind},
    value::LoxValue,
    Interpreter, ProgramState, StatementInterpreter, SystemContext,
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

pub struct TreeWalkInterpreter<S, C> {
    program: Program,
    environment: SharedEnvironment,
    counter: usize,
    interpreter: S,
    _context: std::marker::PhantomData<C>,
}

impl<S, C> Interpreter<C> for TreeWalkInterpreter<S, C>
where
    S: StatementInterpreter<C>,
    C: SystemContext,
{
    fn step(&mut self, context: &mut C) -> Result<ProgramState, RuntimeError> {
        if let Some(statement) = { self.program.get_statement(self.counter) } {
            let state =
                self.interpreter
                    .interpret_statement(statement, &mut self.environment, context)?;
            self.counter += 1;

            Ok(state)
        } else {
            Ok(ProgramState::Terminate)
        }
    }
}

impl<S, C> TreeWalkInterpreter<S, C>
where
    S: StatementInterpreter<C>,
    C: SystemContext,
{
    pub fn new(program: Program) -> Self {
        Self {
            program,
            environment: SharedEnvironment::new(),
            counter: 0,
            interpreter: S::create(),
            _context: std::marker::PhantomData,
        }
    }
}

pub struct TreeWalkStatementInterpreter;

impl<C> StatementInterpreter<C> for TreeWalkStatementInterpreter
where
    C: SystemContext,
{
    fn create() -> Self {
        Self {}
    }

    fn interpret_statement(
        &self,
        statement: &Statement,
        environment: &mut SharedEnvironment,
        context: &mut C,
    ) -> Result<ProgramState, RuntimeError> {
        let state = match statement {
            Statement::Declaration(Declaration::Variable { name, initial }) => {
                self.interpret_variable_declaration(environment, context, name, initial.as_ref())?
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
                self.interpret_non_declaration(statement, environment, context)?
            }
        };
        Ok(state)
    }

    fn evaluate(
        &self,
        expr: &Expression,
        environment: &mut SharedEnvironment,
        context: &mut C,
    ) -> Result<LoxValue, RuntimeError> {
        self.evaluate_expression_node(expr, expr.get_root_ref(), environment, context)
    }
}

// Statement interpreter
impl TreeWalkStatementInterpreter {
    fn interpret_non_declaration<C: SystemContext>(
        &self,
        statement: &NonDeclaration,
        environment: &mut SharedEnvironment,
        context: &mut C,
    ) -> Result<ProgramState, RuntimeError> {
        let state = match statement {
            NonDeclaration::Expression(expr) => {
                self.interpret_expression_statement(environment, context, expr)?
            }
            NonDeclaration::Print(expr) => {
                self.interpret_print_statement(environment, context, expr)?
            }
            NonDeclaration::Block(statements) => {
                self.interpret_block_statement(environment, context, statements)?
            }
            NonDeclaration::If {
                condition,
                success,
                failure,
            } => self.interpret_if_statement(
                environment,
                context,
                condition,
                success,
                failure.as_ref().as_ref(),
            )?,
            NonDeclaration::While { condition, body } => {
                self.interpret_while_statement(environment, context, condition, body.as_ref())?
            }
            NonDeclaration::For {
                initializer,
                condition,
                increment,
                body,
            } => self.interpret_for_statement(
                environment,
                context,
                initializer.as_ref(),
                condition.as_ref(),
                increment.as_ref(),
                body,
            )?,
            NonDeclaration::Return { value } => {
                self.interpret_return_statement(environment, context, value.as_ref())?
            }
        };
        Ok(state)
    }

    fn interpret_variable_declaration<C: SystemContext>(
        &self,
        environment: &mut SharedEnvironment,
        context: &mut C,
        name: &str,
        initial: Option<&Expression>,
    ) -> Result<ProgramState, RuntimeError> {
        let initial = if let Some(expr) = initial {
            self.evaluate(expr, environment, context)?
        } else {
            LoxValue::Nil
        };
        environment.declare(name, initial);
        Ok(ProgramState::Run)
    }

    fn interpret_function_declaration(
        &self,
        environment: &mut SharedEnvironment,
        name: &str,
        parameters: &[&str],
        body: &[Statement],
    ) -> Result<ProgramState, RuntimeError> {
        environment.declare(
            name,
            LoxValue::Function {
                name: name.into(),
                parameters: parameters.to_vec().iter().map(|&s| s.into()).collect(),
                body: body.to_vec(),
                closure: environment.new_scope(),
            },
        );
        Ok(ProgramState::Run)
    }

    fn interpret_print_statement<C: SystemContext>(
        &self,
        environment: &mut SharedEnvironment,
        context: &mut C,
        expr: &Expression,
    ) -> Result<ProgramState, RuntimeError> {
        let result = self.evaluate(expr, environment, context)?;
        context.writeln(&format!("{result}"));
        Ok(ProgramState::Run)
    }

    fn interpret_expression_statement<C: SystemContext>(
        &self,
        environment: &mut SharedEnvironment,
        context: &mut C,
        expr: &Expression,
    ) -> Result<ProgramState, RuntimeError> {
        let _ = self.evaluate(expr, environment, context)?;
        Ok(ProgramState::Run)
    }

    fn interpret_if_statement<C: SystemContext>(
        &self,
        environment: &mut SharedEnvironment,
        context: &mut C,
        condition: &Expression,
        success: &Statement,
        failure: Option<&Statement>,
    ) -> Result<ProgramState, RuntimeError> {
        let mut state = ProgramState::Run;
        if self.evaluate(condition, environment, context)?.is_truthy() {
            state = self.interpret_statement(success, environment, context)?;
        } else {
            if let Some(failure) = failure {
                state = self.interpret_statement(failure, environment, context)?;
            }
        }
        Ok(state)
    }

    fn interpret_while_statement<C: SystemContext>(
        &self,
        environment: &mut SharedEnvironment,
        context: &mut C,
        condition: &Expression,
        body: &Statement,
    ) -> Result<ProgramState, RuntimeError> {
        while self.evaluate(condition, environment, context)?.is_truthy() {
            match self.interpret_statement(body, environment, context)? {
                ProgramState::Run => {}
                s => {
                    return Ok(s);
                }
            }
        }

        Ok(ProgramState::Run)
    }

    fn interpret_return_statement<C: SystemContext>(
        &self,
        environment: &mut SharedEnvironment,
        context: &mut C,
        value: Option<&Expression>,
    ) -> Result<ProgramState, RuntimeError> {
        let value = if let Some(expr) = value {
            self.evaluate(expr, environment, context)?
        } else {
            LoxValue::Nil
        };

        Ok(ProgramState::Return(value))
    }

    fn interpret_block_statement<C: SystemContext>(
        &self,
        environment: &mut SharedEnvironment,
        context: &mut C,
        statements: &Vec<Statement>,
    ) -> Result<ProgramState, RuntimeError> {
        let mut environment = environment.new_scope();
        let mut state = ProgramState::Run;
        for statement in statements {
            match self.interpret_statement(statement, &mut environment, context)? {
                ProgramState::Run => {}
                s => {
                    state = s;
                    break;
                }
            }
        }
        Ok(state)
    }

    fn interpret_for_statement<C: SystemContext>(
        &self,
        environment: &mut SharedEnvironment,
        context: &mut C,
        initializer: Option<&Initializer>,
        condition: Option<&Expression>,
        increment: Option<&Expression>,
        body: &NonDeclaration,
    ) -> Result<ProgramState, RuntimeError> {
        // Run the initializer
        let mut environment = environment.new_scope();
        match initializer {
            Some(Initializer::VarDecl { ref name, initial }) => {
                self.interpret_variable_declaration(
                    &mut environment,
                    context,
                    name,
                    initial.as_ref(),
                )?;
            }
            Some(Initializer::Expression(ref expr)) => {
                self.evaluate(expr, &mut environment, context)?;
            }
            _ => {}
        };

        loop {
            let flag = match condition {
                Option::Some(condition) => self
                    .evaluate(condition, &mut environment, context)?
                    .is_truthy(),
                Option::None => true,
            };

            if !flag {
                break;
            }

            self.interpret_non_declaration(body, &mut environment, context)?;
            match increment {
                Option::Some(increment) => {
                    self.evaluate(increment, &mut environment, context)?;
                }
                Option::None => {}
            }
        }
        Ok(ProgramState::Run)
    }
}

// Expression evaluator
impl TreeWalkStatementInterpreter {
    fn evaluate_expression_node<C: SystemContext>(
        &self,
        expr: &Expression,
        node: ExpressionNodeRef,
        environment: &mut SharedEnvironment,
        context: &mut C,
    ) -> Result<LoxValue, RuntimeError> {
        let current_node = expr
            .get_node(node)
            .expect("Node ref came from the tree so it must exist.");
        let span = expr.get_span();

        let result = match current_node {
            ExpressionNode::Atom(ref atom) => self.evaluate_atom(atom, environment)?,
            ExpressionNode::Prefix { operator, rhs } => {
                let rhs = self.evaluate_expression_node(expr, *rhs, environment, context)?;
                self.evaluate_prefix(*operator, &rhs)
                    .map_err(|kind| RuntimeError { kind, span })?
            }
            ExpressionNode::Group { inner } => {
                self.evaluate_expression_node(expr, *inner, environment, context)?
            }
            ExpressionNode::Infix { operator, lhs, rhs } => {
                let lhs = self.evaluate_expression_node(expr, *lhs, environment, context)?;
                let rhs = self.evaluate_expression_node(expr, *rhs, environment, context)?;
                self.evaluate_infix(*operator, &lhs, &rhs)
                    .map_err(|kind| RuntimeError { kind, span })?
            }
            ExpressionNode::InfixAssignment { lhs, rhs } => {
                let rhs = self.evaluate_expression_node(expr, *rhs, environment, context)?;
                let _ = environment
                    .assign(&lhs.get_name(), rhs.clone())
                    .map_err(|_| RuntimeError {
                        kind: RuntimeErrorKind::InvalidAccess(lhs.get_name()),
                        span,
                    })?;

                rhs
            }
            ExpressionNode::InfixShortCircuit { operator, lhs, rhs } => self
                .evaluate_infix_short_circuit(*operator, *lhs, *rhs, expr, environment, context)?,
            ExpressionNode::Call { callee, arguments } => {
                self.evaluate_call(*callee, arguments, expr, environment, context)?
            }
        };
        Ok(result)
    }
    // Atoms
    fn evaluate_atom(
        &self,
        atom: &ExpressionAtom,
        environment: &mut SharedEnvironment,
    ) -> Result<LoxValue, RuntimeError> {
        let span = atom.span;
        let result = match &atom.kind {
            ExpressionAtomKind::Number(v) => LoxValue::Number(*v),
            ExpressionAtomKind::Bool(v) => LoxValue::Bool(*v),
            ExpressionAtomKind::Nil => LoxValue::Nil,
            ExpressionAtomKind::StringLiteral(ref v) => LoxValue::String(v.to_compact_string()),
            ExpressionAtomKind::Identifier(ref name) => environment
                .access(name)
                .ok_or(RuntimeError {
                    kind: RuntimeErrorKind::InvalidAccess(name.clone()),
                    span,
                })?
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

    fn evaluate_infix_short_circuit<C: SystemContext>(
        &self,
        operator: InfixShortCircuitOperator,
        lhs: ExpressionNodeRef,
        rhs: ExpressionNodeRef,
        expr: &Expression,
        environment: &mut SharedEnvironment,
        context: &mut C,
    ) -> Result<LoxValue, RuntimeError> {
        type Operator = InfixShortCircuitOperator;
        let lhs = { self.evaluate_expression_node(expr, lhs, environment, context)? };

        match operator {
            Operator::And => {
                if !lhs.is_truthy() {
                    Ok(lhs)
                } else {
                    let rhs = self.evaluate_expression_node(expr, rhs, environment, context)?;
                    Ok(rhs)
                }
            }
            Operator::Or => {
                if lhs.is_truthy() {
                    Ok(lhs)
                } else {
                    let rhs = self.evaluate_expression_node(expr, rhs, environment, context)?;
                    Ok(rhs)
                }
            }
        }
    }

    fn evaluate_call<C: SystemContext>(
        &self,
        callee: ExpressionNodeRef,
        arguments: &[ExpressionNodeRef],
        expr: &Expression,
        environment: &mut SharedEnvironment,
        context: &mut C,
    ) -> Result<LoxValue, RuntimeError> {
        let span = expr
            .get_subspan(callee)
            .expect("Caller is expected to give a valid callee node ref.");
        let callee = self.evaluate_expression_node(expr, callee, environment, context)?;
        let result = match callee {
            LoxValue::NativeFunction(fun) => {
                // Set up scope
                let mut environment = environment.new_scope();

                // Check that the argument list is the same length as the parameter list.
                if arguments.len() != fun.get_parameters().len() {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::InvalidArgumentCount {
                            actual: arguments.len(),
                            expected: fun.get_parameters().len(),
                        },
                        span,
                    });
                }

                // Define the arguments in the function scope
                for (name, argument) in fun.get_parameters().iter().zip(arguments.iter()) {
                    let argument =
                        self.evaluate_expression_node(expr, *argument, &mut environment, context)?;
                    environment.declare(name, argument);
                }

                let result = fun.call(&mut environment)?;

                result
            }
            LoxValue::Function {
                name,
                parameters,
                body,
                closure,
            } => {
                let _ = name;
                // Set up scope
                let mut inner_scope = closure.new_scope();

                // Check that the argument list is the same length as the parameter list.
                if arguments.len() != parameters.len() {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::InvalidArgumentCount {
                            actual: arguments.len(),
                            expected: parameters.len(),
                        },
                        span,
                    });
                }

                // Define the arguments in the function scope
                for (name, argument) in parameters.iter().zip(arguments.iter()) {
                    let argument =
                        self.evaluate_expression_node(expr, *argument, environment, context)?;
                    inner_scope.declare(name, argument);
                }

                let mut result = LoxValue::Nil;
                for statement in body {
                    match self.interpret_statement(&statement, &mut inner_scope, context)? {
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
                result
            }
            v => {
                return Err(RuntimeError {
                    kind: RuntimeErrorKind::InvalidCallee(v),
                    span,
                });
            }
        };
        Ok(result)
    }
}
