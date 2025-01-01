use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use compact_str::ToCompactString;

use super::{Interpreter, ProgramState, StatementInterpreter, SystemContext};
use crate::environment::SharedEnvironment;
use crate::lexer::Span;
use crate::parser::statement::{
    BlockStatement, ClassDecl, ExpressionStatement, ForStatement, FunctionDecl, IfStatement,
    PrintStatement, ReturnStatement, VariableDecl, WhileStatement,
};
use crate::resolver::ResolutionMap;
use crate::value::error::{RuntimeError, RuntimeErrorKind};
use crate::value::{Class, Function, Instance, LoxValue};
use crate::{
    parser::{
        expression::{
            Expression, ExpressionAtom, ExpressionAtomKind, ExpressionNode, ExpressionNodeRef,
            InfixOperator, InfixShortCircuitOperator, PrefixOperator,
        },
        statement::{Declaration, Initializer, NonDeclaration, Statement},
        Program,
    },
    string::Ident,
};

pub struct TreeWalkInterpreter<S, C> {
    program: Program,
    environment: SharedEnvironment,
    counter: usize,
    resolution: ResolutionMap,
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
            let state = self.interpreter.interpret_statement(
                statement,
                &mut self.environment,
                context,
                &self.resolution,
            )?;
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
    pub fn new(program: Program, resolution: ResolutionMap) -> Self {
        Self {
            program,
            environment: SharedEnvironment::new(),
            counter: 0,
            interpreter: S::create(),
            _context: std::marker::PhantomData,
            resolution,
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
        resolution: &ResolutionMap,
    ) -> Result<ProgramState, RuntimeError> {
        let state = match statement {
            Statement::Declaration(Declaration::Variable(decl)) => {
                self.interpret_variable_declaration(environment, context, &decl, resolution)?
            }
            Statement::Declaration(Declaration::Function(decl)) => {
                self.interpret_function_declaration(environment, decl)?
            }
            Statement::Declaration(Declaration::Class(decl)) => {
                self.interpret_class_declaration(environment, decl, resolution)?
            }
            Statement::NonDeclaration(statement) => {
                self.interpret_non_declaration(statement, environment, context, resolution)?
            }
        };
        Ok(state)
    }

    fn evaluate(
        &self,
        expr: &Expression,
        environment: &mut SharedEnvironment,
        context: &mut C,
        resolution: &ResolutionMap,
    ) -> Result<LoxValue, RuntimeError> {
        self.evaluate_expression_node(expr, expr.get_root_ref(), environment, context, resolution)
    }
}

// Statement interpreter
impl TreeWalkStatementInterpreter {
    fn interpret_non_declaration<C: SystemContext>(
        &self,
        statement: &NonDeclaration,
        environment: &mut SharedEnvironment,
        context: &mut C,
        resolution: &ResolutionMap,
    ) -> Result<ProgramState, RuntimeError> {
        let state = match statement {
            NonDeclaration::Expression(stmt) => {
                self.interpret_expression_statement(environment, context, stmt, resolution)?
            }
            NonDeclaration::Print(stmt) => {
                self.interpret_print_statement(environment, context, stmt, resolution)?
            }
            NonDeclaration::Block(stmt) => {
                self.interpret_block_statement(environment, context, stmt, resolution)?
            }
            NonDeclaration::If(stmt) => {
                self.interpret_if_statement(environment, context, stmt, resolution)?
            }
            NonDeclaration::While(stmt) => {
                self.interpret_while_statement(environment, context, stmt, resolution)?
            }
            NonDeclaration::For(stmt) => {
                self.interpret_for_statement(environment, context, stmt, resolution)?
            }
            NonDeclaration::Return(stmt) => {
                self.interpret_return_statement(environment, context, stmt, resolution)?
            }
        };
        Ok(state)
    }

    fn interpret_variable_declaration<C: SystemContext>(
        &self,
        environment: &mut SharedEnvironment,
        context: &mut C,
        decl: &VariableDecl,
        resolution: &ResolutionMap,
    ) -> Result<ProgramState, RuntimeError> {
        let initial = if let Some(expr) = &decl.initial {
            self.evaluate(&expr, environment, context, resolution)?
        } else {
            LoxValue::Nil
        };
        environment.declare(&decl.name.name, initial);
        Ok(ProgramState::Run)
    }

    fn interpret_function_declaration(
        &self,
        environment: &mut SharedEnvironment,
        decl: &FunctionDecl,
    ) -> Result<ProgramState, RuntimeError> {
        let function = Function {
            name: decl.name.clone(),
            parameters: decl.parameters.to_vec(),
            body: decl.body.clone(),
            closure: environment.new_scope(),
            is_constructor: false,
        };
        environment.declare(&decl.name.name, LoxValue::Function(Arc::new(function)));
        Ok(ProgramState::Run)
    }

    fn interpret_class_declaration(
        &self,
        environment: &mut SharedEnvironment,
        decl: &ClassDecl,
        resolution: &ResolutionMap,
    ) -> Result<ProgramState, RuntimeError> {
        // Either enum
        enum BorrowedOrOwned<'a> {
            Borrowed(&'a mut SharedEnvironment),
            Owned(SharedEnvironment),
        }

        impl<'a> BorrowedOrOwned<'a> {
            pub fn new_scope(&self) -> SharedEnvironment {
                match self {
                    BorrowedOrOwned::Borrowed(environment) => environment.new_scope(),
                    BorrowedOrOwned::Owned(environment) => environment.new_scope(),
                }
            }
        }

        // Handle subclassing
        let super_class = if let Some(super_class) = &decl.super_class {
            let super_class_value = self
                .read_variable(&super_class, &environment, resolution)
                .ok_or(RuntimeError {
                    kind: RuntimeErrorKind::InvalidAccess(super_class.name.clone()),
                    span: super_class.span,
                })?;
            match super_class_value {
                LoxValue::Class(super_class) => Some(super_class),
                _ => {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::InvalidSuperClass(super_class.name.clone()),
                        span: super_class.span,
                    });
                }
            }
        } else {
            None
        };

        let name = &decl.name.name;
        environment.declare(name, LoxValue::Nil);

        let inner_environment = match super_class {
            Some(ref super_class) => {
                let mut new_environment = environment.new_scope();
                new_environment.declare("super", LoxValue::Class(super_class.clone()));
                BorrowedOrOwned::Owned(new_environment)
            }
            None => BorrowedOrOwned::Borrowed(environment),
        };

        // Create methods
        let mut methods = Vec::new();
        for decl in decl.methods.iter() {
            let is_constructor = &(*decl.name.name) == "init";
            let method = Function {
                name: decl.name.clone(),
                parameters: decl.parameters.clone(),
                body: decl.body.clone(),
                closure: inner_environment.new_scope(),
                is_constructor,
            };
            methods.push(Arc::new(method));
        }

        let class = Class {
            name: decl.name.clone(),
            methods,
            super_class,
        };
        let value = LoxValue::Class(Arc::new(class));

        // NOTE(pavyamsiri): We use the given environment as a way to exit the scope where we defined `super`.
        environment
            .assign(name, value)
            .expect("Just declared the class to exist so assignment can't fail.");
        Ok(ProgramState::Run)
    }

    fn interpret_print_statement<C: SystemContext>(
        &self,
        environment: &mut SharedEnvironment,
        context: &mut C,
        stmt: &PrintStatement,
        resolution: &ResolutionMap,
    ) -> Result<ProgramState, RuntimeError> {
        let result = self.evaluate(&stmt.expr, environment, context, resolution)?;
        context.writeln(&format!("{result}"));
        Ok(ProgramState::Run)
    }

    fn interpret_expression_statement<C: SystemContext>(
        &self,
        environment: &mut SharedEnvironment,
        context: &mut C,
        stmt: &ExpressionStatement,
        resolution: &ResolutionMap,
    ) -> Result<ProgramState, RuntimeError> {
        let _ = self.evaluate(&stmt.expr, environment, context, resolution)?;
        Ok(ProgramState::Run)
    }

    fn interpret_if_statement<C: SystemContext>(
        &self,
        environment: &mut SharedEnvironment,
        context: &mut C,
        stmt: &IfStatement,
        resolution: &ResolutionMap,
    ) -> Result<ProgramState, RuntimeError> {
        let mut state = ProgramState::Run;
        if self
            .evaluate(&stmt.condition, environment, context, resolution)?
            .is_truthy()
        {
            state =
                self.interpret_statement(stmt.success.as_ref(), environment, context, resolution)?;
        } else {
            if let Some(failure) = stmt.failure.as_ref() {
                state = self.interpret_statement(failure, environment, context, resolution)?;
            }
        }
        Ok(state)
    }

    fn interpret_while_statement<C: SystemContext>(
        &self,
        environment: &mut SharedEnvironment,
        context: &mut C,
        stmt: &WhileStatement,
        resolution: &ResolutionMap,
    ) -> Result<ProgramState, RuntimeError> {
        while self
            .evaluate(&stmt.condition, environment, context, resolution)?
            .is_truthy()
        {
            match self.interpret_statement(&stmt.body, environment, context, resolution)? {
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
        stmt: &ReturnStatement,
        resolution: &ResolutionMap,
    ) -> Result<ProgramState, RuntimeError> {
        let value = if let Some(ref expr) = stmt.value {
            self.evaluate(expr, environment, context, resolution)?
        } else {
            LoxValue::Nil
        };

        Ok(ProgramState::Return(value))
    }

    fn interpret_block_statement<C: SystemContext>(
        &self,
        environment: &mut SharedEnvironment,
        context: &mut C,
        stmt: &BlockStatement,
        resolution: &ResolutionMap,
    ) -> Result<ProgramState, RuntimeError> {
        let mut environment = environment.new_scope();
        let mut state = ProgramState::Run;
        for statement in stmt.body.iter() {
            match self.interpret_statement(statement, &mut environment, context, resolution)? {
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
        stmt: &ForStatement,
        resolution: &ResolutionMap,
    ) -> Result<ProgramState, RuntimeError> {
        // Run the initializer
        let mut environment = environment.new_scope();
        match &stmt.initializer {
            Some(Initializer::VariableDecl(decl)) => {
                self.interpret_variable_declaration(&mut environment, context, decl, resolution)?;
            }
            Some(Initializer::Expression(stmt)) => {
                self.interpret_expression_statement(&mut environment, context, stmt, resolution)?;
            }
            _ => {}
        };

        let state = 'body: loop {
            let flag = match stmt.condition {
                Option::Some(ref condition) => self
                    .evaluate(condition, &mut environment, context, resolution)?
                    .is_truthy(),
                Option::None => true,
            };

            if !flag {
                break 'body ProgramState::Run;
            }

            match self.interpret_non_declaration(
                &stmt.body,
                &mut environment,
                context,
                resolution,
            )? {
                ProgramState::Run => {}
                state @ (ProgramState::Return(_) | ProgramState::Terminate) => break 'body state,
            }
            match stmt.increment {
                Option::Some(ref increment) => {
                    self.evaluate(increment, &mut environment, context, resolution)?;
                }
                Option::None => {}
            }
        };
        Ok(state)
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
        resolution: &ResolutionMap,
    ) -> Result<LoxValue, RuntimeError> {
        let current_node = expr
            .get_node(node)
            .expect("Node ref came from the tree so it must exist.");
        let span = expr.get_span();

        let result = match current_node {
            ExpressionNode::Atom(ref atom) => self.evaluate_atom(atom, environment, resolution)?,
            ExpressionNode::Prefix { operator, rhs } => {
                let rhs =
                    self.evaluate_expression_node(expr, *rhs, environment, context, resolution)?;
                self.evaluate_prefix(*operator, &rhs)
                    .map_err(|kind| RuntimeError { kind, span })?
            }
            ExpressionNode::Group { inner } => {
                self.evaluate_expression_node(expr, *inner, environment, context, resolution)?
            }
            ExpressionNode::Infix { operator, lhs, rhs } => {
                let lhs =
                    self.evaluate_expression_node(expr, *lhs, environment, context, resolution)?;
                let rhs =
                    self.evaluate_expression_node(expr, *rhs, environment, context, resolution)?;
                self.evaluate_infix(*operator, &lhs, &rhs)
                    .map_err(|kind| RuntimeError { kind, span })?
            }
            ExpressionNode::InfixAssignment { lhs, rhs } => {
                let rhs =
                    self.evaluate_expression_node(expr, *rhs, environment, context, resolution)?;
                self.assign_variable(&lhs, rhs.clone(), environment, resolution)
                    .map_err(|kind| RuntimeError { kind, span })?;

                rhs
            }
            ExpressionNode::InfixShortCircuit { operator, lhs, rhs } => self
                .evaluate_infix_short_circuit(
                    *operator,
                    *lhs,
                    *rhs,
                    expr,
                    environment,
                    context,
                    resolution,
                )?,
            ExpressionNode::Call { callee, arguments } => {
                self.evaluate_call(*callee, arguments, expr, environment, context, resolution)?
            }
            ExpressionNode::Get { object, name } => {
                let object_value =
                    self.evaluate_expression_node(expr, *object, environment, context, resolution)?;
                let LoxValue::Instance(ref instance) = object_value else {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::InvalidInstanceGet(object_value),
                        span,
                    });
                };
                let fields = instance.fields.lock().unwrap();
                let class = &instance.class.name;

                let field_name = name;
                // Field is a property (value)
                if let Some(value) = fields.get(&field_name.to_compact_string()) {
                    value.clone()
                }
                // Field is a method
                else {
                    let class = self.read_variable(class, environment, resolution).expect("Classes bound to instances should always exist as long as the instance does.");
                    let LoxValue::Class(class) = class else {
                        panic!("Values bound as the `class` of an instance is always a class.");
                    };

                    if let Some(value) = class.find_method(&field_name.name) {
                        // Bind the instance to the method
                        let bound_method = value.bind(instance.clone());
                        LoxValue::Function(Arc::new(bound_method))
                    } else {
                        let undefined_access = RuntimeError {
                            kind: RuntimeErrorKind::UndefinedProperty {
                                object: object_value.clone(),
                                name: field_name.name.clone(),
                            },
                            span,
                        };
                        return Err(undefined_access);
                    }
                }
            }
            ExpressionNode::Set {
                object,
                name,
                value,
            } => {
                let lhs =
                    self.evaluate_expression_node(expr, *object, environment, context, resolution)?;
                let LoxValue::Instance(instance) = lhs else {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::InvalidInstanceSet(lhs),
                        span,
                    });
                };
                let rhs =
                    self.evaluate_expression_node(expr, *value, environment, context, resolution)?;
                // Grab fields after evaluating to avoid deadlocks
                let mut fields = instance.fields.lock().unwrap();
                fields.insert(name.name.to_compact_string(), rhs.clone());
                rhs
            }
        };
        Ok(result)
    }
    // Atoms
    fn evaluate_atom(
        &self,
        atom: &ExpressionAtom,
        environment: &mut SharedEnvironment,
        resolution: &ResolutionMap,
    ) -> Result<LoxValue, RuntimeError> {
        let span = atom.span;
        let result = match &atom.kind {
            ExpressionAtomKind::Number(v) => LoxValue::Number(*v),
            ExpressionAtomKind::Bool(v) => LoxValue::Bool(*v),
            ExpressionAtomKind::Nil => LoxValue::Nil,
            ExpressionAtomKind::StringLiteral(ref v) => LoxValue::String(v.to_compact_string()),
            ExpressionAtomKind::Identifier(ref name) => {
                let ident = Ident {
                    name: name.clone(),
                    span,
                };
                self.read_variable(&ident, environment, resolution)
                    .ok_or(RuntimeError {
                        kind: RuntimeErrorKind::InvalidAccess(name.clone()),
                        span,
                    })?
                    .clone()
            }
            ExpressionAtomKind::This => self
                .read_variable(
                    &Ident {
                        name: "this".into(),
                        span,
                    },
                    environment,
                    resolution,
                )
                .ok_or(RuntimeError {
                    kind: RuntimeErrorKind::InvalidAccess("this".into()),
                    span,
                })?
                .clone(),
            ExpressionAtomKind::Super(method) => {
                let super_ident = Ident {
                    name: "super".into(),
                    span,
                };
                let (super_class, depth) = self
                    .read_local(&super_ident, environment, resolution)
                    .ok_or(RuntimeError {
                    kind: RuntimeErrorKind::InvalidAccess("super".into()),
                    span,
                })?;

                // Check that this is a class
                let LoxValue::Class(super_class) = super_class else {
                    panic!("`super` should not be able to be not a class because it is a reserved word!");
                };

                let object = environment
                    .access_at("this", depth - 1)
                    .ok_or(RuntimeError {
                        kind: RuntimeErrorKind::InvalidAccess("super".into()),
                        span,
                    })?;
                let LoxValue::Instance(ref instance) = object else {
                    panic!("`this` should not be able to be not an instance because it is a reserved word!");
                };
                let method = super_class.find_method(&method).ok_or(RuntimeError {
                    kind: RuntimeErrorKind::UndefinedProperty {
                        object: object.clone(),
                        name: method.clone(),
                    },
                    span,
                })?;
                LoxValue::Function(Arc::new(method.bind(instance.clone())))
            }
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
        resolution: &ResolutionMap,
    ) -> Result<LoxValue, RuntimeError> {
        type Operator = InfixShortCircuitOperator;
        let lhs = { self.evaluate_expression_node(expr, lhs, environment, context, resolution)? };

        match operator {
            Operator::And => {
                if !lhs.is_truthy() {
                    Ok(lhs)
                } else {
                    let rhs =
                        self.evaluate_expression_node(expr, rhs, environment, context, resolution)?;
                    Ok(rhs)
                }
            }
            Operator::Or => {
                if lhs.is_truthy() {
                    Ok(lhs)
                } else {
                    let rhs =
                        self.evaluate_expression_node(expr, rhs, environment, context, resolution)?;
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
        resolution: &ResolutionMap,
    ) -> Result<LoxValue, RuntimeError> {
        let span = expr
            .get_subspan(callee)
            .expect("Caller is expected to give a valid callee node ref.");
        let callee =
            self.evaluate_expression_node(expr, callee, environment, context, resolution)?;
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
                    let argument = self.evaluate_expression_node(
                        expr,
                        *argument,
                        &mut environment,
                        context,
                        resolution,
                    )?;
                    environment.declare(name, argument);
                }

                let result = fun.call(&mut environment)?;

                result
            }
            LoxValue::Function(function) => self.evaluate_function(
                &function,
                arguments,
                &span,
                expr,
                environment,
                context,
                resolution,
            )?,
            LoxValue::Class(class) => {
                let instance = Arc::new(Instance {
                    class: class.clone(),
                    fields: Arc::new(Mutex::new(HashMap::new())),
                });
                let value = LoxValue::Instance(instance.clone());

                if let Some(constructor) = class.find_method("init") {
                    let bound_method = constructor.bind(instance);
                    self.evaluate_function(
                        &bound_method,
                        arguments,
                        &span,
                        expr,
                        environment,
                        context,
                        resolution,
                    )?;
                }

                value
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

    fn evaluate_function<C: SystemContext>(
        &self,
        function: &Function,
        arguments: &[ExpressionNodeRef],
        span: &Span,
        expr: &Expression,
        environment: &mut SharedEnvironment,
        context: &mut C,
        resolution: &ResolutionMap,
    ) -> Result<LoxValue, RuntimeError> {
        let _ = function.name;
        // Set up scope
        let mut inner_scope = function.closure.new_scope();

        // Check that the argument list is the same length as the parameter list.
        if arguments.len() != function.parameters.len() {
            return Err(RuntimeError {
                kind: RuntimeErrorKind::InvalidArgumentCount {
                    actual: arguments.len(),
                    expected: function.parameters.len(),
                },
                span: span.clone(),
            });
        }

        // Define the arguments in the function scope
        for (ident, argument) in function.parameters.iter().zip(arguments.iter()) {
            let argument =
                self.evaluate_expression_node(expr, *argument, environment, context, resolution)?;
            inner_scope.declare(&ident.name, argument);
        }

        let mut result = LoxValue::Nil;
        for statement in function.body.iter() {
            match self.interpret_statement(&statement, &mut inner_scope, context, resolution)? {
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
        if function.is_constructor {
            Ok(function
                .closure
                .access_at("this", 0)
                .expect("`this` should be bound to the method."))
        } else {
            Ok(result)
        }
    }
}

impl TreeWalkStatementInterpreter {
    fn read_variable(
        &self,
        ident: &Ident,
        environment: &SharedEnvironment,
        resolution: &ResolutionMap,
    ) -> Option<LoxValue> {
        match resolution.get(ident) {
            Some(depth) => environment.access_at(&ident.name, *depth),
            None => environment.access_global(&ident.name),
        }
    }

    fn read_local(
        &self,
        ident: &Ident,
        environment: &SharedEnvironment,
        resolution: &ResolutionMap,
    ) -> Option<(LoxValue, usize)> {
        match resolution.get(ident) {
            Some(depth) => environment
                .access_at(&ident.name, *depth)
                .map(|v| (v, *depth)),
            None => None,
        }
    }

    fn assign_variable(
        &self,
        ident: &Ident,
        value: LoxValue,
        environment: &mut SharedEnvironment,
        resolution: &ResolutionMap,
    ) -> Result<(), RuntimeErrorKind> {
        let result = match resolution.get(ident) {
            Some(depth) => environment.assign_at(&ident.name, value, *depth),
            None => environment.assign_global(&ident.name, value),
        };
        result.map_err(|_| RuntimeErrorKind::InvalidAccess(ident.name.clone()))
    }
}
