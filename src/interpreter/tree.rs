use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use compact_str::ToCompactString;

use super::SystemContext;
use crate::environment::SharedEnvironment;
use crate::lexer::Span;
use crate::parser::statement::{
    BlockStatement, ClassDecl, ExpressionStatement, ForStatement, FunctionDecl, IfStatement,
    PrintStatement, ReturnStatement, VariableDecl, WhileStatement,
};
use crate::resolver::ResolvedProgram;
use crate::value::error::{RuntimeError, RuntimeErrorKind};
use crate::value::{Class, Function, Instance, LoxValue};
use crate::{
    parser::{
        expression::{
            Expression, ExpressionAtom, ExpressionAtomKind, ExpressionNode, ExpressionNodeRef,
            InfixOperator, InfixShortCircuitOperator, PrefixOperator,
        },
        statement::{Initializer, NonDeclaration, Statement},
    },
    string::Ident,
};

#[derive(Debug)]
enum ProgramState {
    Run,
    Return(LoxValue),
}

pub struct TreeWalkInterpreter<C: SystemContext> {
    environment: SharedEnvironment,
    context: C,
    interpreter: TreeWalkStatementInterpreter,
}

impl<C> TreeWalkInterpreter<C>
where
    C: SystemContext,
{
    pub fn new(context: C) -> Self {
        Self {
            environment: SharedEnvironment::new(),
            context,
            interpreter: TreeWalkStatementInterpreter::create(),
        }
    }

    pub fn run(mut self, program: &ResolvedProgram) -> Result<C, RuntimeError> {
        for stmt in program.iter() {
            self.interpreter.interpret_statement(
                program,
                &mut self.environment,
                &mut self.context,
                stmt,
            )?;
        }
        Ok(self.context)
    }
}

pub struct TreeWalkStatementInterpreter;

impl TreeWalkStatementInterpreter {
    fn create() -> Self {
        Self {}
    }

    fn interpret_statement<'stmt, C: SystemContext>(
        &self,
        program: &ResolvedProgram,
        environment: &mut SharedEnvironment,
        context: &mut C,
        statement: Statement<'stmt>,
    ) -> Result<ProgramState, RuntimeError> {
        match statement {
            Statement::VariableDecl(decl) => {
                self.interpret_variable_declaration(program, environment, context, decl)
            }
            Statement::FunctionDecl(decl) => {
                self.interpret_function_declaration(program, environment, context, decl)
            }
            Statement::ClassDecl(decl) => {
                self.interpret_class_declaration(program, environment, context, decl)
            }
            Statement::Expression(stmt) => {
                self.interpret_expression_statement(program, environment, context, stmt)
            }
            Statement::Print(stmt) => {
                self.interpret_print_statement(program, environment, context, stmt)
            }
            Statement::Block(stmt) => {
                self.interpret_block_statement(program, environment, context, stmt)
            }
            Statement::If(stmt) => self.interpret_if_statement(program, environment, context, stmt),
            Statement::While(stmt) => {
                self.interpret_while_statement(program, environment, context, stmt)
            }
            Statement::For(stmt) => {
                self.interpret_for_statement(program, environment, context, stmt)
            }
            Statement::Return(stmt) => {
                self.interpret_return_statement(program, environment, context, stmt)
            }
        }
    }

    pub fn evaluate<C: SystemContext>(
        &self,
        program: &ResolvedProgram,
        environment: &mut SharedEnvironment,
        context: &mut C,
        expr: &Expression,
    ) -> Result<LoxValue, RuntimeError> {
        self.evaluate_expression_node(program, environment, context, expr, expr.get_root_ref())
    }
}

// Statement interpreter
impl TreeWalkStatementInterpreter {
    fn interpret_non_declaration<'stmt, C: SystemContext>(
        &self,
        program: &ResolvedProgram,
        environment: &mut SharedEnvironment,
        context: &mut C,
        statement: NonDeclaration<'stmt>,
    ) -> Result<ProgramState, RuntimeError> {
        match statement {
            NonDeclaration::Expression(stmt) => {
                self.interpret_expression_statement(program, environment, context, stmt)
            }
            NonDeclaration::Print(stmt) => {
                self.interpret_print_statement(program, environment, context, stmt)
            }
            NonDeclaration::Block(stmt) => {
                self.interpret_block_statement(program, environment, context, stmt)
            }
            NonDeclaration::If(stmt) => {
                self.interpret_if_statement(program, environment, context, stmt)
            }
            NonDeclaration::While(stmt) => {
                self.interpret_while_statement(program, environment, context, stmt)
            }
            NonDeclaration::For(stmt) => {
                self.interpret_for_statement(program, environment, context, stmt)
            }
            NonDeclaration::Return(stmt) => {
                self.interpret_return_statement(program, environment, context, stmt)
            }
        }
    }

    fn interpret_variable_declaration<C: SystemContext>(
        &self,
        program: &ResolvedProgram,
        environment: &mut SharedEnvironment,
        context: &mut C,
        decl: &VariableDecl,
    ) -> Result<ProgramState, RuntimeError> {
        let initial = if let Some(expr) = &decl.initial {
            self.evaluate(program, environment, context, &expr)?
        } else {
            LoxValue::Nil
        };
        environment.declare(&decl.name.name, initial);
        Ok(ProgramState::Run)
    }

    fn interpret_function_declaration<C: SystemContext>(
        &self,
        program: &ResolvedProgram,
        environment: &mut SharedEnvironment,
        context: &mut C,
        decl: &FunctionDecl,
    ) -> Result<ProgramState, RuntimeError> {
        let _ = program;
        let _ = context;
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

    fn interpret_class_declaration<C: SystemContext>(
        &self,
        program: &ResolvedProgram,
        environment: &mut SharedEnvironment,
        context: &mut C,
        decl: &ClassDecl,
    ) -> Result<ProgramState, RuntimeError> {
        let _ = context;
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
                .read_variable(program, environment, &super_class)
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

    fn interpret_expression_statement<C: SystemContext>(
        &self,
        program: &ResolvedProgram,
        environment: &mut SharedEnvironment,
        context: &mut C,
        stmt: &ExpressionStatement,
    ) -> Result<ProgramState, RuntimeError> {
        let _ = self.evaluate(program, environment, context, &stmt.expr)?;
        Ok(ProgramState::Run)
    }

    fn interpret_print_statement<C: SystemContext>(
        &self,
        program: &ResolvedProgram,
        environment: &mut SharedEnvironment,
        context: &mut C,
        stmt: &PrintStatement,
    ) -> Result<ProgramState, RuntimeError> {
        let result = self.evaluate(program, environment, context, &stmt.expr)?;
        context.writeln(&format!("{result}"));
        Ok(ProgramState::Run)
    }

    fn interpret_block_statement<C: SystemContext>(
        &self,
        program: &ResolvedProgram,
        environment: &mut SharedEnvironment,
        context: &mut C,
        block: &BlockStatement,
    ) -> Result<ProgramState, RuntimeError> {
        const MSG: &'static str = "[Intepret Block]: All handles are valid.";
        let mut environment = environment.new_scope();
        let mut state = ProgramState::Run;
        for stmt in block.body.iter() {
            let stmt = program.get_statement(*stmt).expect(MSG);
            match self.interpret_statement(program, &mut environment, context, stmt)? {
                ProgramState::Run => {}
                s => {
                    state = s;
                    break;
                }
            }
        }
        Ok(state)
    }
    fn interpret_if_statement<C: SystemContext>(
        &self,
        program: &ResolvedProgram,
        environment: &mut SharedEnvironment,
        context: &mut C,
        stmt: &IfStatement,
    ) -> Result<ProgramState, RuntimeError> {
        const MSG: &'static str = "[Interpret If]: All handles are valid";
        let mut state = ProgramState::Run;
        if self
            .evaluate(program, environment, context, &stmt.condition)?
            .is_truthy()
        {
            let success = program.get_statement(stmt.success).expect(MSG);
            state = self.interpret_statement(program, environment, context, success)?;
        } else {
            if let Some(failure) = stmt.failure {
                let failure = program.get_statement(failure).expect(MSG);
                state = self.interpret_statement(program, environment, context, failure)?;
            }
        }
        Ok(state)
    }

    fn interpret_while_statement<C: SystemContext>(
        &self,
        program: &ResolvedProgram,
        environment: &mut SharedEnvironment,
        context: &mut C,
        stmt: &WhileStatement,
    ) -> Result<ProgramState, RuntimeError> {
        const MSG: &'static str = "[Interpret While]: All handles are valid";
        let body = program.get_non_declaration(stmt.body).expect(MSG);
        while self
            .evaluate(program, environment, context, &stmt.condition)?
            .is_truthy()
        {
            match self.interpret_non_declaration(program, environment, context, body.clone())? {
                ProgramState::Run => {}
                s => {
                    return Ok(s);
                }
            }
        }

        Ok(ProgramState::Run)
    }

    fn interpret_for_statement<C: SystemContext>(
        &self,
        program: &ResolvedProgram,
        environment: &mut SharedEnvironment,
        context: &mut C,
        stmt: &ForStatement,
    ) -> Result<ProgramState, RuntimeError> {
        const MSG: &'static str = "[Interpret For]: All handles are valid.";
        // Run the initializer
        let mut environment = environment.new_scope();
        match &stmt.initializer {
            Some(Initializer::VariableDecl(decl)) => {
                let decl = program.get_variable_decl(*decl).expect(MSG);
                self.interpret_variable_declaration(program, &mut environment, context, decl)?;
            }
            Some(Initializer::Expression(stmt)) => {
                let stmt = program.get_expression_stmt(*stmt).expect(MSG);
                self.interpret_expression_statement(program, &mut environment, context, stmt)?;
            }
            _ => {}
        };

        let body = program.get_non_declaration(stmt.body).expect(MSG);
        loop {
            let flag = match stmt.condition {
                Option::Some(ref condition) => self
                    .evaluate(program, &mut environment, context, condition)?
                    .is_truthy(),
                Option::None => true,
            };

            if !flag {
                return Ok(ProgramState::Run);
            }

            // Run body
            match self.interpret_non_declaration(
                program,
                &mut environment,
                context,
                body.clone(),
            )? {
                ProgramState::Run => {}
                state @ ProgramState::Return(_) => return Ok(state),
            }

            // Increment
            match stmt.increment {
                Option::Some(ref increment) => {
                    self.evaluate(program, &mut environment, context, increment)?;
                }
                Option::None => {}
            }
        }
    }

    fn interpret_return_statement<C: SystemContext>(
        &self,
        program: &ResolvedProgram,
        environment: &mut SharedEnvironment,
        context: &mut C,
        stmt: &ReturnStatement,
    ) -> Result<ProgramState, RuntimeError> {
        let value = if let Some(ref expr) = stmt.value {
            self.evaluate(program, environment, context, expr)?
        } else {
            LoxValue::Nil
        };

        Ok(ProgramState::Return(value))
    }
}

// Expression evaluator
impl TreeWalkStatementInterpreter {
    fn evaluate_expression_node<C: SystemContext>(
        &self,
        program: &ResolvedProgram,
        environment: &mut SharedEnvironment,
        context: &mut C,
        expr: &Expression,
        node: ExpressionNodeRef,
    ) -> Result<LoxValue, RuntimeError> {
        let current_node = expr
            .get_node(node)
            .expect("Node ref came from the tree so it must exist.");
        let span = expr.get_span();

        let result = match current_node {
            ExpressionNode::Atom(ref atom) => {
                self.evaluate_atom(program, environment, context, atom)?
            }
            ExpressionNode::Prefix { operator, rhs } => {
                let rhs =
                    self.evaluate_expression_node(program, environment, context, expr, *rhs)?;
                self.evaluate_prefix(*operator, &rhs)
                    .map_err(|kind| RuntimeError { kind, span })?
            }
            ExpressionNode::Group { inner } => {
                self.evaluate_expression_node(program, environment, context, expr, *inner)?
            }
            ExpressionNode::Infix { operator, lhs, rhs } => {
                let lhs =
                    self.evaluate_expression_node(program, environment, context, expr, *lhs)?;
                let rhs =
                    self.evaluate_expression_node(program, environment, context, expr, *rhs)?;
                self.evaluate_infix(*operator, &lhs, &rhs)
                    .map_err(|kind| RuntimeError { kind, span })?
            }
            ExpressionNode::InfixAssignment { lhs, rhs } => {
                let rhs =
                    self.evaluate_expression_node(program, environment, context, expr, *rhs)?;
                self.assign_variable(program, environment, &lhs, rhs.clone())
                    .map_err(|kind| RuntimeError { kind, span })?;

                rhs
            }
            ExpressionNode::InfixShortCircuit { operator, lhs, rhs } => self
                .evaluate_infix_short_circuit(
                    program,
                    environment,
                    context,
                    expr,
                    *operator,
                    *lhs,
                    *rhs,
                )?,
            ExpressionNode::Call { callee, arguments } => {
                self.evaluate_call(program, environment, context, expr, *callee, arguments)?
            }
            ExpressionNode::Get { object, name } => {
                let object_value =
                    self.evaluate_expression_node(program, environment, context, expr, *object)?;
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
                    let class = self.read_variable(program, &environment, class).expect("Classes bound to instances should always exist as long as the instance does.");
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
                    self.evaluate_expression_node(program, environment, context, expr, *object)?;
                let LoxValue::Instance(instance) = lhs else {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::InvalidInstanceSet(lhs),
                        span,
                    });
                };
                let rhs =
                    self.evaluate_expression_node(program, environment, context, expr, *value)?;
                // Grab fields after evaluating to avoid deadlocks
                let mut fields = instance.fields.lock().unwrap();
                fields.insert(name.name.to_compact_string(), rhs.clone());
                rhs
            }
        };
        Ok(result)
    }
    // Atoms
    fn evaluate_atom<C: SystemContext>(
        &self,
        program: &ResolvedProgram,
        environment: &SharedEnvironment,
        context: &mut C,
        atom: &ExpressionAtom,
    ) -> Result<LoxValue, RuntimeError> {
        let _ = context;
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
                self.read_variable(program, environment, &ident)
                    .ok_or(RuntimeError {
                        kind: RuntimeErrorKind::InvalidAccess(name.clone()),
                        span,
                    })?
                    .clone()
            }
            ExpressionAtomKind::This => self
                .read_variable(
                    program,
                    environment,
                    &Ident {
                        name: "this".into(),
                        span,
                    },
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
                    .read_local(program, environment, &super_ident)
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
        program: &ResolvedProgram,
        environment: &mut SharedEnvironment,
        context: &mut C,
        expr: &Expression,
        operator: InfixShortCircuitOperator,
        lhs: ExpressionNodeRef,
        rhs: ExpressionNodeRef,
    ) -> Result<LoxValue, RuntimeError> {
        type Operator = InfixShortCircuitOperator;
        let lhs = { self.evaluate_expression_node(program, environment, context, expr, lhs)? };

        match operator {
            Operator::And => {
                if !lhs.is_truthy() {
                    Ok(lhs)
                } else {
                    let rhs =
                        self.evaluate_expression_node(program, environment, context, expr, rhs)?;
                    Ok(rhs)
                }
            }
            Operator::Or => {
                if lhs.is_truthy() {
                    Ok(lhs)
                } else {
                    let rhs =
                        self.evaluate_expression_node(program, environment, context, expr, rhs)?;
                    Ok(rhs)
                }
            }
        }
    }

    fn evaluate_call<C: SystemContext>(
        &self,
        program: &ResolvedProgram,
        environment: &mut SharedEnvironment,
        context: &mut C,
        expr: &Expression,
        callee: ExpressionNodeRef,
        arguments: &[ExpressionNodeRef],
    ) -> Result<LoxValue, RuntimeError> {
        let span = expr
            .get_subspan(callee)
            .expect("Caller is expected to give a valid callee node ref.");
        let callee = self.evaluate_expression_node(program, environment, context, expr, callee)?;
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
                        program,
                        &mut environment,
                        context,
                        expr,
                        *argument,
                    )?;
                    environment.declare(name, argument);
                }

                let result = fun.call(&mut environment)?;

                result
            }
            LoxValue::Function(function) => self.evaluate_function(
                program,
                environment,
                context,
                expr,
                &function,
                arguments,
                &span,
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
                        program,
                        environment,
                        context,
                        expr,
                        &bound_method,
                        arguments,
                        &span,
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
        program: &ResolvedProgram,
        environment: &mut SharedEnvironment,
        context: &mut C,
        expr: &Expression,
        function: &Function,
        arguments: &[ExpressionNodeRef],
        span: &Span,
    ) -> Result<LoxValue, RuntimeError> {
        const MSG: &'static str = "[Evaluate Function]: All handles are valid.";
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
                self.evaluate_expression_node(program, environment, context, expr, *argument)?;
            inner_scope.declare(&ident.name, argument);
        }

        let mut result = LoxValue::Nil;
        let body = program.get_block_stmt(function.body).expect(MSG);
        for stmt in body.iter() {
            let stmt = program.get_statement(stmt).expect(MSG);
            match self.interpret_statement(program, &mut inner_scope, context, stmt)? {
                ProgramState::Run => {}
                ProgramState::Return(v) => {
                    result = v;
                    break;
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
        program: &ResolvedProgram,
        environment: &SharedEnvironment,
        ident: &Ident,
    ) -> Option<LoxValue> {
        match program.get_resolution().get(ident) {
            Some(depth) => environment.access_at(&ident.name, *depth),
            None => environment.access_global(&ident.name),
        }
    }

    fn read_local(
        &self,
        program: &ResolvedProgram,
        environment: &SharedEnvironment,
        ident: &Ident,
    ) -> Option<(LoxValue, usize)> {
        match program.get_resolution().get(ident) {
            Some(depth) => environment
                .access_at(&ident.name, *depth)
                .map(|v| (v, *depth)),
            None => None,
        }
    }

    fn assign_variable(
        &self,
        program: &ResolvedProgram,
        environment: &mut SharedEnvironment,
        ident: &Ident,
        value: LoxValue,
    ) -> Result<(), RuntimeErrorKind> {
        let result = match program.get_resolution().get(ident) {
            Some(depth) => environment.assign_at(&ident.name, value, *depth),
            None => environment.assign_global(&ident.name, value),
        };
        result.map_err(|_| RuntimeErrorKind::InvalidAccess(ident.name.clone()))
    }
}
