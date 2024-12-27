mod error;

use crate::{
    lexer::Span,
    parser::{
        expression::{
            Expression, ExpressionAtom, ExpressionAtomKind, ExpressionNode, ExpressionNodeRef,
        },
        statement::{Declaration, Initializer, NonDeclaration, Statement},
        Program,
    },
    string::IdentifierString,
};
pub use error::{ResolutionError, ResolutionErrorKind};
use std::collections::HashMap;

enum Resolution {
    Declared,
    Defined,
}

#[derive(Clone, Copy)]
enum FunctionEnvironment {
    None,
    Function,
}

pub struct Resolver {
    resolution: HashMap<Span, usize>,
    function: FunctionEnvironment,
    scopes: Vec<HashMap<IdentifierString, Resolution>>,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            resolution: HashMap::new(),
            scopes: Vec::new(),
            function: FunctionEnvironment::None,
        }
    }

    pub fn resolve_program(
        mut self,
        program: &Program,
    ) -> Result<HashMap<Span, usize>, ResolutionError> {
        for index in 0..program.len() {
            let statement = program
                .get_statement(index)
                .expect("Iterating over valid indices.");
            self.resolve_statement(statement)?;
        }
        Ok(self.resolution)
    }

    pub fn resolve_expression_and_consume(
        mut self,
        expr: &Expression,
    ) -> Result<HashMap<Span, usize>, ResolutionError> {
        self.resolve_expression(expr)?;
        Ok(self.resolution)
    }

    fn resolve_statement(&mut self, statement: &Statement) -> Result<(), ResolutionError> {
        match statement {
            Statement::Declaration(declaration) => {
                self.resolve_declaration(declaration)?;
            }
            Statement::NonDeclaration(non_declaration) => {
                self.resolve_non_declaration(non_declaration)?;
            }
        }
        Ok(())
    }
}

// Base
impl Resolver {
    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &IdentifierString, span: &Span) -> Result<(), ResolutionError> {
        if let Some(inner_scope) = self.scopes.last_mut() {
            if inner_scope.contains_key(name) {
                return Err(ResolutionError {
                    kind: ResolutionErrorKind::ShadowLocal,
                    span: span.clone(),
                });
            }

            inner_scope.insert(name.clone(), Resolution::Declared);
        }
        Ok(())
    }

    fn define(&mut self, name: &IdentifierString) {
        if let Some(inner_scope) = self.scopes.last_mut() {
            inner_scope.insert(name.clone(), Resolution::Defined);
        }
    }

    fn get_resolution(&self, name: &IdentifierString) -> Option<&Resolution> {
        if let Some(inner_scope) = self.scopes.last() {
            inner_scope.get(name)
        } else {
            None
        }
    }

    fn resolve_variable(&mut self, name: &IdentifierString, span: &Span) {
        let total_depth = self.scopes.len();
        for (depth, scope) in self.scopes.iter_mut().enumerate().rev() {
            if let Some(Resolution::Defined) = scope.get(name) {
                self.resolution
                    .insert(span.clone(), total_depth - 1 - depth);
            }
        }
    }
}

// Implementation
impl Resolver {
    fn resolve_declaration(&mut self, declaration: &Declaration) -> Result<(), ResolutionError> {
        match declaration {
            Declaration::Variable { name, initial } => {
                self.resolve_variable_declaration(name, initial.as_ref())?;
            }
            Declaration::Function {
                name,
                parameters,
                body,
            } => {
                self.resolve_function_declaration(name, parameters, body)?;
            }
        }
        Ok(())
    }

    fn resolve_variable_declaration(
        &mut self,
        name: &IdentifierString,
        initializer: Option<&Expression>,
    ) -> Result<(), ResolutionError> {
        // TODO(pavyamsiri): Fix the incorrect span. Blocked on statements not having spans.
        self.declare(
            &name,
            &Span {
                start: 0.into(),
                length: 0.into(),
            },
        )?;
        if let Some(initializer) = initializer {
            self.resolve_expression(initializer)?;
        }
        self.define(&name);
        Ok(())
    }

    fn resolve_function_declaration(
        &mut self,
        name: &IdentifierString,
        parameters: &[IdentifierString],
        body: &[Statement],
    ) -> Result<(), ResolutionError> {
        // TODO(pavyamsiri): Fix the incorrect span. Blocked on statements not having spans.
        self.declare(
            &name,
            &Span {
                start: 0.into(),
                length: 0.into(),
            },
        )?;
        self.define(name);
        self.resolve_function(parameters, body, FunctionEnvironment::Function)?;

        Ok(())
    }

    fn resolve_function(
        &mut self,
        parameters: &[IdentifierString],
        body: &[Statement],
        environment: FunctionEnvironment,
    ) -> Result<(), ResolutionError> {
        let enclosing = self.function;
        self.function = environment;
        self.enter_scope();
        for param in parameters {
            // TODO(pavyamsiri): Fix the incorrect span. Blocked on statements not having spans.
            self.declare(
                &param,
                &Span {
                    start: 0.into(),
                    length: 0.into(),
                },
            )?;
            self.define(param);
        }
        for statement in body {
            self.resolve_statement(statement)?;
        }
        self.exit_scope();
        self.function = enclosing;
        Ok(())
    }
}

// Non-declarations
impl Resolver {
    fn resolve_non_declaration(
        &mut self,
        statement: &NonDeclaration,
    ) -> Result<(), ResolutionError> {
        match statement {
            NonDeclaration::Expression(expression) => self.resolve_expression(expression)?,
            NonDeclaration::Print(expression) => self.resolve_expression(expression)?,
            NonDeclaration::Block(statements) => {
                self.enter_scope();
                for statement in statements {
                    self.resolve_statement(statement)?;
                }
                self.exit_scope();
            }
            NonDeclaration::If {
                condition,
                success,
                failure,
            } => {
                self.resolve_if_statement(condition, success, failure.as_ref().as_ref())?;
            }
            NonDeclaration::While { condition, body } => {
                self.resolve_while_statement(condition, body)?;
            }
            NonDeclaration::For {
                initializer,
                condition,
                increment,
                body,
            } => {
                self.resolve_for_statement(
                    initializer.as_ref(),
                    condition.as_ref(),
                    increment.as_ref(),
                    body,
                )?;
            }
            NonDeclaration::Return { value } => {
                self.resolve_return_statement(value.as_ref())?;
            }
        }
        Ok(())
    }

    fn resolve_if_statement(
        &mut self,
        condition: &Expression,
        success: &Statement,
        failure: Option<&Statement>,
    ) -> Result<(), ResolutionError> {
        self.resolve_expression(condition)?;
        self.resolve_statement(success)?;
        if let Some(failure) = failure {
            self.resolve_statement(failure)?;
        }
        Ok(())
    }

    fn resolve_while_statement(
        &mut self,
        condition: &Expression,
        body: &Statement,
    ) -> Result<(), ResolutionError> {
        self.resolve_expression(condition)?;
        self.resolve_statement(body)?;
        Ok(())
    }

    fn resolve_for_statement(
        &mut self,
        initializer: Option<&Initializer>,
        condition: Option<&Expression>,
        increment: Option<&Expression>,
        body: &NonDeclaration,
    ) -> Result<(), ResolutionError> {
        if let Some(initializer) = initializer {
            match initializer {
                Initializer::VarDecl { name, initial } => {
                    self.resolve_variable_declaration(name, initial.as_ref())?;
                }
                Initializer::Expression(expression) => {
                    self.resolve_expression(expression)?;
                }
            }
        }
        if let Some(condition) = condition {
            self.resolve_expression(condition)?;
        }
        if let Some(increment) = increment {
            self.resolve_expression(increment)?;
        }
        self.resolve_non_declaration(body)?;
        Ok(())
    }

    fn resolve_return_statement(
        &mut self,
        expr: Option<&Expression>,
    ) -> Result<(), ResolutionError> {
        if matches!(self.function, FunctionEnvironment::None) {
            // TODO(pavyamsiri): Fix the incorrect span. Blocked on statements not having spans.
            return Err(ResolutionError {
                kind: ResolutionErrorKind::NonFunctionReturn,
                span: Span {
                    start: 0.into(),
                    length: 0.into(),
                },
            });
        }

        if let Some(expr) = expr {
            self.resolve_expression(expr)?;
        }
        Ok(())
    }
}

// Expressions
impl Resolver {
    fn resolve_expression(&mut self, expr: &Expression) -> Result<(), ResolutionError> {
        self.resolve_expression_node(expr, expr.get_root_ref())
    }

    fn resolve_expression_node(
        &mut self,
        expr: &Expression,
        node_ref: ExpressionNodeRef,
    ) -> Result<(), ResolutionError> {
        let node = expr
            .get_node(node_ref)
            .expect("Caller must provide a valid reference");
        match node {
            ExpressionNode::Atom(atom) => {
                self.resolve_atom(atom)?;
            }
            ExpressionNode::Group { inner } => {
                self.resolve_expression_node(expr, inner.clone())?;
            }
            ExpressionNode::Prefix { rhs, .. } => {
                self.resolve_expression_node(expr, rhs.clone())?;
            }
            ExpressionNode::Infix { lhs, rhs, .. } => {
                self.resolve_expression_node(expr, lhs.clone())?;
                self.resolve_expression_node(expr, rhs.clone())?;
            }
            ExpressionNode::InfixAssignment { lhs, rhs } => {
                self.resolve_expression_node(expr, rhs.clone())?;
                self.resolve_variable(&lhs.name, &lhs.span);
            }
            ExpressionNode::InfixShortCircuit { lhs, rhs, .. } => {
                self.resolve_expression_node(expr, lhs.clone())?;
                self.resolve_expression_node(expr, rhs.clone())?;
            }
            ExpressionNode::Call { callee, arguments } => {
                self.resolve_expression_node(expr, callee.clone())?;
                for argument in arguments {
                    self.resolve_expression_node(expr, argument.clone())?;
                }
            }
        }
        Ok(())
    }

    fn resolve_atom(&mut self, atom: &ExpressionAtom) -> Result<(), ResolutionError> {
        match &atom.kind {
            ExpressionAtomKind::Identifier(name) => {
                self.resolve_variable_expression(name, &atom.span)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn resolve_variable_expression(
        &mut self,
        name: &IdentifierString,
        span: &Span,
    ) -> Result<(), ResolutionError> {
        // Can't access variable when it is declared but not defined
        if let Some(Resolution::Declared) = self.get_resolution(name) {
            Err(ResolutionError {
                kind: ResolutionErrorKind::SelfReferentialInitializer,
                span: span.clone(),
            })
        } else {
            self.resolve_variable(name, span);
            Ok(())
        }
    }
}
