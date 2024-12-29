mod error;
pub mod formatter;

use crate::{
    lexer::Span,
    parser::{
        expression::{
            Expression, ExpressionAtom, ExpressionAtomKind, ExpressionNode, ExpressionNodeRef,
        },
        statement::{
            Declaration, DeclarationKind, FunctionDecl, Initializer, NonDeclaration,
            NonDeclarationKind, Statement,
        },
        Program,
    },
    string::{Ident, IdentName},
};
pub use error::{ResolutionError, ResolutionErrorKind};
use std::collections::HashMap;

pub type ResolutionMap = HashMap<Ident, usize>;

enum Resolution {
    Declared { name: Ident, span: Span },
    Defined { name: Ident, span: Span },
}

impl Resolution {
    pub fn get_ident(&self) -> &Ident {
        match self {
            Resolution::Declared { name, .. } => name,
            Resolution::Defined { name, .. } => name,
        }
    }

    pub fn get_span(&self) -> &Span {
        match self {
            Resolution::Declared { span, .. } => span,
            Resolution::Defined { span, .. } => span,
        }
    }
}

#[derive(Clone, Copy)]
enum FunctionEnvironment {
    None,
    Function,
    Method,
}

#[derive(Clone, Copy)]
enum ClassEnvironment {
    None,
    Class,
}

pub struct Resolver {
    resolution: ResolutionMap,
    function: FunctionEnvironment,
    class: ClassEnvironment,
    scopes: Vec<HashMap<IdentName, Resolution>>,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            resolution: HashMap::new(),
            scopes: Vec::new(),
            function: FunctionEnvironment::None,
            class: ClassEnvironment::None,
        }
    }

    pub fn resolve_program(mut self, program: &Program) -> Result<ResolutionMap, ResolutionError> {
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
    ) -> Result<ResolutionMap, ResolutionError> {
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

    /// Takes another span separate to ident that is the span of the declaration statement itself.
    fn declare(&mut self, ident: &Ident, span: &Span) -> Result<(), ResolutionError> {
        if let Some(inner_scope) = self.scopes.last_mut() {
            if let Some(resolution) = inner_scope.get(&ident.name) {
                let old_ident = resolution.get_ident();
                let old_span = resolution.get_span();
                if inner_scope.contains_key(&ident.name) {
                    let encompassing = old_span.merge(span);
                    return Err(ResolutionError {
                        kind: ResolutionErrorKind::ShadowLocal {
                            old: old_ident.clone(),
                            new: ident.clone(),
                        },
                        span: encompassing,
                    });
                }
            }

            inner_scope.insert(
                ident.name.clone(),
                Resolution::Declared {
                    name: ident.clone(),
                    span: span.clone(),
                },
            );
        }
        Ok(())
    }

    fn define(&mut self, ident: &Ident, span: &Span) {
        if let Some(inner_scope) = self.scopes.last_mut() {
            inner_scope.insert(
                ident.name.clone(),
                Resolution::Defined {
                    name: ident.clone(),
                    span: span.clone(),
                },
            );
        }
    }

    fn get_resolution(&self, ident: &Ident) -> Option<&Resolution> {
        if let Some(inner_scope) = self.scopes.last() {
            inner_scope.get(&ident.name)
        } else {
            None
        }
    }

    fn resolve_variable(&mut self, ident: &Ident) {
        let total_depth = self.scopes.len();
        for (depth, scope) in self.scopes.iter_mut().enumerate().rev() {
            if let Some(Resolution::Defined { .. }) = scope.get(&ident.name) {
                self.resolution
                    .insert(ident.clone(), total_depth - 1 - depth);
            }
        }
    }
}

// Implementation
impl Resolver {
    fn resolve_declaration(&mut self, decl: &Declaration) -> Result<(), ResolutionError> {
        match &decl.kind {
            DeclarationKind::Variable { name, initial } => {
                self.resolve_variable_declaration(name, &decl.span, initial.as_ref())?;
            }
            DeclarationKind::Function(FunctionDecl {
                name,
                parameters,
                body,
            }) => {
                self.resolve_function_declaration(name, &decl.span, parameters, body)?;
            }
            DeclarationKind::Class { name, methods } => {
                self.resolve_class_declaration(name, &decl.span, methods)?;
            }
        }
        Ok(())
    }

    fn resolve_variable_declaration(
        &mut self,
        ident: &Ident,
        span: &Span,
        initializer: Option<&Expression>,
    ) -> Result<(), ResolutionError> {
        self.declare(ident, span)?;
        if let Some(initializer) = initializer {
            self.resolve_expression(initializer)?;
        }
        self.define(ident, span);
        Ok(())
    }

    fn resolve_function_declaration(
        &mut self,
        ident: &Ident,
        span: &Span,
        parameters: &[Ident],
        body: &[Statement],
    ) -> Result<(), ResolutionError> {
        self.declare(ident, span)?;
        self.define(ident, span);
        self.resolve_function(parameters, body, FunctionEnvironment::Function)?;

        Ok(())
    }

    fn resolve_class_declaration(
        &mut self,
        ident: &Ident,
        span: &Span,
        methods: &[FunctionDecl],
    ) -> Result<(), ResolutionError> {
        let enclosing = self.class;
        self.class = ClassEnvironment::Class;

        self.declare(ident, span)?;
        self.define(ident, span);

        self.enter_scope();
        let this_ident = Ident {
            name: "this".into(),
            span: span.clone(),
        };
        self.declare(&this_ident, span)?;
        self.define(&this_ident, span);
        for decl in methods {
            self.resolve_function(&decl.parameters, &decl.body, FunctionEnvironment::Method)?
        }
        self.exit_scope();

        self.class = enclosing;
        Ok(())
    }

    fn resolve_function(
        &mut self,
        parameters: &[Ident],
        body: &[Statement],
        environment: FunctionEnvironment,
    ) -> Result<(), ResolutionError> {
        let enclosing = self.function;
        self.function = environment;
        self.enter_scope();
        for param in parameters {
            self.declare(param, &param.span)?;
            self.define(param, &param.span);
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
    fn resolve_non_declaration(&mut self, stmt: &NonDeclaration) -> Result<(), ResolutionError> {
        match &stmt.kind {
            NonDeclarationKind::Expression(expression) => self.resolve_expression(expression)?,
            NonDeclarationKind::Print(expression) => self.resolve_expression(expression)?,
            NonDeclarationKind::Block(statements) => {
                self.enter_scope();
                for statement in statements {
                    self.resolve_statement(statement)?;
                }
                self.exit_scope();
            }
            NonDeclarationKind::If {
                condition,
                success,
                failure,
            } => {
                self.resolve_if_statement(condition, success, failure.as_ref().as_ref())?;
            }
            NonDeclarationKind::While { condition, body } => {
                self.resolve_while_statement(condition, body)?;
            }
            NonDeclarationKind::For {
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
            NonDeclarationKind::Return { value } => {
                self.resolve_return_statement(value.as_ref(), &stmt.span)?;
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
                    // TODO(pavyamsiri): This span is wrong, it should be the span of decl.
                    self.resolve_variable_declaration(name, &name.span, initial.as_ref())?;
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
        span: &Span,
    ) -> Result<(), ResolutionError> {
        if matches!(self.function, FunctionEnvironment::None) {
            return Err(ResolutionError {
                kind: ResolutionErrorKind::NonFunctionReturn,
                span: span.clone(),
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
                self.resolve_variable(&lhs);
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
            ExpressionNode::Get { object, .. } => {
                self.resolve_expression_node(expr, object.clone())?;
            }
            ExpressionNode::Set { object, value, .. } => {
                self.resolve_expression_node(expr, value.clone())?;
                self.resolve_expression_node(expr, object.clone())?;
            }
        }
        Ok(())
    }

    fn resolve_atom(&mut self, atom: &ExpressionAtom) -> Result<(), ResolutionError> {
        match &atom.kind {
            ExpressionAtomKind::Identifier(name) => {
                self.resolve_variable_expression(&Ident {
                    name: name.clone(),
                    span: atom.span,
                })?;
            }
            ExpressionAtomKind::This => match self.class {
                ClassEnvironment::None => {
                    return Err(ResolutionError {
                        kind: ResolutionErrorKind::NonClassThis,
                        span: atom.span,
                    });
                }
                ClassEnvironment::Class => {
                    self.resolve_variable_expression(&Ident {
                        name: "this".into(),
                        span: atom.span,
                    })?;
                }
            },
            _ => {}
        }
        Ok(())
    }

    fn resolve_variable_expression(&mut self, ident: &Ident) -> Result<(), ResolutionError> {
        // Can't access variable when it is declared but not defined
        if let Some(Resolution::Declared { name, span }) = self.get_resolution(ident) {
            Err(ResolutionError {
                kind: ResolutionErrorKind::SelfReferentialInitializer {
                    destination: name.clone(),
                    reference: ident.clone(),
                },
                span: span.clone(),
            })
        } else {
            self.resolve_variable(ident);
            Ok(())
        }
    }
}
