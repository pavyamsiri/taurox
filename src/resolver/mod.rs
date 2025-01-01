mod error;
pub mod formatter;

use crate::{
    lexer::Span,
    parser::{
        expression::{
            Expression, ExpressionAtom, ExpressionAtomKind, ExpressionNode, ExpressionNodeRef,
        },
        statement::{
            BlockStatement, ClassDecl, Declaration, ForStatement, FunctionDecl, IfStatement,
            Initializer, NonDeclaration, ReturnStatement, Statement, VariableDecl, WhileStatement,
        },
        Program,
    },
    string::{Ident, IdentName},
};
pub use error::{ResolutionError, ResolutionErrorKind};
use std::collections::HashMap;

pub type ResolutionMap = HashMap<Ident, usize>;

#[derive(Debug)]
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
    Constructor,
}

#[derive(Clone, Copy)]
enum ClassEnvironment {
    None,
    Class,
    SubClass,
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

    pub fn resolve_program(
        mut self,
        program: &Program,
    ) -> Result<ResolutionMap, Vec<ResolutionError>> {
        let mut errors = Vec::new();
        for index in 0..program.len() {
            let statement = program
                .get_statement(index)
                .expect("Iterating over valid indices.");
            match self.resolve_statement(statement) {
                Ok(_) => {}
                Err(e) => errors.push(e),
            }
        }
        if errors.is_empty() {
            Ok(self.resolution)
        } else {
            Err(errors)
        }
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
                break;
            }
        }
    }
}

// Implementation
impl Resolver {
    fn resolve_declaration(&mut self, decl: &Declaration) -> Result<(), ResolutionError> {
        match &decl {
            Declaration::Variable(decl) => {
                self.resolve_variable_declaration(decl)?;
            }
            Declaration::Function(decl) => {
                self.resolve_function_declaration(decl)?;
            }
            Declaration::Class(decl) => {
                self.resolve_class_declaration(decl)?;
            }
        }
        Ok(())
    }

    fn resolve_variable_declaration(&mut self, decl: &VariableDecl) -> Result<(), ResolutionError> {
        self.declare(&decl.name, &decl.span)?;
        if let Some(initial) = &decl.initial {
            self.resolve_expression(initial)?;
        }
        self.define(&decl.name, &decl.span);
        Ok(())
    }

    fn resolve_function_declaration(&mut self, decl: &FunctionDecl) -> Result<(), ResolutionError> {
        self.declare(&decl.name, &decl.span)?;
        self.define(&decl.name, &decl.span);
        self.resolve_function(&decl.parameters, &decl.body, FunctionEnvironment::Function)?;

        Ok(())
    }

    fn resolve_class_declaration(&mut self, decl: &ClassDecl) -> Result<(), ResolutionError> {
        let enclosing = self.class;
        self.class = ClassEnvironment::Class;

        self.declare(&decl.name, &decl.span)?;
        self.define(&decl.name, &decl.span);

        if let Some(super_class) = &decl.super_class {
            if &super_class.name == &decl.name.name {
                return Err(ResolutionError {
                    kind: ResolutionErrorKind::SelfReferentialInheritance {
                        destination: decl.name.clone(),
                        reference: super_class.clone(),
                    },
                    span: decl.span,
                });
            }

            self.class = ClassEnvironment::SubClass;
            self.resolve_variable(super_class);
        }

        // Handle super class scoping
        if decl.super_class.is_some() {
            let super_ident = Ident {
                name: "super".into(),
                span: decl.span.clone(),
            };
            self.enter_scope();
            self.declare(&super_ident, &decl.span)?;
            self.define(&super_ident, &decl.span);
        }

        self.enter_scope();
        let this_ident = Ident {
            name: "this".into(),
            span: decl.span,
        };
        self.declare(&this_ident, &decl.span)?;
        self.define(&this_ident, &decl.span);
        for decl in decl.methods.iter() {
            let function_type = if &(*decl.name.name) == "init" {
                FunctionEnvironment::Constructor
            } else {
                FunctionEnvironment::Method
            };
            self.resolve_function(&decl.parameters, &decl.body, function_type)?
        }
        self.exit_scope();

        // Pop off extra super class scope
        if decl.super_class.is_some() {
            self.exit_scope();
        }

        self.class = enclosing;
        // NOTE(pavyamsiri): This resolve doesn't happen in the book but this fixes a bug where methods can't reference their class.
        self.resolve_variable(&decl.name);
        Ok(())
    }

    fn resolve_function(
        &mut self,
        parameters: &[Ident],
        body: &BlockStatement,
        environment: FunctionEnvironment,
    ) -> Result<(), ResolutionError> {
        let enclosing = self.function;
        self.function = environment;
        self.enter_scope();
        for param in parameters {
            self.declare(param, &param.span)?;
            self.define(param, &param.span);
        }
        for statement in body.iter() {
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
        match &stmt {
            NonDeclaration::Expression(stmt) => self.resolve_expression(&stmt.expr)?,
            NonDeclaration::Print(stmt) => self.resolve_expression(&stmt.expr)?,
            NonDeclaration::Block(block) => {
                self.enter_scope();
                for stmt in block.iter() {
                    self.resolve_statement(stmt)?;
                }
                self.exit_scope();
            }
            NonDeclaration::If(stmt) => {
                self.resolve_if_statement(stmt)?;
            }
            NonDeclaration::While(stmt) => {
                self.resolve_while_statement(stmt)?;
            }
            NonDeclaration::For(stmt) => {
                self.resolve_for_statement(stmt)?;
            }
            NonDeclaration::Return(stmt) => {
                self.resolve_return_statement(stmt)?;
            }
        }
        Ok(())
    }

    fn resolve_if_statement(&mut self, stmt: &IfStatement) -> Result<(), ResolutionError> {
        self.resolve_expression(&stmt.condition)?;
        self.resolve_statement(&stmt.success)?;
        if let Some(failure) = &stmt.failure.as_ref() {
            self.resolve_statement(failure)?;
        }
        Ok(())
    }

    fn resolve_while_statement(&mut self, stmt: &WhileStatement) -> Result<(), ResolutionError> {
        self.resolve_expression(&stmt.condition)?;
        self.resolve_non_declaration(&stmt.body)?;
        Ok(())
    }

    fn resolve_for_statement(&mut self, stmt: &ForStatement) -> Result<(), ResolutionError> {
        self.enter_scope();
        if let Some(initializer) = &stmt.initializer {
            match initializer {
                Initializer::VariableDecl(decl) => {
                    self.resolve_variable_declaration(decl)?;
                }
                Initializer::Expression(stmt) => {
                    self.resolve_expression(&stmt.expr)?;
                }
            }
        }
        if let Some(condition) = &stmt.condition {
            self.resolve_expression(condition)?;
        }
        if let Some(increment) = &stmt.increment {
            self.resolve_expression(increment)?;
        }
        self.resolve_non_declaration(&stmt.body)?;
        self.exit_scope();
        Ok(())
    }

    fn resolve_return_statement(&mut self, stmt: &ReturnStatement) -> Result<(), ResolutionError> {
        if matches!(self.function, FunctionEnvironment::None) {
            return Err(ResolutionError {
                kind: ResolutionErrorKind::NonFunctionReturn,
                span: stmt.span,
            });
        }
        match self.function {
            FunctionEnvironment::None => Err(ResolutionError {
                kind: ResolutionErrorKind::NonFunctionReturn,
                span: stmt.span,
            }),
            FunctionEnvironment::Constructor if stmt.value.is_some() => Err(ResolutionError {
                kind: ResolutionErrorKind::ReturnInConstructor,
                span: stmt.span,
            }),
            _ => {
                if let Some(expr) = &stmt.value {
                    self.resolve_expression(expr)?;
                }
                Ok(())
            }
        }
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
                ClassEnvironment::Class | ClassEnvironment::SubClass => {
                    self.resolve_variable_expression(&Ident {
                        name: "this".into(),
                        span: atom.span,
                    })?;
                }
            },
            ExpressionAtomKind::Super(_) => match self.class {
                ClassEnvironment::None => {
                    return Err(ResolutionError {
                        kind: ResolutionErrorKind::NonClassSuper,
                        span: atom.span,
                    });
                }
                ClassEnvironment::Class => {
                    return Err(ResolutionError {
                        kind: ResolutionErrorKind::NonSubClassSuper,
                        span: atom.span,
                    });
                }
                ClassEnvironment::SubClass => {
                    self.resolve_variable_expression(&Ident {
                        name: "super".into(),
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
