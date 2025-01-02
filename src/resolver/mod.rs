mod error;
pub mod formatter;

use crate::{
    lexer::Span,
    parser::{
        expression::{
            Expression, ExpressionAtom, ExpressionAtomKind, ExpressionNode, ExpressionNodeRef,
        },
        program::ProgramIterator,
        statement::{
            BlockStatement, ClassDecl, ExpressionStatement, ForStatement, FunctionDecl,
            IfStatement, Initializer, NonDeclaration, PrintStatement, ReturnStatement, Statement,
            VariableDecl, WhileStatement,
        },
        Program,
    },
    string::{Ident, IdentName},
};
pub use error::{ResolutionError, ResolutionErrorKind};
use std::collections::HashMap;

pub type ResolutionMap = HashMap<Ident, usize>;

pub struct ResolvedProgram {
    program: Program,
    resolution: ResolutionMap,
}

impl ResolvedProgram {
    pub fn empty() -> Self {
        Self {
            program: Program::empty(),
            resolution: HashMap::new(),
        }
    }

    pub fn iter<'a>(&'a self) -> ProgramIterator<'a> {
        self.program.iter()
    }

    pub fn get_resolution(&self) -> &ResolutionMap {
        &self.resolution
    }
}

impl std::ops::Deref for ResolvedProgram {
    type Target = Program;

    fn deref(&self) -> &Self::Target {
        &self.program
    }
}

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

    pub fn resolve(mut self, program: Program) -> Result<ResolvedProgram, Vec<ResolutionError>> {
        let mut errors = Vec::new();
        for stmt in program.iter() {
            match self.resolve_stmt(&program, stmt) {
                Ok(_) => {}
                Err(e) => errors.push(e),
            }
        }
        if errors.is_empty() {
            Ok(ResolvedProgram {
                program,
                resolution: self.resolution,
            })
        } else {
            Err(errors)
        }
    }
    pub fn resolve_expression_and_consume(
        mut self,
        expr: &Expression,
    ) -> Result<ResolvedProgram, ResolutionError> {
        self.resolve_expression(expr)?;
        Ok(ResolvedProgram {
            program: Program::empty(),
            resolution: self.resolution,
        })
    }

    fn resolve_stmt<'stmt>(
        &mut self,
        program: &'stmt Program,
        statement: Statement<'stmt>,
    ) -> Result<(), ResolutionError> {
        match statement {
            Statement::VariableDecl(decl) => self.resolve_variable_decl(program, decl),
            Statement::FunctionDecl(decl) => self.resolve_function_decl(program, decl),
            Statement::ClassDecl(decl) => self.resolve_class_decl(program, decl),
            Statement::Expression(stmt) => self.resolve_expression_stmt(program, stmt),
            Statement::Print(stmt) => self.resolve_print_stmt(program, stmt),
            Statement::Block(stmt) => self.resolve_block_stmt(program, stmt),
            Statement::If(stmt) => self.resolve_if_stmt(program, stmt),
            Statement::While(stmt) => self.resolve_while_stmt(program, stmt),
            Statement::For(stmt) => self.resolve_for_stmt(program, stmt),
            Statement::Return(stmt) => self.resolve_return_stmt(program, stmt),
        }
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
    fn resolve_variable_decl(
        &mut self,
        program: &Program,
        decl: &VariableDecl,
    ) -> Result<(), ResolutionError> {
        let _ = program;
        self.declare(&decl.name, &decl.span)?;
        if let Some(initial) = &decl.initial {
            self.resolve_expression(initial)?;
        }
        self.define(&decl.name, &decl.span);
        Ok(())
    }

    fn resolve_function_decl(
        &mut self,
        program: &Program,
        decl: &FunctionDecl,
    ) -> Result<(), ResolutionError> {
        self.declare(&decl.name, &decl.span)?;
        self.define(&decl.name, &decl.span);
        self.resolve_function(program, decl, FunctionEnvironment::Function)?;
        Ok(())
    }

    fn resolve_class_decl(
        &mut self,
        program: &Program,
        decl: &ClassDecl,
    ) -> Result<(), ResolutionError> {
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
            self.resolve_function(program, decl, function_type)?
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
        program: &Program,
        decl: &FunctionDecl,
        environment: FunctionEnvironment,
    ) -> Result<(), ResolutionError> {
        const MSG: &'static str = "[Resolve Function]: All handles are valid.";
        let enclosing = self.function;
        self.function = environment;
        self.enter_scope();
        for param in decl.parameters.iter() {
            self.declare(param, &param.span)?;
            self.define(param, &param.span);
        }
        let body = program.get_block_stmt(decl.body).expect(MSG);
        for stmt in body.iter() {
            let stmt = program.get_statement(stmt).expect(MSG);
            self.resolve_stmt(program, stmt)?;
        }
        self.exit_scope();
        self.function = enclosing;
        Ok(())
    }
}

// Non-declarations
impl Resolver {
    fn resolve_non_declaration<'stmt>(
        &mut self,
        program: &Program,
        stmt: NonDeclaration<'stmt>,
    ) -> Result<(), ResolutionError> {
        match stmt {
            NonDeclaration::Expression(stmt) => self.resolve_expression_stmt(program, stmt),
            NonDeclaration::Print(stmt) => self.resolve_print_stmt(program, stmt),
            NonDeclaration::Block(stmt) => self.resolve_block_stmt(program, stmt),
            NonDeclaration::If(stmt) => self.resolve_if_stmt(program, stmt),
            NonDeclaration::While(stmt) => self.resolve_while_stmt(program, stmt),
            NonDeclaration::For(stmt) => self.resolve_for_stmt(program, stmt),
            NonDeclaration::Return(stmt) => self.resolve_return_stmt(program, stmt),
        }
    }

    fn resolve_expression_stmt(
        &mut self,
        program: &Program,
        stmt: &ExpressionStatement,
    ) -> Result<(), ResolutionError> {
        let _ = program;
        self.resolve_expression(&stmt.expr)?;
        Ok(())
    }

    fn resolve_print_stmt(
        &mut self,
        program: &Program,
        stmt: &PrintStatement,
    ) -> Result<(), ResolutionError> {
        let _ = program;
        self.resolve_expression(&stmt.expr)?;
        Ok(())
    }

    fn resolve_block_stmt(
        &mut self,
        program: &Program,
        block: &BlockStatement,
    ) -> Result<(), ResolutionError> {
        const MSG: &'static str = "[Resolve Block]: All handles are valid.";
        self.enter_scope();
        for stmt in block.iter() {
            let stmt = program.get_statement(stmt).expect(MSG);
            self.resolve_stmt(program, stmt)?;
        }
        self.exit_scope();
        Ok(())
    }

    fn resolve_if_stmt(
        &mut self,
        program: &Program,
        stmt: &IfStatement,
    ) -> Result<(), ResolutionError> {
        const MSG: &'static str = "[Resolve If]: All handles are valid.";
        self.resolve_expression(&stmt.condition)?;
        let success = program.get_statement(stmt.success).expect(MSG);
        self.resolve_stmt(program, success)?;
        if let Some(failure) = stmt.failure {
            let failure = program.get_statement(failure).expect(MSG);
            self.resolve_stmt(program, failure)?;
        }
        Ok(())
    }

    fn resolve_while_stmt(
        &mut self,
        program: &Program,
        stmt: &WhileStatement,
    ) -> Result<(), ResolutionError> {
        const MSG: &'static str = "[Resolve While]: All handles are valid.";
        self.resolve_expression(&stmt.condition)?;
        let body = program.get_non_declaration(stmt.body).expect(MSG);
        self.resolve_non_declaration(program, body)
    }

    fn resolve_for_stmt(
        &mut self,
        program: &Program,
        stmt: &ForStatement,
    ) -> Result<(), ResolutionError> {
        const MSG: &'static str = "[Resolve For]: All handles are valid.";
        self.enter_scope();
        if let Some(ref initializer) = stmt.initializer {
            match initializer {
                Initializer::VariableDecl(decl) => {
                    let decl = program.get_variable_decl(*decl).expect(MSG);
                    self.resolve_variable_decl(program, decl)?;
                }
                Initializer::Expression(stmt) => {
                    let stmt = program.get_expression_stmt(*stmt).expect(MSG);
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
        let body = program.get_non_declaration(stmt.body).expect(MSG);
        self.resolve_non_declaration(program, body)?;
        self.exit_scope();
        Ok(())
    }

    fn resolve_return_stmt(
        &mut self,
        program: &Program,
        stmt: &ReturnStatement,
    ) -> Result<(), ResolutionError> {
        let _ = program;
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
