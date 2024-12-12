use compact_str::ToCompactString;
use error::{ResolverError, ResolverErrorKind};

use crate::parser::{
    expression::Expression,
    statement::{Declaration, NonDeclaration, Statement},
};
mod error;

use super::scope::{Liveness, Scope};

pub struct Resolver;

impl Resolver {
    pub fn resolve_statement(
        &self,
        statement: &Statement,
        scope: &mut Scope,
    ) -> Result<(), ResolverError> {
        match statement {
            Statement::Declaration(declaration) => todo!(),
            Statement::NonDeclaration(statement) => self.resolve_non_declaration(statement, scope),
        }
    }

    fn resolve_declaration(
        &self,
        statement: &Declaration,
        scope: &mut Scope,
    ) -> Result<(), ResolverError> {
        match statement {
            Declaration::Variable { name, initial } => {
                self.resolve_variable_declaration(name, initial.as_ref(), scope)
            }
            Declaration::Function(_) => todo!(),
            Declaration::Class { name, methods } => todo!(),
        }
    }

    fn resolve_non_declaration(
        &self,
        statement: &NonDeclaration,
        scope: &mut Scope,
    ) -> Result<(), ResolverError> {
        match statement {
            NonDeclaration::Expression(expression) => todo!(),
            NonDeclaration::Print(expression) => todo!(),
            NonDeclaration::Block(block) => self.resolve_block(block, scope),
            NonDeclaration::If {
                condition,
                success,
                failure,
            } => todo!(),
            NonDeclaration::While { condition, body } => todo!(),
            NonDeclaration::For {
                initializer,
                condition,
                increment,
                body,
            } => todo!(),
            NonDeclaration::Return { value } => todo!(),
        }
    }
}

// Declarations
impl Resolver {
    fn resolve_variable_declaration(
        &self,
        name: &str,
        initializer: Option<&Expression>,
        scope: &mut Scope,
    ) -> Result<(), ResolverError> {
        scope.declare(name);
        if let Some(initializer) = initializer {
            self.resolve_expression(initializer, scope)?;
        }
        scope.define(name);
        Ok(())
    }
}

// Non-declarations
impl Resolver {
    fn resolve_block(&self, block: &[Statement], scope: &mut Scope) -> Result<(), ResolverError> {
        scope.enter_scope();
        for statement in block {
            self.resolve_statement(statement, scope)?;
        }
        scope.exit_scope();
        Ok(())
    }
}

// Expressions
impl Resolver {
    fn resolve_expression(
        &self,
        expr: &Expression,
        scope: &mut Scope,
    ) -> Result<(), ResolverError> {
        let Some(name) = expr.get_name() else {
            return Ok(());
        };
        let name = name.to_compact_string();

        match scope.get(&name) {
            Some(Liveness::Undefined) => Err(ResolverError {
                kind: ResolverErrorKind::InvalidLocalInitializer(name),
                line: expr.get_line_of_root(),
            }),
            _ => Ok(()),
        }
    }
}
