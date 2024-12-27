use super::expression::Expression;
use crate::{lexer::Span, string::IdentifierString};

#[derive(Debug, Clone)]
pub enum Statement {
    Declaration(Declaration),
    NonDeclaration(NonDeclaration),
}

impl Statement {
    pub fn get_span(&self) -> Span {
        match self {
            Statement::Declaration(decl) => decl.span,
            Statement::NonDeclaration(stmt) => stmt.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub kind: DeclarationKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum DeclarationKind {
    Variable {
        name: IdentifierString,
        initial: Option<Expression>,
    },
    Function {
        name: IdentifierString,
        parameters: Vec<IdentifierString>,
        body: Vec<Statement>,
    },
}

#[derive(Debug, Clone)]
pub enum Initializer {
    VarDecl {
        name: IdentifierString,
        initial: Option<Expression>,
    },
    Expression(Expression),
}

#[derive(Debug, Clone)]
pub struct NonDeclaration {
    pub kind: NonDeclarationKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum NonDeclarationKind {
    Expression(Expression),
    Print(Expression),
    Block(Vec<Statement>),
    If {
        condition: Expression,
        success: Box<Statement>,
        failure: Box<Option<Statement>>,
    },
    While {
        condition: Expression,
        body: Box<Statement>,
    },
    For {
        initializer: Option<Initializer>,
        condition: Option<Expression>,
        increment: Option<Expression>,
        body: Box<NonDeclaration>,
    },
    Return {
        value: Option<Expression>,
    },
}
