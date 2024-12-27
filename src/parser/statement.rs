use super::expression::Expression;
use crate::{lexer::Span, string::Ident};

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
        name: Ident,
        initial: Option<Expression>,
    },
    Function {
        name: Ident,
        parameters: Vec<Ident>,
        body: Vec<Statement>,
    },
}

impl std::fmt::Display for DeclarationKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DeclarationKind::Variable { .. } => write!(f, "VARDECL"),
            DeclarationKind::Function { .. } => write!(f, "FUNDECL"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Initializer {
    VarDecl {
        name: Ident,
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

impl std::fmt::Display for NonDeclarationKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NonDeclarationKind::Expression(_) => write!(f, "EXPR"),
            NonDeclarationKind::Print(_) => write!(f, "PRINT"),
            NonDeclarationKind::Block(_) => write!(f, "BLOCK"),
            NonDeclarationKind::If { .. } => write!(f, "IF"),
            NonDeclarationKind::While { .. } => write!(f, "WHILE"),
            NonDeclarationKind::For { .. } => write!(f, "FOR"),
            NonDeclarationKind::Return { .. } => write!(f, "RETURN"),
        }
    }
}
