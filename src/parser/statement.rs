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
pub struct VariableDecl {
    pub name: Ident,
    pub initial: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub name: Ident,
    pub parameters: Vec<Ident>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct ClassDecl {
    pub name: Ident,
    pub methods: Vec<FunctionDecl>,
    pub super_class: Option<Ident>,
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub kind: DeclarationKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum DeclarationKind {
    Variable(VariableDecl),
    Function(FunctionDecl),
    Class(ClassDecl),
}

impl std::fmt::Display for DeclarationKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DeclarationKind::Variable { .. } => write!(f, "VARDECL"),
            DeclarationKind::Function(_) => write!(f, "FUNDECL"),
            DeclarationKind::Class { .. } => write!(f, "CLADECL"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Initializer {
    VarDecl {
        name: Ident,
        initial: Option<Expression>,
        stmt_span: Span,
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
