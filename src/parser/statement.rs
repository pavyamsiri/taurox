use super::{
    expression::Expression,
    program::{BlockStmtRef, ExpressionStmtRef, NonDeclarationRef, StatementRef, VariableDeclRef},
};
use crate::{lexer::Span, string::Ident};

#[derive(Debug, Clone)]
pub enum Statement<'a> {
    VariableDecl(&'a VariableDecl),
    FunctionDecl(&'a FunctionDecl),
    ClassDecl(&'a ClassDecl),
    Expression(&'a ExpressionStatement),
    Print(&'a PrintStatement),
    Block(&'a BlockStatement),
    If(&'a IfStatement),
    While(&'a WhileStatement),
    For(&'a ForStatement),
    Return(&'a ReturnStatement),
}

// Declarations

#[derive(Debug, Clone)]
pub struct VariableDecl {
    pub name: Ident,
    pub initial: Option<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub name: Ident,
    pub parameters: Vec<Ident>,
    pub body: BlockStmtRef,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ClassDecl {
    pub name: Ident,
    pub methods: Vec<FunctionDecl>,
    pub super_class: Option<Ident>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Variable(VariableDecl),
    Function(FunctionDecl),
    Class(ClassDecl),
}

impl Declaration {
    pub fn get_span(&self) -> Span {
        match self {
            Declaration::Variable(decl) => decl.span,
            Declaration::Function(decl) => decl.span,
            Declaration::Class(decl) => decl.span,
        }
    }
}

impl std::fmt::Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Declaration::Variable { .. } => write!(f, "VARDECL"),
            Declaration::Function(_) => write!(f, "FUNDECL"),
            Declaration::Class { .. } => write!(f, "CLADECL"),
        }
    }
}

// Statements
#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub expr: Expression,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct PrintStatement {
    pub expr: Expression,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub body: Vec<StatementRef>,
    pub span: Span,
}

impl BlockStatement {
    pub fn iter<'a>(&'a self) -> BlockStatementIterator<'a> {
        BlockStatementIterator {
            block: self,
            index: 0,
        }
    }
}

pub struct BlockStatementIterator<'a> {
    block: &'a BlockStatement,
    index: usize,
}

impl<'a> std::iter::Iterator for BlockStatementIterator<'a> {
    type Item = StatementRef;

    fn next(&mut self) -> Option<Self::Item> {
        let value = self.block.body.get(self.index);
        self.index += 1;
        value.cloned()
    }
}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub condition: Expression,
    pub success: StatementRef,
    pub failure: Option<StatementRef>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: NonDeclarationRef,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ForStatement {
    pub initializer: Option<Initializer>,
    pub condition: Option<Expression>,
    pub increment: Option<Expression>,
    pub body: NonDeclarationRef,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub value: Option<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Initializer {
    VariableDecl(VariableDeclRef),
    Expression(ExpressionStmtRef),
}

#[derive(Debug, Clone)]
pub enum NonDeclaration<'a> {
    Expression(&'a ExpressionStatement),
    Print(&'a PrintStatement),
    Block(&'a BlockStatement),
    If(&'a IfStatement),
    While(&'a WhileStatement),
    For(&'a ForStatement),
    Return(&'a ReturnStatement),
}

impl<'a> NonDeclaration<'a> {
    pub const fn get_span(&self) -> Span {
        match self {
            NonDeclaration::Expression(stmt) => stmt.span,
            NonDeclaration::Print(stmt) => stmt.span,
            NonDeclaration::Block(stmt) => stmt.span,
            NonDeclaration::If(stmt) => stmt.span,
            NonDeclaration::While(stmt) => stmt.span,
            NonDeclaration::For(stmt) => stmt.span,
            NonDeclaration::Return(stmt) => stmt.span,
        }
    }
}

impl<'a> std::fmt::Display for NonDeclaration<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NonDeclaration::Expression(_) => write!(f, "EXPR"),
            NonDeclaration::Print(_) => write!(f, "PRINT"),
            NonDeclaration::Block(_) => write!(f, "BLOCK"),
            NonDeclaration::If { .. } => write!(f, "IF"),
            NonDeclaration::While { .. } => write!(f, "WHILE"),
            NonDeclaration::For { .. } => write!(f, "FOR"),
            NonDeclaration::Return { .. } => write!(f, "RETURN"),
        }
    }
}
