use compact_str::CompactString;

use super::expression::Expression;

#[derive(Debug, Clone)]
pub enum Statement {
    Declaration(Declaration),
    NonDeclaration(NonDeclaration),
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Variable {
        name: CompactString,
        initial: Option<Expression>,
    },
    Function {
        name: CompactString,
        parameters: Vec<CompactString>,
        body: Vec<Statement>,
    },
}

#[derive(Debug, Clone)]
pub enum Initializer {
    VarDecl {
        name: CompactString,
        initial: Option<Expression>,
    },
    Expression(Expression),
}

#[derive(Debug, Clone)]
pub enum NonDeclaration {
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
}
