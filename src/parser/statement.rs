use compact_str::CompactString;

use super::expression::ExpressionTreeWithRoot;

#[derive(Debug, Clone)]
pub enum Statement {
    Declaration(Declaration),
    NonDeclaration(NonDeclaration),
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Variable {
        name: CompactString,
        initial: Option<ExpressionTreeWithRoot>,
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
        initial: Option<ExpressionTreeWithRoot>,
    },
    Expression(ExpressionTreeWithRoot),
}

#[derive(Debug, Clone)]
pub enum NonDeclaration {
    Expression(ExpressionTreeWithRoot),
    Print(ExpressionTreeWithRoot),
    Block(Vec<Statement>),
    If {
        condition: ExpressionTreeWithRoot,
        success: Box<Statement>,
        failure: Box<Option<Statement>>,
    },
    While {
        condition: ExpressionTreeWithRoot,
        body: Box<Statement>,
    },
    For {
        initializer: Option<Initializer>,
        condition: Option<ExpressionTreeWithRoot>,
        increment: Option<ExpressionTreeWithRoot>,
        body: Box<NonDeclaration>,
    },
}
