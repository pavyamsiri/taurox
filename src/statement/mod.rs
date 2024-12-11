use compact_str::CompactString;

use crate::expression::ExpressionTreeWithRoot;

#[derive(Debug)]
pub enum Statement {
    Declaration(Declaration),
    NonDeclaration(NonDeclaration),
}

#[derive(Debug)]
pub enum Declaration {
    Variable {
        name: CompactString,
        initial: Option<ExpressionTreeWithRoot>,
    },
}

#[derive(Debug)]
pub enum Initializer {
    VarDecl {
        name: CompactString,
        initial: Option<ExpressionTreeWithRoot>,
    },
    Expression(ExpressionTreeWithRoot),
}

#[derive(Debug)]
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
        body: Box<Statement>,
    },
}
