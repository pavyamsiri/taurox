use crate::expression::ExpressionTreeWithRoot;

#[derive(Debug)]
pub enum Statement {
    Expression(ExpressionTreeWithRoot),
    Print(ExpressionTreeWithRoot),
}
