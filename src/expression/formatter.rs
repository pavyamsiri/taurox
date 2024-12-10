use super::{
    BinaryOperator, ExpressionTreeAtom, ExpressionTreeNode, ExpressionTreeNodeRef,
    ExpressionTreeWithRoot,
};

pub trait ExpressionFormatter {
    fn format(&self, tree: &ExpressionTreeWithRoot) -> String;
}

pub struct DebugFormatter;

impl ExpressionFormatter for DebugFormatter {
    fn format(&self, tree: &ExpressionTreeWithRoot) -> String {
        format!("{tree:?}")
    }
}

pub struct SExpressionFormatter;

impl SExpressionFormatter {
    fn format_node(tree: &ExpressionTreeWithRoot, node: &ExpressionTreeNodeRef) -> String {
        let current_node = &tree.nodes[node.0 as usize];

        match current_node {
            ExpressionTreeNode::Atom(ExpressionTreeAtom::Number(value)) => format!("{value:?}"),
            ExpressionTreeNode::Atom(ExpressionTreeAtom::Identifier(value)) => format!("{value}"),
            ExpressionTreeNode::Atom(ExpressionTreeAtom::StringLiteral(value)) => {
                format!("{value}")
            }
            ExpressionTreeNode::Binary { operator, lhs, rhs } => {
                format!(
                    "({} {} {})",
                    SExpressionFormatter::format_operator(operator),
                    SExpressionFormatter::format_node(tree, &lhs),
                    SExpressionFormatter::format_node(tree, &rhs),
                )
            }
        }
    }

    fn format_operator(operator: &BinaryOperator) -> String {
        match operator {
            BinaryOperator::Add => "+",
            BinaryOperator::Subtract => "-",
            BinaryOperator::Multiply => "*",
            BinaryOperator::Divide => "/",
        }
        .into()
    }
}

impl ExpressionFormatter for SExpressionFormatter {
    fn format(&self, tree: &ExpressionTreeWithRoot) -> String {
        SExpressionFormatter::format_node(tree, &tree.root)
    }
}
