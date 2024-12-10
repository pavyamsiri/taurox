use super::{
    ExpressionOperator, ExpressionTreeNode, ExpressionTreeNodeRef, ExpressionTreeWithRoot,
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
            ExpressionTreeNode::Number(value) => format!("{value:?}"),
            ExpressionTreeNode::Expression(operator, expressions) => {
                let mut buffer = format!("({}", SExpressionFormatter::format_operator(operator));
                for expression in expressions {
                    buffer.push(' ');
                    buffer += &SExpressionFormatter::format_node(tree, &expression);
                }
                buffer.push(')');
                buffer
            }
        }
    }

    fn format_operator(operator: &ExpressionOperator) -> String {
        match operator {
            ExpressionOperator::Add => "+",
            ExpressionOperator::Subtract => "-",
            ExpressionOperator::Multiply => "*",
            ExpressionOperator::Divide => "/",
        }
        .into()
    }
}

impl ExpressionFormatter for SExpressionFormatter {
    fn format(&self, tree: &ExpressionTreeWithRoot) -> String {
        SExpressionFormatter::format_node(tree, &tree.root)
    }
}
