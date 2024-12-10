use super::{
    BinaryOperator, ExpressionTreeAtom, ExpressionTreeNode, ExpressionTreeNodeRef,
    ExpressionTreeWithRoot, UnaryOperator,
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
            ExpressionTreeNode::Atom(ExpressionTreeAtom::Nil) => "nil".into(),
            ExpressionTreeNode::Atom(ExpressionTreeAtom::Bool(value)) => format!("{value}"),
            ExpressionTreeNode::Atom(ExpressionTreeAtom::Number(value)) => format!("{value:?}"),
            ExpressionTreeNode::Atom(ExpressionTreeAtom::Identifier(value)) => format!("{value}"),
            ExpressionTreeNode::Atom(ExpressionTreeAtom::StringLiteral(value)) => {
                format!("{value}")
            }
            ExpressionTreeNode::Unary { operator, rhs } => {
                format!(
                    "({} {})",
                    SExpressionFormatter::format_unary_operator(operator),
                    SExpressionFormatter::format_node(tree, &rhs),
                )
            }
            ExpressionTreeNode::Binary { operator, lhs, rhs } => {
                format!(
                    "({} {} {})",
                    SExpressionFormatter::format_binary_operator(operator),
                    SExpressionFormatter::format_node(tree, &lhs),
                    SExpressionFormatter::format_node(tree, &rhs),
                )
            }
            ExpressionTreeNode::Group { inner } => {
                format!(
                    "(group {})",
                    SExpressionFormatter::format_node(tree, &inner)
                )
            }
        }
    }

    fn format_unary_operator(operator: &UnaryOperator) -> String {
        match operator {
            UnaryOperator::Bang => "!",
            UnaryOperator::Minus => "-",
        }
        .into()
    }
    fn format_binary_operator(operator: &BinaryOperator) -> String {
        match operator {
            BinaryOperator::Add => "+",
            BinaryOperator::Subtract => "-",
            BinaryOperator::Multiply => "*",
            BinaryOperator::Divide => "/",
            BinaryOperator::LessThan => "<",
            BinaryOperator::LessThanEqual => "<=",
            BinaryOperator::GreaterThan => ">",
            BinaryOperator::GreaterThanEqual => ">=",
            BinaryOperator::EqualEqual => "==",
            BinaryOperator::BangEqual => "!=",
        }
        .into()
    }
}

impl ExpressionFormatter for SExpressionFormatter {
    fn format(&self, tree: &ExpressionTreeWithRoot) -> String {
        SExpressionFormatter::format_node(tree, &tree.root)
    }
}
