pub mod formatter;

use compact_str::CompactString;

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl BinaryOperator {
    pub fn get_binding_power(&self) -> (u8, u8) {
        match self {
            BinaryOperator::Add | BinaryOperator::Subtract => (1, 2),
            BinaryOperator::Multiply | BinaryOperator::Divide => (3, 4),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionTreeNodeRef(u32);
#[derive(Debug, Clone)]
pub enum ExpressionTreeNode {
    Atom(ExpressionTreeAtom),
    Binary {
        operator: BinaryOperator,
        lhs: ExpressionTreeNodeRef,
        rhs: ExpressionTreeNodeRef,
    },
}

#[derive(Debug, Clone)]
pub enum ExpressionTreeAtom {
    Number(f64),
    Identifier(CompactString),
    StringLiteral(CompactString),
}

#[derive(Debug, Clone)]
pub struct ExpressionTree {
    nodes: Vec<ExpressionTreeNode>,
}

impl ExpressionTree {
    pub fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    pub fn push(&mut self, node: ExpressionTreeNode) -> ExpressionTreeNodeRef {
        self.nodes.push(node);
        ExpressionTreeNodeRef(self.nodes.len() as u32 - 1)
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionTreeWithRoot {
    nodes: Vec<ExpressionTreeNode>,
    root: ExpressionTreeNodeRef,
}

impl ExpressionTreeWithRoot {
    pub fn new(tree: ExpressionTree, root: ExpressionTreeNodeRef) -> Option<Self> {
        if !(0..tree.nodes.len()).contains(&(root.0 as usize)) {
            None
        } else {
            Some(Self {
                nodes: tree.nodes,
                root,
            })
        }
    }
}
