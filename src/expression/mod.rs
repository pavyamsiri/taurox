pub mod formatter;

#[derive(Debug, Clone)]
pub enum ExpressionOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl ExpressionOperator {
    pub fn get_binding_power(&self) -> (u8, u8) {
        match self {
            ExpressionOperator::Add | ExpressionOperator::Subtract => (1, 2),
            ExpressionOperator::Multiply | ExpressionOperator::Divide => (3, 4),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionTreeNodeRef(u32);
#[derive(Debug, Clone)]
pub enum ExpressionTreeNode {
    Number(f64),
    Expression(ExpressionOperator, Vec<ExpressionTreeNodeRef>),
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
