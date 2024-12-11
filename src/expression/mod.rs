pub mod formatter;

use compact_str::CompactString;

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Bang,
    Minus,
}

impl UnaryOperator {
    pub fn get_binding_power(&self) -> u8 {
        match self {
            UnaryOperator::Bang | UnaryOperator::Minus => 7,
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    EqualEqual,
    BangEqual,
}

impl BinaryOperator {
    pub fn get_binding_power(&self) -> (u8, u8) {
        match self {
            // 1. Multiplicative operators
            BinaryOperator::Multiply | BinaryOperator::Divide => (5, 6),
            // 2. Additive operators
            BinaryOperator::Add | BinaryOperator::Subtract => (3, 4),
            // 3. Comparison operators
            BinaryOperator::LessThan
            | BinaryOperator::LessThanEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterThanEqual => (2, 3),
            // 4. Equality operators
            BinaryOperator::EqualEqual | BinaryOperator::BangEqual => (1, 2),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ExpressionTreeNodeRef(u32);
#[derive(Debug, Clone)]
pub enum ExpressionTreeNode {
    Atom(ExpressionTreeAtom),
    Unary {
        operator: UnaryOperator,
        rhs: ExpressionTreeNodeRef,
    },
    Binary {
        operator: BinaryOperator,
        lhs: ExpressionTreeNodeRef,
        rhs: ExpressionTreeNodeRef,
    },
    Group {
        inner: ExpressionTreeNodeRef,
    },
}

#[derive(Debug, Clone)]
pub enum ExpressionTreeAtom {
    Number(f64),
    Bool(bool),
    Nil,
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

    pub fn get_root(&self) -> ExpressionTreeNodeRef {
        self.root
    }

    pub fn get_node(&self, node: &ExpressionTreeNodeRef) -> Option<&ExpressionTreeNode> {
        self.nodes.get(node.0 as usize)
    }
}
