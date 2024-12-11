pub mod formatter;

use compact_str::CompactString;

use crate::token::TokenKind;

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Bang,
    Minus,
}

impl UnaryOperator {
    pub fn get_binding_power(&self) -> u8 {
        match self {
            // 1. Unary operators
            UnaryOperator::Bang | UnaryOperator::Minus => 9,
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
            // 2. Multiplicative operators
            BinaryOperator::Multiply | BinaryOperator::Divide => (7, 8),
            // 3. Additive operators
            BinaryOperator::Add | BinaryOperator::Subtract => (5, 6),
            // 4. Comparison operators
            BinaryOperator::LessThan
            | BinaryOperator::LessThanEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterThanEqual => (4, 5),
            // 5. Equality operators
            BinaryOperator::EqualEqual | BinaryOperator::BangEqual => (3, 4),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryAssignmentOperator {
    Assign,
}

impl BinaryAssignmentOperator {
    pub fn get_binding_power(&self) -> (u8, u8) {
        match self {
            BinaryAssignmentOperator::Assign => (2, 1),
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
    BinaryAssignment {
        operator: BinaryAssignmentOperator,
        lhs: CompactString,
        rhs: ExpressionTreeNodeRef,
    },
}

impl ExpressionTreeNode {
    pub fn get_l_value(&self) -> Option<CompactString> {
        match self {
            ExpressionTreeNode::Atom(ExpressionTreeAtom {
                kind: ExpressionTreeAtomKind::Identifier(name),
                ..
            }) => Some(name.clone()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionTreeAtomKind {
    Number(f64),
    Bool(bool),
    Nil,
    Identifier(CompactString),
    StringLiteral(CompactString),
}

#[derive(Debug, Clone)]
pub struct ExpressionTreeAtom {
    pub kind: ExpressionTreeAtomKind,
    pub line: u32,
}

#[derive(Debug, Clone)]
pub struct ExpressionTree {
    nodes: Vec<ExpressionTreeNode>,
}

#[derive(Debug, Clone)]
pub struct ExpressionTreeWithRoot {
    nodes: Vec<ExpressionTreeNode>,
    root: ExpressionTreeNodeRef,
}

impl ExpressionTree {
    pub fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    pub fn push(&mut self, node: ExpressionTreeNode) -> ExpressionTreeNodeRef {
        self.nodes.push(node);
        ExpressionTreeNodeRef(self.nodes.len() as u32 - 1)
    }

    pub fn get_node(&self, index: &ExpressionTreeNodeRef) -> Option<&ExpressionTreeNode> {
        self.nodes.get(index.0 as usize)
    }

    pub fn get_line(&self, node: &ExpressionTreeNodeRef) -> Option<u32> {
        let node = self.nodes.get(node.0 as usize)?;
        match node {
            ExpressionTreeNode::Atom(ExpressionTreeAtom { line, .. }) => Some(*line),
            ExpressionTreeNode::Unary { rhs, .. } => self.get_line(rhs),
            ExpressionTreeNode::Binary { lhs, .. } => self.get_line(lhs),
            ExpressionTreeNode::Group { inner } => self.get_line(inner),
            ExpressionTreeNode::BinaryAssignment { rhs, .. } => self.get_line(rhs),
        }
    }

    pub fn get_kind(&self, node: &ExpressionTreeNodeRef) -> Option<TokenKind> {
        let node = self.nodes.get(node.0 as usize)?;
        match node {
            ExpressionTreeNode::Atom(ExpressionTreeAtom { kind, .. }) => match kind {
                ExpressionTreeAtomKind::Number(_) => Some(TokenKind::NumericLiteral),
                ExpressionTreeAtomKind::Bool(true) => Some(TokenKind::KeywordTrue),
                ExpressionTreeAtomKind::Bool(false) => Some(TokenKind::KeywordFalse),
                ExpressionTreeAtomKind::Nil => Some(TokenKind::KeywordNil),
                ExpressionTreeAtomKind::Identifier(_) => Some(TokenKind::Ident),
                ExpressionTreeAtomKind::StringLiteral(_) => Some(TokenKind::StringLiteral),
            },
            ExpressionTreeNode::Unary { rhs, .. } => self.get_kind(rhs),
            ExpressionTreeNode::Binary { lhs, .. } => self.get_kind(lhs),
            ExpressionTreeNode::Group { inner } => self.get_kind(inner),
            ExpressionTreeNode::BinaryAssignment { rhs, .. } => self.get_kind(rhs),
        }
    }
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

    pub fn get_root_ref(&self) -> ExpressionTreeNodeRef {
        self.root
    }

    pub fn get_root(&self) -> &ExpressionTreeNode {
        self.get_node(&self.get_root_ref())
            .expect("The root exists within the tree.")
    }

    pub fn get_node(&self, node: &ExpressionTreeNodeRef) -> Option<&ExpressionTreeNode> {
        self.nodes.get(node.0 as usize)
    }

    pub fn get_line(&self, node: &ExpressionTreeNodeRef) -> Option<u32> {
        let node = self.nodes.get(node.0 as usize)?;
        match node {
            ExpressionTreeNode::Atom(ExpressionTreeAtom { line, .. }) => Some(*line),
            ExpressionTreeNode::Unary { rhs, .. } => self.get_line(rhs),
            ExpressionTreeNode::Binary { lhs, .. } => self.get_line(lhs),
            ExpressionTreeNode::Group { inner } => self.get_line(inner),
            ExpressionTreeNode::BinaryAssignment { rhs, .. } => self.get_line(rhs),
        }
    }
}
