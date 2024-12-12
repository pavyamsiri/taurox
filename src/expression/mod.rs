pub mod formatter;

use crate::{evaluator::NativeFunction, token::TokenKind};
use compact_str::CompactString;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum ExpressionTreeAtomKind {
    Number(f64),
    Bool(bool),
    Nil,
    Identifier(CompactString),
    StringLiteral(CompactString),
    NativeFunction(Arc<dyn NativeFunction>),
}

#[derive(Debug, Clone)]
pub struct ExpressionTreeAtom {
    pub kind: ExpressionTreeAtomKind,
    pub line: u32,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Bang,
    Minus,
}

impl UnaryOperator {
    pub fn get_binding_power(&self) -> u8 {
        match self {
            // 2. Unary operators
            UnaryOperator::Bang | UnaryOperator::Minus => 15,
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Multiply,
    Divide,
    Add,
    Subtract,
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
            // 3. Multiplicative operators
            Self::Multiply | Self::Divide => (13, 14),
            // 4. Additive operators
            Self::Add | Self::Subtract => (11, 12),
            // 5. Comparison operators
            Self::LessThan | Self::LessThanEqual | Self::GreaterThan | Self::GreaterThanEqual => {
                (9, 10)
            }
            // 6. Equality operators
            Self::EqualEqual | Self::BangEqual => (7, 9),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryShortCircuitOperator {
    And,
    Or,
}

impl BinaryShortCircuitOperator {
    pub fn get_binding_power(&self) -> (u8, u8) {
        match self {
            // 7. Logical AND operator
            Self::And => (5, 6),
            // 8. Logical OR operator
            Self::Or => (3, 4),
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
            // 9. Assignment operator
            Self::Assign => (2, 1),
        }
    }
}

#[derive(Debug, Clone)]
pub enum PostfixOperator {
    Call,
}

impl PostfixOperator {
    pub fn get_binding_power(&self) -> u8 {
        match self {
            // 1. Call operator
            Self::Call => 17,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ExpressionTreeNodeRef(u32);
#[derive(Debug, Clone)]
pub enum ExpressionTreeNode {
    Atom(ExpressionTreeAtom),
    Group {
        inner: ExpressionTreeNodeRef,
    },
    Unary {
        operator: UnaryOperator,
        rhs: ExpressionTreeNodeRef,
    },
    Binary {
        operator: BinaryOperator,
        lhs: ExpressionTreeNodeRef,
        rhs: ExpressionTreeNodeRef,
    },
    BinaryAssignment {
        operator: BinaryAssignmentOperator,
        lhs: CompactString,
        rhs: ExpressionTreeNodeRef,
    },
    BinaryShortCircuit {
        operator: BinaryShortCircuitOperator,
        lhs: ExpressionTreeNodeRef,
        rhs: ExpressionTreeNodeRef,
    },
    Call {
        callee: ExpressionTreeNodeRef,
        arguments: Vec<ExpressionTreeNodeRef>,
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
            ExpressionTreeNode::BinaryAssignment { rhs, .. } => self.get_line(rhs),
            ExpressionTreeNode::BinaryShortCircuit { lhs, .. } => self.get_line(lhs),
            ExpressionTreeNode::Group { inner } => self.get_line(inner),
            ExpressionTreeNode::Call { callee, .. } => self.get_line(callee),
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
                ExpressionTreeAtomKind::NativeFunction(_) => Some(TokenKind::Ident),
            },
            ExpressionTreeNode::Unary { rhs, .. } => self.get_kind(rhs),
            ExpressionTreeNode::Binary { lhs, .. } => self.get_kind(lhs),
            ExpressionTreeNode::BinaryAssignment { rhs, .. } => self.get_kind(rhs),
            ExpressionTreeNode::BinaryShortCircuit { lhs, .. } => self.get_kind(lhs),
            ExpressionTreeNode::Group { inner } => self.get_kind(inner),
            ExpressionTreeNode::Call { callee, .. } => self.get_kind(callee),
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
            ExpressionTreeNode::BinaryAssignment { rhs, .. } => self.get_line(rhs),
            ExpressionTreeNode::BinaryShortCircuit { lhs, .. } => self.get_line(lhs),
            ExpressionTreeNode::Group { inner } => self.get_line(inner),
            ExpressionTreeNode::Call { callee, .. } => self.get_line(callee),
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
                ExpressionTreeAtomKind::NativeFunction(_) => Some(TokenKind::Ident),
            },
            ExpressionTreeNode::Unary { rhs, .. } => self.get_kind(rhs),
            ExpressionTreeNode::Binary { lhs, .. } => self.get_kind(lhs),
            ExpressionTreeNode::BinaryAssignment { rhs, .. } => self.get_kind(rhs),
            ExpressionTreeNode::BinaryShortCircuit { lhs, .. } => self.get_kind(lhs),
            ExpressionTreeNode::Group { inner } => self.get_kind(inner),
            ExpressionTreeNode::Call { callee, .. } => self.get_kind(callee),
        }
    }
}
