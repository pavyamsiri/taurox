use compact_str::CompactString;

use crate::lexer::TokenKind;

use super::{BinaryOperator, BinaryShortCircuitOperator, UnaryOperator};

#[derive(Debug, Clone)]
pub enum ExpressionAtomKind {
    Number(f64),
    Bool(bool),
    Nil,
    Identifier(CompactString),
    StringLiteral(CompactString),
}

#[derive(Debug, Clone)]
pub struct ExpressionAtom {
    pub kind: ExpressionAtomKind,
    pub line: u32,
}

#[derive(Debug, Clone, Copy)]
pub struct ExpressionNodeRef(pub u32);
#[derive(Debug, Clone)]
pub enum ExpressionNode {
    Atom(ExpressionAtom),
    Group {
        inner: ExpressionNodeRef,
    },
    Unary {
        operator: UnaryOperator,
        rhs: ExpressionNodeRef,
    },
    Binary {
        operator: BinaryOperator,
        lhs: ExpressionNodeRef,
        rhs: ExpressionNodeRef,
    },
    BinaryAssignment {
        lhs: CompactString,
        rhs: ExpressionNodeRef,
    },
    BinaryShortCircuit {
        operator: BinaryShortCircuitOperator,
        lhs: ExpressionNodeRef,
        rhs: ExpressionNodeRef,
    },
    Call {
        callee: ExpressionNodeRef,
        arguments: Vec<ExpressionNodeRef>,
    },
}

impl ExpressionNode {
    pub fn get_l_value(&self) -> Option<CompactString> {
        match self {
            ExpressionNode::Atom(ExpressionAtom {
                kind: ExpressionAtomKind::Identifier(name),
                ..
            }) => Some(name.clone()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct IncompleteExpression {
    nodes: Vec<ExpressionNode>,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub nodes: Vec<ExpressionNode>,
    pub root: ExpressionNodeRef,
}

impl IncompleteExpression {
    pub fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    pub fn push(&mut self, node: ExpressionNode) -> ExpressionNodeRef {
        self.nodes.push(node);
        ExpressionNodeRef(self.nodes.len() as u32 - 1)
    }

    pub fn get_node(&self, index: &ExpressionNodeRef) -> Option<&ExpressionNode> {
        self.nodes.get(index.0 as usize)
    }

    pub fn get_line(&self, node: &ExpressionNodeRef) -> Option<u32> {
        let node = self.nodes.get(node.0 as usize)?;
        match node {
            ExpressionNode::Atom(ExpressionAtom { line, .. }) => Some(*line),
            ExpressionNode::Unary { rhs, .. } => self.get_line(rhs),
            ExpressionNode::Binary { lhs, .. } => self.get_line(lhs),
            ExpressionNode::BinaryAssignment { rhs, .. } => self.get_line(rhs),
            ExpressionNode::BinaryShortCircuit { lhs, .. } => self.get_line(lhs),
            ExpressionNode::Group { inner } => self.get_line(inner),
            ExpressionNode::Call { callee, .. } => self.get_line(callee),
        }
    }

    pub fn get_kind(&self, node: &ExpressionNodeRef) -> Option<TokenKind> {
        let node = self.nodes.get(node.0 as usize)?;
        match node {
            ExpressionNode::Atom(ExpressionAtom { kind, .. }) => match kind {
                ExpressionAtomKind::Number(_) => Some(TokenKind::NumericLiteral),
                ExpressionAtomKind::Bool(true) => Some(TokenKind::KeywordTrue),
                ExpressionAtomKind::Bool(false) => Some(TokenKind::KeywordFalse),
                ExpressionAtomKind::Nil => Some(TokenKind::KeywordNil),
                ExpressionAtomKind::Identifier(_) => Some(TokenKind::Ident),
                ExpressionAtomKind::StringLiteral(_) => Some(TokenKind::StringLiteral),
            },
            ExpressionNode::Unary { rhs, .. } => self.get_kind(rhs),
            ExpressionNode::Binary { lhs, .. } => self.get_kind(lhs),
            ExpressionNode::BinaryAssignment { rhs, .. } => self.get_kind(rhs),
            ExpressionNode::BinaryShortCircuit { lhs, .. } => self.get_kind(lhs),
            ExpressionNode::Group { inner } => self.get_kind(inner),
            ExpressionNode::Call { callee, .. } => self.get_kind(callee),
        }
    }
}

impl Expression {
    pub fn new(tree: IncompleteExpression, root: ExpressionNodeRef) -> Option<Self> {
        if !(0..tree.nodes.len()).contains(&(root.0 as usize)) {
            None
        } else {
            Some(Self {
                nodes: tree.nodes,
                root,
            })
        }
    }

    pub fn get_root_ref(&self) -> ExpressionNodeRef {
        self.root
    }

    pub fn get_root(&self) -> &ExpressionNode {
        self.get_node(&self.get_root_ref())
            .expect("The root exists within the tree.")
    }

    pub fn get_node(&self, node: &ExpressionNodeRef) -> Option<&ExpressionNode> {
        self.nodes.get(node.0 as usize)
    }

    pub fn get_line(&self, node: &ExpressionNodeRef) -> Option<u32> {
        let node = self.nodes.get(node.0 as usize)?;
        match node {
            ExpressionNode::Atom(ExpressionAtom { line, .. }) => Some(*line),
            ExpressionNode::Unary { rhs, .. } => self.get_line(rhs),
            ExpressionNode::Binary { lhs, .. } => self.get_line(lhs),
            ExpressionNode::BinaryAssignment { rhs, .. } => self.get_line(rhs),
            ExpressionNode::BinaryShortCircuit { lhs, .. } => self.get_line(lhs),
            ExpressionNode::Group { inner } => self.get_line(inner),
            ExpressionNode::Call { callee, .. } => self.get_line(callee),
        }
    }

    pub fn get_kind(&self, node: &ExpressionNodeRef) -> Option<TokenKind> {
        let node = self.nodes.get(node.0 as usize)?;
        match node {
            ExpressionNode::Atom(ExpressionAtom { kind, .. }) => match kind {
                ExpressionAtomKind::Number(_) => Some(TokenKind::NumericLiteral),
                ExpressionAtomKind::Bool(true) => Some(TokenKind::KeywordTrue),
                ExpressionAtomKind::Bool(false) => Some(TokenKind::KeywordFalse),
                ExpressionAtomKind::Nil => Some(TokenKind::KeywordNil),
                ExpressionAtomKind::Identifier(_) => Some(TokenKind::Ident),
                ExpressionAtomKind::StringLiteral(_) => Some(TokenKind::StringLiteral),
            },
            ExpressionNode::Unary { rhs, .. } => self.get_kind(rhs),
            ExpressionNode::Binary { lhs, .. } => self.get_kind(lhs),
            ExpressionNode::BinaryAssignment { rhs, .. } => self.get_kind(rhs),
            ExpressionNode::BinaryShortCircuit { lhs, .. } => self.get_kind(lhs),
            ExpressionNode::Group { inner } => self.get_kind(inner),
            ExpressionNode::Call { callee, .. } => self.get_kind(callee),
        }
    }
}
