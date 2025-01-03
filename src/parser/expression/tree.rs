use super::{InfixOperator, InfixShortCircuitOperator, PrefixOperator};
use crate::{
    lexer::{Span, TokenKind},
    string::{Ident, IdentName},
};

#[derive(Debug, Clone)]
pub enum ExpressionAtomKind {
    Number(f64),
    Bool(bool),
    Nil,
    Identifier(IdentName),
    StringLiteral(IdentName),
    This,
    Super(IdentName),
}

#[derive(Debug, Clone)]
pub struct ExpressionAtom {
    pub kind: ExpressionAtomKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct ExpressionNodeRef(u32);
#[derive(Debug, Clone)]
pub enum ExpressionNode {
    Atom(ExpressionAtom),
    Group {
        inner: ExpressionNodeRef,
    },
    Prefix {
        operator: PrefixOperator,
        rhs: ExpressionNodeRef,
    },
    Infix {
        operator: InfixOperator,
        lhs: ExpressionNodeRef,
        rhs: ExpressionNodeRef,
    },
    InfixAssignment {
        lhs: Ident,
        rhs: ExpressionNodeRef,
    },
    InfixShortCircuit {
        operator: InfixShortCircuitOperator,
        lhs: ExpressionNodeRef,
        rhs: ExpressionNodeRef,
    },
    Call {
        callee: ExpressionNodeRef,
        arguments: Vec<ExpressionNodeRef>,
    },
    Get {
        object: ExpressionNodeRef,
        name: Ident,
    },
    Set {
        object: ExpressionNodeRef,
        name: Ident,
        value: ExpressionNodeRef,
    },
}

impl ExpressionNode {
    pub fn get_l_value(&self) -> Option<&str> {
        match self {
            ExpressionNode::Atom(ExpressionAtom {
                kind: ExpressionAtomKind::Identifier(name),
                ..
            }) => Some(&name),
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
    inner: IncompleteExpression,
    root: ExpressionNodeRef,
}

impl IncompleteExpression {
    pub fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    pub fn push(&mut self, node: ExpressionNode) -> ExpressionNodeRef {
        self.nodes.push(node);
        ExpressionNodeRef(self.nodes.len() as u32 - 1)
    }

    pub fn get_l_value(&self, index: ExpressionNodeRef) -> Option<&str> {
        self.get_node(index).and_then(|n| n.get_l_value())
    }

    pub fn get_node(&self, index: ExpressionNodeRef) -> Option<&ExpressionNode> {
        self.nodes.get(index.0 as usize)
    }

    pub fn get_span(&self, node: ExpressionNodeRef) -> Option<Span> {
        const MSG: &'static str = "Nodes came from in-tree expressions so they must exist";
        let node = self.nodes.get(node.0 as usize)?;
        match node {
            ExpressionNode::Atom(ExpressionAtom { span, .. }) => Some(*span),
            ExpressionNode::Prefix { rhs, .. } => self.get_span(*rhs),
            ExpressionNode::Infix { lhs, rhs, .. } => {
                let left = self.get_span(*lhs).expect(MSG);
                let right = self.get_span(*rhs).expect(MSG);
                Some(left.merge(&right))
            }
            ExpressionNode::InfixAssignment {
                lhs: Ident { span, .. },
                rhs,
                ..
            } => {
                let right = self.get_span(*rhs).expect(MSG);
                Some(span.merge(&right))
            }
            ExpressionNode::InfixShortCircuit { lhs, rhs, .. } => {
                let left = self.get_span(*lhs).expect(MSG);
                let right = self.get_span(*rhs).expect(MSG);
                Some(left.merge(&right))
            }
            ExpressionNode::Group { inner } => {
                let inner = self.get_span(*inner);
                inner.and_then(|i| Some(i.expand(1)))
            }
            ExpressionNode::Call { callee, arguments } => {
                let left = self.get_span(*callee).expect(MSG);
                if arguments.is_empty() {
                    Some(left.right_expand(2))
                } else {
                    let mut current = left;
                    for arg in arguments.iter() {
                        current = current.merge(&self.get_span(*arg).expect(MSG));
                    }
                    Some(current.right_expand(1))
                }
            }
            ExpressionNode::Get { object, name } => {
                let span = self.get_span(*object).expect(MSG).merge(&name.span);
                Some(span)
            }
            ExpressionNode::Set { object, value, .. } => {
                let leftmost = self.get_span(*object).expect(MSG);
                let rightmost = self.get_span(*value).expect(MSG);
                Some(leftmost.merge(&rightmost))
            }
        }
    }

    pub fn get_kind(&self, node: ExpressionNodeRef) -> Option<TokenKind> {
        let node = self.nodes.get(node.0 as usize)?;
        match node {
            ExpressionNode::Atom(ExpressionAtom { kind, .. }) => match kind {
                ExpressionAtomKind::Number(_) => Some(TokenKind::NumericLiteral),
                ExpressionAtomKind::Bool(true) => Some(TokenKind::KeywordTrue),
                ExpressionAtomKind::Bool(false) => Some(TokenKind::KeywordFalse),
                ExpressionAtomKind::Nil => Some(TokenKind::KeywordNil),
                ExpressionAtomKind::Identifier(_) => Some(TokenKind::Ident),
                ExpressionAtomKind::StringLiteral(_) => Some(TokenKind::StringLiteral),
                ExpressionAtomKind::This => Some(TokenKind::KeywordThis),
                ExpressionAtomKind::Super(_) => Some(TokenKind::KeywordSuper),
            },
            ExpressionNode::Prefix { rhs, .. } => self.get_kind(*rhs),
            ExpressionNode::Infix { lhs, .. } => self.get_kind(*lhs),
            ExpressionNode::InfixAssignment { rhs, .. } => self.get_kind(*rhs),
            ExpressionNode::InfixShortCircuit { lhs, .. } => self.get_kind(*lhs),
            ExpressionNode::Group { inner } => self.get_kind(*inner),
            ExpressionNode::Call { callee, .. } => self.get_kind(*callee),
            ExpressionNode::Get { object, .. } => self.get_kind(*object),
            ExpressionNode::Set { object, .. } => self.get_kind(*object),
        }
    }
}

impl Expression {
    pub fn new(tree: IncompleteExpression, root: ExpressionNodeRef) -> Option<Self> {
        if !(0..tree.nodes.len()).contains(&(root.0 as usize)) {
            None
        } else {
            Some(Self { inner: tree, root })
        }
    }

    pub fn get_root_ref(&self) -> ExpressionNodeRef {
        self.root
    }

    pub fn get_root(&self) -> &ExpressionNode {
        self.get_node(self.get_root_ref())
            .expect("The root exists within the tree.")
    }

    pub fn get_node(&self, node: ExpressionNodeRef) -> Option<&ExpressionNode> {
        self.inner.get_node(node)
    }

    pub fn get_span(&self) -> Span {
        self.inner
            .get_span(self.get_root_ref())
            .expect("The root exists within the tree")
    }

    pub fn get_subspan(&self, node: ExpressionNodeRef) -> Option<Span> {
        self.inner.get_span(node)
    }

    pub fn get_kind(&self, node: ExpressionNodeRef) -> Option<TokenKind> {
        self.inner.get_kind(node)
    }
}
