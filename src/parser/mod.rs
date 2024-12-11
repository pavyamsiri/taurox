use crate::{
    expression::{
        BinaryOperator, ExpressionTree, ExpressionTreeAtom, ExpressionTreeNode,
        ExpressionTreeNodeRef, ExpressionTreeWithRoot, UnaryOperator,
    },
    lexer::{Lexer, LexicalError},
    token::{Token, TokenKind},
};

use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum ParserErrorKind {
    #[error("Expected {expected:?} but got token {actual:?}.")]
    UnexpectedToken {
        actual: TokenKind,
        expected: TokenKind,
    },
    #[error("Expected an operator but got token {0:?}.")]
    NonOperator(TokenKind),
    #[error("Expected an left hand side to expression but got token {0:?}.")]
    NonExpression(TokenKind),
    #[error("Expected a non-EOF token.")]
    UnexpectedEof,
    #[error("Encountered a lexer error {0}.")]
    LexicalError(#[from] LexicalError),
}

#[derive(Debug, Error, Clone)]
#[error("[line {line}] {kind}")]
pub struct ParserError {
    #[source]
    pub kind: ParserErrorKind,
    pub line: u32,
}

pub struct Parser<'src> {
    lexer: Lexer<'src>,
    lookahead: Option<Result<Token, ParserError>>,
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            lexer: Lexer::new(source),
            lookahead: None,
        }
    }

    fn peek(&mut self) -> Result<Token, ParserError> {
        match self.lookahead {
            Some(ref token_or_error) => token_or_error.clone(),
            None => {
                let next_token = self.next_token();
                self.lookahead = Some(next_token.clone());
                next_token
            }
        }
    }

    fn next_token(&mut self) -> Result<Token, ParserError> {
        match self.lookahead.take() {
            Some(token_or_error) => token_or_error,
            None => {
                let token_or_error = self.lexer.next_token();
                token_or_error.map_err(|e| ParserError {
                    kind: ParserErrorKind::LexicalError(e.clone()),
                    line: e.line,
                })
            }
        }
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Token, ParserError> {
        let next_token = self.next_token()?;
        if next_token.kind != expected {
            Err(ParserError {
                line: next_token.line,
                kind: ParserErrorKind::UnexpectedToken {
                    actual: next_token.kind,
                    expected,
                },
            })
        } else {
            Ok(next_token)
        }
    }
}

// Pratt parser for expressions
impl<'src> Parser<'src> {
    pub fn parse_expression(&mut self) -> Result<ExpressionTreeWithRoot, ParserError> {
        let mut tree = ExpressionTree::new();
        let res = self.parse_expression_pratt(0, &mut tree)?;

        Ok(ExpressionTreeWithRoot::new(tree, res)
            .expect("Root was obtained from the tree itself so it must be valid."))
    }

    fn peek_binary_operator(&mut self) -> Result<Option<BinaryOperator>, ParserError> {
        let token = self.peek()?;

        match token.kind {
            TokenKind::Plus => Ok(Some(BinaryOperator::Add)),
            TokenKind::Minus => Ok(Some(BinaryOperator::Subtract)),
            TokenKind::Star => Ok(Some(BinaryOperator::Multiply)),
            TokenKind::Slash => Ok(Some(BinaryOperator::Divide)),
            TokenKind::LessThan => Ok(Some(BinaryOperator::LessThan)),
            TokenKind::LessThanEqual => Ok(Some(BinaryOperator::LessThanEqual)),
            TokenKind::GreaterThan => Ok(Some(BinaryOperator::GreaterThan)),
            TokenKind::GreaterThanEqual => Ok(Some(BinaryOperator::GreaterThanEqual)),
            TokenKind::EqualEqual => Ok(Some(BinaryOperator::EqualEqual)),
            TokenKind::BangEqual => Ok(Some(BinaryOperator::BangEqual)),
            _ => Ok(None),
        }
    }

    fn expect_left_expression(
        &mut self,
        tree: &mut ExpressionTree,
    ) -> Result<ExpressionTreeNodeRef, ParserError> {
        let token = self.next_token()?;

        if matches!(token.kind, TokenKind::Eof) {
            return Err(ParserError {
                kind: ParserErrorKind::UnexpectedEof,
                line: token.line,
            });
        }

        let lexeme = self
            .lexer
            .get_lexeme(&token.span)
            .expect("Lexed token has a valid span");

        let node = match token.kind {
            TokenKind::NumericLiteral => {
                let node = ExpressionTreeNode::Atom(ExpressionTreeAtom::Number(
                    lexeme
                        .parse::<f64>()
                        .expect("Numeric literal tokens can always be parsed into a `f64`."),
                ));
                tree.push(node)
            }
            TokenKind::Ident => {
                let node = ExpressionTreeNode::Atom(ExpressionTreeAtom::Identifier(lexeme.into()));
                tree.push(node)
            }
            TokenKind::StringLiteral => {
                let value = lexeme
                    .get(1..lexeme.len() - 1)
                    .expect("String literal tokens are at least length 2.");
                let node =
                    ExpressionTreeNode::Atom(ExpressionTreeAtom::StringLiteral(value.into()));
                tree.push(node)
            }
            TokenKind::KeywordNil => {
                let node = ExpressionTreeNode::Atom(ExpressionTreeAtom::Nil);
                tree.push(node)
            }
            TokenKind::KeywordTrue => {
                let node = ExpressionTreeNode::Atom(ExpressionTreeAtom::Bool(true));
                tree.push(node)
            }
            TokenKind::KeywordFalse => {
                let node = ExpressionTreeNode::Atom(ExpressionTreeAtom::Bool(false));
                tree.push(node)
            }
            // Unary operators
            TokenKind::Minus => {
                let operator = UnaryOperator::Minus;
                let rbp = operator.get_binding_power();
                let rhs = self.parse_expression_pratt(rbp, tree)?;
                tree.push(ExpressionTreeNode::Unary { operator, rhs })
            }
            TokenKind::Bang => {
                let operator = UnaryOperator::Bang;
                let rbp = operator.get_binding_power();
                let rhs = self.parse_expression_pratt(rbp, tree)?;
                tree.push(ExpressionTreeNode::Unary { operator, rhs })
            }
            // Bracketed expression
            TokenKind::LeftParenthesis => {
                let inner = self.parse_expression_pratt(0, tree)?;
                self.expect(TokenKind::RightParenthesis)?;
                tree.push(ExpressionTreeNode::Group { inner })
            }
            kind => {
                return Err(ParserError {
                    kind: ParserErrorKind::NonExpression(kind),
                    line: token.line,
                })
            }
        };
        Ok(node)
    }

    fn parse_expression_pratt(
        &mut self,
        min_bp: u8,
        tree: &mut ExpressionTree,
    ) -> Result<ExpressionTreeNodeRef, ParserError> {
        let mut lhs = self.expect_left_expression(tree)?;

        loop {
            if let Some(operator) = self.peek_binary_operator()? {
                let (lbp, rbp) = operator.get_binding_power();
                if lbp < min_bp {
                    break;
                }
                let _ = self.next_token()?;

                let rhs = self.parse_expression_pratt(rbp, tree)?;
                lhs = tree.push(ExpressionTreeNode::Binary { operator, lhs, rhs });
                continue;
            }
            break;
        }
        Ok(lhs)
    }
}
