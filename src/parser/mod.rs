use crate::{
    expression::{
        ExpressionOperator, ExpressionTree, ExpressionTreeAtom, ExpressionTreeNode,
        ExpressionTreeNodeRef, ExpressionTreeWithRoot,
    },
    lexer::{Lexer, LexicalError},
    token::{Token, TokenKind},
};

use thiserror::Error;

#[derive(Debug, Error, Clone)]
pub enum ParserErrorKind {
    #[error("Expected {expected:?} but got token {actual:?}")]
    UnexpectedToken {
        actual: TokenKind,
        expected: TokenKind,
    },
    #[error("Expected an operator but got token {0:?}")]
    NonOperator(TokenKind),
    #[error("Encountered a lexer error {0}")]
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
                line: 0,
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

    fn peek_operator(&mut self) -> Result<Option<ExpressionOperator>, ParserError> {
        let token = self.peek()?;

        match token.kind {
            TokenKind::Plus => Ok(Some(ExpressionOperator::Add)),
            TokenKind::Minus => Ok(Some(ExpressionOperator::Subtract)),
            TokenKind::Star => Ok(Some(ExpressionOperator::Multiply)),
            TokenKind::Slash => Ok(Some(ExpressionOperator::Divide)),
            TokenKind::Eof => Ok(None),
            _ => Err(ParserError {
                kind: ParserErrorKind::NonOperator(token.kind),
                line: 0,
            }),
        }
    }

    fn next_expression_atom(&mut self) -> Result<ExpressionTreeAtom, ParserError> {
        let token = self.next_token()?;

        let atom = match token.kind {
            TokenKind::NumericLiteral => {
                let lexeme = self
                    .lexer
                    .get_lexeme(&token.span)
                    .expect("Lexed token has a valid span");
                ExpressionTreeAtom::Number(
                    lexeme
                        .parse::<f64>()
                        .expect("Numeric literal tokens can always be parsed into a `f64`."),
                )
            }
            TokenKind::StringLiteral => {
                let lexeme = self
                    .lexer
                    .get_lexeme(&token.span)
                    .expect("Lexed token has a valid span");
                let value = lexeme
                    .get(1..lexeme.len() - 1)
                    .expect("String literal tokens are at least length 2.");
                ExpressionTreeAtom::StringLiteral(value.into())
            }
            TokenKind::Ident => ExpressionTreeAtom::Identifier(
                self.lexer
                    .get_lexeme(&token.span)
                    .expect("Lexed token has a valid span.")
                    .into(),
            ),
            _ => panic!(),
        };
        Ok(atom)
    }

    fn parse_expression_pratt(
        &mut self,
        min_bp: u8,
        tree: &mut ExpressionTree,
    ) -> Result<ExpressionTreeNodeRef, ParserError> {
        let mut lhs = tree.push(ExpressionTreeNode::Atom(self.next_expression_atom()?));

        loop {
            let Some(operator) = self.peek_operator()? else {
                break;
            };

            let (lbp, rbp) = operator.get_binding_power();
            if lbp < min_bp {
                break;
            }
            let _ = self.next_token()?;

            let rhs = self.parse_expression_pratt(rbp, tree)?;
            lhs = tree.push(ExpressionTreeNode::Expression(operator, vec![lhs, rhs]));
        }
        Ok(lhs)
    }
}
