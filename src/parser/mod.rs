mod error;
pub mod expression;
pub mod formatter;
pub mod statement;

use crate::lexer::{Lexer, Token, TokenKind};
use compact_str::{CompactString, ToCompactString};
pub use error::ParserError;
use error::ParserErrorKind;
use expression::{
    BinaryAssignmentOperator, BinaryOperator, BinaryShortCircuitOperator, ExpressionTree,
    ExpressionTreeAtom, ExpressionTreeAtomKind, ExpressionTreeNode, ExpressionTreeNodeRef,
    ExpressionTreeWithRoot, PostfixOperator, UnaryOperator,
};
use statement::{Declaration, Initializer, NonDeclaration, Statement};

#[derive(Debug)]
pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    pub fn get_statement(&self, index: usize) -> Option<&Statement> {
        self.statements.get(index)
    }
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

    fn expect_ident(&mut self) -> Result<CompactString, ParserError> {
        let next_token = self.next_token()?;
        if next_token.kind != TokenKind::Ident {
            return Err(ParserError {
                line: next_token.line,
                kind: ParserErrorKind::UnexpectedToken {
                    actual: next_token.kind,
                    expected: TokenKind::Ident,
                },
            });
        }

        let name = self
            .lexer
            .get_lexeme(&next_token.span)
            .expect("Token came from lexer so it is valid.");

        Ok(name.to_compact_string())
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

    fn eat_if(&mut self, next: TokenKind) -> Result<Option<Token>, ParserError> {
        let next_token = self.peek()?;
        if next_token.kind != next {
            Ok(None)
        } else {
            let _ = self.next_token().expect("Just peeked.");
            Ok(Some(next_token))
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

    fn peek_infix_operator(&mut self) -> Result<Option<BinaryOperator>, ParserError> {
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

    fn peek_postfix_operator(&mut self) -> Result<Option<PostfixOperator>, ParserError> {
        let token = self.peek()?;

        match token.kind {
            TokenKind::LeftParenthesis => Ok(Some(PostfixOperator::Call)),
            _ => Ok(None),
        }
    }
    fn peek_infix_assignment_operator(
        &mut self,
    ) -> Result<Option<BinaryAssignmentOperator>, ParserError> {
        let token = self.peek()?;

        match token.kind {
            TokenKind::Equal => Ok(Some(BinaryAssignmentOperator::Assign)),
            _ => Ok(None),
        }
    }

    fn peek_infix_short_circuit_operator(
        &mut self,
    ) -> Result<Option<BinaryShortCircuitOperator>, ParserError> {
        let token = self.peek()?;

        match token.kind {
            TokenKind::KeywordAnd => Ok(Some(BinaryShortCircuitOperator::And)),
            TokenKind::KeywordOr => Ok(Some(BinaryShortCircuitOperator::Or)),
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
                let node = ExpressionTreeNode::Atom(ExpressionTreeAtom {
                    kind: ExpressionTreeAtomKind::Number(
                        lexeme
                            .parse()
                            .expect("Numeric literal tokens are valid `f64`"),
                    ),
                    line: token.line,
                });
                tree.push(node)
            }
            TokenKind::Ident => {
                let node = ExpressionTreeNode::Atom(ExpressionTreeAtom {
                    kind: ExpressionTreeAtomKind::Identifier(lexeme.into()),
                    line: token.line,
                });
                tree.push(node)
            }
            TokenKind::StringLiteral => {
                let value = lexeme
                    .get(1..lexeme.len() - 1)
                    .expect("String literal tokens are at least length 2.");
                let node = ExpressionTreeNode::Atom(ExpressionTreeAtom {
                    kind: ExpressionTreeAtomKind::StringLiteral(value.into()),
                    line: token.line,
                });
                tree.push(node)
            }
            TokenKind::KeywordNil => {
                let node = ExpressionTreeNode::Atom(ExpressionTreeAtom {
                    kind: ExpressionTreeAtomKind::Nil,
                    line: token.line,
                });
                tree.push(node)
            }
            TokenKind::KeywordTrue => {
                let node = ExpressionTreeNode::Atom(ExpressionTreeAtom {
                    kind: ExpressionTreeAtomKind::Bool(true),
                    line: token.line,
                });
                tree.push(node)
            }
            TokenKind::KeywordFalse => {
                let node = ExpressionTreeNode::Atom(ExpressionTreeAtom {
                    kind: ExpressionTreeAtomKind::Bool(false),
                    line: token.line,
                });
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
            if let Some(operator) = self.peek_postfix_operator()? {
                let lbp = operator.get_binding_power();
                if lbp < min_bp {
                    break;
                }

                // Special case
                match operator {
                    PostfixOperator::Call => {
                        let _ = self.expect(TokenKind::LeftParenthesis)?;
                        // Parse arguments/expressions
                        let mut arguments = Vec::new();

                        // Hit the end of the argument list right away
                        if let Some(_) = self.eat_if(TokenKind::RightParenthesis)? {
                            lhs = tree.push(ExpressionTreeNode::Call {
                                callee: lhs,
                                arguments: Vec::new(),
                            })
                        }
                        // Collect arguments
                        else {
                            // NOTE: Only support up to 255 arguments
                            for _ in 0..255 {
                                let argument = self.parse_expression_pratt(0, tree)?;
                                arguments.push(argument);

                                // Hit the end of the argument list
                                if let Some(_) = self.eat_if(TokenKind::RightParenthesis)? {
                                    break;
                                }

                                let _ = self.expect(TokenKind::Comma)?;
                            }
                            lhs = tree.push(ExpressionTreeNode::Call {
                                callee: lhs,
                                arguments,
                            })
                        }
                    }
                }
            }

            if let Some(operator) = self.peek_infix_assignment_operator()? {
                let place = {
                    let lhs_node = tree
                        .get_node(&lhs)
                        .expect("The node ref is valid because it came from pushing to the tree.");
                    lhs_node.get_l_value().ok_or(ParserError {
                        kind: ParserErrorKind::InvalidLValue(tree.get_kind(&lhs).expect(
                            "The node ref is valid because it came from pushing to the tree.",
                        )),
                        line: tree.get_line(&lhs).expect(
                            "The node ref is valid because it came from pushing to the tree.",
                        ),
                    })?
                };

                let (lbp, rbp) = operator.get_binding_power();
                if lbp < min_bp {
                    break;
                }
                let _ = self.next_token()?;

                let rhs = self.parse_expression_pratt(rbp, tree)?;
                lhs = tree.push(ExpressionTreeNode::BinaryAssignment { lhs: place, rhs });
                continue;
            }

            if let Some(operator) = self.peek_infix_short_circuit_operator()? {
                let (lbp, rbp) = operator.get_binding_power();
                if lbp < min_bp {
                    break;
                }
                let _ = self.next_token()?;

                let rhs = self.parse_expression_pratt(rbp, tree)?;
                lhs = tree.push(ExpressionTreeNode::BinaryShortCircuit { operator, lhs, rhs });
                continue;
            }

            if let Some(operator) = self.peek_infix_operator()? {
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

// Parse program/statements
impl<'src> Parser<'src> {
    pub fn parse(&mut self) -> Result<Program, ParserError> {
        let mut statements = Vec::new();

        while let Some(statement) = self.parse_statement()? {
            statements.push(statement);
        }

        Ok(Program { statements })
    }

    pub fn parse_statement(&mut self) -> Result<Option<Statement>, ParserError> {
        let first = self.peek()?;
        let line = first.line;
        let statement = match first.kind {
            TokenKind::KeywordPrint => self.parse_print_statement()?,
            TokenKind::KeywordVar => self.parse_variable_declaration()?,
            TokenKind::LeftBrace => self.parse_block_statement(line)?,
            TokenKind::KeywordIf => self.parse_if_statement(line)?,
            TokenKind::KeywordWhile => self.parse_while_statement(line)?,
            TokenKind::KeywordFor => self.parse_for_statement(line)?,
            TokenKind::KeywordFun => self.parse_function_declaration(line)?,
            TokenKind::Eof => {
                return Ok(None);
            }
            _ => {
                let expr = self.parse_expression()?;
                let _ = self.expect(TokenKind::Semicolon)?;
                Statement::NonDeclaration(NonDeclaration::Expression(expr))
            }
        };
        Ok(Some(statement))
    }

    fn parse_print_statement(&mut self) -> Result<Statement, ParserError> {
        let _ = self.expect(TokenKind::KeywordPrint)?;
        let rhs = self.parse_expression()?;
        let statement = Statement::NonDeclaration(NonDeclaration::Print(rhs));
        let _ = self.expect(TokenKind::Semicolon)?;
        Ok(statement)
    }

    fn parse_variable_declaration(&mut self) -> Result<Statement, ParserError> {
        let _ = self.expect(TokenKind::KeywordVar)?;
        let place = self.expect_ident()?;

        let mut initial = None;

        // Assignment
        if let Some(_) = self.eat_if(TokenKind::Equal)? {
            let rhs = self.parse_expression()?;
            initial = Some(rhs);
        }

        let statement = Statement::Declaration(Declaration::Variable {
            name: place.clone(),
            initial,
        });
        let _ = self.expect(TokenKind::Semicolon)?;
        Ok(statement)
    }

    fn parse_function_declaration(&mut self, line: u32) -> Result<Statement, ParserError> {
        let _ = self.expect(TokenKind::KeywordFun)?;
        let name = self.expect_ident()?;
        let _ = self.expect(TokenKind::LeftParenthesis)?;

        // No parameters
        let parameters = if let Some(_) = self.eat_if(TokenKind::RightParenthesis)? {
            Vec::new()
        } else {
            let mut parameters = Vec::new();
            // NOTE(pavyamsiri): Only allow up to 255 parameters
            for _ in 0..255 {
                let parameter = self.expect_ident()?;
                parameters.push(parameter);

                if let Some(_) = self.eat_if(TokenKind::RightParenthesis)? {
                    break;
                }
                let _ = self.expect(TokenKind::Comma)?;
            }
            parameters
        };

        let body = {
            let body_statement = self.parse_statement()?.ok_or(ParserError {
                kind: ParserErrorKind::UnexpectedEof,
                line,
            })?;
            match body_statement {
                Statement::NonDeclaration(NonDeclaration::Block(body)) => body,
                s => {
                    return Err(ParserError {
                        kind: ParserErrorKind::NonBlock(s),
                        line,
                    })
                }
            }
        };

        Ok(Statement::Declaration(Declaration::Function {
            name,
            parameters,
            body,
        }))
    }

    fn parse_block_statement(&mut self, line: u32) -> Result<Statement, ParserError> {
        let _ = self.expect(TokenKind::LeftBrace)?;
        let mut statements = Vec::new();

        // Check if we are empty
        if let Some(_) = self.eat_if(TokenKind::RightBrace)? {
            Ok(Statement::NonDeclaration(NonDeclaration::Block(statements)))
        } else {
            loop {
                let statement = self.parse_statement()?.ok_or(ParserError {
                    kind: ParserErrorKind::UnexpectedEof,
                    line,
                })?;
                statements.push(statement);
                if let Some(_) = self.eat_if(TokenKind::RightBrace)? {
                    break;
                }
            }
            Ok(Statement::NonDeclaration(NonDeclaration::Block(statements)))
        }
    }

    fn parse_if_statement(&mut self, line: u32) -> Result<Statement, ParserError> {
        let _ = self.expect(TokenKind::KeywordIf)?;
        let _ = self.expect(TokenKind::LeftParenthesis)?;
        let condition = self.parse_expression()?;
        let _ = self.expect(TokenKind::RightParenthesis)?;
        let success = self.parse_statement()?.ok_or(ParserError {
            kind: ParserErrorKind::UnexpectedEof,
            line,
        })?;

        let failure = if let Some(_) = self.eat_if(TokenKind::KeywordElse)? {
            Some(self.parse_statement()?.ok_or(ParserError {
                kind: ParserErrorKind::UnexpectedEof,
                line,
            })?)
        } else {
            None
        };

        Ok(Statement::NonDeclaration(NonDeclaration::If {
            condition,
            success: Box::new(success),
            failure: Box::new(failure),
        }))
    }

    fn parse_while_statement(&mut self, line: u32) -> Result<Statement, ParserError> {
        let _ = self.expect(TokenKind::KeywordWhile)?;
        let _ = self.expect(TokenKind::LeftParenthesis)?;
        let condition = self.parse_expression()?;
        let _ = self.expect(TokenKind::RightParenthesis)?;
        let body = self.parse_statement()?.ok_or(ParserError {
            kind: ParserErrorKind::UnexpectedEof,
            line,
        })?;

        Ok(Statement::NonDeclaration(NonDeclaration::While {
            condition,
            body: Box::new(body),
        }))
    }

    fn parse_for_statement(&mut self, line: u32) -> Result<Statement, ParserError> {
        let _ = self.expect(TokenKind::KeywordFor)?;
        let _ = self.expect(TokenKind::LeftParenthesis)?;

        // Parse initializer
        let initializer: Option<Initializer> = {
            if let Some(_) = self.eat_if(TokenKind::Semicolon)? {
                None
            }
            // Initializer exists
            else {
                let statement = self.parse_statement()?.ok_or(ParserError {
                    kind: ParserErrorKind::UnexpectedEof,
                    line,
                })?;
                let initializer = match statement {
                    Statement::Declaration(Declaration::Variable { name, initial }) => {
                        Some(Initializer::VarDecl { name, initial })
                    }
                    Statement::NonDeclaration(NonDeclaration::Expression(expr)) => {
                        Some(Initializer::Expression(expr))
                    }
                    _ => None,
                };
                initializer
            }
        };

        // Parse condition
        let condition: Option<ExpressionTreeWithRoot> = {
            if let Some(_) = self.eat_if(TokenKind::Semicolon)? {
                None
            }
            // Condition exists
            else {
                let expr = self.parse_expression()?;
                self.expect(TokenKind::Semicolon)?;
                Some(expr)
            }
        };

        // Parse increment
        let increment: Option<ExpressionTreeWithRoot> = {
            if let Some(_) = self.eat_if(TokenKind::RightParenthesis)? {
                None
            }
            // Increment exists
            else {
                let expr = self.parse_expression()?;
                self.expect(TokenKind::RightParenthesis)?;
                Some(expr)
            }
        };

        // Parse body
        let body = self.parse_statement()?.ok_or(ParserError {
            kind: ParserErrorKind::UnexpectedEof,
            line,
        })?;

        let body = match body {
            Statement::Declaration(declaration) => Err(ParserError {
                kind: ParserErrorKind::InvalidNonDeclaration(declaration),
                line,
            }),
            Statement::NonDeclaration(non_declaration) => Ok(non_declaration),
        }?;

        Ok(Statement::NonDeclaration(NonDeclaration::For {
            initializer,
            condition,
            increment,
            body: Box::new(body),
        }))
    }
}
