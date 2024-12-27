mod error;
pub mod expression;
pub mod formatter;
pub mod statement;

use crate::{
    lexer::{Lexer, LexicalError, LineBreaks, Span, Token, TokenKind},
    string::IdentifierString,
};
pub use error::ParserError;
use error::{
    ExpressionParserError, GeneralExpressionParserError, GeneralParserError, StatementParserError,
};
use expression::{
    AssignmentDestination, Expression, ExpressionAtom, ExpressionAtomKind, ExpressionNode,
    ExpressionNodeRef, IncompleteExpression, InfixAssignmentOperator, InfixOperator,
    InfixShortCircuitOperator, PostfixOperator, PrefixOperator,
};
use statement::{
    Declaration, DeclarationKind, Initializer, NonDeclaration, NonDeclarationKind, Statement,
};
use std::path::Path;

/// Only allow up to 255 parameters
const MAX_PARAMETERS: usize = 255;

#[derive(Debug)]
pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    pub fn get_statement(&self, index: usize) -> Option<&Statement> {
        self.statements.get(index)
    }

    pub fn as_slice(&self) -> &[Statement] {
        &self.statements
    }

    pub fn len(&self) -> usize {
        self.statements.len()
    }
}

pub struct Parser<'src> {
    lexer: Lexer<'src>,
    lookahead: Option<Result<Token, LexicalError>>,
}

// Lexer based helpers
impl<'src> Parser<'src> {
    fn create_eof_error(&self) -> GeneralParserError {
        GeneralParserError::UnexpectedEof(self.lexer.get_eof_span())
    }

    pub fn get_line_breaks(&self) -> LineBreaks {
        self.lexer.get_line_breaks()
    }

    pub fn get_source(&self) -> &'src str {
        &self.lexer.get_source()
    }

    pub fn get_path(&self) -> &'src Path {
        &self.lexer.get_path()
    }
}

// Parse program
impl<'src> Parser<'src> {
    pub fn parse(&mut self) -> Result<Program, ParserError> {
        let mut statements = Vec::new();

        while let Some(statement) = self.parse_statement()? {
            statements.push(statement);
        }

        Ok(Program { statements })
    }
}

// Parse statements
impl<'src> Parser<'src> {
    pub fn parse_statement(&mut self) -> Result<Option<Statement>, ParserError> {
        let first = self.peek()?;
        let statement = match first.kind {
            TokenKind::KeywordFun => self.parse_function_declaration()?,
            TokenKind::KeywordVar => self.parse_variable_declaration()?,
            TokenKind::KeywordPrint => self.parse_print_statement()?,
            TokenKind::LeftBrace => self.parse_block_statement()?,
            TokenKind::KeywordIf => self.parse_if_statement()?,
            TokenKind::KeywordWhile => self.parse_while_statement()?,
            TokenKind::KeywordFor => self.parse_for_statement()?,
            TokenKind::KeywordReturn => self.parse_return_statement()?,
            TokenKind::Eof => {
                return Ok(None);
            }
            _ => {
                let expr = self.parse_expression()?;
                let rightmost = self.expect(TokenKind::Semicolon)?;
                let span = expr.get_span().merge(&rightmost.span);
                Statement::NonDeclaration(NonDeclaration {
                    kind: NonDeclarationKind::Expression(expr),
                    span,
                })
            }
        };
        Ok(Some(statement))
    }

    fn parse_print_statement(&mut self) -> Result<Statement, ParserError> {
        let leftmost = self.expect(TokenKind::KeywordPrint)?;
        let rhs = self.parse_expression()?;
        let rightmost = self.expect(TokenKind::Semicolon)?;

        let span = leftmost.span.merge(&rightmost.span);
        let statement = Statement::NonDeclaration(NonDeclaration {
            kind: NonDeclarationKind::Print(rhs),
            span,
        });
        Ok(statement)
    }

    fn parse_variable_declaration(&mut self) -> Result<Statement, ParserError> {
        let leftmost = self.expect(TokenKind::KeywordVar)?;
        let (name, name_span) = self.expect_ident()?;
        let mut initial = None;
        // Assignment
        if let Some(_) = self.eat_if(TokenKind::Equal)? {
            let rhs = self.parse_expression()?;
            initial = Some(rhs);
        }
        let rightmost = self.expect(TokenKind::Semicolon)?;

        let span = leftmost.span.merge(&rightmost.span);
        let statement = Statement::Declaration(Declaration {
            kind: DeclarationKind::Variable {
                name: name.into(),
                initial,
                span: name_span,
            },
            span,
        });

        Ok(statement)
    }

    fn parse_function_declaration(&mut self) -> Result<Statement, ParserError> {
        let leftmost = self.expect(TokenKind::KeywordFun)?;
        let (name, name_span) = self.expect_ident()?;
        let _ = self.expect(TokenKind::LeftParenthesis)?;

        // No parameters
        let parameters = if let Some(_) = self.eat_if(TokenKind::RightParenthesis)? {
            Vec::new()
        } else {
            let mut parameters = Vec::new();
            for _ in 0..MAX_PARAMETERS {
                let (parameter, _) = self.expect_ident()?;
                parameters.push(parameter);

                if let Some(_) = self.eat_if(TokenKind::RightParenthesis)? {
                    break;
                }
                let _ = self.expect(TokenKind::Comma)?;
            }
            parameters
        };

        let (body, rightmost) = {
            let body_statement = self.parse_statement()?.ok_or(self.create_eof_error())?;
            match body_statement {
                Statement::NonDeclaration(NonDeclaration {
                    kind: NonDeclarationKind::Block(body),
                    span,
                }) => (body, span),
                s => Err(StatementParserError::NonBlock(s))?,
            }
        };

        let span = leftmost.span.merge(&rightmost);
        let decl = Statement::Declaration(Declaration {
            kind: DeclarationKind::Function {
                name,
                parameters,
                body,
                span: name_span,
            },
            span,
        });

        Ok(decl)
    }

    fn parse_block_statement(&mut self) -> Result<Statement, ParserError> {
        let leftmost = self.expect(TokenKind::LeftBrace)?;
        let mut statements = Vec::new();

        // Check if we are empty
        let rightmost = if let Some(rightmost) = self.eat_if(TokenKind::RightBrace)? {
            rightmost
        } else {
            let rightmost = 'stmts: loop {
                let statement = self.parse_statement()?.ok_or(self.create_eof_error())?;
                statements.push(statement);
                if let Some(rightmost) = self.eat_if(TokenKind::RightBrace)? {
                    break 'stmts rightmost;
                }
            };
            rightmost
        };
        let span = leftmost.span.merge(&rightmost.span);
        Ok(Statement::NonDeclaration(NonDeclaration {
            kind: NonDeclarationKind::Block(statements),
            span,
        }))
    }

    fn parse_if_statement(&mut self) -> Result<Statement, ParserError> {
        let leftmost = self.expect(TokenKind::KeywordIf)?;
        let _ = self.expect(TokenKind::LeftParenthesis)?;
        let condition = self.parse_expression()?;
        let _ = self.expect(TokenKind::RightParenthesis)?;
        let success = self.parse_statement()?.ok_or(self.create_eof_error())?;
        let mut rightmost = success.get_span();

        let failure = if let Some(_) = self.eat_if(TokenKind::KeywordElse)? {
            let stmt = self.parse_statement()?.ok_or(self.create_eof_error())?;
            rightmost = stmt.get_span();
            Some(stmt)
        } else {
            None
        };

        let span = leftmost.span.merge(&rightmost);
        Ok(Statement::NonDeclaration(NonDeclaration {
            kind: NonDeclarationKind::If {
                condition,
                success: Box::new(success),
                failure: Box::new(failure),
            },
            span,
        }))
    }

    fn parse_while_statement(&mut self) -> Result<Statement, ParserError> {
        let leftmost = self.expect(TokenKind::KeywordWhile)?;
        let _ = self.expect(TokenKind::LeftParenthesis)?;
        let condition = self.parse_expression()?;
        let _ = self.expect(TokenKind::RightParenthesis)?;
        let body = self.parse_statement()?.ok_or(self.create_eof_error())?;

        let span = leftmost.span.merge(&body.get_span());
        Ok(Statement::NonDeclaration(NonDeclaration {
            kind: NonDeclarationKind::While {
                condition,
                body: Box::new(body),
            },
            span,
        }))
    }

    fn parse_for_statement(&mut self) -> Result<Statement, ParserError> {
        let leftmost = self.expect(TokenKind::KeywordFor)?;
        let _ = self.expect(TokenKind::LeftParenthesis)?;

        // Parse initializer
        let initializer: Option<Initializer> = {
            if let Some(_) = self.eat_if(TokenKind::Semicolon)? {
                None
            }
            // Initializer exists
            else {
                let statement = self.parse_statement()?.ok_or(self.create_eof_error())?;
                let initializer = match statement {
                    Statement::Declaration(Declaration {
                        kind:
                            DeclarationKind::Variable {
                                name,
                                initial,
                                span,
                            },
                        ..
                    }) => Some(Initializer::VarDecl {
                        name,
                        initial,
                        span,
                    }),
                    Statement::NonDeclaration(NonDeclaration {
                        kind: NonDeclarationKind::Expression(expr),
                        ..
                    }) => Some(Initializer::Expression(expr)),
                    _ => None,
                };
                initializer
            }
        };

        // Parse condition
        let condition: Option<Expression> = {
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
        let increment: Option<Expression> = {
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
        let body = self.parse_statement()?.ok_or(self.create_eof_error())?;
        let body = match body {
            Statement::Declaration(declaration) => {
                Err(StatementParserError::InvalidNonDeclaration(declaration))
            }
            Statement::NonDeclaration(non_declaration) => Ok(non_declaration),
        }?;

        let span = leftmost.span.merge(&body.span);

        Ok(Statement::NonDeclaration(NonDeclaration {
            kind: NonDeclarationKind::For {
                initializer,
                condition,
                increment,
                body: Box::new(body),
            },
            span,
        }))
    }
}
enum PrattParseOutcome {
    NoOperator,
    Break,
    NewLHS(ExpressionNodeRef),
}

// Pratt parser for expressions
impl<'src> Parser<'src> {
    pub fn parse_expression(&mut self) -> Result<Expression, GeneralExpressionParserError> {
        let mut tree = IncompleteExpression::new();
        let res = self.parse_expression_pratt(0, &mut tree)?;

        Ok(Expression::new(tree, res)
            .expect("Root was obtained from the tree itself so it must be valid."))
    }

    fn peek_infix_operator(
        &mut self,
    ) -> Result<Option<InfixOperator>, GeneralExpressionParserError> {
        let token = self.peek()?;

        match token.kind {
            TokenKind::Plus => Ok(Some(InfixOperator::Add)),
            TokenKind::Minus => Ok(Some(InfixOperator::Subtract)),
            TokenKind::Star => Ok(Some(InfixOperator::Multiply)),
            TokenKind::Slash => Ok(Some(InfixOperator::Divide)),
            TokenKind::LessThan => Ok(Some(InfixOperator::LessThan)),
            TokenKind::LessThanEqual => Ok(Some(InfixOperator::LessThanEqual)),
            TokenKind::GreaterThan => Ok(Some(InfixOperator::GreaterThan)),
            TokenKind::GreaterThanEqual => Ok(Some(InfixOperator::GreaterThanEqual)),
            TokenKind::EqualEqual => Ok(Some(InfixOperator::EqualEqual)),
            TokenKind::BangEqual => Ok(Some(InfixOperator::BangEqual)),
            _ => Ok(None),
        }
    }

    fn parse_expression_pratt(
        &mut self,
        min_bp: u8,
        tree: &mut IncompleteExpression,
    ) -> Result<ExpressionNodeRef, GeneralExpressionParserError> {
        let mut lhs = self.expect_left_expression(tree)?;

        loop {
            match self.parse_postfix_expression_pratt(tree, lhs, min_bp)? {
                PrattParseOutcome::NoOperator => {}
                PrattParseOutcome::Break => {
                    break;
                }
                PrattParseOutcome::NewLHS(new_lhs) => {
                    lhs = new_lhs;
                    continue;
                }
            }

            match self.parse_infix_assignment_expression_pratt(tree, lhs, min_bp)? {
                PrattParseOutcome::NoOperator => {}
                PrattParseOutcome::Break => {
                    break;
                }
                PrattParseOutcome::NewLHS(new_lhs) => {
                    lhs = new_lhs;
                    continue;
                }
            }

            match self.parse_infix_short_circuit_expression_pratt(tree, lhs, min_bp)? {
                PrattParseOutcome::NoOperator => {}
                PrattParseOutcome::Break => {
                    break;
                }
                PrattParseOutcome::NewLHS(new_lhs) => {
                    lhs = new_lhs;
                    continue;
                }
            }

            match self.parse_infix_expression_pratt(tree, lhs, min_bp)? {
                PrattParseOutcome::NoOperator => {}
                PrattParseOutcome::Break => {
                    break;
                }
                PrattParseOutcome::NewLHS(new_lhs) => {
                    lhs = new_lhs;
                    continue;
                }
            }
            break;
        }
        Ok(lhs)
    }
}

// Pratt parser helpers
impl<'src> Parser<'src> {
    fn peek_postfix_operator(
        &mut self,
    ) -> Result<Option<PostfixOperator>, GeneralExpressionParserError> {
        let token = self.peek()?;

        match token.kind {
            TokenKind::LeftParenthesis => Ok(Some(PostfixOperator::Call)),
            _ => Ok(None),
        }
    }
    fn peek_infix_assignment_operator(
        &mut self,
    ) -> Result<Option<InfixAssignmentOperator>, GeneralExpressionParserError> {
        type Operator = InfixAssignmentOperator;
        let token = self.peek()?;

        match token.kind {
            TokenKind::Equal => Ok(Some(Operator::Assign)),
            _ => Ok(None),
        }
    }

    fn peek_infix_short_circuit_operator(
        &mut self,
    ) -> Result<Option<InfixShortCircuitOperator>, GeneralExpressionParserError> {
        type Operator = InfixShortCircuitOperator;
        let token = self.peek()?;

        match token.kind {
            TokenKind::KeywordAnd => Ok(Some(Operator::And)),
            TokenKind::KeywordOr => Ok(Some(Operator::Or)),
            _ => Ok(None),
        }
    }

    fn expect_left_expression(
        &mut self,
        tree: &mut IncompleteExpression,
    ) -> Result<ExpressionNodeRef, GeneralExpressionParserError> {
        let token = self.next_token()?;

        if matches!(token.kind, TokenKind::Eof) {
            return Err(self.create_eof_error().into());
        }

        let lexeme = self
            .lexer
            .get_lexeme(&token.span)
            .expect("Lexed token has a valid span");

        let node = match token.kind {
            TokenKind::NumericLiteral => {
                let node = ExpressionNode::Atom(ExpressionAtom {
                    kind: ExpressionAtomKind::Number(
                        lexeme
                            .parse()
                            .expect("Numeric literal tokens are valid `f64`"),
                    ),
                    span: token.span,
                });
                tree.push(node)
            }
            TokenKind::Ident => {
                let node = ExpressionNode::Atom(ExpressionAtom {
                    kind: ExpressionAtomKind::Identifier(lexeme.into()),
                    span: token.span,
                });
                tree.push(node)
            }
            TokenKind::StringLiteral => {
                let value = lexeme
                    .get(1..lexeme.len() - 1)
                    .expect("String literal tokens are at least length 2.");
                let node = ExpressionNode::Atom(ExpressionAtom {
                    kind: ExpressionAtomKind::StringLiteral(value.into()),
                    span: token.span,
                });
                tree.push(node)
            }
            TokenKind::KeywordNil => {
                let node = ExpressionNode::Atom(ExpressionAtom {
                    kind: ExpressionAtomKind::Nil,
                    span: token.span,
                });
                tree.push(node)
            }
            TokenKind::KeywordTrue => {
                let node = ExpressionNode::Atom(ExpressionAtom {
                    kind: ExpressionAtomKind::Bool(true),
                    span: token.span,
                });
                tree.push(node)
            }
            TokenKind::KeywordFalse => {
                let node = ExpressionNode::Atom(ExpressionAtom {
                    kind: ExpressionAtomKind::Bool(false),
                    span: token.span,
                });
                tree.push(node)
            }
            // Prefix operators
            TokenKind::Minus => {
                let operator = PrefixOperator::Minus;
                let rbp = operator.get_binding_power();
                let rhs = self.parse_expression_pratt(rbp, tree)?;
                tree.push(ExpressionNode::Prefix { operator, rhs })
            }
            TokenKind::Bang => {
                let operator = PrefixOperator::Bang;
                let rbp = operator.get_binding_power();
                let rhs = self.parse_expression_pratt(rbp, tree)?;
                tree.push(ExpressionNode::Prefix { operator, rhs })
            }
            // Bracketed expression
            TokenKind::LeftParenthesis => {
                let inner = self.parse_expression_pratt(0, tree)?;
                self.expect(TokenKind::RightParenthesis)?;
                tree.push(ExpressionNode::Group { inner })
            }
            _ => {
                return Err(ExpressionParserError::NonExpression(token).into());
            }
        };
        Ok(node)
    }
}

// Pratt parser operator handlers
impl<'src> Parser<'src> {
    fn parse_infix_assignment_expression_pratt(
        &mut self,
        tree: &mut IncompleteExpression,
        lhs: ExpressionNodeRef,
        min_bp: u8,
    ) -> Result<PrattParseOutcome, GeneralExpressionParserError> {
        const MSG: &'static str = "Caller must make sure `lhs` is a valid expression node ref.";
        if let Some(operator) = self.peek_infix_assignment_operator()? {
            let span = tree.get_span(lhs).expect(MSG);
            let place = tree
                .get_l_value(lhs)
                .ok_or(ExpressionParserError::InvalidLValue(Token {
                    kind: tree.get_kind(lhs).expect(MSG),
                    span,
                }))?;
            let place: IdentifierString = place.into();

            let (lbp, rbp) = operator.get_binding_power();
            if lbp < min_bp {
                return Ok(PrattParseOutcome::Break);
            }
            let _ = self.next_token()?;

            let rhs = self.parse_expression_pratt(rbp, tree)?;
            return Ok(PrattParseOutcome::NewLHS(tree.push(
                ExpressionNode::InfixAssignment {
                    lhs: AssignmentDestination { name: place, span },
                    rhs,
                },
            )));
        }
        Ok(PrattParseOutcome::NoOperator)
    }

    fn parse_infix_short_circuit_expression_pratt(
        &mut self,
        tree: &mut IncompleteExpression,
        lhs: ExpressionNodeRef,
        min_bp: u8,
    ) -> Result<PrattParseOutcome, GeneralExpressionParserError> {
        if let Some(operator) = self.peek_infix_short_circuit_operator()? {
            let (lbp, rbp) = operator.get_binding_power();
            if lbp < min_bp {
                return Ok(PrattParseOutcome::Break);
            }
            let _ = self.next_token()?;

            let rhs = self.parse_expression_pratt(rbp, tree)?;
            return Ok(PrattParseOutcome::NewLHS(
                tree.push(ExpressionNode::InfixShortCircuit { operator, lhs, rhs }),
            ));
        }
        Ok(PrattParseOutcome::NoOperator)
    }

    fn parse_infix_expression_pratt(
        &mut self,
        tree: &mut IncompleteExpression,
        lhs: ExpressionNodeRef,
        min_bp: u8,
    ) -> Result<PrattParseOutcome, GeneralExpressionParserError> {
        if let Some(operator) = self.peek_infix_operator()? {
            let (lbp, rbp) = operator.get_binding_power();
            if lbp < min_bp {
                return Ok(PrattParseOutcome::Break);
            }
            let _ = self.next_token()?;

            let rhs = self.parse_expression_pratt(rbp, tree)?;
            return Ok(PrattParseOutcome::NewLHS(
                tree.push(ExpressionNode::Infix { operator, lhs, rhs }),
            ));
        }
        Ok(PrattParseOutcome::NoOperator)
    }

    fn parse_postfix_expression_pratt(
        &mut self,
        tree: &mut IncompleteExpression,
        lhs: ExpressionNodeRef,
        min_bp: u8,
    ) -> Result<PrattParseOutcome, GeneralExpressionParserError> {
        if let Some(operator) = self.peek_postfix_operator()? {
            let lbp = operator.get_binding_power();
            if lbp < min_bp {
                return Ok(PrattParseOutcome::Break);
            }

            // Special case
            match operator {
                PostfixOperator::Call => {
                    let _ = self.expect(TokenKind::LeftParenthesis)?;
                    // Parse arguments/expressions
                    let mut arguments = Vec::new();

                    // Hit the end of the argument list right away
                    if let Some(_) = self.eat_if(TokenKind::RightParenthesis)? {
                        return Ok(PrattParseOutcome::NewLHS(tree.push(ExpressionNode::Call {
                            callee: lhs,
                            arguments: Vec::new(),
                        })));
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
                        return Ok(PrattParseOutcome::NewLHS(tree.push(ExpressionNode::Call {
                            callee: lhs,
                            arguments,
                        })));
                    }
                }
            }
        }
        Ok(PrattParseOutcome::NoOperator)
    }
}

// Generic helpers
impl<'src> Parser<'src> {
    pub fn new(source: &'src str, path: &'src Path) -> Self {
        let lexer = Lexer::new(source, path);
        Self {
            lexer,
            lookahead: None,
        }
    }

    fn peek(&mut self) -> Result<Token, LexicalError> {
        match self.lookahead {
            Option::Some(ref token_or_error) => token_or_error.clone(),
            Option::None => {
                let next_token = self.next_token();
                self.lookahead = Some(next_token.clone());
                next_token
            }
        }
    }

    fn next_token(&mut self) -> Result<Token, LexicalError> {
        match self.lookahead.take() {
            Option::Some(token_or_error) => token_or_error,
            Option::None => {
                let token_or_error = self.lexer.next_token();
                token_or_error
            }
        }
    }

    fn expect_ident(&mut self) -> Result<(IdentifierString, Span), GeneralParserError> {
        let next_token = self.next_token()?;
        if next_token.kind != TokenKind::Ident {
            return Err(GeneralParserError::UnexpectedToken {
                actual: next_token,
                expected: TokenKind::Ident,
            });
        }

        let name = self
            .lexer
            .get_lexeme(&next_token.span)
            .expect("Token came from lexer so it is valid.");

        Ok((name.into(), next_token.span))
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Token, GeneralParserError> {
        let next_token = self.next_token()?;
        if next_token.kind != expected {
            Err(GeneralParserError::UnexpectedToken {
                actual: next_token,
                expected,
            })
        } else {
            Ok(next_token)
        }
    }

    fn eat_if(&mut self, next: TokenKind) -> Result<Option<Token>, GeneralParserError> {
        let next_token = self.peek()?;
        if next_token.kind != next {
            Ok(None)
        } else {
            let _ = self.next_token().expect("Just peeked.");
            Ok(Some(next_token))
        }
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        let leftmost = self.expect(TokenKind::KeywordReturn)?;

        let (value, rightmost) = if let Some(rightmost) = self.eat_if(TokenKind::Semicolon)? {
            (None, rightmost)
        } else {
            let value = Some(self.parse_expression()?);
            let rightmost = self.expect(TokenKind::Semicolon)?;
            (value, rightmost)
        };

        let span = leftmost.span.merge(&rightmost.span);
        Ok(Statement::NonDeclaration(NonDeclaration {
            kind: NonDeclarationKind::Return { value },
            span,
        }))
    }
}
