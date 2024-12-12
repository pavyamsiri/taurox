mod error;
pub mod expression;
pub mod formatter;
pub mod statement;

use crate::lexer::{Lexer, Token, TokenKind};
use compact_str::{CompactString, ToCompactString};
pub use error::ParserError;
use error::ParserErrorKind;
use expression::{
    Expression, ExpressionAtom, ExpressionAtomKind, ExpressionNode, ExpressionNodeRef,
    IncompleteExpression, InfixAssignmentOperator, InfixOperator, InfixShortCircuitOperator,
    PostfixOperator, PrefixOperator,
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
        let line = first.line;
        let statement = match first.kind {
            TokenKind::KeywordFun => self.parse_function_declaration(line)?,
            TokenKind::KeywordVar => self.parse_variable_declaration()?,
            TokenKind::KeywordPrint => self.parse_print_statement()?,
            TokenKind::LeftBrace => self.parse_block_statement(line)?,
            TokenKind::KeywordIf => self.parse_if_statement(line)?,
            TokenKind::KeywordWhile => self.parse_while_statement(line)?,
            TokenKind::KeywordFor => self.parse_for_statement(line)?,
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
enum PrattParseOutcome {
    NoOperator,
    Break,
    NewLHS(ExpressionNodeRef),
}

// Pratt parser for expressions
impl<'src> Parser<'src> {
    pub fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        let mut tree = IncompleteExpression::new();
        let res = self.parse_expression_pratt(0, &mut tree)?;

        Ok(Expression::new(tree, res)
            .expect("Root was obtained from the tree itself so it must be valid."))
    }

    fn peek_infix_operator(&mut self) -> Result<Option<InfixOperator>, ParserError> {
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
    ) -> Result<ExpressionNodeRef, ParserError> {
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
    fn peek_postfix_operator(&mut self) -> Result<Option<PostfixOperator>, ParserError> {
        let token = self.peek()?;

        match token.kind {
            TokenKind::LeftParenthesis => Ok(Some(PostfixOperator::Call)),
            _ => Ok(None),
        }
    }
    fn peek_infix_assignment_operator(
        &mut self,
    ) -> Result<Option<InfixAssignmentOperator>, ParserError> {
        type Operator = InfixAssignmentOperator;
        let token = self.peek()?;

        match token.kind {
            TokenKind::Equal => Ok(Some(Operator::Assign)),
            _ => Ok(None),
        }
    }

    fn peek_infix_short_circuit_operator(
        &mut self,
    ) -> Result<Option<InfixShortCircuitOperator>, ParserError> {
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
    ) -> Result<ExpressionNodeRef, ParserError> {
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
                let node = ExpressionNode::Atom(ExpressionAtom {
                    kind: ExpressionAtomKind::Number(
                        lexeme
                            .parse()
                            .expect("Numeric literal tokens are valid `f64`"),
                    ),
                    line: token.line,
                });
                tree.push(node)
            }
            TokenKind::Ident => {
                let node = ExpressionNode::Atom(ExpressionAtom {
                    kind: ExpressionAtomKind::Identifier(lexeme.into()),
                    line: token.line,
                });
                tree.push(node)
            }
            TokenKind::StringLiteral => {
                let value = lexeme
                    .get(1..lexeme.len() - 1)
                    .expect("String literal tokens are at least length 2.");
                let node = ExpressionNode::Atom(ExpressionAtom {
                    kind: ExpressionAtomKind::StringLiteral(value.into()),
                    line: token.line,
                });
                tree.push(node)
            }
            TokenKind::KeywordNil => {
                let node = ExpressionNode::Atom(ExpressionAtom {
                    kind: ExpressionAtomKind::Nil,
                    line: token.line,
                });
                tree.push(node)
            }
            TokenKind::KeywordTrue => {
                let node = ExpressionNode::Atom(ExpressionAtom {
                    kind: ExpressionAtomKind::Bool(true),
                    line: token.line,
                });
                tree.push(node)
            }
            TokenKind::KeywordFalse => {
                let node = ExpressionNode::Atom(ExpressionAtom {
                    kind: ExpressionAtomKind::Bool(false),
                    line: token.line,
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
            kind => {
                return Err(ParserError {
                    kind: ParserErrorKind::NonExpression(kind),
                    line: token.line,
                })
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
    ) -> Result<PrattParseOutcome, ParserError> {
        const MSG: &'static str = "Caller must make sure `lhs` is a valid expression node ref.";
        if let Some(operator) = self.peek_infix_assignment_operator()? {
            let place = tree
                .get_l_value(lhs)
                .ok_or(ParserError {
                    kind: ParserErrorKind::InvalidLValue(tree.get_kind(lhs).expect(MSG)),
                    line: tree.get_line(lhs).expect(MSG),
                })?
                .to_compact_string();

            let (lbp, rbp) = operator.get_binding_power();
            if lbp < min_bp {
                return Ok(PrattParseOutcome::Break);
            }
            let _ = self.next_token()?;

            let rhs = self.parse_expression_pratt(rbp, tree)?;
            return Ok(PrattParseOutcome::NewLHS(
                tree.push(ExpressionNode::InfixAssignment { lhs: place, rhs }),
            ));
        }
        Ok(PrattParseOutcome::NoOperator)
    }

    fn parse_infix_short_circuit_expression_pratt(
        &mut self,
        tree: &mut IncompleteExpression,
        lhs: ExpressionNodeRef,
        min_bp: u8,
    ) -> Result<PrattParseOutcome, ParserError> {
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
    ) -> Result<PrattParseOutcome, ParserError> {
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
    ) -> Result<PrattParseOutcome, ParserError> {
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
