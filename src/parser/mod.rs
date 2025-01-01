mod error;
pub mod expression;
pub mod formatter;
pub mod statement;

use crate::{
    lexer::{Lexer, LineBreaks, Span, Token, TokenKind},
    string::Ident,
};
pub use error::ParserError;
use error::{
    ExpressionParserError, GeneralExpressionParserError, GeneralParserError, StatementParserError,
};
use expression::{
    Expression, ExpressionAtom, ExpressionAtomKind, ExpressionNode, ExpressionNodeRef,
    IncompleteExpression, InfixAssignmentOperator, InfixOperator, InfixShortCircuitOperator,
    PostfixOperator, PrefixOperator,
};
use statement::{
    ClassDecl, Declaration, DeclarationKind, FunctionDecl, Initializer, NonDeclaration,
    NonDeclarationKind, Statement, VariableDecl,
};
use std::{collections::VecDeque, path::Path};

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
    lookahead: Option<Token>,
    expression_reports: VecDeque<GeneralExpressionParserError>,
    statement_reports: VecDeque<ParserError>,
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
    pub fn parse(mut self) -> Result<Program, Vec<ParserError>> {
        let mut statements = Vec::new();

        loop {
            match self.parse_statement() {
                Ok(Some(statement)) => {
                    statements.push(statement);
                }
                Ok(None) => {
                    break;
                }
                Err(e) => {
                    self.statement_reports.push_back(e);
                    if self.synchronize() {
                        break;
                    }
                }
            }
        }

        if self.expression_reports.is_empty() {
            Ok(Program { statements })
        } else {
            self.statement_reports
                .extend(self.expression_reports.into_iter().map(|v| v.into()));
            Err(self.statement_reports.into())
        }
    }

    fn synchronize(&mut self) -> bool {
        // Synchronize to next statement boundary
        loop {
            let next = self.peek();
            match next.kind {
                TokenKind::KeywordClass
                | TokenKind::KeywordFun
                | TokenKind::KeywordFor
                | TokenKind::KeywordVar
                | TokenKind::KeywordIf
                | TokenKind::KeywordWhile
                | TokenKind::KeywordPrint
                | TokenKind::LeftBrace
                | TokenKind::KeywordReturn => {
                    break;
                }
                TokenKind::Semicolon => {
                    let _ = self.next_token();
                    break;
                }
                TokenKind::Eof => {
                    return true;
                }
                _ => {}
            }
            let _ = self.next_token();
        }
        false
    }
}

// Parse statements
impl<'src> Parser<'src> {
    pub fn parse_statement(&mut self) -> Result<Option<Statement>, ParserError> {
        let first = self.peek();
        let statement = match first.kind {
            TokenKind::KeywordFun => self.parse_function_declaration()?,
            TokenKind::KeywordVar => self.parse_variable_declaration()?,
            TokenKind::KeywordClass => self.parse_class_declaration()?,
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
                let rightmost = {
                    let next_token = self.next_token();
                    match next_token.kind {
                        TokenKind::Semicolon => next_token,
                        _ => {
                            return Err(
                                StatementParserError::NoSemicolonAfterExpr(next_token).into()
                            );
                        }
                    }
                };
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
        let name = {
            match self.peek() {
                Token {
                    kind: TokenKind::Ident,
                    ..
                } => self.expect_ident()?,
                _ => {
                    let token = self.next_token();
                    return Err(StatementParserError::InvalidVariableName(token).into());
                }
            }
        };
        let mut initial = None;
        // Assignment
        if let Some(_) = self.eat_if(TokenKind::Equal)? {
            let rhs = self.parse_expression()?;
            initial = Some(rhs);
        }
        let rightmost = self.expect(TokenKind::Semicolon)?;

        let span = leftmost.span.merge(&rightmost.span);
        let statement = Statement::Declaration(Declaration {
            kind: DeclarationKind::Variable(VariableDecl { name, initial }),
            span,
        });

        Ok(statement)
    }

    fn parse_function_declaration(&mut self) -> Result<Statement, ParserError> {
        let leftmost = self.expect(TokenKind::KeywordFun)?;
        let (decl, rightmost) = self.try_parse_function()?;
        let span = leftmost.span.merge(&rightmost);
        let decl = Statement::Declaration(Declaration {
            kind: DeclarationKind::Function(decl),
            span,
        });

        Ok(decl)
    }

    fn try_parse_function(&mut self) -> Result<(FunctionDecl, Span), ParserError> {
        let name = self.expect_ident()?;
        let _ = self.expect(TokenKind::LeftParenthesis)?;

        // No parameters
        let parameters = if let Some(_) = self.eat_if(TokenKind::RightParenthesis)? {
            Vec::new()
        } else {
            let mut parameters = Vec::new();
            loop {
                let parameter = self.expect_ident()?;
                if parameters.len() >= MAX_PARAMETERS {
                    let err = StatementParserError::TooManyParameters {
                        max: MAX_PARAMETERS,
                        location: Token {
                            kind: TokenKind::Ident,
                            span: parameter.span,
                        },
                    }
                    .into();
                    self.statement_reports.push_back(err);
                } else {
                    parameters.push(parameter);
                }

                let next_token = self.next_token();
                match next_token.kind {
                    TokenKind::RightParenthesis => {
                        break;
                    }
                    TokenKind::Comma => {
                        continue;
                    }
                    _ => {
                        return Err(StatementParserError::NoRightParenthesisAfterParameters(
                            next_token,
                        )
                        .into());
                    }
                }
            }
            parameters
        };

        let (body, rightmost) = {
            let next_token = self.peek();
            let body = match next_token.kind {
                TokenKind::LeftBrace => self.parse_block_statement(),
                _ => Err(StatementParserError::NonBlock(next_token).into()),
            }?;
            let Statement::NonDeclaration(NonDeclaration {
                kind: NonDeclarationKind::Block(body),
                span,
            }) = body
            else {
                panic!("`parse_block_statement` will always return a block statement.");
            };
            (body, span)
        };

        let decl = FunctionDecl {
            name,
            parameters,
            body,
        };
        Ok((decl, rightmost))
    }

    fn parse_class_declaration(&mut self) -> Result<Statement, ParserError> {
        let leftmost = self.expect(TokenKind::KeywordClass)?;
        let name = self.expect_ident()?;

        // Optional super class
        let super_class = if let Some(_) = self.eat_if(TokenKind::LessThan)? {
            let next_token = self.peek();
            match next_token.kind {
                TokenKind::Ident => Some(self.expect_ident()?),
                _ => {
                    let _ = self.next_token();
                    return Err(StatementParserError::InvalidSuperClassName(next_token))?;
                }
            }
        } else {
            None
        };
        let _ = self.expect(TokenKind::LeftBrace)?;

        let mut methods = Vec::new();
        let rightmost = 'block: loop {
            match self.eat_if(TokenKind::RightBrace)? {
                Some(rightmost) => {
                    break 'block rightmost;
                }
                None => {
                    let (func, _) = self.try_parse_function()?;
                    methods.push(func);
                }
            }
        };

        let span = leftmost.span.merge(&rightmost.span);
        let statement = Statement::Declaration(Declaration {
            kind: DeclarationKind::Class(ClassDecl {
                name,
                methods,
                super_class,
            }),
            span,
        });

        Ok(statement)
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
        // Parse body
        let body = {
            let next = self.peek();
            match next.kind {
                TokenKind::KeywordVar | TokenKind::KeywordClass | TokenKind::KeywordFun => {
                    return Err(StatementParserError::InvalidNonDeclaration(next).into());
                }
                _ => {
                    let stmt @ Statement::NonDeclaration(_) =
                        self.parse_statement()?.ok_or(self.create_eof_error())?
                    else {
                        panic!("We already guarded against non-declarations above so we should only get non-declarations.");
                    };
                    stmt
                }
            }
        };

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
                let token = self.peek();
                match token.kind {
                    TokenKind::KeywordVar => {
                        let Statement::Declaration(Declaration {
                            kind: DeclarationKind::Variable(decl),
                            span,
                        }) = self.parse_variable_declaration()?
                        else {
                            panic!("`parse_variable_declaration` will only return variable declaration statements.");
                        };
                        Some(Initializer::VarDecl {
                            name: decl.name,
                            initial: decl.initial,
                            stmt_span: span,
                        })
                    }
                    _ => {
                        let expr = self.parse_expression()?;
                        self.expect(TokenKind::Semicolon)?;
                        Some(Initializer::Expression(expr))
                    }
                }
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
        let body = {
            let next = self.peek();
            match next.kind {
                TokenKind::KeywordVar | TokenKind::KeywordClass | TokenKind::KeywordFun => {
                    return Err(StatementParserError::InvalidNonDeclaration(next).into());
                }
                _ => {
                    let Statement::NonDeclaration(body) =
                        self.parse_statement()?.ok_or(self.create_eof_error())?
                    else {
                        panic!("We already guarded against non-declarations above so we should only get non-declarations.");
                    };
                    body
                }
            }
        };

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
        let root = self.parse_expression_pratt(0, &mut tree)?;

        Ok(Expression::new(tree, root)
            .expect("Root was obtained from the tree itself so it must be valid."))
    }

    fn peek_infix_operator(
        &mut self,
    ) -> Result<Option<InfixOperator>, GeneralExpressionParserError> {
        let token = self.peek();

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
        let token = self.peek();

        match token.kind {
            TokenKind::LeftParenthesis => Ok(Some(PostfixOperator::Call)),
            TokenKind::Dot => Ok(Some(PostfixOperator::Access)),
            _ => Ok(None),
        }
    }
    fn peek_infix_assignment_operator(
        &mut self,
    ) -> Result<Option<InfixAssignmentOperator>, GeneralExpressionParserError> {
        type Operator = InfixAssignmentOperator;
        let token = self.peek();

        match token.kind {
            TokenKind::Equal => Ok(Some(Operator::Assign)),
            _ => Ok(None),
        }
    }

    fn peek_infix_short_circuit_operator(
        &mut self,
    ) -> Result<Option<InfixShortCircuitOperator>, GeneralExpressionParserError> {
        type Operator = InfixShortCircuitOperator;
        let token = self.peek();

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
        let token = self.next_token();

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
            // This
            TokenKind::KeywordThis => {
                let node = ExpressionNode::Atom(ExpressionAtom {
                    kind: ExpressionAtomKind::This,
                    span: token.span,
                });
                tree.push(node)
            }
            // This
            TokenKind::KeywordSuper => {
                let _ = match self.next_token() {
                    token @ Token {
                        kind: TokenKind::Dot,
                        ..
                    } => token,
                    token => {
                        return Err(ExpressionParserError::MissingDotAfterSuper(token).into());
                    }
                };
                let method = match self.peek() {
                    Token {
                        kind: TokenKind::Ident,
                        ..
                    } => self.expect_ident()?,
                    _ => {
                        let token = self.next_token();
                        return Err(ExpressionParserError::MissingMethodName(token).into());
                    }
                };
                let span = token.span.merge(&method.span);

                let node = ExpressionNode::Atom(ExpressionAtom {
                    kind: ExpressionAtomKind::Super(method.name),
                    span,
                });
                tree.push(node)
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
            let (lbp, rbp) = operator.get_binding_power();
            if lbp < min_bp {
                return Ok(PrattParseOutcome::Break);
            }
            let _ = self.next_token();

            let rhs = self.parse_expression_pratt(rbp, tree)?;

            let place = match tree.get_l_value(lhs) {
                Some(name) => name,
                None => {
                    self.expression_reports.push_back(
                        ExpressionParserError::InvalidLValue(Token {
                            kind: tree.get_kind(lhs).expect(MSG),
                            span,
                        })
                        .into(),
                    );
                    "INVALID_L_VALUE"
                }
            };
            let name = Ident {
                name: place.into(),
                span,
            };

            return Ok(PrattParseOutcome::NewLHS(
                tree.push(ExpressionNode::InfixAssignment { lhs: name, rhs }),
            ));
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
            let _ = self.next_token();

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
            let _ = self.next_token();

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
                        loop {
                            if arguments.len() >= MAX_PARAMETERS {
                                let next_token = self.peek();
                                let err = ExpressionParserError::TooManyArguments {
                                    max: MAX_PARAMETERS,
                                    location: next_token,
                                };
                                self.expression_reports.push_back(err.into());
                            }
                            let argument = self.parse_expression_pratt(0, tree)?;
                            arguments.push(argument);

                            let next_token = self.next_token();
                            match next_token.kind {
                                TokenKind::RightParenthesis => {
                                    break;
                                }
                                TokenKind::Comma => {
                                    continue;
                                }
                                _ => {
                                    return Err(
                                        ExpressionParserError::NoRightParenthesisAfterArguments(
                                            next_token,
                                        )
                                        .into(),
                                    );
                                }
                            }
                        }
                        return Ok(PrattParseOutcome::NewLHS(tree.push(ExpressionNode::Call {
                            callee: lhs,
                            arguments,
                        })));
                    }
                }
                PostfixOperator::Access => {
                    let _ = self.expect(TokenKind::Dot)?;
                    let ident = {
                        let next_token = self.peek();
                        match next_token.kind {
                            TokenKind::Ident => self.expect_ident()?,
                            _ => {
                                let _ = self.next_token();
                                return Err(
                                    ExpressionParserError::MissingPropertyName(next_token).into()
                                );
                            }
                        }
                    };

                    if let Some(_) = self.eat_if(TokenKind::Equal)? {
                        let rhs = self.parse_expression_pratt(0, tree)?;
                        return Ok(PrattParseOutcome::NewLHS(tree.push(ExpressionNode::Set {
                            object: lhs,
                            name: ident,
                            value: rhs,
                        })));
                    } else {
                        return Ok(PrattParseOutcome::NewLHS(tree.push(ExpressionNode::Get {
                            object: lhs,
                            name: ident,
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
            expression_reports: VecDeque::new(),
            statement_reports: VecDeque::new(),
        }
    }

    fn peek(&mut self) -> Token {
        match self.lookahead {
            Option::Some(ref token) => token.clone(),
            Option::None => {
                let next_token = self.next_token();
                self.lookahead = Some(next_token.clone());
                next_token
            }
        }
    }

    fn next_token(&mut self) -> Token {
        match self.lookahead.take() {
            Option::Some(token) => token,
            Option::None => {
                let token = 'token: loop {
                    match self.lexer.next_token() {
                        Ok(token) => break 'token token,
                        Err(e) => {
                            self.statement_reports.push_back(e.into());
                        }
                    }
                };

                token
            }
        }
    }

    fn expect_ident(&mut self) -> Result<Ident, GeneralParserError> {
        let next_token = self.next_token();
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

        Ok(Ident {
            name: name.into(),
            span: next_token.span,
        })
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Token, GeneralParserError> {
        let next_token = self.next_token();
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
        let next_token = self.peek();
        if next_token.kind != next {
            Ok(None)
        } else {
            let _ = self.next_token();
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
