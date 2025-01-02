mod error;
pub mod expression;
pub mod formatter;
pub mod program;
pub mod statement;

use crate::{
    lexer::{Lexer, LineBreaks, Token, TokenKind},
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
pub use program::Program;
use program::{
    BlockStmtRef, ClassDeclRef, DeclarationRef, ExpressionStmtRef, ForStmtRef, FunctionDeclRef,
    IfStmtRef, IncompleteProgram, NonDeclarationRef, PrintStmtRef, ReturnStmtRef, StatementRef,
    VariableDeclRef, WhileStmtRef,
};
use statement::{
    BlockStatement, ClassDecl, ExpressionStatement, ForStatement, FunctionDecl, IfStatement,
    Initializer, PrintStatement, ReturnStatement, VariableDecl, WhileStatement,
};
use std::path::Path;

/// Only allow up to 255 parameters
const MAX_PARAMETERS: usize = 255;

pub struct Parser<'src> {
    lexer: Lexer<'src>,
    lookahead: Option<Token>,
    expression_reports: Vec<GeneralExpressionParserError>,
    statement_reports: Vec<ParserError>,
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
        let mut program = IncompleteProgram::new();
        let mut statements = Vec::new();

        loop {
            match self.parse_statement(&mut program) {
                Ok(Some(stmt)) => {
                    statements.push(stmt);
                }
                Ok(None) => {
                    break;
                }
                Err(e) => {
                    self.statement_reports.push(e);
                    if self.synchronize() {
                        break;
                    }
                }
            }
        }

        if self.expression_reports.is_empty() && self.statement_reports.is_empty() {
            Ok(program.finish(statements))
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
                    if matches!(self.peek().kind, TokenKind::RightBrace) {
                        continue;
                    }
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
    pub fn parse_statement(
        &mut self,
        program: &mut IncompleteProgram,
    ) -> Result<Option<StatementRef>, ParserError> {
        if let Some(decl) = self.parse_declaration(program)? {
            Ok(Some(decl.into()))
        } else if let Some(stmt) = self.parse_non_declaration(program)? {
            Ok(Some(stmt.into()))
        } else {
            Ok(None)
        }
    }

    fn parse_declaration(
        &mut self,
        program: &mut IncompleteProgram,
    ) -> Result<Option<DeclarationRef>, ParserError> {
        let first = self.peek();
        let decl = match first.kind {
            TokenKind::KeywordVar => self.parse_variable_declaration(program)?.into(),
            TokenKind::KeywordFun => self.parse_function_declaration(program)?.into(),
            TokenKind::KeywordClass => self.parse_class_declaration(program)?.into(),
            _ => {
                return Ok(None);
            }
        };
        Ok(Some(decl))
    }

    fn parse_non_declaration(
        &mut self,
        program: &mut IncompleteProgram,
    ) -> Result<Option<NonDeclarationRef>, ParserError> {
        let first = self.peek();
        let stmt: NonDeclarationRef = match first.kind {
            TokenKind::KeywordPrint => self.parse_print_statement(program)?.into(),
            TokenKind::LeftBrace => self.parse_block_statement(program)?.into(),
            TokenKind::KeywordIf => self.parse_if_statement(program)?.into(),
            TokenKind::KeywordWhile => self.parse_while_statement(program)?.into(),
            TokenKind::KeywordFor => self.parse_for_statement(program)?.into(),
            TokenKind::KeywordReturn => self.parse_return_statement(program)?.into(),
            TokenKind::Eof => {
                return Ok(None);
            }
            _ => self.parse_expression_statement(program)?.into(),
        };
        Ok(Some(stmt))
    }

    fn parse_expression_statement(
        &mut self,
        program: &mut IncompleteProgram,
    ) -> Result<ExpressionStmtRef, ParserError> {
        let expr = self.parse_expression()?;
        let rightmost = self.expect_or(TokenKind::Semicolon, |token| {
            StatementParserError::NoSemicolonAfterExpr(token).into()
        })?;
        let span = expr.get_span().merge(&rightmost.span);
        let stmt = ExpressionStatement { expr, span };
        Ok(program.push_expression_stmt(stmt))
    }

    fn parse_print_statement(
        &mut self,
        program: &mut IncompleteProgram,
    ) -> Result<PrintStmtRef, ParserError> {
        let leftmost = self.expect(TokenKind::KeywordPrint)?;
        let expr = self.parse_expression()?;
        let rightmost = self.expect(TokenKind::Semicolon)?;

        let span = leftmost.span.merge(&rightmost.span);
        let stmt = PrintStatement { expr, span };
        Ok(program.push_print_stmt(stmt))
    }

    fn parse_variable_declaration(
        &mut self,
        program: &mut IncompleteProgram,
    ) -> Result<VariableDeclRef, ParserError> {
        let leftmost = self.expect(TokenKind::KeywordVar)?;
        let name =
            self.expect_ident_or(|token| StatementParserError::InvalidVariableName(token).into())?;
        let mut initial = None;
        // Assignment
        if let Some(_) = self.eat_if(TokenKind::Equal)? {
            let rhs = self.parse_expression()?;
            initial = Some(rhs);
        }
        let rightmost = self.expect(TokenKind::Semicolon)?;

        let span = leftmost.span.merge(&rightmost.span);
        let decl = VariableDecl {
            name,
            initial,
            span,
        };

        Ok(program.push_variable_decl(decl))
    }

    fn parse_function_declaration(
        &mut self,
        program: &mut IncompleteProgram,
    ) -> Result<FunctionDeclRef, ParserError> {
        let leftmost = self.expect(TokenKind::KeywordFun)?;
        let mut decl = self.try_parse_function(program)?;
        decl.span = decl.span.merge(&leftmost.span);

        Ok(program.push_function_decl(decl))
    }

    fn try_parse_function(
        &mut self,
        program: &mut IncompleteProgram,
    ) -> Result<FunctionDecl, ParserError> {
        const MSG: &'static str = "[Parse Function]: All handles are valid.";
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
                    self.statement_reports.push(err);
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

        let block = {
            let next_token = self.peek();
            match next_token.kind {
                TokenKind::LeftBrace => self.parse_block_statement(program),
                _ => Err(StatementParserError::NonBlock(next_token).into()),
            }?
        };

        let rightmost = program
            .get_span(Into::<NonDeclarationRef>::into(block).into())
            .expect(MSG);
        let span = name.span.merge(&rightmost);
        let decl = FunctionDecl {
            name,
            parameters,
            body: block,
            span,
        };
        Ok(decl)
    }

    fn parse_class_declaration(
        &mut self,
        program: &mut IncompleteProgram,
    ) -> Result<ClassDeclRef, ParserError> {
        let leftmost = self.expect(TokenKind::KeywordClass)?;
        let name = self.expect_ident()?;

        // Optional super class
        let super_class = if let Some(_) = self.eat_if(TokenKind::LessThan)? {
            Some(self.expect_ident_or(|token| {
                StatementParserError::InvalidSuperClassName(token).into()
            })?)
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
                    let func = self.try_parse_function(program)?;
                    methods.push(func);
                }
            }
        };

        let span = leftmost.span.merge(&rightmost.span);
        let decl = ClassDecl {
            name,
            methods,
            super_class,
            span,
        };

        Ok(program.push_class_decl(decl))
    }
    fn parse_block_statement(
        &mut self,
        program: &mut IncompleteProgram,
    ) -> Result<BlockStmtRef, ParserError> {
        let leftmost = self.expect(TokenKind::LeftBrace)?;
        let mut statements = Vec::new();

        // Check if we are empty
        let rightmost = if let Some(rightmost) = self.eat_if(TokenKind::RightBrace)? {
            rightmost
        } else {
            let rightmost = 'stmts: loop {
                let statement = self
                    .parse_statement(program)?
                    .ok_or(self.create_eof_error())?;
                statements.push(statement);
                if let Some(rightmost) = self.eat_if(TokenKind::RightBrace)? {
                    break 'stmts rightmost;
                }
            };
            rightmost
        };
        let span = leftmost.span.merge(&rightmost.span);
        let stmt = BlockStatement {
            body: statements,
            span,
        };
        Ok(program.push_block_stmt(stmt))
    }

    fn parse_if_statement(
        &mut self,
        program: &mut IncompleteProgram,
    ) -> Result<IfStmtRef, ParserError> {
        const MSG: &'static str = "[Parse If]: All handles are valid.";
        let leftmost = self.expect(TokenKind::KeywordIf)?;
        let _ = self.expect(TokenKind::LeftParenthesis)?;
        let condition = self.parse_expression()?;
        let _ = self.expect(TokenKind::RightParenthesis)?;
        let success = self
            .parse_statement(program)?
            .ok_or(self.create_eof_error())?;
        let mut rightmost = program.get_span(success).expect(MSG);

        let failure = if let Some(_) = self.eat_if(TokenKind::KeywordElse)? {
            let stmt = self
                .parse_statement(program)?
                .ok_or(self.create_eof_error())?;
            rightmost = program.get_span(stmt).expect(MSG);
            Some(stmt)
        } else {
            None
        };

        let span = leftmost.span.merge(&rightmost);
        let stmt = IfStatement {
            condition,
            success,
            failure,
            span,
        };
        Ok(program.push_if_stmt(stmt))
    }

    fn parse_while_statement(
        &mut self,
        program: &mut IncompleteProgram,
    ) -> Result<WhileStmtRef, ParserError> {
        const MSG: &'static str = "[Parse While]: All handles are valid";
        let leftmost = self.expect(TokenKind::KeywordWhile)?;
        let _ = self.expect(TokenKind::LeftParenthesis)?;
        let condition = self.parse_expression()?;
        let _ = self.expect(TokenKind::RightParenthesis)?;
        // Parse body
        let body = self.expect_non_declaration(program)?;
        let rightmost = program.get_span(body.into()).expect(MSG);
        let span = leftmost.span.merge(&rightmost);
        let stmt = WhileStatement {
            condition,
            body,
            span,
        };
        Ok(program.push_while_stmt(stmt))
    }

    fn parse_for_statement(
        &mut self,
        program: &mut IncompleteProgram,
    ) -> Result<ForStmtRef, ParserError> {
        const MSG: &'static str = "[Parse For]: All handles are valid";
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
                    TokenKind::KeywordVar => Some(Initializer::VariableDecl(
                        self.parse_variable_declaration(program)?,
                    )),
                    _ => Some(Initializer::Expression(
                        self.parse_expression_statement(program)?,
                    )),
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
        let body = self.expect_non_declaration(program)?;
        let rightmost = program.get_span(body.into()).expect(MSG);
        let span = leftmost.span.merge(&rightmost);

        let stmt = ForStatement {
            initializer,
            condition,
            increment,
            body,
            span,
        };

        Ok(program.push_for_stmt(stmt))
    }
}
enum PrattParseOutcome {
    NoOperator,
    Break,
    NewLHS(ExpressionNodeRef),
}

// Pratt parser for expressions
impl<'src> Parser<'src> {
    pub fn consume_expression(mut self) -> Result<Expression, Vec<GeneralExpressionParserError>> {
        let mut tree = IncompleteExpression::new();
        let root = match self.parse_expression_pratt(0, &mut tree) {
            Ok(root) => root,
            Err(e) => {
                self.expression_reports.push(e);
                return Err(self.expression_reports.into());
            }
        };

        if self.expression_reports.is_empty() {
            Ok(Expression::new(tree, root)
                .expect("Root was obtained from the tree itself so it must be valid."))
        } else {
            assert_eq!(
                self.statement_reports.len(),
                0,
                "Parsing just an expression should not encounter any statement errors."
            );

            Err(self.expression_reports.into())
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, GeneralExpressionParserError> {
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
                    self.expression_reports.push(
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
                                self.expression_reports.push(err.into());
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
            expression_reports: Vec::new(),
            statement_reports: Vec::new(),
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
                            self.statement_reports.push(e.into());
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

    fn expect_ident_or(
        &mut self,
        wrap_err: impl FnOnce(Token) -> ParserError,
    ) -> Result<Ident, ParserError> {
        let next_token = self.next_token();
        if next_token.kind != TokenKind::Ident {
            return Err(wrap_err(next_token));
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

    fn expect_or(
        &mut self,
        expected: TokenKind,
        wrap_err: impl FnOnce(Token) -> ParserError,
    ) -> Result<Token, ParserError> {
        let next_token = self.next_token();
        if next_token.kind != expected {
            Err(wrap_err(next_token))
        } else {
            Ok(next_token)
        }
    }

    fn expect_non_declaration(
        &mut self,
        program: &mut IncompleteProgram,
    ) -> Result<NonDeclarationRef, ParserError> {
        let next = self.peek();
        let stmt = match next.kind {
            TokenKind::KeywordVar | TokenKind::KeywordClass | TokenKind::KeywordFun => {
                return Err(StatementParserError::InvalidNonDeclaration(next).into());
            }
            _ => self
                .parse_non_declaration(program)?
                .ok_or(self.create_eof_error())?,
        };
        Ok(stmt)
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

    fn parse_return_statement(
        &mut self,
        program: &mut IncompleteProgram,
    ) -> Result<ReturnStmtRef, ParserError> {
        let leftmost = self.expect(TokenKind::KeywordReturn)?;

        let (value, rightmost) = if let Some(rightmost) = self.eat_if(TokenKind::Semicolon)? {
            (None, rightmost)
        } else {
            let value = Some(self.parse_expression()?);
            let rightmost = self.expect(TokenKind::Semicolon)?;
            (value, rightmost)
        };

        let span = leftmost.span.merge(&rightmost.span);
        let stmt = ReturnStatement { value, span };
        Ok(program.push_return_stmt(stmt))
    }
}
