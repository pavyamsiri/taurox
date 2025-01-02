use super::statement::{
    BlockStatement, ClassDecl, ExpressionStatement, ForStatement, FunctionDecl, IfStatement,
    NonDeclaration, PrintStatement, ReturnStatement, Statement, VariableDecl, WhileStatement,
};
use crate::lexer::Span;

#[derive(Debug, Clone, Copy)]
pub enum StatementRef {
    // Declarations
    VariableDecl(VariableDeclRef),
    FunctionDecl(FunctionDeclRef),
    ClassDecl(ClassDeclRef),
    // Non-declarations
    Expression(ExpressionStmtRef),
    Print(PrintStmtRef),
    Block(BlockStmtRef),
    If(IfStmtRef),
    While(WhileStmtRef),
    For(ForStmtRef),
    Return(ReturnStmtRef),
}

impl From<DeclarationRef> for StatementRef {
    fn from(value: DeclarationRef) -> Self {
        match value {
            DeclarationRef::VariableDecl(handle) => StatementRef::VariableDecl(handle),
            DeclarationRef::FunctionDecl(handle) => StatementRef::FunctionDecl(handle),
            DeclarationRef::ClassDecl(handle) => StatementRef::ClassDecl(handle),
        }
    }
}

impl From<NonDeclarationRef> for StatementRef {
    fn from(value: NonDeclarationRef) -> Self {
        match value {
            NonDeclarationRef::Expression(handle) => StatementRef::Expression(handle),
            NonDeclarationRef::Print(handle) => StatementRef::Print(handle),
            NonDeclarationRef::Block(handle) => StatementRef::Block(handle),
            NonDeclarationRef::If(handle) => StatementRef::If(handle),
            NonDeclarationRef::While(handle) => StatementRef::While(handle),
            NonDeclarationRef::For(handle) => StatementRef::For(handle),
            NonDeclarationRef::Return(handle) => StatementRef::Return(handle),
        }
    }
}

// Declarations
#[derive(Debug, Clone, Copy)]
pub enum DeclarationRef {
    VariableDecl(VariableDeclRef),
    FunctionDecl(FunctionDeclRef),
    ClassDecl(ClassDeclRef),
}
#[derive(Debug, Clone, Copy)]
pub struct VariableDeclRef(u32);
#[derive(Debug, Clone, Copy)]
pub struct FunctionDeclRef(u32);
#[derive(Debug, Clone, Copy)]
pub struct ClassDeclRef(u32);

impl From<VariableDeclRef> for DeclarationRef {
    fn from(value: VariableDeclRef) -> Self {
        Self::VariableDecl(value)
    }
}

impl From<FunctionDeclRef> for DeclarationRef {
    fn from(value: FunctionDeclRef) -> Self {
        Self::FunctionDecl(value)
    }
}

impl From<ClassDeclRef> for DeclarationRef {
    fn from(value: ClassDeclRef) -> Self {
        Self::ClassDecl(value)
    }
}

// Statements
#[derive(Debug, Clone, Copy)]
pub enum NonDeclarationRef {
    Expression(ExpressionStmtRef),
    Print(PrintStmtRef),
    Block(BlockStmtRef),
    If(IfStmtRef),
    While(WhileStmtRef),
    For(ForStmtRef),
    Return(ReturnStmtRef),
}
#[derive(Debug, Clone, Copy)]
pub struct ExpressionStmtRef(u32);
#[derive(Debug, Clone, Copy)]
pub struct PrintStmtRef(u32);
#[derive(Debug, Clone, Copy)]
pub struct BlockStmtRef(u32);
#[derive(Debug, Clone, Copy)]
pub struct IfStmtRef(u32);
#[derive(Debug, Clone, Copy)]
pub struct WhileStmtRef(u32);
#[derive(Debug, Clone, Copy)]
pub struct ForStmtRef(u32);
#[derive(Debug, Clone, Copy)]
pub struct ReturnStmtRef(u32);

impl From<ExpressionStmtRef> for NonDeclarationRef {
    fn from(value: ExpressionStmtRef) -> Self {
        Self::Expression(value)
    }
}

impl From<PrintStmtRef> for NonDeclarationRef {
    fn from(value: PrintStmtRef) -> Self {
        Self::Print(value)
    }
}

impl From<BlockStmtRef> for NonDeclarationRef {
    fn from(value: BlockStmtRef) -> Self {
        Self::Block(value)
    }
}

impl From<IfStmtRef> for NonDeclarationRef {
    fn from(value: IfStmtRef) -> Self {
        Self::If(value)
    }
}

impl From<WhileStmtRef> for NonDeclarationRef {
    fn from(value: WhileStmtRef) -> Self {
        Self::While(value)
    }
}

impl From<ForStmtRef> for NonDeclarationRef {
    fn from(value: ForStmtRef) -> Self {
        Self::For(value)
    }
}

impl From<ReturnStmtRef> for NonDeclarationRef {
    fn from(value: ReturnStmtRef) -> Self {
        Self::Return(value)
    }
}

pub struct Program {
    inner: IncompleteProgram,
    statements: Vec<StatementRef>,
}

pub struct ProgramIterator<'a> {
    program: &'a Program,
    index: usize,
}

impl Program {
    pub fn empty() -> Self {
        Self {
            inner: IncompleteProgram::new(),
            statements: Vec::new(),
        }
    }

    pub fn iter<'a>(&'a self) -> ProgramIterator<'a> {
        ProgramIterator {
            program: self,
            index: 0,
        }
    }

    pub fn get_statement<'a>(&'a self, handle: StatementRef) -> Option<Statement<'a>> {
        self.inner.get_statement(handle)
    }

    pub fn get_non_declaration<'a>(
        &'a self,
        handle: NonDeclarationRef,
    ) -> Option<NonDeclaration<'a>> {
        self.inner.get_non_declaration(handle)
    }

    pub fn get_variable_decl<'a>(&'a self, handle: VariableDeclRef) -> Option<&'a VariableDecl> {
        self.inner.get_variable_decl(handle)
    }

    pub fn get_function_decl<'a>(&'a self, handle: FunctionDeclRef) -> Option<&'a FunctionDecl> {
        self.inner.get_function_decl(handle)
    }

    pub fn get_class_decl<'a>(&'a self, handle: ClassDeclRef) -> Option<&'a ClassDecl> {
        self.inner.get_class_decl(handle)
    }

    pub fn get_expression_stmt<'a>(
        &'a self,
        handle: ExpressionStmtRef,
    ) -> Option<&'a ExpressionStatement> {
        self.inner.get_expression_stmt(handle)
    }

    pub fn get_print_stmt<'a>(&'a self, handle: PrintStmtRef) -> Option<&'a PrintStatement> {
        self.inner.get_print_stmt(handle)
    }

    pub fn get_block_stmt<'a>(&'a self, handle: BlockStmtRef) -> Option<&'a BlockStatement> {
        self.inner.get_block_stmt(handle)
    }

    pub fn get_if_stmt<'a>(&'a self, handle: IfStmtRef) -> Option<&'a IfStatement> {
        self.inner.get_if_stmt(handle)
    }

    pub fn get_while_stmt<'a>(&'a self, handle: WhileStmtRef) -> Option<&'a WhileStatement> {
        self.inner.get_while_stmt(handle)
    }

    pub fn get_for_stmt<'a>(&'a self, handle: ForStmtRef) -> Option<&'a ForStatement> {
        self.inner.get_for_stmt(handle)
    }

    pub fn get_return_stmt<'a>(&'a self, handle: ReturnStmtRef) -> Option<&'a ReturnStatement> {
        self.inner.get_return_stmt(handle)
    }
}

impl<'a> std::iter::Iterator for ProgramIterator<'a> {
    type Item = Statement<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let handle = self.program.statements.get(self.index)?;
        if let Some(stmt) = self.program.inner.get_statement(*handle) {
            self.index += 1;
            Some(stmt)
        } else {
            None
        }
    }
}

pub struct IncompleteProgram {
    // Declarations
    variable_decls: Vec<VariableDecl>,
    function_decls: Vec<FunctionDecl>,
    class_decls: Vec<ClassDecl>,
    // Statements
    expression_stmts: Vec<ExpressionStatement>,
    print_stmts: Vec<PrintStatement>,
    block_stmts: Vec<BlockStatement>,
    if_stmts: Vec<IfStatement>,
    while_stmts: Vec<WhileStatement>,
    for_stmts: Vec<ForStatement>,
    return_stmts: Vec<ReturnStatement>,
}

impl IncompleteProgram {
    pub fn new() -> Self {
        Self {
            variable_decls: Vec::new(),
            function_decls: Vec::new(),
            class_decls: Vec::new(),
            expression_stmts: Vec::new(),
            print_stmts: Vec::new(),
            block_stmts: Vec::new(),
            if_stmts: Vec::new(),
            while_stmts: Vec::new(),
            for_stmts: Vec::new(),
            return_stmts: Vec::new(),
        }
    }

    pub fn finish(self, statements: Vec<StatementRef>) -> Program {
        Program {
            inner: self,
            statements,
        }
    }

    pub fn get_span(&self, handle: StatementRef) -> Option<Span> {
        match handle {
            StatementRef::VariableDecl(handle) => self
                .variable_decls
                .get(handle.0 as usize)
                .and_then(|d| Some(d.span)),
            StatementRef::FunctionDecl(handle) => self
                .function_decls
                .get(handle.0 as usize)
                .and_then(|d| Some(d.span)),
            StatementRef::ClassDecl(handle) => self
                .class_decls
                .get(handle.0 as usize)
                .and_then(|d| Some(d.span)),
            StatementRef::Expression(handle) => self
                .expression_stmts
                .get(handle.0 as usize)
                .and_then(|d| Some(d.span)),
            StatementRef::Print(handle) => self
                .print_stmts
                .get(handle.0 as usize)
                .and_then(|d| Some(d.span)),
            StatementRef::Block(handle) => self
                .block_stmts
                .get(handle.0 as usize)
                .and_then(|d| Some(d.span)),
            StatementRef::If(handle) => self
                .if_stmts
                .get(handle.0 as usize)
                .and_then(|d| Some(d.span)),
            StatementRef::While(handle) => self
                .while_stmts
                .get(handle.0 as usize)
                .and_then(|d| Some(d.span)),
            StatementRef::For(handle) => self
                .for_stmts
                .get(handle.0 as usize)
                .and_then(|d| Some(d.span)),
            StatementRef::Return(handle) => self
                .return_stmts
                .get(handle.0 as usize)
                .and_then(|d| Some(d.span)),
        }
    }
}

// Declarations
impl IncompleteProgram {
    pub fn get_statement<'a>(&'a self, handle: StatementRef) -> Option<Statement<'a>> {
        match handle {
            StatementRef::VariableDecl(handle) => Some(Statement::VariableDecl(
                self.variable_decls.get(handle.0 as usize)?,
            )),
            StatementRef::FunctionDecl(handle) => Some(Statement::FunctionDecl(
                self.function_decls.get(handle.0 as usize)?,
            )),
            StatementRef::ClassDecl(handle) => Some(Statement::ClassDecl(
                self.class_decls.get(handle.0 as usize)?,
            )),
            StatementRef::Expression(handle) => Some(Statement::Expression(
                self.expression_stmts.get(handle.0 as usize)?,
            )),
            StatementRef::Print(handle) => {
                Some(Statement::Print(self.print_stmts.get(handle.0 as usize)?))
            }
            StatementRef::Block(handle) => {
                Some(Statement::Block(self.block_stmts.get(handle.0 as usize)?))
            }
            StatementRef::If(handle) => Some(Statement::If(self.if_stmts.get(handle.0 as usize)?)),
            StatementRef::While(handle) => {
                Some(Statement::While(self.while_stmts.get(handle.0 as usize)?))
            }
            StatementRef::For(handle) => {
                Some(Statement::For(self.for_stmts.get(handle.0 as usize)?))
            }
            StatementRef::Return(handle) => {
                Some(Statement::Return(self.return_stmts.get(handle.0 as usize)?))
            }
        }
    }

    pub fn push_variable_decl(&mut self, decl: VariableDecl) -> VariableDeclRef {
        self.variable_decls.push(decl);
        VariableDeclRef((self.variable_decls.len() - 1) as u32)
    }

    pub fn push_function_decl(&mut self, decl: FunctionDecl) -> FunctionDeclRef {
        self.function_decls.push(decl);
        FunctionDeclRef((self.function_decls.len() - 1) as u32)
    }

    pub fn push_class_decl(&mut self, decl: ClassDecl) -> ClassDeclRef {
        self.class_decls.push(decl);
        ClassDeclRef((self.class_decls.len() - 1) as u32)
    }

    pub fn get_variable_decl<'a>(&'a self, handle: VariableDeclRef) -> Option<&'a VariableDecl> {
        self.variable_decls.get(handle.0 as usize)
    }

    pub fn get_function_decl<'a>(&'a self, handle: FunctionDeclRef) -> Option<&'a FunctionDecl> {
        self.function_decls.get(handle.0 as usize)
    }

    pub fn get_class_decl<'a>(&'a self, handle: ClassDeclRef) -> Option<&'a ClassDecl> {
        self.class_decls.get(handle.0 as usize)
    }
}

// Non-declarations
impl IncompleteProgram {
    pub fn get_non_declaration<'a>(
        &'a self,
        handle: NonDeclarationRef,
    ) -> Option<NonDeclaration<'a>> {
        match handle {
            NonDeclarationRef::Expression(handle) => Some(NonDeclaration::Expression(
                self.expression_stmts.get(handle.0 as usize)?,
            )),
            NonDeclarationRef::Print(handle) => Some(NonDeclaration::Print(
                self.print_stmts.get(handle.0 as usize)?,
            )),
            NonDeclarationRef::Block(handle) => Some(NonDeclaration::Block(
                self.block_stmts.get(handle.0 as usize)?,
            )),
            NonDeclarationRef::If(handle) => {
                Some(NonDeclaration::If(self.if_stmts.get(handle.0 as usize)?))
            }
            NonDeclarationRef::While(handle) => Some(NonDeclaration::While(
                self.while_stmts.get(handle.0 as usize)?,
            )),
            NonDeclarationRef::For(handle) => {
                Some(NonDeclaration::For(self.for_stmts.get(handle.0 as usize)?))
            }
            NonDeclarationRef::Return(handle) => Some(NonDeclaration::Return(
                self.return_stmts.get(handle.0 as usize)?,
            )),
        }
    }

    pub fn push_expression_stmt(&mut self, stmt: ExpressionStatement) -> ExpressionStmtRef {
        self.expression_stmts.push(stmt);
        ExpressionStmtRef((self.expression_stmts.len() - 1) as u32)
    }

    pub fn push_print_stmt(&mut self, stmt: PrintStatement) -> PrintStmtRef {
        self.print_stmts.push(stmt);
        PrintStmtRef((self.print_stmts.len() - 1) as u32)
    }

    pub fn push_block_stmt(&mut self, stmt: BlockStatement) -> BlockStmtRef {
        self.block_stmts.push(stmt);
        BlockStmtRef((self.block_stmts.len() - 1) as u32)
    }

    pub fn push_if_stmt(&mut self, stmt: IfStatement) -> IfStmtRef {
        self.if_stmts.push(stmt);
        IfStmtRef((self.if_stmts.len() - 1) as u32)
    }

    pub fn push_while_stmt(&mut self, stmt: WhileStatement) -> WhileStmtRef {
        self.while_stmts.push(stmt);
        WhileStmtRef((self.while_stmts.len() - 1) as u32)
    }

    pub fn push_for_stmt(&mut self, stmt: ForStatement) -> ForStmtRef {
        self.for_stmts.push(stmt);
        ForStmtRef((self.for_stmts.len() - 1) as u32)
    }

    pub fn push_return_stmt(&mut self, stmt: ReturnStatement) -> ReturnStmtRef {
        self.return_stmts.push(stmt);
        ReturnStmtRef((self.return_stmts.len() - 1) as u32)
    }

    pub fn get_expression_stmt<'a>(
        &'a self,
        handle: ExpressionStmtRef,
    ) -> Option<&'a ExpressionStatement> {
        self.expression_stmts.get(handle.0 as usize)
    }

    pub fn get_print_stmt<'a>(&'a self, handle: PrintStmtRef) -> Option<&'a PrintStatement> {
        self.print_stmts.get(handle.0 as usize)
    }

    pub fn get_block_stmt<'a>(&'a self, handle: BlockStmtRef) -> Option<&'a BlockStatement> {
        self.block_stmts.get(handle.0 as usize)
    }

    pub fn get_if_stmt<'a>(&'a self, handle: IfStmtRef) -> Option<&'a IfStatement> {
        self.if_stmts.get(handle.0 as usize)
    }

    pub fn get_while_stmt<'a>(&'a self, handle: WhileStmtRef) -> Option<&'a WhileStatement> {
        self.while_stmts.get(handle.0 as usize)
    }

    pub fn get_for_stmt<'a>(&'a self, handle: ForStmtRef) -> Option<&'a ForStatement> {
        self.for_stmts.get(handle.0 as usize)
    }

    pub fn get_return_stmt<'a>(&'a self, handle: ReturnStmtRef) -> Option<&'a ReturnStatement> {
        self.return_stmts.get(handle.0 as usize)
    }
}
