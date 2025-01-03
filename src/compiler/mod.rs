mod constant;
mod opcode;

use crate::lexer::{LineBreaks, Span};
use crate::parser::expression::{
    Expression, ExpressionAtom, ExpressionAtomKind, ExpressionNode, ExpressionNodeRef,
    InfixOperator, InfixShortCircuitOperator, PrefixOperator,
};
use crate::parser::statement::{
    BlockStatement, ExpressionStatement, IfStatement, NonDeclaration, PrintStatement, Statement,
    VariableDecl, WhileStatement,
};
use crate::resolver::ResolvedProgram;
use crate::string::{Ident, IdentName, InternStringHandle, StringInterner};
pub use constant::{ConstRef, ConstantPool, LoxConstant};
pub use opcode::{DecodeError, Opcode};
use opcode::{InstructionOffset, StackSlot};
use std::fmt::Write;
use std::sync::Arc;

const WRITE_FMT_MSG: &'static str =
    "Encountered an error while attempting to write format string to buffer.";

pub struct IncompleteChunk<'src> {
    name: IdentName,
    text: &'src str,
    line_breaks: LineBreaks,
    data: Vec<u8>,
    spans: Vec<Span>,
    constants: ConstantPool,
    starts: Vec<usize>,
}

impl<'src> IncompleteChunk<'src> {
    pub fn new(name: IdentName, text: &'src str) -> Self {
        let line_breaks = LineBreaks::new(text);
        Self {
            name,
            text,
            line_breaks,
            data: Vec::new(),
            spans: Vec::new(),
            constants: ConstantPool::new(),
            starts: Vec::new(),
        }
    }

    pub fn emit_u8(&mut self, byte: u8) {
        self.data.push(byte);
    }

    pub fn emit_u32(&mut self, value: u32) {
        // Little endian
        let [first, second, third, fourth] = value.to_le_bytes();
        self.data.push(first);
        self.data.push(second);
        self.data.push(third);
        self.data.push(fourth);
    }

    pub fn emit_i32(&mut self, value: i32) {
        // Little endian
        let [first, second, third, fourth] = value.to_le_bytes();
        self.data.push(first);
        self.data.push(second);
        self.data.push(third);
        self.data.push(fourth);
    }

    pub fn emit_return(&mut self, span: Span) {
        self.starts.push(self.data.len());
        self.spans.push(span);
        Opcode::Return.encode(self);
    }

    pub fn emit_multiply(&mut self, span: Span) {
        self.starts.push(self.data.len());
        self.spans.push(span);
        Opcode::Multiply.encode(self);
    }

    pub fn emit_divide(&mut self, span: Span) {
        self.starts.push(self.data.len());
        self.spans.push(span);
        Opcode::Divide.encode(self);
    }

    pub fn emit_add(&mut self, span: Span) {
        self.starts.push(self.data.len());
        self.spans.push(span);
        Opcode::Add.encode(self);
    }

    pub fn emit_subtract(&mut self, span: Span) {
        self.starts.push(self.data.len());
        self.spans.push(span);
        Opcode::Subtract.encode(self);
    }

    pub fn emit_negate(&mut self, span: Span) {
        self.starts.push(self.data.len());
        self.spans.push(span);
        Opcode::Negate.encode(self);
    }

    pub fn emit_not(&mut self, span: Span) {
        self.starts.push(self.data.len());
        self.spans.push(span);
        Opcode::Not.encode(self);
    }

    pub fn emit_equals(&mut self, span: Span) {
        self.starts.push(self.data.len());
        self.spans.push(span);
        Opcode::Equals.encode(self);
    }

    pub fn emit_less_than(&mut self, span: Span) {
        self.starts.push(self.data.len());
        self.spans.push(span);
        Opcode::LessThan.encode(self);
    }

    pub fn emit_greater_than(&mut self, span: Span) {
        self.starts.push(self.data.len());
        self.spans.push(span);
        Opcode::GreaterThan.encode(self);
    }

    pub fn emit_print(&mut self, span: Span) {
        self.starts.push(self.data.len());
        self.spans.push(span);
        Opcode::Print.encode(self);
    }

    pub fn emit_pop(&mut self, span: Span) {
        self.starts.push(self.data.len());
        self.spans.push(span);
        Opcode::Pop.encode(self);
    }

    pub fn emit_constant(&mut self, span: Span, value: LoxConstant) {
        self.starts.push(self.data.len());
        self.spans.push(span);
        let handle = self.constants.push_constant(value);
        Opcode::Const(handle).encode(self);
    }

    pub fn emit_define_global(&mut self, span: Span, identifier: &str) {
        self.starts.push(self.data.len());
        self.spans.push(span);
        let handle = self.constants.push_str(identifier);
        Opcode::DefineGlobal(handle).encode(self);
    }

    pub fn emit_get_global(&mut self, span: Span, identifier: &str) {
        self.starts.push(self.data.len());
        self.spans.push(span);
        let handle = self.constants.push_str(identifier);
        Opcode::GetGlobal(handle).encode(self);
    }

    pub fn emit_set_global(&mut self, span: Span, identifier: &str) {
        self.starts.push(self.data.len());
        self.spans.push(span);
        let handle = self.constants.push_str(identifier);
        Opcode::SetGlobal(handle).encode(self);
    }

    pub fn emit_string_literal(&mut self, span: Span, text: &str) {
        self.starts.push(self.data.len());
        self.spans.push(span);
        let handle = self.constants.push_str(text);
        Opcode::Const(handle).encode(self);
    }

    pub fn emit_get_local(&mut self, span: Span, slot: StackSlot) {
        self.starts.push(self.data.len());
        self.spans.push(span);
        Opcode::GetLocal(slot).encode(self);
    }

    pub fn emit_set_local(&mut self, span: Span, slot: StackSlot) {
        self.starts.push(self.data.len());
        self.spans.push(span);
        Opcode::SetLocal(slot).encode(self);
    }

    pub fn emit_unpatched_jump(&mut self, span: Span, conditional: bool) -> usize {
        const DEFAULT_OFFSET: i32 = -0xD0128;
        let instruction_start = self.data.len();
        self.starts.push(instruction_start);
        self.spans.push(span);
        if conditional {
            Opcode::JumpIfFalse(InstructionOffset(DEFAULT_OFFSET)).encode(self);
        } else {
            Opcode::Jump(InstructionOffset(DEFAULT_OFFSET)).encode(self);
        }
        instruction_start
    }

    pub fn emit_jump(&mut self, span: Span, offset: i32) -> usize {
        let instruction_start = self.data.len();
        self.starts.push(instruction_start);
        self.spans.push(span);
        Opcode::Jump(InstructionOffset(offset)).encode(self);
        instruction_start
    }

    // This function is a bit error prone so we probably should make better named errors.
    pub fn patch_jump(&mut self, start: usize) -> Result<(), ()> {
        let offset = (self.data.len() - start) as u32;
        let [first, second, third, fourth] = offset.to_le_bytes();
        let opcode = self.data.get(start).ok_or(())?;
        // Make sure that the patch actually applies to a jump
        if opcode != &Opcode::C_JUMP_IF_FALSE && opcode != &Opcode::C_JUMP {
            return Err(());
        }
        // Make sure that we can write the bytes i.e. the instruction stream is large enough
        if start + 4 >= self.data.len() {
            return Err(());
        }

        *self.data.get_mut(start + 1).ok_or(())? = first;
        *self.data.get_mut(start + 2).ok_or(())? = second;
        *self.data.get_mut(start + 3).ok_or(())? = third;
        *self.data.get_mut(start + 4).ok_or(())? = fourth;

        Ok(())
    }

    pub fn get_label(&self) -> usize {
        self.data.len()
    }

    pub fn finish(self) -> Chunk<'src> {
        Chunk {
            name: self.name,
            data: self.data.into(),
            spans: self.spans.into(),
            constants: self.constants,
            text: &self.text,
            line_breaks: self.line_breaks,
            starts: self.starts.into(),
        }
    }
}

pub struct Chunk<'src> {
    name: IdentName,
    text: &'src str,
    line_breaks: LineBreaks,
    data: Arc<[u8]>,
    spans: Arc<[Span]>,
    starts: Arc<[usize]>,
    constants: ConstantPool,
}

struct OpcodeIterator<'a, 'src> {
    text: &'src str,
    starts: &'a [usize],
    line_breaks: &'a LineBreaks,
    data: &'a [u8],
    index: usize,
}

impl<'a, 'src> std::iter::Iterator for OpcodeIterator<'a, 'src> {
    type Item = (Option<Opcode>, usize);

    fn next(&mut self) -> Option<Self::Item> {
        let start = *self.starts.get(self.index)?;
        let value: Option<(Option<Opcode>, usize)> = match Opcode::decode_at(&self.data, start) {
            Ok(Some((opcode, _))) => Some((Some(opcode), start)),
            Ok(None) => None,
            Err(_) => Some((None, start)),
        };

        self.index += 1;
        value
    }
}

impl<'src> Chunk<'src> {
    fn iter<'a>(&'a self) -> OpcodeIterator<'a, 'src> {
        OpcodeIterator {
            data: &self.data,
            index: 0,
            text: &self.text,
            line_breaks: &self.line_breaks,
            starts: &self.starts,
        }
    }

    fn get_span(&self, index: usize) -> Option<Span> {
        assert_eq!(
            self.starts.len(),
            self.spans.len(),
            "The spans and starts should be the same size."
        );
        let span_index = self.starts.binary_search(&index).ok()?;
        Some(
            self.spans
                .get(span_index)
                .expect("Already checked that all `self.starts` indices are valid `self.spans` indices.")
                .clone(),
        )
    }

    pub fn decode_at(&self, index: usize) -> Result<Option<(Opcode, usize, Span)>, DecodeError> {
        match Opcode::decode_at(&self.data, index) {
            Ok(Some((opcode, next_offset))) if self.starts.contains(&index) => {
                let span = self
                    .get_span(index)
                    .expect("`{index}` is a valid instruction start so its span must exist.");
                Ok(Some((opcode, next_offset, span)))
            }
            Ok(_) => Ok(None),
            Err(e) => Err(e),
        }
    }

    pub fn get_data<'a>(&'a self) -> &'a [u8] {
        &self.data
    }

    pub fn get_constant(&self, handle: ConstRef) -> Option<&LoxConstant> {
        self.constants.get(handle)
    }

    pub fn get_string_through_ref(&self, handle: ConstRef) -> Option<&str> {
        self.constants.get_string_through_ref(handle)
    }

    pub fn get_string(&self, handle: InternStringHandle) -> Option<&str> {
        self.constants.get_string(handle)
    }

    pub fn disassemble(&self) -> String {
        const INDENT: &'static str = "  ";
        let max_line = self.line_breaks.get_max_line();
        let num_digits = 4usize.max((max_line.checked_ilog10().unwrap_or(0) + 1) as usize);

        let mut buffer = String::new();
        write!(buffer, "Chunk <{}>:\n", self.name).expect(WRITE_FMT_MSG);
        let mut previous_line_number: Option<u32> = None;
        for (instruction_index, (opcode, offset)) in self.iter().enumerate() {
            let span = self
                .spans
                .get(instruction_index)
                .expect("# of spans == # of opcodes");
            let line_number = self.line_breaks.get_line_from_span(*span);
            write!(buffer, "{INDENT}{offset:04x}:").expect(WRITE_FMT_MSG);

            let same = match previous_line_number {
                Some(previous) => previous == line_number,
                None => false,
            };
            if same {
                write!(buffer, "{:>width$}| ", " ", width = num_digits - 1).expect(WRITE_FMT_MSG);
            } else {
                write!(
                    buffer,
                    "{:>width$} ",
                    format!("L{line_number}"),
                    width = num_digits
                )
                .expect(WRITE_FMT_MSG);
            }
            if let Some(opcode) = opcode {
                opcode.format(&mut buffer, self);
            } else {
                buffer.push_str("invalid");
            }
            buffer.push('\n');

            previous_line_number = Some(line_number);
        }
        buffer.push('\n');
        buffer
    }
}

struct Local {
    name: InternStringHandle,
    depth: usize,
}

pub struct Compiler {
    locals: Vec<Local>,
    interner: StringInterner,
    depth: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            locals: Vec::new(),
            depth: 0,
            interner: StringInterner::new(),
        }
    }

    fn add_local(&mut self, ident: &Ident) {
        let handle = self.interner.intern(&ident.name);
        let local = Local {
            name: handle,
            depth: self.depth,
        };
        self.locals.push(local);
    }

    fn enter_scope(&mut self) {
        self.depth += 1;
    }

    #[must_use]
    fn exit_scope(&mut self) -> usize {
        let old_depth = self.depth;
        self.depth = self.depth.saturating_sub(1);

        if let Some(index) = self
            .locals
            .iter()
            .position(|local| local.depth >= old_depth)
        {
            self.locals.drain(index..).count()
        } else {
            0
        }
    }

    pub fn compile<'src>(mut self, program: &ResolvedProgram, text: &'src str) -> Chunk<'src> {
        let mut chunk = IncompleteChunk::new("PROGRAM".into(), text);

        for stmt in program.iter() {
            self.compile_stmt(program, &mut chunk, stmt);
        }

        chunk.finish()
    }

    fn compile_stmt<'stmt>(
        &mut self,
        program: &ResolvedProgram,
        chunk: &mut IncompleteChunk,
        stmt: Statement<'stmt>,
    ) {
        match stmt {
            Statement::VariableDecl(decl) => self.compile_variable_decl(program, chunk, decl),
            Statement::FunctionDecl(_) => todo!(),
            Statement::ClassDecl(_) => todo!(),
            Statement::Expression(stmt) => self.compile_expression_stmt(program, chunk, stmt),
            Statement::Print(stmt) => self.compile_print_stmt(program, chunk, stmt),
            Statement::Block(stmt) => self.compile_block_stmt(program, chunk, stmt),
            Statement::If(stmt) => self.compile_if_stmt(program, chunk, stmt),
            Statement::While(stmt) => self.compile_while_stmt(program, chunk, stmt),
            Statement::For(_) => todo!(),
            Statement::Return(_) => todo!(),
        }
    }

    fn compile_non_declaration<'stmt>(
        &mut self,
        program: &ResolvedProgram,
        chunk: &mut IncompleteChunk,
        stmt: NonDeclaration<'stmt>,
    ) {
        match stmt {
            NonDeclaration::Expression(stmt) => self.compile_expression_stmt(program, chunk, stmt),
            NonDeclaration::Print(stmt) => self.compile_print_stmt(program, chunk, stmt),
            NonDeclaration::Block(stmt) => self.compile_block_stmt(program, chunk, stmt),
            NonDeclaration::If(stmt) => self.compile_if_stmt(program, chunk, stmt),
            NonDeclaration::While(stmt) => self.compile_while_stmt(program, chunk, stmt),
            NonDeclaration::For(_) => todo!(),
            NonDeclaration::Return(_) => todo!(),
        }
    }
    fn compile_variable_decl(
        &mut self,
        program: &ResolvedProgram,
        chunk: &mut IncompleteChunk,
        decl: &VariableDecl,
    ) {
        if let Some(initial) = &decl.initial {
            self.compile_expression(program, chunk, initial);
        } else {
            chunk.emit_constant(decl.span, LoxConstant::Nil);
        }

        // Local scope
        if self.depth > 0 {
            self.add_local(&decl.name);
        }
        // Global scope
        else {
            chunk.emit_define_global(decl.span, &decl.name.name);
        }
    }

    fn compile_expression_stmt(
        &self,
        program: &ResolvedProgram,
        chunk: &mut IncompleteChunk,
        stmt: &ExpressionStatement,
    ) {
        self.compile_expression(program, chunk, &stmt.expr);
        chunk.emit_pop(stmt.span);
    }

    fn compile_print_stmt(
        &self,
        program: &ResolvedProgram,
        chunk: &mut IncompleteChunk,
        stmt: &PrintStatement,
    ) {
        self.compile_expression(program, chunk, &stmt.expr);
        chunk.emit_print(stmt.span);
    }

    fn compile_block_stmt(
        &mut self,
        program: &ResolvedProgram,
        chunk: &mut IncompleteChunk,
        block: &BlockStatement,
    ) {
        self.enter_scope();

        for stmt in block.iter() {
            let stmt = program
                .get_statement(stmt)
                .expect("[Compile Block]: Handles are all valid.");
            self.compile_stmt(program, chunk, stmt);
        }

        let to_pop = self.exit_scope();
        for _ in 0..to_pop {
            chunk.emit_pop(block.span);
        }
    }

    fn compile_if_stmt(
        &mut self,
        program: &ResolvedProgram,
        chunk: &mut IncompleteChunk,
        stmt: &IfStatement,
    ) {
        // Condition
        self.compile_expression(program, chunk, &stmt.condition);

        // Emit unpatched jump
        let then_jump = chunk.emit_unpatched_jump(stmt.span, true);
        chunk.emit_pop(stmt.span);

        // Compile then statement
        let then_stmt = program
            .get_statement(stmt.success)
            .expect("[Compile If]: All handles are valid.");
        self.compile_stmt(program, chunk, then_stmt);

        let else_jump = chunk.emit_unpatched_jump(stmt.span, false);
        // Patch then jump
        chunk
            .patch_jump(then_jump)
            .expect("[Compile If]: Failed to patch then jump.");

        // Handle else
        chunk.emit_pop(stmt.span);
        if let Some(failure) = stmt.failure {
            let else_stmt = program
                .get_statement(failure)
                .expect("[Compile If]: All handles are valid.");
            self.compile_stmt(program, chunk, else_stmt);
        }
        chunk
            .patch_jump(else_jump)
            .expect("[Compile If]: Failed to patch else jump.");
    }

    fn compile_while_stmt(
        &mut self,
        program: &ResolvedProgram,
        chunk: &mut IncompleteChunk,
        stmt: &WhileStatement,
    ) {
        let condition_label = chunk.get_label();
        // Condition
        self.compile_expression(program, chunk, &stmt.condition);

        // Emit unpatched jump
        let exit_jump = chunk.emit_unpatched_jump(stmt.span, true);
        chunk.emit_pop(stmt.span);

        // Compile body
        let body_stmt = program
            .get_non_declaration(stmt.body)
            .expect("[Compile While]: All handles are valid.");
        self.compile_non_declaration(program, chunk, body_stmt);

        // Set up loop
        let offset = (chunk.get_label() - condition_label) as i32;
        chunk.emit_jump(stmt.span, -offset);

        chunk
            .patch_jump(exit_jump)
            .expect("[Compile While]: Failed to patch exit jump.");
        chunk.emit_pop(stmt.span);
    }
}

// Expression compilation
impl Compiler {
    fn compile_expression(
        &self,
        program: &ResolvedProgram,
        chunk: &mut IncompleteChunk,
        expr: &Expression,
    ) {
        self.compile_expression_node(program, chunk, expr, expr.get_root_ref())
    }

    fn compile_expression_node(
        &self,
        program: &ResolvedProgram,
        chunk: &mut IncompleteChunk,
        expr: &Expression,
        node: ExpressionNodeRef,
    ) {
        let current_node = expr
            .get_node(node)
            .expect("Node ref came from the tree so it must exist.");
        let span = expr.get_span();
        match current_node {
            ExpressionNode::Atom(atom) => self.compile_expression_atom(program, chunk, atom),
            ExpressionNode::Group { inner } => {
                self.compile_expression_node(program, chunk, expr, *inner);
            }
            ExpressionNode::Prefix { operator, rhs } => {
                self.compile_expression_node(program, chunk, expr, *rhs);
                match operator {
                    PrefixOperator::Bang => chunk.emit_not(span),
                    PrefixOperator::Minus => chunk.emit_negate(span),
                }
            }
            ExpressionNode::Infix { operator, lhs, rhs } => {
                self.compile_expression_node(program, chunk, expr, *lhs);
                self.compile_expression_node(program, chunk, expr, *rhs);
                match operator {
                    InfixOperator::Multiply => chunk.emit_multiply(span),
                    InfixOperator::Divide => chunk.emit_divide(span),
                    InfixOperator::Add => chunk.emit_add(span),
                    InfixOperator::Subtract => chunk.emit_subtract(span),
                    InfixOperator::EqualEqual => chunk.emit_equals(span),
                    InfixOperator::LessThan => chunk.emit_less_than(span),
                    InfixOperator::GreaterThan => chunk.emit_greater_than(span),
                    InfixOperator::GreaterThanEqual => {
                        chunk.emit_less_than(span);
                        chunk.emit_not(span);
                    }
                    InfixOperator::LessThanEqual => {
                        chunk.emit_greater_than(span);
                        chunk.emit_not(span);
                    }
                    InfixOperator::BangEqual => {
                        chunk.emit_equals(span);
                        chunk.emit_not(span);
                    }
                }
            }
            ExpressionNode::InfixAssignment { lhs, rhs } => {
                self.compile_expression_node(program, chunk, expr, *rhs);
                let ident_name = &lhs.name;
                if let Some(slot) = self.resolve_local(ident_name) {
                    chunk.emit_set_local(span, StackSlot(slot as u32));
                } else {
                    chunk.emit_set_global(span, &lhs.name);
                }
            }
            ExpressionNode::InfixShortCircuit { operator, lhs, rhs } => match operator {
                InfixShortCircuitOperator::And => {
                    self.compile_expression_node(program, chunk, expr, *lhs);
                    // Skip if already false
                    let short_circuit_jump = chunk.emit_unpatched_jump(span, true);
                    chunk.emit_pop(span);
                    self.compile_expression_node(program, chunk, expr, *rhs);
                    // Patch jump here
                    chunk
                        .patch_jump(short_circuit_jump)
                        .expect("[Compile And]: Failed to patch short circuit jump.")
                }
                InfixShortCircuitOperator::Or => {
                    self.compile_expression_node(program, chunk, expr, *lhs);
                    // Jump if false to rhs evaluation
                    let jump_to_rhs = chunk.emit_unpatched_jump(span, true);
                    // Otherwise jump past it
                    let skip_rhs = chunk.emit_unpatched_jump(span, false);

                    // Jump to here to evaluate rhs
                    chunk
                        .patch_jump(jump_to_rhs)
                        .expect("[Compile Or]: Failed to patch jump to RHS evaluation.");
                    chunk.emit_pop(span);
                    self.compile_expression_node(program, chunk, expr, *rhs);
                    // Patch jump here
                    chunk
                        .patch_jump(skip_rhs)
                        .expect("[Compile Or]: Failed to patch jump past RHS evaluation.")
                }
            },
            ExpressionNode::Call { callee, arguments } => todo!(),
            ExpressionNode::Get { object, name } => todo!(),
            ExpressionNode::Set {
                object,
                name,
                value,
            } => todo!(),
        }
    }

    fn compile_expression_atom(
        &self,
        program: &ResolvedProgram,
        chunk: &mut IncompleteChunk,
        atom: &ExpressionAtom,
    ) {
        let _ = program;
        let span = atom.span;
        match &atom.kind {
            ExpressionAtomKind::Number(value) => {
                chunk.emit_constant(span, LoxConstant::Number(*value));
            }
            ExpressionAtomKind::Bool(value) => {
                chunk.emit_constant(span, LoxConstant::Bool(*value));
            }
            ExpressionAtomKind::Nil => {
                chunk.emit_constant(span, LoxConstant::Nil);
            }
            ExpressionAtomKind::Identifier(ident) => {
                let ident_name = &(**ident);
                if let Some(slot) = self.resolve_local(ident_name) {
                    chunk.emit_get_local(span, StackSlot(slot as u32));
                } else {
                    chunk.emit_get_global(span, ident);
                }
            }
            ExpressionAtomKind::StringLiteral(text) => {
                chunk.emit_string_literal(span, &text);
            }
            ExpressionAtomKind::This => todo!(),
            ExpressionAtomKind::Super(rc) => todo!(),
        }
    }

    fn resolve_local(&self, name: &str) -> Option<usize> {
        for (index, local) in self.locals.iter().enumerate().rev() {
            let local_name = self
                .interner
                .get_string(local.name)
                .expect("[Resolve Local]: All handles are valid.");
            if local_name == name {
                return Some(index);
            }
        }
        None
    }
}
