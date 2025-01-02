mod opcode;

use crate::lexer::{LineBreaks, Span};
use crate::parser::expression::{
    Expression, ExpressionAtom, ExpressionAtomKind, ExpressionNode, ExpressionNodeRef,
    InfixOperator, PrefixOperator,
};
use crate::parser::statement::{ExpressionStatement, Statement};
use crate::resolver::ResolvedProgram;
use crate::string::IdentName;
pub use opcode::DecodeError;
pub use opcode::{ConstRef, ConstantPool, LoxConstant, Opcode};
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

    pub fn emit_return(&mut self, span: Span) {
        self.spans.push(span);
        Opcode::Return.encode(self);
    }

    pub fn emit_multiply(&mut self, span: Span) {
        self.spans.push(span);
        Opcode::Multiply.encode(self);
    }

    pub fn emit_divide(&mut self, span: Span) {
        self.spans.push(span);
        Opcode::Divide.encode(self);
    }

    pub fn emit_add(&mut self, span: Span) {
        self.spans.push(span);
        Opcode::Add.encode(self);
    }

    pub fn emit_subtract(&mut self, span: Span) {
        self.spans.push(span);
        Opcode::Subtract.encode(self);
    }

    pub fn emit_negate(&mut self, span: Span) {
        self.spans.push(span);
        Opcode::Negate.encode(self);
    }

    pub fn emit_not(&mut self, span: Span) {
        self.spans.push(span);
        Opcode::Not.encode(self);
    }

    pub fn emit_equals(&mut self, span: Span) {
        self.spans.push(span);
        Opcode::Equals.encode(self);
    }

    pub fn emit_less_than(&mut self, span: Span) {
        self.spans.push(span);
        Opcode::LessThan.encode(self);
    }

    pub fn emit_greater_than(&mut self, span: Span) {
        self.spans.push(span);
        Opcode::GreaterThan.encode(self);
    }

    pub fn emit_constant(&mut self, span: Span, value: LoxConstant) {
        let handle = self.constants.push_constant(value);
        self.spans.push(span);
        Opcode::Const(handle).encode(self);
    }

    pub fn finish(self) -> Chunk<'src> {
        Chunk {
            name: self.name,
            data: self.data.into(),
            spans: self.spans.into(),
            constants: self.constants,
            text: &self.text,
            line_breaks: self.line_breaks,
        }
    }
}

pub struct Chunk<'src> {
    name: IdentName,
    text: &'src str,
    line_breaks: LineBreaks,
    data: Arc<[u8]>,
    spans: Arc<[Span]>,
    constants: ConstantPool,
}

struct OpcodeIterator<'a, 'src> {
    text: &'src str,
    line_breaks: &'a LineBreaks,
    data: &'a [u8],
    index: usize,
}

impl<'a, 'src> std::iter::Iterator for OpcodeIterator<'a, 'src> {
    type Item = (usize, Opcode);

    fn next(&mut self) -> Option<Self::Item> {
        let value = Opcode::decode(&self.data);
        if let Ok(Some((opcode, rest))) = value {
            // Should not underflow because `rest.len()` is at most `self.data.len()`.
            let offset = self.index;
            self.index += self.data.len() - rest.len();
            self.data = rest;
            Some((offset, opcode))
        } else {
            None
        }
    }
}

impl<'src> Chunk<'src> {
    fn iter<'a>(&'a self) -> OpcodeIterator<'a, 'src> {
        OpcodeIterator {
            data: &self.data,
            index: 0,
            text: &self.text,
            line_breaks: &self.line_breaks,
        }
    }

    pub fn decode_at(&self, index: usize) -> Result<Option<(Opcode, usize)>, DecodeError> {
        let slice = &self.data[index..];
        let (opcode, rest) = match Opcode::decode(&self.data[index..])? {
            Some(s) => s,
            None => return Ok(None),
        };
        let offset = slice.len() - rest.len() + index;
        Ok(Some((opcode, offset)))
    }

    pub fn get_data<'a>(&'a self) -> &'a [u8] {
        &self.data
    }

    pub fn get_constant(&self, handle: ConstRef) -> Option<&LoxConstant> {
        self.constants.get(handle)
    }

    pub fn disassemble(&self) -> String {
        const INDENT: &'static str = "  ";
        let max_line = self.line_breaks.get_max_line();
        let num_digits = 4usize.max((max_line.checked_ilog10().unwrap_or(0) + 1) as usize);

        let mut buffer = String::new();
        write!(buffer, "Chunk <{}>:\n", self.name).expect(WRITE_FMT_MSG);
        let mut previous_line_number: Option<u32> = None;
        for (instruction_index, (offset, opcode)) in self.iter().enumerate() {
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
                write!(buffer, "{:>width$}| ", " ", width = num_digits).expect(WRITE_FMT_MSG);
            } else {
                write!(buffer, "{:>width$}{line_number} ", "L", width = num_digits)
                    .expect(WRITE_FMT_MSG);
            }
            opcode.format(&mut buffer, self);
            buffer.push('\n');

            previous_line_number = Some(line_number);
        }
        buffer.push('\n');
        buffer
    }
}

pub struct Compiler;

impl Compiler {
    pub fn compile<'src>(&self, program: &ResolvedProgram, text: &'src str) -> Chunk<'src> {
        let mut chunk = IncompleteChunk::new("PROGRAM".into(), text);

        for stmt in program.iter() {
            self.compile_stmt(program, &mut chunk, stmt);
        }

        chunk.finish()
    }

    fn compile_stmt<'stmt>(
        &self,
        program: &ResolvedProgram,
        chunk: &mut IncompleteChunk,
        stmt: Statement<'stmt>,
    ) {
        match stmt {
            Statement::VariableDecl(_) => todo!(),
            Statement::FunctionDecl(_) => todo!(),
            Statement::ClassDecl(_) => todo!(),
            Statement::Expression(stmt) => self.compile_expression_stmt(program, chunk, stmt),
            Statement::Print(_) => todo!(),
            Statement::Block(_) => todo!(),
            Statement::If(_) => todo!(),
            Statement::While(_) => todo!(),
            Statement::For(_) => todo!(),
            Statement::Return(_) => todo!(),
        }
    }

    fn compile_expression_stmt(
        &self,
        program: &ResolvedProgram,
        chunk: &mut IncompleteChunk,
        stmt: &ExpressionStatement,
    ) {
        self.compile_expression(program, chunk, &stmt.expr);
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
            ExpressionNode::InfixAssignment { lhs, rhs } => todo!(),
            ExpressionNode::InfixShortCircuit { operator, lhs, rhs } => {
                todo!()
            }
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
            ExpressionAtomKind::Identifier(rc) => todo!(),
            ExpressionAtomKind::StringLiteral(rc) => todo!(),
            ExpressionAtomKind::This => todo!(),
            ExpressionAtomKind::Super(rc) => todo!(),
        }
    }
}
