pub mod error;
pub mod garbage;
pub mod value;

use crate::{
    compiler::{Chunk, Compiler, LoxConstant, Opcode},
    interpreter::SystemContext,
    resolver::ResolvedProgram,
    string::{InternStringHandle, StringInterner},
};
use error::{VMError, VMRuntimeError, VMRuntimeErrorKind};
use garbage::StringAllocator;
use std::{collections::HashMap, fmt::Write};
use value::VMValue;

const WRITE_FMT_MSG: &'static str =
    "Encountered an error while attempting to write format string to buffer.";

pub struct VirtualMachine<C: SystemContext> {
    ip: usize,
    stack: Vec<VMValue>,
    context: C,

    // Globals
    globals: HashMap<InternStringHandle, VMValue>,
    interner: StringInterner,

    // Allocators
    string_allocator: StringAllocator,
}

impl<C> VirtualMachine<C>
where
    C: SystemContext,
{
    pub fn new(context: C) -> Self {
        Self {
            ip: 0,
            context,
            stack: Vec::new(),
            string_allocator: StringAllocator::new(),
            globals: HashMap::new(),
            interner: StringInterner::new(),
        }
    }

    pub fn run(mut self, program: &ResolvedProgram, text: &str) -> Result<C, VMError> {
        let compiler = Compiler::new();
        let chunk = compiler.compile(program, text);

        println!("Compiled:\n{}", chunk.disassemble());
        match self.interpret_chunk(&chunk) {
            Ok(_) => {}
            Err(e) => {
                self.trace_stack();
                println!("GLOBALS\n{}", self.print_globals());
                println!("STACK\n{}", self.print_stack());
                println!("MEMORY\n{}", self.print_memory());
                return Err(e);
            }
        }
        self.trace_stack();
        println!("GLOBALS\n{}", self.print_globals());
        println!("STACK\n{}", self.print_stack());
        println!("MEMORY\n{}", self.print_memory());

        Ok(self.context)
    }
}

impl<C> VirtualMachine<C>
where
    C: SystemContext,
{
    fn interpret_chunk(&mut self, chunk: &Chunk) -> Result<(), VMError> {
        loop {
            let (inst, offset, span) = match chunk.decode_at(self.ip) {
                Ok(Some(v)) => v,
                Ok(None) => {
                    break;
                }
                Err(e) => {
                    eprintln!("SEGFAULT AT IP = {} due to {e}", self.ip);
                    eprintln!("{}", chunk.disassemble());
                    eprintln!("{:?}", chunk.get_data());
                    return Err(e.into());
                }
            };

            match inst {
                Opcode::Const(handle) => {
                    let value = chunk
                        .get_constant(handle)
                        .expect("Compiled chunks should have valid constant handles.");
                    let value = self.convert_constant_to_value(chunk, value);
                    self.stack.push(value);
                }
                Opcode::Return => {
                    break;
                }
                Opcode::Multiply => {
                    let (lhs, rhs) = self.pop_binary_operands()?;
                    self.stack.push(
                        lhs.multiply(&rhs)
                            .map_err(|kind| VMRuntimeError { kind, span })?,
                    );
                }
                Opcode::Divide => {
                    let (lhs, rhs) = self.pop_binary_operands()?;
                    self.stack.push(
                        lhs.divide(&rhs)
                            .map_err(|kind| VMRuntimeError { kind, span })?,
                    );
                }
                Opcode::Add => {
                    let (lhs, rhs) = self.pop_binary_operands()?;
                    let value = self
                        .add(&lhs, &rhs)
                        .map_err(|kind| VMRuntimeError { kind, span })?;
                    self.stack.push(value);
                }
                Opcode::Subtract => {
                    let (lhs, rhs) = self.pop_binary_operands()?;
                    self.stack.push(
                        lhs.subtract(&rhs)
                            .map_err(|kind| VMRuntimeError { kind, span })?,
                    );
                }
                Opcode::Negate => {
                    let operand = self.pop_unary_operand()?;
                    self.stack.push(
                        operand
                            .numeric_negate()
                            .map_err(|kind| VMRuntimeError { kind, span })?,
                    );
                }
                Opcode::Not => {
                    let operand = self.pop_unary_operand()?;
                    self.stack.push(VMValue::Bool(operand.logical_not()));
                }
                Opcode::Equals => {
                    let (lhs, rhs) = self.pop_binary_operands()?;
                    let value = VMValue::Bool(self.is_equal(&lhs, &rhs));
                    self.stack.push(value);
                }
                Opcode::LessThan => {
                    let (lhs, rhs) = self.pop_binary_operands()?;
                    self.stack.push(
                        lhs.less_than(&rhs)
                            .map_err(|kind| VMRuntimeError { kind, span })?,
                    );
                }
                Opcode::GreaterThan => {
                    let (lhs, rhs) = self.pop_binary_operands()?;
                    self.stack.push(
                        lhs.greater_than(&rhs)
                            .map_err(|kind| VMRuntimeError { kind, span })?,
                    );
                }
                Opcode::Print => {
                    let expr = self.pop_unary_operand()?;
                    let mut buffer = String::new();
                    self.format_value(&expr, &mut buffer);
                    self.context.writeln(&buffer);
                }
                Opcode::Pop => {
                    let _ = self.pop_unary_operand()?;
                }
                Opcode::DefineGlobal(handle) => {
                    let identifier = chunk
                        .get_string_through_ref(handle)
                        .expect("Compiled chunks should have valid constant handles.");
                    let value = self.pop_unary_operand()?;
                    let handle = self.interner.intern(identifier);
                    self.globals.insert(handle, value);
                }
                Opcode::GetGlobal(handle) => {
                    let identifier = chunk
                        .get_string_through_ref(handle)
                        .expect("Compiled chunks should have valid constant handles.");
                    let handle = self.interner.intern(identifier);
                    let value = self.globals.get(&handle).ok_or(VMRuntimeError {
                        kind: VMRuntimeErrorKind::InvalidAccess(identifier.into()),
                        span,
                    })?;
                    self.stack.push(value.clone());
                }
                Opcode::SetGlobal(handle) => {
                    let identifier = chunk
                        .get_string_through_ref(handle)
                        .expect("Compiled chunks should have valid constant handles.");
                    let value = self.pop_unary_operand()?;
                    let handle = self.interner.intern(identifier);
                    if self.globals.contains_key(&handle) {
                        self.globals.insert(handle, value.clone());
                        self.stack.push(value);
                    } else {
                        return Err(VMRuntimeError {
                            kind: VMRuntimeErrorKind::InvalidAccess(identifier.into()),
                            span,
                        }
                        .into());
                    }
                }
                Opcode::GetLocal(slot) => {
                    let value = self.get_stack(slot.0 as usize)?;
                    self.stack.push(value.clone());
                }
                Opcode::SetLocal(slot) => {
                    let value = self.peek()?;
                    self.set_stack(value.clone(), slot.0 as usize)?;
                }
                Opcode::JumpIfFalse(offset) => {
                    let flag = self.peek()?.is_truthy();
                    if !flag {
                        self.ip = self
                            .ip
                            .checked_add_signed(offset.0 as isize)
                            .expect("jif caused overflow");
                        continue;
                    }
                }
                Opcode::Jump(offset) => {
                    self.ip = self
                        .ip
                        .checked_add_signed(offset.0 as isize)
                        .expect("jpa caused overflow");
                    continue;
                }
            }
            self.ip = offset;
        }
        Ok(())
    }

    fn get_stack(&self, slot: usize) -> Result<&VMValue, VMError> {
        let size = self.stack.len();
        Ok(self
            .stack
            .get(slot)
            .ok_or(VMError::InvalidSlot { slot, size })?)
    }

    fn set_stack(&mut self, value: VMValue, slot: usize) -> Result<(), VMError> {
        let size = self.stack.len();
        *self
            .stack
            .get_mut(slot)
            .ok_or(VMError::InvalidSlot { slot, size })? = value;
        Ok(())
    }

    fn peek(&self) -> Result<&VMValue, VMError> {
        let value = self.stack.last().ok_or(VMError::MissingStackOperands {
            expected: 1,
            actual: 0,
        })?;
        Ok(value)
    }

    fn pop_unary_operand(&mut self) -> Result<VMValue, VMError> {
        let value = self.stack.pop().ok_or(VMError::MissingStackOperands {
            expected: 1,
            actual: 0,
        })?;
        Ok(value)
    }

    fn pop_binary_operands(&mut self) -> Result<(VMValue, VMValue), VMError> {
        let rhs = self.stack.pop().ok_or(VMError::MissingStackOperands {
            expected: 2,
            actual: 0,
        })?;
        let lhs = self.stack.pop().ok_or(VMError::MissingStackOperands {
            expected: 2,
            actual: 1,
        })?;
        Ok((lhs, rhs))
    }
}

impl<C> VirtualMachine<C>
where
    C: SystemContext,
{
    pub fn add(&mut self, left: &VMValue, right: &VMValue) -> Result<VMValue, VMRuntimeErrorKind> {
        match (left, right) {
            (VMValue::Number(lhs), VMValue::Number(rhs)) => Ok(VMValue::Number(lhs + rhs)),
            (VMValue::String(lhs), VMValue::String(rhs)) => {
                let lhs = self
                    .string_allocator
                    .get(*lhs)
                    .expect("Invalid string ref?");
                let rhs = self
                    .string_allocator
                    .get(*rhs)
                    .expect("Invalid string ref?");
                let value = format!("{lhs}{rhs}");
                let handle = self.string_allocator.allocate(&value);
                Ok(VMValue::String(handle))
            }
            (lhs, rhs) => Err(VMRuntimeErrorKind::NonAddable(lhs.clone(), rhs.clone())),
        }
    }

    // Equality
    pub fn is_equal(&self, left: &VMValue, right: &VMValue) -> bool {
        match (left, right) {
            (VMValue::Number(lhs), VMValue::Number(rhs)) => lhs == rhs,
            (VMValue::Bool(lhs), VMValue::Bool(rhs)) => lhs == rhs,
            (VMValue::Nil, VMValue::Nil) => true,
            (VMValue::String(lhs), VMValue::String(rhs)) => {
                let lhs = self
                    .string_allocator
                    .get(*lhs)
                    .expect("[Equal]: LHS Handle should be valid.");
                let rhs = self
                    .string_allocator
                    .get(*rhs)
                    .expect("[Equal]: RHS Handle should be valid.");
                lhs == rhs
            }
            _ => false,
        }
    }
}

impl<C> VirtualMachine<C>
where
    C: SystemContext,
{
    fn convert_constant_to_value(&mut self, chunk: &Chunk, value: &LoxConstant) -> VMValue {
        match value {
            LoxConstant::Nil => VMValue::Nil,
            LoxConstant::Number(value) => VMValue::Number(*value),
            LoxConstant::Bool(value) => VMValue::Bool(*value),
            LoxConstant::String(handle) => {
                let value = chunk
                    .get_string(*handle)
                    .expect("String constants should have valid intern string handles.");
                let new_handle = self.string_allocator.allocate(value);
                VMValue::String(new_handle)
            }
        }
    }
}

impl<C> VirtualMachine<C>
where
    C: SystemContext,
{
    fn print_globals(&self) -> String {
        const INDENT: &'static str = "  ";
        let mut buffer = String::new();
        for (handle, value) in self.globals.iter() {
            let identifier = self
                .interner
                .get_string(*handle)
                .expect("[Print Globals]: Invalid string handle");
            write!(buffer, "{INDENT}{identifier} = ").expect(WRITE_FMT_MSG);
            self.debug_format_value(value, &mut buffer);
            buffer.push('\n');
        }
        buffer
    }

    fn print_stack(&self) -> String {
        const INDENT: &'static str = "  ";
        let mut buffer = String::new();
        for (index, value) in self.stack.iter().enumerate().rev() {
            write!(buffer, "{INDENT}{index:02}: ").expect(WRITE_FMT_MSG);
            self.debug_format_value(value, &mut buffer);
            buffer.push('\n');
        }
        buffer
    }

    fn print_memory(&self) -> String {
        const INDENT: &'static str = "  ";
        let mut buffer = String::new();
        for (index, (string, marker, generation)) in self.string_allocator.iter().enumerate() {
            write!(buffer, "{INDENT}{index:02}: ").expect(WRITE_FMT_MSG);
            write!(
                buffer,
                "[\"{string}\", alive = {marker}, generation = {generation}]"
            )
            .expect(WRITE_FMT_MSG);
            buffer.push('\n');
        }
        buffer
    }

    fn debug_format_value(&self, value: &VMValue, buffer: &mut String) {
        match value {
            VMValue::String(handle) => {
                let value = self
                    .string_allocator
                    .debug_get(*handle)
                    .unwrap_or("INVALID_STRING_HANDLE");
                write!(buffer, "\"{value}\"").expect(WRITE_FMT_MSG);
            }
            value => {
                write!(buffer, "{value}").expect(WRITE_FMT_MSG);
            }
        }
    }

    fn format_value(&self, value: &VMValue, buffer: &mut String) {
        match value {
            VMValue::String(handle) => {
                let value = self
                    .string_allocator
                    .get(*handle)
                    .unwrap_or("INVALID_STRING_HANDLE");
                write!(buffer, "{value}").expect(WRITE_FMT_MSG);
            }
            value => {
                write!(buffer, "{value}").expect(WRITE_FMT_MSG);
            }
        }
    }
}

impl<C> VirtualMachine<C>
where
    C: SystemContext,
{
    // Trace stack

    fn trace_stack(&mut self) {
        let alive: Vec<_> = self
            .stack
            .iter()
            .filter_map(|v| match v {
                VMValue::String(handle) => Some(*handle),
                _ => None,
            })
            .collect();
        self.string_allocator.mark(&alive);
    }
}
