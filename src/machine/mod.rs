pub mod error;

use error::VMError;

use crate::{
    compiler::{Chunk, Compiler, Opcode},
    interpreter::SystemContext,
    lexer::Span,
    resolver::ResolvedProgram,
    value::{error::RuntimeError, LoxValue},
};
use std::fmt::Write;

const WRITE_FMT_MSG: &'static str =
    "Encountered an error while attempting to write format string to buffer.";

pub struct VirtualMachine<C: SystemContext> {
    ip: usize,
    stack: Vec<LoxValue>,
    context: C,
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
        }
    }

    pub fn run(mut self, program: &ResolvedProgram, text: &str) -> Result<C, VMError> {
        let compiler = Compiler;
        let chunk = compiler.compile(program, text);

        self.interpret_chunk(&chunk)?;
        println!("{}", chunk.disassemble());
        println!("STACK\n{}", self.print_stack());

        Ok(self.context)
    }
}

impl<C> VirtualMachine<C>
where
    C: SystemContext,
{
    fn interpret_chunk(&mut self, chunk: &Chunk) -> Result<(), VMError> {
        loop {
            let (inst, offset) = match chunk.decode_at(self.ip) {
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
                    self.stack.push(value.into());
                }
                Opcode::Return => {
                    break;
                }
                Opcode::Multiply => {
                    let (lhs, rhs) = self.pop_binary_operands()?;
                    self.stack
                        .push(lhs.multiply(&rhs).map_err(|kind| RuntimeError {
                            kind,
                            span: Span {
                                start: 0.into(),
                                length: 0.into(),
                            },
                        })?);
                }
                Opcode::Divide => {
                    let (lhs, rhs) = self.pop_binary_operands()?;
                    self.stack
                        .push(lhs.divide(&rhs).map_err(|kind| RuntimeError {
                            kind,
                            span: Span {
                                start: 0.into(),
                                length: 0.into(),
                            },
                        })?);
                }
                Opcode::Add => {
                    let (lhs, rhs) = self.pop_binary_operands()?;
                    self.stack.push(lhs.add(&rhs).map_err(|kind| RuntimeError {
                        kind,
                        span: Span {
                            start: 0.into(),
                            length: 0.into(),
                        },
                    })?);
                }
                Opcode::Subtract => {
                    let (lhs, rhs) = self.pop_binary_operands()?;
                    self.stack
                        .push(lhs.subtract(&rhs).map_err(|kind| RuntimeError {
                            kind,
                            span: Span {
                                start: 0.into(),
                                length: 0.into(),
                            },
                        })?);
                }
                Opcode::Negate => {
                    let operand = self.pop_unary_operand()?;
                    self.stack
                        .push(operand.numeric_negate().map_err(|kind| RuntimeError {
                            kind,
                            span: Span {
                                start: 0.into(),
                                length: 0.into(),
                            },
                        })?);
                }
                Opcode::Not => {
                    let operand = self.pop_unary_operand()?;
                    self.stack.push(LoxValue::Bool(operand.logical_not()));
                }
                Opcode::Equals => {
                    let (lhs, rhs) = self.pop_binary_operands()?;
                    self.stack.push(LoxValue::Bool(lhs.is_equal(&rhs)));
                }
                Opcode::LessThan => {
                    let (lhs, rhs) = self.pop_binary_operands()?;
                    self.stack
                        .push(lhs.less_than(&rhs).map_err(|kind| RuntimeError {
                            kind,
                            span: Span {
                                start: 0.into(),
                                length: 0.into(),
                            },
                        })?);
                }
                Opcode::GreaterThan => {
                    let (lhs, rhs) = self.pop_binary_operands()?;
                    self.stack
                        .push(lhs.greater_than(&rhs).map_err(|kind| RuntimeError {
                            kind,
                            span: Span {
                                start: 0.into(),
                                length: 0.into(),
                            },
                        })?);
                }
            }
            self.ip = offset;
        }
        Ok(())
    }

    fn print_stack(&self) -> String {
        const INDENT: &'static str = "  ";
        let mut buffer = String::new();
        for (index, value) in self.stack.iter().enumerate().rev() {
            write!(buffer, "{INDENT}{index:02}: {value}").expect(WRITE_FMT_MSG);
        }
        buffer
    }

    fn pop_unary_operand(&mut self) -> Result<LoxValue, VMError> {
        let value = self.stack.pop().ok_or(VMError::MissingStackOperands {
            expected: 1,
            actual: 0,
        })?;
        Ok(value)
    }

    fn pop_binary_operands(&mut self) -> Result<(LoxValue, LoxValue), VMError> {
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
