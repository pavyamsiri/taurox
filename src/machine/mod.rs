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

    pub fn run(mut self, program: &ResolvedProgram) -> Result<C, RuntimeError> {
        let compiler = Compiler;
        let chunk = compiler.compile(program, "TEST");

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
    fn interpret_chunk(&mut self, chunk: &Chunk) -> Result<(), RuntimeError> {
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
                    panic!("SEGFAULT");
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
                    let (lhs, rhs) = self
                        .pop_binary_operands()
                        .expect("Not enough values on the stack!");
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
                    let (lhs, rhs) = self
                        .pop_binary_operands()
                        .expect("Not enough values on the stack!");
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
                    let (lhs, rhs) = self
                        .pop_binary_operands()
                        .expect("Not enough values on the stack!");
                    self.stack.push(lhs.add(&rhs).map_err(|kind| RuntimeError {
                        kind,
                        span: Span {
                            start: 0.into(),
                            length: 0.into(),
                        },
                    })?);
                }
                Opcode::Subtract => {
                    let (lhs, rhs) = self
                        .pop_binary_operands()
                        .expect("Not enough values on the stack!");
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
                    let operand = self.stack.pop().expect("Not enough values on the stack");
                    self.stack
                        .push(operand.numeric_negate().map_err(|kind| RuntimeError {
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

    fn pop_binary_operands(&mut self) -> Option<(LoxValue, LoxValue)> {
        let rhs = self.stack.pop()?;
        let lhs = self.stack.pop()?;
        Some((lhs, rhs))
    }
}
