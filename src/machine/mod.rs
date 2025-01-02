use crate::{
    compiler::{Chunk, Compiler, Opcode},
    interpreter::SystemContext,
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
        let chunk = Compiler::compile(program, "TEST");

        self.interpret_chunk(&chunk)?;

        Ok(self.context)
    }
}

impl<C> VirtualMachine<C>
where
    C: SystemContext,
{
    fn interpret_chunk(&mut self, chunk: &Chunk) -> Result<(), RuntimeError> {
        loop {
            let (inst, offset) = chunk
                .decode_at(self.ip)
                .expect("Reading bytes out of bounds should never happen unless we segfault.");

            match inst {
                Opcode::Return => {
                    println!("{}", chunk.disassemble());
                    println!("STACK\n{}", self.print_stack());
                    return Ok(());
                }
                Opcode::Const(handle) => {
                    let value = chunk
                        .get_constant(handle)
                        .expect("Compiled chunks should have valid constant handles.");
                    self.stack.push(value.into());
                }
            }
            self.ip = offset;
        }
    }

    fn print_stack(&self) -> String {
        const INDENT: &'static str = "  ";
        let mut buffer = String::new();
        for (index, value) in self.stack.iter().enumerate().rev() {
            write!(buffer, "{INDENT}{index:02}: {value}").expect(WRITE_FMT_MSG);
        }
        buffer
    }
}
