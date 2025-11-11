use crate::chunk;
use crate::compiler;
use crate::error::Error;

const STACK_SIZE: usize = 256;

#[derive(Debug)]
pub struct VM {
    ip: usize,
    stack: Vec<chunk::Value>,
}

impl VM {
    pub fn new() -> Self {
        VM {
            ip: 0,
            stack: Vec::with_capacity(STACK_SIZE),
        }
    }

    pub fn interpret(&mut self, source: &str) -> Result<(), Error> {
        self.reset();
        let compiler = compiler::Compiler::new(source);
        let chunk = compiler.compile()?;
        #[cfg(debug_assertions)]
        chunk.disassemble_chunk("Test");
        self.run(&chunk)
    }

    fn reset(&mut self) {
        self.ip = 0;
        self.stack.clear();
    }

    fn peek(&mut self, distance: usize) -> Option<&chunk::Value> {
        self.stack.get(self.stack.len() - 1 - distance)
    }

    fn run(&mut self, chunk: &chunk::Chunk) -> Result<(), Error> {
        loop {
            let instruction = chunk.read_code(self.ip);
            #[cfg(debug_assertions)]
            self.debug_instruction(chunk, instruction);
            self.ip += 1;
            match *instruction {
                chunk::OpCode::Constant(index) => {
                    let constant = chunk.read_constant(index.into());
                    self.stack.push(*constant);
                }
                chunk::OpCode::ConstantLong(index) => {
                    let constant = chunk.read_constant(index);
                    self.stack.push(*constant);
                }
                chunk::OpCode::Return => {
                    let value = self.stack.pop().unwrap_or(chunk::Value::Number(0.));
                    println!("Output is: {}", value);
                    return Ok(());
                }
                chunk::OpCode::Negate => {
                    if let Some(chunk::Value::Number(value)) = self.stack.pop() {
                        self.stack.push(chunk::Value::Number(-value));
                    } else {
                        return Err(Error::RuntimeError("Tried to negate non-number value."));
                    }
                }
                chunk::OpCode::Add => self.binary_op(std::ops::Add::add)?,
                chunk::OpCode::Divide => self.binary_op(std::ops::Div::div)?,
                chunk::OpCode::Multiply => self.binary_op(std::ops::Mul::mul)?,
                chunk::OpCode::Subtract => self.binary_op(std::ops::Sub::sub)?,
            }
        }
    }

    fn binary_op<T: Fn(f64, f64) -> f64>(&mut self, op: T) -> Result<(), Error> {
        let a = self.stack.pop();
        let b = self.stack.pop();
        match (b, a) {
            (Some(chunk::Value::Number(i)), Some(chunk::Value::Number(j))) => {
                Ok(self.stack.push(chunk::Value::Number(op(i, j))))
            }
            _ => Err(Error::RuntimeError(
                "Missing one or more operands to a binary operation.",
            )),
        }
    }

    #[cfg(debug_assertions)]
    fn debug_instruction(&self, chunk: &chunk::Chunk, instruction: &chunk::OpCode) {
        println!("{:?}", self.stack);
        chunk.disassemble_instruction(self.ip, instruction);
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn basic_arithmetic() {
        let source = "1 + 2";
        let mut vm = VM::new();
        let result = vm.interpret(source);
        assert_eq!(Ok(()), result);
    }
}
