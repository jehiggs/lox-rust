use crate::chunk;

const STACK_SIZE: usize = 256;

#[derive(Debug)]
pub struct VM {
    chunk: chunk::Chunk,
    ip: usize,
    stack: Vec<chunk::Value>,
}

pub enum InterpretResult {
    OK,
    CompileError,
    RuntimeError,
}

impl VM {
    pub fn new(chunk: chunk::Chunk) -> Self {
        VM {
            chunk,
            ip: 0,
            stack: Vec::with_capacity(STACK_SIZE),
        }
    }

    pub fn interpret(&mut self) -> InterpretResult {
        loop {
            let instruction = self.chunk.read_code(self.ip);
            #[cfg(debug_assertions)]
            self.debug_instruction(instruction);
            self.ip += 1;
            match *instruction {
                chunk::OpCode::Constant(index) => {
                    let constant = self.chunk.read_constant(index.into());
                    self.stack.push(constant);
                }
                chunk::OpCode::ConstantLong(index) => {
                    let constant = self.chunk.read_constant(index);
                    self.stack.push(constant);
                }
                chunk::OpCode::Return => {
                    let value = self.stack.pop().unwrap_or(0.);
                    println!("Output is: {}", value);
                    return InterpretResult::OK;
                }
                chunk::OpCode::Negate => {
                    if let Some(value) = self.stack.pop() {
                        self.stack.push(-value);
                    }
                }
                chunk::OpCode::Add => self.binary_op(std::ops::Add::add),
                chunk::OpCode::Divide => self.binary_op(std::ops::Div::div),
                chunk::OpCode::Multiply => self.binary_op(std::ops::Mul::mul),
                chunk::OpCode::Subtract => self.binary_op(std::ops::Sub::sub),
            }
        }
    }

    fn binary_op<T: Fn(f64, f64) -> f64>(&mut self, op: T) {
        let a = self.stack.pop();
        let b = self.stack.pop();
        match (b, a) {
            (Some(i), Some(j)) => self.stack.push(op(i, j)),
            _ => panic!("Could not perform binary operation due to missing operands."),
        }
    }

    #[cfg(debug_assertions)]
    fn debug_instruction(&self, instruction: &chunk::OpCode) {
        println!("{:?}", self.stack);
        self.chunk.disassemble_instruction(self.ip, instruction);
    }
}

#[cfg(test)]
mod tests {
    use crate::chunk::Chunk;

    use super::*;

    #[test]
    fn execute_binary_ops() {
        let mut vm = VM::new(Chunk::new());
        vm.stack.push(1.0);
        vm.stack.push(2.0);
        vm.binary_op(std::ops::Add::add);
        assert_eq!(3.0, vm.stack.pop().unwrap());
        vm.stack.push(4.0);
        vm.stack.push(3.0);
        vm.binary_op(std::ops::Sub::sub);
        assert_eq!(1.0, vm.stack.pop().unwrap());

        vm.stack.push(2.0);
        vm.stack.push(3.0);
        vm.binary_op(std::ops::Mul::mul);
        assert_eq!(6.0, vm.stack.pop().unwrap());

        vm.stack.push(6.0);
        vm.stack.push(3.0);
        vm.binary_op(std::ops::Div::div);
        assert_eq!(2.0, vm.stack.pop().unwrap());
    }
}
