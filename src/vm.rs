use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

use crate::chunk;
use crate::compiler;
use crate::error::Error;

const STACK_SIZE: usize = 256;
const TABLE_SIZE: usize = 10;

#[derive(Debug)]
pub struct VM {
    ip: usize,
    stack: Vec<chunk::Value>,
    strings: HashSet<Rc<str>>,
    globals: HashMap<Rc<str>, chunk::Value>,
}

impl VM {
    pub fn new() -> Self {
        VM {
            ip: 0,
            stack: Vec::with_capacity(STACK_SIZE),
            strings: HashSet::with_capacity(TABLE_SIZE),
            globals: HashMap::with_capacity(TABLE_SIZE),
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

    #[allow(clippy::too_many_lines)]
    fn run(&mut self, chunk: &chunk::Chunk) -> Result<(), Error> {
        loop {
            let instruction = chunk.read_code(self.ip);
            #[cfg(debug_assertions)]
            self.debug_instruction(chunk, instruction);
            self.ip += 1;
            match instruction {
                &chunk::OpCode::Constant(index) => {
                    let constant = chunk.read_constant(index.into());
                    self.push_constant(constant);
                }
                &chunk::OpCode::ConstantLong(index) => {
                    let constant = chunk.read_constant(index);
                    self.push_constant(constant);
                }
                chunk::OpCode::Return => {
                    return Ok(());
                }
                chunk::OpCode::Negate => {
                    if let Some(chunk::Value::Number(value)) = self.stack.pop() {
                        self.stack.push(chunk::Value::Number(-value));
                    } else {
                        return Err(self.runtime_error(chunk, "Tried to negate non-number value."));
                    }
                }
                chunk::OpCode::Add => match self.peek(0) {
                    Some(chunk::Value::String(_)) => self.concatenate(chunk)?,
                    Some(chunk::Value::Number(_)) => self.binary_op(std::ops::Add::add, chunk)?,
                    _ => Err(self.runtime_error(chunk, "Tried to add two non-addable types"))?,
                },
                chunk::OpCode::Divide => self.binary_op(std::ops::Div::div, chunk)?,
                chunk::OpCode::Multiply => self.binary_op(std::ops::Mul::mul, chunk)?,
                chunk::OpCode::Subtract => self.binary_op(std::ops::Sub::sub, chunk)?,
                chunk::OpCode::False => self.stack.push(chunk::Value::Bool(false)),
                chunk::OpCode::Nil => self.stack.push(chunk::Value::Nil),
                chunk::OpCode::True => self.stack.push(chunk::Value::Bool(true)),
                chunk::OpCode::Not => {
                    if let Some(value) = self.stack.pop() {
                        self.stack.push(chunk::Value::Bool(value.is_falsey()));
                    } else {
                        return Err(
                            self.runtime_error(chunk, "Missing value to perform not operation.")
                        );
                    }
                }
                chunk::OpCode::Equal => match (self.stack.pop(), self.stack.pop()) {
                    (Some(a), Some(b)) => self.stack.push(chunk::Value::Bool(a == b)),
                    _ => Err(self.runtime_error(
                        chunk,
                        "Missing values when trying to compare for equality.",
                    ))?,
                },
                chunk::OpCode::Greater => {
                    let a = self.stack.pop();
                    let b = self.stack.pop();
                    match (b, a) {
                        (Some(chunk::Value::Number(i)), Some(chunk::Value::Number(j))) => {
                            self.stack.push(chunk::Value::Bool(i > j));
                        }
                        _ => Err(self.runtime_error(
                            chunk,
                            "Cannot compare greater two non-number operands.",
                        ))?,
                    }
                }
                chunk::OpCode::Less => {
                    let a = self.stack.pop();
                    let b = self.stack.pop();
                    match (b, a) {
                        (Some(chunk::Value::Number(i)), Some(chunk::Value::Number(j))) => {
                            self.stack.push(chunk::Value::Bool(i < j));
                        }
                        _ => Err(self
                            .runtime_error(chunk, "Cannot compare less two non-number operands."))?,
                    }
                }
                chunk::OpCode::Print => {
                    let item = self.stack.pop();
                    if let Some(value) = item {
                        println!("{value}");
                    } else {
                        Err(self.runtime_error(chunk, "Could not find a value to print."))?;
                    }
                }
                chunk::OpCode::Pop => {
                    self.stack.pop();
                }
                chunk::OpCode::DefineGlobal(index) => {
                    if let chunk::Value::String(string) = chunk.read_constant(*index) {
                        let value = self.stack.pop().ok_or_else(|| {
                            self.runtime_error(chunk, "Missing value for global variable.")
                        })?;
                        self.globals.insert(Rc::clone(string), value);
                    } else {
                        Err(self.runtime_error(chunk, "Name of variable was not a string."))?;
                    }
                }
                chunk::OpCode::GetGlobal(index) => {
                    if let chunk::Value::String(string) = chunk.read_constant(*index) {
                        let value = self.globals.get(string).ok_or_else(|| {
                            self.runtime_error(chunk, "Could not read value for global variable.")
                        })?;
                        self.stack.push(value.clone());
                    }
                }
            }
        }
    }

    fn push_constant(&mut self, constant: &chunk::Value) {
        if let chunk::Value::String(string) = constant {
            self.strings.insert(Rc::clone(string));
        }
        self.stack.push(constant.clone());
    }

    fn binary_op<T: Fn(f64, f64) -> f64>(
        &mut self,
        op: T,
        chunk: &chunk::Chunk,
    ) -> Result<(), Error> {
        let a = self.stack.pop();
        let b = self.stack.pop();
        match (b, a) {
            (Some(chunk::Value::Number(i)), Some(chunk::Value::Number(j))) => {
                self.stack.push(chunk::Value::Number(op(i, j)));
                Ok(())
            }
            _ => Err(self.runtime_error(
                chunk,
                "Operands were missing or incorrect to binary operation.",
            )),
        }
    }

    fn concatenate(&mut self, chunk: &chunk::Chunk) -> Result<(), Error> {
        let a = self.stack.pop();
        let b = self.stack.pop();
        match (b, a) {
            (Some(chunk::Value::String(prefix)), Some(chunk::Value::String(suffix))) => {
                let string = format!("{prefix}{suffix}");
                let new = if let Some(item) = self.strings.get(&string[..]) {
                    Rc::clone(item)
                } else {
                    let item = Rc::from(&string[..]);
                    self.strings.insert(Rc::clone(&item));
                    item
                };
                self.stack.push(chunk::Value::String(new));
                Ok(())
            }
            _ => Err(self.runtime_error(
                chunk,
                "Operand was missing or incorrect in string concatenation.",
            )),
        }
    }

    #[cfg(debug_assertions)]
    fn debug_instruction(&self, chunk: &chunk::Chunk, instruction: &chunk::OpCode) {
        println!("{:?}", self.stack);
        chunk.disassemble_instruction(self.ip, instruction);
    }

    fn runtime_error(&self, chunk: &chunk::Chunk, message: &'static str) -> Error {
        eprintln!(
            "[Line {}] Error in script: {}",
            chunk.get_line(self.ip),
            message
        );
        Error::RuntimeError(message)
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn basic_arithmetic() {
        let source = "1 + 2;";
        let mut vm = VM::new();
        let result = vm.interpret(source);
        assert_eq!(Ok(()), result);
    }

    #[test]
    fn negate_non_number() {
        let source = "-false;";
        let mut vm = VM::new();
        let result = vm.interpret(source);
        assert!(matches!(result, Err(Error::RuntimeError(_))));
    }

    #[test]
    fn compare_non_numbers() {
        let source = "false > true;";
        let mut vm = VM::new();
        let result = vm.interpret(source);
        assert!(matches!(result, Err(Error::RuntimeError(_))));
    }

    #[test]
    fn multiple_comparisons() {
        let source = "1 < 2 < 3;";
        let mut vm = VM::new();
        let result = vm.interpret(source);
        assert!(matches!(result, Err(Error::RuntimeError(_))));
    }

    #[test]
    fn string_concatenate() {
        let source = "\"foo\" + \"bar\";";
        let mut vm = VM::new();
        let result = vm.interpret(source);
        assert_eq!(Ok(()), result);
    }

    #[test]
    fn addition_wrong_types() {
        let source = "1 + \"foo\";";
        let mut vm = VM::new();
        let result = vm.interpret(source);
        assert!(matches!(result, Err(Error::RuntimeError(_))));
    }

    #[test]
    fn string_equality() {
        let source = "\"foo\" == \"foo\";";
        let mut vm = VM::new();
        let result = vm.interpret(source);
        assert_eq!(Ok(()), result);
    }

    #[test]
    fn print_statement() {
        let source = "print 1 * 2;";
        let mut vm = VM::new();
        let result = vm.interpret(source);
        assert_eq!(Ok(()), result);
    }
}
