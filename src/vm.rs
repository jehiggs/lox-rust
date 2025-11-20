use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

use crate::chunk;
use crate::compiler;
use crate::error::Error;
use crate::object;

const STACK_SIZE: usize = 256;
const TABLE_SIZE: usize = 10;
const FRAME_SIZE: usize = 64;

#[derive(Debug)]
pub struct VM {
    stack: Vec<chunk::Value>,
    strings: HashSet<Rc<str>>,
    globals: HashMap<Rc<str>, chunk::Value>,
    call_stack: Vec<CallFrame>,
}

impl VM {
    pub fn new() -> Self {
        VM {
            stack: Vec::with_capacity(STACK_SIZE),
            strings: HashSet::with_capacity(TABLE_SIZE),
            globals: HashMap::with_capacity(TABLE_SIZE),
            call_stack: Vec::with_capacity(FRAME_SIZE),
        }
    }

    pub fn interpret(&mut self, source: &str) -> Result<(), Error> {
        self.reset();
        let compiler = compiler::Compiler::new(source);
        let script = compiler.compile()?;
        #[cfg(debug_assertions)]
        {
            let name = if script.name.is_empty() {
                "<script>"
            } else {
                &script.name
            };
            script.chunk.disassemble_chunk(name);
        }
        let initial_frame = CallFrame::new(&Rc::from(script), 0);
        self.call_stack.push(initial_frame);
        self.run()
    }

    fn reset(&mut self) {
        self.stack.clear();
    }

    fn peek(&mut self, distance: usize) -> Option<&chunk::Value> {
        self.stack.get(self.stack.len() - 1 - distance)
    }

    #[allow(clippy::too_many_lines)]
    fn run(&mut self) -> Result<(), Error> {
        let mut frame = self
            .call_stack
            .pop()
            .ok_or(Error::RuntimeError("No frame on call stack."))?;
        let chunk = &frame.function.chunk;
        loop {
            let instruction = chunk.read_code(frame.ip);
            #[cfg(debug_assertions)]
            self.debug_instruction(&frame, instruction);
            frame.ip += 1;
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
                        return Err(Self::runtime_error(
                            &frame,
                            "Tried to negate non-number value.",
                        ));
                    }
                }
                chunk::OpCode::Add => match self.peek(0) {
                    Some(chunk::Value::String(_)) => self.concatenate(&frame)?,
                    Some(chunk::Value::Number(_)) => self.binary_op(std::ops::Add::add, &frame)?,
                    _ => Err(Self::runtime_error(
                        &frame,
                        "Tried to add two non-addable types",
                    ))?,
                },
                chunk::OpCode::Divide => self.binary_op(std::ops::Div::div, &frame)?,
                chunk::OpCode::Multiply => self.binary_op(std::ops::Mul::mul, &frame)?,
                chunk::OpCode::Subtract => self.binary_op(std::ops::Sub::sub, &frame)?,
                chunk::OpCode::False => self.stack.push(chunk::Value::Bool(false)),
                chunk::OpCode::Nil => self.stack.push(chunk::Value::Nil),
                chunk::OpCode::True => self.stack.push(chunk::Value::Bool(true)),
                chunk::OpCode::Not => {
                    if let Some(value) = self.stack.pop() {
                        self.stack.push(chunk::Value::Bool(value.is_falsey()));
                    } else {
                        return Err(Self::runtime_error(
                            &frame,
                            "Missing value to perform not operation.",
                        ));
                    }
                }
                chunk::OpCode::Equal => match (self.stack.pop(), self.stack.pop()) {
                    (Some(a), Some(b)) => self.stack.push(chunk::Value::Bool(a == b)),
                    _ => Err(Self::runtime_error(
                        &frame,
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
                        _ => Err(Self::runtime_error(
                            &frame,
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
                        _ => Err(Self::runtime_error(
                            &frame,
                            "Cannot compare less two non-number operands.",
                        ))?,
                    }
                }
                chunk::OpCode::Print => {
                    let item = self.stack.pop();
                    if let Some(value) = item {
                        println!("{value}");
                    } else {
                        Err(Self::runtime_error(
                            &frame,
                            "Could not find a value to print.",
                        ))?;
                    }
                }
                chunk::OpCode::Pop => {
                    self.stack.pop();
                }
                chunk::OpCode::DefineGlobal(index) => {
                    if let chunk::Value::String(string) = chunk.read_constant(*index) {
                        let value = self.stack.pop().ok_or_else(|| {
                            Self::runtime_error(&frame, "Missing value for global variable.")
                        })?;
                        self.globals.insert(Rc::clone(string), value);
                    } else {
                        Err(Self::runtime_error(
                            &frame,
                            "Name of variable was not a string.",
                        ))?;
                    }
                }
                chunk::OpCode::GetGlobal(index) => {
                    if let chunk::Value::String(string) = chunk.read_constant(*index) {
                        let value = self.globals.get(string).ok_or_else(|| {
                            Self::runtime_error(&frame, "Could not read value for global variable.")
                        })?;
                        self.stack.push(value.clone());
                    } else {
                        Err(Self::runtime_error(
                            &frame,
                            "Name of variable was not a string",
                        ))?;
                    }
                }
                chunk::OpCode::SetGlobal(index) => {
                    if let chunk::Value::String(string) = chunk.read_constant(*index) {
                        let next = self.peek(0);
                        let value = match next {
                            Some(value) => value.clone(),
                            None => {
                                Err(Self::runtime_error(&frame, "No value to set for variable."))?
                            }
                        };
                        let previous = self.globals.insert(Rc::clone(string), value);
                        if previous.is_none() {
                            self.globals.remove(string);
                            Err(Self::runtime_error(
                                &frame,
                                "Cannot assign to an undefined variable.",
                            ))?;
                        }
                    } else {
                        Err(Self::runtime_error(
                            &frame,
                            "Name of variable was not a string",
                        ))?;
                    }
                }
                chunk::OpCode::GetLocal(index) => {
                    let slot = *index + frame.stack_idx;
                    let val = self.stack.get(slot).ok_or_else(|| {
                        Self::runtime_error(&frame, "No value found in stack for local variable.")
                    })?;
                    self.stack.push(val.clone());
                }
                chunk::OpCode::SetLocal(index) => {
                    let slot = *index + frame.stack_idx;
                    let val = self.peek(0);
                    let value = match val {
                        Some(value) => value.clone(),
                        None => Err(Self::runtime_error(
                            &frame,
                            "No value found in stack to set local variable to.",
                        ))?,
                    };
                    self.stack[slot] = value;
                }
                chunk::OpCode::JumpIfFalse(jump_size) => {
                    let val = self.peek(0);
                    if let Some(value) = val
                        && value.is_falsey()
                    {
                        frame.ip += jump_size;
                    }
                }
                chunk::OpCode::Jump(jump_size) => {
                    frame.ip += jump_size;
                }
                chunk::OpCode::Loop(jump_size) => {
                    frame.ip -= jump_size;
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

    fn binary_op<T: Fn(f64, f64) -> f64>(&mut self, op: T, frame: &CallFrame) -> Result<(), Error> {
        let a = self.stack.pop();
        let b = self.stack.pop();
        match (b, a) {
            (Some(chunk::Value::Number(i)), Some(chunk::Value::Number(j))) => {
                self.stack.push(chunk::Value::Number(op(i, j)));
                Ok(())
            }
            _ => Err(Self::runtime_error(
                frame,
                "Operands were missing or incorrect to binary operation.",
            )),
        }
    }

    fn concatenate(&mut self, frame: &CallFrame) -> Result<(), Error> {
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
            _ => Err(Self::runtime_error(
                frame,
                "Operand was missing or incorrect in string concatenation.",
            )),
        }
    }

    #[cfg(debug_assertions)]
    fn debug_instruction(&self, frame: &CallFrame, instruction: &chunk::OpCode) {
        println!("{:?}", self.stack);
        frame
            .function
            .chunk
            .disassemble_instruction(frame.ip, instruction);
    }

    fn runtime_error(frame: &CallFrame, message: &'static str) -> Error {
        eprintln!(
            "[Line {}] Error in script: {}",
            frame.function.chunk.get_line(frame.ip),
            message
        );
        Error::RuntimeError(message)
    }
}

#[derive(Debug)]
struct CallFrame {
    ip: usize,
    function: Rc<object::Function>,
    stack_idx: usize,
}

impl CallFrame {
    fn new(function: &Rc<object::Function>, stack_idx: usize) -> Self {
        CallFrame {
            ip: 0,
            function: Rc::clone(function),
            stack_idx,
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn basic_arithmetic() {
        let source = "1 + 2;";
        test_valid(source);
    }

    #[test]
    fn negate_non_number() {
        let source = "-false;";
        test_invalid(source);
    }

    #[test]
    fn compare_non_numbers() {
        let source = "false > true;";
        test_invalid(source);
    }

    #[test]
    fn multiple_comparisons() {
        let source = "1 < 2 < 3;";
        test_invalid(source);
    }

    #[test]
    fn string_concatenate() {
        let source = "\"foo\" + \"bar\";";
        test_valid(source);
    }

    #[test]
    fn addition_wrong_types() {
        let source = "1 + \"foo\";";
        test_invalid(source);
    }

    #[test]
    fn string_equality() {
        let source = "\"foo\" == \"foo\";";
        test_valid(source);
    }

    #[test]
    fn print_statement() {
        let source = "print 1 * 2;";
        test_valid(source);
    }

    #[test]
    fn global_variable() {
        let source = "var foo = 12;
            print foo + 1;";
        test_valid(source);
    }

    #[test]
    fn missing_declaration() {
        let source = "print foo + 1;";
        test_invalid(source);
    }

    #[test]
    fn redefine_global() {
        let source = "var foo = 1;
            var foo = 2;";
        test_valid(source);
    }

    #[test]
    fn assignment() {
        let source = "var foo = 2;
            foo = 3;";
        test_valid(source);
    }

    #[test]
    fn variable_in_wrong_scope() {
        let source = "{ var a = 10; { var b = 20; } print b; }";
        test_invalid(source);
    }

    #[test]
    fn if_statement() {
        let source = "if (true) print 1; else print 0;";
        test_valid(source);
    }

    #[test]
    fn while_statement() {
        let source = "var j = 0; while (j < 4) { print j; j = j + 1; }";
        test_valid(source);
    }

    #[test]
    fn for_statement() {
        let source = "for (var i = 0; i < 10; i = i + 1) print i;";
        test_valid(source);
    }

    #[test]
    fn and_condition() {
        let source = "if (true and false) print 0;";
        test_valid(source);
    }

    #[test]
    fn or_condition() {
        let source = "if (false or true) print 1;";
        test_valid(source);
    }

    fn test_valid(source: &str) {
        let mut vm = VM::new();
        let result = vm.interpret(source);
        assert_eq!(Ok(()), result);
    }

    fn test_invalid(source: &str) {
        let mut vm = VM::new();
        let result = vm.interpret(source);
        assert!(matches!(result, Err(Error::RuntimeError(_))));
    }
}
