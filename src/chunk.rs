use std::fmt;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Bool(bool),
    Number(f64),
    Nil,
    String(Rc<str>),
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Bool(value) => !value,
            Value::Number(_) | Value::String(_) => false,
            Value::Nil => true,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(flag) => write!(f, "{flag}"),
            Value::Number(number) => write!(f, "{number}"),
            Value::Nil => write!(f, "nil"),
            Value::String(string) => write!(f, "{string}"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum OpCode {
    Add,
    Constant(u8),
    ConstantLong(usize),
    DefineGlobal(usize),
    Divide,
    Equal,
    False,
    GetGlobal(usize),
    GetLocal(usize),
    Greater,
    Jump(usize),
    JumpIfFalse(usize),
    Less,
    Loop(usize),
    Multiply,
    Negate,
    Nil,
    Not,
    Pop,
    Print,
    Return,
    SetGlobal(usize),
    SetLocal(usize),
    Subtract,
    True,
}

#[derive(Debug, PartialEq)]
pub struct Chunk {
    code: Vec<OpCode>,
    constants: Vec<Value>,
    lines: Vec<(usize, usize)>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn write_chunk(&mut self, code: OpCode, line: usize) {
        self.code.push(code);
        let last = self.lines.last_mut();
        if let Some((count, last_line)) = last
            && *last_line == line
        {
            *count += 1;
        } else {
            self.lines.push((1, line));
        }
    }

    pub fn write_constant_instruction(&mut self, constant: Value, line: usize) {
        let index = self.write_constant(constant);
        let byte =
            u8::try_from(index).map_or_else(|_| OpCode::ConstantLong(index), OpCode::Constant);
        self.write_chunk(byte, line);
    }

    pub fn write_constant(&mut self, constant: Value) -> usize {
        self.constants.push(constant);
        self.constants.len() - 1
    }

    pub fn read_code(&self, index: usize) -> &OpCode {
        self.code.get(index).unwrap_or(&OpCode::Return)
    }

    pub fn read_constant(&self, index: usize) -> &Value {
        &self.constants[index]
    }

    pub fn code_len(&self) -> usize {
        self.code.len()
    }

    pub fn patch_code(&mut self, index: usize) -> &mut OpCode {
        &mut self.code[index]
    }

    #[cfg(debug_assertions)]
    pub fn disassemble_chunk(&self, name: &str) {
        println!("== START CHUNK {name} ==");
        for (i, code) in self.code.iter().enumerate() {
            self.disassemble_instruction(i, code);
        }
        println!("== END CHUNK ==\n\n");
    }

    #[cfg(debug_assertions)]
    pub fn disassemble_instruction(&self, index: usize, code: &OpCode) {
        print!("{index:04} ");
        let line = self.get_line(index);
        if index > 0 && line == self.get_line(index - 1) {
            print!("{:>4} ", "|");
        } else {
            print!("{line:>4} ");
        }
        self.print_code(code, index);
    }

    pub fn get_line(&self, index: usize) -> usize {
        let mut current = index;
        for (count, line) in &self.lines {
            if current < *count {
                return *line;
            }
            current -= count;
        }
        self.lines.last().map_or(0, |(_, line)| *line)
    }

    #[cfg(debug_assertions)]
    fn print_code(&self, code: &OpCode, index: usize) {
        match code {
            OpCode::Constant(const_index) => {
                self.print_constant("Constant", usize::from(*const_index));
            }
            OpCode::ConstantLong(const_index) => self.print_constant("ConstantLong", *const_index),
            OpCode::Return => println!("Return"),
            OpCode::Negate => println!("Negate"),
            OpCode::Add => println!("Add"),
            OpCode::Divide => println!("Divide"),
            OpCode::Multiply => println!("Multiply"),
            OpCode::Subtract => println!("Subtract"),
            OpCode::False => println!("False"),
            OpCode::Nil => println!("Nil"),
            OpCode::True => println!("True"),
            OpCode::Not => println!("Not"),
            OpCode::Equal => println!("Equal"),
            OpCode::Greater => println!("Greater"),
            OpCode::Less => println!("Less"),
            OpCode::Print => println!("Print"),
            OpCode::Pop => println!("Pop"),
            OpCode::DefineGlobal(const_index) => self.print_constant("DefineGlobal", *const_index),
            OpCode::GetGlobal(const_index) => self.print_constant("GetGlobal", *const_index),
            OpCode::SetGlobal(const_index) => self.print_constant("SetGlobal", *const_index),
            OpCode::GetLocal(const_index) => Self::print_local("GetLocal", *const_index),
            OpCode::SetLocal(const_index) => Self::print_local("SetLocal", *const_index),
            OpCode::JumpIfFalse(jump_size) => {
                Self::print_jump("JumpIfFalse", *jump_size, index, true);
            }
            OpCode::Jump(jump_size) => {
                Self::print_jump("Jump", *jump_size, index, true);
            }
            OpCode::Loop(jump_size) => Self::print_jump("Loop", *jump_size, index, false),
        }
    }

    #[cfg(debug_assertions)]
    fn print_constant(&self, name: &str, index: usize) {
        let constant = self.constants.get(index);
        if let Some(value) = constant {
            println!("{name:<16} {index:04} {value}");
        } else {
            println!("{name:<16} {index:04} !!No Value!!");
        }
    }

    #[cfg(debug_assertions)]
    fn print_local(name: &str, index: usize) {
        println!("{name:<16} {index:04}");
    }

    #[cfg(debug_assertions)]
    fn print_jump(name: &str, jump: usize, index: usize, add: bool) {
        let target = if add {
            jump + index + 1
        } else {
            index + 1 - jump
        };
        println!("{name:<16} {index:04}->{target:04}");
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn write_byte() {
        let mut chunk = Chunk::new();
        chunk.write_chunk(OpCode::Return, 0);
        let expected = Chunk {
            code: vec![OpCode::Return],
            constants: vec![],
            lines: vec![(1, 0)],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn add_constant() {
        let mut chunk = Chunk::new();
        chunk.write_constant_instruction(Value::Number(45.0), 0);
        let expected = Chunk {
            code: vec![OpCode::Constant(0)],
            constants: vec![Value::Number(45.0)],
            lines: vec![(1, 0)],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn many_constants() {
        let mut chunk = Chunk::new();
        for i in 0..300 {
            let number = Value::Number(i.into());
            chunk.write_constant_instruction(number, 0);
        }
        chunk.write_constant_instruction(Value::Number(123.into()), 0);
        assert_eq!(OpCode::ConstantLong(300), *chunk.code.last().unwrap());
    }

    #[test]
    fn write_single_constant() {
        let mut chunk = Chunk::new();
        chunk.write_constant_instruction(Value::Number(1.2), 0);
        let expected = Chunk {
            code: vec![OpCode::Constant(0)],
            constants: vec![Value::Number(1.2)],
            lines: vec![(1, 0)],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn falsey_value() {
        assert!(Value::Bool(false).is_falsey());
        assert!(Value::Nil.is_falsey());
        assert!(!Value::Bool(true).is_falsey());
        assert!(!Value::Number(0.0).is_falsey());
        assert!(!Value::Number(1.0).is_falsey());
    }
}
