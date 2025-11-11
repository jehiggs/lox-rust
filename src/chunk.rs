use std::fmt;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Value {
    Bool(bool),
    Number(f64),
    Nil,
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Bool(value) => !value,
            Value::Number(_) => false,
            Value::Nil => true,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(flag) => write!(f, "{}", flag),
            Value::Number(number) => write!(f, "{}", number),
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum OpCode {
    Add,
    Constant(u8),
    ConstantLong(usize),
    Divide,
    Equal,
    False,
    Greater,
    Less,
    Multiply,
    Negate,
    Nil,
    Not,
    Return,
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

    pub fn write_constant(&mut self, constant: Value, line: usize) {
        self.constants.push(constant);
        let index = self.constants.len() - 1;
        let byte =
            u8::try_from(index).map_or_else(|_| OpCode::ConstantLong(index), OpCode::Constant);
        self.write_chunk(byte, line);
    }

    pub fn read_code(&self, index: usize) -> &OpCode {
        self.code.get(index).unwrap_or(&OpCode::Return)
    }

    pub fn read_constant(&self, index: usize) -> &Value {
        &self.constants[index]
    }

    #[cfg(debug_assertions)]
    pub fn disassemble_chunk(&self, name: &str) {
        println!("== {} ==", name);
        for (i, code) in self.code.iter().enumerate() {
            self.disassemble_instruction(i, code);
        }
    }

    #[cfg(debug_assertions)]
    pub fn disassemble_instruction(&self, index: usize, code: &OpCode) {
        print!("{:04} ", index);
        let line = self.get_line(index);
        if index > 0 && line == self.get_line(index - 1) {
            print!("{:>4} ", "|");
        } else {
            print!("{:>4} ", line);
        }
        self.print_code(code);
    }

    pub fn get_line(&self, index: usize) -> usize {
        let mut current = index;
        for (count, line) in &self.lines {
            if current < *count {
                return *line;
            } else {
                current -= count;
            }
        }
        0
    }

    #[cfg(debug_assertions)]
    fn print_code(&self, code: &OpCode) {
        match code {
            OpCode::Constant(const_index) => {
                let constant = self.constants.get(usize::from(*const_index));
                if let Some(value) = constant {
                    println!("{:<16} {:04} {}", "Constant", const_index, value);
                } else {
                    println!("{:<16} {:04} !!No Value!!", "Constant", const_index);
                }
            }
            OpCode::ConstantLong(const_index) => {
                let constant = self.constants.get(*const_index);
                if let Some(value) = constant {
                    println!("{:<16} {:04} {}", "ConstantLong", const_index, value);
                } else {
                    println!("{:<16} {:04} !!No Value!!", "ConstantLong", const_index);
                }
            }
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
        }
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
        assert_eq!(expected, chunk)
    }

    #[test]
    fn add_constant() {
        let mut chunk = Chunk::new();
        chunk.write_constant(Value::Number(45.0), 0);
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
            chunk.write_constant(number, 0);
        }
        chunk.write_constant(Value::Number(123.into()), 0);
        assert_eq!(OpCode::ConstantLong(300), *chunk.code.last().unwrap());
    }

    #[test]
    fn write_single_constant() {
        let mut chunk = Chunk::new();
        chunk.write_constant(Value::Number(1.2), 0);
        let expected = Chunk {
            code: vec![OpCode::Constant(0)],
            constants: vec![Value::Number(1.2)],
            lines: vec![(1, 0)],
        };
        assert_eq!(expected, chunk);
    }

    #[test]
    fn falsey_value() {
        assert_eq!(true, Value::Bool(false).is_falsey());
        assert_eq!(true, Value::Nil.is_falsey());
        assert_eq!(false, Value::Bool(true).is_falsey());
        assert_eq!(false, Value::Number(0.0).is_falsey());
        assert_eq!(false, Value::Number(1.0).is_falsey());
    }
}
