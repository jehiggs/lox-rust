type Value = f64;

#[derive(Debug, PartialEq)]
pub enum OpCode {
    OpConstant(usize),
    OpReturn,
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

    pub fn write_chunk(&mut self, byte: OpCode, line: usize) {
        self.code.push(byte);
        let last = self.lines.last_mut();
        if let Some((count, last_line)) = last
            && *last_line == line
        {
            *count += 1;
        } else {
            self.lines.push((1, line));
        }
    }

    pub fn add_constant(&mut self, constant: Value) -> usize {
        self.constants.push(constant);
        self.constants.len() - 1
    }

    #[cfg(debug_assertions)]
    pub fn disassemble_chunk(&self, name: &str) {
        println!("== {} ==", name);
        for (i, code) in self.code.iter().enumerate() {
            print!("{:04} ", i);
            let line = self.get_line(i);
            if i > 0 && line == self.get_line(i - 1) {
                print!("{:>4} ", "|");
            } else {
                print!("{:04} ", line);
            }
            self.print_code(code);
        }
    }

    #[cfg(debug_assertions)]
    fn get_line(&self, index: usize) -> usize {
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
            OpCode::OpConstant(const_index) => {
                let constant = self.constants.get(*const_index);
                if let Some(value) = constant {
                    println!("{:<16} {:04} {}", "OpConstant", const_index, value);
                } else {
                    println!("{:<16} {:04} !!No Value!!", "OpConstant", const_index);
                }
            }
            OpCode::OpReturn => println!("OpReturn"),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_write_byte() {
        let mut chunk = Chunk::new();
        chunk.write_chunk(OpCode::OpReturn, 0);
        let expected = Chunk {
            code: vec![OpCode::OpReturn],
            constants: vec![],
            lines: vec![(1, 0)],
        };
        assert_eq!(expected, chunk)
    }

    #[test]
    fn test_add_constant() {
        let mut chunk = Chunk::new();
        let index = chunk.add_constant(45.0);
        let expected = Chunk {
            code: vec![],
            constants: vec![45.0],
            lines: vec![],
        };
        assert_eq!(expected, chunk);
        assert_eq!(0, index);
    }
}
