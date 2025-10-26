#[derive(Debug, PartialEq)]
pub enum OpCode {
    OpReturn,
}

impl std::fmt::Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::OpReturn => write!(f, "OpReturn"),
        }
    }
}

impl OpCode {
    #[cfg(debug_assertions)]
    fn print_instruction(&self) {
        println!("{}", self);
    }
}

#[derive(Debug, PartialEq)]
pub struct Chunk {
    code: Vec<OpCode>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk { code: Vec::new() }
    }

    pub fn write_byte(&mut self, byte: OpCode) {
        self.code.push(byte)
    }

    #[cfg(debug_assertions)]
    pub fn disassemble_chunk(&self, name: &str) {
        println!("== {} ==", name);
        for (i, code) in self.code.iter().enumerate() {
            print!("{:04} ", i);
            code.print_instruction();
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_write_byte() {
        let mut chunk = Chunk::new();
        chunk.write_byte(OpCode::OpReturn);
        let expected = Chunk {
            code: vec![OpCode::OpReturn],
        };
        assert_eq!(expected, chunk)
    }
}
