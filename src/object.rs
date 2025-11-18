use crate::chunk;

use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub struct Function {
    arity: usize,
    pub chunk: chunk::Chunk,
    pub name: String,
}

impl Function {
    pub fn new() -> Self {
        Function {
            arity: 0,
            chunk: chunk::Chunk::new(),
            name: String::new(),
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = &self.name;
        if name.is_empty() {
            write!(f, "<script>")
        } else {
            write!(f, "<fn {name}>")
        }
    }
}

pub enum FunctionType {
    Function,
    Script,
}
