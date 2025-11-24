use crate::chunk;

use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub struct Function {
    pub arity: usize,
    pub chunk: chunk::Chunk,
    pub name: String,
}

impl Function {
    pub fn new(name: &str) -> Self {
        Function {
            arity: 0,
            chunk: chunk::Chunk::new(),
            name: String::from(name),
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

#[derive(Debug, PartialEq)]
pub enum FunctionType {
    Function,
    Script,
}
