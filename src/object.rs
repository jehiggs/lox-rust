use crate::{chunk, error::Error};

use std::fmt::Display;
use std::rc::Rc;

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

pub type NativeFunction = fn(usize, &[chunk::Value]) -> Result<chunk::Value, Error>;

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub function: Rc<Function>,
}

impl Closure {
    pub fn new(function: &Rc<Function>) -> Self {
        Closure {
            function: Rc::clone(function),
        }
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.function.fmt(f)
    }
}
