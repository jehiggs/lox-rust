use crate::{chunk, error::Error};

use std::fmt::Display;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub struct Function {
    pub arity: usize,
    pub chunk: chunk::Chunk,
    pub name: String,
    pub upvalues: Vec<UpValue>,
}

impl Function {
    pub fn new(name: &str) -> Self {
        Function {
            arity: 0,
            chunk: chunk::Chunk::new(),
            name: String::from(name),
            upvalues: Vec::new(),
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
    pub upvalues: Vec<RuntimeUpvalue>,
}

impl Closure {
    pub fn new(function: &Rc<Function>) -> Self {
        Closure {
            function: Rc::clone(function),
            upvalues: Vec::new(),
        }
    }
}

impl Display for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.function.fmt(f)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct UpValue {
    pub index: usize,
    pub is_local: bool,
}

impl Display for UpValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let locality = if self.is_local { "local" } else { "enclosed" };
        let index = self.index;
        write!(f, "[index {index}, {locality}]")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeUpvalue {
    // Before hoisting into the heap
    Open(usize),
    // Once closed by the function scope ending.
    Closed(Rc<chunk::Value>),
}
