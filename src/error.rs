use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Error {
    CompileError(&'static str),
    RuntimeError(&'static str),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::CompileError(msg) => write!(f, "Compile Error: {}", msg),
            Error::RuntimeError(msg) => write!(f, "Runtime Error: {}", msg),
        }
    }
}
