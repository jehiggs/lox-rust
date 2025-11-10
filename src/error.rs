use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Error {
    CompileError(&'static str),
    RuntimeError(&'static str),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::CompileError(msg) => f.write_str("Compile Error: ").and(f.write_str(msg)),
            Error::RuntimeError(msg) => f.write_str("Runtime Error: ").and(f.write_str(msg)),
        }
    }
}
