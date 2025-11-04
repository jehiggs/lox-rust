use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Error {
    CompileError,
    RuntimeError,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::CompileError => f.write_str("Compile Error."),
            Error::RuntimeError => f.write_str("Runtime Error."),
        }
    }
}
