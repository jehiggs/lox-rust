use crate::chunk;
use crate::error::Error;
use crate::scanner;

use std::iter;
use std::mem;

pub struct Compiler<'a> {
    scanner: iter::Peekable<scanner::Scanner<'a>>,
    chunk: chunk::Chunk,
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str) -> Self {
        Compiler {
            scanner: scanner::Scanner::new(source).peekable(),
            chunk: chunk::Chunk::new(),
        }
    }

    pub fn compile(mut self) -> Result<chunk::Chunk, Error> {
        if self.peek().is_none() {
            return Ok(self.chunk);
        }
        self.expression()?;
        if self.scanner.next().is_some() {
            return Err(Error::CompileError);
        }
        self.chunk.write_chunk(chunk::OpCode::Return, 0); // TODO fix the line?
        Ok(self.chunk)
    }

    // Returns the current token to process!
    fn advance(&mut self) -> Option<scanner::Token<'a>> {
        for token in self.scanner.by_ref() {
            match token.token_type {
                scanner::TokenType::Error(message) => Self::report_error(&token, message),
                _ => return Some(token),
            }
        }
        None
    }

    fn peek(&mut self) -> Option<&scanner::Token<'a>> {
        while let Some(token) = self.scanner.peek() {
            match token.token_type {
                scanner::TokenType::Error(message) => {
                    Self::report_error(token, message);
                    self.scanner.next();
                }
                _ => break,
            }
        }
        self.scanner.peek()
    }

    fn consume(
        &mut self,
        token_type: mem::Discriminant<scanner::TokenType<'a>>,
        message: &str,
    ) -> Result<(), Error> {
        if let Some(token) = self.peek() {
            if mem::discriminant(&token.token_type) == token_type {
                self.advance();
                Ok(())
            } else {
                Self::report_error(token, message);
                Err(Error::CompileError)
            }
        } else {
            Err(Error::CompileError)
        }
    }

    fn expression(&mut self) -> Result<(), Error> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn number(&mut self) -> Result<(), Error> {
        let token = self.advance().ok_or(Error::CompileError)?;
        match token.token_type {
            scanner::TokenType::Number(value) => {
                self.chunk.write_constant(value, token.line);
                Ok(())
            }
            _ => {
                Self::report_error(&token, "Non-number token cannot be parsed as number.");
                Err(Error::CompileError)
            }
        }
    }

    fn grouping(&mut self) -> Result<(), Error> {
        self.consume(
            mem::discriminant(&scanner::TokenType::LeftParen),
            "Called grouping with no left parenthesis.",
        )?;
        self.expression()?;
        self.consume(
            mem::discriminant(&scanner::TokenType::RightParen),
            "Expect ')' after a group expression.",
        )?;
        Ok(())
    }

    fn unary(&mut self) -> Result<(), Error> {
        let token = self.advance().ok_or(Error::CompileError)?; // TODO

        self.parse_precedence(Precedence::Unary)?;
        match token.token_type {
            scanner::TokenType::Minus => {
                self.chunk.write_chunk(chunk::OpCode::Negate, token.line);
                Ok(())
            }
            _ => {
                Self::report_error(&token, "Non-unary operation found.");
                Err(Error::CompileError)
            }
        }
    }

    fn binary(&mut self) -> Result<(), Error> {
        let token = self.advance().ok_or(Error::CompileError)?;
        let bin_rule = Self::get_rule(&token.token_type);
        let precedence = bin_rule.precedence.increment();
        self.parse_precedence(precedence)?;

        match token.token_type {
            scanner::TokenType::Minus => {
                self.chunk.write_chunk(chunk::OpCode::Subtract, token.line);
                Ok(())
            }
            scanner::TokenType::Plus => {
                self.chunk.write_chunk(chunk::OpCode::Add, token.line);
                Ok(())
            }
            scanner::TokenType::Star => {
                self.chunk.write_chunk(chunk::OpCode::Multiply, token.line);
                Ok(())
            }
            scanner::TokenType::Slash => {
                self.chunk.write_chunk(chunk::OpCode::Divide, token.line);
                Ok(())
            }
            _ => {
                Self::report_error(&token, "Binary operation expected but not found.");
                Err(Error::CompileError)
            }
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), Error> {
        let Some(current) = self.peek() else {
            return Err(Error::CompileError);
        };
        let parse_rule = Self::get_rule(&current.token_type);

        if let Some(prefix_fn) = parse_rule.prefix_fn {
            prefix_fn(self)?;
        }

        while let Some(rule) = self.peek().map(|token| Self::get_rule(&token.token_type))
            && precedence <= rule.precedence
        {
            if let Some(infix_fn) = rule.infix_fn {
                infix_fn(self)?;
            }
        }
        Ok(())
    }

    fn get_rule(token_type: &scanner::TokenType<'a>) -> ParseTableEntry<'a> {
        match token_type {
            scanner::TokenType::LeftParen => {
                ParseTableEntry::new(Some(Self::grouping), None, Precedence::None)
            }
            scanner::TokenType::RightParen => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::LeftBrace => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::RightBrace => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Comma => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Dot => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Minus => {
                ParseTableEntry::new(Some(Self::unary), Some(Self::binary), Precedence::Term)
            }
            scanner::TokenType::Plus => {
                ParseTableEntry::new(None, Some(Self::binary), Precedence::Term)
            }
            scanner::TokenType::Semicolon => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Slash => {
                ParseTableEntry::new(None, Some(Self::binary), Precedence::Factor)
            }
            scanner::TokenType::Star => {
                ParseTableEntry::new(None, Some(Self::binary), Precedence::Factor)
            }
            scanner::TokenType::Bang => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::BangEqual => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Equal => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::EqualEqual => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Greater => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::GreaterEqual => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Less => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::LessEqual => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Identifier(_) => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::String(_) => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Number(_) => {
                ParseTableEntry::new(Some(Self::number), None, Precedence::None)
            }
            scanner::TokenType::And => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Class => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Else => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::False => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::For => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Fun => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::If => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Nil => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Or => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Print => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Return => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Super => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::This => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::True => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Var => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::While => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Error(_) => ParseTableEntry::new(None, None, Precedence::None),
        }
    }

    fn report_error(token: &scanner::Token<'a>, message: &str) {
        eprint!("[line {}] Error", token.line);
        match &token.token_type {
            scanner::TokenType::Error(message) => eprintln!(" : {}", message),
            other => eprintln!(" at {:?}: {}", other, message),
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // > >= < <=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl Precedence {
    fn increment(&self) -> Precedence {
        match &self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => Precedence::Primary, // This is highest so we can't increment higher anyway.
        }
    }
}

type ParseFn<'a> = fn(&mut Compiler<'a>) -> Result<(), Error>;

struct ParseTableEntry<'a> {
    prefix_fn: Option<ParseFn<'a>>,
    infix_fn: Option<ParseFn<'a>>,
    precedence: Precedence,
}

impl<'a> ParseTableEntry<'a> {
    fn new(
        prefix_fn: Option<ParseFn<'a>>,
        infix_fn: Option<ParseFn<'a>>,
        precedence: Precedence,
    ) -> Self {
        ParseTableEntry {
            prefix_fn,
            infix_fn,
            precedence,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chunk::*;

    #[test]
    fn basic_parse() {
        let source = "1 - 2";
        let compiler = Compiler::new(source);
        let chunk = compiler.compile().unwrap();
        check_chunk(
            &chunk,
            vec![
                OpCode::Constant(0),
                OpCode::Constant(1),
                OpCode::Subtract,
                OpCode::Return,
            ],
            vec![1.0, 2.0],
        );
    }

    #[test]
    fn precedence_left() {
        let source = "1 * 2 + 3";
        let compiler = Compiler::new(source);
        let chunk = compiler.compile().unwrap();
        check_chunk(
            &chunk,
            vec![
                OpCode::Constant(0),
                OpCode::Constant(1),
                OpCode::Multiply,
                OpCode::Constant(2),
                OpCode::Add,
                OpCode::Return,
            ],
            vec![1.0, 2.0, 3.0],
        )
    }

    #[test]
    fn precedence_right() {
        let source = "1 + 2 / 3";
        let compiler = Compiler::new(source);
        let chunk = compiler.compile().unwrap();
        check_chunk(
            &chunk,
            vec![
                OpCode::Constant(0),
                OpCode::Constant(1),
                OpCode::Constant(2),
                OpCode::Divide,
                OpCode::Add,
                OpCode::Return,
            ],
            vec![1.0, 2.0, 3.0],
        )
    }

    #[test]
    fn unary() {
        let source = "-1";
        let compiler = Compiler::new(source);
        let chunk = compiler.compile().unwrap();
        check_chunk(
            &chunk,
            vec![OpCode::Constant(0), OpCode::Negate, OpCode::Return],
            vec![1.0],
        )
    }

    #[test]
    fn prefix_and_infix() {
        let source = "-1 + 2";
        let compiler = Compiler::new(source);
        let chunk = compiler.compile().unwrap();
        check_chunk(
            &chunk,
            vec![
                OpCode::Constant(0),
                OpCode::Negate,
                OpCode::Constant(1),
                OpCode::Add,
                OpCode::Return,
            ],
            vec![1.0, 2.0],
        )
    }

    #[test]
    fn grouping() {
        let source = "(1 + 2) * 3";
        let compiler = Compiler::new(source);
        let chunk = compiler.compile().unwrap();
        check_chunk(
            &chunk,
            vec![
                OpCode::Constant(0),
                OpCode::Constant(1),
                OpCode::Add,
                OpCode::Constant(2),
                OpCode::Multiply,
                OpCode::Return,
            ],
            vec![1.0, 2.0, 3.0],
        )
    }

    #[test]
    fn nested_grouping() {
        let source = "1 + (2 * (3 + 4))";
        let compiler = Compiler::new(source);
        let chunk = compiler.compile().unwrap();
        check_chunk(
            &chunk,
            vec![
                OpCode::Constant(0),
                OpCode::Constant(1),
                OpCode::Constant(2),
                OpCode::Constant(3),
                OpCode::Add,
                OpCode::Multiply,
                OpCode::Add,
                OpCode::Return,
            ],
            vec![1.0, 2.0, 3.0, 4.0],
        )
    }

    #[test]
    fn empty() {
        let source = "";
        let compiler = Compiler::new(source);
        let chunk = compiler.compile().unwrap();
        check_chunk(&chunk, vec![], vec![])
    }

    #[test]
    fn missing_arg() {
        let source = "1 +";
        let compiler = Compiler::new(source);
        assert_eq!(Err(Error::CompileError), compiler.compile());
    }

    #[test]
    fn dangling_unary() {
        let source = "-";
        let compiler = Compiler::new(source);
        assert_eq!(Err(Error::CompileError), compiler.compile());
    }

    #[test]
    fn missing_parens() {
        let source = "(1 + 2";
        let compiler = Compiler::new(source);
        assert_eq!(Err(Error::CompileError), compiler.compile());
    }

    #[test]
    fn spare_closing_parens() {
        let source = "1 + 2) - 4";
        let compiler = Compiler::new(source);
        assert_eq!(Err(Error::CompileError), compiler.compile());
    }

    fn check_chunk(chunk: &Chunk, opcodes: Vec<OpCode>, constants: Vec<Value>) {
        for (index, opcode) in opcodes.iter().enumerate() {
            assert_eq!(opcode, chunk.read_code(index));
            if let OpCode::Constant(const_index) = opcode {
                assert_eq!(
                    constants[usize::from(*const_index)],
                    chunk.read_constant((*const_index).into())
                )
            }
        }
        assert_eq!(OpCode::Return, *chunk.read_code(opcodes.len()));
    }
}
