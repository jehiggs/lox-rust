use crate::chunk;
use crate::error::Error;
use crate::object;
use crate::scanner;

use std::iter;
use std::mem;
use std::rc::Rc;

const LOCAL_SIZE: usize = 255;

pub struct FuncScope<'a> {
    function: object::Function,
    function_type: object::FunctionType,
    locals: [Option<Local<'a>>; LOCAL_SIZE],
    local_count: usize,
    scope_depth: usize,
}

impl<'a> FuncScope<'a> {
    fn new(name: &str, func_type: object::FunctionType) -> Self {
        FuncScope {
            function: object::Function::new(name),
            function_type: func_type,
            locals: [const { None }; LOCAL_SIZE],
            local_count: 0,
            scope_depth: 0,
        }
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self, line: usize) {
        self.scope_depth -= 1;
        for item in self.locals[0..self.local_count].iter_mut().rev() {
            match item {
                Some(Local::Initialized(depth, _)) if *depth > self.scope_depth => {
                    *item = None;
                    self.local_count -= 1;
                    self.function.chunk.write_chunk(chunk::OpCode::Pop, line);
                }
                Some(_) | None => break,
            }
        }
    }

    fn declare_variable(&mut self, token: scanner::Token<'a>) -> Result<(), Error> {
        if self.scope_depth == 0 {
            return Ok(());
        }
        if self.local_count >= LOCAL_SIZE {
            return Err(Compiler::report_error(
                &token,
                "Could not declare variable due to insufficient size.",
            ));
        }
        if let scanner::TokenType::Identifier(_) = token.token_type {
            for item in self.locals[0..self.local_count].iter().rev() {
                match item {
                    Some(Local::Initialized(depth, _)) if *depth < self.scope_depth => break,
                    Some(local) => {
                        if *local.name() == token {
                            return Err(Compiler::report_error(
                                &token,
                                "Attempted to redeclare a variable in the same scope.",
                            ));
                        }
                    }
                    None => break,
                }
            }
            let local = Local::new(token);
            self.locals[self.local_count] = Some(local);
            self.local_count += 1;
            Ok(())
        } else {
            Err(Compiler::report_error(
                &token,
                "Received non-identifier for declaring a variable.",
            ))
        }
    }

    fn resolve_local(&mut self, token: &scanner::Token<'a>) -> Result<Option<usize>, Error> {
        let token_name = Compiler::extract_name(token)?;
        for (index, item) in self.locals[0..self.local_count].iter().enumerate().rev() {
            if let Some(local) = item {
                let local_name = Compiler::extract_name(local.name())?;
                if token_name == local_name {
                    match local {
                        Local::Initialized(_, _) => return Ok(Some(index)),
                        Local::Uninitialized(_) => {
                            return Err(Compiler::error(
                                "Can't read variable in its own initializer.",
                            ));
                        }
                    }
                }
            }
        }
        Ok(None)
    }

    fn mark_initialized(&mut self) {
        if self.scope_depth > 0
            && let Some(local) = self.locals[self.local_count - 1].take()
        {
            self.locals[self.local_count - 1] = Some(local.initialize(self.scope_depth));
        }
    }
}

pub struct Compiler<'a> {
    scanner: iter::Peekable<scanner::Scanner<'a>>,
    stack: Vec<FuncScope<'a>>,
}

impl<'a> Compiler<'a> {
    pub fn new(source: &'a str) -> Self {
        let scope = FuncScope::new("", object::FunctionType::Script);
        let stack = Vec::from([scope]);
        Compiler {
            scanner: scanner::Scanner::new(source).peekable(),
            stack,
        }
    }

    pub fn compile(mut self) -> Result<object::Function, Error> {
        if self.peek().is_none() {
            let function = self
                .stack
                .pop()
                .ok_or_else(|| Self::error("Did not get a default empty function."))
                .map(|scope| scope.function)?;
            return Ok(function);
        }
        let mut result = Ok(());
        while self.peek().is_some() {
            result = result.and(self.declaration());
        }
        self.end_function()?;
        if self.scanner.next().is_some() {
            return Err(Self::error("Compiler failed to parse all code in source."));
        }
        let function = self
            .stack
            .pop()
            .ok_or_else(|| Self::error("Should have a top-level function compiled"))
            .map(|scope| scope.function)?;
        result.map(|()| function)
    }

    // Returns the current token to process!
    fn advance(&mut self) -> Option<scanner::Token<'a>> {
        for token in self.scanner.by_ref() {
            match token.token_type {
                scanner::TokenType::Error(message) => {
                    // We don't want to return an error here - keep going instead.
                    _ = Self::report_error(&token, message);
                }
                _ => return Some(token),
            }
        }
        None
    }

    fn peek(&mut self) -> Option<&scanner::Token<'a>> {
        while let Some(token) = self.scanner.peek() {
            match token.token_type {
                scanner::TokenType::Error(message) => {
                    // We don't want to report an error here - keep going instead.
                    _ = Self::report_error(token, message);
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
        message: &'static str,
    ) -> Result<usize, Error> {
        if let Some(token) = self.peek() {
            if mem::discriminant(&token.token_type) == token_type {
                let line = token.line;
                self.advance();
                Ok(line)
            } else {
                Err(Self::report_error(token, message))
            }
        } else {
            Err(Self::error(message))
        }
    }

    fn match_token(&mut self, token_type: mem::Discriminant<scanner::TokenType<'a>>) -> bool {
        if let Some(token) = self.peek() {
            if mem::discriminant(&token.token_type) == token_type {
                self.advance();
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    fn check(&mut self, token_type: mem::Discriminant<scanner::TokenType<'a>>) -> bool {
        if let Some(token) = self.peek()
            && mem::discriminant(&token.token_type) == token_type
        {
            true
        } else {
            false
        }
    }

    fn current_chunk(&mut self) -> Result<&mut chunk::Chunk, Error> {
        self.current_frame().map(|scope| &mut scope.function.chunk)
    }

    fn current_frame(&mut self) -> Result<&mut FuncScope<'a>, Error> {
        self.stack
            .last_mut()
            .ok_or_else(|| Self::error("Missing function to compile into."))
    }

    fn synchronize(&mut self) {
        while let Some(token) = self.peek() {
            match token.token_type {
                scanner::TokenType::Class
                | scanner::TokenType::Fun
                | scanner::TokenType::Var
                | scanner::TokenType::For
                | scanner::TokenType::If
                | scanner::TokenType::While
                | scanner::TokenType::Print
                | scanner::TokenType::Return => {
                    return;
                }
                scanner::TokenType::Semicolon => {
                    self.scanner.next();
                    return;
                }
                _ => {
                    self.scanner.next();
                }
            }
        }
    }

    fn declaration(&mut self) -> Result<(), Error> {
        let result = match self.peek().map(|token| &token.token_type) {
            Some(scanner::TokenType::Var) => self.var_declaration(),
            Some(scanner::TokenType::Fun) => self.fun_declaration(),
            _ => self.statement(),
        };
        result.inspect_err(|_| self.synchronize())
    }

    fn var_declaration(&mut self) -> Result<(), Error> {
        let line = self
            .advance()
            .map(|token| token.line)
            .ok_or_else(|| Self::error("Expected a token in a variable declaration."))?;
        let global = self.parse_variable("Expected a variable name.")?;

        if let Some(scanner::TokenType::Equal) = self.peek().map(|token| &token.token_type) {
            _ = self.advance();
            self.expression()?;
        } else {
            self.current_chunk()?.write_chunk(chunk::OpCode::Nil, line);
        }

        self.consume(
            mem::discriminant(&scanner::TokenType::Semicolon),
            "Expect ';' after a variable declaration.",
        )?;
        self.define_variable(global, line)?;
        Ok(())
    }

    fn fun_declaration(&mut self) -> Result<(), Error> {
        let line = self
            .advance()
            .map(|token| token.line)
            .ok_or_else(|| Self::error("Expected a token in a function declaration."))?;
        let name = if let Some(token) = self.peek() {
            if let scanner::TokenType::Identifier(ident) = token.token_type {
                ident
            } else {
                return Err(Self::report_error(
                    token,
                    "Did not get a function name identifier.",
                ));
            }
        } else {
            return Err(Self::error("Missing token when parsing a function."));
        };
        let global = self.parse_variable("Failed to parse a function name.")?;
        self.mark_initialized()?;
        self.function(object::FunctionType::Function, name)?;
        self.define_variable(global, line)?;
        Ok(())
    }

    fn function(&mut self, function_type: object::FunctionType, name: &str) -> Result<(), Error> {
        let new_scope = FuncScope::new(name, function_type);
        self.stack.push(new_scope);
        self.begin_scope()?;
        let line = self.consume(
            mem::discriminant(&scanner::TokenType::LeftParen),
            "Expect '(' after function name.",
        )?;
        while !self.check(mem::discriminant(&scanner::TokenType::RightParen)) {
            self.current_frame()?.function.arity += 1;
            if self.current_frame()?.function.arity > 255 {
                return Err(Self::error("Over 255 function parameters provided."));
            }
            let param_line = self.peek().map_or(line, |token| token.line);
            let constant = self.parse_variable("Expected a parameter name.")?;
            self.define_variable(constant, param_line)?;
            if !self.check(mem::discriminant(&scanner::TokenType::RightParen)) {
                self.consume(
                    mem::discriminant(&scanner::TokenType::Comma),
                    "Expect a ',' between parameters.",
                )?;
            }
        }
        self.consume(
            mem::discriminant(&scanner::TokenType::RightParen),
            "Expect ')' after function parameters.",
        )?;
        self.block()?;

        self.end_function()?;
        let end_line = self.peek().map_or(0, |token| token.line);
        self.end_scope(end_line)?;
        let completed_scope = self.stack.pop().expect("Item must be on stack.");
        let index = self
            .current_chunk()?
            .write_constant(chunk::Value::Function(Rc::from(completed_scope.function)));
        self.current_chunk()?
            .write_chunk(chunk::OpCode::Closure(index), line);
        Ok(())
    }

    fn end_function(&mut self) -> Result<(), Error> {
        let line = self.peek().map_or(0, |token| token.line);
        self.current_chunk()?.write_chunk(chunk::OpCode::Nil, line);
        self.current_chunk()?
            .write_chunk(chunk::OpCode::Return, line);
        Ok(())
    }

    fn statement(&mut self) -> Result<(), Error> {
        match self.peek().map(|token| &token.token_type) {
            Some(scanner::TokenType::Print) => self.print_statement(),
            Some(scanner::TokenType::LeftBrace) => {
                self.begin_scope()?;
                let result = self.block();
                let line = self.peek().map_or(0, |token| token.line);
                self.end_scope(line)?;
                result
            }
            Some(scanner::TokenType::If) => self.if_statement(),
            Some(scanner::TokenType::While) => self.while_statement(),
            Some(scanner::TokenType::For) => self.for_statement(),
            Some(scanner::TokenType::Return) => self.return_statement(),
            _ => self.expression_statement(),
        }
    }

    fn begin_scope(&mut self) -> Result<(), Error> {
        self.current_frame()?.begin_scope();
        Ok(())
    }

    fn end_scope(&mut self, line: usize) -> Result<(), Error> {
        self.current_frame()?.end_scope(line);
        Ok(())
    }

    fn print_statement(&mut self) -> Result<(), Error> {
        let token = self
            .advance()
            .filter(|item| item.token_type == scanner::TokenType::Print)
            .ok_or_else(|| Self::error("Missing required print token."))?;
        self.expression()?;
        self.consume(
            mem::discriminant(&scanner::TokenType::Semicolon),
            "Expect ; after value.",
        )?;
        self.current_chunk()?
            .write_chunk(chunk::OpCode::Print, token.line);
        Ok(())
    }

    fn expression_statement(&mut self) -> Result<(), Error> {
        let line = self
            .peek()
            .map(|token| token.line)
            .ok_or_else(|| Self::error("Expected a token in an expression statement."))?;
        self.expression()?;
        self.consume(
            mem::discriminant(&scanner::TokenType::Semicolon),
            "Expect ; after expression statement.",
        )?;
        self.current_chunk()?.write_chunk(chunk::OpCode::Pop, line);
        Ok(())
    }

    fn if_statement(&mut self) -> Result<(), Error> {
        let if_line = self.consume(
            mem::discriminant(&scanner::TokenType::If),
            "Require an if token to parse an if statement.",
        )?;
        self.consume(
            mem::discriminant(&scanner::TokenType::LeftParen),
            "Expect '(' after if.",
        )?;
        self.expression()?;
        self.consume(
            mem::discriminant(&scanner::TokenType::RightParen),
            "Expect ')' after if condition.",
        )?;

        let then_jump_offset = self.emit_jump(chunk::OpCode::JumpIfFalse(0), if_line)?;
        self.current_chunk()?
            .write_chunk(chunk::OpCode::Pop, if_line);
        self.statement()?;
        let next_line = self.peek().map_or(0, |token| token.line);
        let else_jump_offset = self.emit_jump(chunk::OpCode::Jump(0), next_line)?;
        self.patch_jump(then_jump_offset)?;
        self.current_chunk()?
            .write_chunk(chunk::OpCode::Pop, next_line);
        if self.match_token(mem::discriminant(&scanner::TokenType::Else)) {
            self.statement()?;
        }
        self.patch_jump(else_jump_offset)
    }

    fn while_statement(&mut self) -> Result<(), Error> {
        let loop_start = self.current_chunk()?.code_len();
        let while_line = self.consume(
            mem::discriminant(&scanner::TokenType::While),
            "Require a while token to parse a while statement.",
        )?;
        self.consume(
            mem::discriminant(&scanner::TokenType::LeftParen),
            "Expect '(' after while.",
        )?;
        self.expression()?;
        self.consume(
            mem::discriminant(&scanner::TokenType::RightParen),
            "Expect ')' after while condition.",
        )?;

        let loop_exit = self.emit_jump(chunk::OpCode::JumpIfFalse(0), while_line)?;
        self.current_chunk()?
            .write_chunk(chunk::OpCode::Pop, while_line);
        self.statement()?;
        let next_line = self.peek().map_or(0, |token| token.line);
        self.emit_loop(loop_start, next_line)?;
        self.patch_jump(loop_exit)?;
        self.current_chunk()?
            .write_chunk(chunk::OpCode::Pop, while_line);
        Ok(())
    }

    fn for_statement(&mut self) -> Result<(), Error> {
        self.begin_scope()?;
        let for_line = self.consume(
            mem::discriminant(&scanner::TokenType::For),
            "Require a for token to parse a for statement.",
        )?;
        self.consume(
            mem::discriminant(&scanner::TokenType::LeftParen),
            "Require '(' after a for statement.",
        )?;
        // Initializer.
        match self.peek().map(|token| &token.token_type) {
            Some(scanner::TokenType::Var) => self.var_declaration()?,
            Some(scanner::TokenType::Semicolon) => {
                self.advance();
            }
            Some(_) => self.expression_statement()?,
            None => {
                return Err(Self::error("Required a token in a for loop."));
            }
        }
        let mut loop_start = self.current_chunk()?.code_len();

        // Guard
        let condition_jump = match self.peek().map(|token| &token.token_type) {
            Some(scanner::TokenType::Semicolon) => None,
            Some(_) => {
                self.expression()?;
                self.consume(
                    mem::discriminant(&scanner::TokenType::Semicolon),
                    "Require a ';' after loop condition.",
                )?;
                let exit_jump = self.emit_jump(chunk::OpCode::JumpIfFalse(0), for_line)?;
                self.current_chunk()?
                    .write_chunk(chunk::OpCode::Pop, for_line);
                Some(exit_jump)
            }
            None => {
                return Err(Self::error(
                    "Required a token while parsing the guard in a 'for' loop.",
                ));
            }
        };

        // Increment
        match self.peek().map(|token| &token.token_type) {
            Some(scanner::TokenType::RightParen) => {}
            Some(_) => {
                let body_jump = self.emit_jump(chunk::OpCode::Jump(0), for_line)?;
                let increment_start = self.current_chunk()?.code_len();
                self.expression()?;
                self.current_chunk()?
                    .write_chunk(chunk::OpCode::Pop, for_line);
                self.emit_loop(loop_start, for_line)?;
                loop_start = increment_start;
                self.patch_jump(body_jump)?;
            }
            None => {
                return Err(Self::error(
                    "Required a token while parsing the increment in a 'for' loop.",
                ));
            }
        }
        self.consume(
            mem::discriminant(&scanner::TokenType::RightParen),
            "Require a ')' at the end of a for loop body.",
        )?;

        // Body
        self.statement()?;
        let line = self.peek().map_or(0, |token| token.line);
        self.emit_loop(loop_start, line)?;
        if let Some(jump_size) = condition_jump {
            self.patch_jump(jump_size)?;
            self.current_chunk()?.write_chunk(chunk::OpCode::Pop, line);
        }
        self.end_scope(line)?;
        Ok(())
    }

    fn return_statement(&mut self) -> Result<(), Error> {
        if self.current_frame()?.function_type == object::FunctionType::Script {
            return Err(Self::error("Cannot return from a top level script."));
        }
        self.consume(
            mem::discriminant(&scanner::TokenType::Return),
            "Require a 'return' keyword to parse a return statement.",
        )?;
        match self.peek().map(|token| &token.token_type) {
            Some(scanner::TokenType::Semicolon) => {
                self.advance();
                self.end_function()?;
            }
            Some(_) => {
                self.expression()?;
                let line = self.consume(
                    mem::discriminant(&scanner::TokenType::Semicolon),
                    "Expect a ';' at the end of a return statement.",
                )?;
                self.current_chunk()?
                    .write_chunk(chunk::OpCode::Return, line);
            }
            None => {
                return Err(Self::error("Expect a token when parsing a return"));
            }
        }
        Ok(())
    }

    fn emit_jump(&mut self, instruction: chunk::OpCode, line: usize) -> Result<usize, Error> {
        self.current_chunk()?.write_chunk(instruction, line);
        Ok(self.current_chunk()?.code_len() - 1)
    }

    fn patch_jump(&mut self, offset: usize) -> Result<(), Error> {
        let code_to_jump = self.current_chunk()?.code_len() - offset - 1;
        let code = self.current_chunk()?.patch_code(offset);
        match code {
            chunk::OpCode::JumpIfFalse(_) => {
                *code = chunk::OpCode::JumpIfFalse(code_to_jump);
                Ok(())
            }
            chunk::OpCode::Jump(_) => {
                *code = chunk::OpCode::Jump(code_to_jump);
                Ok(())
            }
            _ => Err(Self::error("Tried to patch a non-jump instruction.")),
        }
    }

    fn emit_loop(&mut self, offset: usize, line: usize) -> Result<(), Error> {
        let code_to_jump = self.current_chunk()?.code_len() - offset + 1;
        self.current_chunk()?
            .write_chunk(chunk::OpCode::Loop(code_to_jump), line);
        Ok(())
    }

    fn expression(&mut self) -> Result<(), Error> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn block(&mut self) -> Result<(), Error> {
        self.consume(
            mem::discriminant(&scanner::TokenType::LeftBrace),
            "Did not get left brace before block.",
        )?;
        while let Some(token) = self.peek() {
            match token.token_type {
                scanner::TokenType::RightBrace => {
                    self.advance();
                    return Ok(());
                }
                _ => self.declaration()?,
            }
        }

        Err(Self::error("Expect '}' at end of block."))
    }

    fn number(&mut self, _: bool) -> Result<(), Error> {
        let token = self
            .advance()
            .ok_or_else(|| Self::error("Missing required number token."))?;
        match token.token_type {
            scanner::TokenType::Number(value) => {
                self.current_chunk()?
                    .write_constant_instruction(chunk::Value::Number(value), token.line);
                Ok(())
            }
            _ => Err(Self::report_error(
                &token,
                "Non-number token cannot be parsed as number.",
            )),
        }
    }

    fn grouping(&mut self, _: bool) -> Result<(), Error> {
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

    fn unary(&mut self, _: bool) -> Result<(), Error> {
        let token = self
            .advance()
            .ok_or_else(|| Self::error("Missing required unary operation token."))?;

        self.parse_precedence(Precedence::Unary)?;
        match token.token_type {
            scanner::TokenType::Minus => {
                self.current_chunk()?
                    .write_chunk(chunk::OpCode::Negate, token.line);
                Ok(())
            }
            scanner::TokenType::Bang => {
                self.current_chunk()?
                    .write_chunk(chunk::OpCode::Not, token.line);
                Ok(())
            }
            _ => Err(Self::report_error(&token, "Non-unary operation found.")),
        }
    }

    fn binary(&mut self, _: bool) -> Result<(), Error> {
        let token = self
            .advance()
            .ok_or_else(|| Self::error("Missing required binary operation token."))?;
        let bin_rule = Self::get_rule(&token.token_type);
        let precedence = bin_rule.precedence.increment();
        self.parse_precedence(precedence)?;

        match token.token_type {
            scanner::TokenType::Minus => {
                self.current_chunk()?
                    .write_chunk(chunk::OpCode::Subtract, token.line);
                Ok(())
            }
            scanner::TokenType::Plus => {
                self.current_chunk()?
                    .write_chunk(chunk::OpCode::Add, token.line);
                Ok(())
            }
            scanner::TokenType::Star => {
                self.current_chunk()?
                    .write_chunk(chunk::OpCode::Multiply, token.line);
                Ok(())
            }
            scanner::TokenType::Slash => {
                self.current_chunk()?
                    .write_chunk(chunk::OpCode::Divide, token.line);
                Ok(())
            }
            scanner::TokenType::BangEqual => {
                self.current_chunk()?
                    .write_chunk(chunk::OpCode::Equal, token.line);
                self.current_chunk()?
                    .write_chunk(chunk::OpCode::Not, token.line);
                Ok(())
            }
            scanner::TokenType::EqualEqual => {
                self.current_chunk()?
                    .write_chunk(chunk::OpCode::Equal, token.line);
                Ok(())
            }
            scanner::TokenType::Greater => {
                self.current_chunk()?
                    .write_chunk(chunk::OpCode::Greater, token.line);
                Ok(())
            }
            scanner::TokenType::GreaterEqual => {
                self.current_chunk()?
                    .write_chunk(chunk::OpCode::Less, token.line);
                self.current_chunk()?
                    .write_chunk(chunk::OpCode::Not, token.line);
                Ok(())
            }
            scanner::TokenType::Less => {
                self.current_chunk()?
                    .write_chunk(chunk::OpCode::Less, token.line);
                Ok(())
            }
            scanner::TokenType::LessEqual => {
                self.current_chunk()?
                    .write_chunk(chunk::OpCode::Greater, token.line);
                self.current_chunk()?
                    .write_chunk(chunk::OpCode::Not, token.line);
                Ok(())
            }
            _ => Err(Self::report_error(
                &token,
                "Binary operation expected but not found.",
            )),
        }
    }

    fn call(&mut self, _: bool) -> Result<(), Error> {
        let token = self
            .advance()
            .ok_or_else(|| Self::error("Missing required left parenthesis in call."))?;
        let arg_count = self.argument_list()?;
        self.current_chunk()?
            .write_chunk(chunk::OpCode::Call(arg_count), token.line);
        Ok(())
    }

    fn literal(&mut self, _: bool) -> Result<(), Error> {
        let token = self
            .advance()
            .ok_or_else(|| Self::error("Missing required literal token."))?;
        match token.token_type {
            scanner::TokenType::False => {
                self.current_chunk()?
                    .write_chunk(chunk::OpCode::False, token.line);
                Ok(())
            }
            scanner::TokenType::True => {
                self.current_chunk()?
                    .write_chunk(chunk::OpCode::True, token.line);
                Ok(())
            }
            scanner::TokenType::Nil => {
                self.current_chunk()?
                    .write_chunk(chunk::OpCode::Nil, token.line);
                Ok(())
            }
            _ => Err(Self::report_error(
                &token,
                "Did not get a literal token when parsing literal.",
            )),
        }
    }

    fn string(&mut self, _: bool) -> Result<(), Error> {
        let token = self
            .advance()
            .ok_or_else(|| Self::error("Missing required string token."))?;
        match token.token_type {
            scanner::TokenType::String(string) => {
                self.current_chunk()?
                    .write_constant_instruction(chunk::Value::String(Rc::from(string)), token.line);
                Ok(())
            }
            _ => Err(Self::report_error(
                &token,
                "Did not get a string token when parsing a string.",
            )),
        }
    }

    fn variable(&mut self, can_assign: bool) -> Result<(), Error> {
        let token = self
            .advance()
            .ok_or_else(|| Self::error("Missing required variable token."))?;
        self.named_variable(&token, can_assign)
    }

    fn named_variable(
        &mut self,
        token: &scanner::Token<'a>,
        can_assign: bool,
    ) -> Result<(), Error> {
        let scanner::TokenType::Identifier(ident) = token.token_type else {
            return Err(Self::report_error(
                token,
                "Did not get identifier when reading variable.",
            ));
        };
        let (get_op, set_op) = if let Some(index) = self.resolve_local(token)? {
            (
                chunk::OpCode::GetLocal(index),
                chunk::OpCode::SetLocal(index),
            )
        } else {
            let index = self
                .current_chunk()?
                .write_constant(chunk::Value::String(Rc::from(ident)));
            (
                chunk::OpCode::GetGlobal(index),
                chunk::OpCode::SetGlobal(index),
            )
        };
        if can_assign && self.match_token(mem::discriminant(&scanner::TokenType::Equal)) {
            self.expression()?;
            self.current_chunk()?.write_chunk(set_op, token.line);
        } else {
            self.current_chunk()?.write_chunk(get_op, token.line);
        }
        Ok(())
    }

    fn resolve_local(&mut self, token: &scanner::Token<'a>) -> Result<Option<usize>, Error> {
        self.current_frame()?.resolve_local(token)
    }

    fn extract_name(token: &scanner::Token<'a>) -> Result<&'a str, Error> {
        if let scanner::TokenType::Identifier(ident) = token.token_type {
            Ok(ident)
        } else {
            Err(Self::report_error(token, "Token should be an identifier."))
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), Error> {
        let current = self
            .peek()
            .ok_or_else(|| Self::error("Required more tokens to parse rule."))?;
        let parse_rule = Self::get_rule(&current.token_type);

        let can_assign = parse_rule.precedence <= Precedence::Assignment;
        if let Some(prefix_fn) = parse_rule.prefix_fn {
            prefix_fn(self, can_assign)?;
        }

        while let Some(rule) = self.peek().map(|token| Self::get_rule(&token.token_type))
            && precedence <= rule.precedence
        {
            if let Some(infix_fn) = rule.infix_fn {
                infix_fn(self, can_assign)?;
            }

            if can_assign && self.match_token(mem::discriminant(&scanner::TokenType::Equal)) {
                return Err(Self::error("Invalid assignment target"));
            }
        }
        Ok(())
    }

    fn parse_variable(&mut self, error_message: &'static str) -> Result<usize, Error> {
        if let Some(current) = self.advance() {
            match current.token_type {
                scanner::TokenType::Identifier(ident) => {
                    if self.current_frame()?.scope_depth > 0 {
                        self.declare_variable(current)?;
                        Ok(0)
                    } else {
                        let index = self
                            .current_chunk()?
                            .write_constant(chunk::Value::String(Rc::from(ident)));
                        Ok(index)
                    }
                }
                _ => Err(Self::report_error(&current, error_message)),
            }
        } else {
            Err(Self::error("No token found when parsing a variable."))
        }
    }

    fn declare_variable(&mut self, token: scanner::Token<'a>) -> Result<(), Error> {
        self.current_frame()?.declare_variable(token)
    }

    fn define_variable(&mut self, index: usize, line: usize) -> Result<(), Error> {
        if self.current_frame()?.scope_depth > 0 {
            self.mark_initialized()?;
            return Ok(());
        }
        self.current_chunk()?
            .write_chunk(chunk::OpCode::DefineGlobal(index), line);
        Ok(())
    }

    fn argument_list(&mut self) -> Result<usize, Error> {
        let mut arg_count = 0;
        while !self.check(mem::discriminant(&scanner::TokenType::RightParen)) {
            self.expression()?;
            arg_count += 1;
            if arg_count >= 255 {
                return Err(Self::error(
                    "Cannot pass more than 255 arguments to a function.",
                ));
            }
            if !self.check(mem::discriminant(&scanner::TokenType::RightParen)) {
                self.consume(
                    mem::discriminant(&scanner::TokenType::Comma),
                    "Arguments to a function call should be separated with ','.",
                )?;
            }
        }
        self.consume(
            mem::discriminant(&scanner::TokenType::RightParen),
            "Call should end with ')'.",
        )?;
        Ok(arg_count)
    }

    fn mark_initialized(&mut self) -> Result<(), Error> {
        self.current_frame()?.mark_initialized();
        Ok(())
    }

    fn parse_and(&mut self, _: bool) -> Result<(), Error> {
        let next_line = self.consume(
            mem::discriminant(&scanner::TokenType::And),
            "Expected an 'and' token.",
        )?;
        let jump = self.emit_jump(chunk::OpCode::JumpIfFalse(0), next_line)?;
        self.current_chunk()?
            .write_chunk(chunk::OpCode::Pop, next_line);
        self.parse_precedence(Precedence::And)?;
        self.patch_jump(jump)
    }

    fn parse_or(&mut self, _: bool) -> Result<(), Error> {
        let next_line = self.consume(
            mem::discriminant(&scanner::TokenType::Or),
            "Expected an 'or' token.",
        )?;
        let first_jump = self.emit_jump(chunk::OpCode::JumpIfFalse(0), next_line)?;
        let second_jump = self.emit_jump(chunk::OpCode::Jump(0), next_line)?;
        self.patch_jump(first_jump)?;
        self.current_chunk()?
            .write_chunk(chunk::OpCode::Pop, next_line);

        self.parse_precedence(Precedence::Or)?;
        self.patch_jump(second_jump)
    }

    #[allow(clippy::match_same_arms)]
    fn get_rule(token_type: &scanner::TokenType<'a>) -> ParseTableEntry<'a> {
        match token_type {
            scanner::TokenType::LeftParen => {
                ParseTableEntry::new(Some(Self::grouping), Some(Self::call), Precedence::Call)
            }
            scanner::TokenType::RightParen => {
                ParseTableEntry::new(Some(Self::grouping), None, Precedence::None)
            }
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
            scanner::TokenType::Bang => {
                ParseTableEntry::new(Some(Self::unary), None, Precedence::None)
            }
            scanner::TokenType::BangEqual => {
                ParseTableEntry::new(None, Some(Self::binary), Precedence::Equality)
            }
            scanner::TokenType::Equal => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::EqualEqual => {
                ParseTableEntry::new(None, Some(Self::binary), Precedence::Equality)
            }
            scanner::TokenType::Greater => {
                ParseTableEntry::new(None, Some(Self::binary), Precedence::Comparison)
            }
            scanner::TokenType::GreaterEqual => {
                ParseTableEntry::new(None, Some(Self::binary), Precedence::Comparison)
            }
            scanner::TokenType::Less => {
                ParseTableEntry::new(None, Some(Self::binary), Precedence::Comparison)
            }
            scanner::TokenType::LessEqual => {
                ParseTableEntry::new(None, Some(Self::binary), Precedence::Comparison)
            }
            scanner::TokenType::Identifier(_) => {
                ParseTableEntry::new(Some(Self::variable), None, Precedence::None)
            }
            scanner::TokenType::String(_) => {
                ParseTableEntry::new(Some(Self::string), None, Precedence::None)
            }
            scanner::TokenType::Number(_) => {
                ParseTableEntry::new(Some(Self::number), None, Precedence::None)
            }
            scanner::TokenType::And => {
                ParseTableEntry::new(None, Some(Self::parse_and), Precedence::And)
            }
            scanner::TokenType::Class => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Else => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::False => {
                ParseTableEntry::new(Some(Self::literal), None, Precedence::None)
            }
            scanner::TokenType::For => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Fun => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::If => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Nil => {
                ParseTableEntry::new(Some(Self::literal), None, Precedence::None)
            }
            scanner::TokenType::Or => {
                ParseTableEntry::new(None, Some(Self::parse_or), Precedence::Or)
            }
            scanner::TokenType::Print => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Return => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Super => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::This => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::True => {
                ParseTableEntry::new(Some(Self::literal), None, Precedence::None)
            }
            scanner::TokenType::Var => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::While => ParseTableEntry::new(None, None, Precedence::None),
            scanner::TokenType::Error(_) => ParseTableEntry::new(None, None, Precedence::None),
        }
    }

    fn report_error(token: &scanner::Token<'a>, message: &'static str) -> Error {
        eprint!("[line {}] Error", token.line);
        match &token.token_type {
            scanner::TokenType::Error(scan_message) => eprintln!(" : {scan_message}"),
            other => eprintln!(" at {other:?}: {message}"),
        }
        Error::CompileError(message)
    }

    fn error(message: &'static str) -> Error {
        eprintln!("Parse error occurred: {message}");
        Error::CompileError(message)
    }
}

#[derive(Debug, PartialEq, PartialOrd, Copy, Clone)]
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
    fn increment(self) -> Precedence {
        match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call | Precedence::Primary => Precedence::Primary, // Safe to return highest if already there.
        }
    }
}

type ParseFn<'a> = fn(&mut Compiler<'a>, bool) -> Result<(), Error>;

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

enum Local<'a> {
    Initialized(usize, scanner::Token<'a>),
    Uninitialized(scanner::Token<'a>),
}

impl<'a> Local<'a> {
    fn new(name: scanner::Token<'a>) -> Self {
        Self::Uninitialized(name)
    }

    fn initialize(self, depth: usize) -> Self {
        match self {
            Self::Uninitialized(name) => Self::Initialized(depth, name),
            Self::Initialized(_, _) => self,
        }
    }

    fn name(&self) -> &scanner::Token<'a> {
        match self {
            Self::Uninitialized(name) | Self::Initialized(_, name) => name,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::chunk::*;

    fn compile(source: &str) -> Chunk {
        let compiler = Compiler::new(source);
        compiler
            .compile()
            .expect("Compiler error unexpected.")
            .chunk
    }

    #[test]
    fn basic_parse() {
        let source = "1 - 2;";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Constant(0),
                OpCode::Constant(1),
                OpCode::Subtract,
                OpCode::Pop,
                OpCode::Nil,
            ],
            &[chunk::Value::Number(1.0), chunk::Value::Number(2.0)],
        );
    }

    #[test]
    fn precedence_left() {
        let source = "1 * 2 + 3;";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Constant(0),
                OpCode::Constant(1),
                OpCode::Multiply,
                OpCode::Constant(2),
                OpCode::Add,
                OpCode::Pop,
                OpCode::Nil,
            ],
            &[
                chunk::Value::Number(1.0),
                chunk::Value::Number(2.0),
                chunk::Value::Number(3.0),
            ],
        );
    }

    #[test]
    fn precedence_right() {
        let source = "1 + 2 / 3;";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Constant(0),
                OpCode::Constant(1),
                OpCode::Constant(2),
                OpCode::Divide,
                OpCode::Add,
                OpCode::Pop,
                OpCode::Nil,
            ],
            &[
                chunk::Value::Number(1.0),
                chunk::Value::Number(2.0),
                chunk::Value::Number(3.0),
            ],
        );
    }

    #[test]
    fn unary() {
        let source = "-1;";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Constant(0),
                OpCode::Negate,
                OpCode::Pop,
                OpCode::Nil,
            ],
            &[chunk::Value::Number(1.0)],
        );
    }

    #[test]
    fn prefix_and_infix() {
        let source = "-1 + 2;";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Constant(0),
                OpCode::Negate,
                OpCode::Constant(1),
                OpCode::Add,
                OpCode::Pop,
                OpCode::Nil,
            ],
            &[chunk::Value::Number(1.0), chunk::Value::Number(2.0)],
        );
    }

    #[test]
    fn grouping() {
        let source = "(1 + 2) * 3;";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Constant(0),
                OpCode::Constant(1),
                OpCode::Add,
                OpCode::Constant(2),
                OpCode::Multiply,
                OpCode::Pop,
                OpCode::Nil,
            ],
            &[
                chunk::Value::Number(1.0),
                chunk::Value::Number(2.0),
                chunk::Value::Number(3.0),
            ],
        );
    }

    #[test]
    fn nested_grouping() {
        let source = "1 + (2 * (3 + 4));";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Constant(0),
                OpCode::Constant(1),
                OpCode::Constant(2),
                OpCode::Constant(3),
                OpCode::Add,
                OpCode::Multiply,
                OpCode::Add,
                OpCode::Pop,
                OpCode::Nil,
            ],
            &[
                chunk::Value::Number(1.0),
                chunk::Value::Number(2.0),
                chunk::Value::Number(3.0),
                chunk::Value::Number(4.0),
            ],
        );
    }

    #[test]
    fn empty() {
        let source = "";
        let chunk = compile(source);
        check_chunk(&chunk, &[], &[]);
    }

    #[test]
    fn missing_arg() {
        let source = "1 +";
        let compiler = Compiler::new(source);
        let output = compiler.compile();
        assert!(matches!(output, Err(Error::CompileError(_))));
    }

    #[test]
    fn dangling_unary() {
        let source = "-";
        let compiler = Compiler::new(source);
        let output = compiler.compile();
        assert!(matches!(output, Err(Error::CompileError(_))));
    }

    #[test]
    fn missing_parens() {
        let source = "(1 + 2;";
        let compiler = Compiler::new(source);
        let output = compiler.compile();
        assert!(matches!(output, Err(Error::CompileError(_))));
    }

    #[test]
    fn spare_closing_parens() {
        let source = "1 + 2) - 4;";
        let compiler = Compiler::new(source);
        let output = compiler.compile();
        assert!(matches!(output, Err(Error::CompileError(_))));
    }

    #[test]
    fn true_value() {
        let source = "true;";
        let chunk = compile(source);
        check_chunk(&chunk, &[OpCode::True, OpCode::Pop, OpCode::Nil], &[]);
    }

    #[test]
    fn false_value() {
        let source = "false;";
        let chunk = compile(source);
        check_chunk(&chunk, &[OpCode::False, OpCode::Pop, OpCode::Nil], &[]);
    }

    #[test]
    fn nil_value() {
        let source = "nil;";
        let chunk = compile(source);
        check_chunk(&chunk, &[OpCode::Nil, OpCode::Pop, OpCode::Nil], &[]);
    }

    #[test]
    fn not_operation() {
        let source = "!true;";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[OpCode::True, OpCode::Not, OpCode::Pop, OpCode::Nil],
            &[],
        );
    }

    #[test]
    fn negate_non_number() {
        let source = "-false;";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[OpCode::False, OpCode::Negate, OpCode::Pop, OpCode::Nil],
            &[],
        );
    }

    #[test]
    fn equality() {
        let source = "1 == 2;";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Constant(0),
                OpCode::Constant(1),
                OpCode::Equal,
                OpCode::Pop,
                OpCode::Nil,
            ],
            &[chunk::Value::Number(1.0), chunk::Value::Number(2.0)],
        );
    }

    #[test]
    fn greater_and_lesser() {
        let source = "1 < 2 > 3;";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Constant(0),
                OpCode::Constant(1),
                OpCode::Less,
                OpCode::Constant(2),
                OpCode::Greater,
                OpCode::Pop,
                OpCode::Nil,
            ],
            &[
                chunk::Value::Number(1.0),
                chunk::Value::Number(2.0),
                chunk::Value::Number(3.0),
            ],
        );
    }

    #[test]
    fn greater_equal() {
        let source = "1 >= 2;";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Constant(0),
                OpCode::Constant(1),
                OpCode::Less,
                OpCode::Not,
                OpCode::Pop,
                OpCode::Nil,
            ],
            &[chunk::Value::Number(1.0), chunk::Value::Number(2.0)],
        );
    }

    #[test]
    fn less_equal() {
        let source = "1 <= 2;";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Constant(0),
                OpCode::Constant(1),
                OpCode::Greater,
                OpCode::Not,
                OpCode::Pop,
                OpCode::Nil,
            ],
            &[chunk::Value::Number(1.0), chunk::Value::Number(2.0)],
        );
    }

    #[test]
    fn not_equal() {
        let source = "1 != 2;";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Constant(0),
                OpCode::Constant(1),
                OpCode::Equal,
                OpCode::Not,
                OpCode::Pop,
                OpCode::Nil,
            ],
            &[chunk::Value::Number(1.0), chunk::Value::Number(2.0)],
        );
    }

    #[test]
    fn strings() {
        let source = "\"foo\";";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[OpCode::Constant(0), OpCode::Pop, OpCode::Nil],
            &[chunk::Value::String(Rc::from(String::from("foo")))],
        );
    }

    #[test]
    fn addition_wrong_types() {
        let source = "1 + \"foo\";";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Constant(0),
                OpCode::Constant(1),
                OpCode::Add,
                OpCode::Pop,
                OpCode::Nil,
            ],
            &[
                chunk::Value::Number(1.0),
                chunk::Value::String(Rc::from(String::from("foo"))),
            ],
        );
    }

    #[test]
    fn global_variable() {
        let source = "var foo = 12; print foo;";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Constant(1),
                OpCode::DefineGlobal(0),
                OpCode::GetGlobal(2),
                OpCode::Print,
                OpCode::Nil,
            ],
            &[
                chunk::Value::String(Rc::from(String::from("foo"))),
                chunk::Value::Number(12.),
                chunk::Value::String(Rc::from(String::from("foo"))),
            ],
        );
    }

    #[test]
    fn uninitialized_global() {
        let source = "var foo; print foo;";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Nil,
                OpCode::DefineGlobal(0),
                OpCode::GetGlobal(1),
                OpCode::Print,
                OpCode::Nil,
            ],
            &[
                chunk::Value::String(Rc::from(String::from("foo"))),
                chunk::Value::String(Rc::from(String::from("foo"))),
            ],
        );
    }

    #[test]
    fn dangling_var() {
        let source = "var;";
        let compiler = Compiler::new(source);
        let output = compiler.compile();
        assert!(matches!(output, Err(Error::CompileError(_))));
    }

    #[test]
    fn variable_in_statement() {
        let source = "var foo = 1; print 1 + foo;";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Constant(1),
                OpCode::DefineGlobal(0),
                OpCode::Constant(2),
                OpCode::GetGlobal(3),
                OpCode::Add,
                OpCode::Print,
                OpCode::Nil,
            ],
            &[
                chunk::Value::String(Rc::from(String::from("foo"))),
                chunk::Value::Number(1.),
                chunk::Value::Number(1.),
                chunk::Value::String(Rc::from(String::from("foo"))),
            ],
        );
    }

    #[test]
    fn error_in_first_line() {
        let source = "var; var foo = 1;";
        let compiler = Compiler::new(source);
        let result = compiler.compile();
        assert!(matches!(result, Err(Error::CompileError(_))));
    }

    #[test]
    fn assignment() {
        let source = "var foo; foo = 1;";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Nil,
                OpCode::DefineGlobal(0),
                OpCode::Constant(2),
                OpCode::SetGlobal(1),
                OpCode::Pop,
                OpCode::Nil,
            ],
            &[
                chunk::Value::String(Rc::from(String::from("foo"))),
                chunk::Value::String(Rc::from(String::from("foo"))),
                chunk::Value::Number(1.),
            ],
        );
    }

    #[test]
    fn invalid_assignment_target() {
        let source = "1 + 2 = 3 * 5;";
        let compiler = Compiler::new(source);
        let result = compiler.compile();
        assert!(matches!(result, Err(Error::CompileError(_))));
    }

    #[test]
    fn define_local_variable() {
        let source = "{ var a = 10; print a; }";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Constant(0),
                OpCode::GetLocal(0),
                OpCode::Print,
                OpCode::Pop,
                OpCode::Nil,
            ],
            &[chunk::Value::Number(10.)],
        );
    }

    #[test]
    fn get_local_variable() {
        let source = "{ var a = 10; a = 20; print a;}";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Constant(0),
                OpCode::Constant(1),
                OpCode::SetLocal(0),
                OpCode::Pop,
                OpCode::GetLocal(0),
                OpCode::Print,
                OpCode::Pop,
                OpCode::Nil,
            ],
            &[chunk::Value::Number(10.), chunk::Value::Number(20.)],
        );
    }

    #[test]
    fn shadowed_variable() {
        let source = "{ var a = 10; { var a = 20; print a; } print a; }";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Constant(0),
                OpCode::Constant(1),
                OpCode::GetLocal(1),
                OpCode::Print,
                OpCode::Pop,
                OpCode::GetLocal(0),
                OpCode::Print,
                OpCode::Pop,
                OpCode::Nil,
            ],
            &[chunk::Value::Number(10.), chunk::Value::Number(20.)],
        );
    }

    #[test]
    fn redeclared_variable() {
        let source = "{ var a = 20; var a = 30; }";
        let compiler = Compiler::new(source);
        let result = compiler.compile();
        assert!(matches!(result, Err(Error::CompileError(_))));
    }

    #[test]
    fn variable_use_in_initialization() {
        let source = "{ var a = a + 20; }";
        let compiler = Compiler::new(source);
        let result = compiler.compile();
        assert!(matches!(result, Err(Error::CompileError(_))));
    }

    #[test]
    fn and_keyword() {
        let source = "if (true and false) print \"no\";";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::True,
                OpCode::JumpIfFalse(2),
                OpCode::Pop,
                OpCode::False,
                OpCode::JumpIfFalse(4),
                OpCode::Pop,
                OpCode::Constant(0),
                OpCode::Print,
                OpCode::Jump(1),
                OpCode::Pop,
                OpCode::Nil,
            ],
            &[chunk::Value::String(Rc::from(String::from("no")))],
        );
    }

    #[test]
    fn or_keyword() {
        let source = "if (true or false) print \"no\";";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::True,
                OpCode::JumpIfFalse(1),
                OpCode::Jump(2),
                OpCode::Pop,
                OpCode::False,
                OpCode::JumpIfFalse(4),
                OpCode::Pop,
                OpCode::Constant(0),
                OpCode::Print,
                OpCode::Jump(1),
                OpCode::Pop,
                OpCode::Nil,
            ],
            &[chunk::Value::String(Rc::from(String::from("no")))],
        );
    }

    #[test]
    fn while_keyword() {
        let source = "while (true) print 1;";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::True,
                OpCode::JumpIfFalse(4),
                OpCode::Pop,
                OpCode::Constant(0),
                OpCode::Print,
                OpCode::Loop(6),
                OpCode::Pop,
                OpCode::Nil,
            ],
            &[chunk::Value::Number(1.)],
        );
    }

    #[test]
    fn for_keyword_all_specified() {
        let source = "for (var i = 0; i < 10; i = i + 1) print i;";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Constant(0),
                OpCode::GetLocal(0),
                OpCode::Constant(1),
                OpCode::Less,
                OpCode::JumpIfFalse(11),
                OpCode::Pop,
                OpCode::Jump(6),
                OpCode::GetLocal(0),
                OpCode::Constant(2),
                OpCode::Add,
                OpCode::SetLocal(0),
                OpCode::Pop,
                OpCode::Loop(12),
                OpCode::GetLocal(0),
                OpCode::Print,
                OpCode::Loop(9),
                OpCode::Pop,
                OpCode::Pop,
                OpCode::Nil,
            ],
            &[
                chunk::Value::Number(0.),
                chunk::Value::Number(10.),
                chunk::Value::Number(1.),
            ],
        );
    }

    #[test]
    fn for_keyword_no_initializer() {
        let source = "var i = 0; for (; i < 10; i = i + 1) print i;";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Constant(1),
                OpCode::DefineGlobal(0),
                OpCode::GetGlobal(2),
                OpCode::Constant(3),
                OpCode::Less,
                OpCode::JumpIfFalse(11),
                OpCode::Pop,
                OpCode::Jump(6),
                OpCode::GetGlobal(5),
                OpCode::Constant(6),
                OpCode::Add,
                OpCode::SetGlobal(4),
                OpCode::Pop,
                OpCode::Loop(12),
                OpCode::GetGlobal(7),
                OpCode::Print,
                OpCode::Loop(9),
                OpCode::Pop,
                OpCode::Nil,
            ],
            &[
                chunk::Value::String(Rc::from(String::from("j"))),
                chunk::Value::Number(0.),
                chunk::Value::String(Rc::from(String::from("j"))),
                chunk::Value::Number(10.),
                chunk::Value::String(Rc::from(String::from("j"))),
                chunk::Value::String(Rc::from(String::from("j"))),
                chunk::Value::Number(1.),
                chunk::Value::String(Rc::from(String::from("j"))),
            ],
        );
    }

    #[test]
    fn function_declaration() {
        let source = "fun areWeHavingIt() { print 1; } print areWeHavingIt;";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Closure(1),
                OpCode::DefineGlobal(0),
                OpCode::GetGlobal(2),
                OpCode::Print,
                OpCode::Nil,
            ],
            &[
                test_string("areWeHavingIt"),
                test_function("areWeHavingIt", 0),
                test_string("areWeHavingIt"),
            ],
        );
        let function = extract_function(&chunk, 1);
        check_chunk(
            &function.chunk,
            &[
                OpCode::Constant(0),
                OpCode::Print,
                OpCode::Nil,
                OpCode::Return,
            ],
            &[chunk::Value::Number(1.)],
        );
    }

    #[test]
    fn call_with_return() {
        let source = "fun add(a, b) { return a + b; } add(1, 2);";
        let chunk = compile(source);
        check_chunk(
            &chunk,
            &[
                OpCode::Closure(1),
                OpCode::DefineGlobal(0),
                OpCode::GetGlobal(2),
                OpCode::Constant(3),
                OpCode::Constant(4),
                OpCode::Call(2),
                OpCode::Pop,
                OpCode::Nil,
                OpCode::Return,
            ],
            &[
                test_string("add"),
                test_function("add", 2),
                test_string("add"),
                chunk::Value::Number(1.),
                chunk::Value::Number(2.),
            ],
        );
    }

    fn check_chunk(chunk: &Chunk, opcodes: &[OpCode], constants: &[Value]) {
        for (index, opcode) in opcodes.iter().enumerate() {
            assert_eq!(opcode, chunk.read_code(index));
            if let OpCode::Constant(const_index) = opcode {
                let left = &constants[usize::from(*const_index)];
                match left {
                    chunk::Value::Function(func) => {
                        let chunk::Value::Function(rfunc) =
                            chunk.read_constant((*const_index).into())
                        else {
                            panic!("Both sides should be functions.")
                        };
                        assert_eq!(func.name, rfunc.name);
                        assert_eq!(func.arity, rfunc.arity);
                    }
                    _ => {
                        assert_eq!(
                            constants[usize::from(*const_index)],
                            *chunk.read_constant((*const_index).into())
                        );
                    }
                }
            }
        }
        assert_eq!(OpCode::Return, *chunk.read_code(opcodes.len()));
    }

    fn test_function(name: &str, arity: usize) -> chunk::Value {
        let mut function = object::Function::new(name);
        function.arity = arity;
        chunk::Value::Function(Rc::from(function))
    }

    fn test_string(value: &str) -> chunk::Value {
        chunk::Value::String(Rc::from(String::from(value)))
    }

    fn extract_function(chunk: &Chunk, index: usize) -> &object::Function {
        match chunk.read_constant(index) {
            chunk::Value::Function(func) => func,
            _ => panic!("Did not get a function where expected."),
        }
    }
}
