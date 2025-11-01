const UNTERMINATED_ERROR: &str = "Unterminated string literal found.";

pub struct Scanner<'a> {
    line: usize,
    source: &'a str,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner { line: 0, source }
    }

    fn parse(&mut self) -> Option<Token<'a>> {
        if let Some(idx) = self.source.find(|c: char| !c.is_ascii_whitespace()) {
            let whitespace = &self.source[0..idx];
            self.source = &self.source[idx..];
            self.line += whitespace.chars().filter(|&c| c == '\n').count();
        }
        let mut iter = self.source.chars().peekable();
        match iter.next() {
            Some('{') => self.parse_single_character(Token::LeftBrace),
            Some('}') => self.parse_single_character(Token::RightBrace),
            Some('(') => self.parse_single_character(Token::LeftParen),
            Some(')') => self.parse_single_character(Token::RightParen),
            Some(';') => self.parse_single_character(Token::Semicolon),
            Some(',') => self.parse_single_character(Token::Comma),
            Some('.') => self.parse_single_character(Token::Dot),
            Some('+') => self.parse_single_character(Token::Plus),
            Some('-') => self.parse_single_character(Token::Minus),
            Some('*') => self.parse_single_character(Token::Star),
            Some('/') => {
                if let Some('/') = iter.next() {
                    let idx = self.source.find('\n').unwrap_or(self.source.len());
                    self.source = &self.source[idx..];
                    self.parse()
                } else {
                    self.parse_single_character(Token::Slash)
                }
            }
            Some('!') => self.parse_double_character(Token::Bang, Token::BangEqual),
            Some('=') => self.parse_double_character(Token::Equal, Token::EqualEqual),
            Some('>') => self.parse_double_character(Token::Greater, Token::GreaterEqual),
            Some('<') => self.parse_double_character(Token::Less, Token::LessEqual),
            Some('"') => Some(self.parse_string()),
            Some(c) if c.is_numeric() => Some(self.parse_numeric()),
            Some(_) => Some(self.parse_identifier()),
            None => None,
        }
    }

    fn parse_single_character(&mut self, token: Token<'a>) -> Option<Token<'a>> {
        self.source = &self.source[1..];
        Some(token)
    }

    fn parse_double_character(
        &mut self,
        single_token: Token<'a>,
        double_token: Token<'a>,
    ) -> Option<Token<'a>> {
        if let Some('=') = self.source.chars().nth(1) {
            self.source = &self.source[2..];
            Some(double_token)
        } else {
            self.source = &self.source[1..];
            Some(single_token)
        }
    }

    fn parse_string(&mut self) -> Token<'a> {
        // Remove the initial quote.
        self.source = &self.source[1..];
        if let Some(idx) = self.source.find('"') {
            let literal = &self.source[0..idx];
            // Need to go past the terminating quote, hence the +1.
            self.source = &self.source[idx + 1..];
            self.line += literal.chars().filter(|&c| c == '\n').count();
            Token::String(literal)
        } else {
            self.source = &self.source[self.source.len()..];
            Token::Error(UNTERMINATED_ERROR)
        }
    }

    fn number_end_index(&self) -> usize {
        if let Some(idx) = self.source.find(|c: char| !c.is_numeric()) {
            if let Some('.') = &self.source[idx..].chars().nth(0) {
                self.source[idx + 1..]
                    .find(|c: char| !c.is_numeric())
                    .unwrap_or(self.source.len() - idx - 1)
                    + idx
                    + 1
            } else {
                idx
            }
        } else {
            self.source.len()
        }
    }

    fn parse_numeric(&mut self) -> Token<'a> {
        let idx = self.number_end_index();
        let number = &self.source[0..idx];
        self.source = &self.source[idx..];
        Token::Number(number.parse::<f64>().unwrap())
    }

    fn parse_identifier(&mut self) -> Token<'a> {
        let ident = if let Some(idx) = self.source.find(|c: char| !c.is_alphanumeric() && c != '_')
        {
            let ident = &self.source[0..idx];
            self.source = &self.source[idx..];
            ident
        } else {
            let ident = self.source;
            self.source = &self.source[self.source.len()..];
            ident
        };
        self.check_keyword(ident)
            .unwrap_or(Token::Identifier(ident))
    }

    fn check_keyword(&self, ident: &'a str) -> Option<Token<'a>> {
        match ident {
            "and" => Some(Token::And),
            "class" => Some(Token::Class),
            "else" => Some(Token::Else),
            "false" => Some(Token::False),
            "for" => Some(Token::For),
            "fun" => Some(Token::Fun),
            "if" => Some(Token::If),
            "nil" => Some(Token::Nil),
            "or" => Some(Token::Or),
            "print" => Some(Token::Print),
            "return" => Some(Token::Return),
            "super" => Some(Token::Super),
            "this" => Some(Token::This),
            "true" => Some(Token::True),
            "var" => Some(Token::Var),
            "while" => Some(Token::While),
            _ => None,
        }
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse()
    }
}

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier(&'a str),
    String(&'a str),
    Number(f64),
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Error(&'static str),
}

#[cfg(test)]
mod tests {
    use crate::scanner::{Scanner, Token, UNTERMINATED_ERROR};

    #[test]
    fn parse_single_characters() {
        let text = "{}();,.+-*/";
        let scanner = Scanner::new(text);
        assert_eq!(
            vec![
                Token::LeftBrace,
                Token::RightBrace,
                Token::LeftParen,
                Token::RightParen,
                Token::Semicolon,
                Token::Comma,
                Token::Dot,
                Token::Plus,
                Token::Minus,
                Token::Star,
                Token::Slash
            ],
            scanner.collect::<Vec<_>>()
        );
    }

    #[test]
    fn parse_double_characters() {
        let text = "!!====>=><=<";
        let scanner = Scanner::new(text);
        assert_eq!(
            vec![
                Token::Bang,
                Token::BangEqual,
                Token::EqualEqual,
                Token::Equal,
                Token::GreaterEqual,
                Token::Greater,
                Token::LessEqual,
                Token::Less
            ],
            scanner.collect::<Vec<_>>()
        );
    }

    #[test]
    fn advance_whitespace() {
        let text = "   \n    \n.  \t    ==";
        let mut scanner = Scanner::new(text);
        assert_eq!(
            vec![Token::Dot, Token::EqualEqual],
            scanner.by_ref().collect::<Vec<_>>()
        );
        assert_eq!(2, scanner.line);
    }

    #[test]
    fn skip_comments() {
        let text = ".+ // this is a comment \n.-";
        let mut scanner = Scanner::new(text);
        assert_eq!(
            vec![Token::Dot, Token::Plus, Token::Dot, Token::Minus],
            scanner.by_ref().collect::<Vec<_>>()
        );
        assert_eq!(1, scanner.line);
    }

    #[test]
    fn parse_string() {
        let text = "\"string literal \n more literal\"";
        let mut scanner = Scanner::new(text);
        assert_eq!(
            vec![Token::String("string literal \n more literal")],
            scanner.by_ref().collect::<Vec<_>>()
        );
        assert_eq!(1, scanner.line);
    }

    #[test]
    fn parse_unterminated_string() {
        let text = "\"unterminated literal";
        let scanner = Scanner::new(text);
        assert_eq!(
            vec![Token::Error(UNTERMINATED_ERROR)],
            scanner.collect::<Vec<_>>()
        );
    }

    #[test]
    fn parse_number() {
        let text = "45 12.34 765.";
        let scanner = Scanner::new(text);
        assert_eq!(
            vec![
                Token::Number(45.0),
                Token::Number(12.34),
                Token::Number(765.0)
            ],
            scanner.collect::<Vec<_>>()
        );
    }

    #[test]
    fn parse_keyword() {
        let text = "and class else false for fun if nil or print return super this true var while";
        let scanner = Scanner::new(text);
        assert_eq!(
            vec![
                Token::And,
                Token::Class,
                Token::Else,
                Token::False,
                Token::For,
                Token::Fun,
                Token::If,
                Token::Nil,
                Token::Or,
                Token::Print,
                Token::Return,
                Token::Super,
                Token::This,
                Token::True,
                Token::Var,
                Token::While
            ],
            scanner.collect::<Vec<_>>()
        )
    }

    #[test]
    fn parse_identifier() {
        let text = "my_ident forb tr o";
        let scanner = Scanner::new(text);
        assert_eq!(
            vec![
                Token::Identifier("my_ident"),
                Token::Identifier("forb"),
                Token::Identifier("tr"),
                Token::Identifier("o")
            ],
            scanner.collect::<Vec<_>>()
        )
    }

    #[test]
    fn numeric_and_identifier() {
        let text = "123.abc";
        let scanner = Scanner::new(text);
        assert_eq!(
            vec![Token::Number(123.0), Token::Identifier("abc")],
            scanner.collect::<Vec<_>>()
        )
    }
}
