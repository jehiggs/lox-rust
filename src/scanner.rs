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
        let mut next_non_whitespace = 0;
        for (idx, c) in self
            .source
            .char_indices()
            .take_while(|&(_, c)| c.is_ascii_whitespace())
        {
            if c == '\n' {
                self.line += 1;
            }
            next_non_whitespace = idx + 1;
        }
        self.source = &self.source[next_non_whitespace..];
        let mut iter = self.source.chars().peekable();
        match iter.next() {
            Some('{') => Some(self.parse_single_character(TokenType::LeftBrace)),
            Some('}') => Some(self.parse_single_character(TokenType::RightBrace)),
            Some('(') => Some(self.parse_single_character(TokenType::LeftParen)),
            Some(')') => Some(self.parse_single_character(TokenType::RightParen)),
            Some(';') => Some(self.parse_single_character(TokenType::Semicolon)),
            Some(',') => Some(self.parse_single_character(TokenType::Comma)),
            Some('.') => Some(self.parse_single_character(TokenType::Dot)),
            Some('+') => Some(self.parse_single_character(TokenType::Plus)),
            Some('-') => Some(self.parse_single_character(TokenType::Minus)),
            Some('*') => Some(self.parse_single_character(TokenType::Star)),
            Some('/') => {
                if let Some('/') = iter.next() {
                    let idx = self.source.find('\n').unwrap_or(self.source.len());
                    self.source = &self.source[idx..];
                    self.parse()
                } else {
                    Some(self.parse_single_character(TokenType::Slash))
                }
            }
            Some('!') => Some(self.parse_double_character(TokenType::Bang, TokenType::BangEqual)),
            Some('=') => Some(self.parse_double_character(TokenType::Equal, TokenType::EqualEqual)),
            Some('>') => {
                Some(self.parse_double_character(TokenType::Greater, TokenType::GreaterEqual))
            }
            Some('<') => Some(self.parse_double_character(TokenType::Less, TokenType::LessEqual)),
            Some('"') => Some(self.parse_string()),
            Some(c) if c.is_numeric() => Some(self.parse_numeric()),
            Some(_) => Some(self.parse_identifier()),
            None => None,
        }
    }

    fn parse_single_character(&mut self, token_type: TokenType<'a>) -> Token<'a> {
        self.source = &self.source[1..];
        Token::new(self.line, token_type)
    }

    fn parse_double_character(
        &mut self,
        single_token: TokenType<'a>,
        double_token: TokenType<'a>,
    ) -> Token<'a> {
        if let Some('=') = self.source.chars().nth(1) {
            self.source = &self.source[2..];
            Token::new(self.line, double_token)
        } else {
            self.source = &self.source[1..];
            Token::new(self.line, single_token)
        }
    }

    fn parse_string(&mut self) -> Token<'a> {
        // Remove the initial quote.
        self.source = &self.source[1..];
        let current_line = self.line;
        if let Some(idx) = self.source.find('"') {
            let literal = &self.source[0..idx];
            // Need to go past the terminating quote, hence the +1.
            self.source = &self.source[idx + 1..];
            self.line += literal.chars().filter(|&c| c == '\n').count();
            Token::new(current_line, TokenType::String(literal))
        } else {
            self.source = &self.source[self.source.len()..];
            Token::new(current_line, TokenType::Error(UNTERMINATED_ERROR))
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
        Token::new(self.line, TokenType::Number(number.parse::<f64>().unwrap()))
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
        let token_type = Self::check_keyword(ident).unwrap_or(TokenType::Identifier(ident));
        Token::new(self.line, token_type)
    }

    fn check_keyword(ident: &'a str) -> Option<TokenType<'a>> {
        match ident {
            "and" => Some(TokenType::And),
            "class" => Some(TokenType::Class),
            "else" => Some(TokenType::Else),
            "false" => Some(TokenType::False),
            "for" => Some(TokenType::For),
            "fun" => Some(TokenType::Fun),
            "if" => Some(TokenType::If),
            "nil" => Some(TokenType::Nil),
            "or" => Some(TokenType::Or),
            "print" => Some(TokenType::Print),
            "return" => Some(TokenType::Return),
            "super" => Some(TokenType::Super),
            "this" => Some(TokenType::This),
            "true" => Some(TokenType::True),
            "var" => Some(TokenType::Var),
            "while" => Some(TokenType::While),
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
pub struct Token<'a> {
    pub line: usize,
    pub token_type: TokenType<'a>,
}

impl<'a> Token<'a> {
    pub fn new(line: usize, token_type: TokenType<'a>) -> Self {
        Token { line, token_type }
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenType<'a> {
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
    use crate::scanner::*;

    #[test]
    fn parse_single_characters() {
        let text = "{}();,.+-*/";
        let scanner = Scanner::new(text);
        assert_eq!(
            vec![
                TokenType::LeftBrace,
                TokenType::RightBrace,
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::Semicolon,
                TokenType::Comma,
                TokenType::Dot,
                TokenType::Plus,
                TokenType::Minus,
                TokenType::Star,
                TokenType::Slash
            ],
            scanner.map(|token| token.token_type).collect::<Vec<_>>()
        );
    }

    #[test]
    fn parse_double_characters() {
        let text = "!!====>=><=<";
        let scanner = Scanner::new(text);
        assert_eq!(
            vec![
                TokenType::Bang,
                TokenType::BangEqual,
                TokenType::EqualEqual,
                TokenType::Equal,
                TokenType::GreaterEqual,
                TokenType::Greater,
                TokenType::LessEqual,
                TokenType::Less
            ],
            scanner.map(|token| token.token_type).collect::<Vec<_>>()
        );
    }

    #[test]
    fn advance_whitespace() {
        let text = "   \n    \n.  \t    ==";
        let scanner = Scanner::new(text);
        assert_eq!(
            vec![
                Token::new(2, TokenType::Dot),
                Token::new(2, TokenType::EqualEqual)
            ],
            scanner.collect::<Vec<_>>()
        );
    }

    #[test]
    fn skip_comments() {
        let text = ".+ // this is a comment \n.-";
        let scanner = Scanner::new(text);
        assert_eq!(
            vec![
                Token::new(0, TokenType::Dot),
                Token::new(0, TokenType::Plus),
                Token::new(1, TokenType::Dot),
                Token::new(1, TokenType::Minus)
            ],
            scanner.collect::<Vec<_>>()
        );
    }

    #[test]
    fn parse_string() {
        let text = "\"string literal \n more literal\" .";
        let scanner = Scanner::new(text);
        assert_eq!(
            vec![
                Token::new(0, TokenType::String("string literal \n more literal")),
                Token::new(1, TokenType::Dot)
            ],
            scanner.collect::<Vec<_>>()
        );
    }

    #[test]
    fn parse_unterminated_string() {
        let text = "\"unterminated literal";
        let scanner = Scanner::new(text);
        assert_eq!(
            vec![Token::new(0, TokenType::Error(UNTERMINATED_ERROR))],
            scanner.collect::<Vec<_>>()
        );
    }

    #[test]
    fn parse_number() {
        let text = "45 12.34 765.";
        let scanner = Scanner::new(text);
        assert_eq!(
            vec![
                Token::new(0, TokenType::Number(45.0)),
                Token::new(0, TokenType::Number(12.34)),
                Token::new(0, TokenType::Number(765.0))
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
                Token::new(0, TokenType::And),
                Token::new(0, TokenType::Class),
                Token::new(0, TokenType::Else),
                Token::new(0, TokenType::False),
                Token::new(0, TokenType::For),
                Token::new(0, TokenType::Fun),
                Token::new(0, TokenType::If),
                Token::new(0, TokenType::Nil),
                Token::new(0, TokenType::Or),
                Token::new(0, TokenType::Print),
                Token::new(0, TokenType::Return),
                Token::new(0, TokenType::Super),
                Token::new(0, TokenType::This),
                Token::new(0, TokenType::True),
                Token::new(0, TokenType::Var),
                Token::new(0, TokenType::While)
            ],
            scanner.collect::<Vec<_>>()
        );
    }

    #[test]
    fn parse_identifier() {
        let text = "my_ident forb tr o";
        let scanner = Scanner::new(text);
        assert_eq!(
            vec![
                Token::new(0, TokenType::Identifier("my_ident")),
                Token::new(0, TokenType::Identifier("forb")),
                Token::new(0, TokenType::Identifier("tr")),
                Token::new(0, TokenType::Identifier("o"))
            ],
            scanner.collect::<Vec<_>>()
        );
    }

    #[test]
    fn numeric_and_identifier() {
        let text = "123.abc";
        let scanner = Scanner::new(text);
        assert_eq!(
            vec![
                Token::new(0, TokenType::Number(123.0)),
                Token::new(0, TokenType::Identifier("abc"))
            ],
            scanner.collect::<Vec<_>>()
        );
    }

    #[test]
    fn empty_string_returns_none() {
        let text = "";
        let scanner = Scanner::new(text);
        assert_eq!(Vec::<Token>::new(), scanner.collect::<Vec<Token>>());
    }

    #[test]
    fn only_whitespace_returns_none() {
        let text = "   \n";
        let scanner = Scanner::new(text);
        assert_eq!(Vec::<Token>::new(), scanner.collect::<Vec<Token>>());
    }
}
