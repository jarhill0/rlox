use std::collections::HashMap;
use std::iter;
use std::str;

pub struct Scanner<'a> {
    source: iter::Peekable<str::Chars<'a>>,
    source_str: &'a str,
    start: usize,
    current: usize,
    len: usize,
    line: u64,
    keywords: HashMap<&'static str, TokenType>,
}

impl Scanner<'_> {
    pub fn new(source: &str) -> Scanner {
        Scanner {
            source_str: source,
            source: source.chars().peekable(),
            current: 0,
            start: 0,
            line: 1,
            len: source.len(),
            keywords: [
                ("and", TokenType::And),
                ("class", TokenType::Class),
                ("else", TokenType::Else),
                ("false", TokenType::False),
                ("for", TokenType::For),
                ("fun", TokenType::Fun),
                ("if", TokenType::If),
                ("nil", TokenType::Nil),
                ("or", TokenType::Or),
                ("print", TokenType::Print),
                ("return", TokenType::Return),
                ("super", TokenType::Super),
                ("this", TokenType::This),
                ("true", TokenType::True),
                ("var", TokenType::Var),
                ("while", TokenType::While),
            ]
            .iter()
            .cloned()
            .collect(),
        }
    }

    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();

        self.start = self.current;
        if self.at_end() {
            return self.make_token(TokenType::EOF);
        }

        let c = self.advance();

        if alpha(c) {
            return self.identifier();
        }
        if digit(c) {
            return self.number();
        }

        match c {
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            ';' => self.make_token(TokenType::Semicolon),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '+' => self.make_token(TokenType::Plus),
            '-' => self.make_token(TokenType::Minus),
            '/' => self.make_token(TokenType::Slash),
            '*' => self.make_token(TokenType::Star),
            '!' => {
                let kind = if self.matched('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                self.make_token(kind)
            }
            '=' => {
                let kind = if self.matched('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                self.make_token(kind)
            }
            '<' => {
                let kind = if self.matched('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.make_token(kind)
            }
            '>' => {
                let kind = if self.matched('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.make_token(kind)
            }
            '"' => self.string(),
            _ => self.error_token("Unexpected character."),
        }
    }

    fn identifier(&mut self) -> Token {
        while alpha(self.peek()) || digit(self.peek()) {
            self.advance();
        }
        self.make_token(self.identifier_type())
    }

    fn identifier_type(&self) -> TokenType {
        match self.keywords.get(self.token_slice()) {
            Some(val) => *val,
            None => TokenType::Identifier,
        }
    }

    fn number(&mut self) -> Token {
        while digit(self.peek()) {
            self.advance();
        }

        if self.peek() == '.' && digit(self.peek_next()) {
            self.advance();

            while digit(self.peek()) {
                self.advance();
            }
        }

        self.make_token(TokenType::Number)
    }

    fn string(&mut self) -> Token {
        while self.peek() != '"' && !self.at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.at_end() {
            return self.error_token("Unterminated string.");
        }

        self.advance(); // close quote.
        self.make_token(TokenType::String)
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.advance();
                    self.line += 1;
                }
                '/' => {
                    if self.peek_next() == '/' {
                        while self.peek() != '\n' && !self.at_end() {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }
                _ => return,
            }
        }
    }

    fn at_end(&self) -> bool {
        self.current == self.len
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source.next().unwrap()
    }

    fn matched(&mut self, expected: char) -> bool {
        if self.at_end() {
            return false;
        }
        if self.peek() != expected {
            return false;
        }
        self.advance();
        true
    }

    fn peek(&mut self) -> char {
        *self.source.peek().unwrap_or(&'\0')
    }

    fn peek_next(&self) -> char {
        // A dirty hack — creating a new iterator just for this.
        // Horrifically inefficient -- this is O(n) for what should be O(1).
        self.source_str
            .chars()
            .nth(self.current + 1)
            .unwrap_or('\0')
    }

    fn make_token(&self, kind: TokenType) -> Token {
        Token {
            kind,
            slice: self.token_slice(),
            line: self.line,
        }
    }

    fn token_slice(&self) -> &str {
        &self.source_str[self.start..self.current]
    }

    fn error_token(&self, message: &'static str) -> Token {
        Token {
            kind: TokenType::Error,
            slice: message,
            line: self.line,
        }
    }
}

fn digit(c: char) -> bool {
    c >= '0' && c <= '9'
}

fn alpha(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

pub struct Token<'a> {
    pub kind: TokenType,
    pub slice: &'a str,
    pub line: u64,
}

#[derive(Debug, Clone, Copy)]
pub enum TokenType {
    // Single-character tokens.
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

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
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

    Error,
    EOF,
}
