use std::cmp::PartialOrd;
use std::str::FromStr;

use crate::chunk::{Byte, Chunk, OpCode};
use crate::common::DEBUG_PRINT_CODE;
use crate::debug::disassemble;
use crate::scanner::{Scanner, Token, TokenType};
use crate::value::Value;

pub fn compile<'a>(source: &'a str, chunk: &mut Chunk) -> bool {
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner, chunk);
    parser.advance();
    parser.expression();
    parser.consume(TokenType::EOF, "Expect end of expression.");
    parser.end();
    !parser.had_error
}

struct Parser<'a> {
    current: Option<Token<'a>>,
    previous: Option<Token<'a>>,
    scanner: Scanner<'a>,
    compiling_chunk: &'a mut Chunk,
    had_error: bool,
    panic_mode: bool,
}

impl<'a> Parser<'a> {
    fn new(scanner: Scanner<'a>, chunk: &'a mut Chunk) -> Parser<'a> {
        Parser {
            scanner,
            compiling_chunk: chunk,
            current: None,
            previous: None,
            had_error: false,
            panic_mode: false,
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn number(&mut self) {
        // it will parse because it was validated in the parser.
        let value = f64::from_str(self.previous().slice).unwrap();
        self.emit_constant(value);
    }

    fn unary(&mut self) {
        let operator_type = self.previous().kind;

        self.parse_precedence(Precedence::Unary);

        match operator_type {
            TokenType::Minus => self.emit_byte(OpCode::Negate),
            _ => (),
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let prefix_rule = get_rule(self.previous().kind).prefix;
        match prefix_rule {
            None => {
                self.error("Expect expression.");
                return;
            }
            Some(callable) => callable(self),
        }

        while precedence <= get_rule(self.current().kind).precedence {
            self.advance();
            let infix_rule = get_rule(self.previous().kind).infix.unwrap();
            infix_rule(self);
        }
    }

    fn consume(&mut self, kind: TokenType, err: &str) {
        if self.current().kind == kind {
            self.advance();
            return;
        }

        self.error_at_current(err);
    }

    fn advance(&mut self) {
        self.previous = self.current;
        loop {
            let token = self.scanner.scan_token();
            self.current = Some(token);

            match self.current().kind {
                TokenType::Error => (),
                _ => break,
            };

            self.error_at_current(self.current().slice);
        }
    }

    fn emit_byte<T: Byte>(&mut self, byte: T) {
        let line = self.previous().line;
        self.current_chunk_mut().write(byte, line);
    }

    fn emit_bytes<T: Byte, S: Byte>(&mut self, byte1: T, byte2: S) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_constant(&mut self, value: Value) {
        let constant = self.make_constant(value);
        self.emit_bytes(OpCode::Constant, constant);
    }

    fn make_constant(&mut self, value: Value) -> u8 {
        let constant = self.current_chunk_mut().add_constant(value);
        if constant > std::u8::MAX as usize {
            self.error("Too many constants in one chunk.");
            0
        } else {
            constant as u8
        }
    }

    fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.compiling_chunk
    }

    fn current_chunk(&self) -> &Chunk {
        &self.compiling_chunk
    }

    fn end(&mut self) {
        self.emit_return();
        if DEBUG_PRINT_CODE {
            if !self.had_error {
                disassemble(self.current_chunk(), "code");
            }
        }
    }

    fn binary(&mut self) {
        let operator_type = self.previous().kind;
        
        let rule = get_rule(operator_type);
        self.parse_precedence(rule.precedence.next_highest());

        match operator_type {
            TokenType::Plus => self.emit_byte(OpCode::Add),
            TokenType::Minus => self.emit_byte(OpCode::Subtract),
            TokenType::Star => self.emit_byte(OpCode::Multiply),
            TokenType::Slash => self.emit_byte(OpCode::Divide),
            _ => (),
        }
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::Return);
    }

    fn current(&self) -> Token<'a> {
        self.current.expect("Expected current token.")
    }

    fn previous(&self) -> Token<'a> {
        self.previous.expect("Expected previous token.")
    }

    fn error_at_current(&mut self, message: &str) {
        self.error_at(&self.current(), message);
    }

    fn error(&mut self, message: &str) {
        self.error_at(&self.previous(), message);
    }

    fn error_at(&mut self, token: &Token, message: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;

        eprint!("[line {}] Error", token.line);

        if let TokenType::EOF = token.kind {
            eprint!(" at end");
        } else if let TokenType::Error = token.kind {
        } else {
            eprint!(" at '{}'", token.slice);
        }

        eprintln!(": {}", message);
        self.had_error = true;
    }
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    fn next_highest(&self) -> Precedence {
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
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => Precedence::Primary,
        }
    }
}

type ParseFn<'a> = fn(&mut Parser<'a>);

struct ParseRule<'a> {
    prefix: Option<ParseFn<'a>>,
    infix: Option<ParseFn<'a>>,
    precedence: Precedence,
}

fn get_rule<'a>(kind: TokenType) -> ParseRule<'a> {
    match kind {
        TokenType::LeftParen => ParseRule {
            prefix: Some(Parser::grouping),
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::RightParen => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::LeftBrace => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::RightBrace => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::Comma => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::Dot => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::Minus => ParseRule {
            prefix: Some(Parser::unary),
            infix: Some(Parser::binary),
            precedence: Precedence::Term,
        },
        TokenType::Plus => ParseRule {
            prefix: None,
            infix: Some(Parser::binary),
            precedence: Precedence::Term,
        },
        TokenType::Semicolon => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::Slash => ParseRule {
            prefix: None,
            infix: Some(Parser::binary),
            precedence: Precedence::Factor,
        },
        TokenType::Star => ParseRule {
            prefix: None,
            infix: Some(Parser::binary),
            precedence: Precedence::Factor,
        },
        TokenType::Bang => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::BangEqual => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::Equal => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::EqualEqual => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::Greater => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::GreaterEqual => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::Less => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::LessEqual => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::Identifier => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::String => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::Number => ParseRule {
            prefix: Some(Parser::number),
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::And => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::Class => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::Else => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::False => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::For => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::Fun => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::If => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::Nil => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::Or => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::Print => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::Return => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::Super => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::This => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::True => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::Var => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::While => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::Error => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::EOF => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
    }
}
