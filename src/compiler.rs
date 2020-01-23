use std::cmp::PartialOrd;
use std::convert::TryInto;
use std::str::FromStr;

use crate::bits::from_u16;
use crate::chunk::{Byte, Chunk, OpCode};
use crate::common::DEBUG_PRINT_CODE;
use crate::debug::disassemble;
use crate::object::Object;
use crate::scanner::{Scanner, Token, TokenType};
use crate::value::Value;

pub fn compile<'a>(source: &'a str, chunk: &mut Chunk) -> bool {
    let scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner, chunk);
    parser.advance();
    parser.parse();
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
    compiler: Compiler<'a>,
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
            compiler: Compiler::new(),
        }
    }

    fn matches(&mut self, kind: TokenType) -> bool {
        if !self.check(kind) {
            false
        } else {
            self.advance();
            true
        }
    }

    fn check(&self, kind: TokenType) -> bool {
        self.current().kind == kind
    }

    fn parse(&mut self) {
        while !self.matches(TokenType::EOF) {
            self.declaration();
        }
    }

    fn declaration(&mut self) {
        if self.matches(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn statement(&mut self) {
        if self.matches(TokenType::Print) {
            self.print_statement();
        } else if self.matches(TokenType::If) {
            self.if_statement();
        } else if self.matches(TokenType::While) {
            self.while_statement();
        } else if self.matches(TokenType::For) {
            self.for_statement();
        } else if self.matches(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let then_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit_byte(OpCode::Pop); // if cond is true, pop it!
        self.statement();

        let else_jump = self.emit_jump(OpCode::Jump);
        self.patch_jump(then_jump);

        self.emit_byte(OpCode::Pop); // if cond was false, pop it where we jump

        if self.matches(TokenType::Else) {
            self.statement();
        }
        self.patch_jump(else_jump);
    }

    fn while_statement(&mut self) {
        let loop_start = self.current_chunk().len();

        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse);

        self.emit_byte(OpCode::Pop);
        self.statement();

        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_byte(OpCode::Pop);
    }

    fn for_statement(&mut self) {
        self.begin_scope();

        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");
        if self.matches(TokenType::Semicolon) {
            // no initializer
        } else if self.matches(TokenType::Var) {
            self.var_declaration();
        } else {
            self.expression_statement();
        }

        let mut loop_start = self.current_chunk().len();

        let mut exit_jump = None;
        if !self.matches(TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition.");

            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse));
            self.emit_byte(OpCode::Pop); // removes condition
        }

        if !self.matches(TokenType::RightParen) {
            // We jump right into the body, only to jump back later for the
            // increment. Yes, it's weird.
            let body_jump = self.emit_jump(OpCode::Jump);

            let increment_start = self.current_chunk().len();
            self.expression();
            self.emit_byte(OpCode::Pop);
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement();

        self.emit_loop(loop_start);

        match exit_jump {
            Some(exit_jump) => {
                self.patch_jump(exit_jump);
                self.emit_byte(OpCode::Pop);
            }
            None => (),
        }

        self.end_scope();
    }

    fn block(&mut self) {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::EOF) {
            self.declaration();
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.");
    }

    fn begin_scope(&mut self) {
        self.compiler.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.compiler.scope_depth -= 1;

        let drop_count = self
            .compiler
            .locals
            .iter()
            .rev()
            .take_while(|local| local.depth > self.compiler.scope_depth)
            .count();
        self.compiler
            .locals
            .truncate(self.compiler.locals.len() - drop_count);
        for _ in 0..drop_count {
            self.emit_byte(OpCode::Pop);
        }
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self.matches(TokenType::Equal) {
            self.expression();
        } else {
            self.emit_byte(OpCode::Nil);
        }
        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        );

        self.define_variable(global);
    }

    fn parse_variable(&mut self, error_message: &str) -> u8 {
        self.consume(TokenType::Identifier, error_message);

        self.declare_variable();
        if self.compiler.local_scope() {
            // locals have different lookup
            return 0; // doesn't matter
        }

        self.identifier_constant(&self.previous())
    }

    fn define_variable(&mut self, global: u8) {
        if self.compiler.local_scope() {
            self.compiler.mark_top_initialized();
            return;
        }
        self.emit_bytes(OpCode::DefineGlobal, global);
    }

    fn and(&mut self, _can_assign: bool) {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse);

        self.emit_byte(OpCode::Pop);
        self.parse_precedence(Precedence::And);

        self.patch_jump(end_jump);
    }

    fn or(&mut self, _can_assign: bool) {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse);
        let end_jump = self.emit_jump(OpCode::Jump);

        self.patch_jump(else_jump);
        self.emit_byte(OpCode::Pop);

        self.parse_precedence(Precedence::Or);
        self.patch_jump(end_jump);
    }

    fn identifier_constant(&mut self, name: &Token) -> u8 {
        self.make_constant(Value::Obj(Object::String(String::from(name.slice))))
    }

    fn declare_variable(&mut self) {
        // Globals implicitly declared
        if self.compiler.global_scope() {
            return;
        }

        let name = self.previous().slice;
        let mut had_error = false;
        for _ in self
            .compiler
            .locals
            .iter()
            .rev()
            .take_while(|local| !(local.initialized && local.depth < self.compiler.scope_depth))
            .filter(|local| name == local.name)
        {
            had_error = true;
        }
        if had_error {
            self.error("Variable with this name already declared in this scope.");
        }

        self.add_local(name);
    }

    fn add_local(&mut self, name: &'a str) {
        if self.compiler.count() > std::u8::MAX as usize {
            self.error("Too many local variables in function.");
            return;
        }

        self.compiler
            .locals
            .push(Local::new(name, self.compiler.scope_depth));
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.emit_byte(OpCode::Pop);
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit_byte(OpCode::Print);
    }

    fn synchronize(&mut self) {
        use TokenType::*;

        self.panic_mode = false;

        while self.current().kind != EOF {
            if self.previous().kind == Semicolon {
                return;
            }

            match self.current().kind {
                Class | Fun | Var | For | If | While | Print | Return => return,
                _ => (),
            }

            self.advance();
        }
    }

    fn number(&mut self, _can_assign: bool) {
        // it will parse because it was validated in the parser.
        let value = f64::from_str(self.previous().slice).unwrap();
        self.emit_constant(Value::Number(value));
    }

    fn string(&mut self, _can_assign: bool) {
        let slice = self.previous().slice;
        self.emit_constant(Value::Obj(Object::String(String::from(
            &slice[1..slice.len() - 1],
        ))));
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(self.previous(), can_assign);
    }

    fn named_variable(&mut self, name: Token, can_assign: bool) {
        let (result, error) = self.compiler.resolve_local(&name);
        match error {
            Some(message) => self.error(message),
            None => (),
        }
        let (get_op, set_op, arg) = match result {
            Some(arg) => (OpCode::GetLocal, OpCode::SetLocal, arg),
            None => (
                OpCode::GetGlobal,
                OpCode::SetGlobal,
                self.identifier_constant(&name),
            ),
        };

        if can_assign && self.matches(TokenType::Equal) {
            self.expression();
            self.emit_bytes(set_op, arg as u8);
        } else {
            self.emit_bytes(get_op, arg as u8);
        }
    }

    fn unary(&mut self, _can_assign: bool) {
        let operator_type = self.previous().kind;

        self.parse_precedence(Precedence::Unary);

        match operator_type {
            TokenType::Minus => self.emit_byte(OpCode::Negate),
            TokenType::Bang => self.emit_byte(OpCode::Not),
            _ => panic!("unreachable"),
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let prefix_rule = get_rule(self.previous().kind).prefix;
        let can_assign = precedence <= Precedence::Assignment;
        match prefix_rule {
            None => {
                self.error("Expect expression.");
                return;
            }
            Some(callable) => callable(self, can_assign),
        }

        while precedence <= get_rule(self.current().kind).precedence {
            self.advance();
            let infix_rule = get_rule(self.previous().kind).infix.unwrap();
            infix_rule(self, can_assign);
        }

        if can_assign && self.matches(TokenType::Equal) {
            self.error("Invalid assignment target.");
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

    fn emit_jump(&mut self, instruction: impl Byte) -> usize {
        self.emit_byte(instruction);
        self.emit_bytes(0xff, 0xff);
        self.current_chunk().len() - 2
    }

    fn patch_jump(&mut self, offset: usize) {
        // -2 accounts for storing the jump offset
        let jump = self.current_chunk().len() - offset - 2;

        if jump > std::u16::MAX as usize {
            self.error("Too much code to jump over.");
        }

        let (high, low) = from_u16(jump as u16);
        self.current_chunk_mut().set_at(offset, high);
        self.current_chunk_mut().set_at(offset + 1, low);
    }

    fn emit_loop(&mut self, loop_start: usize) {
        self.emit_byte(OpCode::Loop);

        let offset = self.current_chunk().len() - loop_start + 2;
        let offset: Result<u16, _> = offset.try_into();
        match offset {
            Err(_) => self.error("Loop body too large."),
            Ok(offset) => {
                let (high, low) = from_u16(offset);
                self.emit_bytes(high, low)
            }
        }
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
        if DEBUG_PRINT_CODE && !self.had_error {
            disassemble(self.current_chunk(), "code");
        }
    }

    fn binary(&mut self, _can_assign: bool) {
        use OpCode::*;
        use TokenType::*;

        let operator_type = self.previous().kind;

        let rule = get_rule(operator_type);
        self.parse_precedence(rule.precedence.next_highest());

        match operator_type {
            Plus => self.emit_byte(Add),
            Minus => self.emit_byte(Subtract),
            Star => self.emit_byte(Multiply),
            Slash => self.emit_byte(Divide),
            BangEqual => self.emit_bytes(OpCode::Equal, Not),
            EqualEqual => self.emit_byte(OpCode::Equal),
            TokenType::Greater => self.emit_byte(OpCode::Greater),
            GreaterEqual => self.emit_bytes(OpCode::Less, Not),
            TokenType::Less => self.emit_byte(OpCode::Less),
            LessEqual => self.emit_bytes(OpCode::Greater, Not),
            _ => panic!("unreachable"),
        }
    }

    fn literal(&mut self, _can_assign: bool) {
        match self.previous().kind {
            TokenType::False => self.emit_byte(OpCode::False),
            TokenType::Nil => self.emit_byte(OpCode::Nil),
            TokenType::True => self.emit_byte(OpCode::True),
            _ => panic!("unreachable"),
        }
    }

    fn grouping(&mut self, _can_assign: bool) {
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

struct Compiler<'a> {
    locals: Vec<Local<'a>>,
    scope_depth: usize,
}

impl<'a> Compiler<'_> {
    fn new() -> Compiler<'a> {
        Compiler {
            locals: vec![],
            scope_depth: 0,
        }
    }

    fn global_scope(&self) -> bool {
        self.scope_depth == 0
    }

    fn local_scope(&self) -> bool {
        !self.global_scope()
    }

    fn count(&self) -> usize {
        self.locals.len()
    }

    fn resolve_local(&self, name: &Token) -> (Option<u8>, Option<&'static str>) {
        match self
            .locals
            .iter()
            .rev()
            .enumerate()
            .filter(|(_, local)| local.name == name.slice)
            .next()
        {
            Some((i, local)) => {
                let error = if local.initialized {
                    None
                } else {
                    Some("Cannot read local variable in its own initializer.")
                };
                (Some(i.try_into().unwrap()), error)
            }
            None => (None, None),
        }
    }

    fn mark_top_initialized(&mut self) {
        let top = self.locals.len() - 1;
        let top = self.locals.get_mut(top).unwrap();
        top.initialized = true;
    }
}

#[derive(Copy, Clone)]
struct Local<'a> {
    name: &'a str,
    depth: usize,
    initialized: bool,
}

impl<'a> Local<'_> {
    fn new(name: &'a str, depth: usize) -> Local<'a> {
        Local {
            name,
            depth,
            initialized: false,
        }
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
    fn next_highest(self) -> Precedence {
        use Precedence::*;

        match self {
            Precedence::None => Assignment,
            Assignment => Or,
            Or => And,
            And => Equality,
            Equality => Comparison,
            Comparison => Term,
            Term => Factor,
            Factor => Unary,
            Unary => Call,
            Call => Primary,
            Primary => Primary,
        }
    }
}

type ParseFn<'a> = fn(&mut Parser<'a>, bool);

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
            prefix: Some(Parser::unary),
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::BangEqual => ParseRule {
            prefix: None,
            infix: Some(Parser::binary),
            precedence: Precedence::Equality,
        },
        TokenType::Equal => ParseRule {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::EqualEqual => ParseRule {
            prefix: None,
            infix: Some(Parser::binary),
            precedence: Precedence::Equality,
        },
        TokenType::Greater => ParseRule {
            prefix: None,
            infix: Some(Parser::binary),
            precedence: Precedence::Comparison,
        },
        TokenType::GreaterEqual => ParseRule {
            prefix: None,
            infix: Some(Parser::binary),
            precedence: Precedence::Comparison,
        },
        TokenType::Less => ParseRule {
            prefix: None,
            infix: Some(Parser::binary),
            precedence: Precedence::Comparison,
        },
        TokenType::LessEqual => ParseRule {
            prefix: None,
            infix: Some(Parser::binary),
            precedence: Precedence::Comparison,
        },
        TokenType::Identifier => ParseRule {
            prefix: Some(Parser::variable),
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::String => ParseRule {
            prefix: Some(Parser::string),
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
            infix: Some(Parser::and),
            precedence: Precedence::And,
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
            prefix: Some(Parser::literal),
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
            prefix: Some(Parser::literal),
            infix: None,
            precedence: Precedence::None,
        },
        TokenType::Or => ParseRule {
            prefix: None,
            infix: Some(Parser::or),
            precedence: Precedence::Or,
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
            prefix: Some(Parser::literal),
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
