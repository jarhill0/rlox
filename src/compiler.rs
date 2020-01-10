use crate::scanner::{Scanner, TokenType};

pub fn compile(source: &str) {
    let mut scanner = Scanner::new(source);
    let line = 0;
    loop {
        let token = scanner.scan_token();
        if token.line != line {
            print!("{:4} ", token.line);
        } else {
            print!("   | ");
        }
        println!("{:?} {}", token.kind, token.slice);

        if let TokenType::EOF = token.kind {
            break;
        }
    }
}
