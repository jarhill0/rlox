#[macro_use]
extern crate num_derive;

mod chunk;
mod common;
mod debug;
mod value;
mod vm;

use chunk::{Chunk, OpCode};

fn main() {
    let mut vm = vm::VM::new();
    let mut chunk = Chunk::new();
    let constant = chunk.add_constant(1.2) as u8;
    chunk.write(OpCode::Constant, 123);
    chunk.write(constant, 123);

    let constant = chunk.add_constant(3.4) as u8;
    chunk.write(OpCode::Constant, 123);
    chunk.write(constant, 123);

    chunk.write(OpCode::Add, 123);

    let constant = chunk.add_constant(5.6) as u8;
    chunk.write(OpCode::Constant, 123);
    chunk.write(constant, 123);

    chunk.write(OpCode::Divide, 123);

    chunk.write(OpCode::Negate, 123);
    chunk.write(OpCode::Return, 123);
    vm.interpret(chunk);
}
