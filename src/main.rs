#[macro_use]
extern crate num_derive;

mod chunk;
mod debug;
mod value;

use chunk::{Chunk, OpCode};

fn main() {
    let mut chunk = Chunk::new();
    let constant = chunk.add_constant(value::Value::new(1.2)) as u8;
    chunk.write_opcode(OpCode::Constant, 123);
    chunk.write_byte(constant, 123);
    chunk.write_opcode(OpCode::Return, 123);
    debug::disassemble(&chunk, "test chunk");
}
