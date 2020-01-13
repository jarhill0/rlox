use crate::chunk::{self, OpCode};
use crate::value;

pub fn disassemble(chunk: &chunk::Chunk, name: &str) {
    println!("== {} ==", name);

    let mut offset = 0;
    while offset < chunk.len() {
        offset = disassemble_instr(&chunk, offset);
    }
}

fn get_op_code(chunk: &chunk::Chunk, offset: usize) -> chunk::OpCode {
    let op_code = chunk.get_at(offset).unwrap();
    chunk::OpCode::from_u8(*op_code)
}

pub fn disassemble_instr(chunk: &chunk::Chunk, offset: usize) -> usize {
    print!("{:04} ", offset);

    if offset > 0 && chunk.line_of(offset) == chunk.line_of(offset - 1) {
        print!("   | ");
    } else {
        print!("{:4} ", chunk.line_of(offset));
    }

    let op_code = get_op_code(chunk, offset);

    match op_code {
        OpCode::Return => simple_instruction("Return", offset),
        OpCode::Constant => constant_instruction("Constant", chunk, offset),
        OpCode::Negate => simple_instruction("Negate", offset),
        OpCode::Add => simple_instruction("Add", offset),
        OpCode::Subtract => simple_instruction("Subtract", offset),
        OpCode::Multiply => simple_instruction("Multiply", offset),
        OpCode::Divide => simple_instruction("Divide", offset),
        OpCode::Nil => simple_instruction("Nil", offset),
        OpCode::True => simple_instruction("True", offset),
        OpCode::False => simple_instruction("False", offset),
        OpCode::Not => simple_instruction("Not", offset),
        OpCode::Equal => simple_instruction("Equal", offset),
        OpCode::Less => simple_instruction("Less", offset),
        OpCode::Greater => simple_instruction("Greater", offset),
    }
}

fn simple_instruction(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}

fn constant_instruction(name: &str, chunk: &chunk::Chunk, offset: usize) -> usize {
    let constant = *chunk
        .get_at(offset + 1)
        .expect("Constant has one immediate.");
    print!("{:<16} {:4} '", name, constant);

    value::print(*chunk.get_value(constant).unwrap());
    println!("'");
    offset + 2
}
