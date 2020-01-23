use crate::bits::to_u16;
use crate::chunk::{self, Chunk, OpCode};

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
    use OpCode::*;

    print!("{:04} ", offset);

    if offset > 0 && chunk.line_of(offset) == chunk.line_of(offset - 1) {
        print!("   | ");
    } else {
        print!("{:4} ", chunk.line_of(offset));
    }

    let op_code = get_op_code(chunk, offset);

    match op_code {
        Return => simple_instruction("Return", offset),
        Constant => constant_instruction("Constant", chunk, offset),
        Negate => simple_instruction("Negate", offset),
        Add => simple_instruction("Add", offset),
        Subtract => simple_instruction("Subtract", offset),
        Multiply => simple_instruction("Multiply", offset),
        Divide => simple_instruction("Divide", offset),
        Nil => simple_instruction("Nil", offset),
        True => simple_instruction("True", offset),
        False => simple_instruction("False", offset),
        Not => simple_instruction("Not", offset),
        Equal => simple_instruction("Equal", offset),
        Less => simple_instruction("Less", offset),
        Greater => simple_instruction("Greater", offset),
        Print => simple_instruction("Print", offset),
        Pop => simple_instruction("Pop", offset),
        DefineGlobal => constant_instruction("DefineGlobal", chunk, offset),
        GetGlobal => constant_instruction("GetGlobal", chunk, offset),
        SetGlobal => constant_instruction("SetGlobal", chunk, offset),
        GetLocal => byte_instruction("GetLocal", chunk, offset),
        SetLocal => byte_instruction("SetLocal", chunk, offset),
        Jump => jump_instruction("Jump", 1, chunk, offset),
        JumpIfFalse => jump_instruction("JumpIfFalse", 1, chunk, offset),
        Loop => jump_instruction("Loop", -1, chunk, offset),
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

    chunk.get_value(constant).unwrap().print();
    println!("'");
    offset + 2
}

fn byte_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let slot = chunk
        .get_at(offset + 1)
        .expect("Byte instr has one immediate.");
    println!("{:<16} {:4}", name, slot);
    offset + 2
}

fn jump_instruction(name: &str, sign: isize, chunk: &Chunk, offset: usize) -> usize {
    let jump = to_u16(
        *chunk
            .get_at(offset + 1)
            .expect("jump instruction has first immediate"),
        *chunk
            .get_at(offset + 2)
            .expect("jump instruction has second immediate"),
    );
    println!(
        "{:<16} {:4} -> {}",
        name,
        offset,
        (offset as isize) + 3 + sign * jump as isize
    );
    offset + 3
}
