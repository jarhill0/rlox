use crate::chunk;

pub fn disassemble(chunk: &chunk::Chunk, name: &str) {
    println!("== {} ==", name);

    let mut offset = 0;
    while offset < chunk.len() {
        disassemble_instr(&chunk, &mut offset);
    }
}

fn get_op_code(chunk: &chunk::Chunk, offset: usize) -> chunk::OpCode {
    let op_code = chunk.get_at(offset).unwrap();
    chunk::OpCode::from_u8(*op_code)
}

fn disassemble_instr(chunk: &chunk::Chunk, offset: &mut usize) {
    print!("{:04} ", offset);

    {
        let offset = *offset;

        if offset > 0 && chunk.line_of(offset) == chunk.line_of(offset - 1) {
            print!("   | ");
        } else {
            print!("{:4} ", chunk.line_of(offset));
        }
    }

    let op_code = get_op_code(chunk, *offset);

    *offset = match op_code {
        chunk::OpCode::Return => simple_instruction("Return", *offset),
        chunk::OpCode::Constant => constant_instruction("Constant", chunk, *offset),
    };
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
