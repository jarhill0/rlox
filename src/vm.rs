use crate::chunk::{Chunk, OpCode};
use crate::common;
use crate::debug;
use crate::value::{self, Value};

pub struct VM {
    chunk: Option<Chunk>,
    pc: usize,
    stack: Vec<Value>,
}

impl VM {
    pub fn new() -> VM {
        VM {
            chunk: None,
            pc: 0,
            stack: vec![],
        }
    }

    pub fn interpret(&mut self, chunk: Chunk) -> InterpretResult {
        self.chunk = Some(chunk);
        self.run()
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            if common::DEBUG_TRACE_EXECUTION {
                // Printing the stack.
                print!("          ");
                for val in self.stack.iter() {
                    print!("[ ");
                    value::print(*val);
                    print!(" ]");
                }
                println!("");

                debug::disassemble_instr(self.chunk(), self.pc);
            }

            let instruction = self.read_byte();
            let op_code = OpCode::from_u8(instruction);
            match op_code {
                OpCode::Return => {
                    value::print(self.pop());
                    println!("");
                    return InterpretResult::Ok;
                }
                OpCode::Constant => {
                    let constant = self.read_constant();
                    self.push(constant);
                }
                OpCode::Negate => {
                    let neg = -self.pop();
                    self.push(neg);
                }
                OpCode::Add => {
                    let (a, b) = self.pop_two();
                    self.push(a + b);
                }
                OpCode::Subtract => {
                    let (a, b) = self.pop_two();
                    self.push(a - b);
                }
                OpCode::Multiply => {
                    let (a, b) = self.pop_two();
                    self.push(a * b);
                }
                OpCode::Divide => {
                    let (a, b) = self.pop_two();
                    self.push(a / b);
                }
            };
        }
    }

    fn pop_two(&mut self) -> (Value, Value) {
        let b = self.pop();
        let a = self.pop();
        (a, b)
    }

    fn chunk(&self) -> &Chunk {
        self.chunk.as_ref().unwrap()
    }

    fn read_byte(&mut self) -> u8 {
        self.pc += 1;
        *self.chunk().get_at(self.pc - 1).unwrap()
    }

    fn read_constant(&mut self) -> Value {
        let index = self.read_byte();
        *self.chunk().get_value(index).expect("Needed constant")
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Needed stack value.")
    }
}

pub enum InterpretResult {
    Ok,
    // CompileError,
    // RuntimeError,
}
