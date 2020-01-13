use crate::chunk::{Chunk, OpCode};
use crate::common;
use crate::compiler;
use crate::debug;
use crate::value::{self, Value};

pub struct VM {
    chunk: Option<Chunk>,
    pc: usize,
    stack: Vec<Value>,
}

impl<'a> VM {
    pub fn new() -> VM {
        VM {
            chunk: None,
            pc: 0,
            stack: vec![],
        }
    }

    pub fn interpret(&mut self, source: &'a str) -> InterpretResult {
        let mut chunk = Chunk::new();

        if !compiler::compile(source, &mut chunk) {
            return InterpretResult::CompileError;
        }

        self.chunk = Some(chunk);
        self.pc = 0;

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
                println!();

                debug::disassemble_instr(self.chunk(), self.pc);
            }

            let instruction = self.read_byte();
            let op_code = OpCode::from_u8(instruction);
            match op_code {
                OpCode::Return => {
                    value::print(self.pop());
                    println!();
                    return InterpretResult::Ok;
                }
                OpCode::Constant => {
                    let constant = self.read_constant();
                    self.push(constant);
                }
                OpCode::Nil => self.push(Value::new_nil()),
                OpCode::True => self.push(Value::new_bool(true)),
                OpCode::False => self.push(Value::new_bool(false)),
                OpCode::Equal => {
                    let (a, b) = self.pop_two();
                    self.push(Value::new_bool(a == b));
                }
                OpCode::Greater => {
                    if let Some(result) = self.binary_num_op(Value::new_bool, |a, b| a > b) {
                        return result;
                    }
                }
                OpCode::Less => {
                    if let Some(result) = self.binary_num_op(Value::new_bool, |a, b| a < b) {
                        return result;
                    }
                }
                OpCode::Negate => {
                    let val = self.pop();
                    if let Value::Number(val) = val {
                        self.push(Value::new_number(-val));
                    } else {
                        self.push(val);
                        self.runtime_error("Operand must be a number.");
                        return InterpretResult::RuntimeError;
                    }
                }
                OpCode::Not => {
                    let val = self.pop();
                    self.push(Value::new_bool(val.is_falsey()));
                }
                OpCode::Add => {
                    if let Some(result) = self.binary_num_op(Value::new_number, |a, b| a + b) {
                        return result;
                    }
                }
                OpCode::Subtract => {
                    if let Some(result) = self.binary_num_op(Value::new_number, |a, b| a - b) {
                        return result;
                    }
                }
                OpCode::Multiply => {
                    if let Some(result) = self.binary_num_op(Value::new_number, |a, b| a * b) {
                        return result;
                    }
                }
                OpCode::Divide => {
                    if let Some(result) = self.binary_num_op(Value::new_number, |a, b| a / b) {
                        return result;
                    }
                }
            };
        }
    }

    fn binary_num_op<R, S, T>(&mut self, constructor: S, operation: T) -> Option<InterpretResult>
    where
        S: Fn(R) -> Value,
        T: Fn(f64, f64) -> R,
    {
        let (a, b) = self.pop_two();
        let mut good_types = true;
        if let Value::Number(a) = a {
            if let Value::Number(b) = b {
                self.push(constructor(operation(a, b)));
            } else {
                good_types = false
            }
        } else {
            good_types = false
        }
        if !good_types {
            self.push_two(a, b);
            self.runtime_error("Operands must be numbers.");
            Some(InterpretResult::RuntimeError)
        } else {
            None
        }
    }

    fn reset_stack(&mut self) {
        self.stack.clear();
    }

    fn runtime_error(&mut self, msg: &str) {
        eprintln!("{}", msg);

        let instruction = self.pc - 1;
        let line = self.chunk().line_of(instruction);
        eprintln!("[line {}] in script\n", line);

        self.reset_stack();
    }

    fn pop_two(&mut self) -> (Value, Value) {
        let b = self.pop();
        let a = self.pop();
        (a, b)
    }

    fn push_two(&mut self, a: Value, b: Value) {
        self.push(a);
        self.push(b);
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
    CompileError,
    RuntimeError,
}
