use std::collections::HashMap;

use crate::chunk::{Chunk, OpCode};
use crate::common;
use crate::compiler;
use crate::debug;
use crate::object::Object;
use crate::value::Value;

pub struct VM {
    chunk: Option<Chunk>,
    pc: usize,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
}

impl<'a> VM {
    pub fn new() -> VM {
        VM {
            chunk: None,
            pc: 0,
            stack: vec![],
            globals: HashMap::new(),
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
        use OpCode::*;
        use Value::*;

        loop {
            if common::DEBUG_TRACE_EXECUTION {
                // Printing the stack.
                print!("          ");
                for val in self.stack.iter() {
                    print!("[ ");
                    val.print();
                    print!(" ]");
                }
                println!();

                debug::disassemble_instr(self.chunk(), self.pc);
            }

            let instruction = self.read_byte();
            let op_code = OpCode::from_u8(instruction);
            match op_code {
                Print => {
                    self.pop().print();
                    println!();
                }
                Return => {
                    return InterpretResult::Ok;
                }
                Constant => {
                    let constant = self.read_constant();
                    let constant = constant.clone();
                    self.push(constant);
                }
                OpCode::Nil => self.push(Value::Nil),
                True => self.push(Bool(true)),
                False => self.push(Bool(false)),
                Pop => {
                    self.pop();
                }
                GetGlobal => {
                    let name = self.read_string();
                    match self.globals.get(&name) {
                        None => {
                            self.runtime_error(&format!("Undefined variable '{}'.", name));
                            return InterpretResult::RuntimeError;
                        }
                        Some(value) => {
                            let value = value.clone();
                            self.push(value);
                        }
                    }
                }
                DefineGlobal => {
                    let name = self.read_string();
                    let value = self.pop();
                    self.globals.insert(name, value);
                }
                SetGlobal => {
                    let name = self.read_string();
                    let value = self.pop();
                    match self.globals.insert(name, value.clone()) {
                        Some(_) => (),
                        None => {
                            let name = self.read_string();
                            self.globals.remove(&name);
                            self.runtime_error(&format!("Undefined variable '{}'.", name));
                            return InterpretResult::RuntimeError;
                        }
                    }
                    self.push(value);
                }
                Equal => {
                    let (a, b) = self.pop_two();
                    self.push(Bool(a == b));
                }
                Greater => {
                    if let Some(result) = self.binary_num_op(|a, b| Bool(a > b)) {
                        return result;
                    }
                }
                Less => {
                    if let Some(result) = self.binary_num_op(|a, b| Bool(a < b)) {
                        return result;
                    }
                }
                Negate => {
                    let val = self.pop();
                    if let Number(val) = val {
                        self.push(Number(-val));
                    } else {
                        self.push(val);
                        self.runtime_error("Operand must be a number.");
                        return InterpretResult::RuntimeError;
                    }
                }
                Not => {
                    let val = self.pop();
                    self.push(Bool(val.is_falsey()));
                }
                Add => match self.pop_two() {
                    (Obj(Object::String(a)), Obj(Object::String(b))) => {
                        self.push(Obj(Object::String(concatenate(&a, &b))))
                    }
                    (Number(a), Number(b)) => self.push(Number(a + b)),
                    (a, b) => {
                        self.push_two(a, b);
                        self.runtime_error("Operands must be two numbers or two strings.");
                        return InterpretResult::RuntimeError;
                    }
                },
                Subtract => {
                    if let Some(result) = self.binary_num_op(|a, b| Number(a - b)) {
                        return result;
                    }
                }
                Multiply => {
                    if let Some(result) = self.binary_num_op(|a, b| Number(a * b)) {
                        return result;
                    }
                }
                Divide => {
                    if let Some(result) = self.binary_num_op(|a, b| Number(a / b)) {
                        return result;
                    }
                }
            };
        }
    }

    fn binary_num_op<T>(&mut self, operation: T) -> Option<InterpretResult>
    where
        T: Fn(f64, f64) -> Value,
    {
        let (a, b) = self.pop_two();
        let mut good_types = true;
        if let Value::Number(a) = a {
            if let Value::Number(b) = b {
                self.push(operation(a, b));
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

    fn read_constant(&mut self) -> &Value {
        let index = self.read_byte();
        self.chunk().get_value(index).expect("Needed constant")
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("Needed stack value.")
    }

    fn read_string(&mut self) -> String {
        match self.read_constant() {
            Value::Obj(object) => match object {
                Object::String(string) => string.clone(),
            },
            _ => panic!("Not an object."),
        }
    }
}

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}

fn concatenate(a: &str, b: &str) -> String {
    String::from(a) + b
}
