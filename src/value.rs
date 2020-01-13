use std::cmp::PartialEq;

pub fn print(val: Value) {
    match val {
        Value::Nil => print!("nil"),
        Value::Bool(val) => print!("{}", val),
        Value::Number(val) => print!("{}", val),
    }
}

#[derive(Copy, Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
}

impl Value {
    pub fn new_bool(value: bool) -> Value {
        Value::Bool(value)
    }

    pub fn new_nil() -> Value {
        Value::Nil
    }

    pub fn new_number(value: f64) -> Value {
        Value::Number(value)
    }

    pub fn is_falsey(self) -> bool {
        match self {
            Value::Nil => true,
            Value::Bool(val) => !val,
            Value::Number(_) => false,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        // thanks! https://www.reddit.com/r/rust/comments/4fi8ka/_/d2902oa/
        use Value::*;
        match (self, other) {
            (Bool(a), Bool(b)) => a == b,
            (Nil, Nil) => true,
            (Number(a), Number(b)) => a == b,
            _ => false,
        }
    }
}
