use std::cmp::PartialEq;

#[derive(Copy, Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
}

impl Value {
    pub fn is_falsey(self) -> bool {
        match self {
            Value::Nil => true,
            Value::Bool(val) => !val,
            Value::Number(_) => false,
        }
    }

    pub fn print(&self) {
        match self {
            Value::Nil => print!("nil"),
            Value::Bool(val) => print!("{}", val),
            Value::Number(val) => print!("{}", val),
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
