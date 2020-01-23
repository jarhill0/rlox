use std::cmp::PartialEq;

use crate::object::Object;

#[derive(Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    Obj(Object),
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Nil => true,
            Value::Bool(val) => !val,
            Value::Number(_) => false,
            Value::Obj(_) => false,
        }
    }

    pub fn print(&self) {
        match self {
            Value::Nil => print!("nil"),
            Value::Bool(val) => print!("{}", val),
            Value::Number(val) => print!("{}", val),
            Value::Obj(obj) => obj.print(),
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
            (Obj(a), Obj(b)) => a == b,
            _ => false,
        }
    }
}
