use crate::value;

use num;

#[derive(FromPrimitive, ToPrimitive)]
pub enum OpCode {
    Return,
    Constant,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl OpCode {
    pub fn from_u8(n: u8) -> OpCode {
        num::FromPrimitive::from_u8(n).expect("Invalid OpCode value.")
    }
}

impl Byte for OpCode {
    fn byte(&self) -> u8 {
        num::ToPrimitive::to_u8(self).unwrap()
    }
}

pub trait Byte {
    fn byte(&self) -> u8;
}

impl Byte for u8 {
    fn byte(&self) -> u8 {
        *self
    }
}

pub struct Chunk {
    codes: Vec<u8>,
    constants: Vec<value::Value>,
    lines: Vec<u64>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            codes: vec![],
            constants: vec![],
            lines: vec![],
        }
    }

    pub fn write<T: Byte>(&mut self, byte: T, line: u64) {
        self.codes.push(byte.byte());
        self.lines.push(line);
    }

    pub fn add_constant(&mut self, value: value::Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn len(&self) -> usize {
        self.codes.len()
    }

    pub fn get_at(&self, ind: usize) -> Option<&u8> {
        self.codes.get(ind)
    }

    pub fn line_of(&self, ind: usize) -> u64 {
        match self.lines.get(ind) {
            Some(val) => *val,
            None => 0,
        }
    }

    pub fn get_value(&self, ind: u8) -> Option<&value::Value> {
        self.constants.get(ind as usize)
    }
}
