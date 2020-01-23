pub fn to_u16(lo: u8, hi: u8) -> u16 {
    (hi as u16) << 8 | lo as u16
}

pub fn from_u16(x: u16) -> (u8, u8) {
    (((x >> 8) & 0xff) as u8, (x & 0xff) as u8)
}
