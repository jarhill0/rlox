pub struct Value(f64);

impl Value {
    pub fn new(val: f64) -> Value {
        Value(val)
    }

    pub fn print(&self) {
        print!("{}", self.0);
    }
}
