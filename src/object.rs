#[derive(Clone)]
pub enum Object {
    String(String),
}

impl Object {
    pub fn print(&self) {
        match self {
            Object::String(string) => print!("{}", string),
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::String(a), Object::String(b)) => a == b,
        }
    }
}
