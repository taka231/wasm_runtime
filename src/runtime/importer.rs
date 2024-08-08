use super::{store::Store, value::Value};

pub trait Importer {
    fn call(&mut self, store: &mut Store, name: &str, args: Vec<Value>) -> Result<Value, String>;
}
