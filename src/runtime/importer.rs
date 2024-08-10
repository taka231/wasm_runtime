use super::{store::Store, value::Value};

pub trait Importer {
    fn name(&self) -> &str;
    fn call(&mut self, store: &mut Store, name: &str, args: Vec<Value>) -> Result<Value, String>;
}
