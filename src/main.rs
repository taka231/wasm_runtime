use wasm_runtime::parser::Parser;
use wasm_runtime::runtime::Runtime;

fn main() {
    let mut parser = Parser::new(include_bytes!("../tests/wasi/wasm_runtime.wasm"));
    let sections = parser.parse().unwrap();
    let mut runtime = Runtime::new(sections);
    runtime.call_with_name("_start", Vec::new()).unwrap();
}
