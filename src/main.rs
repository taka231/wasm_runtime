use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;

use wasm_runtime::parser::Parser;
use wasm_runtime::runtime::Runtime;

fn main() {
    let mut filename = String::new();
    print!("Enter the filename: ");
    std::io::stdout().flush().unwrap();
    std::io::stdin().read_line(&mut filename).unwrap();
    let path = Path::new(".").join(filename.trim());
    let mut file = File::open(path).unwrap();
    let mut bytes = Vec::new();
    file.read_to_end(&mut bytes).unwrap();
    let mut parser = Parser::new(&bytes);
    let module = parser.parse().unwrap();
    let mut runtime = Runtime::new(module);
    let result = runtime.call_with_name("_start", vec![]).unwrap();
    println!("{:?}", result);
}
