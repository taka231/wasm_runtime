use wasm_runtime::parser::Parser;
use wasm_runtime::runtime::Runtime;

fn main() {
    // (1)
    let bytes = include_bytes!("../tests/wasm/sample.wasm");
    let mut parser = Parser::new(bytes);
    let sections = parser.parse().unwrap();
    println!("(1)");
    println!("{:?}", &sections);
    // let mut runtime = Runtime::new(sections);
    // runtime.call_with_name("_start", Vec::new()).unwrap();
    // (4)
    let add_bytes = include_bytes!("../tests/wasm/add.wasm");
    let mut parser = Parser::new(add_bytes);
    let sections = parser.parse().unwrap();
    println!("(5)");
    println!("{:?}", sections);
    println!("(6)");
    let mut runtime = Runtime::new(sections);
    let result = runtime.call_with_name("_start", Vec::new());
    println!("{:?}", result);
    // (7)
    // 1~10までの和を計算するsum.wasmを実行出来るようにする
    let sum_bytes = include_bytes!("../tests/wasm/sum.wasm");
    let mut parser = Parser::new(sum_bytes);
    let sections = parser.parse().unwrap();
    println!("(7)(5)");
    println!("{:?}", sections);
    println!("(7)(6)");
    let mut runtime = Runtime::new(sections);
    let result = runtime.call_with_name("_start", Vec::new());
    println!("{:?}", result);
}
