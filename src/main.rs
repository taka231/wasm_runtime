use wasm_runtime::parser::Parser;
use wasm_runtime::runtime::Runtime;
use wasm_runtime::wasm::SectionContent;

fn main() {
    let bytes = include_bytes!("../tests/wasm/sample.wasm");
    let mut parser = Parser::new(bytes);
    let sections = parser.parse().unwrap();
    println!("(1)");
    for section in &sections {
        println!(
            "section kind: {}, size: {:x}",
            if matches!(section.content, SectionContent::Code(_)) {
                "Code".into()
            } else {
                format!("{:?}", section.content)
            },
            section.size
        );
    }
    println!("(2), (3)");
    for section in &sections {
        if let SectionContent::Code(funcs) = &section.content {
            for func in funcs {
                println!("function size: {:x}", func.size);
                for instr in &func.instrs {
                    if let wasm_runtime::wasm::Instr::I64Const(n) = instr {
                        println!("  I64Const({})", n);
                    }
                }
            }
        }
    }
    // (4)
    let add_bytes = include_bytes!("../tests/wasm/add.wasm");
    let mut parser = Parser::new(add_bytes);
    let sections = parser.parse().unwrap();
    println!("(5)");
    println!("{:?}", sections);
    println!("(6)");
    let mut runtime = Runtime::new(sections);
    runtime.run();
    println!("{:?}", runtime.stack[0]);
    // (7)
    // 1~10までの和を計算するsum.wasmを実行出来るようにする
    let sum_bytes = include_bytes!("../tests/wasm/sum.wasm");
    let mut parser = Parser::new(sum_bytes);
    let sections = parser.parse().unwrap();
    println!("(7)(5)");
    println!("{:?}", sections);
    println!("(7)(6)");
    let mut runtime = Runtime::new(sections);
    runtime.run();
    println!("{:?}", runtime.stack[0]);
}
