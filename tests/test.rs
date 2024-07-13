use std::process::Command;

use serde::{Deserialize, Serialize};
use wasm_runtime::{
    self,
    parser::Parser,
    runtime::{Runtime, Value},
};

#[derive(Debug, Serialize, Deserialize)]
struct TestSuite {
    source_filename: String,
    commands: Vec<Test>,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
enum Test {
    Module {
        line: u32,
        filename: String,
    },
    AssertReturn {
        line: u32,
        action: Action,
        expected: Vec<Val>,
    },
    AssertTrap {
        line: u32,
        action: Action,
        text: String,
        expected: Vec<Val>,
    },
    AssertInvalid {
        line: u32,
        filename: String,
        text: String,
        module_type: String,
    },
    AssertMalformed {
        line: u32,
        filename: String,
        text: String,
        module_type: String,
    },
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
enum Action {
    Invoke { field: String, args: Vec<Val> },
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
enum Val {
    I32 { value: Option<String> },
    I64 { value: Option<String> },
    F32 { value: Option<String> },
    F64 { value: Option<String> },
}

fn test_suite(file_path: &str) {
    let temp_dir = tempfile::tempdir().unwrap();
    Command::new("wast2json")
        .arg(&format!("./testsuite/{file_path}"))
        .arg("-o")
        .arg(temp_dir.path().join("test.json"))
        .output()
        .unwrap();
    let test_suite: TestSuite =
        serde_json::from_reader(std::fs::File::open(temp_dir.path().join("test.json")).unwrap())
            .unwrap();
    let tests = test_suite.commands;
    let mut runtime = None;
    for test in tests {
        match test {
            Test::Module { line: _, filename } => {
                let filename = temp_dir.path().join(&filename);
                let bytes = std::fs::read(filename).unwrap();
                let mut parser = Parser::new(&bytes);
                let module = parser.parse().unwrap();
                runtime = Some(Runtime::new(module));
            }
            Test::AssertReturn {
                line: _,
                action,
                expected,
            } => {
                let action = match dbg!(action) {
                    Action::Invoke { field, args } => {
                        let args = args
                            .into_iter()
                            .map(|arg| match arg {
                                Val::I32 { value } => {
                                    Value::I32(value.unwrap().parse::<u32>().unwrap() as i32)
                                }
                                Val::I64 { value } => {
                                    Value::I64(value.unwrap().parse::<u64>().unwrap() as i64)
                                }
                                Val::F32 { value } => Value::F32(value.unwrap().parse().unwrap()),
                                Val::F64 { value } => Value::F64(value.unwrap().parse().unwrap()),
                            })
                            .collect();
                        runtime.as_mut().unwrap().call_with_name(&field, args)
                    }
                };
                let expected = Ok(expected
                    .into_iter()
                    .map(|val| match val {
                        Val::I32 { value } => {
                            Value::I32(value.unwrap().parse::<u32>().unwrap() as i32)
                        }
                        Val::I64 { value } => {
                            Value::I64(value.unwrap().parse::<u64>().unwrap() as i64)
                        }
                        Val::F32 { value } => Value::F32(value.unwrap().parse().unwrap()),
                        Val::F64 { value } => Value::F64(value.unwrap().parse().unwrap()),
                    })
                    .collect());
                assert_eq!(action, expected);
            }
            // todo
            Test::AssertTrap { .. } => {}
            Test::AssertInvalid { .. } => {}
            Test::AssertMalformed { .. } => {}
        }
    }
}

#[test]
fn test_i32() {
    test_suite("i32.wast");
}
