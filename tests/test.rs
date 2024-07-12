use serde::{Deserialize, Serialize};
use wasm_runtime;

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
enum TestSuite {
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
    I32 { value: Option<i32> },
    I64 { value: Option<i64> },
    F32 { value: Option<f32> },
    F64 { value: Option<f64> },
}
