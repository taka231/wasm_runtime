use wasm_runtime::{
    parser::Parser,
    runtime::value::{StructValue, Value},
    wasm::{FieldType, StorageType, ValType},
};

#[cfg(feature = "wasmgc")]
#[test]
fn test_test() {
    let mut parser = Parser::new(include_bytes!("./wasmgc/test.wasm"));
    let module = parser.parse().unwrap();
    let mut runtime = wasm_runtime::runtime::Runtime::new(module, None);
    let Value::StructRef(struct_ref) = runtime
        .call_with_name("make", vec![Value::I32(10)])
        .unwrap()[0]
    else {
        panic!("Expected StructRef");
    };
    let struct_value = runtime
        .store
        .borrow()
        .structs
        .get(struct_ref)
        .unwrap()
        .clone();
    assert_eq!(
        struct_value,
        StructValue {
            types: vec![FieldType {
                ty: StorageType::ValType(ValType::I32),
                is_mutable: false
            }],
            values: vec![10, 0, 0, 0]
        }
    );
    let value = &runtime
        .call_with_name("get", vec![Value::StructRef(struct_ref)])
        .unwrap()[0];
    assert_eq!(value, &Value::I32(10));
}

#[cfg(feature = "wasmgc")]
#[test]
fn test_struct_set() {
    let mut parser = Parser::new(include_bytes!("./wasmgc/struct_set.wasm"));
    let module = parser.parse().unwrap();
    let mut runtime = wasm_runtime::runtime::Runtime::new(module, None);
    let struct_ref = runtime.call_with_name("make", vec![Value::I32(0)]).unwrap()[0].clone();
    let value = runtime
        .call_with_name("get", vec![struct_ref.clone()])
        .unwrap()[0]
        .clone();
    assert_eq!(value, Value::I32(0));
    runtime
        .call_with_name("set", vec![struct_ref.clone(), Value::I32(10)])
        .unwrap();
    let value = runtime.call_with_name("get", vec![struct_ref]).unwrap()[0].clone();
    assert_eq!(value, Value::I32(10));
}

#[cfg(feature = "wasmgc")]
#[test]
fn test_struct() {
    let mut parser = Parser::new(include_bytes!("./wasmgc/struct.wasm"));
    let module = parser.parse().unwrap();
    let mut runtime = wasm_runtime::runtime::Runtime::new(module, None);
    let value = runtime.call_with_name("_start", vec![]).unwrap()[0].clone();
    assert_eq!(value, Value::I32(43));
}
