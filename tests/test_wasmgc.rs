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
        .heap
        .structs
        .get(struct_ref as u32)
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

#[cfg(feature = "wasmgc")]
#[test]
fn test_array() {
    use wasm_runtime::runtime::value::ArrayValue;

    let mut parser = Parser::new(include_bytes!("./wasmgc/array.wasm"));
    let module = parser.parse().unwrap();
    let mut runtime = wasm_runtime::runtime::Runtime::new(module, None);
    let global = runtime
        .store
        .borrow()
        .global
        .iter()
        .map(|global| global.value.clone())
        .collect::<Vec<_>>();
    assert_eq!(&global, &vec![Value::ArrayRef(1), Value::ArrayRef(2)]);
    assert_eq!(
        &runtime.store.borrow().heap.arrays.get(1).unwrap().values,
        &(1.0_f32.to_le_bytes().repeat(3))
    );
    let Value::ArrayRef(array_ref) = runtime.call_with_name("new", vec![]).unwrap()[0] else {
        panic!("Expected ArrayRef");
    };
    let array_value = runtime
        .store
        .borrow()
        .heap
        .arrays
        .get(array_ref as u32)
        .unwrap()
        .clone();
    assert_eq!(
        array_value,
        ArrayValue {
            ty: FieldType {
                ty: StorageType::ValType(ValType::F32),
                is_mutable: false
            },
            values: vec![0; 12],
        },
    );
    let value = runtime.call_with_name("get", vec![Value::I32(0)]).unwrap()[0].clone();
    assert_eq!(value, Value::F32(0.0));
    let value = runtime
        .call_with_name("set_get", vec![Value::I32(1), Value::F32(7.0)])
        .unwrap()[0]
        .clone();
    assert_eq!(value, Value::F32(7.0));
    let len = runtime.call_with_name("len", vec![]).unwrap()[0].clone();
    assert_eq!(len, Value::I32(3));
}

#[test]
fn test_mandelbrot() {
    let mut parser = Parser::new(include_bytes!("./wasmgc/mandelbrot.wasm"));
    let module = parser.parse().unwrap();
    let mut runtime = wasm_runtime::runtime::Runtime::new(module, None);
    let size = 1000;
    let Value::ArrayRef(array_ref) = runtime
        .call_with_name(
            "Mandelbrot",
            vec![
                Value::F64(-5.0),
                Value::F64(5.0),
                Value::F64(0.001),
                Value::I32(size),
            ],
        )
        .unwrap()[0]
    else {
        panic!("Expected ArrayRef");
    };
    let array = runtime
        .store
        .borrow()
        .heap
        .arrays
        .get(array_ref as u32)
        .unwrap()
        .clone();
    for i in 0..(size as usize) {
        for j in 0..(size as usize) {
            let start = i * size as usize + j;
            let value = &array.values[start..start + 4];
            let value = i32::from_le_bytes(value.try_into().unwrap());
            if value == 1 {
                // println!("{} {}", 0.001 * i as f32 - 5.0, 0.001 * j as f32 - 5.0);
            }
        }
    }
}
