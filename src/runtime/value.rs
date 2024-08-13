use crate::wasm::{AbsHeapType, HeapType, RefNullInstrType, RefType};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    RefNull(RefNullInstrType),
    ExternRef(usize),
    FuncRef(usize),
}

impl Value {
    pub fn as_i32(&self) -> Result<i32, String> {
        match self {
            Value::I32(x) => Ok(*x),
            _ => Err("Expected i32".to_string()),
        }
    }
    pub fn as_i64(&self) -> Result<i64, String> {
        match self {
            Value::I64(x) => Ok(*x),
            _ => Err("Expected i64".to_string()),
        }
    }
    pub fn as_f32(&self) -> Result<f32, String> {
        match self {
            Value::F32(x) => Ok(*x),
            _ => Err("Expected f32".to_string()),
        }
    }
    pub fn as_f64(&self) -> Result<f64, String> {
        match self {
            Value::F64(x) => Ok(*x),
            _ => Err("Expected f64".to_string()),
        }
    }
    pub fn is_nan(&self) -> bool {
        match self {
            Value::F32(x) => x.is_nan(),
            Value::F64(x) => x.is_nan(),
            _ => false,
        }
    }
    pub fn is_pos_inf(&self) -> bool {
        match self {
            Value::F32(x) => x.is_infinite() && x.is_sign_positive(),
            Value::F64(x) => x.is_infinite() && x.is_sign_positive(),
            _ => false,
        }
    }
    pub fn is_neg_inf(&self) -> bool {
        match self {
            Value::F32(x) => x.is_infinite() && x.is_sign_negative(),
            Value::F64(x) => x.is_infinite() && x.is_sign_negative(),
            _ => false,
        }
    }

    #[cfg(feature = "wasmgc")]
    pub const REF_NULL_EXTERN: Value = Value::RefNull(HeapType::Abs(AbsHeapType::Extern));
    #[cfg(feature = "wasm")]
    pub const REF_NULL_EXTERN: Value = Value::RefNull(RefType::ExternRef);
    #[cfg(feature = "wasmgc")]
    pub const REF_NULL_FUNC: Value = Value::RefNull(HeapType::Abs(AbsHeapType::Func));
    #[cfg(feature = "wasm")]
    pub const REF_NULL_FUNC: Value = Value::RefNull(RefType::FuncRef);
}

macro_rules! impl_into_value {
    (&$ty:ty, $expr: expr) => {
        impl<'a> From<&'a $ty> for Value {
            fn from(x: &'a $ty) -> Self {
                $expr(*x)
            }
        }
    };
    ($ty:ty, $expr: expr) => {
        impl From<$ty> for Value {
            fn from(x: $ty) -> Self {
                $expr(x)
            }
        }
    };
}
impl_into_value!(i32, Value::I32);
impl_into_value!(i64, Value::I64);
impl_into_value!(f32, Value::F32);
impl_into_value!(f64, Value::F64);
impl_into_value!(&i32, Value::I32);
impl_into_value!(&i64, Value::I64);
impl_into_value!(&f32, Value::F32);
impl_into_value!(&f64, Value::F64);
