use crate::wasm::{
    AbsHeapType, CompositeType, FieldType, HeapType, RefNullInstrType, RefType, StorageType,
    ValType,
};

#[cfg(feature = "wasm")]
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

#[cfg(feature = "wasmgc")]
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    RefNull(RefNullInstrType),
    ExternRef(usize),
    FuncRef(usize),
    StructRef(usize),
    ArrayRef(usize),
}

impl Value {
    pub fn default(ty: &StorageType) -> Option<Self> {
        match ty {
            StorageType::ValType(ty) => match ty {
                ValType::I32 => Some(Value::I32(0)),
                ValType::I64 => Some(Value::I64(0)),
                ValType::F32 => Some(Value::F32(0.0)),
                ValType::F64 => Some(Value::F64(0.0)),
                ValType::V128 => todo!(),
                ValType::RefType(RefType::RefNull(heaptype)) => {
                    Some(Value::RefNull(heaptype.clone()))
                }
                ValType::RefType(_) => None,
            },
            StorageType::PackedType(_) => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructValue {
    pub types: Vec<FieldType>,
    pub values: Vec<u8>,
}

#[cfg(feature = "wasmgc")]
impl StructValue {
    pub fn get_field(
        &self,
        index: usize,
        types: &[CompositeType],
        offset: &[u32],
        size: u32,
    ) -> Result<Value, String> {
        let ty = self.types.get(index).ok_or("Invalid index".to_string())?;
        let start = *offset.get(index).ok_or("Invalid index".to_string())?;
        let end = start + ty.ty.size_of() as u32;
        if end > size {
            return Err("Invalid index".to_string());
        }
        Value::from_vec_u8(&self.values[start as usize..end as usize], &ty.ty, types)
    }

    pub fn set_field(
        &mut self,
        index: usize,
        value: &Value,
        offset: &[u32],
        size: u32,
    ) -> Result<(), String> {
        let ty = self.types.get(index).ok_or("Invalid index".to_string())?;
        if !ty.is_mutable {
            return Err("Field is not mutable".to_string());
        }
        let start = *offset.get(index).ok_or("Invalid index".to_string())?;
        let end = start + ty.ty.size_of() as u32;
        if end > size {
            return Err("Invalid index".to_string());
        }
        let vec = value.to_vec_u8();
        if vec.len() != ty.ty.size_of() as usize {
            return Err("Invalid value".to_string());
        }
        self.values[start as usize..end as usize].copy_from_slice(&vec);
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayValue {
    pub ty: FieldType,
    pub values: Vec<u8>,
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
    pub fn to_vec_u8(&self) -> Vec<u8> {
        match self {
            Value::I32(n) => n.to_le_bytes().to_vec(),
            Value::I64(n) => n.to_le_bytes().to_vec(),
            Value::F32(f) => f.to_le_bytes().to_vec(),
            Value::F64(f) => f.to_le_bytes().to_vec(),
            Value::RefNull(_) => vec![0, 0, 0, 0],
            Value::ExternRef(n) | Value::FuncRef(n) | Value::StructRef(n) | Value::ArrayRef(n) => {
                (*n as u32).to_le_bytes().to_vec()
            }
        }
    }

    #[cfg(feature = "wasmgc")]
    pub fn from_vec_u8(
        vec: &[u8],
        ty: &StorageType,
        types: &[CompositeType],
    ) -> Result<Self, String> {
        match ty {
            StorageType::ValType(ValType::I32) => {
                if vec.len() == 4 {
                    Ok(Value::I32(i32::from_le_bytes([
                        vec[0], vec[1], vec[2], vec[3],
                    ])))
                } else {
                    Err("Invalid length".to_string())
                }
            }
            StorageType::ValType(ValType::I64) => {
                if vec.len() == 8 {
                    Ok(Value::I64(i64::from_le_bytes([
                        vec[0], vec[1], vec[2], vec[3], vec[4], vec[5], vec[6], vec[7],
                    ])))
                } else {
                    Err("Invalid length".to_string())
                }
            }
            StorageType::ValType(ValType::F32) => {
                if vec.len() == 4 {
                    Ok(Value::F32(f32::from_le_bytes([
                        vec[0], vec[1], vec[2], vec[3],
                    ])))
                } else {
                    Err("Invalid length".to_string())
                }
            }
            StorageType::ValType(ValType::F64) => {
                if vec.len() == 8 {
                    Ok(Value::F64(f64::from_le_bytes([
                        vec[0], vec[1], vec[2], vec[3], vec[4], vec[5], vec[6], vec[7],
                    ])))
                } else {
                    Err("Invalid length".to_string())
                }
            }
            StorageType::ValType(ValType::V128) => todo!(),
            StorageType::ValType(ValType::RefType(ref_type)) => {
                if vec.len() == 4 {
                    let index = u32::from_le_bytes([vec[0], vec[1], vec[2], vec[3]]) as usize;
                    Ok(match ref_type {
                        RefType::Ref(HeapType::Abs(AbsHeapType::Extern))
                        | RefType::Abs(AbsHeapType::Extern) => Value::ExternRef(index),
                        RefType::Ref(HeapType::Abs(AbsHeapType::Func))
                        | RefType::Abs(AbsHeapType::Func) => Value::FuncRef(index),
                        RefType::Ref(HeapType::Abs(AbsHeapType::Struct))
                        | RefType::Abs(AbsHeapType::Struct) => Value::StructRef(index),
                        RefType::Ref(HeapType::Abs(AbsHeapType::Array))
                        | RefType::Abs(AbsHeapType::Array) => Value::ArrayRef(index),
                        RefType::Ref(HeapType::TypeIdx(typeidx)) => {
                            let ty = types
                                .get(*typeidx as usize)
                                .ok_or("Invalid type index".to_string())?;
                            match ty {
                                CompositeType::StructType(_) => Value::StructRef(index),
                                CompositeType::ArrayType(_) => Value::ArrayRef(index),
                                _ => unimplemented!(),
                            }
                        }
                        RefType::RefNull(heaptype) => {
                            if vec == [0, 0, 0, 0] {
                                Value::RefNull(heaptype.clone())
                            } else {
                                Err("Invalid value")?
                            }
                        }
                        _ => unimplemented!(),
                    })
                } else {
                    Err("Invalid length".to_string())
                }
            }
            StorageType::PackedType(_) => todo!(),
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
