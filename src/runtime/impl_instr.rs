use crate::{
    runtime::Value,
    wasm::{CompositeType, Opcode, TruncSatOp, WasmGCInstr},
};

use super::{value::StructValue, Runtime};

impl Runtime {
    pub(super) fn exec_memory_instr_with_memarg(
        &mut self,
        op: &Opcode,
        offset: &u32,
    ) -> Result<(), String> {
        use Opcode::*;
        match op {
            I32Load | I64Load | F32Load | F64Load | I32Load8S | I32Load8U | I32Load16S
            | I32Load16U | I64Load8S | I64Load8U | I64Load16S | I64Load16U | I64Load32S
            | I64Load32U => {
                let addr = self.stack.pop().ok_or("expected value")?.as_i32()? as u32;
                let size = match op {
                    I32Load8S | I32Load8U | I64Load8S | I64Load8U => 1,
                    I32Load16S | I32Load16U | I64Load16S | I64Load16U => 2,
                    I32Load | F32Load | I64Load32S | I64Load32U => 4,
                    F64Load | I64Load => 8,
                    _ => unreachable!(),
                };
                let store = self.store.borrow();
                let value = store.memory.load(*offset, addr, size)?;
                let value = match op {
                    I32Load => i32::from_le_bytes([value[0], value[1], value[2], value[3]]).into(),
                    I64Load => i64::from_le_bytes([
                        value[0], value[1], value[2], value[3], value[4], value[5], value[6],
                        value[7],
                    ])
                    .into(),
                    F32Load => {
                        f32::from_bits(u32::from_le_bytes([value[0], value[1], value[2], value[3]]))
                            .into()
                    }
                    F64Load => f64::from_bits(u64::from_le_bytes([
                        value[0], value[1], value[2], value[3], value[4], value[5], value[6],
                        value[7],
                    ]))
                    .into(),
                    I32Load8S => (i8::from_le_bytes([value[0]]) as i32).into(),
                    I32Load8U => (u8::from_le_bytes([value[0]]) as i32).into(),
                    I32Load16S => (i16::from_le_bytes([value[0], value[1]]) as i32).into(),
                    I32Load16U => (u16::from_le_bytes([value[0], value[1]]) as i32).into(),
                    I64Load8S => (i8::from_le_bytes([value[0]]) as i64).into(),
                    I64Load8U => (u8::from_le_bytes([value[0]]) as i64).into(),
                    I64Load16S => (i16::from_le_bytes([value[0], value[1]]) as i64).into(),
                    I64Load16U => (u16::from_le_bytes([value[0], value[1]]) as i64).into(),
                    I64Load32S => {
                        (i32::from_le_bytes([value[0], value[1], value[2], value[3]]) as i64).into()
                    }
                    I64Load32U => {
                        (u32::from_le_bytes([value[0], value[1], value[2], value[3]]) as i64).into()
                    }
                    _ => unreachable!(),
                };
                self.stack.push(value);
            }
            I32Store | I64Store | F32Store | F64Store | I32Store8 | I32Store16 | I64Store8
            | I64Store16 | I64Store32 => {
                let value = self.stack.pop().ok_or("expected value")?;
                let addr = self.stack.pop().ok_or("expected value")?;
                let value = match value {
                    Value::I32(value) => value.to_le_bytes().to_vec(),
                    Value::I64(value) => value.to_le_bytes().to_vec(),
                    Value::F32(value) => value.to_bits().to_le_bytes().to_vec(),
                    Value::F64(value) => value.to_bits().to_le_bytes().to_vec(),
                    _ => Err("Invalid value type")?,
                };
                let size = match op {
                    I32Store8 | I64Store8 => 1,
                    I32Store16 | I64Store16 => 2,
                    I32Store | F32Store | I64Store32 => 4,
                    F64Store | I64Store => 8,
                    _ => unreachable!(),
                };
                let addr = addr.as_i32()? as u32;
                self.store
                    .borrow_mut()
                    .memory
                    .store(*offset, addr, size, &value)?;
            }
            _ => unreachable!("opcode {:?} is not a memory instruction", op),
        };
        Ok(())
    }
    pub(super) fn exec_ibinop(op: &Opcode, lhs: Value, rhs: Value) -> Result<Value, String> {
        use Opcode::*;
        let result = match op {
            I32Add => (lhs.as_i32()?.wrapping_add(rhs.as_i32()?)).into(),
            I32Sub => (lhs.as_i32()?.wrapping_sub(rhs.as_i32()?)).into(),
            I32Mul => (lhs.as_i32()?.wrapping_mul(rhs.as_i32()?)).into(),
            I32DivS => (lhs.as_i32()?.wrapping_div(rhs.as_i32()?)).into(),
            I32DivU => (((lhs.as_i32()? as u32).wrapping_div(rhs.as_i32()? as u32)) as i32).into(),
            I32RemS => (lhs.as_i32()?.wrapping_rem(rhs.as_i32()?)).into(),
            I32RemU => (((lhs.as_i32()? as u32).wrapping_rem(rhs.as_i32()? as u32)) as i32).into(),
            I32And => (lhs.as_i32()? & rhs.as_i32()?).into(),
            I32Or => (lhs.as_i32()? | rhs.as_i32()?).into(),
            I32Xor => (lhs.as_i32()? ^ rhs.as_i32()?).into(),
            I32Shl => (lhs.as_i32()?.wrapping_shl(rhs.as_i32()? as u32)).into(),
            I32ShrS => (lhs.as_i32()?.wrapping_shr(rhs.as_i32()? as u32)).into(),
            I32ShrU => (((lhs.as_i32()? as u32).wrapping_shr(rhs.as_i32()? as u32)) as i32).into(),
            I32Rotl => {
                let a = lhs.as_i32()? as u32;
                let b = rhs.as_i32()? as u32;
                let b = b % 32;
                (((a.wrapping_shl(b)) | (a.wrapping_shr(32 - b))) as i32).into()
            }
            I32Rotr => {
                let a = lhs.as_i32()? as u32;
                let b = rhs.as_i32()? as u32;
                let b = b % 32;
                (((a.wrapping_shr(b)) | (a.wrapping_shl(32 - b))) as i32).into()
            }
            I64Add => (lhs.as_i64()?.wrapping_add(rhs.as_i64()?)).into(),
            I64Sub => (lhs.as_i64()?.wrapping_sub(rhs.as_i64()?)).into(),
            I64Mul => (lhs.as_i64()?.wrapping_mul(rhs.as_i64()?)).into(),
            I64DivS => (lhs.as_i64()?.wrapping_div(rhs.as_i64()?)).into(),
            I64DivU => (((lhs.as_i64()? as u64).wrapping_div(rhs.as_i64()? as u64)) as i64).into(),
            I64RemS => (lhs.as_i64()?.wrapping_rem(rhs.as_i64()?)).into(),
            I64RemU => (((lhs.as_i64()? as u64).wrapping_rem(rhs.as_i64()? as u64)) as i64).into(),
            I64And => (lhs.as_i64()? & rhs.as_i64()?).into(),
            I64Or => (lhs.as_i64()? | rhs.as_i64()?).into(),
            I64Xor => (lhs.as_i64()? ^ rhs.as_i64()?).into(),
            I64Shl => (lhs.as_i64()?.wrapping_shl(rhs.as_i64()? as u32)).into(),
            I64ShrS => (lhs.as_i64()?.wrapping_shr(rhs.as_i64()? as u32)).into(),
            I64ShrU => (((lhs.as_i64()? as u64).wrapping_shr(rhs.as_i64()? as u32)) as i64).into(),
            I64Rotl => {
                let a = lhs.as_i64()? as u64;
                let b = rhs.as_i64()? as u32;
                let b = b % 64;
                (((a.wrapping_shl(b)) | (a.wrapping_shr(64 - b))) as i64).into()
            }
            I64Rotr => {
                let a = lhs.as_i64()? as u64;
                let b = rhs.as_i64()? as u32;
                let b = b % 64;
                (((a.wrapping_shr(b)) | (a.wrapping_shl(64 - b))) as i64).into()
            }
            _ => unreachable!("opcode {:?} is not a ibinop", op),
        };
        Ok(result)
    }
    pub(super) fn exec_funop(op: &Opcode, value: Value) -> Result<Value, String> {
        use Opcode::*;
        let result = match op {
            F32Abs => value.as_f32()?.abs().into(),
            F32Neg => (-value.as_f32()?).into(),
            F32Ceil => value.as_f32()?.ceil().into(),
            F32Floor => value.as_f32()?.floor().into(),
            F32Trunc => value.as_f32()?.trunc().into(),
            F32Nearest => value.as_f32()?.round_ties_even().into(),
            F32Sqrt => value.as_f32()?.sqrt().into(),
            F64Abs => value.as_f64()?.abs().into(),
            F64Neg => (-value.as_f64()?).into(),
            F64Ceil => value.as_f64()?.ceil().into(),
            F64Floor => value.as_f64()?.floor().into(),
            F64Trunc => value.as_f64()?.trunc().into(),
            F64Nearest => value.as_f64()?.round_ties_even().into(),
            F64Sqrt => value.as_f64()?.sqrt().into(),
            _ => unreachable!("opcode {:?} is not a funop", op),
        };
        Ok(result)
    }
    pub(super) fn exec_fbinop(op: &Opcode, lhs: Value, rhs: Value) -> Result<Value, String> {
        use Opcode::*;
        let result = match op {
            F32Add => (lhs.as_f32()? + rhs.as_f32()?).into(),
            F32Sub => (lhs.as_f32()? - rhs.as_f32()?).into(),
            F32Mul => (lhs.as_f32()? * rhs.as_f32()?).into(),
            F32Div => (lhs.as_f32()? / rhs.as_f32()?).into(),
            F32Min => {
                let a = lhs.as_f32()?;
                let b = rhs.as_f32()?;
                if a.is_nan() || b.is_nan() {
                    f32::NAN
                } else {
                    a.min(b)
                }
                .into()
            }
            F32Max => {
                let a = lhs.as_f32()?;
                let b = rhs.as_f32()?;
                if a.is_nan() || b.is_nan() {
                    f32::NAN
                } else {
                    a.max(b)
                }
                .into()
            }
            F32Copysign => lhs.as_f32()?.copysign(rhs.as_f32()?).into(),
            F64Add => (lhs.as_f64()? + rhs.as_f64()?).into(),
            F64Sub => (lhs.as_f64()? - rhs.as_f64()?).into(),
            F64Mul => (lhs.as_f64()? * rhs.as_f64()?).into(),
            F64Div => (lhs.as_f64()? / rhs.as_f64()?).into(),
            F64Min => {
                let a = lhs.as_f64()?;
                let b = rhs.as_f64()?;
                if a.is_nan() || b.is_nan() {
                    f64::NAN
                } else {
                    a.min(b)
                }
                .into()
            }
            F64Max => {
                let a = lhs.as_f64()?;
                let b = rhs.as_f64()?;
                if a.is_nan() || b.is_nan() {
                    f64::NAN
                } else {
                    a.max(b)
                }
                .into()
            }
            F64Copysign => lhs.as_f64()?.copysign(rhs.as_f64()?).into(),
            _ => unreachable!("opcode {:?} is not a fbinop", op),
        };
        Ok(result)
    }
    pub(super) fn exec_frelop(op: &Opcode, lhs: Value, rhs: Value) -> Result<Value, String> {
        use Opcode::*;
        let result = match op {
            F32Eq => ((lhs.as_f32()? == rhs.as_f32()?) as i32).into(),
            F32Ne => ((lhs.as_f32()? != rhs.as_f32()?) as i32).into(),
            F32Lt => ((lhs.as_f32()? < rhs.as_f32()?) as i32).into(),
            F32Gt => ((lhs.as_f32()? > rhs.as_f32()?) as i32).into(),
            F32Le => ((lhs.as_f32()? <= rhs.as_f32()?) as i32).into(),
            F32Ge => ((lhs.as_f32()? >= rhs.as_f32()?) as i32).into(),
            F64Eq => ((lhs.as_f64()? == rhs.as_f64()?) as i32).into(),
            F64Ne => ((lhs.as_f64()? != rhs.as_f64()?) as i32).into(),
            F64Lt => ((lhs.as_f64()? < rhs.as_f64()?) as i32).into(),
            F64Gt => ((lhs.as_f64()? > rhs.as_f64()?) as i32).into(),
            F64Le => ((lhs.as_f64()? <= rhs.as_f64()?) as i32).into(),
            F64Ge => ((lhs.as_f64()? >= rhs.as_f64()?) as i32).into(),
            _ => unreachable!("opcode {:?} is not a frelop", op),
        };
        Ok(result)
    }
    pub(super) fn exec_irelop(op: &Opcode, lhs: Value, rhs: Value) -> Result<Value, String> {
        use Opcode::*;
        let result = match op {
            I32Eq => ((lhs.as_i32()? == rhs.as_i32()?) as i32).into(),
            I32Ne => ((lhs.as_i32()? != rhs.as_i32()?) as i32).into(),
            I32LtS => ((lhs.as_i32()? < rhs.as_i32()?) as i32).into(),
            I32LtU => (((lhs.as_i32()? as u32) < (rhs.as_i32()? as u32)) as i32).into(),
            I32GtS => ((lhs.as_i32()? > rhs.as_i32()?) as i32).into(),
            I32GtU => (((lhs.as_i32()? as u32) > (rhs.as_i32()? as u32)) as i32).into(),
            I32LeS => ((lhs.as_i32()? <= rhs.as_i32()?) as i32).into(),
            I32LeU => (((lhs.as_i32()? as u32) <= (rhs.as_i32()? as u32)) as i32).into(),
            I32GeS => ((lhs.as_i32()? >= rhs.as_i32()?) as i32).into(),
            I32GeU => (((lhs.as_i32()? as u32) >= (rhs.as_i32()? as u32)) as i32).into(),
            I64Eq => ((lhs.as_i64()? == rhs.as_i64()?) as i32).into(),
            I64Ne => ((lhs.as_i64()? != rhs.as_i64()?) as i32).into(),
            I64LtS => ((lhs.as_i64()? < rhs.as_i64()?) as i32).into(),
            I64LtU => (((lhs.as_i64()? as u64) < (rhs.as_i64()? as u64)) as i32).into(),
            I64GtS => ((lhs.as_i64()? > rhs.as_i64()?) as i32).into(),
            I64GtU => (((lhs.as_i64()? as u64) > (rhs.as_i64()? as u64)) as i32).into(),
            I64LeS => ((lhs.as_i64()? <= rhs.as_i64()?) as i32).into(),
            I64LeU => (((lhs.as_i64()? as u64) <= (rhs.as_i64()? as u64)) as i32).into(),
            I64GeS => ((lhs.as_i64()? >= rhs.as_i64()?) as i32).into(),
            I64GeU => (((lhs.as_i64()? as u64) >= (rhs.as_i64()? as u64)) as i32).into(),
            _ => unreachable!("opcode {:?} is not a relop", op),
        };
        Ok(result)
    }
    pub(super) fn exec_iunop(op: &Opcode, value: Value) -> Result<Value, String> {
        let result = match op {
            Opcode::I32Clz => (value.as_i32()?.leading_zeros() as i32).into(),
            Opcode::I32Ctz => (value.as_i32()?.trailing_zeros() as i32).into(),
            Opcode::I32Popcnt => (value.as_i32()?.count_ones() as i32).into(),
            Opcode::I64Clz => (value.as_i64()?.leading_zeros() as i64).into(),
            Opcode::I64Ctz => (value.as_i64()?.trailing_zeros() as i64).into(),
            Opcode::I64Popcnt => (value.as_i64()?.count_ones() as i64).into(),
            _ => unreachable!("opcode {:?} is not a unop", op),
        };
        Ok(result)
    }
    pub(super) fn exec_cutop(op: &Opcode, value: Value) -> Result<Value, String> {
        use Opcode::*;
        let result = match op {
            I32Extend8S => (value.as_i32()? as i8 as i32).into(),
            I32Extend16S => (value.as_i32()? as i16 as i32).into(),
            I64Extend8S => (value.as_i64()? as i8 as i64).into(),
            I64Extend16S => (value.as_i64()? as i16 as i64).into(),
            I64Extend32S => (value.as_i64()? as i32 as i64).into(),
            I32WrapI64 => (value.as_i64()? as i32).into(),
            I64ExtendI32S => (value.as_i32()? as i64).into(),
            I64ExtendI32U => (value.as_i32()? as u32 as i64).into(),
            I32TruncF32S => (value.as_f32()?.trunc() as i32).into(),
            I32TruncF32U => (value.as_f32()?.trunc() as u32 as i32).into(),
            I32TruncF64S => (value.as_f64()?.trunc() as i32).into(),
            I32TruncF64U => (value.as_f64()?.trunc() as u32 as i32).into(),
            I64TruncF32S => (value.as_f32()?.trunc() as i64).into(),
            I64TruncF32U => (value.as_f32()?.trunc() as u64 as i64).into(),
            I64TruncF64S => (value.as_f64()?.trunc() as i64).into(),
            I64TruncF64U => (value.as_f64()?.trunc() as u64 as i64).into(),
            F32ConvertI32S => (value.as_i32()? as f32).into(),
            F32ConvertI32U => (value.as_i32()? as u32 as f32).into(),
            F32ConvertI64S => (value.as_i64()? as f32).into(),
            F32ConvertI64U => (value.as_i64()? as u64 as f32).into(),
            F32DemoteF64 => (value.as_f64()? as f32).into(),
            F64ConvertI32S => (value.as_i32()? as f64).into(),
            F64ConvertI32U => (value.as_i32()? as u32 as f64).into(),
            F64ConvertI64S => (value.as_i64()? as f64).into(),
            F64ConvertI64U => (value.as_i64()? as u64 as f64).into(),
            F64PromoteF32 => (value.as_f32()? as f64).into(),
            I32ReinterpretF32 => (value.as_f32()?.to_bits() as i32).into(),
            I64ReinterpretF64 => (value.as_f64()?.to_bits() as i64).into(),
            F32ReinterpretI32 => (f32::from_bits(value.as_i32()? as u32)).into(),
            F64ReinterpretI64 => (f64::from_bits(value.as_i64()? as u64)).into(),
            _ => unreachable!("opcode {:?} is not a cutop", op),
        };
        Ok(result)
    }
    pub(super) fn exec_trunc_sat(op: &TruncSatOp, a: Value) -> Result<Value, String> {
        use TruncSatOp::*;
        let result = if a.is_nan() {
            match op {
                I32TruncSatF32S | I32TruncSatF32U | I32TruncSatF64S | I32TruncSatF64U => {
                    Value::I32(0)
                }
                I64TruncSatF32S | I64TruncSatF32U | I64TruncSatF64S | I64TruncSatF64U => {
                    Value::I64(0)
                }
            }
        } else if a.is_pos_inf() {
            match op {
                I32TruncSatF32S | I32TruncSatF64S => Value::I32(i32::MAX),
                I32TruncSatF32U | I32TruncSatF64U => Value::I32(u32::MAX as i32),
                I64TruncSatF32S | I64TruncSatF64S => Value::I64(i64::MAX),
                I64TruncSatF32U | I64TruncSatF64U => Value::I64(u64::MAX as i64),
            }
        } else if a.is_neg_inf() {
            match op {
                I32TruncSatF32S | I32TruncSatF64S => Value::I32(i32::MIN),
                I32TruncSatF32U | I32TruncSatF64U => Value::I32(0),
                I64TruncSatF32S | I64TruncSatF64S => Value::I64(i64::MIN),
                I64TruncSatF32U | I64TruncSatF64U => Value::I64(0),
            }
        } else {
            match op {
                I32TruncSatF32S => Value::I32(a.as_f32()?.trunc() as i32),
                I32TruncSatF32U => Value::I32(a.as_f32()?.trunc() as u32 as i32),
                I32TruncSatF64S => Value::I32(a.as_f64()?.trunc() as i32),
                I32TruncSatF64U => Value::I32(a.as_f64()?.trunc() as u32 as i32),
                I64TruncSatF32S => Value::I64(a.as_f32()?.trunc() as i64),
                I64TruncSatF32U => Value::I64(a.as_f32()?.trunc() as u64 as i64),
                I64TruncSatF64S => Value::I64(a.as_f64()?.trunc() as i64),
                I64TruncSatF64U => Value::I64(a.as_f64()?.trunc() as u64 as i64),
            }
        };
        Ok(result)
    }

    #[cfg(feature = "wasmgc")]
    pub(super) fn exec_wasm_gc_instr(&mut self, instr: &WasmGCInstr) -> Result<(), String> {
        use crate::wasm::{FieldType, RefType, StorageType};

        use super::value::ArrayValue;

        match instr {
            WasmGCInstr::StructNew(typeidx) => {
                let CompositeType::StructType(struct_type) = self
                    .store
                    .borrow()
                    .types
                    .get(*typeidx as usize)
                    .ok_or("type not found")?
                    .clone()
                else {
                    Err("struct type required")?
                };
                let (offset, size) = self
                    .store
                    .borrow()
                    .strcuttype_offset
                    .get(typeidx)
                    .expect("struct type offset")
                    .clone();
                if self.stack.len() < offset.len() {
                    Err("stack underflow")?
                }
                let values = self.stack.split_off(self.stack.len() - offset.len());
                let mut fields: Vec<u8> = Vec::new();
                for value in values {
                    fields.extend(value.to_vec_u8())
                }
                if fields.len() != size {
                    Err("field size is invalid")?
                }
                self.store.borrow_mut().structs.push(StructValue {
                    types: struct_type.clone(),
                    values: fields,
                });
                let index = self.store.borrow().structs.len() - 1;
                self.stack.push(Value::StructRef(index));
            }
            WasmGCInstr::StructGet { typeidx, fieldidx } => {
                let Value::StructRef(struct_ref) = self.stack.pop().ok_or("expected value")? else {
                    Err("struct reference required")?
                };

                let struct_data = self
                    .store
                    .borrow()
                    .structs
                    .get(struct_ref)
                    .ok_or("struct not found")?
                    .clone();
                let (struct_offset, size) = self
                    .store
                    .borrow()
                    .strcuttype_offset
                    .get(typeidx)
                    .expect("struct type offset")
                    .clone();
                let value = struct_data.get_field(
                    *fieldidx as usize,
                    &self.store.borrow().types,
                    &struct_offset,
                    size as u32,
                )?;
                self.stack.push(value);
            }
            WasmGCInstr::StructSet { typeidx, fieldidx } => {
                let value = self.stack.pop().ok_or("expected value")?;
                let Value::StructRef(struct_ref) = self.stack.pop().ok_or("expected value")? else {
                    Err("struct reference required")?
                };
                let (struct_offset, size) = self
                    .store
                    .borrow()
                    .strcuttype_offset
                    .get(typeidx)
                    .expect("struct type offset")
                    .clone();
                let mut store = self.store.borrow_mut();
                let struct_data = store
                    .structs
                    .get_mut(struct_ref)
                    .ok_or("struct not found")?;
                struct_data.set_field(*fieldidx as usize, &value, &struct_offset, size as u32)?;
            }
            WasmGCInstr::ArrayNew(typeidx) => {
                let n = self.stack.pop().ok_or("expected value")?.as_i32()? as usize;
                let val = self.stack.pop().ok_or("expected value")?;
                let CompositeType::ArrayType(array_type) = self
                    .store
                    .borrow()
                    .types
                    .get(*typeidx as usize)
                    .ok_or("type not found")?
                    .clone()
                else {
                    Err("array type required")?
                };
                let array_size = n * array_type.ty.size_of() as usize;
                let array = ArrayValue {
                    ty: array_type,
                    values: vec![0; array_size],
                };
                self.store.borrow_mut().arrays.push(array);
                let array_ref = self.store.borrow().arrays.len() - 1;
                self.stack.push(Value::ArrayRef(array_ref));
            }
            WasmGCInstr::ArrayNewDefault(typeidx) => {
                let n = self.stack.pop().ok_or("expected value")?.as_i32()? as usize;
                let CompositeType::ArrayType(array_type) = self
                    .store
                    .borrow()
                    .types
                    .get(*typeidx as usize)
                    .ok_or("type not found")?
                    .clone()
                else {
                    Err("array type required")?
                };
                let values = Value::to_vec_u8(
                    &Value::default(&array_type.ty).ok_or("default value not exists")?,
                )
                .repeat(n);
                let array_size = n * array_type.ty.size_of() as usize;
                assert!(values.len() == array_size);
                let array = ArrayValue {
                    ty: array_type,
                    values,
                };
                self.store.borrow_mut().arrays.push(array);
                let array_ref = self.store.borrow().arrays.len() - 1;
                self.stack.push(Value::ArrayRef(array_ref));
            }
            WasmGCInstr::ArrayGet(typeidx) => {
                let start = self.stack.pop().ok_or("expected value")?.as_i32()? as usize;
                let Value::ArrayRef(array_ref) = self.stack.pop().ok_or("expected value")? else {
                    Err("array reference required")?
                };
                let (size, ty) = {
                    let store = self.store.borrow();
                    let CompositeType::ArrayType(array_type) =
                        store.types.get(*typeidx as usize).ok_or("type not found")?
                    else {
                        Err("array type required")?
                    };
                    (array_type.ty.size_of() as usize, array_type.ty.clone())
                };
                let end = start + size;
                let store = self.store.borrow();
                let array = store.arrays.get(array_ref).ok_or("array not found")?;
                // todo: check ref.null
                let vec = array.values.get(start..end).ok_or("invalid index")?;
                let value = Value::from_vec_u8(vec, &ty, &store.types)?;
                self.stack.push(value);
            }
            WasmGCInstr::ArraySet(typeidx) => {
                let value = self.stack.pop().ok_or("expected value")?;
                let start = self.stack.pop().ok_or("expected value")?.as_i32()? as usize;
                let Value::ArrayRef(array_ref) = self.stack.pop().ok_or("expected value")? else {
                    Err("array reference required")?
                };
                let size = {
                    let store = self.store.borrow();
                    let CompositeType::ArrayType(array_type) =
                        store.types.get(*typeidx as usize).ok_or("type not found")?
                    else {
                        Err("array type required")?
                    };
                    array_type.ty.size_of() as usize
                };
                let end = start + size;
                let mut store = self.store.borrow_mut();
                let array = store.arrays.get_mut(array_ref).ok_or("array not found")?;
                if end > array.values.len() {
                    Err("invalid index")?
                }
                array.values[start..end].copy_from_slice(&value.to_vec_u8());
            }
            WasmGCInstr::ArrayLen => {
                let Value::ArrayRef(array_ref) = self.stack.pop().ok_or("expected value")? else {
                    Err("array reference required")?
                };
                let store = self.store.borrow();
                let array = store.arrays.get(array_ref).ok_or("array not found")?;
                let value_size = array.ty.ty.size_of() as usize;
                self.stack
                    .push(Value::I32((array.values.len() / value_size) as i32));
            }
        }
        Ok(())
    }

    #[cfg(feature = "wasm")]
    pub(super) fn exec_wasm_gc_instr(&mut self, instr: &WasmGCInstr) -> Result<(), String> {
        Err("wasmgc feature is not enabled".into())
    }
}
