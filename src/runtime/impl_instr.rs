use crate::{runtime::Value, wasm::Opcode};

use super::Runtime;

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
                let value = self.memory.load(*offset, addr, size)?;
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
                self.memory.store(*offset, addr, size, &value)?;
            }
            _ => unreachable!("opcode {:?} is not a memory instruction", op),
        };
        Ok(())
    }
}
