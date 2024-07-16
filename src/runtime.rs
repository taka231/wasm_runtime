use std::collections::HashMap;

use crate::wasm::{
    ExportDesc, Func, FuncType, ImportDesc, Instr, Locals, Memarg, Opcode, Section, SectionContent,
    TruncSatOp, TypeIdx, ValType,
};

#[derive(Debug)]
pub struct Runtime {
    pub stack: Vec<Value>,
    pub codes: Option<Vec<Func>>,
    pub func_types: Option<Vec<TypeIdx>>,
    pub types: Vec<FuncType>,
    pub imports: Option<HashMap<(String, String), ImportDesc>>,
    pub exports: Option<HashMap<String, ExportDesc>>,
    pub memory: Memory,
    pub func_offset: u32,
    pub frames: Vec<Frame>,
}

#[derive(Debug)]
pub struct Frame {
    pub pc: usize,
    pub sp: usize,
    pub instrs: Vec<Instr>,
    pub return_num: usize,
    pub locals: Vec<Value>,
    pub labels: Vec<Label>,
}

#[derive(Debug)]
pub struct Memory {
    pub data: Vec<u8>,
    pub max: Option<u32>,
}

impl Memory {
    fn store(&mut self, offset: u32, index: u32, size: u32, value: &[u8]) -> Result<(), String> {
        let addr = offset + index;
        let addr = addr as usize;
        let size = size as usize;
        if addr + size > self.data.len() {
            return Err("Out of memory".to_string());
        }
        self.data[addr..addr + size].copy_from_slice(value);
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

impl Value {
    fn as_i32(&self) -> Result<i32, String> {
        match self {
            Value::I32(x) => Ok(*x),
            _ => Err("Expected i32".to_string()),
        }
    }
    fn as_i64(&self) -> Result<i64, String> {
        match self {
            Value::I64(x) => Ok(*x),
            _ => Err("Expected i64".to_string()),
        }
    }
    fn as_f32(&self) -> Result<f32, String> {
        match self {
            Value::F32(x) => Ok(*x),
            _ => Err("Expected f32".to_string()),
        }
    }
    fn as_f64(&self) -> Result<f64, String> {
        match self {
            Value::F64(x) => Ok(*x),
            _ => Err("Expected f64".to_string()),
        }
    }
    fn is_nan(&self) -> bool {
        match self {
            Value::F32(x) => x.is_nan(),
            Value::F64(x) => x.is_nan(),
            _ => false,
        }
    }
    fn is_pos_inf(&self) -> bool {
        match self {
            Value::F32(x) => x.is_infinite() && x.is_sign_positive(),
            Value::F64(x) => x.is_infinite() && x.is_sign_positive(),
            _ => false,
        }
    }
    fn is_neg_inf(&self) -> bool {
        match self {
            Value::F32(x) => x.is_infinite() && x.is_sign_negative(),
            Value::F64(x) => x.is_infinite() && x.is_sign_negative(),
            _ => false,
        }
    }
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

#[derive(Debug, Clone)]
pub struct Label {
    sp: usize,
    return_num: usize,
    jump_pc: usize,
}

impl Runtime {
    pub fn new(sections: Vec<Section>) -> Runtime {
        let mut codes = None;
        let mut types = None;
        let mut func_types = None;
        let mut imports = None;
        let mut exports = None;
        let mut func_offset = 0;
        let mut memory = Memory {
            data: vec![],
            max: None,
        };
        for section in sections {
            if let SectionContent::Code(funcs) = section.content {
                codes = Some(funcs);
            } else if let SectionContent::Function(functypes) = section.content {
                func_types = Some(functypes);
            } else if let SectionContent::Type(func_types) = section.content {
                types = Some(func_types);
            } else if let SectionContent::Import {
                import_map,
                import_func_count,
            } = section.content
            {
                imports = Some(import_map);
                func_offset = import_func_count;
            } else if let SectionContent::Export(export_map) = section.content {
                exports = Some(export_map);
            } else if let SectionContent::Memory(limits) = section.content {
                memory.data = vec![0; 8192 * limits[0].min as usize];
                memory.max = limits[0].max;
            }
        }
        if types.is_none() {
            panic!("Type section not found");
        }
        Runtime {
            stack: Vec::new(),
            codes,
            func_types,
            types: types.unwrap(),
            imports,
            exports,
            memory,
            func_offset,
            frames: Vec::new(),
        }
    }

    pub fn call_with_name(&mut self, name: &str, args: Vec<Value>) -> Result<Vec<Value>, String> {
        let exports = self.exports.as_ref().ok_or("Export section not found")?;
        let export_desc = exports.get(name).ok_or("Export not found")?;
        let func_idx = match export_desc {
            ExportDesc::Func(idx) => *idx as usize,
            _ => return Err("Export is not a function".to_string()),
        };
        let func_types = self.func_types.as_ref().ok_or("Function type not found")?;
        let func_type_idx = func_types[func_idx];
        for arg in args {
            self.stack.push(arg);
        }
        self.call(func_idx)?;
        let func_type = self
            .types
            .get(func_type_idx as usize)
            .ok_or("Type not found")?;
        let result_num = func_type.results.len();
        let results = self.stack.split_off(self.stack.len() - result_num);
        Ok(results)
    }

    pub fn call(&mut self, idx: usize) -> Result<(), String> {
        let func = self
            .codes
            .as_ref()
            .unwrap()
            .get(idx)
            .ok_or("Function not found")?;
        let func_types = self.func_types.as_ref().ok_or("Function type not found")?;
        let fun_type_idx = func_types[idx];
        let fun_type = self
            .types
            .get(fun_type_idx as usize)
            .ok_or("Type not found")?;
        let bottom = self.stack.len() - fun_type.params.len();
        let mut locals = self.stack.split_off(bottom);
        for Locals { count, ty } in &func.locals {
            for _ in 0..*count {
                locals.push(match ty {
                    ValType::I32 => Value::I32(0),
                    ValType::I64 => Value::I64(0),
                    ValType::F32 => Value::F32(0.0),
                    ValType::F64 => Value::F64(0.0),
                    ValType::V128 => todo!(),
                    ValType::FuncRef => todo!(),
                    ValType::ExternRef => todo!(),
                });
            }
        }
        let mut frame = Frame {
            pc: 0,
            sp: self.stack.len(),
            instrs: func.instrs.clone(),
            return_num: fun_type.results.len(),
            locals,
            labels: vec![],
        };
        loop {
            let pc = frame.pc;
            match &frame.instrs[pc] {
                Instr::Nop => {}
                Instr::I64Const(n) => {
                    self.stack.push(n.into());
                }
                Instr::I32Const(n) => {
                    self.stack.push(n.into());
                }
                Instr::F32Const(f) => {
                    self.stack.push(f.into());
                }
                Instr::F64Const(f) => {
                    self.stack.push(f.into());
                }
                Instr::LocalSet(n) => {
                    let n = *n;
                    let value = self.stack.pop().ok_or("expected value")?;
                    frame.locals[n as usize] = value.into();
                }
                Instr::LocalGet(n) => {
                    let value = frame.locals[*n as usize].clone();
                    self.stack.push(value);
                }
                Instr::MemoryInstrWithMemarg(op, Memarg { offset, .. }) => {
                    use Opcode::*;
                    match op {
                        I32Store => {
                            let value = self.stack.pop().ok_or("expected value")?.as_i32()?;
                            let addr = self.stack.pop().ok_or("expected value")?.as_i32()?;
                            self.memory
                                .store(*offset, addr as u32, 4, &value.to_le_bytes())?;
                        }
                        I64Store => {
                            let value = self.stack.pop().ok_or("expected value")?.as_i64()?;
                            let addr = self.stack.pop().ok_or("expected value")?.as_i32()?;
                            self.memory
                                .store(*offset, addr as u32, 8, &value.to_le_bytes())?;
                        }
                        _ => unreachable!("opcode {:?} is not a memory instruction", op),
                    }
                }
                Instr::Block {
                    block_type,
                    jump_pc,
                } => {
                    let param_num = block_type.count_args(&self.types);
                    frame.labels.push(Label {
                        sp: self.stack.len() - param_num,
                        return_num: block_type.count_results(&self.types),
                        jump_pc: *jump_pc,
                    });
                }
                Instr::Loop {
                    block_type,
                    jump_pc,
                } => {
                    let param_num = block_type.count_args(&self.types);
                    frame.labels.push(Label {
                        sp: self.stack.len() - param_num,
                        return_num: block_type.count_results(&self.types),
                        jump_pc: *jump_pc,
                    });
                }
                Instr::If {
                    block_type,
                    jump_pc,
                } => {
                    let value = self.stack.pop().ok_or("expected value")?;
                    let param_num = block_type.count_args(&self.types);
                    let mut jump_pc = *jump_pc;
                    if value.as_i32()? == 0 {
                        frame.pc = jump_pc;
                        if let Instr::Else { jump_pc: else_pc } = &frame.instrs[jump_pc] {
                            jump_pc = *else_pc;
                        } else {
                            // jump before the end instruction
                            frame.pc -= 1;
                        }
                    }
                    frame.labels.push(Label {
                        sp: self.stack.len() - param_num,
                        return_num: block_type.count_results(&self.types),
                        jump_pc,
                    });
                }
                Instr::Br(n) => {
                    let n = *n as usize;
                    let is_func_end = self.pop_labels(&mut frame, n)?;
                    if is_func_end {
                        break;
                    }
                    continue;
                }
                Instr::BrIf(n) => {
                    let n = *n as usize;
                    let value = self.stack.pop().ok_or("expected value")?;
                    if value.as_i32()? != 0 {
                        let is_func_end = self.pop_labels(&mut frame, n)?;
                        if is_func_end {
                            break;
                        }
                        continue;
                    }
                }
                Instr::BrTable(labels, default) => {
                    let n = self.stack.pop().ok_or("expected value")?.as_i32()? as usize;
                    let n = if n < labels.len() {
                        labels[n]
                    } else {
                        *default
                    } as usize;
                    let is_func_end = self.pop_labels(&mut frame, n)?;
                    if is_func_end {
                        break;
                    }
                    continue;
                }
                Instr::End => match frame.labels.pop() {
                    Some(Label { sp, return_num, .. }) => {
                        self.trunc_and_stack_results(sp, return_num)?;
                    }
                    None => break,
                },
                Instr::Else { jump_pc } => {
                    frame.pc = *jump_pc;
                    match frame.labels.pop() {
                        Some(Label { sp, return_num, .. }) => {
                            self.trunc_and_stack_results(sp, return_num)?;
                        }
                        None => Err("Expected label")?,
                    }
                }
                Instr::Return => break,
                Instr::Call(n) => {
                    let n = *n;
                    self.frames.push(frame);
                    self.call(n as usize)?;
                    frame = self.frames.pop().ok_or("expected frame")?;
                }
                Instr::Drop => {
                    self.stack.pop().ok_or("expected value")?;
                }
                Instr::Select => {
                    let selector = self.stack.pop().ok_or("expected value")?.as_i32()?;
                    let b = self.stack.pop().ok_or("expected value")?;
                    let a = self.stack.pop().ok_or("expected value")?;
                    if selector != 0 {
                        self.stack.push(a);
                    } else {
                        self.stack.push(b);
                    }
                }
                Instr::Ibinop(op) => {
                    let b = self.stack.pop().ok_or("expected value")?;
                    let a = self.stack.pop().ok_or("expected value")?;
                    use Opcode::*;
                    let result = match op {
                        I32Add => (a.as_i32()?.wrapping_add(b.as_i32()?)).into(),
                        I32Sub => (a.as_i32()?.wrapping_sub(b.as_i32()?)).into(),
                        I32Mul => (a.as_i32()?.wrapping_mul(b.as_i32()?)).into(),
                        I32DivS => (a.as_i32()?.wrapping_div(b.as_i32()?)).into(),
                        I32DivU => {
                            (((a.as_i32()? as u32).wrapping_div(b.as_i32()? as u32)) as i32).into()
                        }
                        I32RemS => (a.as_i32()?.wrapping_rem(b.as_i32()?)).into(),
                        I32RemU => {
                            (((a.as_i32()? as u32).wrapping_rem(b.as_i32()? as u32)) as i32).into()
                        }
                        I32And => (a.as_i32()? & b.as_i32()?).into(),
                        I32Or => (a.as_i32()? | b.as_i32()?).into(),
                        I32Xor => (a.as_i32()? ^ b.as_i32()?).into(),
                        I32Shl => (a.as_i32()?.wrapping_shl(b.as_i32()? as u32)).into(),
                        I32ShrS => (a.as_i32()?.wrapping_shr(b.as_i32()? as u32)).into(),
                        I32ShrU => {
                            (((a.as_i32()? as u32).wrapping_shr(b.as_i32()? as u32)) as i32).into()
                        }
                        I32Rotl => {
                            let a = a.as_i32()? as u32;
                            let b = b.as_i32()? as u32;
                            let b = b % 32;
                            (((a.wrapping_shl(b)) | (a.wrapping_shr(32 - b))) as i32).into()
                        }
                        I32Rotr => {
                            let a = a.as_i32()? as u32;
                            let b = b.as_i32()? as u32;
                            let b = b % 32;
                            (((a.wrapping_shr(b)) | (a.wrapping_shl(32 - b))) as i32).into()
                        }
                        I64Add => (a.as_i64()?.wrapping_add(b.as_i64()?)).into(),
                        I64Sub => (a.as_i64()?.wrapping_sub(b.as_i64()?)).into(),
                        I64Mul => (a.as_i64()?.wrapping_mul(b.as_i64()?)).into(),
                        I64DivS => (a.as_i64()?.wrapping_div(b.as_i64()?)).into(),
                        I64DivU => {
                            (((a.as_i64()? as u64).wrapping_div(b.as_i64()? as u64)) as i64).into()
                        }
                        I64RemS => (a.as_i64()?.wrapping_rem(b.as_i64()?)).into(),
                        I64RemU => {
                            (((a.as_i64()? as u64).wrapping_rem(b.as_i64()? as u64)) as i64).into()
                        }
                        I64And => (a.as_i64()? & b.as_i64()?).into(),
                        I64Or => (a.as_i64()? | b.as_i64()?).into(),
                        I64Xor => (a.as_i64()? ^ b.as_i64()?).into(),
                        I64Shl => (a.as_i64()?.wrapping_shl(b.as_i64()? as u32)).into(),
                        I64ShrS => (a.as_i64()?.wrapping_shr(b.as_i64()? as u32)).into(),
                        I64ShrU => {
                            (((a.as_i64()? as u64).wrapping_shr(b.as_i64()? as u32)) as i64).into()
                        }
                        I64Rotl => {
                            let a = a.as_i64()? as u64;
                            let b = b.as_i64()? as u32;
                            let b = b % 64;
                            (((a.wrapping_shl(b)) | (a.wrapping_shr(64 - b))) as i64).into()
                        }
                        I64Rotr => {
                            let a = a.as_i64()? as u64;
                            let b = b.as_i64()? as u32;
                            let b = b % 64;
                            (((a.wrapping_shr(b)) | (a.wrapping_shl(64 - b))) as i64).into()
                        }
                        _ => unreachable!("opcode {:?} is not a ibinop", op),
                    };
                    self.stack.push(result);
                }
                Instr::Funop(op) => {
                    let a = self.stack.pop().ok_or("expected value")?;
                    use Opcode::*;
                    let result = match op {
                        F32Abs => a.as_f32()?.abs().into(),
                        F32Neg => (-a.as_f32()?).into(),
                        F32Ceil => a.as_f32()?.ceil().into(),
                        F32Floor => a.as_f32()?.floor().into(),
                        F32Trunc => a.as_f32()?.trunc().into(),
                        F32Nearest => a.as_f32()?.round_ties_even().into(),
                        F32Sqrt => a.as_f32()?.sqrt().into(),
                        F64Abs => a.as_f64()?.abs().into(),
                        F64Neg => (-a.as_f64()?).into(),
                        F64Ceil => a.as_f64()?.ceil().into(),
                        F64Floor => a.as_f64()?.floor().into(),
                        F64Trunc => a.as_f64()?.trunc().into(),
                        F64Nearest => a.as_f64()?.round_ties_even().into(),
                        F64Sqrt => a.as_f64()?.sqrt().into(),
                        _ => unreachable!("opcode {:?} is not a funop", op),
                    };
                    self.stack.push(result);
                }
                Instr::Fbinop(op) => {
                    let b = self.stack.pop().ok_or("expected value")?;
                    let a = self.stack.pop().ok_or("expected value")?;
                    use Opcode::*;
                    let result = match op {
                        F32Add => (a.as_f32()? + b.as_f32()?).into(),
                        F32Sub => (a.as_f32()? - b.as_f32()?).into(),
                        F32Mul => (a.as_f32()? * b.as_f32()?).into(),
                        F32Div => (a.as_f32()? / b.as_f32()?).into(),
                        F32Min => {
                            let a = a.as_f32()?;
                            let b = b.as_f32()?;
                            if a.is_nan() || b.is_nan() {
                                f32::NAN
                            } else {
                                a.min(b)
                            }
                            .into()
                        }
                        F32Max => {
                            let a = a.as_f32()?;
                            let b = b.as_f32()?;
                            if a.is_nan() || b.is_nan() {
                                f32::NAN
                            } else {
                                a.max(b)
                            }
                            .into()
                        }
                        F32Copysign => a.as_f32()?.copysign(b.as_f32()?).into(),
                        F64Add => (a.as_f64()? + b.as_f64()?).into(),
                        F64Sub => (a.as_f64()? - b.as_f64()?).into(),
                        F64Mul => (a.as_f64()? * b.as_f64()?).into(),
                        F64Div => (a.as_f64()? / b.as_f64()?).into(),
                        F64Min => {
                            let a = a.as_f64()?;
                            let b = b.as_f64()?;
                            if a.is_nan() || b.is_nan() {
                                f64::NAN
                            } else {
                                a.min(b)
                            }
                            .into()
                        }
                        F64Max => {
                            let a = a.as_f64()?;
                            let b = b.as_f64()?;
                            if a.is_nan() || b.is_nan() {
                                f64::NAN
                            } else {
                                a.max(b)
                            }
                            .into()
                        }
                        F64Copysign => a.as_f64()?.copysign(b.as_f64()?).into(),
                        _ => unreachable!("opcode {:?} is not a fbinop", op),
                    };
                    self.stack.push(result);
                }
                Instr::Irelop(op) => {
                    let b = self.stack.pop().ok_or("expected value")?;
                    let a = self.stack.pop().ok_or("expected value")?;
                    use Opcode::*;
                    let result = match op {
                        I32Eq => ((a.as_i32()? == b.as_i32()?) as i32).into(),
                        I32Ne => ((a.as_i32()? != b.as_i32()?) as i32).into(),
                        I32LtS => ((a.as_i32()? < b.as_i32()?) as i32).into(),
                        I32LtU => (((a.as_i32()? as u32) < (b.as_i32()? as u32)) as i32).into(),
                        I32GtS => ((a.as_i32()? > b.as_i32()?) as i32).into(),
                        I32GtU => (((a.as_i32()? as u32) > (b.as_i32()? as u32)) as i32).into(),
                        I32LeS => ((a.as_i32()? <= b.as_i32()?) as i32).into(),
                        I32LeU => (((a.as_i32()? as u32) <= (b.as_i32()? as u32)) as i32).into(),
                        I32GeS => ((a.as_i32()? >= b.as_i32()?) as i32).into(),
                        I32GeU => (((a.as_i32()? as u32) >= (b.as_i32()? as u32)) as i32).into(),
                        I64Eq => ((a.as_i64()? == b.as_i64()?) as i32).into(),
                        I64Ne => ((a.as_i64()? != b.as_i64()?) as i32).into(),
                        I64LtS => ((a.as_i64()? < b.as_i64()?) as i32).into(),
                        I64LtU => (((a.as_i64()? as u64) < (b.as_i64()? as u64)) as i32).into(),
                        I64GtS => ((a.as_i64()? > b.as_i64()?) as i32).into(),
                        I64GtU => (((a.as_i64()? as u64) > (b.as_i64()? as u64)) as i32).into(),
                        I64LeS => ((a.as_i64()? <= b.as_i64()?) as i32).into(),
                        I64LeU => (((a.as_i64()? as u64) <= (b.as_i64()? as u64)) as i32).into(),
                        I64GeS => ((a.as_i64()? >= b.as_i64()?) as i32).into(),
                        I64GeU => (((a.as_i64()? as u64) >= (b.as_i64()? as u64)) as i32).into(),
                        _ => unreachable!("opcode {:?} is not a relop", op),
                    };
                    self.stack.push(result);
                }
                Instr::Iunop(op) => {
                    let a = self.stack.pop().ok_or("expected value")?;
                    let result = match op {
                        Opcode::I32Clz => (a.as_i32()?.leading_zeros() as i32).into(),
                        Opcode::I32Ctz => (a.as_i32()?.trailing_zeros() as i32).into(),
                        Opcode::I32Popcnt => (a.as_i32()?.count_ones() as i32).into(),
                        Opcode::I64Clz => (a.as_i64()?.leading_zeros() as i64).into(),
                        Opcode::I64Ctz => (a.as_i64()?.trailing_zeros() as i64).into(),
                        Opcode::I64Popcnt => (a.as_i64()?.count_ones() as i64).into(),
                        _ => unreachable!("opcode {:?} is not a unop", op),
                    };
                    self.stack.push(result);
                }
                Instr::Itestop(op) => {
                    let a = self.stack.pop().ok_or("expected value")?;
                    let result = match op {
                        Opcode::I32Eqz => ((a.as_i32()? == 0) as i32).into(),
                        Opcode::I64Eqz => ((a.as_i64()? == 0) as i32).into(),
                        _ => unreachable!("opcode {:?} is not a testop", op),
                    };
                    self.stack.push(result);
                }
                Instr::Cutop(op) => {
                    let a = self.stack.pop().ok_or("expected value")?;
                    use Opcode::*;
                    let result = match op {
                        I32Extend8S => (a.as_i32()? as i8 as i32).into(),
                        I32Extend16S => (a.as_i32()? as i16 as i32).into(),
                        I64Extend8S => (a.as_i64()? as i8 as i64).into(),
                        I64Extend16S => (a.as_i64()? as i16 as i64).into(),
                        I64Extend32S => (a.as_i64()? as i32 as i64).into(),
                        I32WrapI64 => (a.as_i64()? as i32).into(),
                        I64ExtendI32S => (a.as_i32()? as i64).into(),
                        I64ExtendI32U => (a.as_i32()? as u32 as i64).into(),
                        I32TruncF32S => (a.as_f32()?.trunc() as i32).into(),
                        I32TruncF32U => (a.as_f32()?.trunc() as u32 as i32).into(),
                        I32TruncF64S => (a.as_f64()?.trunc() as i32).into(),
                        I32TruncF64U => (a.as_f64()?.trunc() as u32 as i32).into(),
                        I64TruncF32S => (a.as_f32()?.trunc() as i64).into(),
                        I64TruncF32U => (a.as_f32()?.trunc() as u64 as i64).into(),
                        I64TruncF64S => (a.as_f64()?.trunc() as i64).into(),
                        I64TruncF64U => (a.as_f64()?.trunc() as u64 as i64).into(),
                        F32ConvertI32S => (a.as_i32()? as f32).into(),
                        F32ConvertI32U => (a.as_i32()? as u32 as f32).into(),
                        F32ConvertI64S => (a.as_i64()? as f32).into(),
                        F32ConvertI64U => (a.as_i64()? as u64 as f32).into(),
                        F32DemoteF64 => (a.as_f64()? as f32).into(),
                        F64ConvertI32S => (a.as_i32()? as f64).into(),
                        F64ConvertI32U => (a.as_i32()? as u32 as f64).into(),
                        F64ConvertI64S => (a.as_i64()? as f64).into(),
                        F64ConvertI64U => (a.as_i64()? as u64 as f64).into(),
                        F64PromoteF32 => (a.as_f32()? as f64).into(),
                        I32ReinterpretF32 => (a.as_f32()?.to_bits() as i32).into(),
                        I64ReinterpretF64 => (a.as_f64()?.to_bits() as i64).into(),
                        F32ReinterpretI32 => (f32::from_bits(a.as_i32()? as u32)).into(),
                        F64ReinterpretI64 => (f64::from_bits(a.as_i64()? as u64)).into(),
                        _ => unreachable!("opcode {:?} is not a cutop", op),
                    };
                    self.stack.push(result);
                }
                Instr::TruncSat(op) => {
                    let a = self.stack.pop().ok_or("expected value")?;
                    use TruncSatOp::*;
                    let result = if a.is_nan() {
                        match op {
                            I32TruncSatF32S | I32TruncSatF32U | I32TruncSatF64S
                            | I32TruncSatF64U => Value::I32(0),
                            I64TruncSatF32S | I64TruncSatF32U | I64TruncSatF64S
                            | I64TruncSatF64U => Value::I64(0),
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
                    self.stack.push(result);
                }
            }
            frame.pc += 1;
        }
        self.trunc_and_stack_results(frame.sp, frame.return_num)
    }
    fn trunc_and_stack_results(&mut self, sp: usize, result_num: usize) -> Result<(), String> {
        let mut pop_vec = Vec::new();
        for _ in 0..result_num {
            pop_vec.push(self.stack.pop().ok_or("expected value")?);
        }
        self.stack.truncate(sp);
        while let Some(value) = pop_vec.pop() {
            self.stack.push(value);
        }
        Ok(())
    }
    fn pop_labels(&mut self, frame: &mut Frame, count: usize) -> Result<bool, String> {
        for i in (0..=count).rev() {
            match frame.labels.last() {
                Some(Label { jump_pc, .. }) => {
                    frame.pc = *jump_pc;
                    if i != 0 {
                        frame.labels.pop();
                    }
                }
                None => return Ok(true),
            }
        }
        Ok(false)
    }
}
