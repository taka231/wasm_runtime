use std::collections::HashMap;

use crate::wasm::{
    ExportDesc, Func, FuncType, ImportDesc, Instr, Locals, Opcode, Section, SectionContent,
    TypeIdx, ValType,
};

#[derive(Debug)]
pub struct Runtime {
    pub stack: Vec<Value>,
    pub codes: Vec<Func>,
    pub func_types: Vec<TypeIdx>,
    pub types: Vec<FuncType>,
    pub imports: Option<HashMap<(String, String), ImportDesc>>,
    pub exports: Option<HashMap<String, ExportDesc>>,
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
    is_loop: bool,
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
            };
        }
        if codes.is_none() {
            panic!("Code section not found");
        } else if types.is_none() {
            panic!("Type section not found");
        } else if func_types.is_none() {
            panic!("Function section not found");
        }

        Runtime {
            stack: Vec::new(),
            codes: codes.unwrap(),
            func_types: func_types.unwrap(),
            types: types.unwrap(),
            imports,
            exports,
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
        let func_type_idx = self.func_types[func_idx];
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
        let func = self.codes.get(idx).ok_or("Function not found")?;
        let fun_type_idx = self.func_types[idx];
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
                Instr::I64Const(n) => {
                    self.stack.push(n.into());
                }
                Instr::I32Const(n) => {
                    self.stack.push(n.into());
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
                Instr::Loop {
                    block_type: _,
                    jump_pc,
                } => {
                    frame.labels.push(Label {
                        sp: self.stack.len(),
                        is_loop: true,
                        jump_pc: *jump_pc,
                    });
                }
                Instr::BrIf(n) => {
                    let n = *n;
                    let value = self.stack.pop().ok_or("expected value")?;
                    if value.as_i32()? != 0 {
                        for i in (0..=n).rev() {
                            match frame.labels.last() {
                                Some(Label {
                                    sp,
                                    jump_pc,
                                    is_loop,
                                }) => {
                                    self.stack.truncate(*sp);
                                    frame.pc = *jump_pc;
                                    if !(*is_loop && i == 0) {
                                        frame.labels.pop();
                                    }
                                }
                                None => break,
                            }
                        }
                    }
                }
                Instr::End => match frame.labels.pop() {
                    Some(Label { sp, .. }) => {
                        self.stack.truncate(sp);
                    }
                    None => break,
                },
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
                            let a = a.as_i64()?;
                            let b = b.as_i64()?;
                            ((a << b) | (a >> (64 - b))).into()
                        }
                        I64Rotr => {
                            let a = a.as_i64()?;
                            let b = b.as_i64()?;
                            ((a >> b) | (a << (64 - b))).into()
                        }
                        _ => unreachable!("opcode {:?} is not a ibinop", op),
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
                Instr::Instr(op) => {
                    use Opcode::*;
                    match op {
                        I32Extend8S => {
                            let a = self.stack.pop().ok_or("expected value")?;
                            self.stack.push((a.as_i32()? as i8 as i32).into());
                        }
                        I32Extend16S => {
                            let a = self.stack.pop().ok_or("expected value")?;
                            self.stack.push((a.as_i32()? as i16 as i32).into());
                        }
                        I64Extend8S => {
                            let a = self.stack.pop().ok_or("expected value")?;
                            self.stack.push((a.as_i64()? as i8 as i64).into());
                        }
                        I64Extend16S => {
                            let a = self.stack.pop().ok_or("expected value")?;
                            self.stack.push((a.as_i64()? as i16 as i64).into());
                        }
                        I64Extend32S => {
                            let a = self.stack.pop().ok_or("expected value")?;
                            self.stack.push((a.as_i64()? as i32 as i64).into());
                        }
                        _ => unimplemented!("opcode {:?} is not implemented", op),
                    }
                }
            }
            frame.pc += 1;
        }
        let result_num = frame.return_num;
        let mut pop_vec = Vec::new();
        for _ in 0..result_num {
            pop_vec.push(self.stack.pop().ok_or("expected value")?);
        }
        self.stack.truncate(frame.sp);
        while let Some(value) = pop_vec.pop() {
            self.stack.push(value);
        }
        Ok(())
    }
}
