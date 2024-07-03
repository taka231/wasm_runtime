use crate::wasm::{Instr, Opcode, Section, SectionContent};

#[derive(Debug)]
pub struct Runtime {
    pub instrs: Vec<Instr>,
    pub pc: usize,
    pub stack: Vec<Value>,
    pub locals: Vec<Option<Value>>,
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
        for section in sections {
            if let SectionContent::Code(funcs) = section.content {
                let locals_size = funcs[0].locals.iter().map(|x| x.count).sum::<u32>();
                return Runtime {
                    instrs: funcs[0].instrs.clone(),
                    pc: 0,
                    stack: vec![],
                    locals: vec![None; locals_size as usize],
                    labels: vec![],
                };
            }
        }
        panic!("No code section found");
    }
    pub fn run(&mut self) -> Result<(), String> {
        loop {
            let pc = self.pc;
            match &self.instrs[pc] {
                Instr::I64Const(n) => {
                    self.stack.push(n.into());
                }
                Instr::I32Const(n) => {
                    self.stack.push(n.into());
                }
                Instr::LocalSet(n) => {
                    let n = *n;
                    let value = self.stack.pop().ok_or("expected value")?;
                    self.locals[n as usize] = value.into();
                }
                Instr::LocalGet(n) => {
                    let value = self.locals[*n as usize]
                        .clone()
                        .ok_or("Local not initialized")?;
                    self.stack.push(value);
                }
                Instr::Loop {
                    block_type: _,
                    jump_pc,
                } => {
                    self.labels.push(Label {
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
                            match self.labels.last() {
                                Some(Label {
                                    sp,
                                    jump_pc,
                                    is_loop,
                                }) => {
                                    self.stack.truncate(*sp);
                                    self.pc = *jump_pc;
                                    if !(*is_loop && i == 0) {
                                        self.labels.pop();
                                    }
                                }
                                None => break,
                            }
                        }
                    }
                }
                Instr::End => match self.labels.pop() {
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
                        I32Add => (a.as_i32()? + b.as_i32()?).into(),
                        I32Sub => (a.as_i32()? - b.as_i32()?).into(),
                        I32Mul => (a.as_i32()? * b.as_i32()?).into(),
                        I32DivS => (a.as_i32()? / b.as_i32()?).into(),
                        I32DivU => ((a.as_i32()? as u32 / b.as_i32()? as u32) as i32).into(),
                        I32RemS => (a.as_i32()? % b.as_i32()?).into(),
                        I32RemU => ((a.as_i32()? as u32 % b.as_i32()? as u32) as i32).into(),
                        I32And => (a.as_i32()? & b.as_i32()?).into(),
                        I32Or => (a.as_i32()? | b.as_i32()?).into(),
                        I32Xor => (a.as_i32()? ^ b.as_i32()?).into(),
                        I32Shl => (a.as_i32()? << b.as_i32()?).into(),
                        I32ShrS => (a.as_i32()? >> b.as_i32()?).into(),
                        I32ShrU => ((a.as_i32()? as u32 >> b.as_i32()? as u32) as i32).into(),
                        I32Rotl => {
                            let a = a.as_i32()?;
                            let b = b.as_i32()?;
                            ((a << b) | (a >> (32 - b))).into()
                        }
                        I32Rotr => {
                            let a = a.as_i32()?;
                            let b = b.as_i32()?;
                            ((a >> b) | (a << (32 - b))).into()
                        }
                        I64Add => (a.as_i64()? + b.as_i64()?).into(),
                        I64Sub => (a.as_i64()? - b.as_i64()?).into(),
                        I64Mul => (a.as_i64()? * b.as_i64()?).into(),
                        I64DivS => (a.as_i64()? / b.as_i64()?).into(),
                        I64DivU => ((a.as_i64()? as u64 / b.as_i64()? as u64) as i64).into(),
                        I64RemS => (a.as_i64()? % b.as_i64()?).into(),
                        I64RemU => ((a.as_i64()? as u64 % b.as_i64()? as u64) as i64).into(),
                        I64And => (a.as_i64()? & b.as_i64()?).into(),
                        I64Or => (a.as_i64()? | b.as_i64()?).into(),
                        I64Xor => (a.as_i64()? ^ b.as_i64()?).into(),
                        I64Shl => (a.as_i64()? << b.as_i64()?).into(),
                        I64ShrS => (a.as_i64()? >> b.as_i64()?).into(),
                        I64ShrU => ((a.as_i64()? as u64 >> b.as_i64()? as u64) as i64).into(),
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
                Instr::Iunop(_) => todo!(),
                Instr::Itestop(_) => todo!(),
            }
            self.pc += 1;
        }
        Ok(())
    }
}
