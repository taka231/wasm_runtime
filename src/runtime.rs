mod impl_instr;
pub mod store;
pub mod value;

use std::{cell::RefCell, rc::Rc};

use store::{FuncInstance, Store, Table};
use value::Value;

use crate::wasm::{ExportDesc, Instr, Locals, Memarg, Modules, Opcode, ValType};

#[derive(Debug)]
pub struct Runtime {
    pub stack: Vec<Value>,
    pub frames: Vec<Frame>,
    pub store: Rc<RefCell<Store>>,
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

#[derive(Debug, Clone)]
pub struct Label {
    sp: usize,
    return_num: usize,
    jump_pc: usize,
}

impl Runtime {
    pub fn new(modules: Modules) -> Runtime {
        let store = Rc::new(RefCell::new(Store::new(modules)));
        Runtime {
            stack: vec![],
            frames: vec![],
            store,
        }
    }

    pub fn eval_expr(instrs: Vec<Instr>) -> Result<Value, String> {
        if instrs.len() >= 2 {
            unimplemented!("expr with more than 1 instr");
        }
        Ok(match instrs[0] {
            Instr::I32Const(n) => Value::I32(n),
            Instr::I64Const(n) => Value::I64(n),
            Instr::F32Const(f) => Value::F32(f),
            Instr::F64Const(f) => Value::F64(f),
            Instr::RefNull(ref_type) => Value::RefNull(ref_type),
            Instr::RefFunc(funcidx) => Value::FuncRef(funcidx as usize),
            _ => unimplemented!(),
        })
    }

    pub fn call_with_name(&mut self, name: &str, args: Vec<Value>) -> Result<Vec<Value>, String> {
        let export_desc = self
            .store
            .borrow()
            .exports
            .get(name)
            .ok_or("Export not found")?
            .clone();
        let func_idx = match export_desc {
            ExportDesc::Func(idx) => idx as usize,
            _ => return Err("Export is not a function".to_string()),
        };
        for arg in args {
            self.stack.push(arg);
        }
        self.call(func_idx)?;
        let func_type = self
            .store
            .borrow()
            .func_instances
            .get(func_idx)
            .ok_or("Function not found")?
            .ty();
        let result_num = func_type.results.len();
        let results = self.stack.split_off(self.stack.len() - result_num);
        Ok(results)
    }

    pub fn call(&mut self, idx: usize) -> Result<(), String> {
        let func = self
            .store
            .borrow()
            .func_instances
            .get(idx)
            .ok_or("Function not found")?
            .clone();
        let FuncInstance::Internal(func) = func else {
            unimplemented!("External function");
        };
        let bottom = self.stack.len() - func.ty.params.len();
        let mut locals = self.stack.split_off(bottom);
        for Locals { count, ty } in &func.code.locals {
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
            instrs: func.code.instrs.clone(),
            return_num: func.ty.results.len(),
            locals,
            labels: vec![],
        };
        loop {
            let pc = frame.pc;
            match &frame.instrs[pc] {
                Instr::Unreachable => Err("unreachable")?,
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
                    let value = self.stack.pop().ok_or("expected value")?;
                    frame.locals[*n as usize] = value.into();
                }
                Instr::LocalTee(n) => {
                    let value = self.stack.last().ok_or("expected value")?;
                    frame.locals[*n as usize] = value.clone();
                }
                Instr::LocalGet(n) => {
                    let value = frame.locals[*n as usize].clone();
                    self.stack.push(value);
                }
                Instr::GlobalGet(n) => {
                    let value = self.store.borrow().global_get(*n)?;
                    self.stack.push(value);
                }
                Instr::GlobalSet(n) => {
                    let value = self.stack.pop().ok_or("expected value")?;
                    self.store.borrow_mut().global_set(*n, value)?;
                }
                Instr::MemoryInstrWithMemarg(op, Memarg { offset, .. }) => {
                    self.exec_memory_instr_with_memarg(op, offset)?;
                }
                Instr::Block {
                    block_type,
                    jump_pc,
                } => {
                    let types = &self.store.borrow().types;
                    let param_num = block_type.count_args(&types);
                    frame.labels.push(Label {
                        sp: self.stack.len() - param_num,
                        return_num: block_type.count_results(&types),
                        jump_pc: *jump_pc,
                    });
                }
                Instr::Loop {
                    block_type,
                    jump_pc,
                } => {
                    let types = &self.store.borrow().types;
                    let param_num = block_type.count_args(&types);
                    frame.labels.push(Label {
                        sp: self.stack.len() - param_num,
                        return_num: block_type.count_results(&types),
                        jump_pc: *jump_pc,
                    });
                }
                Instr::If {
                    block_type,
                    jump_pc,
                } => {
                    let types = &self.store.borrow().types;
                    let value = self.stack.pop().ok_or("expected value")?;
                    let param_num = block_type.count_args(&types);
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
                        return_num: block_type.count_results(&types),
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
                Instr::CallIndirect(typeidx, tableidx) => {
                    let typeidx = *typeidx;
                    let tableidx = *tableidx;
                    let funcidx = match &self.store.borrow().tables.get(tableidx as usize) {
                        Some(Table::Funcs(funcs)) => {
                            let index = self.stack.pop().ok_or("expected value")?.as_i32()?;
                            funcs[index as usize].ok_or("expected function index")?
                        }
                        None => Err("Table not found")?,
                        _ => Err("Invalid table type")?,
                    };
                    let func_type = self.store.borrow().types[typeidx as usize].clone();
                    let called_func_type = self.store.borrow().func_instances[funcidx].ty();
                    if func_type != called_func_type {
                        Err("Function type mismatch")?;
                    }
                    self.frames.push(frame);
                    self.call(funcidx)?;
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
                Instr::SelectValType(_) => {
                    let selector = self.stack.pop().ok_or("expected value")?.as_i32()?;
                    let b = self.stack.pop().ok_or("expected value")?;
                    let a = self.stack.pop().ok_or("expected value")?;
                    if selector != 0 {
                        self.stack.push(a);
                    } else {
                        self.stack.push(b);
                    }
                }
                Instr::MemorySize => {
                    self.stack.push(self.store.borrow().memory.size());
                }
                Instr::MemoryGrow => {
                    let grow_size = self.stack.pop().ok_or("expected value")?.as_i32()?;
                    self.stack
                        .push(self.store.borrow_mut().memory.grow(grow_size as usize));
                }
                Instr::MemoryFill => {
                    let size = self.stack.pop().ok_or("expected value")?.as_i32()? as usize;
                    let val = self.stack.pop().ok_or("expected value")?.as_i32()?;
                    let addr = self.stack.pop().ok_or("expected value")?.as_i32()? as usize;
                    self.store.borrow_mut().memory.fill(addr, size, val as u8)?;
                }
                Instr::MemoryCopy => {
                    let size = self.stack.pop().ok_or("expected value")?.as_i32()? as usize;
                    let src = self.stack.pop().ok_or("expected value")?.as_i32()? as usize;
                    let dest = self.stack.pop().ok_or("expected value")?.as_i32()? as usize;
                    self.store.borrow_mut().memory.copy(src, dest, size)?;
                }
                Instr::MemoryInit(dataidx) => {
                    let dataidx = *dataidx as usize;
                    let size = self.stack.pop().ok_or("expected value")?.as_i32()? as usize;
                    let src = self.stack.pop().ok_or("expected value")?.as_i32()? as usize;
                    let dest = self.stack.pop().ok_or("expected value")?.as_i32()? as usize;
                    self.store
                        .borrow_mut()
                        .memory_init(dataidx, src, dest, size)?;
                }
                Instr::DataDrop(dataidx) => {
                    self.store.borrow_mut().data_drop(*dataidx as usize)?;
                }
                Instr::Ibinop(op) => {
                    let b = self.stack.pop().ok_or("expected value")?;
                    let a = self.stack.pop().ok_or("expected value")?;
                    let result = Self::exec_ibinop(op, a, b)?;
                    self.stack.push(result);
                }
                Instr::Funop(op) => {
                    let a = self.stack.pop().ok_or("expected value")?;
                    let result = Self::exec_funop(op, a)?;
                    self.stack.push(result);
                }
                Instr::Fbinop(op) => {
                    let b = self.stack.pop().ok_or("expected value")?;
                    let a = self.stack.pop().ok_or("expected value")?;
                    let result = Self::exec_fbinop(op, a, b)?;
                    self.stack.push(result);
                }
                Instr::Frelop(op) => {
                    let b = self.stack.pop().ok_or("expected value")?;
                    let a = self.stack.pop().ok_or("expected value")?;
                    let result = Self::exec_frelop(op, a, b)?;
                    self.stack.push(result);
                }
                Instr::Irelop(op) => {
                    let b = self.stack.pop().ok_or("expected value")?;
                    let a = self.stack.pop().ok_or("expected value")?;
                    let result = Self::exec_irelop(op, a, b)?;
                    self.stack.push(result);
                }
                Instr::Iunop(op) => {
                    let a = self.stack.pop().ok_or("expected value")?;
                    let result = Self::exec_iunop(op, a)?;
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
                    let result = Self::exec_cutop(op, a)?;
                    self.stack.push(result);
                }
                Instr::TruncSat(op) => {
                    let a = self.stack.pop().ok_or("expected value")?;
                    let result = Self::exec_trunc_sat(op, a)?;
                    self.stack.push(result);
                }
                Instr::RefNull(ref_type) => {
                    self.stack.push(Value::RefNull(ref_type.clone()));
                }
                Instr::RefIsNull => {
                    let value = self.stack.pop().ok_or("expected value")?;
                    let result = match value {
                        Value::RefNull(_) => 1,
                        _ => 0,
                    };
                    self.stack.push(Value::I32(result));
                }
                Instr::RefFunc(funcidx) => {
                    let funcidx = *funcidx as usize;
                    self.stack.push(Value::FuncRef(funcidx));
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
