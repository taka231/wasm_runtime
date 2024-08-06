mod impl_instr;
pub mod value;

use std::collections::HashMap;

use value::Value;

use crate::wasm::{
    Data, ExportDesc, Func, FuncType, ImportDesc, Instr, Limits, Locals, Memarg, Mode, Opcode,
    RefType, Section, SectionContent, TypeIdx, ValType,
};

#[derive(Debug)]
pub struct Runtime {
    pub stack: Vec<Value>,
    pub codes: Option<Vec<Func>>,
    pub func_types: Option<Vec<TypeIdx>>,
    pub types: Option<Vec<FuncType>>,
    pub imports: Option<HashMap<(String, String), ImportDesc>>,
    pub exports: Option<HashMap<String, ExportDesc>>,
    pub memory: Memory,
    pub global: Vec<Global>,
    pub tables: Vec<Table>,
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
pub enum Table {
    Funcs(Vec<Option<usize>>),
    Refs(Vec<Option<Value>>),
}

#[derive(Debug)]
pub struct Global {
    pub value: Value,
    pub mutable: bool,
}

#[derive(Debug)]
pub struct Memory {
    pub data: Vec<u8>,
    pub max: Option<u32>,
}

impl Memory {
    fn new(limit: &Limits) -> Memory {
        Memory {
            data: vec![0; Self::PAGE_SIZE * limit.min as usize],
            max: limit.max,
        }
    }
    const PAGE_SIZE: usize = 65536;
    fn store(&mut self, offset: u32, index: u32, size: u32, value: &[u8]) -> Result<(), String> {
        let addr = offset + index;
        let addr = addr as usize;
        let size = size as usize;
        if addr + size > self.data.len() {
            return Err("Out of memory".to_string());
        }
        self.data[addr..addr + size].copy_from_slice(&value[0..size]);
        Ok(())
    }
    fn load(&self, offset: u32, index: u32, size: u32) -> Result<&[u8], String> {
        let addr = offset + index;
        let addr = addr as usize;
        let size = size as usize;
        if addr + size > self.data.len() {
            return Err("Out of memory".to_string());
        }
        Ok(&self.data[addr..addr + size])
    }
}

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
        let mut tables = Vec::new();
        let mut globals = Vec::new();
        for section in sections {
            match section.content {
                SectionContent::Code(funcs) => {
                    codes = Some(funcs);
                }
                SectionContent::Function(functypes) => {
                    func_types = Some(functypes);
                }
                SectionContent::Type(func_types) => {
                    types = Some(func_types);
                }
                SectionContent::Import {
                    import_map,
                    import_func_count,
                } => {
                    imports = Some(import_map);
                    func_offset = import_func_count;
                }
                SectionContent::Export(export_map) => {
                    exports = Some(export_map);
                }
                SectionContent::Memory(limits) => {
                    memory = Memory::new(&limits[0]);
                }
                SectionContent::Table(table_types) => {
                    for table_type in table_types {
                        let table = match table_type.elem_type {
                            RefType::FuncRef => {
                                Table::Funcs(vec![None; table_type.limits.min as usize])
                            }
                            RefType::ExternRef => {
                                Table::Refs(vec![None; table_type.limits.min as usize])
                            }
                        };
                        tables.push(table);
                    }
                }
                SectionContent::Element(elements) => {
                    for element in elements {
                        if let Mode::Active { tableidx, offset } = element.mode {
                            if element.ref_type == RefType::FuncRef {
                                let table = &mut tables[tableidx as usize];
                                let offset = match Self::eval_expr(offset).unwrap() {
                                    Value::I32(n) => n as usize,
                                    Value::I64(n) => n as usize,
                                    _ => panic!("Invalid offset type"),
                                };
                                match table {
                                    Table::Funcs(funcs) => {
                                        for (i, funcidx) in element.funcidxs.iter().enumerate() {
                                            funcs[offset as usize + i] = Some(*funcidx as usize);
                                        }
                                    }
                                    _ => panic!("Invalid table type"),
                                }
                            }
                        }
                    }
                }
                SectionContent::Global(globals_) => {
                    for global in globals_ {
                        let value = Self::eval_expr(global.init).unwrap();
                        let global = Global {
                            value,
                            mutable: global.is_mutable,
                        };
                        globals.push(global);
                    }
                }
                SectionContent::Data(data) => {
                    for data in data {
                        match data {
                            Data::Active {
                                memidx,
                                offset,
                                data,
                            } => {
                                if memidx != 0 {
                                    panic!("Invalid memory index");
                                }
                                let offset = match Self::eval_expr(offset).unwrap() {
                                    Value::I32(n) => n as u32,
                                    Value::I64(n) => n as u32,
                                    _ => panic!("Invalid offset type"),
                                };
                                let addr = offset as usize;
                                memory.data[addr..addr + data.len()].copy_from_slice(&data);
                            }
                            Data::Passive { data: _ } => todo!(),
                        }
                    }
                }
                _ => {}
            }
        }

        Runtime {
            stack: Vec::new(),
            codes,
            func_types,
            types,
            imports,
            exports,
            memory,
            tables,
            global: globals,
            func_offset,
            frames: Vec::new(),
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
        let types = self.types.as_ref().unwrap();
        let func_type = types.get(func_type_idx as usize).ok_or("Type not found")?;
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
            .as_ref()
            .unwrap()
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
                    let n = *n;
                    let value = self.stack.pop().ok_or("expected value")?;
                    frame.locals[n as usize] = value.into();
                }
                Instr::LocalTee(n) => {
                    let n = *n;
                    let value = self.stack.last().ok_or("expected value")?;
                    frame.locals[n as usize] = value.clone();
                }
                Instr::LocalGet(n) => {
                    let value = frame.locals[*n as usize].clone();
                    self.stack.push(value);
                }
                Instr::GlobalGet(n) => {
                    let global = &self.global[*n as usize];
                    self.stack.push(global.value.clone());
                }
                Instr::GlobalSet(n) => {
                    let global = &mut self.global[*n as usize];
                    if global.mutable {
                        let value = self.stack.pop().ok_or("expected value")?;
                        global.value = value;
                    } else {
                        Err("Global is immutable")?;
                    }
                }
                Instr::MemoryInstrWithMemarg(op, Memarg { offset, .. }) => {
                    self.exec_memory_instr_with_memarg(op, offset)?;
                }
                Instr::Block {
                    block_type,
                    jump_pc,
                } => {
                    let param_num = block_type.count_args(self.types.as_ref().unwrap());
                    frame.labels.push(Label {
                        sp: self.stack.len() - param_num,
                        return_num: block_type.count_results(self.types.as_ref().unwrap()),
                        jump_pc: *jump_pc,
                    });
                }
                Instr::Loop {
                    block_type,
                    jump_pc,
                } => {
                    let param_num = block_type.count_args(self.types.as_ref().unwrap());
                    frame.labels.push(Label {
                        sp: self.stack.len() - param_num,
                        return_num: block_type.count_results(self.types.as_ref().unwrap()),
                        jump_pc: *jump_pc,
                    });
                }
                Instr::If {
                    block_type,
                    jump_pc,
                } => {
                    let value = self.stack.pop().ok_or("expected value")?;
                    let param_num = block_type.count_args(self.types.as_ref().unwrap());
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
                        return_num: block_type.count_results(self.types.as_ref().unwrap()),
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
                    let funcidx = match &self.tables[tableidx as usize] {
                        Table::Funcs(funcs) => {
                            let index = self.stack.pop().ok_or("expected value")?.as_i32()?;
                            funcs[index as usize].ok_or("expected function index")?
                        }
                        _ => Err("Invalid table type")?,
                    };
                    let func_type = &self.types.as_ref().unwrap()[typeidx as usize];
                    let called_func_type = &self.types.as_ref().unwrap()
                        [self.func_types.as_ref().unwrap()[funcidx] as usize];
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
                Instr::MemorySize => {
                    let current_size = self.memory.data.len() / Memory::PAGE_SIZE;
                    self.stack.push(Value::I32(current_size as i32));
                }
                Instr::MemoryGrow => {
                    let grow_size = self.stack.pop().ok_or("expected value")?.as_i32()?;
                    let current_size = self.memory.data.len() / Memory::PAGE_SIZE;
                    let new_size = current_size + grow_size as usize;
                    if let Some(max) = self.memory.max {
                        if new_size > max as usize {
                            self.stack.push((-1).into());
                            continue;
                        }
                    }
                    self.memory.data.resize(new_size * Memory::PAGE_SIZE, 0);
                    self.stack.push(Value::I32(current_size as i32));
                }
                Instr::MemoryFill => {
                    let size = self.stack.pop().ok_or("expected value")?.as_i32()? as usize;
                    let val = self.stack.pop().ok_or("expected value")?.as_i32()?;
                    let addr = self.stack.pop().ok_or("expected value")?.as_i32()? as usize;
                    if addr + size > self.memory.data.len() {
                        Err("Out of memory")?;
                    }
                    if size != 0 {
                        self.memory.data[addr..addr + size].fill(val as u8);
                    }
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
