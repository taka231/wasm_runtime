use std::collections::HashMap;

use crate::wasm::{
    AbsHeapType, CompositeType, Data, ExportDesc, Func, FuncType, ImportDesc, Limits, Mode,
    Modules, RefType, TableType,
};

use super::{value::Value, Runtime};

type Result<T> = std::result::Result<T, String>;

#[cfg(feature = "wasm")]
pub struct Store {
    pub func_instances: Vec<FuncInstance>,
    pub types: Vec<FuncType>,
    pub imports: HashMap<(String, String), ImportDesc>,
    pub exports: HashMap<String, ExportDesc>,
    pub memory: Memory,
    pub global: Vec<Global>,
    pub tables: Vec<Table>,
    pub data: Vec<Vec<u8>>,
}

#[cfg(feature = "wasmgc")]
pub struct Store {
    pub func_instances: Vec<FuncInstance>,
    pub types: Vec<CompositeType>,
    pub imports: HashMap<(String, String), ImportDesc>,
    pub exports: HashMap<String, ExportDesc>,
    pub memory: Memory,
    pub global: Vec<Global>,
    pub tables: Vec<Table>,
    pub data: Vec<Vec<u8>>,
}

impl Store {
    pub fn new(modules: Modules) -> Self {
        let types = modules.types;
        let imports = modules.import.import_map;
        let exports = modules.export;
        let mut memory = if modules.memory.len() >= 1 {
            Memory::new(&modules.memory[0])
        } else {
            Memory {
                data: vec![],
                max: None,
            }
        };
        let mut globals = vec![];
        for global in modules.global {
            let value = Runtime::eval_expr(global.init).unwrap();
            let global = Global {
                value,
                mutable: global.is_mutable,
            };
            globals.push(global);
        }
        let mut tables = vec![];
        for table_type in modules.table {
            #[cfg(feature = "wasmgc")]
            fn tabletype_to_table(table_type: TableType) -> Table {
                match table_type.elem_type {
                    RefType::FUNCREF | RefType::Abs(AbsHeapType::Func) => {
                        Table::Funcs(vec![None; table_type.limits.min as usize])
                    }
                    RefType::EXTERNREF | RefType::Abs(AbsHeapType::Extern) => {
                        Table::Refs(vec![None; table_type.limits.min as usize])
                    }
                    _ => unimplemented!("expected funcref or externref"),
                }
            }
            #[cfg(feature = "wasm")]
            fn tabletype_to_table(table_type: TableType) -> Table {
                match table_type.elem_type {
                    RefType::FuncRef => Table::Funcs(vec![None; table_type.limits.min as usize]),
                    RefType::ExternRef => Table::Refs(vec![None; table_type.limits.min as usize]),
                }
            }
            let table = tabletype_to_table(table_type);
            tables.push(table);
        }
        for element in modules.elem {
            if let Mode::Active { tableidx, offset } = element.mode {
                if element.ref_type == RefType::FUNCREF {
                    let table = &mut tables[tableidx as usize];
                    let offset = match Runtime::eval_expr(offset).unwrap() {
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
        let mut data_ = vec![];
        for data in modules.data {
            match data {
                Data::Active {
                    memidx,
                    offset,
                    data,
                } => {
                    if memidx != 0 {
                        panic!("Invalid memory index");
                    }
                    let offset = match Runtime::eval_expr(offset).unwrap() {
                        Value::I32(n) => n as u32,
                        Value::I64(n) => n as u32,
                        _ => panic!("Invalid offset type"),
                    };
                    let addr = offset as usize;
                    memory.data[addr..addr + data.len()].copy_from_slice(&data);
                    data_.push(data);
                }
                Data::Passive { data } => {
                    data_.push(data);
                }
            }
        }
        let mut func_instances = vec![];
        for (module, name) in modules.import.imported_functions {
            let ImportDesc::Func(typeidx) = imports[&(module.clone(), name.clone())] else {
                panic!("Invalid import type");
            };
            let ty = types[typeidx as usize].clone().try_into().unwrap();
            let func = ExternalFunc { module, name, ty };
            func_instances.push(FuncInstance::External(func));
        }
        for i in 0..modules.func.len() {
            let ty = types[modules.func[i] as usize].clone().try_into().unwrap();
            let code = modules.code[i].clone();
            let func = InternalFunc { code, ty };
            func_instances.push(FuncInstance::Internal(func));
        }

        Self {
            func_instances,
            types,
            imports,
            exports,
            memory,
            global: globals,
            tables,
            data: data_,
        }
    }
    pub fn global_get(&self, index: u32) -> Result<Value> {
        self.global
            .get(index as usize)
            .map(|global| global.value.clone())
            .ok_or("Global not found".to_string())
    }

    pub fn global_set(&mut self, index: u32, value: Value) -> Result<()> {
        if let Some(global) = self.global.get_mut(index as usize) {
            if global.mutable {
                global.value = value;
                Ok(())
            } else {
                Err("Global is immutable".to_string())
            }
        } else {
            Err("Global not found".to_string())
        }
    }
    pub fn memory_init(
        &mut self,
        dataidx: usize,
        src: usize,
        dest: usize,
        size: usize,
    ) -> Result<()> {
        if src + size > self.data[dataidx].len() {
            Err("Out of data")?;
        }
        if dest + size > self.memory.data.len() {
            Err("Out of memory")?;
        }
        if size != 0 {
            self.memory.data[dest..dest + size]
                .copy_from_slice(&self.data[dataidx][src..src + size]);
        }
        Ok(())
    }
    pub fn data_drop(&mut self, dataidx: usize) -> Result<()> {
        if dataidx >= self.data.len() {
            Err("Data not found")?;
        }
        self.data.get_mut(dataidx).ok_or("Data not found")?.clear();
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum FuncInstance {
    Internal(InternalFunc),
    External(ExternalFunc),
}

impl FuncInstance {
    pub fn ty(&self) -> FuncType {
        match self {
            FuncInstance::Internal(func) => func.ty.clone(),
            FuncInstance::External(func) => func.ty.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct InternalFunc {
    pub code: Func,
    pub ty: FuncType,
}

#[derive(Debug, Clone)]
pub struct ExternalFunc {
    pub module: String,
    pub name: String,
    pub ty: FuncType,
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
    pub const PAGE_SIZE: usize = 65536;
    pub fn store(&mut self, offset: u32, index: u32, size: u32, value: &[u8]) -> Result<()> {
        let addr = offset + index;
        let addr = addr as usize;
        let size = size as usize;
        if addr + size > self.data.len() {
            return Err("Out of memory".to_string());
        }
        self.data[addr..addr + size].copy_from_slice(&value[0..size]);
        Ok(())
    }
    pub fn load(&self, offset: u32, index: u32, size: u32) -> Result<&[u8]> {
        let addr = offset + index;
        let addr = addr as usize;
        let size = size as usize;
        if addr + size > self.data.len() {
            return Err("Out of memory".to_string());
        }
        Ok(&self.data[addr..addr + size])
    }
    pub fn size(&self) -> Value {
        Value::I32((self.data.len() / Self::PAGE_SIZE) as i32)
    }
    pub fn grow(&mut self, grow_size: usize) -> Value {
        let current_size = self.data.len() / Self::PAGE_SIZE;
        let new_size = current_size + grow_size as usize;
        let max = self.max.unwrap_or(u32::MAX / Memory::PAGE_SIZE as u32);
        if new_size > max as usize {
            Value::I32(-1)
        } else {
            self.data.resize(new_size * Memory::PAGE_SIZE, 0);
            Value::I32(current_size as i32)
        }
    }
    pub fn fill(&mut self, addr: usize, size: usize, value: u8) -> Result<()> {
        if addr + size > self.data.len() {
            Err("Out of memory")?;
        }
        if size != 0 {
            self.data[addr..addr + size].fill(value);
        }
        Ok(())
    }
    pub fn copy(&mut self, src: usize, dest: usize, size: usize) -> Result<()> {
        if src + size > self.data.len() || dest + size > self.data.len() {
            Err("Out of memory")?;
        }
        if size != 0 {
            let src_data = self.data[src..src + size].to_owned();
            self.data[dest..dest + size].copy_from_slice(&src_data);
        }
        Ok(())
    }
}
