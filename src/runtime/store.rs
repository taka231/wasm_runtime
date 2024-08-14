use std::{
    borrow::Borrow,
    collections::{BTreeMap, HashMap, HashSet},
};

use crate::wasm::{
    AbsHeapType, CompositeType, Data, ExportDesc, FieldType, Func, FuncType, HeapType, ImportDesc,
    Limits, Mode, Modules, RefType, StorageType, TableType, ValType,
};

use super::{
    value::{ArrayValue, StructValue, Value},
    Runtime,
};

type Result<T> = std::result::Result<T, String>;

#[cfg(feature = "wasm")]
#[derive(Default, Clone)]
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
#[derive(Default, Clone)]
pub struct Store {
    pub func_instances: Vec<FuncInstance>,
    pub types: Vec<CompositeType>,
    pub strcuttype_offset: BTreeMap<u32, (Vec<u32>, usize)>,
    pub imports: HashMap<(String, String), ImportDesc>,
    pub exports: HashMap<String, ExportDesc>,
    pub memory: Memory,
    pub global: Vec<Global>,
    pub tables: Vec<Table>,
    pub data: Vec<Vec<u8>>,
    pub heap: HeapManager,
}

impl Store {
    pub fn new(modules: Modules) -> Self {
        let mut store = Self::default();
        store.types = modules.types;
        store.imports = modules.import.import_map;
        store.exports = modules.export;
        if modules.memory.len() >= 1 {
            store.memory = Memory::new(&modules.memory[0])
        };
        for global in modules.global {
            let value = Runtime::eval_expr(&mut store, global.init).unwrap();
            let global = Global {
                value,
                mutable: global.is_mutable,
            };
            store.global.push(global);
        }
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
            store.tables.push(table);
        }
        for element in modules.elem {
            if let Mode::Active { tableidx, offset } = element.mode {
                if element.ref_type == RefType::FUNCREF {
                    let offset = match Runtime::eval_expr(&mut store, offset).unwrap() {
                        Value::I32(n) => n as usize,
                        Value::I64(n) => n as usize,
                        _ => panic!("Invalid offset type"),
                    };
                    let table = &mut store.tables[tableidx as usize];
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
                    let offset = match Runtime::eval_expr(&mut store, offset).unwrap() {
                        Value::I32(n) => n as u32,
                        Value::I64(n) => n as u32,
                        _ => panic!("Invalid offset type"),
                    };
                    let addr = offset as usize;
                    store.memory.data[addr..addr + data.len()].copy_from_slice(&data);
                    store.data.push(data);
                }
                Data::Passive { data } => {
                    store.data.push(data);
                }
            }
        }
        for (module, name) in modules.import.imported_functions {
            let ImportDesc::Func(typeidx) = store.imports[&(module.clone(), name.clone())] else {
                panic!("Invalid import type");
            };
            let ty = store.types[typeidx as usize].clone().try_into().unwrap();
            let func = ExternalFunc { module, name, ty };
            store.func_instances.push(FuncInstance::External(func));
        }
        for i in 0..modules.func.len() {
            let ty = store.types[modules.func[i] as usize]
                .clone()
                .try_into()
                .unwrap();
            let code = modules.code[i].clone();
            let func = InternalFunc { code, ty };
            store.func_instances.push(FuncInstance::Internal(func));
        }

        store.calc_struct_offset();

        store
    }

    #[cfg(feature = "wasmgc")]
    fn calc_struct_offset(&mut self) {
        for (i, ty) in self.types.iter().enumerate() {
            if let CompositeType::StructType(fields) = ty {
                let mut offset = 0;
                let mut offsets = vec![];
                for field in fields {
                    offsets.push(offset as u32);
                    offset += field.ty.size_of();
                }
                self.strcuttype_offset.insert(i as u32, (offsets, offset));
            }
        }
    }

    #[cfg(feature = "wasm")]
    fn calc_struct_offset(&mut self) {}

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

    pub fn gc(&mut self, root_set: &[&Value]) -> Result<()> {
        let mut root_set = root_set.to_vec();
        let global = self.global.clone();
        root_set.extend(global.iter().map(|global| &global.value));
        let mut reachable_structs: HashSet<u32> = HashSet::new();
        let mut reachable_arrays: HashSet<u32> = HashSet::new();

        for value in root_set {
            self.mark(value, &mut reachable_structs, &mut reachable_arrays)?;
        }
        for index in self
            .heap
            .structs
            .values
            .keys()
            .cloned()
            .collect::<Vec<u32>>()
        {
            if !reachable_structs.contains(&index) {
                self.heap.structs.values.remove(&index);
            }
        }
        for index in self
            .heap
            .arrays
            .values
            .keys()
            .cloned()
            .collect::<Vec<u32>>()
        {
            if !reachable_arrays.contains(&index) {
                self.heap.arrays.values.remove(&index);
            }
        }
        Ok(())
    }

    fn mark(
        &mut self,
        value: &Value,
        reachable_structs: &mut HashSet<u32>,
        reachable_arrays: &mut HashSet<u32>,
    ) -> Result<()> {
        let heap = &mut self.heap;
        match value {
            Value::StructRef(index) => {
                reachable_structs.insert(*index as u32);
                let struct_value = heap
                    .structs
                    .get(*index as u32)
                    .ok_or("Struct not found")?
                    .clone();
                for (i, field_type) in struct_value.types.iter().enumerate() {
                    if let StorageType::ValType(ValType::RefType(reftype)) = &field_type.ty {
                        let start = self.strcuttype_offset[&(*index as u32)].0[i] as usize;
                        let end = start + field_type.ty.size_of();
                        let field_value = Value::from_vec_u8(
                            &struct_value.values[start..end],
                            &field_type.ty,
                            &self.types,
                        )?;
                        if reftype.is_structref(&self.types) {
                            let Value::StructRef(index) = field_value else {
                                unreachable!()
                            };
                            if !reachable_structs.contains(&(index as u32)) {
                                self.mark(&field_value, reachable_structs, reachable_arrays)?;
                            }
                        } else if reftype.is_arrayref(&self.types) {
                            let Value::ArrayRef(index) = field_value else {
                                unreachable!()
                            };
                            if !reachable_arrays.contains(&(index as u32)) {
                                self.mark(&field_value, reachable_structs, reachable_arrays)?;
                            }
                        }
                    }
                }
            }
            Value::ArrayRef(index) => {
                reachable_arrays.insert(*index as u32);
                let array_value = heap
                    .arrays
                    .get(*index as u32)
                    .ok_or("Array not found")?
                    .clone();
                let field_type = &array_value.ty;
                let field_size = field_type.ty.size_of();
                if let StorageType::ValType(ValType::RefType(reftype)) = &field_type.ty {
                    for i in 0..(array_value.values.len() / field_size) {
                        let start = i * field_size;
                        let end = start + field_size;
                        let field_value = Value::from_vec_u8(
                            &array_value.values[start..end],
                            &field_type.ty,
                            &self.types,
                        )?;
                        if reftype.is_structref(&self.types) {
                            let Value::StructRef(index) = field_value else {
                                unreachable!()
                            };
                            if !reachable_structs.contains(&(index as u32)) {
                                self.mark(&field_value, reachable_structs, reachable_arrays)?;
                            }
                        } else if reftype.is_arrayref(&self.types) {
                            let Value::ArrayRef(index) = field_value else {
                                unreachable!()
                            };
                            if !reachable_arrays.contains(&(index as u32)) {
                                self.mark(&field_value, reachable_structs, reachable_arrays)?;
                            }
                        }
                    }
                }
            }
            _ => {}
        }
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

#[derive(Debug, Clone)]
pub enum Table {
    Funcs(Vec<Option<usize>>),
    Refs(Vec<Option<Value>>),
}

#[derive(Debug, Clone)]
pub struct Global {
    pub value: Value,
    pub mutable: bool,
}

#[derive(Debug, Default, Clone)]
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

#[cfg(feature = "wasmgc")]
#[derive(Debug, Clone)]
pub struct HeapValueManager<T> {
    pub values: HashMap<u32, T>,
    pub num: u32,
}

impl<T> Default for HeapValueManager<T> {
    fn default() -> Self {
        Self {
            values: HashMap::new(),
            num: 0,
        }
    }
}

#[cfg(feature = "wasmgc")]
impl<T> HeapValueManager<T> {
    pub fn alloc(&mut self, value: T) -> u32 {
        self.num += 1;
        self.values.insert(self.num, value);
        self.num
    }

    pub fn get(&self, index: u32) -> Option<&T> {
        self.values.get(&index)
    }

    pub fn get_mut(&mut self, index: u32) -> Option<&mut T> {
        self.values.get_mut(&index)
    }
}

#[cfg(feature = "wasmgc")]
#[derive(Debug, Clone, Default)]
pub struct HeapManager {
    pub structs: HeapValueManager<StructValue>,
    pub arrays: HeapValueManager<ArrayValue>,
}
