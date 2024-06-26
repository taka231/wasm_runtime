#[derive(Debug)]
pub enum SectionContent {
    Custom { name: String },
    Type,
    Import,
    Function,
    Table,
    Memory,
    Global,
    Export,
    Start,
    Element,
    Code(Vec<Func>),
    Data,
    DataCount,
}

#[derive(Debug, Clone, Copy)]
pub enum ValType {
    I32,
    I64,
    F32,
    F64,
    V128,
    FuncRef,
    ExternRef,
}

#[derive(Debug, Clone)]
pub struct Locals {
    pub count: u32,
    pub ty: ValType,
}

#[derive(Debug)]
pub struct Func {
    pub size: u32,
    pub locals: Vec<Locals>,
    pub instrs: Vec<Instr>,
}

#[derive(Debug, Clone)]
pub enum Instr {
    I64Const(i64),
    I64Add,
    I64LtU,
    LocalSet(u32),
    LocalGet(u32),
    BrIf(u32),
    Loop(Vec<Instr>),
}

#[derive(Debug)]
pub struct Section {
    pub content: SectionContent,
    pub size: u32,
}
