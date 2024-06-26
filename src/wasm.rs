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

#[repr(u8)]
#[derive(Debug, PartialEq)]
pub enum Opcode {
    I64Const = 0x42,
    I64Add = 0x7c,
    I64LtU = 0x54,
    LocalSet = 0x21,
    LocalGet = 0x20,
    BrIf = 0x0d,
    Loop = 0x03,
}

impl TryFrom<u8> for Opcode {
    type Error = String;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0x42 => Ok(Opcode::I64Const),
            0x7c => Ok(Opcode::I64Add),
            0x54 => Ok(Opcode::I64LtU),
            0x21 => Ok(Opcode::LocalSet),
            0x20 => Ok(Opcode::LocalGet),
            0x0d => Ok(Opcode::BrIf),
            0x03 => Ok(Opcode::Loop),
            _ => Err(format!("Invalid opcode: 0x{:02x}", value)),
        }
    }
}
