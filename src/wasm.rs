use std::collections::HashMap;

#[derive(Debug)]
pub enum SectionContent {
    Custom {
        name: String,
    },
    Type(Vec<FuncType>),
    Import {
        import_map: HashMap<(String, String), ImportDesc>,
        import_func_count: u32,
    },
    Function(Vec<TypeIdx>),
    Table,
    Memory,
    Global,
    Export(HashMap<String, ExportDesc>),
    Start,
    Element,
    Code(Vec<Func>),
    Data,
    DataCount,
}

#[derive(Debug, Clone)]
pub enum ImportDesc {
    Func(u32),
    Table(u32),
    Memory(u32),
    Global(u32),
}

pub type TypeIdx = u32;

#[derive(Debug, Clone)]
pub enum ExportDesc {
    Func(u32),
    Table(u32),
    Memory(u32),
    Global(u32),
}

macro_rules! enum_try_from_int {
    (
        #[repr($T: ident)]
        $( #[$meta: meta] )*
        $vis: vis enum $Name: ident {
            $(
                $Variant: ident = $value: expr
            ),*
            $( , )?
        }
    ) => {
        #[repr($T)]
        $( #[$meta] )*
        $vis enum $Name {
            $(
                $Variant = $value
            ),*
        }

        impl std::convert::TryFrom<$T> for $Name {
            type Error = ();

            fn try_from(value: $T) -> Result<$Name, ()> {
                match value {
                    $(
                        $value => Ok($Name::$Variant),
                    )*
                    _ => Err(())
                }
            }
        }
    }
}

enum_try_from_int! {
    #[repr(u8)]
    #[derive(Debug, Clone, Copy)]
    pub enum ValType {
        I32 = 0x7f,
        I64 = 0x7e,
        F32 = 0x7d,
        F64 = 0x7c,
        V128 = 0x7b,
        FuncRef = 0x70,
        ExternRef = 0x6f,
    }
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
pub enum BlockType {
    Empty,
    ValType(ValType),
}

pub type ResultType = Vec<ValType>;

#[derive(Debug, Clone)]
pub struct FuncType {
    pub params: ResultType,
    pub results: ResultType,
}

#[derive(Debug, Clone)]
pub enum Instr {
    Loop {
        block_type: BlockType,
        jump_pc: usize,
    },
    End,
    BrIf(u32),
    LocalGet(u32),
    LocalSet(u32),
    I32Const(i32),
    I64Const(i64),
    Iunop(Opcode),
    Ibinop(Opcode),
    Itestop(Opcode),
    Irelop(Opcode),
}

#[derive(Debug)]
pub struct Section {
    pub content: SectionContent,
    pub size: u32,
}

enum_try_from_int! {
    #[repr(u8)]
    #[derive(Debug, Clone, PartialEq)]
    pub enum Opcode {
        Loop = 0x03,
        End = 0x0b,
        BrIf = 0x0d,
        LocalGet = 0x20,
        LocalSet = 0x21,
        I32Const = 0x41,
        I64Const = 0x42,
        I32Eqz = 0x45,
        I32Eq = 0x46,
        I32Ne = 0x47,
        I32LtS = 0x48,
        I32LtU = 0x49,
        I32GtS = 0x4a,
        I32GtU = 0x4b,
        I32LeS = 0x4c,
        I32LeU = 0x4d,
        I32GeS = 0x4e,
        I32GeU = 0x4f,
        I64Eqz = 0x50,
        I64Eq = 0x51,
        I64Ne = 0x52,
        I64LtS = 0x53,
        I64LtU = 0x54,
        I64GtS = 0x55,
        I64GtU = 0x56,
        I64LeS = 0x57,
        I64LeU = 0x58,
        I64GeS = 0x59,
        I64GeU = 0x5a,
        I32Clz = 0x67,
        I32Ctz = 0x68,
        I32Popcnt = 0x69,
        I32Add = 0x6a,
        I32Sub = 0x6b,
        I32Mul = 0x6c,
        I32DivS = 0x6d,
        I32DivU = 0x6e,
        I32RemS = 0x6f,
        I32RemU = 0x70,
        I32And = 0x71,
        I32Or = 0x72,
        I32Xor = 0x73,
        I32Shl = 0x74,
        I32ShrS = 0x75,
        I32ShrU = 0x76,
        I32Rotl = 0x77,
        I32Rotr = 0x78,
        I64Clz = 0x79,
        I64Ctz = 0x7a,
        I64Popcnt = 0x7b,
        I64Add = 0x7c,
        I64Sub = 0x7d,
        I64Mul = 0x7e,
        I64DivS = 0x7f,
        I64DivU = 0x80,
        I64RemS = 0x81,
        I64RemU = 0x82,
        I64And = 0x83,
        I64Or = 0x84,
        I64Xor = 0x85,
        I64Shl = 0x86,
        I64ShrS = 0x87,
        I64ShrU = 0x88,
        I64Rotl = 0x89,
        I64Rotr = 0x8a,
    }
}

impl Opcode {
    pub fn is_iunop(&self) -> bool {
        use Opcode::*;
        match self {
            I32Clz | I32Ctz | I32Popcnt | I64Clz | I64Ctz | I64Popcnt => true,
            _ => false,
        }
    }
    pub fn is_ibinop(&self) -> bool {
        use Opcode::*;
        match self {
            I32Add | I32Sub | I32Mul | I32DivS | I32DivU | I32RemS | I32RemU | I32And | I32Or
            | I32Xor | I32Shl | I32ShrS | I32ShrU | I32Rotl | I32Rotr | I64Add | I64Sub
            | I64Mul | I64DivS | I64DivU | I64RemS | I64RemU | I64And | I64Or | I64Xor | I64Shl
            | I64ShrS | I64ShrU | I64Rotl | I64Rotr => true,
            _ => false,
        }
    }
    pub fn is_itestop(&self) -> bool {
        use Opcode::*;
        match self {
            I32Eqz | I64Eqz => true,
            _ => false,
        }
    }
    pub fn is_irelop(&self) -> bool {
        use Opcode::*;
        match self {
            I32Eq | I32Ne | I32LtS | I32LtU | I32GtS | I32GtU | I32LeS | I32LeU | I32GeS
            | I32GeU | I64Eq | I64Ne | I64LtS | I64LtU | I64GtS | I64GtU | I64LeS | I64LeU
            | I64GeS | I64GeU => true,
            _ => false,
        }
    }
}
