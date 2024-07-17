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
    Table(Vec<TableType>),
    Memory(Vec<Limits>),
    Global(Vec<Global>),
    Export(HashMap<String, ExportDesc>),
    Start,
    Element(Vec<Element>),
    Code(Vec<Func>),
    Data,
    DataCount,
}

#[derive(Debug, Clone)]
pub struct Global {
    pub ty: ValType,
    pub is_mutable: bool,
    pub init: Vec<Instr>,
}

#[derive(Debug, Clone)]
pub struct Element {
    pub ref_type: RefType,
    pub mode: Mode,
    pub expr: Vec<Instr>,
    pub funcidxs: Vec<u32>,
}

#[derive(Debug, Clone)]
pub enum Mode {
    Passive,
    Active { tableidx: u32, offset: Vec<Instr> },
    Declarative,
}

#[derive(Debug, Clone)]
pub struct Limits {
    pub min: u32,
    pub max: Option<u32>,
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
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

enum_try_from_int! {
    #[repr(u8)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum RefType {
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
    TypeIdx(TypeIdx),
}

impl BlockType {
    pub fn count_args(&self, types: &Vec<FuncType>) -> usize {
        match self {
            BlockType::Empty => 0,
            BlockType::ValType(_) => 0,
            BlockType::TypeIdx(type_idx) => types[*type_idx as usize].params.len(),
        }
    }
    pub fn count_results(&self, types: &Vec<FuncType>) -> usize {
        match self {
            BlockType::Empty => 0,
            BlockType::ValType(_) => 1,
            BlockType::TypeIdx(type_idx) => types[*type_idx as usize].results.len(),
        }
    }
}

pub type ResultType = Vec<ValType>;

#[derive(Debug, Clone)]
pub struct FuncType {
    pub params: ResultType,
    pub results: ResultType,
}

#[derive(Debug, Clone)]
pub enum Instr {
    Nop,
    Block {
        block_type: BlockType,
        jump_pc: usize,
    },
    Loop {
        block_type: BlockType,
        jump_pc: usize,
    },
    If {
        block_type: BlockType,
        jump_pc: usize,
    },
    Else {
        jump_pc: usize,
    },
    End,
    Return,
    Call(u32),
    CallIndirect(TypeIdx, u32),
    Drop,
    Select,
    Br(u32),
    BrIf(u32),
    BrTable(Vec<u32>, u32),
    LocalGet(u32),
    LocalSet(u32),
    LocalTee(u32),
    MemoryInstrWithMemarg(Opcode, Memarg),
    MemoryGrow,
    I32Const(i32),
    I64Const(i64),
    F32Const(f32),
    F64Const(f64),
    Iunop(Opcode),
    Funop(Opcode),
    Ibinop(Opcode),
    Fbinop(Opcode),
    Itestop(Opcode),
    Irelop(Opcode),
    Cutop(Opcode),
    TruncSat(TruncSatOp),
}

#[derive(Debug, Clone)]
pub struct Memarg {
    pub align: u32,
    pub offset: u32,
}

#[derive(Debug, Clone)]
pub struct TableType {
    pub elem_type: RefType,
    pub limits: Limits,
}

enum_try_from_int! {
    #[repr(u8)]
    #[derive(Debug, Clone, Copy)]
    pub enum TruncSatOp {
        I32TruncSatF32S = 0x00,
        I32TruncSatF32U = 0x01,
        I32TruncSatF64S = 0x02,
        I32TruncSatF64U = 0x03,
        I64TruncSatF32S = 0x04,
        I64TruncSatF32U = 0x05,
        I64TruncSatF64S = 0x06,
        I64TruncSatF64U = 0x07,
    }
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
        Nop = 0x01,
        Block = 0x02,
        Loop = 0x03,
        If = 0x04,
        Else = 0x05,
        End = 0x0b,
        Br = 0x0c,
        BrIf = 0x0d,
        BrTable = 0x0e,
        Return = 0x0f,
        Call = 0x10,
        CallIndirect = 0x11,
        Drop = 0x1a,
        Select = 0x1b,
        LocalGet = 0x20,
        LocalSet = 0x21,
        LocalTee = 0x22,
        I32Store = 0x36,
        I64Store = 0x37,
        MemoryGrow = 0x40,
        I32Const = 0x41,
        I64Const = 0x42,
        F32Const = 0x43,
        F64Const = 0x44,
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
        F32Abs = 0x8b,
        F32Neg = 0x8c,
        F32Ceil = 0x8d,
        F32Floor = 0x8e,
        F32Trunc = 0x8f,
        F32Nearest = 0x90,
        F32Sqrt = 0x91,
        F32Add = 0x92,
        F32Sub = 0x93,
        F32Mul = 0x94,
        F32Div = 0x95,
        F32Min = 0x96,
        F32Max = 0x97,
        F32Copysign = 0x98,
        F64Abs = 0x99,
        F64Neg = 0x9a,
        F64Ceil = 0x9b,
        F64Floor = 0x9c,
        F64Trunc = 0x9d,
        F64Nearest = 0x9e,
        F64Sqrt = 0x9f,
        F64Add = 0xa0,
        F64Sub = 0xa1,
        F64Mul = 0xa2,
        F64Div = 0xa3,
        F64Min = 0xa4,
        F64Max = 0xa5,
        F64Copysign = 0xa6,
        I32WrapI64 = 0xa7,
        I32TruncF32S = 0xa8,
        I32TruncF32U = 0xa9,
        I32TruncF64S = 0xaa,
        I32TruncF64U = 0xab,
        I64ExtendI32S = 0xac,
        I64ExtendI32U = 0xad,
        I64TruncF32S = 0xae,
        I64TruncF32U = 0xaf,
        I64TruncF64S = 0xb0,
        I64TruncF64U = 0xb1,
        F32ConvertI32S = 0xb2,
        F32ConvertI32U = 0xb3,
        F32ConvertI64S = 0xb4,
        F32ConvertI64U = 0xb5,
        F32DemoteF64 = 0xb6,
        F64ConvertI32S = 0xb7,
        F64ConvertI32U = 0xb8,
        F64ConvertI64S = 0xb9,
        F64ConvertI64U = 0xba,
        F64PromoteF32 = 0xbb,
        I32ReinterpretF32 = 0xbc,
        I64ReinterpretF64 = 0xbd,
        F32ReinterpretI32 = 0xbe,
        F64ReinterpretI64 = 0xbf,
        I32Extend8S = 0xc0,
        I32Extend16S = 0xc1,
        I64Extend8S = 0xc2,
        I64Extend16S = 0xc3,
        I64Extend32S = 0xc4,
        TruncSat = 0xfc,
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
    pub fn is_funop(&self) -> bool {
        use Opcode::*;
        match self {
            F32Abs | F32Neg | F32Ceil | F32Floor | F32Trunc | F32Nearest | F32Sqrt | F64Abs
            | F64Neg | F64Ceil | F64Floor | F64Trunc | F64Nearest | F64Sqrt => true,
            _ => false,
        }
    }
    pub fn is_fbinop(&self) -> bool {
        use Opcode::*;
        match self {
            F32Add | F32Sub | F32Mul | F32Div | F32Min | F32Max | F32Copysign | F64Add | F64Sub
            | F64Mul | F64Div | F64Min | F64Max | F64Copysign => true,
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
    pub fn is_cutop(&self) -> bool {
        use Opcode::*;
        match self {
            I32WrapI64 | I32TruncF32S | I32TruncF32U | I32TruncF64S | I32TruncF64U
            | I64ExtendI32S | I64ExtendI32U | I64TruncF32S | I64TruncF32U | I64TruncF64S
            | I64TruncF64U | F32ConvertI32S | F32ConvertI32U | F32ConvertI64S | F32ConvertI64U
            | F32DemoteF64 | F64ConvertI32S | F64ConvertI32U | F64ConvertI64S | F64ConvertI64U
            | F64PromoteF32 | I32ReinterpretF32 | I64ReinterpretF64 | F32ReinterpretI32
            | F64ReinterpretI64 | I32Extend8S | I32Extend16S | I64Extend8S | I64Extend16S
            | I64Extend32S => true,
            _ => false,
        }
    }
    pub fn is_memory_instr_with_memarg(&self) -> bool {
        use Opcode::*;
        match self {
            I32Store | I64Store => true,
            _ => false,
        }
    }
}
