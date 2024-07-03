use crate::wasm::{BlockType, Func, Instr, Locals, Opcode, Section, SectionContent, ValType};

#[derive(Debug)]
pub struct Parser<'a> {
    bytes: &'a [u8],
    pos: usize,
    // label_stack has control instruction's pc
    label_stack: Vec<Option<usize>>,
}

type Result<T> = std::result::Result<T, String>;

impl<'a> Parser<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        Parser {
            bytes,
            pos: 0,
            label_stack: Vec::new(),
        }
    }
    pub fn parse(&mut self) -> Result<Vec<Section>> {
        let magic = self.bytes.get(self.pos..self.pos + 4);
        self.pos += 4;
        if magic != Some(&[0x00, 0x61, 0x73, 0x6d]) {
            return Err("Invalid magic number".to_string());
        }
        let version = self.bytes.get(self.pos..self.pos + 4);
        self.pos += 4;
        if version != Some(&[0x01, 0x00, 0x00, 0x00]) {
            return Err("Invalid version".to_string());
        }
        self.parse_section()
    }
    fn next_byte(&mut self) -> Result<u8> {
        let Some(byte) = self.bytes.get(self.pos) else {
            return Err("Unexpected EOF".to_string());
        };
        self.pos += 1;
        Ok(*byte)
    }
    fn parse_section(&mut self) -> Result<Vec<Section>> {
        let mut sections = Vec::new();
        loop {
            if self.is_eof() {
                break;
            }
            let id = self
                .next_byte()
                .map_err(|err| format!("{err}: expected section id"))?;
            let size = self.parse_leb128_u32()?;
            sections.push(Section {
                content: match id {
                    0 => self.parse_custom(size)?,
                    1 => self.parse_type(size)?,
                    2 => self.parse_import(size)?,
                    3 => self.parse_function(size)?,
                    4 => self.parse_table(size)?,
                    5 => self.parse_memory(size)?,
                    6 => self.parse_global(size)?,
                    7 => self.parse_export(size)?,
                    8 => self.parse_start(size)?,
                    9 => self.parse_element(size)?,
                    10 => self.parse_code(size)?,
                    11 => self.parse_data(size)?,
                    12 => self.parse_data_count(size)?,
                    _ => {
                        return Err(format!("Unknown section id: {}", id));
                    }
                },
                size,
            });
        }
        Ok(sections)
    }
    fn parse_custom(&mut self, size: u32) -> Result<SectionContent> {
        let skip_pos = self.pos + size as usize;
        let name = self.parse_name()?;
        self.pos = skip_pos;
        Ok(SectionContent::Custom { name })
    }
    fn parse_type(&mut self, size: u32) -> Result<SectionContent> {
        let skip_pos = self.pos + size as usize;
        self.pos = skip_pos;
        Ok(SectionContent::Type)
    }
    fn parse_import(&mut self, size: u32) -> Result<SectionContent> {
        let skip_pos = self.pos + size as usize;
        self.pos = skip_pos;
        Ok(SectionContent::Import)
    }
    fn parse_function(&mut self, size: u32) -> Result<SectionContent> {
        let skip_pos = self.pos + size as usize;
        self.pos = skip_pos;
        Ok(SectionContent::Function)
    }
    fn parse_table(&mut self, size: u32) -> Result<SectionContent> {
        let skip_pos = self.pos + size as usize;
        self.pos = skip_pos;
        Ok(SectionContent::Table)
    }
    fn parse_memory(&mut self, size: u32) -> Result<SectionContent> {
        let skip_pos = self.pos + size as usize;
        self.pos = skip_pos;
        Ok(SectionContent::Memory)
    }
    fn parse_global(&mut self, size: u32) -> Result<SectionContent> {
        let skip_pos = self.pos + size as usize;
        self.pos = skip_pos;
        Ok(SectionContent::Global)
    }
    fn parse_export(&mut self, size: u32) -> Result<SectionContent> {
        let skip_pos = self.pos + size as usize;
        self.pos = skip_pos;
        Ok(SectionContent::Export)
    }
    fn parse_start(&mut self, size: u32) -> Result<SectionContent> {
        let skip_pos = self.pos + size as usize;
        self.pos = skip_pos;
        Ok(SectionContent::Start)
    }
    fn parse_element(&mut self, size: u32) -> Result<SectionContent> {
        let skip_pos = self.pos + size as usize;
        self.pos = skip_pos;
        Ok(SectionContent::Element)
    }
    fn parse_code(&mut self, size: u32) -> Result<SectionContent> {
        let skip_pos = self.pos + size as usize;
        let code_count = self.parse_leb128_u32()?;
        let mut funcs = Vec::new();
        while self.pos < skip_pos {
            let func_size = self.parse_leb128_u32()?;
            let current_pos = self.pos;
            let locals_count = self.parse_leb128_u32()?;
            let mut locals = Vec::new();
            for _ in 0..locals_count {
                let n = self.parse_leb128_u32()?;
                let ty = self.parse_valtype()?;
                locals.push(Locals { count: n, ty });
            }
            let mut instrs = Vec::new();
            // label for function
            self.label_stack.push(None);
            while self.pos < current_pos + func_size as usize {
                if let Ok(instr) = self.parse_instr() {
                    let instr = match instr {
                        Instr::Loop {
                            block_type,
                            jump_pc: _,
                        } => {
                            self.label_stack.push(None);
                            Instr::Loop {
                                block_type,
                                // pc of loop instruction
                                jump_pc: instrs.len(),
                            }
                        }
                        Instr::End => {
                            match self.label_stack.pop() {
                                Some(Some(_)) => unimplemented!(),
                                Some(None) => {}
                                None => {
                                    return Err("unexpected end".to_string());
                                }
                            };
                            Instr::End
                        }
                        _ => instr,
                    };
                    instrs.push(instr);
                }
            }
            if self.pos != current_pos + func_size as usize {
                return Err("Invalid function size".to_string());
            }
            funcs.push(Func {
                size: func_size,
                locals,
                instrs,
            });
        }
        if code_count as usize != funcs.len() {
            return Err("Invalid code count".to_string());
        }
        self.ensure_section_end(skip_pos)?;
        Ok(SectionContent::Code(funcs))
    }
    fn parse_instr(&mut self) -> Result<Instr> {
        let opcode = self
            .next_byte()
            .map_err(|err| format!("{err}: expected opcode"))?
            .try_into()
            .map_err(|_| format!("invalid opcode"))?;
        match opcode {
            Opcode::I64Const => {
                let value = self.parse_leb128_i64()?;
                Ok(Instr::I64Const(value))
            }
            Opcode::I32Const => {
                let value = self.parse_leb128_i32()?;
                Ok(Instr::I32Const(value as i32))
            }
            Opcode::LocalGet => {
                let localidx = self.parse_leb128_u32()?;
                Ok(Instr::LocalGet(localidx))
            }
            Opcode::LocalSet => {
                let localidx = self.parse_leb128_u32()?;
                Ok(Instr::LocalSet(localidx))
            }
            Opcode::BrIf => {
                let labelidx = self.parse_leb128_u32()?;
                Ok(Instr::BrIf(labelidx))
            }
            Opcode::Loop => {
                let block_type = self.parse_blocktype()?;
                Ok(Instr::Loop {
                    block_type,
                    // dummy value
                    jump_pc: 0,
                })
            }
            Opcode::End => Ok(Instr::End),
            op => {
                if op.is_iunop() {
                    Ok(Instr::Iunop(op))
                } else if op.is_ibinop() {
                    Ok(Instr::Ibinop(op))
                } else if op.is_itestop() {
                    Ok(Instr::Itestop(op))
                } else if op.is_irelop() {
                    Ok(Instr::Irelop(op))
                } else {
                    unreachable!("uncovered opcode: {:?}", op)
                }
            }
        }
    }
    fn parse_data(&mut self, size: u32) -> Result<SectionContent> {
        let skip_pos = self.pos + size as usize;
        self.pos = skip_pos;
        Ok(SectionContent::Data)
    }
    fn parse_data_count(&mut self, size: u32) -> Result<SectionContent> {
        let skip_pos = self.pos + size as usize;
        self.pos = skip_pos;
        Ok(SectionContent::DataCount)
    }
    fn ensure_section_end(&self, end_pos: usize) -> Result<()> {
        if self.pos != end_pos {
            return Err("Unexpected Section End".to_string());
        }
        Ok(())
    }
    fn is_eof(&self) -> bool {
        self.pos >= self.bytes.len()
    }
    fn parse_name(&mut self) -> Result<String> {
        let size = self.parse_leb128_u32()?;
        let name = self.bytes.get(self.pos..self.pos + size as usize);
        self.pos += size as usize;
        name.map(|name| String::from_utf8_lossy(name).to_string())
            .ok_or("Unexpected EOF".to_string())
    }
    fn parse_valtype(&mut self) -> Result<ValType> {
        self.next_byte()
            .map_err(|err| format!("{err}: expected valtype"))?
            .try_into()
            .map_err(|_| format!("invalid valtype"))
    }
    fn parse_blocktype(&mut self) -> Result<BlockType> {
        let byte = self
            .next_byte()
            .map_err(|err| format!("{err}: expected blocktype"))?;
        Ok(match byte {
            0x40 => BlockType::Empty,
            _ => BlockType::ValType(byte.try_into().map_err(|_| "invalid blocktype")?),
        })
    }
    fn parse_leb128_u32(&mut self) -> Result<u32> {
        let mut result = 0;
        let mut shift = 0;
        loop {
            let byte = self
                .next_byte()
                .map_err(|err| format!("{err}: while parsing u32"))?;
            if shift >= 39 {
                return Err("Invalid LEB128 encoding".to_string());
            }
            result |= ((byte & 0x7f) as u32).wrapping_shl(shift);
            shift += 7;
            if byte & 0x80 == 0 {
                break Ok(result);
            }
        }
    }
    fn parse_leb128_i64(&mut self) -> Result<i64> {
        let mut result = 0;
        let mut shift = 0;
        loop {
            let byte = self
                .next_byte()
                .map_err(|err| format!("{err}: while parsing u32"))?;
            if shift >= 71 {
                return Err("Invalid LEB128 encoding".to_string());
            }
            result |= ((byte & 0x7f) as u64).wrapping_shl(shift) as u64;
            shift += 7;
            if byte & 0x80 == 0 {
                // if the sign bit is set, sign extend
                if (shift < 64) && (byte & 0x40 != 0) {
                    result |= !0 << shift;
                }
                break Ok(result as i64);
            }
        }
    }
    fn parse_leb128_i32(&mut self) -> Result<i64> {
        let mut result = 0;
        let mut shift = 0;
        loop {
            let byte = self
                .next_byte()
                .map_err(|err| format!("{err}: while parsing u32"))?;
            if shift >= 39 {
                return Err("Invalid LEB128 encoding".to_string());
            }
            result |= ((byte & 0x7f) as u64).wrapping_shl(shift) as u64;
            shift += 7;
            if byte & 0x80 == 0 {
                // if the sign bit is set, sign extend
                if (shift < 64) && (byte & 0x40 != 0) {
                    result |= !0 << shift;
                }
                break Ok(result as i64);
            }
        }
    }
}
