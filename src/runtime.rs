use crate::parser::{Instr, Section, SectionContent};

#[derive(Debug)]
pub struct Runtime {
    pub instrs: Vec<Instr>,
    pub pc: Vec<usize>,
    pub stack: Vec<i64>,
    pub jump_table: Vec<usize>,
    pub locals: Vec<i64>,
}

impl Runtime {
    pub fn new(sections: Vec<Section>) -> Runtime {
        for section in sections {
            if let SectionContent::Code(funcs) = section.content {
                let locals_size = funcs[0].locals.iter().map(|x| x.count).sum::<u32>();
                return Runtime {
                    instrs: funcs[0].instrs.clone(),
                    pc: vec![0],
                    stack: vec![],
                    jump_table: vec![],
                    locals: vec![0; locals_size as usize],
                };
            }
        }
        panic!("No code section found");
    }
    fn get_instr(&self) -> &Instr {
        let mut instr = &self.instrs[self.pc[0]];
        for pc in &self.pc[1..] {
            if let Instr::Loop(instrs) = instr {
                instr = &instrs[*pc];
            } else {
                panic!("Invalid pc");
            }
        }
        instr
    }
    fn get_current_pc(&self) -> usize {
        self.pc[self.pc.len() - 1]
    }
    fn get_outer_instr(&self) -> &Vec<Instr> {
        let mut instr = &self.instrs;
        for pc in &self.pc[..self.pc.len() - 1] {
            if let Instr::Loop(instrs) = &instr[*pc] {
                instr = instrs;
            } else {
                panic!("Invalid pc");
            }
        }
        instr
    }
    fn jump(&mut self, pc: usize) {
        let len = self.pc.len();
        self.pc[len - 1] = pc;
    }
    fn inc_pc(&mut self) {
        let len = self.pc.len();
        self.pc[len - 1] += 1;
    }
    pub fn run(&mut self) {
        loop {
            let current_pc = self.get_current_pc();
            if self.pc.len() == 1 && current_pc >= self.instrs.len() {
                break;
            }
            let outer_instrs = self.get_outer_instr();
            if current_pc >= outer_instrs.len() {
                self.pc.pop();
                self.jump_table.pop();
                self.inc_pc();
                continue;
            }
            let instr = self.get_instr();
            match instr {
                Instr::I64Const(n) => {
                    self.stack.push(*n);
                }
                Instr::I64Add => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(a + b);
                }
                Instr::I64LtU => {
                    let b = self.stack.pop().unwrap();
                    let a = self.stack.pop().unwrap();
                    self.stack.push(if a < b { 1 } else { 0 });
                }
                Instr::LocalSet(n) => {
                    let n = *n;
                    let value = self.stack.pop().unwrap();
                    self.locals[n as usize] = value;
                }
                Instr::LocalGet(n) => {
                    let value = self.locals[*n as usize];
                    self.stack.push(value);
                }
                Instr::Loop(_) => {
                    self.pc.push(0);
                    self.jump_table.push(0);
                    continue;
                }
                Instr::BrIf(n) => {
                    let n = *n;
                    let value = self.stack.pop().unwrap();
                    if value != 0 {
                        for _ in 0..n {
                            self.pc.pop();
                            self.jump_table.pop();
                        }
                        self.jump(*self.jump_table.last().unwrap());
                        continue;
                    }
                }
            }
            self.inc_pc();
        }
    }
}
