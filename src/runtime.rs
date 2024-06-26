use crate::wasm::{Instr, Section, SectionContent};

#[derive(Debug)]
pub struct Runtime {
    pub instrs: Vec<Instr>,
    pub pc: usize,
    pub stack: Vec<i64>,
    pub locals: Vec<i64>,
    pub labels: Vec<Label>,
}

#[derive(Debug, Clone)]
pub struct Label {
    sp: usize,
    is_loop: bool,
    jump_pc: usize,
}

impl Runtime {
    pub fn new(sections: Vec<Section>) -> Runtime {
        for section in sections {
            if let SectionContent::Code(funcs) = section.content {
                let locals_size = funcs[0].locals.iter().map(|x| x.count).sum::<u32>();
                return Runtime {
                    instrs: funcs[0].instrs.clone(),
                    pc: 0,
                    stack: vec![],
                    locals: vec![0; locals_size as usize],
                    labels: vec![],
                };
            }
        }
        panic!("No code section found");
    }
    // fn get_instr(&self) -> &Instr {
    //     let mut instr = &self.instrs[self.pc[0]];
    //     for pc in &self.pc[1..] {
    //         if let Instr::Loop(instrs) = instr {
    //             instr = &instrs[*pc];
    //         } else {
    //             panic!("Invalid pc");
    //         }
    //     }
    //     instr
    // }
    // fn get_current_pc(&self) -> usize {
    //     self.pc[self.pc.len() - 1]
    // }
    // fn get_outer_instr(&self) -> &Vec<Instr> {
    //     let mut instr = &self.instrs;
    //     for pc in &self.pc[..self.pc.len() - 1] {
    //         if let Instr::Loop(instrs) = &instr[*pc] {
    //             instr = instrs;
    //         } else {
    //             panic!("Invalid pc");
    //         }
    //     }
    //     instr
    // }
    pub fn run(&mut self) {
        loop {
            let pc = self.pc;
            match &self.instrs[pc] {
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
                Instr::Loop {
                    block_type: _,
                    jump_pc,
                } => {
                    self.labels.push(Label {
                        sp: self.stack.len(),
                        is_loop: true,
                        jump_pc: *jump_pc,
                    });
                }
                Instr::BrIf(n) => {
                    let n = *n;
                    let value = self.stack.pop().unwrap();
                    if value != 0 {
                        for i in (0..=n).rev() {
                            match self.labels.last() {
                                Some(Label {
                                    sp,
                                    jump_pc,
                                    is_loop,
                                }) => {
                                    self.stack.truncate(*sp);
                                    self.pc = *jump_pc;
                                    if !(*is_loop && i == 0) {
                                        self.labels.pop();
                                    }
                                }
                                None => break,
                            }
                        }
                    }
                }
                Instr::End => match self.labels.pop() {
                    Some(Label { sp, .. }) => {
                        self.stack.truncate(sp);
                    }
                    None => break,
                },
            }
            self.pc += 1;
        }
    }
}
