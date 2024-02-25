#![allow(unused)]

use crate::{
    arm64::{Inst, Label, Mem, Three, Two},
    compiler::Res,
};

pub struct InterpArm {
    registers: [u64; 16],
    code: Vec<Inst>,
    flags: u64,
    stack: Vec<u8>,
}

impl InterpArm {
    pub fn new() -> Self {
        Self {
            registers: Default::default(),
            code: vec![],
            flags: 0,
            stack: vec![0; 1024 * 1024 * 8],
        }
    }

    /// # Safety
    /// Simply don't make a mistake and it will be fine.
    pub unsafe fn run(&mut self) -> Res<'_, ()> {
        loop {
            match self.code[self.registers[15] as usize] {
                Inst::TempFakeEnd => break,
                Inst::BranchIf(_, _) => todo!(),
                Inst::Three { op, dest, lhs, rhs } => {
                    let a = self.registers[lhs.r];
                    let b = self.registers[rhs.r];
                    let c = match op {
                        Three::ADD => a.wrapping_add(b),
                        Three::SUB => a.wrapping_sub(b),
                        Three::MUL => a.wrapping_mul(b),
                        Three::UDIV => a / b,
                        Three::AND => a & b,
                        Three::FADD => todo!(),
                        Three::FSUB => todo!(),
                        Three::FMUL => todo!(),
                        Three::FDIV => todo!(),
                    };
                    self.registers[dest.r] = c;
                }
                Inst::Two { op, lhs, rhs } => {
                    let a = self.registers[lhs.r];
                    let b = self.registers[rhs.r];
                    match op {
                        // TODO: idk if this is right
                        Two::CMP => {
                            if a == b {
                                self.flags &= 1 << Z_SHIFT;
                            } else {
                                self.flags &= !(1 << Z_SHIFT);
                            }
                            if b > a {
                                self.flags &= 1 << N_SHIFT;
                            } else {
                                self.flags &= !(1 << N_SHIFT);
                            }
                            if a.overflowing_add(b).1 {
                                self.flags &= 1 << C_SHIFT;
                            } else {
                                self.flags &= !(1 << C_SHIFT);
                            }
                            if a.overflowing_sub(b).1 {
                                self.flags &= 1 << V_SHIFT;
                            } else {
                                self.flags &= !(1 << V_SHIFT);
                            }
                        }
                        Two::FCMP => todo!(),
                        Two::MOV => self.registers[lhs.r] = b,
                        Two::FMOV => todo!(),
                        Two::UCVTF => todo!(),
                        Two::FCVTAU => todo!(),
                        Two::FCVT => todo!(),
                    }
                }
                Inst::Ret => todo!(),
                Inst::Ldr { src, dest } => {
                    self.registers[dest.r] = *self.addr(src);
                }
                Inst::Str { src, dest } => *self.addr(dest) = self.registers[src.r],
                Inst::Ldp {
                    src,
                    dest_a,
                    dest_b,
                } => todo!(),
                Inst::Stp { dest, src_a, src_b } => todo!(),
                Inst::CBZ(r, target) => {
                    if self.registers[r.r] == 0 {
                        self.registers[15] = self.label(target);
                    }
                }
                Inst::MOVZ { dest, imm } => todo!(),
                Inst::MOVK { dest, imm, shift } => todo!(),
                Inst::Bl(_) => todo!(),
            }
            self.registers[15] += 1;
        }
        Ok(())
    }

    fn label(&self, l: Label) -> u64 {
        l.0 as u64
    }

    fn addr(&self, m: Mem) -> *mut u64 {
        let addr = self.registers[m.addr.r] + m.offset;
        addr as usize as *mut u64
    }
}

const N_SHIFT: u64 = 31;
const Z_SHIFT: u64 = 31;
const C_SHIFT: u64 = 31;
const V_SHIFT: u64 = 31;

impl Default for InterpArm {
    fn default() -> Self {
        Self::new()
    }
}
