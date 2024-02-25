use core::slice;
use std::mem;

use codemap::Pos;
use memmap2::{Mmap, MmapOptions};

#[derive(Debug, Copy, Clone)]
pub struct Reg {
    pub r: usize,
}

#[derive(Debug, Copy, Clone)]
pub struct Mem {
    pub addr: Reg,
    pub offset: u64,
}

#[derive(Debug, Copy, Clone)]
pub struct Label(pub usize);

/// https://developer.arm.com/documentation/100076/0100/A32-T32-Instruction-Set-Reference/Condition-Codes/Condition-code-suffixes-and-related-flags?lang=en
#[derive(Debug, Copy, Clone)]
pub enum CmpFlags {
    GT, // signed greater than
    LS, // unsigned less than or equal
    HS, // unsigned greater than or equal
    LO, // unsigned less than
    HI, // unsigned greater than
    EQ, // equal
    NE, // not equal
}

#[derive(Debug, Copy, Clone)]
pub enum Inst {
    BranchIf(CmpFlags, Label),
    Three {
        op: Three,
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    Two {
        op: Two,
        lhs: Reg,
        rhs: Reg,
    },
    Ret,
    Ldr {
        src: Mem,
        dest: Reg,
    },
    Str {
        src: Reg,
        dest: Mem,
    },
    /// Load from sequential slots to a pair of registers
    Ldp {
        src: Mem,
        dest_a: Reg,
        dest_b: Reg,
    },
    /// Store a pair of registers to sequential slots.
    Stp {
        dest: Mem,
        src_a: Reg,
        src_b: Reg,
    },
    /// Branch if zero (without setting flags)
    CBZ(Reg, Label),

    /// Load an immediate and zero the extra bits of the register.
    MOVZ {
        dest: Reg,
        imm: u16,
    },
    /// Load a shifted immediate without changing other bits of the register.
    MOVK {
        dest: Reg,
        imm: u16,
        shift: u8,
    },
    Bl(Label),
    TempFakeEnd,
}

#[derive(Debug, Copy, Clone)]
pub enum Three {
    ADD,
    SUB,
    MUL,
    UDIV,

    FADD,
    FSUB,
    FMUL,
    FDIV,

    AND,
}

#[derive(Debug, Copy, Clone)]
pub enum Two {
    /// Compare two int registers and set the magic flags.
    CMP,
    /// Compare two float registers and set the magic flags.
    FCMP,

    /// Move a value from one int register into another.
    MOV,
    /// Move between float registers OR move raw bits between int<->float without value cast.
    FMOV,

    /// Unsigned int to float
    UCVTF,
    /// Float to unsigned int. TODO: which rounding mode?
    FCVTAU,
    /// Convert between sizes of float
    FCVT,
}

#[derive(Default)]
struct Assembler {
    labels: Vec<usize>,
    insts: Vec<Inst>,
    out: Vec<u32>,
}

impl Assembler {
    fn encode_all(&mut self) {
        for i in 0..self.insts.len() {
            self.encode(i);
        }
    }

    fn encode(&mut self, i: usize) {
        let inst = match self.insts[i] {
            Inst::BranchIf(_, _) => todo!(),
            Inst::Three { op, dest, lhs, rhs } => todo!(),
            Inst::Two { op, lhs, rhs } => todo!(),
            Inst::Ret => 0xD65F03C0,
            Inst::Ldr { src, dest } => todo!(),
            Inst::Str { src, dest } => todo!(),
            Inst::Ldp {
                src,
                dest_a,
                dest_b,
            } => todo!(),
            Inst::Stp { dest, src_a, src_b } => todo!(),
            Inst::CBZ(_, _) => todo!(),
            Inst::MOVZ { dest, imm } => {
                let op: u32 = 0b110100101 << (32 - 9);
                let shift = 0;
                let hw = (shift / 16) << (32 - 9 - 2);
                let imm16 = (imm << 5) as u32;
                let reg = self.r(dest);
                op | hw | imm16 | reg
            }
            Inst::MOVK { dest, imm, shift } => todo!(),
            Inst::Bl(_) => todo!(),
            Inst::TempFakeEnd => todo!(),
        };
        debug_assert_eq!(self.out.len(), i);
        self.out.push(inst);
    }

    fn r(&self, dest: Reg) -> u32 {
        dest.r as u32
    }

    fn log(&self) -> String {
        self.out
            .iter()
            .zip(self.insts.iter())
            .map(|(b, i)| format!("{:#08x}    {:?}\n", b, i))
            .collect()
    }

    fn map_exec(&self) -> Mmap {
        let mut map = MmapOptions::new()
            .len(self.out.len() * 4)
            .map_anon()
            .unwrap();
        let bytes = self.out.as_ptr() as *const u8;
        let bytes = unsafe { slice::from_raw_parts(bytes, self.out.len() * 4) };
        map.copy_from_slice(bytes);
        map.make_exec().unwrap()
    }
}

fn call_jit<A, R>(arg: A, insts: &[Inst]) -> R {
    let mut asm = Assembler::default();
    asm.insts.extend(insts);
    asm.encode_all();
    let map = asm.map_exec();
    let ptr: extern "C" fn(A) -> R = unsafe { mem::transmute(map.as_ptr()) };
    ptr(arg)
}

#[test]
fn call42() {
    let result: u64 = call_jit(
        (),
        &[
            Inst::MOVZ {
                dest: Reg { r: 0 },
                imm: 42,
            },
            Inst::Ret,
        ],
    );
    assert_eq!(result, 42);
}
