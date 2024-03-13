//! https://developer.arm.com/documentation/ddi0487/latest/ (section 6.2 is the good bit)
//! https://armconverter.com/

use core::slice;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Reg {
    pub r: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Mem {
    pub addr: Reg,
    pub offset: u64,
}

#[derive(Debug, Copy, Clone)]
pub struct Label(pub usize);

/// https://developer.arm.com/documentation/100076/0100/A32-T32-Instruction-Set-Reference/Condition-Codes/Condition-code-suffixes-and-related-flags?lang=en
#[derive(Debug, Copy, Clone)]
pub enum CmpFlags {
    EQ = 0b0000, // equal
    NE = 0b0001, // not equal
    HS = 0b0010, // unsigned greater than or equal
    LO = 0b0011, // unsigned less than
    HI = 0b1000, // unsigned greater than
    LS = 0b1001, // unsigned less than or equal
    LT = 0b1011, // signed less than
    GT = 0b1100, // signed greater than
    AL = 0b1111, // Always
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
    WithImm {
        op: Three,
        dest: Reg,
        src: Reg,
        imm: u64,
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
    /// There are restrictions on the range of values that can be encoded.
    MOVZ {
        dest: Reg,
        imm: u64,
    },
    /// Load an immediate without changing other bits of the register.
    /// There are restrictions on the range of values that can be encoded.
    MOVK {
        dest: Reg,
        imm: u64,
    },
    /// Branch to offset and DONT set link
    B(Label),
    /// Branch to offset and SET link
    Bl(Label),
    // Branch to register and DONT set link
    Br(Reg),
    /// Branch to register and SET link
    Blr(Reg),
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
pub struct Assembler {
    labels: Vec<usize>,
    insts: Vec<Inst>,
    out: Vec<u32>,
    #[cfg(target_arch = "aarch64")]
    done: Option<memmap2::Mmap>,
    #[cfg(not(target_arch = "aarch64"))]
    done: Option<()>,
}

// TODO: put .to_le() in the right place? but i dont really care cause its non-trivial to find something to test it on.
macro_rules! build {
    () => { 0 };
    ($val:expr; $shift:literal, $($arg:tt)*) => {
        (($val as u32) << $shift) | build!($($arg)*)
    };
}

impl Assembler {
    pub fn push(&mut self, i: Inst) {
        assert!(self.done.is_none());
        self.insts.push(i)
    }

    pub fn label_next(&mut self) -> Label {
        self.labels.push(self.insts.len());
        Label(self.labels.len() - 1)
    }

    pub fn encode_all(&mut self) {
        debug_assert!(self.out.is_empty());
        for i in 0..self.insts.len() {
            let inst = self.encode(self.insts[i], i);
            self.out.push(inst);
        }
    }

    fn encode(&mut self, inst: Inst, index: usize) -> u32 {
        match inst {
            Inst::BranchIf(_, _) => todo!(),
            Inst::Three { op, dest, lhs, rhs } => match op {
                Three::ADD => {
                    let shift_ty = 0;
                    let shift = 0;
                    build!(0b10001011;24, shift_ty;22, shift;10, self.r(lhs);16, self.r(rhs);5, self.r(dest);0, )
                }
                Three::SUB => todo!(),
                Three::MUL => todo!(),
                Three::UDIV => todo!(),
                Three::FADD => todo!(),
                Three::FSUB => todo!(),
                Three::FMUL => todo!(),
                Three::FDIV => todo!(),
                Three::AND => todo!(),
            },
            Inst::WithImm { .. } => todo!(),
            Inst::Two { op, lhs, rhs } => match op {
                Two::CMP => todo!(),
                Two::FCMP => todo!(),
                Two::MOV => {
                    build!(0b10101010000;21, self.r(rhs);16, 0b00000011111;5, self.r(lhs);0, )
                }
                Two::FMOV => todo!(),
                Two::UCVTF => todo!(),
                Two::FCVTAU => todo!(),
                Two::FCVT => todo!(),
            },
            Inst::Ret => 0xD65F03C0,
            Inst::Ldr { .. } => todo!(),
            Inst::Str { .. } => todo!(),
            Inst::Ldp { .. } => todo!(),
            Inst::Stp { .. } => todo!(),
            Inst::CBZ(reg, label) => {
                let offset = self.labels[label.0] as i64 - index as i64; // measured in instructions, not bytes
                let offset = signed_truncate(offset, 19);
                build!(0b10110100;24, offset;5, self.r(reg);0, )
            }
            Inst::MOVZ { dest, imm } => {
                debug_assert!(imm < u16::MAX as u64, "TODO: shifting");
                let shift = 0;
                let hw = shift / 16;
                build!(0b110100101;23, hw;21, imm;5, self.r(dest);0, )
            }
            Inst::MOVK { .. } => todo!(),
            Inst::Bl(_) => todo!(),
            Inst::TempFakeEnd => todo!(),
            Inst::Br(reg) => {
                build!(0b1101011000011111;16, self.r(reg);5,)
            }
            Inst::Blr(_) => todo!(),
            Inst::B(label) => {
                let offset = self.labels[label.0] as i64 - index as i64; // measured in instructions, not bytes
                let offset = signed_truncate(offset, 26);
                build!(0b000101;26, offset;0,)
            }
        }
    }

    fn r(&self, dest: Reg) -> u32 {
        dest.r as u32
    }

    pub fn log(&self) -> String {
        self.out
            .iter()
            .zip(self.insts.iter())
            .map(|(b, i)| format!("{b:#08x} | {b:#032b}| {i:?}\n"))
            .collect()
    }

    /// The assembler works on other targets, but there's no reason to produce the executable memory since you couldn't run it anyway.
    #[cfg(target_arch = "aarch64")]
    pub fn map_exec(&mut self) -> *const u8 {
        debug_assert!(!self.out.is_empty());
        match &self.done {
            Some(done) => done.as_ptr(),
            None => {
                // TODO: emit into this thing so don't have to copy.
                let mut map = memmap2::MmapOptions::new()
                    .len(self.out.len() * 4)
                    .map_anon()
                    .unwrap();
                let bytes = self.out.as_ptr() as *const u8;
                let bytes = unsafe { slice::from_raw_parts(bytes, self.out.len() * 4) };
                map.copy_from_slice(bytes);
                self.done = Some(map.make_exec().unwrap());
                self.map_exec()
            }
        }
    }
}

// There must be a not insane way to do this but i gave up and read the two's complement wikipedia page.
/// Convert an i64 to an i<bit_count> with the (64-<bit_count>) leading bits 0.
pub fn signed_truncate(mut x: i64, bit_count: i64) -> i64 {
    debug_assert!(x > -(1 << (bit_count - 1)) && (x < (1 << (bit_count - 1))));
    let mask = (1 << bit_count) - 1;
    if x < 0 {
        x *= -1;
        x = !x;
        x += 1;
        x &= mask;
    }
    x
}

impl Reg {
    const fn u64(r: usize) -> Self {
        Self { r }
    }
}

pub const X0: Reg = Reg::u64(0);
pub const SP: Reg = Reg::u64(14);

#[allow(unused)]
#[cfg(target_arch = "aarch64")]
mod encoding_tests {
    use super::{Inst, Reg};
    use crate::experiments::aarch64::{Assembler, Label, Three, Two, X0};
    use std::mem;
    const X1: Reg = Reg::u64(1);
    const X2: Reg = Reg::u64(2);
    const X3: Reg = Reg::u64(3);

    fn call_jit<A, R>(arg: A, insts: &[Inst], labels: &[usize]) -> R {
        let mut asm = Assembler::default();
        asm.insts.extend(insts);
        asm.labels.extend(labels);
        asm.encode_all();
        println!("{}", asm.log());
        let code = asm.map_exec();
        let ptr: extern "C" fn(A) -> R = unsafe { mem::transmute(code) };
        ptr(arg)
    }

    #[test]
    fn call42() {
        let result: u64 = call_jit(99, &[Inst::MOVZ { dest: X0, imm: 42 }, Inst::Ret], &[]);
        assert_eq!(result, 42);
    }

    #[rustfmt::skip]
    #[test]
    fn add() {
        let result: u64 = call_jit(90, &[
            Inst::MOVZ { dest: X1, imm: 10 },
            Inst::Three { op: Three::ADD, dest: X0, lhs: X0, rhs: X1 },
            Inst::Ret
        ], &[]);
        assert_eq!(result, 100);
        let result: u64 = call_jit(80, &[
            Inst::MOVZ { dest: X1, imm: 120 },
            Inst::Three { op: Three::ADD, dest: X0, lhs: X1, rhs: X0 },
            Inst::Ret
        ], &[]);
        assert_eq!(result, 200);
    }

    #[rustfmt::skip]
    #[test]
    fn tail_call() {
        extern "C" fn do_something(a: u64) -> u64 {
            a + 5
        }
    
        let addr: u64 = do_something as *const () as u64;
        let result: u64 = call_jit(addr, &[
            Inst::Two { op: Two::MOV, lhs: X1, rhs: X0 },
            Inst::MOVZ { dest: X0, imm: 20 },
            Inst::Br(X1)
        ], &[]);
        assert_eq!(result, 25);
    }

    #[rustfmt::skip]
    #[test]
    fn jump() {
        let i = &[
            Inst::CBZ(X0, Label(0)),
            Inst::MOVZ { dest: X0, imm: 10 },
            Inst::Ret,
            Inst::MOVZ { dest: X0, imm: 20 },
            Inst::Ret
        ];
        let l = &[3];
        let result: u64 = call_jit(0, i, l);
        assert_eq!(result, 20);
        let result: u64 = call_jit(1, i, l);
        assert_eq!(result, 10);
        let i = &[
            Inst::CBZ(X0, Label(0)),
            Inst::MOVZ { dest: X0, imm: 310 },
            Inst::B(Label(1)),
            Inst::MOVZ { dest: X0, imm: 320 },
            Inst::Ret
        ];
        let l = &[3, 4];
        let result: u64 = call_jit(0, i, l);
        assert_eq!(result, 320);
        let result: u64 = call_jit(1, i, l);
        assert_eq!(result, 310);
        let i = &[
            Inst::MOVZ { dest: X0, imm: 25 },
            Inst::B(Label(0)),
            Inst::Ret,
            Inst::MOVZ { dest: X0, imm: 15 },
            Inst::B(Label(1)),
        ];
        let l = &[3, 2];
        let result: u64 = call_jit(0, i, l);
        assert_eq!(result, 15);
    }
}