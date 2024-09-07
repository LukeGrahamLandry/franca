#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct SelfHosted(*mut ());

pub struct BackendImportVTable {
    pub get_jitted_function: extern "C" fn(s: SelfHosted, f: FuncId) -> BigOption<usize>,
    pub put_jitted_function: extern "C" fn(s: SelfHosted, f: FuncId, addr: usize),
    pub get_jit_addr: extern "C" fn(s: SelfHosted, id: BakedVarId) -> usize,
}

#[repr(transparent)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct FuncId(u32);

impl FuncId {
    pub fn as_index(self) -> usize {
        pub const MASK: u32 = 1 << 30;
        (self.0 & (!MASK)) as usize
    }

    pub fn from_index(f: usize) -> FuncId {
        pub const MASK: usize = 1 << 30;
        FuncId((f as usize & (MASK)) as u32)
    }
}

#[repr(transparent)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Ident(u32);

#[repr(transparent)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct BbId(pub u16);

#[repr(C, i64)]
#[derive(Clone, Debug, Copy, PartialEq)]
pub enum Bc {
    CallDirect { sig: u32, f: FuncId, tail: bool },
    CallFnPtr { sig: u32 },
    PushConstant { value: i64, ty: Prim },
    JumpIf { true_ip: BbId, false_ip: BbId, slots: u16 },
    Goto { ip: BbId, slots: u16 },
    GetNativeFnPtr(FuncId),
    Load { ty: Prim },
    StorePost { ty: Prim },
    StorePre { ty: Prim },
    AddrVar { id: u16 },
    SaveSsa { id: u16, ty: Prim },
    LoadSsa { id: u16 },
    IncPtrBytes { bytes: u16 },
    PeekDup(u16),
    CopyBytesToFrom { bytes: u16 },
    LastUse { id: u16 },
    Unreachable,
    GetCompCtx,
    NoCompile,
    PushGlobalAddr { id: BakedVarId },
    Snipe(u16),
    Ret0,
    Ret1(Prim),
    Ret2((Prim, Prim)),
    Nop,
    Intrinsic(Intrinsic),
    Switch(u32),
}

#[repr(C)]
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct PrimSig {
    pub args: &'static [Prim],
    pub ret1: BigOption<Prim>,
    pub ret2: BigOption<Prim>,
    pub return_value_bytes: u16,
    pub first_arg_is_indirect_return: bool,
    pub no_return: bool,
    pub arg_int_count: u8,
}

impl PrimSig {
    pub fn arg_slots(&self) -> usize {
        if self.first_arg_is_indirect_return {
            self.args.len() + 1
        } else {
            self.args.len()
        }
    }

    pub fn ret_slots(&self) -> usize {
        if self.ret1.is_some() {
            if self.ret2.is_some() {
                2
            } else {
                1
            }
        } else {
            0
        }
    }
}

#[repr(i64)]
#[derive(Clone, Debug, Copy, PartialEq)]
pub enum Prim {
    I8,
    I16,
    I32,
    I64,
    F64,
    F32,
    P64,
}

impl Prim {
    pub(crate) fn is_float(self) -> bool {
        matches!(self, Prim::F64 | Prim::F32)
    }
    pub(crate) fn int_count(self) -> i64 {
        if self.is_float() {
            0
        } else {
            1
        }
    }
}

#[repr(C)]
#[derive(Clone, Debug)]
pub struct BasicBlock {
    pub insts: Vec<Bc>,
    pub debug: [usize; 3],
    pub arg_prims: &'static [Prim],
    pub incoming_jumps: u16,
    pub clock: u16,
}

#[repr(C)]
pub struct FnBody {
    pub blocks: Vec<BasicBlock>,
    pub vars: Vec<VarSlotType>,
    _var_names: [usize; 3],
    _when: usize,
    _hash: i64,
    pub signeture: PrimSig,
    pub func: FuncId,
    _name: u32,
    pub switch_payloads: Vec<Vec<SwitchPayload>>,
    pub sig_payloads: Vec<PrimSig>,
}

#[repr(C)]
pub struct VarSlotType {
    pub size: u16,
    pub align: u16,
}

#[repr(C)]
pub struct SwitchPayload {
    pub value: i64,
    pub block: BbId,
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BakedVarId(pub u32);

#[repr(C, i64)]
#[derive(Debug, Clone)]
pub enum BakedVar {
    Zeros(usize),
    Bytes(Vec<u8>),
    VoidPtrArray(Vec<BakedEntry>),
}

#[repr(C, i64)]
#[derive(Debug, Clone, Copy)]
pub enum BakedEntry {
    Num(i64, Prim),
    FnPtr(FuncId),
    AddrOf(BakedVarId),
}

#[repr(C, i64)]
#[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq)]
pub enum BigOption<T> {
    Some(T),
    #[default]
    None = 1,
}

impl<T> BigOption<T> {
    pub(crate) fn as_mut(&mut self) -> Option<&mut T> {
        match self {
            BigOption::Some(t) => Some(t),
            BigOption::None => None,
        }
    }

    pub(crate) fn as_ref(&self) -> Option<&T> {
        match self {
            BigOption::Some(t) => Some(t),
            BigOption::None => None,
        }
    }
    pub(crate) fn unwrap(self) -> T {
        match self {
            BigOption::Some(t) => t,
            BigOption::None => panic!("Unwrapped missing Option."),
        }
    }

    pub(crate) fn is_none(&self) -> bool {
        match self {
            BigOption::Some(_) => false,
            BigOption::None => true,
        }
    }

    pub(crate) fn is_some(&self) -> bool {
        !self.is_none()
    }

    pub(crate) fn expect(self, arg: &str) -> T {
        match self {
            BigOption::Some(t) => t,
            BigOption::None => panic!("Missing Value. {arg}"),
        }
    }
}

#[repr(i64)]
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Intrinsic {
    Add,
    Sub,
    Mul,
    Div,
    UDiv,
    FAdd,
    FSub,
    FMul,
    FDiv,
    ShiftLeft,
    ShiftRightLogical,
    ShiftRightArithmetic,
    BitAnd,
    BitOr,
    BitXor,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    ULt,
    UGt,
    ULe,
    UGe,
    FEq,
    FNe,
    FLt,
    FGt,
    FLe,
    FGe,
    // First Single Argument
    BitNot,
    IntToFloatBits,
    FloatToIntBits,
    _Unused,
    IntToFloatValue,
    FloatToIntValue,
    ShrinkFloat,
    GrowFloat,
    IntToPtr,
    PtrToInt,
    SignExtend32To64,
    Trunc64To32,
    Trunc64To16,
    Trunc64To8,
    Trunc32To16,
    Trunc32To8,
    Trunc16To8,
    ZeroExtend32To64,
    ZeroExtend16To64,
    ZeroExtend8To64,
    ZeroExtend16To32,
    ZeroExtend8To32,
    ZeroExtend8To16,
}
