//! Low level instructions

use std::mem;
use std::ptr::{slice_from_raw_parts, slice_from_raw_parts_mut};

use crate::ast::{Program, Var};
use crate::self_hosted::Ident;
use crate::unwrap;
use crate::{assert_eq, BitSet};
use crate::{
    ast::{FuncId, TypeId},
    compiler::{ExecStyle, Res},
    err,
    ffi::InterpSend,
};

#[repr(transparent)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct BbId(pub u16);

// TODO: suddenly my new PrimSig makes this super chonky. 80 bytes is not ok
#[repr(C, i64)]
#[derive(Clone, Debug, Copy, PartialEq)]
pub enum Bc<'p> {
    CallDirect { sig: PrimSig<'p>, f: FuncId, tail: bool }, // <args:m> -> <ret:n>
    CallFnPtr { sig: PrimSig<'p> },                         // <ptr:1> <args:m> -> <ret:n>   |OR| <ptr:1> <ret_ptr:1> <arg_ptr:1> -> _
    PushConstant { value: i64, ty: Prim },                  // _ -> <v:1>
    JumpIf { true_ip: BbId, false_ip: BbId, slots: u16 },   // <args:slots> <cond:1> -> !
    Goto { ip: BbId, slots: u16 },                          // <args:slots> -> !
    GetNativeFnPtr(FuncId),                                 // _ -> <ptr:1>
    Load { ty: Prim },                                      // <ptr:1> -> <?:n>
    StorePost { ty: Prim },                                 // <?:n> <ptr:1> -> _
    StorePre { ty: Prim },                                  // <ptr:1> <?:n> -> _
    AddrVar { id: u16 },                                    // _ -> <ptr:1>
    SaveSsa { id: u16, ty: Prim },                          // <p:1> -> _
    LoadSsa { id: u16 },                                    // _ -> <p:1>
    IncPtrBytes { bytes: u16 },                             // <ptr:1> -> <ptr:1>
    PeekDup(u16),                                           // <x:1> <skip:n> -> <x:1> <skip:n> <x:1>,
    CopyBytesToFrom { bytes: u16 },                         // <to_ptr:1> <from_ptr:1> -> _
    LastUse { id: u16 },                                    // _ -> _
    Unreachable,                                            // _ -> !
    GetCompCtx,                                             // _ -> <ptr:1>
    NoCompile,
    PushGlobalAddr { id: BakedVarId },
    Snipe(u16),
    Ret0, // big return uses this too because code has already written to indirect return address.
    Ret1(Prim),
    Ret2((Prim, Prim)),
    Nop,
}

#[repr(C)]
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct PrimSig<'p> {
    pub args: &'p [Prim],
    pub ret1: BigOption<Prim>,
    pub ret2: BigOption<Prim>,
    pub arg_slots: u16,
    pub ret_slots: u16,
    pub return_value_bytes: u16,
    pub first_arg_is_indirect_return: bool,
    pub no_return: bool,
    pub arg_int_count: u8,
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
pub struct BasicBlock<'p> {
    pub insts: Vec<Bc<'p>>,
    pub arg_prims: &'p [Prim], // TODO: not ffi safe!
    pub incoming_jumps: u16,
    pub arg_slots: u16,
    pub clock: u16,
    pub height: u16, // TODO: remove. unused.
}

// TODO: (maybe) instead of storing TypeId of vars, store size+align.
//       then don't throw away fnbody after use, keep them in a hashmap to deduplicate generics.
//       like List(*T) will often generate the same code even with different types.
//       can keep a running hash as you construct the bc so maybe pretty fast to discard non-matches.
//       have that as a build option so you don't have to take the hit for debug builds if you don't want to.
//       but it actually might make it faster in general because it means giving less stuff to llvm which is 90% of the time.
//       I kinda want to wait for emit_bc and bc_to_asm to be self hosted tho cause then its less awkward to change things.
//       Note: you wont get all the matches until you really deduplicate because they might call different functions that actaully generate the same code,
//             so have to have a thing like redirects where if checks if a call has been deduplicated. -- Jul 5
#[repr(C)]
#[derive(Clone)]
pub struct FnBody<'p> {
    pub blocks: Vec<BasicBlock<'p>>,
    pub vars: Vec<TypeId>,
    pub var_names: Vec<BigOption<Var<'p>>>,
    pub when: ExecStyle,
    pub func: FuncId,
    pub current_block: BbId,
    pub clock: u16,
    pub name: Ident<'p>,
    pub want_log: bool,
    // TODO: this should be moved to EmitBc
    pub is_ssa_var: BitSet, // only used for debugging (and during emit_bc). bc has enough info for this.
    pub signeture: PrimSig<'p>,
    _alloc: [usize; 2],
    hash: i64,
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BakedVarId(pub u32);

// A piece of static data that can be baked into an output file (c code, object, etc).
// TODO: deduplicate
// TODO: track type so structs can be more than just a blob of bytes.
// TODO: distinguish between constant and static. For now, everything is a mutable static because the language doesn't have the concept of const pointers.

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
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Values {
    Big(Vec<u8>),
    Small(i64, u8),
}

impl From<u32> for Values {
    fn from(value: u32) -> Self {
        Self::Small(value as i64, 4)
    }
}

impl Values {
    pub(crate) fn bytes(&self) -> &[u8] {
        match self {
            Values::Big(bytes) => bytes,
            Values::Small(value, len) => unsafe { &*slice_from_raw_parts(value as *const i64 as *const u8, *len as usize) },
        }
    }

    pub(crate) fn unit() -> Self {
        Self::Small(0, 0)
    }

    pub(crate) fn one(v: i64) -> Self {
        Self::Small(v, 8)
    }

    pub(crate) fn many(v: Vec<u8>) -> Self {
        if v.len() <= 8 {
            Self::from_bytes(&v)
        } else {
            Self::Big(v)
        }
    }

    pub(crate) fn from_bytes(bytes: &[u8]) -> Self {
        if bytes.len() <= 8 {
            let mut v = 0;
            let mut shift = 0;
            for b in bytes {
                v |= (*b as i64) << shift;
                shift += 8;
            }
            Self::Small(v, bytes.len() as u8)
        } else {
            Self::Big(bytes.to_vec())
        }
    }

    pub(crate) fn len(&self) -> usize {
        self.bytes().len()
    }
}

impl From<i64> for Values {
    fn from(value: i64) -> Self {
        Values::one(value)
    }
}

impl Values {
    pub(crate) fn unwrap_func_id(&self) -> FuncId {
        debug_assert_eq!(self.bytes().len(), 4);
        let i = ReadBytes { bytes: self.bytes(), i: 0 }.next_u32().unwrap();
        FuncId::from_raw(i)
    }
}

use crate::export_ffi::{BigOption, BigResult::*};
#[track_caller]
pub(crate) fn to_values<'p, T: InterpSend<'p>>(_program: &mut Program<'p>, mut t: T) -> Res<'p, Values> {
    let bytes = unsafe { &mut *slice_from_raw_parts_mut(&mut t as *mut T as *mut u8, mem::size_of::<T>()) };
    debug_assert_eq!(bytes.as_ptr() as usize % mem::align_of::<T>(), 0);
    debug_assert_eq!(bytes.len(), mem::size_of::<T>());
    Ok(Values::from_bytes(bytes))
}

pub(crate) fn from_values<'p, T: InterpSend<'p>>(_program: &Program<'p>, t: Values) -> Res<'p, T> {
    assert_eq!(t.bytes().len(), mem::size_of::<T>(), "from_values {t:?}");
    debug_assert_eq!(t.bytes().as_ptr() as usize % mem::align_of::<T>(), 0);
    unsafe { Ok(std::ptr::read(t.bytes().as_ptr() as *const T)) }
}

// When binding const arguments you want to split a large value into smaller ones that can be referred to by name.
pub(crate) fn chop_prefix<'p>(program: &Program<'p>, prefix: TypeId, t: &mut ReadBytes) -> Res<'p, Values> {
    let info = program.get_info(prefix);
    debug_assert_eq!(t.i % info.align_bytes as usize, 0);
    let bytes = info.stride_bytes;
    let taken = unwrap!(t.take(bytes as usize), "");
    Ok(Values::many(taken.to_vec()))
}

#[derive(Debug)]
pub struct ReadBytes<'a> {
    pub bytes: &'a [u8],
    pub i: usize,
}

impl<'a> ReadBytes<'a> {
    pub(crate) fn next_u32(&mut self) -> Option<u32> {
        debug_assert_eq!(self.i % 4, 0);
        if self.i + 3 < self.bytes.len() {
            self.i += 4;
            Some(unsafe { *(self.bytes.as_ptr().add(self.i - 4) as *const u32) })
        } else {
            None
        }
    }

    pub(crate) fn take(&mut self, len: usize) -> Option<&[u8]> {
        if self.bytes.len() - self.i >= len {
            let res = Some(&self.bytes[self.i..self.i + len]);
            self.i += len;
            res
        } else {
            None
        }
    }
}

pub(crate) fn is_float(slot_index: usize, slots: u16, float_mask: u32) -> bool {
    (float_mask >> (slots - slot_index as u16 - 1)) & 1 == 1
}
