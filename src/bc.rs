//! Low level instructions

use std::cell::RefCell;

use crate::ast::{CallConv, LabelId, Program, TypeInfo, Var};
use crate::emit_bc::ResultLoc;
use crate::pool::Ident;
use crate::{
    ast::{FnType, FuncId, TypeId},
    compiler::{ExecStyle, Res},
    err,
    ffi::InterpSend,
};
use crate::{unwrap, Map};
use interp_derive::InterpSend;

#[derive(Copy, Clone, InterpSend, Debug, PartialEq, Eq)]
pub struct BbId(pub u16);

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum Bc {
    CallDirect { f: FuncId, tail: bool },                 // <args:m> -> <ret:n>
    CallDirectFlat { f: FuncId },                         // <ret_ptr:1> <arg_ptr:1> -> _
    CallFnPtr { ty: FnType, cc: CallConv },               // <ptr:1> <args:m> -> <ret:n>   |OR| <ptr:1> <ret_ptr:1> <arg_ptr:1> -> _
    PushConstant { value: i64 },                          // _ -> <v:1>
    JumpIf { true_ip: BbId, false_ip: BbId, slots: u16 }, // <args:slots> <cond:1> -> !
    Goto { ip: BbId, slots: u16 },                        // <args:slots> -> !
    GetNativeFnPtr(FuncId),                               // _ -> <ptr:1>
    Load { ty: Prim },                                    // <ptr:1> -> <?:n>
    StorePost { ty: Prim },                               // <?:n> <ptr:1> -> _
    StorePre { ty: Prim },                                // <ptr:1> <?:n> -> _
    AddrVar { id: u16 },                                  // _ -> <ptr:1>
    IncPtrBytes { bytes: u16 },                           // <ptr:1> -> <ptr:1>
    TagCheck { expected: u16 },                           // <enum_ptr:1> -> <enum_ptr:1>  // TODO: replace with a normal function.
    AddrFnResult,                                         // _ -> <ptr:1>
    PeekDup(u16),                                         // <x:1> <skip:n> -> <x:1> <skip:n> <x:1>,
    CopyBytesToFrom { bytes: u16 },                       // <to_ptr:1> <from_ptr:1> -> _
    NameFlatCallArg { id: u16, offset_bytes: u16 },       // _ -> _
    LastUse { id: u16 },                                  // _ -> _
    Unreachable,                                          // _ -> !
    GetCompCtx,                                           // _ -> <ptr:1>
    NoCompile,
    PushGlobalAddr { id: BakedVarId },
    Snipe(u16),
    Ret0, // flat call uses this too because code has already written to indirect return address.
    Ret1(Prim),
    Ret2((Prim, Prim)),
}

#[derive(Clone, Debug, Copy, PartialEq)]
pub enum Prim {
    I8,
    I16,
    I32,
    I64,
    F64,
    P64,
}

impl Prim {
    pub(crate) fn align_bytes(self) -> usize {
        match self {
            Prim::I8 => 1,
            Prim::I16 => 2,
            Prim::I32 => 4,
            Prim::P64 | Prim::I64 | Prim::F64 => 8,
        }
    }

    pub(crate) fn float(self) -> u32 {
        if self == Prim::F64 {
            1
        } else {
            0
        }
    }
}

#[derive(Clone)]
pub struct BasicBlock {
    pub insts: Vec<Bc>,
    pub arg_slots: u16,
    pub arg_float_mask: u32,
    pub incoming_jumps: usize,
    pub clock: u16,
    pub height: u16,
}

pub struct FnBody<'p> {
    pub blocks: Vec<BasicBlock>,
    pub vars: Vec<TypeId>,
    pub var_names: Vec<Option<Var<'p>>>,
    pub when: ExecStyle,
    pub func: FuncId,
    pub current_block: BbId,
    pub inlined_return_addr: Map<LabelId, (BbId, ResultLoc)>,
    pub clock: u16,
    pub name: Ident<'p>,
    pub want_log: bool,
}

#[derive(Debug, Clone, Copy, InterpSend, PartialEq)]
pub struct BakedVarId(pub u32);

// A piece of static data that can be baked into an output file (c code, object, etc).
// TODO: deduplicate
// TODO: track type so structs can be more than just a blob of bytes.
// TODO: distinguish between constant and static. For now, everything is a mutable static because the language doesn't have the concept of const pointers.
#[derive(Debug, Clone)]
pub enum BakedVar {
    Zeros { bytes: usize },
    Bytes(Vec<u8>),
    Num(i64),
    FnPtr(FuncId),
    AddrOf(BakedVarId),
    VoidPtrArray(Vec<BakedVarId>),
}

#[derive(Debug, Default)]
pub struct Baked {
    pub values: RefCell<Vec<(BakedVar, *const u8)>>,
}

impl Baked {
    pub(crate) fn reserve(&self, ptr: *const u8) -> BakedVarId {
        let mut vals = self.values.borrow_mut();
        vals.push((BakedVar::Bytes(vec![]), ptr));
        BakedVarId(vals.len() as u32 - 1)
    }

    pub(crate) fn set(&self, id: BakedVarId, val: BakedVar) {
        let mut vals = self.values.borrow_mut();
        vals[id.0 as usize].0 = val;
    }

    pub(crate) fn get(&self, id: BakedVarId) -> (BakedVar, *const u8) {
        let v = self.values.borrow();
        v[id.0 as usize].clone()
    }

    pub(crate) fn make(&self, val: BakedVar, ptr: *const u8) -> BakedVarId {
        let id = self.reserve(ptr);
        self.set(id, val);
        id
    }
}

impl<'p> FnBody<'p> {
    pub fn add_var(&mut self, ty: TypeId) -> u16 {
        self.vars.push(ty);
        self.vars.len() as u16 - 1
    }
}

pub type Value = i64;
#[derive(InterpSend, Clone, Hash, PartialEq, Eq, Debug)]
pub struct Values(pub Vec<u8>);

impl From<u32> for Values {
    fn from(value: u32) -> Self {
        Values::many(value.to_le_bytes().to_vec())
    }
}

impl Values {
    pub fn bytes(&self) -> &[u8] {
        &self.0
    }
    pub fn unit() -> Self {
        Self(vec![])
    }

    pub fn one(v: i64) -> Self {
        Self(v.to_le_bytes().to_vec())
    }

    pub fn many(v: Vec<u8>) -> Self {
        Self(v)
    }

    pub fn is_unit(&self) -> bool {
        self.0.is_empty()
    }
}

impl From<Value> for Values {
    fn from(value: Value) -> Self {
        Values::one(value)
    }
}

impl Values {
    pub(crate) fn unwrap_func_id(&self) -> FuncId {
        debug_assert_eq!(self.0.len(), 4);
        let i = ReadBytes { bytes: &self.0, i: 0 }.next_u32().unwrap();
        FuncId::from_raw(i)
    }
}

#[track_caller]
pub fn to_values<'p, T: InterpSend<'p>>(program: &mut Program<'p>, t: T) -> Res<'p, Values> {
    T::get_or_create_type(program); // sigh
    let bytes = t.serialize_to_ints_one(program);
    Ok(Values::many(bytes))
}

pub fn from_values<'p, T: InterpSend<'p>>(program: &Program<'p>, t: Values) -> Res<'p, T> {
    let mut reader = ReadBytes { bytes: &t.0, i: 0 };
    let res = Ok(unwrap!(T::deserialize_from_ints(program, &mut reader), "{} from {reader:?}", T::name()));
    assert_eq!(reader.i, reader.bytes.len());
    res
}

// When binding const arguments you want to split a large value into smaller ones that can be referred to by name.
pub fn chop_prefix<'p>(program: &Program<'p>, prefix: TypeId, t: &mut ReadBytes) -> Res<'p, Values> {
    let info = program.get_info(prefix);
    t.align_to(info.align_bytes as usize);
    let bytes = info.stride_bytes;
    let taken = unwrap!(t.take(bytes as usize), "");
    Ok(Values::many(taken.to_vec()))
}

/// Take some opaque bytes and split them into ints. So (u8, u8) becomes a vec of two i64 but u16 becomes just one.
pub fn deconstruct_values(
    program: &Program,
    ty: TypeId,
    bytes: &mut ReadBytes,
    out: &mut Vec<i64>,
    offsets: &mut Option<&mut Vec<(Prim, u16)>>, // this is stupid but how else do you call it in a loop??
) -> Res<'static, ()> {
    let size = program.get_info(ty).stride_bytes as usize;
    debug_assert!(
        size <= bytes.bytes.len() - bytes.i,
        "deconstruct_values of {} wants {size} bytes but found {bytes:?}",
        program.log_type(ty)
    );
    let ty = program.raw_type(ty);
    match &program[ty] {
        TypeInfo::Unknown | TypeInfo::Never => err!("invalid type",),
        TypeInfo::F64 | TypeInfo::FnPtr { .. } | TypeInfo::Ptr(_) | TypeInfo::VoidPtr => {
            let offset = bytes.i;
            out.push(unwrap!(bytes.next_i64(), ""));
            if let Some(offsets) = offsets {
                offsets.push((Prim::I64, offset as u16));
            }
        }
        TypeInfo::Int(_) => {
            let offset = bytes.i;
            let (value, prim) = match program.get_info(ty).stride_bytes {
                1 => (unwrap!(bytes.next_u8(), "") as i64, Prim::I8),
                2 => (unwrap!(bytes.next_u16(), "") as i64, Prim::I16),
                4 => (unwrap!(bytes.next_u32(), "") as i64, Prim::I32),
                8 => (unwrap!(bytes.next_i64(), ""), Prim::I64),
                n => todo!("bad int stride {n}"),
            };
            out.push(value);
            if let Some(offsets) = offsets {
                offsets.push((prim, offset as u16));
            }
        }
        TypeInfo::Bool => {
            let offset = bytes.i;
            out.push(unwrap!(bytes.next_u8(), "") as i64);
            if let Some(offsets) = offsets {
                offsets.push((Prim::I8, offset as u16));
            }
        }
        &TypeInfo::Array { inner, len } => {
            let inner_align = program.get_info(inner).align_bytes;
            for _ in 0..len {
                bytes.align_to(inner_align as usize);
                deconstruct_values(program, inner, bytes, out, offsets)?;
            }
        }
        TypeInfo::Struct { fields, layout_done } => {
            assert!(*layout_done);
            let mut prev = 0;
            for t in fields {
                assert!(prev <= t.byte_offset);
                bytes.i = t.byte_offset;
                deconstruct_values(program, t.ty, bytes, out, offsets)?;
                prev = t.byte_offset;
            }
            bytes.align_to(program.get_info(ty).align_bytes as usize); // eat trailing stride padding
        }
        TypeInfo::Tagged { .. } => todo!("tagged {}", program.log_type(ty)),
        &TypeInfo::Enum { raw, .. } => deconstruct_values(program, raw, bytes, out, offsets)?,
        TypeInfo::Unique(_, _) | TypeInfo::Named(_, _) => unreachable!(),
        TypeInfo::Unit => {}
        TypeInfo::Type | TypeInfo::Fn(_) | TypeInfo::OverloadSet | TypeInfo::Scope | TypeInfo::Label(_) => {
            let offset = bytes.i;
            out.push(unwrap!(bytes.next_u32(), "") as i64);
            if let Some(offsets) = offsets {
                offsets.push((Prim::I32, offset as u16));
            }
        }
    }
    Ok(())
}

#[derive(Debug)]
pub struct ReadBytes<'a> {
    pub bytes: &'a [u8],
    pub i: usize,
}

impl<'a> ReadBytes<'a> {
    pub fn next_u8(&mut self) -> Option<u8> {
        if self.i < self.bytes.len() {
            self.i += 1;
            Some(self.bytes[self.i - 1])
        } else {
            None
        }
    }
    pub fn next_u16(&mut self) -> Option<u16> {
        if self.i + 1 < self.bytes.len() {
            self.i += 2;
            Some(u16::from_ne_bytes([self.bytes[self.i - 2], self.bytes[self.i - 1]]))
        } else {
            None
        }
    }
    pub fn next_u32(&mut self) -> Option<u32> {
        if self.i + 3 < self.bytes.len() {
            self.i += 4;
            Some(u32::from_ne_bytes([
                self.bytes[self.i - 4],
                self.bytes[self.i - 3],
                self.bytes[self.i - 2],
                self.bytes[self.i - 1],
            ]))
        } else {
            None
        }
    }
    pub fn next_i64(&mut self) -> Option<i64> {
        if self.i + 7 < self.bytes.len() {
            self.i += 8;
            Some(i64::from_ne_bytes([
                self.bytes[self.i - 8],
                self.bytes[self.i - 7],
                self.bytes[self.i - 6],
                self.bytes[self.i - 5],
                self.bytes[self.i - 4],
                self.bytes[self.i - 3],
                self.bytes[self.i - 2],
                self.bytes[self.i - 1],
            ]))
        } else {
            None
        }
    }

    pub fn take(&mut self, len: usize) -> Option<&[u8]> {
        if self.bytes.len() - self.i >= len {
            let res = Some(&self.bytes[self.i..self.i + len]);
            self.i += len;
            res
        } else {
            None
        }
    }

    pub fn align_to(&mut self, v: usize) {
        while self.i % v != 0 {
            self.i += 1; // TODO: math
        }
    }
}

#[derive(Default)]
pub struct WriteBytes(pub Vec<u8>);

impl WriteBytes {
    pub fn push_u8(&mut self, v: u8) {
        self.0.push(v)
    }

    pub fn push_u16(&mut self, v: u16) {
        for v in v.to_le_bytes() {
            self.0.push(v)
        }
    }

    pub fn push_u32(&mut self, v: u32) {
        for v in v.to_le_bytes() {
            self.0.push(v)
        }
    }

    pub fn push_i64(&mut self, v: i64) {
        for v in v.to_le_bytes() {
            self.0.push(v)
        }
    }

    pub fn align_to(&mut self, v: usize) {
        while self.0.len() % v != 0 {
            self.push_u8(0);
        }
    }
}

#[must_use]
pub fn align_to(offset: usize, align: usize) -> usize {
    if offset % align == 0 {
        offset
    } else {
        offset + align - offset % align
    }
}
#[must_use]
pub fn align_backwards(offset: usize, align: usize) -> usize {
    if offset % align == 0 {
        offset
    } else {
        offset - align + offset % align
    }
}
