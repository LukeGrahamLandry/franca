//! Low level instructions that the interpreter can execute.
use crate::ast::{OverloadSetId, TypeInfo};
use crate::bc_to_asm::store_to_ints;
use crate::compiler::Compile;
use crate::emit_bc::DebugInfo;
use crate::reflect::BitSet;
use crate::{
    ast::{FnType, FuncId, TypeId},
    compiler::{ExecTime, Res},
    err,
    ffi::InterpSend,
};
use crate::{impl_index, unwrap};
use codemap::Span;
use interp_derive::InterpSend;
use std::ops::Range;

#[derive(Clone, InterpSend, Debug)]
pub enum Bc {
    CallDirect { f: FuncId },                 // <args:m> -> <ret:n>
    CallSplit { ct: FuncId, rt: FuncId },     // <args:m> -> <ret:n>
    CallFnPtr { ty: FnType, comp_ctx: bool }, // <ptr:1> <args:m> -> <ret:n>
    PushConstant { value: i64 },              // _ -> <v:1>
    JumpIf { true_ip: u16, false_ip: u16 },   // <cond:1> -> _
    Goto { ip: u16 },                         // _ -> _
    Ret,                                      //
    GetNativeFnPtr(FuncId),                   // _ -> <ptr:1>
    Load { slots: u16 },                      // <ptr:1> -> <?:n>
    Store { slots: u16 },                     // <?:n> <ptr:1> -> _
    AddrVar { id: u16 },                      // _ -> <ptr:1>
    IncPtr { offset: u16 },                   // <ptr:1> -> <ptr:1>
    Pop { slots: u16 },                       // <?:n> -> _
    TagCheck { expected: u16 },               // <enum_ptr:1> -> <enum_ptr:1>  // TODO: replace with a normal function.
    Unreachable,
    NoCompile,
    LastUse { id: u16 },
}

#[derive(Clone)]
pub struct FnBody<'p> {
    pub insts: Vec<Bc>,
    pub debug: Vec<DebugInfo<'p>>,
    pub arg_range: StackRange,
    pub stack_slots: usize,
    pub vars: Vec<TypeId>,
    pub when: ExecTime,
    pub slot_types: Vec<TypeId>,
    pub func: FuncId,
    pub why: String,
    pub last_loc: Span,
    pub jump_targets: BitSet,
}

impl<'p> FnBody<'p> {
    pub fn add_var(&mut self, ty: TypeId) -> u16 {
        self.vars.push(ty);
        self.vars.len() as u16 - 1
    }
}

#[derive(Clone, Default)]
pub struct BcReady<'p> {
    pub ready: Vec<Option<FnBody<'p>>>,
    pub sizes: SizeCache,
}

impl_index!(BcReady<'p>, FuncId, Option<FnBody<'p>>, ready);

#[derive(Default, Clone)]
pub struct SizeCache {
    pub known: Vec<Option<usize>>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Value {
    F64(u64), // TODO: hash
    I64(i64),
    Bool(bool),
    // Both closures and types don't have values at runtime, all uses must be inlined.
    Type(TypeId),
    GetFn(FuncId),
    /// The empty tuple.
    Unit,
    // Note: you can't just put these in a function's arena because they get copied by value.
    Heap(*mut i64),
    Symbol(u32), // TODO: this is an Ident<'p> but i really dont want the lifetime
    OverloadSet(OverloadSetId),
    /// TODO: Different from GetFn because this must be compiled and produces a real native function pointer that can be passed to ffi code.
    GetNativeFnPtr(FuncId),
    // TOOD: shrink
    SplitFunc {
        ct: FuncId,
        rt: FuncId,
    },
}
#[repr(C)]
#[derive(Debug, InterpSend, Copy, Clone, Hash, PartialEq, Eq)]
pub enum FuncRef {
    Exact(FuncId),
    Split { ct: FuncId, rt: FuncId },
}

impl FuncRef {
    pub fn as_value(self) -> Value {
        match self {
            FuncRef::Exact(f) => Value::GetFn(f),
            FuncRef::Split { ct, rt } => {
                if ct == rt {
                    Value::GetFn(rt)
                } else {
                    Value::SplitFunc { ct, rt }
                }
            }
        }
    }

    pub fn single<'p>(self) -> Res<'p, FuncId> {
        match self {
            FuncRef::Exact(f) => Ok(f),
            FuncRef::Split { ct, rt } => err!("Illigal split func ct={ct:?} rt={rt:?}",),
        }
    }

    // Can use this for getting type since they should be the same
    pub fn at_rt(self) -> FuncId {
        match self {
            FuncRef::Exact(f) => f,
            FuncRef::Split { rt, .. } => rt,
        }
    }
}

impl From<FuncId> for FuncRef {
    fn from(value: FuncId) -> Self {
        FuncRef::Exact(value)
    }
}
#[repr(C)]
#[derive(InterpSend, Clone, Hash, PartialEq, Eq)]
pub enum Values {
    One(Value),
    Many(Vec<i64>),
}

#[derive(Debug, Clone)]
pub enum Structured {
    Emitted(TypeId, StackRange),
    Const(TypeId, Values),
    TupleDifferent(TypeId, Vec<Structured>),
    RuntimeOnly(TypeId),
}

impl Structured {
    pub fn ty(&self) -> TypeId {
        match self {
            Structured::Emitted(ty, _) | Structured::Const(ty, _) | Structured::TupleDifferent(ty, _) | Structured::RuntimeOnly(ty) => *ty,
        }
    }

    pub fn expect<'p>(self) -> Res<'p, Values> {
        let ty = self.ty();
        Ok(unwrap!(self.get(), "not const {ty:?}"))
    }

    pub fn get(self) -> Option<Values> {
        match self {
            Structured::Emitted(_, _) | Structured::TupleDifferent(_, _) | Structured::RuntimeOnly(_) => None,
            Structured::Const(_, v) => Some(v),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Structured::Emitted(_, s) => s.count == 0,
            Structured::TupleDifferent(_, s) => s.is_empty(),
            Structured::RuntimeOnly(_) | Structured::Const(_, _) => false,
        }
    }

    pub fn unchecked_cast(self, ty: TypeId) -> Structured {
        match self {
            Structured::Emitted(_, v) => Structured::Emitted(ty, v),
            Structured::TupleDifferent(_, v) => Structured::TupleDifferent(ty, v),
            Structured::Const(_, v) => Structured::Const(ty, v),
            Structured::RuntimeOnly(_) => Structured::RuntimeOnly(ty),
        }
    }
}

// TOOD: dont switch the order. cri
impl From<(StackRange, TypeId)> for Structured {
    fn from((slot, ty): (StackRange, TypeId)) -> Self {
        Structured::Emitted(ty, slot)
    }
}

impl From<(Values, TypeId)> for Structured {
    fn from((value, ty): (Values, TypeId)) -> Self {
        Structured::Const(ty, value)
    }
}
#[repr(C)]
#[derive(Copy, Clone, PartialEq, InterpSend)]
pub struct StackOffset(pub usize);

#[repr(C)]
#[derive(Copy, Clone, PartialEq, InterpSend)]
pub struct StackRange {
    pub first: StackOffset,
    pub count: usize,
}

impl StackRange {
    #[track_caller]
    pub fn offset(&self, offset: usize) -> StackOffset {
        debug_assert!(offset < self.count);
        StackOffset(self.first.0 + offset)
    }

    #[track_caller]
    pub fn single(&self) -> StackOffset {
        debug_assert_eq!(self.count, 1, "{self:?}");
        self.first
    }

    pub fn range(&self, offset: usize, count: usize) -> StackRange {
        debug_assert!(self.count >= offset + count, "{self:?}[{offset}..{}]", offset + count);
        StackRange {
            first: self.offset(offset),
            count,
        }
    }
}

pub struct InterpBox {
    pub references: isize,
    pub values: Vec<Value>,
    pub is_constant: bool,
}

#[derive(Clone, Copy)]
pub struct ConstId(pub usize);

impl std::fmt::Debug for ConstId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "C{}", self.0)
    }
}

impl Values {
    pub fn as_overload_set<'p>(&self) -> Res<'p, OverloadSetId> {
        if let Value::OverloadSet(i) = self.clone().single()? {
            Ok(i)
        } else {
            err!("expected OverloadSet not {self:?}",)
        }
    }

    pub(crate) fn as_int_pair(&self) -> Res<'static, (i64, i64)> {
        match self {
            Values::One(_) => err!("expected (i64, i64)",),
            Values::Many(vals) => {
                if vals.len() == 2 {
                    Ok((vals[0], vals[1]))
                } else {
                    err!("expected (i64, i64)",)
                }
            }
        }
    }
}

impl Value {
    pub fn to_overloads(&self) -> Option<OverloadSetId> {
        if let &Value::OverloadSet(f) = self {
            Some(f)
        } else {
            None
        }
    }
}

impl From<Value> for Values {
    fn from(value: Value) -> Self {
        Values::One(value)
    }
}

impl From<Vec<Value>> for Values {
    fn from(value: Vec<Value>) -> Self {
        if value.len() == 1 {
            Values::One(value.into_iter().next().unwrap())
        } else {
            Values::Many(store_to_ints(&mut value.iter()))
        }
    }
}

impl Values {
    #[track_caller]
    pub fn single(self) -> Res<'static, Value> {
        match self {
            Values::One(v) => Ok(v),
            Values::Many(v) => {
                if v.len() == 1 {
                    todo!()
                    // return Ok(v.into_iter().next().unwrap());
                }
                err!("expected single found {v:?}",)
            }
        }
    }

    pub fn vec(self) -> Vec<i64> {
        match self {
            Values::One(i) => store_to_ints(&mut [i].iter()),
            Values::Many(v) => v,
        }
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        match self {
            Values::One(_) => 1,
            Values::Many(v) => v.len(),
        }
    }
}

impl IntoIterator for StackRange {
    type Item = usize;
    type IntoIter = Range<usize>;

    fn into_iter(self) -> Self::IntoIter {
        self.first.0..self.first.0 + self.count
    }
}

pub fn values_from_ints_one(compile: &mut Compile, ty: TypeId, ints: Vec<i64>) -> Res<'static, Vec<Value>> {
    let mut vals = vec![];
    values_from_ints(compile, ty, &mut ints.into_iter(), &mut vals)?;
    Ok(vals)
}

pub fn values_from_ints(compile: &mut Compile, ty: TypeId, ints: &mut impl Iterator<Item = i64>, out: &mut Vec<Value>) -> Res<'static, ()> {
    let ty = compile.program.raw_type(ty); // without this (jsut doing it manually below), big AstExprs use so much recursion that you can only run it in release where it does tail call
    match &compile.program[ty] {
        TypeInfo::Unknown | TypeInfo::Never => err!("bad type {}", compile.program.log_type(ty)),
        TypeInfo::Unit => {
            let _ = unwrap!(ints.next(), "");
            out.push(Value::Unit);
        }
        TypeInfo::F64 => {
            let n = unwrap!(ints.next(), "") as u64;
            out.push(Value::F64(n));
        }
        TypeInfo::Int(_) => {
            let n = unwrap!(ints.next(), "");
            out.push(Value::I64(n));
        }
        TypeInfo::Bool => {
            let n = unwrap!(ints.next(), "");
            out.push(Value::Bool(n != 0));
        }
        TypeInfo::Fn(_) => {
            let n = unwrap!(ints.next(), "");
            out.push(Value::GetFn(FuncId::from_raw(n)));
        }
        TypeInfo::Type => {
            let n = unwrap!(ints.next(), "");
            out.push(Value::Type(TypeId::from_raw(n)));
        }
        TypeInfo::Scope => {
            let a = unwrap!(ints.next(), "");
            let b = unwrap!(ints.next(), "");
            out.push(Value::I64(a));
            out.push(Value::I64(b));
        }
        TypeInfo::OverloadSet => {
            let n = unwrap!(ints.next(), "");
            out.push(Value::OverloadSet(OverloadSetId::from_raw(n)));
        }
        &TypeInfo::Struct { as_tuple: ty, .. } | &TypeInfo::Unique(ty, _) | &TypeInfo::Named(ty, _) => values_from_ints(compile, ty, ints, out)?,
        TypeInfo::Tuple(types) => {
            // TODO: no clone
            for ty in types.clone() {
                values_from_ints(compile, ty, ints, out)?;
            }
        }
        TypeInfo::Enum { cases } => {
            let start = out.len();
            let payload_size = compile.ready.sizes.slot_count(compile.program, ty) - 1;
            let tag = unwrap!(ints.next(), "");
            out.push(Value::I64(tag));
            let ty = cases[tag as usize].1;
            let value_size = compile.ready.sizes.slot_count(compile.program, ty);
            values_from_ints(compile, ty, ints, out)?;

            for _ in 0..payload_size - value_size {
                // NOTE: the other guy must have already put padding there, so we have to pop that, not just add our own.
                // TODO: should preserve the value so you can do weird void cast tricks but meh until i remove the interp since I can't reconstruct the right types anyway.
                let _padding = unwrap!(ints.next(), "");
                // debug_assert!(
                //     padding == 88888 || padding == 99999,
                //     "TODO: this can be removed if you don't want to require specific padding values anymore. {padding}"
                // );
                out.push(Value::I64(99999));
            }
            let end = out.len();
            assert_eq!(end - start, payload_size + 1, "{out:?}");
        }
        TypeInfo::FnPtr(_) => {
            let ptr = unwrap!(ints.next(), "");
            debug_assert!(ptr % 4 == 0);
            out.push(Value::I64(ptr));
        }
        &TypeInfo::Ptr(_) => {
            let addr = unwrap!(ints.next(), "") as usize as *mut i64;
            out.push(Value::Heap(addr));
        }
        TypeInfo::VoidPtr => {
            let n = unwrap!(ints.next(), "");
            out.push(Value::I64(n));
        }

        TypeInfo::Any => {
            // The actual rust type is 'Value', which is serialized as a box ptr.
            // We're loading it to values that will deserialized again by the InterpSend impl, so we don't dereference the pointer here.
            let n = unwrap!(ints.next(), "");
            out.push(Value::I64(n));
        }
    };
    Ok(())
}
