//! Low level instructions that the interpreter can execute.
use crate::ast::TypeInfo;
use crate::compiler::{CErr, Compile};
use crate::crc::CRc;
use crate::emit_bc::DebugInfo;
use crate::reflect::BitSet;
use crate::{
    ast::{FnType, FuncId, TypeId, Var},
    compiler::{ExecTime, Res},
    err,
    ffi::InterpSend,
    pool::Ident,
};
use crate::{impl_index, unwrap, STATS};
use codemap::Span;
use interp_derive::InterpSend;
use std::collections::HashMap;
use std::ops::Range;
use std::ptr::slice_from_raw_parts_mut;

#[derive(Clone, InterpSend, Debug)]
pub enum Bc<'p> {
    CallDirect {
        f: FuncId,
        ret: StackRange,
        arg: StackRange,
    },
    CallSplit {
        ct: FuncId,
        rt: FuncId,
        ret: StackRange,
        arg: StackRange,
    },
    LoadConstant {
        slot: StackOffset,
        value: Value,
    },
    JumpIf {
        cond: StackOffset,
        true_ip: usize,
        false_ip: usize,
    },
    Goto {
        ip: usize,
    },
    Ret(StackRange),
    AbsoluteStackAddr {
        of: StackRange,
        to: StackOffset,
    },
    DebugMarker(Ident<'p>, Ident<'p>),
    DebugLine(Span),
    Clone {
        from: StackOffset,
        to: StackOffset,
    },
    CloneRange {
        from: StackRange,
        to: StackRange,
    },
    SlicePtr {
        base: StackOffset,
        offset: usize,
        count: usize,
        ret: StackOffset,
    },
    Load {
        from: StackOffset,
        to: StackRange,
    },
    TagCheck {
        enum_ptr: StackOffset,
        value: i64,
    },
    Store {
        to: StackOffset,
        from: StackRange,
    },
    CallFnPtr {
        f: StackOffset,
        arg: StackRange,
        ret: StackRange,
        ty: FnType,
        comp_ctx: bool,
    },
    LastUse(StackRange),
    NoCompile,
    Unreachable,
    MarkContiguous(StackRange, TypeId),
}

#[derive(Clone)]
pub struct FnBody<'p> {
    pub insts: Vec<Bc<'p>>,
    pub debug: Vec<DebugInfo<'p>>,
    pub arg_range: StackRange,
    pub stack_slots: usize,
    pub vars: HashMap<Var<'p>, (StackRange, TypeId)>, // TODO: use a vec
    pub when: ExecTime,
    pub slot_types: Vec<TypeId>,
    pub func: FuncId,
    pub why: String,
    pub last_loc: Span,
    pub constants: Constants<'p>,
    pub to_drop: Vec<(StackRange, TypeId)>,
    /// true -> we might Drop but the put a value back. false -> it's an ssa intermediate only needed once.
    pub slot_is_var: BitSet,
    pub jump_targets: BitSet,
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
    Heap {
        value: *mut InterpBox,
        physical_first: usize,
        physical_count: usize,
    },
    Symbol(u32), // TODO: this is an Ident<'p> but i really dont want the lifetime
    OverloadSet(usize),
    /// TODO: Different from GetFn because this must be compiled and produces a real native function pointer that can be passed to ffi code.
    GetNativeFnPtr(FuncId),
    SplitFunc {
        ct: FuncId,
        rt: FuncId,
    },
}

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

#[derive(InterpSend, Clone, Hash, PartialEq, Eq)]
pub enum Values {
    One(Value),
    Many(Vec<Value>),
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

    pub fn get<'p>(self) -> Res<'p, Values> {
        match self {
            Structured::Emitted(_, _) | Structured::TupleDifferent(_, _) | Structured::RuntimeOnly(_) => {
                err!("not const {self:?}",)
            }
            Structured::Const(_, v) => Ok(v),
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

#[derive(Copy, Clone, PartialEq, InterpSend)]
pub struct StackOffset(pub usize);

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

#[derive(Debug, Clone, Default, InterpSend)]
pub struct Constants<'p> {
    pub local: CRc<HashMap<Var<'p>, (Values, TypeId)>>,
    pub is_valid: bool,
}

impl<'p> Constants<'p> {
    #[track_caller]
    pub fn close(&self, vars: &[Var<'p>]) -> Res<'p, Self> {
        debug_assert!(self.is_valid);
        let mut new = Self::empty();
        for k in vars {
            if let Some(val) = self.local.get(k) {
                new.local.insert(*k, val.clone());
            } else {
                // TODO: !quote captures a bunch of stuff that it doesn't actually need?
                //       this seems like a massive problem.
                // err!(CErr::VarNotFound(*k))
            }
        }
        Ok(new)
    }

    pub fn add_all(&mut self, other: &Self) {
        debug_assert!(self.is_valid && other.is_valid);
        for (k, v) in other.local.iter() {
            let _prev = self.local.insert(*k, v.clone());
            // TODO: this seems like a problem
            // assert!(prev.is_none() || prev.as_ref().unwrap() == v, "{:?} = {:?} -> {:?}", k, prev.unwrap(), v);
        }
    }

    pub fn get(&self, k: Var<'p>) -> Option<(Values, TypeId)> {
        debug_assert!(self.is_valid);
        self.local.get(&k).cloned()
    }

    pub fn insert(&mut self, k: Var<'p>, v: (Values, TypeId)) -> Option<(Values, TypeId)> {
        debug_assert!(self.is_valid);
        self.local.insert(k, v)
    }

    pub fn empty() -> Self {
        Self {
            local: Default::default(),
            is_valid: true,
        }
    }
}

impl Values {
    pub fn make_heap_constant(&mut self) {
        match self {
            Values::One(v) => {
                if let Value::Heap { value, .. } = v {
                    let value = unsafe { &mut **value };
                    value.is_constant = true;
                }
            }
            Values::Many(values) => {
                for v in values {
                    if let Value::Heap { value, .. } = v {
                        let value = unsafe { &mut **value };
                        value.is_constant = true;
                    }
                }
            }
        }
    }

    pub fn as_overload_set<'p>(&self) -> Res<'p, usize> {
        if let Value::OverloadSet(i) = self.clone().single()? {
            Ok(i)
        } else {
            err!("expected OverloadSet not {self:?}",)
        }
    }

    pub fn normalize(self) -> Self {
        match self {
            Values::One(_) => self,
            Values::Many(v) => {
                if v.len() == 1 {
                    Values::One(v[0])
                } else {
                    Values::Many(v)
                }
            }
        }
    }
}

impl Value {
    pub fn to_func(&self) -> Option<FuncId> {
        if let &Value::GetFn(f) | &Value::GetNativeFnPtr(f) = self {
            Some(f)
        } else {
            None
        }
    }

    pub fn new_box(values: Vec<Value>, is_constant: bool) -> Value {
        let count = values.len();
        let value = Box::into_raw(Box::new(InterpBox {
            references: 1,
            values,
            is_constant,
        }));
        unsafe {
            STATS.interp_box += 1;
            STATS.interp_box_values += count;
        }
        Value::Heap {
            value,
            physical_first: 0,
            physical_count: count,
        }
    }

    pub fn to_overloads(&self) -> Option<usize> {
        if let &Value::OverloadSet(f) = self {
            Some(f)
        } else {
            None
        }
    }

    pub fn to_bool(&self) -> Option<bool> {
        if let &Value::Bool(f) = self {
            Some(f)
        } else {
            None
        }
    }

    pub(crate) fn to_type(self) -> Option<TypeId> {
        if let Value::Type(f) = self {
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
            Values::Many(value)
        }
    }
}

impl From<Values> for Vec<Value> {
    fn from(value: Values) -> Self {
        match value {
            Values::One(v) => vec![v],
            Values::Many(v) => v,
        }
    }
}
impl From<Vec<Values>> for Values {
    fn from(value: Vec<Values>) -> Self {
        let mut res = vec![];
        for x in value {
            let x: Vec<_> = x.into();
            res.extend(x)
        }
        res.into()
    }
}

impl Values {
    #[track_caller]
    pub fn single(self) -> Res<'static, Value> {
        match self {
            Values::One(v) => Ok(v),
            Values::Many(v) => {
                if v.len() == 1 {
                    return Ok(v.into_iter().next().unwrap());
                }
                err!("expected single found {v:?}",)
            }
        }
    }

    pub fn vec(self) -> Vec<Value> {
        self.into()
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

impl<'p> Value {
    #[track_caller]
    pub fn to_int(self) -> Res<'p, i64> {
        if let Value::I64(r) = self {
            Ok(r)
        } else if let Value::Symbol(r) = self {
            // TODO: have a special unwrap method for this
            Ok(r as i64)
        } else {
            err!(CErr::TypeError("i64", self.into()))
        }
    }
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
        TypeInfo::OverloadSet => {
            let n = unwrap!(ints.next(), "");
            out.push(Value::OverloadSet(n as usize));
        }
        &TypeInfo::Struct { as_tuple: ty, .. } | &TypeInfo::Unique(ty, _) | &TypeInfo::Named(ty, _) => values_from_ints(compile, ty, ints, out)?,
        TypeInfo::Tuple(types) => {
            if types.len() == 2 && types[1] == TypeId::i64() && matches!(compile.program[types[0]], TypeInfo::Ptr(_)) {
                // (ptr, len)
                // TODO: HACK. just assuming this is a slice so we can reconstruct the box
                let addr = unwrap!(ints.next(), "") as usize;
                let len = unwrap!(ints.next(), "") as usize;
                debug_assert!(len < 1000, "read padding?");
                let val_ty = unwrap!(compile.program.unptr_ty(types[0]), "unreachable");
                let count = len * compile.ready.sizes.slot_count(compile.program, val_ty);
                let values = unsafe { &*slice_from_raw_parts_mut(addr as *mut i64, count) };
                let mut output = vec![];
                let mut values = values.iter().copied(); // NOTE: outside the loop, you dont want to read the first element n times!
                for _ in 0..len {
                    values_from_ints(compile, val_ty, &mut values, &mut output)?;
                }
                out.push(Value::new_box(output, false)); // ptr
                out.push(Value::I64(len as i64)); // len

                return Ok(());
            }
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
        TypeInfo::FnPtr(_) => todo!(),
        &TypeInfo::Ptr(val_ty) => {
            // TODO: untested -- Apr 19
            // TODO: this assumes that it points to exactly one thing so slices had special handling somewhere else.
            let addr = unwrap!(ints.next(), "") as usize;
            let count = compile.ready.sizes.slot_count(compile.program, val_ty);
            let values = unsafe { &*slice_from_raw_parts_mut(addr as *mut i64, count) };
            let mut output = vec![];
            let mut values = values.iter().copied();
            values_from_ints(compile, val_ty, &mut values, &mut output)?;
            out.push(Value::new_box(output, false));
        }
        TypeInfo::VoidPtr => todo!(),
        TypeInfo::Any => {
            // The actual rust type is 'Value', which is serialized as a box ptr.
            // We're loading it to values that will deserialized again by the InterpSend impl, so we don't dereference the pointer here.
            let n = unwrap!(ints.next(), "");
            out.push(Value::I64(n));
        }
    };
    Ok(())
}
