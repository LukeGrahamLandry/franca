//! Low level instructions that the interpreter can execute.
use crate::emit_bc::DebugInfo;
use crate::experiments::reflect::BitSet;
use crate::impl_index;
use crate::{
    ast::{FnType, FuncId, TypeId, Var},
    compiler::{ExecTime, Res},
    err,
    ffi::InterpSend,
    pool::Ident,
};
use codemap::Span;
use interp_derive::InterpSend;
use std::collections::HashMap;
use std::ops::Range;

#[derive(Clone, InterpSend)]
pub enum Bc<'p> {
    // Call through a runtime known function pointer.
    CallDynamic {
        f: StackOffset,
        ret: StackRange,
        arg: StackRange,
    },
    CallDirect {
        f: FuncId,
        ret: StackRange,
        arg: StackRange,
    },
    CallBuiltin {
        name: Ident<'p>,
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
    // Clone, Move, and Drop are for managing linear types.
    Clone {
        from: StackOffset,
        to: StackOffset,
    },
    CloneRange {
        from: StackRange,
        to: StackRange,
    },
    Move {
        from: StackOffset,
        to: StackOffset,
    },
    MoveRange {
        from: StackRange,
        to: StackRange,
    },
    Drop(StackRange),
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
    CallC {
        f: StackOffset,
        arg: StackRange,
        ret: StackRange,
        ty: FnType,
        comp_ctx: bool,
    },
    LastUse(StackRange),
    NoCompile,
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

#[derive(Default, Clone)]
pub struct SizeCache {
    pub known: Vec<Option<usize>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Value {
    F64(u64), // TODO: hash
    I64(i64),
    Bool(bool),
    // Both closures and types don't have values at runtime, all uses must be inlined.
    Type(TypeId),
    GetFn(FuncId),
    /// The empty tuple.
    Unit,
    // This is unsed to represent a function's empty stack space.
    // Crash if you try to read one.
    Poison,
    InterpAbsStackAddr(StackAbsoluteRange),
    // Note: you can't just put these in a function's arena because they get copied by value.
    Heap {
        value: *mut InterpBox,
        physical_first: usize,
        physical_count: usize,
    },
    Symbol(usize), // TODO: this is an Ident<'p> but i really dont want the lifetime
    OverloadSet(usize),
    /// TODO: Different from GetFn because this must be compiled and produces a real native function pointer that can be passed to ffi code.
    GetNativeFnPtr(FuncId),
}

#[derive(Debug, InterpSend, Clone, Hash, PartialEq, Eq)]
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

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq, InterpSend)]
pub struct StackAbsolute(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq, InterpSend)]
pub struct StackAbsoluteRange {
    pub first: StackAbsolute,
    pub count: usize,
}

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
    pub local: HashMap<Var<'p>, (Values, TypeId)>,
    pub is_valid: bool,
}

impl<'p> Constants<'p> {
    #[track_caller]
    pub fn close(&self, vars: &[Var<'p>]) -> crate::compiler::Res<'p, Self> {
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
        self.local.extend(other.local.clone())
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
}

impl Value {
    pub fn to_func(self) -> Option<FuncId> {
        if let Value::GetFn(f) | Value::GetNativeFnPtr(f) = self {
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
    pub fn is_poison(&self) -> bool {
        match self {
            Values::One(v) => *v == Value::Poison,
            Values::Many(v) => v.iter().any(|v| *v == Value::Poison),
        }
    }

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

impl_index!(BcReady<'p>, FuncId, Option<FnBody<'p>>, ready);
