//! Low level instructions that the interpreter can execute.
use crate::{
    ast::{FnType, FuncId, TypeId, Var},
    compiler::{CErr, DebugInfo, ExecTime, Res},
    ffi::InterpSend,
    logging::err,
    pool::Ident,
};
use codemap::Span;
use interp_derive::InterpSend;
use std::collections::HashMap;

#[derive(Clone)]
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
    DebugMarker(&'static str, Ident<'p>),
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
    },
}

#[derive(Clone)]
pub struct FnBody<'p> {
    pub insts: Vec<Bc<'p>>,
    pub debug: Vec<DebugInfo<'p>>,
    pub stack_slots: usize,
    pub vars: HashMap<Var<'p>, (StackRange, TypeId)>, // TODO: use a vec
    pub when: ExecTime,
    pub slot_types: Vec<TypeId>,
    pub func: FuncId,
    pub why: String,
    pub last_loc: Span,
    pub constants: Constants<'p>,
    pub to_drop: Vec<(StackRange, TypeId)>,
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
    Heap {
        value: *mut InterpBox,
        logical_first: usize,
        logical_count: usize,
        stride: usize,
    },
    Symbol(usize), // TODO: this is an Ident<'p> but i really dont want the lifetime
    OverloadSet(usize),
    CFnPtr {
        ptr: usize,
        ty: FnType,
    },
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
}

impl Structured {
    pub fn ty(&self) -> TypeId {
        match self {
            Structured::Emitted(ty, _)
            | Structured::Const(ty, _)
            | Structured::TupleDifferent(ty, _) => *ty,
        }
    }

    pub fn get<'p>(self) -> Res<'p, Values> {
        match self {
            Structured::Emitted(_, _) | Structured::TupleDifferent(_, _) => {
                err!("not const {self:?}",)
            }
            Structured::Const(_, v) => Ok(v),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Structured::Emitted(_, s) => s.count == 0,
            Structured::TupleDifferent(_, s) => s.is_empty(),
            Structured::Const(_, _) => false,
        }
    }

    pub fn unchecked_cast(self, ty: TypeId) -> Structured {
        match self {
            Structured::Emitted(_, v) => Structured::Emitted(ty, v),
            Structured::TupleDifferent(_, v) => Structured::TupleDifferent(ty, v),
            Structured::Const(_, v) => Structured::Const(ty, v),
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

#[derive(Copy, Clone, PartialEq)]
pub struct StackOffset(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
pub struct StackAbsolute(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
pub struct StackAbsoluteRange {
    pub first: StackAbsolute,
    pub count: usize,
}

#[derive(Copy, Clone, PartialEq)]
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
        debug_assert_eq!(self.count, 1);
        self.first
    }
}

pub struct InterpBox {
    pub references: isize,
    pub values: Vec<Value>,
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
                err!(CErr::VarNotFound(*k))
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

impl Value {
    pub fn to_func(self) -> Option<FuncId> {
        if let Value::GetFn(f) = self {
            Some(f)
        } else {
            None
        }
    }

    pub fn new_box(stride: usize, values: Vec<Value>) -> Value {
        let count = values.len();
        debug_assert_eq!(values.len() % stride, 0);
        let value = Box::into_raw(Box::new(InterpBox {
            references: 1,
            values,
        }));
        Value::Heap {
            value,
            logical_first: 0,
            logical_count: count / stride,
            stride,
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
