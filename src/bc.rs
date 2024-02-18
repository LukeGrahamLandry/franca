//! Low level instructions that the interpreter can execute.
use crate::{
    ast::{FnType, FuncId, TypeId, Var},
    compiler::{CErr, DebugInfo, ExecTime},
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
    ExpandTuple {
        from: StackOffset,
        to: StackRange,
    },
    Drop(StackRange),
    // TODO: having memory is bad!
    MoveCreateTuple {
        values: StackRange,
        target: StackOffset,
    },
    CloneCreateTuple {
        values: StackRange,
        target: StackOffset,
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Value {
    F64(u64), // TODO: hash
    I64(i64),
    Bool(bool),
    Enum {
        container_type: TypeId,
        tag: usize,
        value: Box<Self>,
    },
    Tuple {
        container_type: TypeId,
        values: Vec<Self>,
    },
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
        first: usize,
        count: usize,
    },
    Symbol(usize), // TODO: this is an Ident<'p> but i really dont want the lifetime
    OverloadSet(usize),
    CFnPtr {
        ptr: usize,
        ty: FnType,
    },
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
    pub local: HashMap<Var<'p>, (Value, TypeId)>,
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

    pub fn get(&self, k: Var<'p>) -> Option<(Value, TypeId)> {
        debug_assert!(self.is_valid);
        self.local.get(&k).cloned()
    }

    pub fn insert(&mut self, k: Var<'p>, v: (Value, TypeId)) -> Option<(Value, TypeId)> {
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
    pub fn to_tuple(self) -> Option<Vec<Value>> {
        if let Value::Tuple { values, .. } = self {
            Some(values)
        } else {
            None
        }
    }

    pub fn to_func(self) -> Option<FuncId> {
        if let Value::GetFn(f) = self {
            Some(f)
        } else {
            None
        }
    }

    pub fn new_box(values: Vec<Value>) -> Value {
        let count = values.len();
        let value = Box::into_raw(Box::new(InterpBox {
            references: 1,
            values,
        }));
        Value::Heap {
            value,
            first: 0,
            count,
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
