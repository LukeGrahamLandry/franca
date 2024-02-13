//! Low level instructions that the interpreter can execute.
use crate::{
    ast::{FuncId, TypeId, Var},
    compiler::{DebugInfo, ExecTime},
    pool::Ident,
};
use codemap::Span;
use std::{collections::HashMap, rc::Rc};

#[derive(Clone)]
pub enum Bc<'p> {
    CallDynamic {
        f: StackOffset,
        ret: StackRange,
        arg: StackRange,
    },
    CallDirectMaybeCached {
        f: FuncId,
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
    DerefPtr {
        from: StackOffset,
        to: StackRange,
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
    pub constants: SharedConstants<'p>,
}

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
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
    Array {
        container_type: TypeId,
        values: Vec<Self>,
    },
    Ptr {
        container_type: TypeId,
        value: *mut Self,
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

    // These are needed because they're using for bootstrapping the comptime types.
    Slice(TypeId),       // for `[]T`
    Map(TypeId, TypeId), // for `{}(K, V)`
    Symbol(usize),       // TODO: this is an Ident<'p> but i really dont want the lifetime
}

#[derive(Copy, Clone)]
pub struct StackOffset(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
pub struct StackAbsolute(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
pub struct StackAbsoluteRange {
    pub first: StackAbsolute,
    pub count: usize,
}

#[derive(Copy, Clone)]
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

// TODO: This must be super fucking slow
#[derive(Debug, Clone, Default)]
pub struct SharedConstants<'p> {
    pub parents: Vec<Rc<SharedConstants<'p>>>,
    pub local: HashMap<Var<'p>, (Value, TypeId)>,
}

impl<'p> SharedConstants<'p> {
    pub fn get(&self, var: &Var<'p>) -> Option<(Value, TypeId)> {
        self.local.get(var).cloned().or_else(|| {
            for p in &self.parents {
                if let Some(v) = p.get(var) {
                    return Some(v.clone());
                }
            }
            None
        })
    }

    pub fn insert(&mut self, k: Var<'p>, v: (Value, TypeId)) -> Option<(Value, TypeId)> {
        self.local.insert(k, v)
    }

    pub fn bake(self) -> Rc<Self> {
        Rc::new(self)
    }
}
