//! Low level instructions that the interpreter can execute.
use crate::{
    ast::{FuncId, Program, TypeId, Var},
    compiler::{DebugInfo, ExecTime},
    pool::Ident,
};
use codemap::Span;
use std::{collections::HashMap, ops::Deref, rc::Rc, sync::atomic::AtomicUsize};

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
    pub constants: ConstId,
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

#[derive(Debug, Clone, Copy)]
pub struct ConstId(pub usize);

// TODO: This must be super fucking slow
#[derive(Debug, Clone)]
pub struct SharedConstants<'p> {
    pub id: ConstId,
    pub parents: Vec<ConstId>,
    pub local: HashMap<Var<'p>, (Value, TypeId)>,
    // Constant names can be overloaded. Functions in general and anything in a generic impl use this.
    // The value in the key is arg of function or arg of generic + arg of function.
    pub overloads: HashMap<(Ident<'p>, Value), (Value, TypeId)>,
    pub references: usize,
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
}
