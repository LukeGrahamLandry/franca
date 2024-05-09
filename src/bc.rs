//! Low level instructions that the interpreter can execute.
use std::marker::PhantomData;

use crate::ast::{LabelId, OverloadSetId, TypeInfo, Var};
use crate::bc_to_asm::store_to_ints;
use crate::compiler::Compile;
use crate::emit_bc::ResultLoc;
use crate::reflect::BitSet;
use crate::{
    ast::{FnType, FuncId, TypeId},
    compiler::{ExecTime, Res},
    err,
    ffi::InterpSend,
};
use crate::{unwrap, Map};
use codemap::Span;
use interp_derive::InterpSend;

#[derive(Copy, Clone, InterpSend, Debug)]
pub struct FloatMask {
    pub arg: u32,
    pub ret: u32,
}

#[derive(Copy, Clone, InterpSend, Debug, PartialEq, Eq)]
pub struct BbId(pub u16);

#[derive(Clone, InterpSend, Debug, Copy, PartialEq)]
pub enum Bc {
    CallDirect { f: FuncId },                             // <args:m> -> <ret:n> OR _ ->  <ret:n>
    CallDirectFlat { f: FuncId },                         // <ret_ptr:1> <arg_ptr:1> -> _
    CallSplit { ct: FuncId, rt: FuncId },                 // <args:m> -> <ret:n>
    CallFnPtr { ty: FnType, comp_ctx: bool },             // <ptr:1> <args:m> -> <ret:n>
    PushConstant { value: i64 },                          // _ -> <v:1>
    JumpIf { true_ip: BbId, false_ip: BbId, slots: u16 }, // <cond:1> -> _
    Goto { ip: BbId, slots: u16 },                        // _ -> _
    Ret,                                                  //
    GetNativeFnPtr(FuncId),                               // _ -> <ptr:1>
    Load { slots: u16 },                                  // <ptr:1> -> <?:n>
    StorePost { slots: u16 },                             // <?:n> <ptr:1> -> _
    StorePre { slots: u16 },                              // <ptr:1> <?:n> -> _
    AddrVar { id: u16 },                                  // _ -> <ptr:1>
    IncPtr { offset: u16 },                               // <ptr:1> -> <ptr:1>
    Pop { slots: u16 },                                   // <?:n> -> _
    TagCheck { expected: u16 },                           // <enum_ptr:1> -> <enum_ptr:1>  // TODO: replace with a normal function.
    Unreachable,
    NoCompile,
    LastUse { id: u16 },
    Pick { back: u16 },
    Noop,
    AddrFnResult,
    Dup,
    CopyToFrom { slots: u16 }, // <to_ptr:1> <from_ptr:1> -> _
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

#[derive(Clone)]
pub struct FnBody<'p> {
    pub blocks: Vec<BasicBlock>,
    pub vars: Vec<TypeId>,
    pub when: ExecTime,
    pub slot_types: Vec<TypeId>,
    pub func: FuncId,
    pub why: String,
    pub last_loc: Span,
    pub jump_targets: BitSet,
    pub if_debug_count: u16,
    pub(crate) _p: PhantomData<&'p ()>,
    pub aarch64_stack_bytes: Option<u16>,
    pub current_block: BbId,
    pub inlined_return_addr: Map<LabelId, (BbId, ResultLoc)>,
    pub clock: u16,
}

impl<'p> FnBody<'p> {
    pub fn add_var(&mut self, ty: TypeId) -> u16 {
        self.vars.push(ty);
        self.vars.len() as u16 - 1
    }
}

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
    Label(LabelId),
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

pub fn values_from_ints_one(compile: &mut Compile, ty: TypeId, ints: Vec<i64>) -> Res<'static, Vec<Value>> {
    let mut vals = vec![];
    values_from_ints(compile, ty, &mut ints.into_iter(), &mut vals)?;
    Ok(vals)
}

pub fn int_to_value(compile: &mut Compile, ty: TypeId, n: i64) -> Res<'static, Value> {
    let ty = compile.program.raw_type(ty);
    Ok(unwrap!(int_to_value_inner(&compile.program[ty], n), "too big for an int"))
}

pub fn int_to_value_inner(info: &TypeInfo, n: i64) -> Option<Value> {
    Some(match info {
        // TODO: struct and tuple with one field?
        TypeInfo::Scope | &TypeInfo::Struct { .. } | TypeInfo::Tuple(_) | TypeInfo::Tagged { .. } => return None,
        TypeInfo::Unknown | TypeInfo::Never => unreachable!("bad type"),
        &TypeInfo::Unique(ty, _) | &TypeInfo::Named(ty, _) => unreachable!("should be raw type {ty:?}"),
        TypeInfo::Unit => Value::Unit,
        TypeInfo::F64 => Value::F64(n as u64), // TODO: high bit?
        TypeInfo::Bool => Value::Bool(n != 0),
        TypeInfo::Fn(_) => Value::GetFn(FuncId::from_raw(n)),
        TypeInfo::Label(_) => Value::Label(LabelId::from_raw(n)),
        TypeInfo::Type => Value::Type(TypeId::from_raw(n)),
        TypeInfo::OverloadSet => Value::OverloadSet(OverloadSetId::from_raw(n)),
        TypeInfo::FnPtr(_) => {
            debug_assert!(n % 4 == 0);
            Value::I64(n)
        }
        &TypeInfo::Ptr(_) => Value::Heap(n as *mut i64),
        // TODO: remove any? its a boxed Value
        TypeInfo::Int(_) | TypeInfo::VoidPtr | TypeInfo::Any => Value::I64(n),
    })
}

pub fn values_from_ints(compile: &mut Compile, ty: TypeId, ints: &mut impl Iterator<Item = i64>, out: &mut Vec<Value>) -> Res<'static, ()> {
    let ty = compile.program.raw_type(ty); // without this (jsut doing it manually below), big AstExprs use so much recursion that you can only run it in release where it does tail call
    match &compile.program[ty] {
        TypeInfo::Scope => {
            let a = unwrap!(ints.next(), "");
            let b = unwrap!(ints.next(), "");
            out.push(Value::I64(a));
            out.push(Value::I64(b));
        }
        &TypeInfo::Struct { as_tuple: ty, .. } | &TypeInfo::Unique(ty, _) | &TypeInfo::Named(ty, _) => values_from_ints(compile, ty, ints, out)?,
        TypeInfo::Tuple(types) => {
            // TODO: no clone
            for ty in types.clone() {
                values_from_ints(compile, ty, ints, out)?;
            }
        }
        TypeInfo::Tagged { cases } => {
            let start = out.len();
            let payload_size = compile.sizes.slot_count(compile.program, ty) - 1;
            let tag = unwrap!(ints.next(), "");
            out.push(Value::I64(tag));
            let ty = cases[tag as usize].1;
            let value_size = compile.sizes.slot_count(compile.program, ty);
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
        info => {
            let n = unwrap!(ints.next(), "");
            let v = unwrap!(int_to_value_inner(info, n), "");
            out.push(v);
        }
    };
    Ok(())
}
