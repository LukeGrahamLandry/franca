//! Low level instructions

use crate::ast::{LabelId, OverloadSetId, Program, TypeInfo, Var};
use crate::bc_to_asm::store_to_ints;
use crate::emit_bc::ResultLoc;
use crate::pool::Ident;
use crate::{
    ast::{FnType, FuncId, TypeId},
    compiler::{ExecTime, Res},
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
    CallFnPtr { ty: FnType, comp_ctx: bool },             // <ptr:1> <args:m> -> <ret:n>
    PushConstant { value: i64 },                          // _ -> <v:1>
    JumpIf { true_ip: BbId, false_ip: BbId, slots: u16 }, // <args:slots> <cond:1> -> !
    Goto { ip: BbId, slots: u16 },                        // <args:slots> -> !
    Ret,                                                  // <vals:n> -> _ OR _-> _
    GetNativeFnPtr(FuncId),                               // _ -> <ptr:1>
    Load { slots: u16 },                                  // <ptr:1> -> <?:n>
    StorePost { slots: u16 },                             // <?:n> <ptr:1> -> _
    StorePre { slots: u16 },                              // <ptr:1> <?:n> -> _
    AddrVar { id: u16 },                                  // _ -> <ptr:1>
    IncPtr { offset: u16 },                               // <ptr:1> -> <ptr:1>
    Pop { slots: u16 },                                   // <?:n> -> _
    TagCheck { expected: u16 },                           // <enum_ptr:1> -> <enum_ptr:1>  // TODO: replace with a normal function.
    AddrFnResult,                                         // _ -> <ptr:1>
    Dup,                                                  // <x:1> -> <x:1> <x:1>
    CopyToFrom { slots: u16 },                            // <to_ptr:1> <from_ptr:1> -> _
    NameFlatCallArg { id: u16, offset: u16 },             // _ -> _
    LastUse { id: u16 },                                  // _ -> _
    Unreachable,                                          // _ -> !
    GetCompCtx,                                           // _ -> <ptr:1>
    NoCompile,
    Noop,
    PushRelocatablePointer { bytes: &'static [u8] },
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
    pub var_names: Vec<Option<Var<'p>>>,
    pub when: ExecTime,
    pub func: FuncId,
    pub aarch64_stack_bytes: Option<u16>,
    pub current_block: BbId,
    pub inlined_return_addr: Map<LabelId, (BbId, ResultLoc)>,
    pub clock: u16,
    pub name: Ident<'p>,
    pub want_log: bool,
}

impl<'p> FnBody<'p> {
    pub fn add_var(&mut self, ty: TypeId) -> u16 {
        self.vars.push(ty);
        self.vars.len() as u16 - 1
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, InterpSend)]
pub enum Value {
    I64(i64),
    // Both closures and types don't have values at runtime, all uses must be inlined.
    GetFn(FuncId),
}

#[derive(InterpSend, Clone, Hash, PartialEq, Eq)]
pub enum Values {
    Unit,
    One(Value),
    Many(Vec<i64>),
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
            Values::Unit => Ok(Value::I64(0)),
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
            Values::Unit => vec![],
            Values::One(i) => store_to_ints(&mut [i].iter()),
            Values::Many(v) => v,
        }
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        match self {
            Values::Unit => 0,
            Values::One(_) => 1,
            Values::Many(v) => v.len(),
        }
    }

    pub(crate) fn unwrap_func_id(&self) -> FuncId {
        match *self {
            Values::One(Value::GetFn(f)) => f,
            Values::One(Value::I64(f)) => FuncId::from_raw(f),
            _ => unreachable!("ICE: expected func id not {self:?}"),
        }
    }
}

pub fn values_from_ints_one(program: &Program, ty: TypeId, ints: Vec<i64>) -> Res<'static, Vec<Value>> {
    let mut vals = vec![];
    values_from_ints(program, ty, &mut ints.into_iter(), &mut vals)?;
    Ok(vals)
}
pub fn to_values<'p, T: InterpSend<'p>>(program: &mut Program<'p>, t: T) -> Res<'p, Values> {
    if T::SIZE == 1 {
        let ty = T::get_type(program);
        // TODO: this is dumb. i make the vec here anyway, then throw it away, then remake it on the other side.
        Ok(Values::One(int_to_value(program, ty, t.serialize_to_ints_one()[0])?))
    } else {
        Ok(Values::Many(t.serialize_to_ints_one()))
    }
}

pub fn from_values<'p, T: InterpSend<'p>>(_rogram: &mut Program<'p>, t: Values) -> Res<'p, T> {
    let ints = t.vec();
    Ok(unwrap!(T::deserialize_from_ints(&mut ints.into_iter()), ""))
}

pub fn int_to_value(program: &Program, ty: TypeId, n: i64) -> Res<'static, Value> {
    let ty = program.raw_type(ty);
    Ok(unwrap!(int_to_value_inner(&program[ty], n), "too big for an int"))
}

pub fn int_to_value_inner(info: &TypeInfo, n: i64) -> Option<Value> {
    Some(match info {
        // TODO: array, struct and tuple with one field?
        TypeInfo::Array { .. } | &TypeInfo::Struct { .. } | TypeInfo::Tuple(_) | TypeInfo::Tagged { .. } => return None,
        TypeInfo::Unknown | TypeInfo::Never => unreachable!("bad type"),
        &TypeInfo::Enum { .. } | &TypeInfo::Unique(_, _) | &TypeInfo::Named(_, _) => unreachable!("should be raw type but {info:?}"),
        TypeInfo::Unit => unreachable!(),
        TypeInfo::Fn(_) => Value::GetFn(FuncId::from_raw(n)),
        TypeInfo::FnPtr(_) => {
            #[cfg(target_arch = "aarch64")]
            debug_assert!(n % 4 == 0);
            Value::I64(n)
        }
        TypeInfo::OverloadSet
        | TypeInfo::Label(_)
        | TypeInfo::Type
        | TypeInfo::Bool
        | TypeInfo::Ptr(_)
        | TypeInfo::F64
        | TypeInfo::Scope
        | TypeInfo::Int(_)
        | TypeInfo::VoidPtr => Value::I64(n),
    })
}

pub fn values_from_ints(program: &Program, ty: TypeId, ints: &mut impl Iterator<Item = i64>, out: &mut Vec<Value>) -> Res<'static, ()> {
    let ty = program.raw_type(ty); // without this (jsut doing it manually below), big AstExprs use so much recursion that you can only run it in release where it does tail call
    match &program[ty] {
        &TypeInfo::Struct { as_tuple: ty, .. } | &TypeInfo::Unique(ty, _) | &TypeInfo::Named(ty, _) => values_from_ints(program, ty, ints, out)?,
        TypeInfo::Tuple(types) => {
            for ty in types {
                values_from_ints(program, *ty, ints, out)?;
            }
        }
        TypeInfo::Tagged { cases } => {
            let start = out.len();
            let payload_size = program.slot_count(ty) - 1;
            let tag = unwrap!(ints.next(), "");
            out.push(Value::I64(tag));
            let ty = cases[tag as usize].1;
            let value_size = program.slot_count(ty);
            values_from_ints(program, ty, ints, out)?;

            for _ in 0..payload_size - value_size {
                // NOTE: the other guy must have already put padding there, so we have to pop that, not just add our own.
                // TODO: should preserve the value so you can do weird void cast tricks but meh until i remove the interp since I can't reconstruct the right types anyway.
                let _padding = unwrap!(ints.next(), "");
                out.push(Value::I64(99999));
            }
            let end = out.len();
            assert_eq!(end - start, payload_size as usize + 1, "{out:?}");
        }
        TypeInfo::Unit => {}
        info => {
            let n = unwrap!(ints.next(), "");
            let v = unwrap!(int_to_value_inner(info, n), "");
            out.push(v);
        }
    };
    Ok(())
}
