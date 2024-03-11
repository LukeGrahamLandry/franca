use std::marker::PhantomData;
use crate::ast::{FuncId, TypeId};
use crate::bc::Value;
use crate::experiments::arena::Arena;

#[derive(Debug)]
pub struct IrFunc<'a> {
    pub arena: &'a Arena<'a>,
    pub blocks: Vec<Block<'a>, &'a Arena<'a>>,
    pub alloc_a: Vec<TypeId, &'a Arena<'a>>,
    pub values: Vec<(TypeId, Item<'a>), &'a Arena<'a>>,
}

#[derive(Debug)]
pub enum Item<'a> {
    Value(Value),
    Offset(Val, usize),
    Register(i64),
    Tuple(Vec<Val, &'a Arena<'a>>),
    Scalar
}

#[derive(Debug)]
pub struct Block<'a> {
    pub args: (TypeId, Val),
    pub body: Vec<Inst, &'a Arena<'a>>,
    pub end: Ret<'a>
}

/// A Val could be a register, or dereference of a pointer in a register plus an offset.
#[derive(Clone, Copy, Debug)]
pub struct Val(pub usize);
#[derive(Clone, Copy, Debug)]
pub struct Ip(pub usize);

#[derive(Debug)]
pub enum Place {
    Val(Val),
    Deref {
        base: Val,
        offset: usize,
    },
    Lr,
}

#[derive(Debug)]
pub struct Call {
    pub convention: Cc,
    pub f: Callable,
    pub arg: Val,
    /// When the function's finished and wants to return, what should it do?
    /// The call site has to put the information in the right place (i.e. set lr for asm).
    pub then: Callable,
}

#[derive(Debug)]
pub enum Cc {
    CCall,  // returnAddress=lr, resetStack=growStack=sp, fp is callee saved.
    CRet,   // restore sp to value at CCall. jump to lr. never returns.
    PushFrame,  // returnAddress=lr, resetStack=fp, growStack=sp
    PopFrame, // set sp to resetStack. jump to returnAddress. never returns.
    Local // just jump
}

#[derive(Debug)]
pub enum Callable {
    Ptr(Val),
    Fn(FuncId),
    Block(Ip),
    Unreachable
}

#[derive(Debug)]
pub enum Inst {
    Load {
        src: Val,
        dest: Val,
    },
    Store {
        src: Val,
        dest_ptr: Val,
    },
    Addr {
        ptr: Val,
        dest: Val
    },
    AllocA {
        ty: TypeId,
        dest: Val,
    }
}

#[derive(Debug)]
pub enum Ret<'a> {
    Goto(Ip),
    GotoWith(Ip, Val),
    If {
        cond: Val,
        if_true: Ip,
        if_false: Ip,
    },
    Call(Call),
    Empty,
    Unused(PhantomData<&'a ()>)
}

