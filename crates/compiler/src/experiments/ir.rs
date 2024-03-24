#![allow(unused)]

use crate::ast::{FuncId, Program, TypeId};
use crate::bc::Value;
use crate::experiments::arena::Arena;
use crate::experiments::reflect::BitSet;
use std::fmt::{Debug, Formatter, Write};
use std::marker::PhantomData;

#[derive(Debug)]
pub struct IrFunc<'a> {
    pub arena: &'a Arena<'a>,
    pub blocks: Vec<Block<'a>, &'a Arena<'a>>,
    pub alloc_a: Vec<TypeId, &'a Arena<'a>>,
    pub values: Vec<(TypeId, Item<'a>), &'a Arena<'a>>,
    pub reachable: BitSet,
}

#[derive(Clone)]
pub enum Item<'a> {
    Value(Value),
    Offset { ptr: Val, bytes: usize },
    Register(i64),
    Tuple(Vec<Val, &'a Arena<'a>>),
    Gep { container: Val, index: usize },
    Scalar,
}

#[derive(Debug)]
pub struct Block<'a> {
    pub args: (TypeId, Val),
    pub body: Vec<Inst, &'a Arena<'a>>,
    pub end: Ret<'a>,
}

/// A Val could be a register, or dereference of a pointer in a register plus an offset.
#[derive(Clone, Copy)]
pub struct Val(pub usize);
#[derive(Clone, Copy)]
pub struct Ip(pub usize);

#[derive(Debug, Clone)]
pub struct Call {
    pub convention: Cc,
    pub f: Callable,
    pub arg: Val,
    /// When the function's finished and wants to return, what should it do?
    /// The call site has to put the information in the right place (i.e. set lr for asm).
    pub then: Callable,
}

#[derive(Debug, Copy, Clone)]
pub enum Cc {
    CCall,     // returnAddress=lr, resetStack=growStack=sp, fp is callee saved.
    CRet,      // restore sp to value at CCall. jump to lr. never returns.
    PushFrame, // returnAddress=lr, resetStack=fp, growStack=sp
    PopFrame,  // set sp to resetStack. jump to returnAddress. never returns.
    Local,     // just jump
}

#[derive(Debug, Copy, Clone)]
pub enum Callable {
    Ptr(Val),
    Fn(FuncId),
    Block(Ip),
    Unreachable,
}

#[derive(Debug, Clone)]
pub enum Inst {
    Load { src: Val, dest: Val },
    Store { src: Val, dest_ptr: Val },
    AllocA { ty: TypeId, dest: Val },
}

#[derive(Debug, Clone)]
pub enum Ret<'a> {
    Goto(Ip),
    GotoWith(Ip, Val),
    If { cond: Val, if_true: Ip, if_false: Ip },
    Call(Call),
    Empty,
    Return(Val),
    Unused(PhantomData<&'a ()>),
}

impl<'a> IrFunc<'a> {
    pub fn fixup(&mut self, program: &Program) {
        loop {
            let mut dirty = false;
            for i in 0..self.blocks.len() {
                self.reachable.set(i); // TODO: do this on create instead.
                if let Ret::Goto(target) = self.blocks[i].end {
                    let insts: Vec<_> = self.blocks[target.0].body.to_vec();
                    self.blocks[i].body.extend(insts.into_iter());
                    self.blocks[i].end = self.blocks[target.0].end.clone();
                    dirty = true;
                }
                if let Ret::GotoWith(target, arg) = self.blocks[i].end {
                    if self.values[arg.0].0.is_unit() {
                        let insts: Vec<_> = self.blocks[target.0].body.to_vec();
                        self.blocks[i].body.extend(insts.into_iter());
                        self.blocks[i].end = self.blocks[target.0].end.clone();
                        dirty = true;
                    }
                }
            }
            if !dirty {
                break;
            }
        }
        self.find_reachable();
    }

    pub fn find_reachable(&mut self) {
        let mut dirty = Vec::new_in(self.arena);
        dirty.push(Ip(0));
        self.reachable.clear(); // TODO: save from before since they'll never be reachable but then need to change the short circuiting.

        while let Some(i) = dirty.pop() {
            if self.reachable.get(i.0) {
                continue;
            }
            self.reachable.set(i.0);

            match &self.blocks[i.0].end {
                &Ret::Goto(b) => dirty.push(b),
                &Ret::GotoWith(b, _) => dirty.push(b),
                &Ret::If { if_true, if_false, .. } => {
                    dirty.push(if_true);
                    dirty.push(if_false);
                }
                Ret::Call(call) => {
                    if let Callable::Block(b) = call.f {
                        dirty.push(b)
                    }
                    if let Callable::Block(b) = call.then {
                        dirty.push(b)
                    }
                }
                Ret::Empty | Ret::Return(_) | Ret::Unused(_) => {}
            }
        }
    }

    pub fn log(&self, program: &Program) -> String {
        let mut out = String::from("digraph {\n");
        for (i, block) in self.blocks.iter().enumerate() {
            if !self.reachable.get(i) {
                continue;
            }
            let mut label = format!("B{i}");
            if !block.args.0.is_unit() {
                write!(label, "({:?}: {})", block.args.1, program.log_type(block.args.0)).unwrap();
            };
            label += "\n";
            for inst in &block.body {
                match inst {
                    Inst::Load { src, dest } => writeln!(label, "{dest:?} = load [{src:?}]"),
                    Inst::Store { src, dest_ptr } => {
                        writeln!(label, "store [{dest_ptr:?}] = {src:?}")
                    }
                    Inst::AllocA { ty, dest } => {
                        writeln!(label, "{dest:?} = alloca {}", program.log_type(*ty))
                    }
                }
                .unwrap();
            }
            writeln!(out, "B{i} [label=\"{label}\"]").unwrap();
            match &block.end {
                Ret::Goto(b) => writeln!(out, "B{i} -> {b:?}"),
                Ret::GotoWith(b, arg) => {
                    write!(out, "B{i} -> {b:?}").unwrap();
                    if !self.values[arg.0].0.is_unit() {
                        write!(out, "  [label=\"{:?} ← {arg:?}={:?}\"]", self.blocks[b.0].args.1, self.values[arg.0].1).unwrap();
                    }
                    writeln!(out)
                }
                Ret::If { cond, if_true, if_false } => writeln!(
                    out,
                    "B{i} -> {if_true:?} [label=\"{cond:?}=T\"]\nB{i} -> {if_false:?} [label=\"{cond:?}=F\"]"
                ),
                Ret::Call(call) => {
                    let show = |callable: &Callable| match callable {
                        Callable::Ptr(v) => format!("{v:?}"),
                        Callable::Fn(f) => format!("{f:?}"),
                        Callable::Block(b) => format!("{b:?}"),
                        Callable::Unreachable => String::from("NEVER"),
                    };
                    if let Callable::Block(b) = &call.then {
                        writeln!(
                            out,
                            "B{i} -> {b:?} [label=\"{:?} ← {} of {:?}\"]",
                            self.blocks[b.0].args.1,
                            show(&call.f),
                            call.arg
                        )
                    } else {
                        writeln!(out, "TODO: TAILCALL")
                    }
                }
                Ret::Empty => writeln!(out, "B{i} -> ???"),
                Ret::Return(val) => writeln!(out, "B{i} -> RETURN [label=\"ret ← {val:?}\"]"),
                Ret::Unused(_) => todo!(),
            }
            .unwrap();
        }

        out += "}";
        out
    }
}

// TODO: macro for these. FuncId and TypeId too.
impl Debug for Val {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}

impl Debug for Ip {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "B{}", self.0)
    }
}

impl Debug for FuncId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Fn{}", self.0)
    }
}

impl<'a> Debug for Item<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Item::Value(v) => write!(f, "{v:?}"),
            Item::Offset { .. } => todo!(),
            Item::Register(_) => todo!(),
            Item::Tuple(parts) => write!(f, "{parts:?}"),
            Item::Gep { container, index } => write!(f, "{container:?}[{index}]"),
            Item::Scalar => write!(f, "Scalar"),
        }
    }
}
