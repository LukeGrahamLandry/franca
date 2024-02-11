#![allow(clippy::wrong_self_convention)]
#![deny(unused_must_use)] // Its a massive pain in the ass if you forget to make a compile error bubble up.
use std::fmt::{self, Write};
use std::marker::PhantomData;
use std::mem;
use std::path::Display;
use std::rc::Rc;
use std::{
    collections::HashMap,
    fmt::{format, Debug},
    mem::replace,
    ops::Deref,
    panic::Location,
    ptr,
};

use codemap::Span;
use interp_derive::InterpSend;
use tree_sitter::Point;

use crate::ast::{Annotation, VarType};
use crate::logging::PoolLog;
use crate::{
    ast::{
        Expr, FatExpr, FnType, Func, FuncId, LazyFnType, LazyType, Program, Stmt, TypeId, TypeInfo,
        Var,
    },
    pool::{Ident, StringPool},
};

#[macro_use]
use crate::logging::{logln, assert, assert_eq, ice, err, log, bin_int, unwrap};

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

pub type Res<'p, T> = Result<T, CompileError<'p>>;

#[derive(Copy, Clone)]
pub struct StackOffset(pub usize);

impl StackOffset {
    fn to_range(&self) -> StackRange {
        StackRange {
            first: *self,
            count: 1,
        }
    }
}

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
    fn index(&self, offset: usize) -> StackOffset {
        debug_assert!(offset < self.count);
        StackOffset(self.first.0 + offset)
    }
}

pub struct InterpBox {
    references: isize,
    values: Vec<Value>,
}

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
    DebugLine(Point),
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
}
#[derive(Debug, Clone)]
pub struct CallFrame<'p> {
    pub stack_base: StackAbsolute,
    current_func: FuncId,
    current_ip: usize,
    return_slot: StackAbsolute,
    return_count: usize,
    is_rust_marker: bool,
    pub debug_name: Ident<'p>,
    when: ExecTime,
    constants: Rc<SharedConstants<'p>>,
}

#[derive(Debug, Clone)]
pub struct DebugInfo<'p> {
    pub internal_loc: &'static Location<'static>,
    pub src_loc: Span,
    pub p: PhantomData<&'p str>,
}

#[derive(Clone)]
pub struct FnBody<'p> {
    pub insts: Vec<Bc<'p>>,
    pub debug: Vec<DebugInfo<'p>>,
    pub stack_slots: usize,
    pub vars: HashMap<Var<'p>, (StackRange, TypeId)>, // TODO: use a vec
    arg_names: Vec<Option<Ident<'p>>>,
    pub when: ExecTime,
    pub slot_types: Vec<TypeId>,
    pub func: FuncId,
    pub why: String,
    pub last_loc: Span,
    pub constants: SharedConstants<'p>,
}

impl<'p> FnBody<'p> {
    #[track_caller]
    fn push(&mut self, inst: Bc<'p>) -> usize {
        let ip = self.insts.len();
        self.insts.push(inst);

        #[cfg(feature = "some_log")]
        {
            self.debug.push(DebugInfo {
                internal_loc: Location::caller(),
                src_loc: self.last_loc,
                p: Default::default(),
            });
            debug_assert_eq!(self.insts.len(), self.debug.len(), "lost debug info");
        }
        ip
    }
}

//
// TODO:
// - any time you try to call a function it might not be ready yet
//   and compiling it might require running other comptime functions.
// - some types need to be passed between the interpreter and the comptime code.
// - some functions run in the interpreter when bootstraping but then are compiled into the compiler.
// - some functions are written in rust when bootstraping but then are compiled into the compiler.
//
// TODO: bucket array for stack so you can take pointers into it
pub struct Interp<'a, 'p> {
    pub pool: &'a StringPool<'p>,
    pub value_stack: Vec<Value>,
    pub call_stack: Vec<CallFrame<'p>>,
    pub program: &'a mut Program<'p>,
    pub ready: Vec<Option<FnBody<'p>>>,
    builtins: Vec<Ident<'p>>,
    log_depth: usize,
    // Since there's a kinda confusing recursive structure for interpreting a program, it feels useful to keep track of where you are.
    pub debug_trace: Vec<DebugState<'p>>,
    pub anon_fn_counter: usize,
    pub assertion_count: usize,
    currently_inlining: Vec<FuncId>,
    pub prelude_length: usize,
}

#[derive(Clone)]
struct StackHeights<'p> {
    value_stack: usize,
    call_stack: usize,
    debug_trace: usize,
    result: FnBody<'p>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DebugState<'p> {
    OuterCall(FuncId, Value),
    JitToBc(FuncId, ExecTime),
    RunInstLoop(FuncId),
    ComputeCached(FatExpr<'p>),
    ResolveFnType(FuncId, LazyType<'p>, LazyType<'p>),
    EvalConstants(FuncId),
}

#[derive(Clone)]
pub struct CompileError<'p> {
    pub loc: &'static Location<'static>,
    pub reason: CErr<'p>,
    pub trace: String,
    pub value_stack: Vec<Value>,
    pub call_stack: Vec<CallFrame<'p>>,
}

#[derive(Clone, Debug)]
pub enum CErr<'p> {
    UndeclaredIdent(Ident<'p>),
    ComptimeCallAtRuntime,
    Ice(&'static str),
    IceFmt(String),
    LeakedValue,
    StackDepthLimit,
    AddrRvalue(FatExpr<'p>),
    TypeError(&'static str, Value),
    Msg(String),
}

impl<'a, 'p> Interp<'a, 'p> {
    pub fn new(pool: &'a StringPool<'p>, program: &'a mut Program<'p>) -> Self {
        Self {
            pool,
            value_stack: vec![],
            call_stack: vec![],
            program,
            ready: vec![],
            builtins: vec![],
            log_depth: 0,
            debug_trace: vec![],
            anon_fn_counter: 0,
            assertion_count: 0,
            currently_inlining: vec![],
            prelude_length: 0, // TODO
        }
    }

    #[track_caller]
    fn push_state(&mut self, s: &DebugState<'p>) {
        self.debug_trace.push(s.clone());
        self.log_trace();
    }

    // TODO: would be nice if you passed it in so could make sure that you're popping the expected one
    //       but just check that its empty at the end and its fine.
    fn pop_state(&mut self, s: DebugState<'p>) {
        let found = self.debug_trace.pop().expect("state stack");
        debug_assert_eq!(found, s);
    }

    #[track_caller]
    fn error(&self, reason: CErr<'p>) -> CompileError<'p> {
        CompileError {
            loc: Location::caller(),
            reason,
            trace: self.log_trace(),
            value_stack: self.value_stack.clone(),
            call_stack: self.call_stack.clone(),
        }
    }

    pub fn add_declarations(
        &mut self,
        constants: &SharedConstants<'p>,
        ast: Func<'p>,
    ) -> Res<'p, ()> {
        let f = self.program.add_func(ast);
        self.ensure_compiled(constants, f, ExecTime::Comptime)?;
        Ok(())
    }

    pub fn lookup_unique_func(&self, name: Ident<'p>) -> Option<FuncId> {
        self.program.declarations.get(&name).map(|decls| {
            debug_assert_eq!(decls.len(), 1);
            decls[0]
        })
    }

    pub fn run(
        &mut self,
        constants: Option<&SharedConstants<'p>>,
        f: FuncId,
        arg: Value,
        when: ExecTime,
    ) -> Res<'p, Value> {
        let state = DebugState::OuterCall(f, arg.clone());
        self.push_state(&state);
        let init_heights = (self.call_stack.len(), self.value_stack.len());

        // A fake callframe representing the calling rust program.
        let constants =
            constants.map_or_else(|| Rc::new(SharedConstants::default()), |c| c.clone().bake());
        let marker_callframe = CallFrame {
            stack_base: StackAbsolute(0),
            current_func: FuncId(0), // TODO: actually reserve 0 cause i do this a lot
            current_ip: 0,
            return_slot: StackAbsolute(0),
            return_count: 0,
            is_rust_marker: true, // used for exiting the run loop. this is the only place we set it true.
            debug_name: self.pool.intern("@interp::run@"),
            when,
            constants,
        };
        self.call_stack.push(marker_callframe);
        // TODO: typecheck
        self.value_stack.push(Value::Poison); // For the return value.
        let ret = StackRange {
            first: StackOffset(0),
            count: 1, // TODO
        };

        // Call the function
        self.push_callframe(f, ret, arg, when)?;
        self.run_inst_loop()?;

        // Give the return value to the caller.
        let result = self.take_slots(ret);

        // Sanity checks that we didn't mess anything up for the next guy.
        assert!(self, result != Value::Poison);
        assert!(self, self.value_stack.pop() == Some(Value::Poison));
        let final_callframe = self.call_stack.pop().unwrap();
        // assert!(self, final_callframe == marker_callframe, "bad frame");
        let end_heights = (self.call_stack.len(), self.value_stack.len());
        assert!(self, init_heights == end_heights, "bad stack size");
        self.pop_state(state);
        Ok(result)
    }

    fn run_inst_loop(&mut self) -> Res<'p, ()> {
        let func = self.call_stack.last().unwrap().current_func;
        let state = DebugState::RunInstLoop(func);
        self.push_state(&state);
        loop {
            let i = self.next_inst();
            logln!("I: {:?}", i.log(self.pool));
            self.log_stack();
            match i {
                &Bc::CallDirect { f, ret, arg } => {
                    // preincrement our ip because ret doesn't do it.
                    // this would be different if i was trying to do tail calls?
                    self.bump_ip();
                    let arg = self.take_slots(arg);
                    let when = self.call_stack.last().unwrap().when;
                    self.push_callframe(f, ret, arg, when)?;
                    logln!("{}", self.log_callstack());
                    // don't bump ip here, we're in a new call frame.
                }
                &Bc::CallDirectMaybeCached { f, ret, arg } => {
                    // preincrement our ip because ret doesn't do it.
                    // this would be different if i was trying to do tail calls?
                    self.bump_ip();
                    let arg = self.take_slots(arg);
                    let key = (f, arg);
                    if let Some(prev) = self.program.generics_memo.get(&key).cloned() {
                        let ret_first = self.slot_to_index(ret.first);
                        self.expand_maybe_tuple(prev.clone(), ret_first, ret.count)?;
                    } else {
                        let when = self.call_stack.last().unwrap().when;
                        self.push_callframe(f, ret, key.1, when)?;
                        logln!("{}", self.log_callstack());
                        // don't bump ip here, we're in a new call frame.
                    }
                }
                Bc::LoadConstant { slot, value } => {
                    let slot = *slot;
                    let value = value.clone();
                    *self.get_slot_mut(slot) = value;
                    self.bump_ip();
                }
                &Bc::JumpIf {
                    cond,
                    true_ip,
                    false_ip,
                } => {
                    let cond = self.take_slot(cond);
                    let cond = self.to_bool(cond)?;
                    let next_ip = if cond { true_ip } else { false_ip };
                    let frame = self.call_stack.last_mut().unwrap();
                    frame.current_ip = next_ip;
                }
                &Bc::Goto { ip } => {
                    let frame = self.call_stack.last_mut().unwrap();
                    frame.current_ip = ip;
                }
                &Bc::CallBuiltin { name, ret, arg } => {
                    let name = self.pool.get(name);
                    // Calling Convention: arguments passed to a function are moved out of your stack.
                    let arg = self.take_slots(arg);
                    let value = self.runtime_builtin(name, arg.clone())?;
                    let abs = self.range_to_index(ret);
                    self.expand_maybe_tuple(value, abs.first, abs.count)?;
                    self.bump_ip();
                }
                &Bc::Ret(slot) => {
                    logln!("{}", self.log_callstack());
                    let value = self.take_slots(slot);
                    let frame = self.call_stack.pop().unwrap();
                    logln!(
                        "return from {:?} --- {:?} --- to {:?} count:{}",
                        slot,
                        value,
                        frame.return_slot,
                        frame.return_count
                    );

                    self.expand_maybe_tuple(value, frame.return_slot, frame.return_count)?;

                    // Release our stack space.
                    let size = self.value_stack.len() - frame.stack_base.0;
                    for i in 0..size {
                        let value = self.value_stack.pop().unwrap();
                        debug_assert_eq!(
                            value,
                            Value::Poison,
                            "{:?} was not empty",
                            StackOffset(size - i - 1)
                        )
                    }
                    // We don't increment the caller's ip, they did it on call.

                    logln!("{}", self.log_callstack());
                    if self.call_stack.last().unwrap().is_rust_marker {
                        break;
                    }
                }
                Bc::CallDynamic { f, ret, arg } => {
                    // preincrement our ip because ret doesn't do it.
                    // this would be different if i was trying to do tail calls?
                    let when = self.call_stack.last().unwrap().when;
                    let (f, arg, ret) = (*f, *arg, *ret);
                    self.bump_ip();
                    let f = self.take_slot(f);
                    let arg = self.take_slots(arg);
                    let f = self.to_func(f)?;
                    self.push_callframe(f, ret, arg, when)?;
                    logln!("{}", self.log_callstack());
                    // don't bump ip here, we're in a new call frame.
                }
                &Bc::MoveCreateTuple { values, target } => {
                    let tuple = self.take_slots(values);
                    *self.get_slot_mut(target) = tuple;
                    self.bump_ip();
                }
                &Bc::CloneCreateTuple { values, target } => {
                    let values = (0..values.count)
                        .map(|i| self.clone_slot(values.index(i)))
                        .collect();
                    *self.get_slot_mut(target) = Value::Tuple {
                        container_type: TypeId::any(),
                        values,
                    };
                    self.bump_ip();
                }
                &Bc::Move { from, to } => {
                    let v = self.take_slot(from);
                    *self.get_slot_mut(to) = v;
                    self.bump_ip();
                }
                &Bc::Clone { from, to } => {
                    let v = self.clone_slot(from);
                    *self.get_slot_mut(to) = v;
                    self.bump_ip();
                }
                &Bc::CloneRange { from, to } => {
                    debug_assert_eq!(from.count, to.count);
                    for i in 0..from.count {
                        let v = self.clone_slot(from.index(i));
                        *self.get_slot_mut(to.index(i)) = v;
                    }
                    self.bump_ip();
                }
                &Bc::MoveRange { from, to } => {
                    debug_assert_eq!(from.count, to.count);
                    for i in 0..from.count {
                        let v = self.take_slot(from.index(i));
                        *self.get_slot_mut(to.index(i)) = v;
                    }
                    self.bump_ip();
                }
                &Bc::ExpandTuple { from, to } => {
                    let tuple = self.take_slot(from);
                    let slot = self.slot_to_index(to.first);
                    self.expand_maybe_tuple(tuple, slot, to.count)?;
                    self.bump_ip();
                }
                &Bc::Drop(slot) => {
                    assert_ne!(slot.count, 0);
                    for i in 0..slot.count {
                        let _ = self.take_slot(slot.index(i));
                    }
                    self.bump_ip();
                }
                &Bc::AbsoluteStackAddr { of, to } => {
                    let ptr = self.range_to_index(of);
                    *self.get_slot_mut(to) = Value::InterpAbsStackAddr(ptr);
                    self.bump_ip();
                }
                Bc::DebugLine(_) | Bc::DebugMarker(_, _) => self.bump_ip(),
            }
        }
        self.pop_state(state);
        Ok(())
    }

    fn expand_maybe_tuple(
        &mut self,
        value: Value,
        first: StackAbsolute,
        expected_count: usize,
    ) -> Res<'p, ()> {
        logln!("Expand {:?} TO {} slots", value, expected_count);
        match expected_count.cmp(&1) {
            std::cmp::Ordering::Equal => {
                self.value_stack[first.0] = value;
            }
            std::cmp::Ordering::Less => {
                todo!("zero argument return. probably just works but untested.")
            }
            std::cmp::Ordering::Greater => {
                let values = self.to_seq(value)?;
                assert_eq!(self, values.len(), expected_count);
                let base = first.0;
                for (i, v) in values.into_iter().enumerate() {
                    // TODO: this needs to be a macro cause the cloning is a problem
                    assert!(
                        self,
                        self.value_stack[base + i] == Value::Poison,
                        "leaked value",
                    );
                    self.value_stack[base + i] = v;
                }
            }
        }
        Ok(())
    }

    fn runtime_builtin(&mut self, name: &str, arg: Value) -> Res<'p, Value> {
        logln!("runtime_builtin: {name} {arg:?}");
        let value = match name {
            "assert_eq" => {
                let (a, b) = self.split_to_pair(arg)?;
                assert_eq!(self, a, b, "runtime_builtin:assert_eq");
                self.assertion_count += 1; // sanity check for making sure tests actually ran
                Value::Unit
            }
            "is_comptime" => {
                assert_eq!(self, arg, Value::Unit);
                let when = self.call_stack.last().unwrap().when;
                Value::Bool(when == ExecTime::Comptime)
            }
            "get" => match arg {
                Value::InterpAbsStackAddr(addr) => {
                    if addr.count == 1 {
                        let value = self.value_stack[addr.first.0].clone();
                        assert_ne!(value, Value::Poison);
                        value
                    } else {
                        let values = &self.value_stack[addr.first.0..addr.first.0 + addr.count];
                        Value::Tuple {
                            container_type: TypeId::any(),
                            values: values.to_vec(),
                        }
                    }
                }
                Value::Heap {
                    value,
                    first,
                    count,
                } => {
                    let data = unsafe { &*value };
                    assert!(self, data.references > 0);
                    if count == 1 {
                        let value = data.values[0].clone();
                        assert_ne!(value, Value::Poison);
                        value
                    } else {
                        let values = &data.values[first..first + count];
                        Value::Tuple {
                            container_type: TypeId::any(),
                            values: values.to_vec(),
                        }
                    }
                }
                _ => panic!("Wanted ptr found {:?}", arg),
            },
            "set" => {
                let (addr, value) = self.to_pair(arg)?;
                match addr {
                    Value::InterpAbsStackAddr(addr) => {
                        // TODO: call drop if not poison
                        // Note: the slots you're setting to are allowed to contain poison
                        if addr.count == 1 {
                            self.value_stack[addr.first.0] = value;
                        } else {
                            let values = self.to_seq(value)?;
                            assert_eq!(self, values.len(), addr.count);
                            for (i, entry) in values.into_iter().enumerate() {
                                self.value_stack[addr.first.0 + i] = entry;
                            }
                        }
                        Value::Unit
                    }
                    Value::Heap {
                        value: ptr_value,
                        first,
                        count,
                    } => {
                        // Slicing operations are bounds checked.
                        let ptr = unsafe { &mut *ptr_value };
                        assert!(
                            self,
                            ptr.references > 0
                                && first < ptr.values.len()
                                && count < ptr.values.len()
                        );
                        if count == 1 {
                            ptr.values[first] = value;
                        } else {
                            let values = self.to_seq(value)?;
                            assert_eq!(self, values.len(), count);
                            for (i, entry) in values.into_iter().enumerate() {
                                ptr.values[first + 1] = entry;
                            }
                        }
                        Value::Unit
                    }
                    _ => panic!("Wanted ptr found {:?}", addr),
                }
            }
            "is_uninit" => {
                let range = match arg {
                    Value::InterpAbsStackAddr(addr) => {
                        &self.value_stack[addr.first.0..addr.first.0 + addr.count]
                    }
                    Value::Heap {
                        value,
                        first,
                        count,
                    } => {
                        let data = unsafe { &*value };
                        assert!(
                            self,
                            data.references > 0
                                && first < data.values.len()
                                && count < data.values.len()
                        );
                        &data.values[first..first + count]
                    }
                    _ => panic!("Wanted ptr found {:?}", arg),
                };
                let result = range.iter().all(|v| v == &Value::Poison);
                Value::Bool(result)
            }
            "len" => {
                if let Value::Tuple { .. } = arg {
                    panic!("Bad argument to builtin len: {arg:?}. consider passing a pointer to the value instead.");
                }
                let addr = self.to_stack_addr(arg)?;
                Value::I64(addr.count as i64)
            }
            "Ptr" => {
                // TODO: pointers shouldn't have a length
                let ty = self.to_type(arg)?;
                Value::Type(self.program.intern_type(TypeInfo::Ptr(ty)))
            }
            "Slice" => {
                let ty = self.to_type(arg)?;
                Value::Type(self.program.intern_type(TypeInfo::Ptr(ty)))
            }
            "is_oob_stack" => {
                let addr = self.to_stack_addr(arg)?;
                Value::Bool(addr.first.0 >= self.value_stack.len())
            }
            "slice" => {
                // last is not included
                let (addr, new_first, new_last) = self.to_triple(arg)?;
                let (new_first, new_last) = (self.to_int(new_first)?, self.to_int(new_last)?);
                assert!(
                    self,
                    new_first >= 0 && new_last >= 0 && new_first < new_last
                );
                match addr {
                    Value::InterpAbsStackAddr(addr) => {
                        assert!(
                            self,
                            (new_first as usize) < addr.count && (new_last as usize) <= addr.count
                        );
                        Value::InterpAbsStackAddr(StackAbsoluteRange {
                            first: StackAbsolute(addr.first.0 + new_first as usize),
                            count: (new_last - new_first) as usize,
                        })
                    }
                    Value::Heap {
                        value,
                        first,
                        count,
                    } => {
                        let abs_first = first + new_first as usize;
                        let abs_last = first + new_last as usize;
                        // Slicing operations are bounds checked.
                        let data = unsafe { &*value };
                        assert!(
                            self,
                            data.references > 0
                                && abs_first < data.values.len()
                                && abs_last <= data.values.len()
                        );
                        Value::Heap {
                            value,
                            first: abs_first,
                            count: abs_last - abs_first,
                        }
                    }
                    _ => panic!("Wanted ptr found {:?}", addr),
                }
            }
            "alloc" => {
                let (ty, count) = self.to_pair(arg)?;
                let (ty, count) = (self.to_type(ty)?, self.to_int(count)?);
                assert!(self, count >= 0);
                let count = count as usize * self.program.slot_count(ty);
                let values = vec![Value::Poison; count];
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
            "dealloc" => {
                let (ty, ptr) = self.to_pair(arg)?;
                let (ty, (ptr, ptr_first, ptr_count)) = (self.to_type(ty)?, self.to_heap_ptr(ptr)?);
                let slots = ptr_count * self.program.slot_count(ty);
                assert_eq!(self, ptr_first, 0);
                let ptr_val = unsafe { &*ptr };
                assert_eq!(self, ptr_val.references, 1);
                assert_eq!(self, ptr_val.values.len(), slots);
                let _ = unsafe { Box::from_raw(ptr) };
                Value::Unit
            }
            "print" => {
                println!("{}", arg);
                Value::Unit
            }
            "print_callstack" => {
                println!("{}", self.log_callstack());
                Value::Unit
            }
            "comptime_cache_insert" => {
                let (f, arg, ret) = self.to_triple(arg)?;
                let (f, arg, ret) = (
                    self.to_func(f)?,
                    flatten_value(vec![arg]),
                    flatten_value(vec![ret]),
                );

                logln!("INSERT_CACHE {f:?}({arg:?}) => {:?}", ret);
                // debug_assert_eq!(self.current_fn_body().when, ExecTime::Comptime);
                self.program.generics_memo.insert((f, arg), ret);
                Value::Unit
            }
            "comptime_cache_get" => {
                let (f, arg) = self.to_pair(arg)?;
                let key = (self.to_func(f)?, arg);
                logln!("CHECK_CACHE {:?}({:?})", key.0, key.1);
                // debug_assert_eq!(self.current_fn_body().when, ExecTime::Comptime);
                let values = if let Some(prev) = self.program.generics_memo.get(&key) {
                    vec![Value::Bool(true), prev.clone()]
                } else {
                    vec![Value::Bool(false), Value::Unit]
                };
                Value::Tuple {
                    container_type: TypeId::any(),
                    values,
                }
            }
            "Fn" => {
                let (arg, ret) = self.to_pair(arg)?;
                let (arg, ret) = (self.to_type(arg)?, self.to_type(ret)?);
                let ty = self.program.intern_type(TypeInfo::Fn(FnType {
                    param: arg,
                    returns: ret,
                }));
                Value::Type(ty)
            }
            "add" => bin_int!(self, +, arg, Value::I64),
            "sub" => bin_int!(self, -, arg, Value::I64),
            "mul" => bin_int!(self, *, arg, Value::I64),
            "div" => bin_int!(self, /, arg, Value::I64),
            "eq" => bin_int!(self, ==, arg, Value::Bool),
            "ne" => bin_int!(self, !=, arg, Value::Bool),
            "gt" => bin_int!(self, >, arg, Value::Bool),
            "lt" => bin_int!(self, <, arg, Value::Bool),
            "ge" => bin_int!(self, >=, arg, Value::Bool),
            "le" => bin_int!(self, <=, arg, Value::Bool),
            "tuple" => {
                // This will become the `(a, b, c)` syntax.
                // It just gives you a way to express that you want to pass multiple things at once.
                // TODO: this is really inefficient. it boxes them from the stack and then writes them all back again?
                arg
            }
            _ => ice!(self, "Known builtin is not implemented. {}", name),
        };
        Ok(value)
    }
    fn push_callframe(
        &mut self,
        f: FuncId,
        ret: StackRange,
        arg: Value,
        when: ExecTime,
    ) -> Res<'p, ()> {
        let constants = if let Some(prev) = self.call_stack.last() {
            prev.constants.clone()
        } else {
            Rc::new(SharedConstants::default())
        };
        self.ensure_compiled(&constants, f, when)?;
        let func = self.ready[f.0].as_ref();
        assert!(
            self,
            func.is_some(),
            "ICE: ensure_compiled didn't work on {f:?}"
        );
        let constants = func.unwrap().constants.clone().bake();
        // Calling Convention: arguments passed to a function are moved out of your stack.
        let return_slot = self.slot_to_index(ret.first); // TODO: what about tuple returns?
        let stack_base = self.value_stack.len(); // Our stack includes the argument but not the return slot.
        self.push_expanded(arg);
        let debug_name = self.program.funcs[f.0].get_name(self.pool);
        self.call_stack.push(CallFrame {
            stack_base: StackAbsolute(stack_base),
            current_func: f,
            current_ip: 0,
            return_slot,
            return_count: ret.count,
            is_rust_marker: false,
            debug_name,
            when,
            constants,
        });
        let empty = self.current_fn_body().stack_slots; // TODO: does this count tuple args right?
        for _ in 0..empty {
            self.value_stack.push(Value::Poison);
        }
        // TODO: only comptime.
        if self.call_stack.len() > 1000 {
            return Err(self.error(CErr::StackDepthLimit));
        }
        Ok(())
    }

    fn push_expanded(&mut self, arg: Value) {
        match arg {
            Value::Tuple {
                container_type,
                values,
            } => {
                for v in values {
                    self.push_expanded(v);
                }
            }
            _ => self.value_stack.push(arg),
        }
    }

    fn bump_ip(&mut self) {
        let frame = self.call_stack.last_mut().unwrap();
        frame.current_ip += 1;
    }

    #[track_caller]
    fn clone_slot(&self, slot: StackOffset) -> Value {
        let frame = self.call_stack.last().unwrap();
        let value = &self.value_stack[frame.stack_base.0 + slot.0];
        debug_assert_ne!(value, &Value::Poison);
        value.clone()
    }

    #[track_caller]
    fn take_slot(&mut self, slot: StackOffset) -> Value {
        let mut value = replace(self.get_slot_mut(slot), Value::Poison);
        debug_assert_ne!(value, Value::Poison);
        value
    }

    /// If slot ranges over multiple, return them as a tuple.
    #[track_caller]
    fn take_slots(&mut self, slot: StackRange) -> Value {
        if slot.count == 0 {
            Value::Unit
        } else if slot.count == 1 {
            self.take_slot(slot.first)
        } else {
            let mut values = vec![];
            for i in 0..slot.count {
                values.push(self.take_slot(StackOffset(slot.first.0 + i)))
            }

            Value::Tuple {
                container_type: TypeId::any(), // TODO
                values,
            }
        }
    }

    #[track_caller]
    fn get_slot_mut(&mut self, slot: StackOffset) -> &mut Value {
        let frame = self.call_stack.last().unwrap();
        &mut self.value_stack[frame.stack_base.0 + slot.0]
    }

    fn slot_to_index(&mut self, slot: StackOffset) -> StackAbsolute {
        let frame = self.call_stack.last().unwrap();
        StackAbsolute(frame.stack_base.0 + slot.0)
    }

    fn range_to_index(&mut self, slot: StackRange) -> StackAbsoluteRange {
        StackAbsoluteRange {
            first: self.slot_to_index(slot.first),
            count: slot.count,
        }
    }

    pub fn empty_fn(
        &self,
        arg_slots: usize,
        arg_names: Vec<Option<Ident<'p>>>,
        when: ExecTime,
        func: FuncId,
        loc: Span,
    ) -> FnBody<'p> {
        FnBody {
            insts: vec![],
            stack_slots: arg_slots,
            vars: Default::default(),
            arg_names,
            when,
            slot_types: vec![],
            func,
            why: self.log_trace(),
            debug: vec![],
            last_loc: loc,
            constants: Default::default(),
        }
    }

    fn ensure_compiled(
        &mut self,
        constants: &SharedConstants<'p>,
        FuncId(index): FuncId,
        when: ExecTime,
    ) -> Res<'p, ()> {
        if let Some(Some(_)) = self.ready.get(index) {
            return Ok(());
        }
        let state = DebugState::JitToBc(FuncId(index), when);
        self.push_state(&state);
        let func = &self.program.funcs[index];
        let mut constants = constants.clone();
        if !func.local_constants.is_empty() {
            // TODO: pass in comptime known args
            // TODO: do i even need to pass an index? probably just for debugging
            let mut result = self.empty_fn(0, vec![], when, FuncId(index + 10000000), func.loc);
            result.constants.parents.push(constants.clone().bake());
            let new_constants = func.local_constants.clone();

            let state = DebugState::EvalConstants(FuncId(index));
            self.push_state(&state);
            for stmt in new_constants {
                self.compile_stmt(&mut result, &stmt)?;
            }
            self.pop_state(state);
            let func = &mut self.program.funcs[index];
            constants.parents.push(result.constants.bake())
        }
        let func = &self.program.funcs[index];
        while self.ready.len() <= index {
            self.ready.push(None);
        }
        self.log_depth += 1;
        logln!(
            "{} Start JIT: {:?} \n{}",
            "=".repeat(self.log_depth),
            FuncId(index),
            self.program.funcs[index].log(self.pool)
        );
        self.infer_types(&constants, FuncId(index))?;
        let mut func = &self.program.funcs[index];
        let (arg, ret) = func.ty.unwrap();
        let arg_slots = self.program.slot_count(arg);
        logln!("{:?} has arg {} slots", FuncId(index), arg_slots);
        assert_ne!(arg_slots, 0);
        let mut result = self.empty_fn(
            arg_slots,
            func.arg_names.clone(),
            when,
            FuncId(index),
            func.loc,
        );
        result.constants = constants;

        let arg_range = StackRange {
            first: StackOffset(0),
            count: arg_slots,
        };

        let return_value = self.emit_body(&mut result, arg_range, FuncId(index))?;
        let mut func = &self.program.funcs[index];

        result.push(Bc::Ret(return_value));

        logln!("{}", result.log(self.pool));

        // TODO: even the test i thought this would fix it didnt, it cant tell which are const.
        // TODO: why are constants in my vars at all?
        // for v in result.vars.iter() {
        //     assert_eq!(
        //         self,
        //         self.program.vars[v.0 .1].kind,
        //         VarType::Const,
        //         "{}",
        //         v.0.log(self.pool)
        //     );
        // }
        assert!(
            self,
            result.vars.is_empty(),
            "undropped vars {:?}",
            result
                .vars
                .iter()
                .map(|v| (v.0.log(self.pool), v.1))
                .collect::<Vec<_>>()
        );
        self.ready[index] = Some(result);
        logln!(
            "{} Done JIT: {:?} {}",
            "=".repeat(self.log_depth),
            FuncId(index),
            func.synth_name(self.pool)
        );
        self.log_depth -= 1;
        self.pop_state(state);
        Ok(())
    }

    // This is used for
    // - emiting normal functions into a fresh FnBody
    // -
    // -
    fn emit_body(
        &mut self,
        result: &mut FnBody<'p>,
        arg_range: StackRange,
        f: FuncId,
    ) -> Res<'p, StackRange> {
        let func = self.program.funcs[f.0].clone();
        let (arg, ret) = func.ty.unwrap();
        let mut to_drop = vec![];
        // TODO: this should add to result.slot_types?
        //       should use normal reserve_slots instead of whatever this is doing and then get it for free?
        let arguments = func.arg_vars.as_ref().unwrap();
        if arguments.len() == 1 {
            // if there's one name, it refers to the whole tuple.
            let prev = result.vars.insert(arguments[0], (arg_range, arg));
            assert!(self, prev.is_none(), "overwrite arg?");
        } else if arguments.len() == arg_range.count {
            // if they match, each element has its own name.
            for (i, var) in arguments.iter().enumerate() {
                // This always starts at 0 for normal functions, but for inlined closures, the args could be anywhere because we're sharing the parent's stack frame.
                let range = StackRange {
                    first: arg_range.index(i),
                    count: 1,
                };
                let prev = result.vars.insert(*var, (range, TypeId::any()));
                assert!(self, prev.is_none(), "overwrite arg?");
            }
        } else {
            // TODO: pattern match destructuring but for now you just cant refer to the arg.
            to_drop.push(arg_range);
        }
        let return_value = match func.body.as_ref() {
            Some(body) => {
                let (ret, found_ret_ty) = self.compile_expr(result, body)?;
                // We're done with our arguments, get rid of them. Same for other vars.
                // TODO: once non-copy types are supported, this needs to get smarter because we might have moved out of our argument.
                result.push(Bc::DebugMarker("drop_args", func.get_name(self.pool)));
                for var in arguments {
                    let (slot, _) = unwrap!(self, result.vars.remove(var), "lost arg");
                    result.push(Bc::Drop(slot));
                }
                for other in to_drop {
                    result.push(Bc::Drop(other));
                }
                ret
            }
            None => {
                // Functions without a body are always builtins.
                // It's convient to give them a FuncId so you can put them in a variable,
                // but just force inline call.
                self.program.funcs[f.0].annotations.push(Annotation {
                    name: self.pool.intern("inline"),
                    args: None,
                });
                let ret = result.reserve_slots(self.program.slot_count(ret), ret);
                let name = unwrap!(self, func.name, "fn no body needs name");
                result.push(Bc::CallBuiltin {
                    name,
                    ret,
                    arg: arg_range,
                });
                for var in arguments {
                    // TODO: why isn't it always there?
                    result.vars.remove(var);
                    // dont drop. was moved to call
                }
                ret
            }
        };
        Ok(return_value)
    }

    fn emit_capturing_call(
        &mut self,
        result: &mut FnBody<'p>,
        arg: StackRange,
        ret: StackRange,
        f: FuncId,
    ) -> Res<'p, ()> {
        let name = self.program.funcs[f.0].get_name(self.pool);
        result.push(Bc::DebugMarker("start:capturing_call", name));
        self.infer_types(&result.constants, f)?;
        assert!(
            self,
            !self.currently_inlining.contains(&f),
            "Tried to inline recursive function."
        );
        self.currently_inlining.push(f);
        let return_range = self.emit_body(result, arg, f)?;
        result.push(Bc::MoveRange {
            from: return_range,
            to: ret,
        });
        result.push(Bc::DebugMarker("end:capturing_call", name));
        self.currently_inlining.retain(|check| *check != f);
        Ok(())
    }

    fn emit_inline_call(
        &mut self,
        result: &mut FnBody<'p>,
        arg: StackRange,
        ret: StackRange,
        f: FuncId,
    ) -> Res<'p, ()> {
        assert!(
            self,
            !self.currently_inlining.contains(&f),
            "Tried to inline recursive function."
        );
        self.currently_inlining.push(f);
        self.ensure_compiled(&result.constants, f, result.when)?;
        let name = self.program.funcs[f.0].get_name(self.pool);
        result.push(Bc::DebugMarker("start:inline_call", name));
        // This move ensures they end up at the base of the renumbered stack. TODO: it can't be capturing
        let arg_slots = result.reserve_slots(arg.count, TypeId::any()); // These are included in the new function's stack
        result.push(Bc::MoveRange {
            from: arg,
            to: arg_slots,
        });
        let (stack_offset, ip_offset) = (result.stack_slots - arg_slots.count, result.insts.len());
        let func = self.ready[f.0].as_ref().unwrap();
        // TODO: check for recusion somewhere.
        // TODO: put constants somewhere so dont have to clone them each time a function is inlined.
        result.stack_slots += func.stack_slots;
        result.slot_types.extend(func.slot_types.iter());
        let mut has_returned = false; // TODO: remove
        for (i, mut inst) in func.insts.iter().cloned().enumerate() {
            assert!(self, !has_returned);
            inst.renumber(stack_offset, ip_offset);
            if let Bc::Ret(return_value) = inst {
                // TODO: what if there are multiple returns?
                has_returned = true;
                result.insts.push(Bc::MoveRange {
                    from: return_value,
                    to: ret,
                });
                #[cfg(feature = "some_log")]
                {
                    result.debug.push(result.debug[i].clone());
                }
            } else {
                result.insts.push(inst);
                #[cfg(feature = "some_log")]
                {
                    result.debug.push(result.debug[i].clone());
                }
            }
        }
        assert!(
            self,
            has_returned,
            "inline function had no ret instruction. \n{}",
            func.log(self.pool)
        );
        result.push(Bc::DebugMarker("end:inline_call", name));
        self.currently_inlining.retain(|check| *check != f);
        Ok(())
    }

    fn compile_stmt(&mut self, result: &mut FnBody<'p>, stmt: &Stmt<'p>) -> Res<'p, ()> {
        match stmt {
            Stmt::Eval(expr) => {
                let (value, _) = self.compile_expr(result, expr)?;
                result.push(Bc::Drop(value));
            }
            Stmt::DeclVar {
                name,
                ty,
                value,
                dropping,
                kind,
            } => {
                let ty = invert(
                    ty.clone()
                        .map(|ty| self.cached_eval_expr(&result.constants, ty)),
                )?;
                if *kind == VarType::Const {
                    let value = match value {
                        Some(value) => self.cached_eval_expr(&result.constants, value.clone())?,
                        None => {
                            let name = self.pool.get(name.0);
                            unwrap!(
                                self,
                                self.builtin_constant(name),
                                "uninit (non-blessed) const: {:?}",
                                name
                            )
                            .0
                        }
                    };
                    let ty = ty
                        .map(|ty| self.to_type(ty).unwrap())
                        .unwrap_or_else(|| self.program.type_of(&value));
                    result.constants.insert(*name, (value, ty));
                    return Ok(());
                }

                let value = invert(value.as_ref().map(|expr| self.compile_expr(result, expr)))?;
                let ty_slots = if let Some(ty) = ty {
                    let ty = self.to_type(ty)?;
                    Some(self.program.slot_count(ty))
                } else {
                    None
                };

                let value = match (value, ty_slots) {
                    (None, Some(slots)) => {
                        (result.reserve_slots(slots, TypeId::any()), TypeId::any())
                    }
                    (Some(value), None) => value,
                    (Some((value, ty)), Some(slots)) => {
                        if slots == value.count {
                            (value, ty)
                        } else {
                            assert_eq!(self, value.count, 1);
                            let expanded = result.reserve_slots(slots, TypeId::any());
                            result.push(Bc::ExpandTuple {
                                from: value.first,
                                to: expanded,
                            });
                            (expanded, ty)
                        }
                    }
                    // TODO: make this an error. dont guess.
                    (None, None) => (result.reserve_slots(1, TypeId::any()), TypeId::any()),
                };
                let prev = result.vars.insert(*name, value);
                assert!(self, prev.is_none(), "shadow is still new var");

                // TODO: what if shadow is const? that would be more consistant if did it like rust.
                if let Some(dropping) = dropping {
                    // Maybe should be like rust and dont call drop on the shadowed thing until the end of scope.
                    // It would be consistant and it mean you can reference its data if you do a chain of transforming something with the same name.
                    // But need to change my debugging check that everything was dropped.
                    // Actually if i did that just put them in the block's list instead of carefully taking them out which i did because i thought i wanted to egarly drop.
                    let (slot, _) = unwrap!(self, result.vars.remove(dropping), "missing shadow");
                    result.push(Bc::Drop(slot));
                }
            }
            Stmt::SetVar(var, expr) => {
                let kind = self.program.vars[var.1].kind;
                assert_eq!(
                    self, kind,
                    VarType::Var,
                    "Only 'var' can be reassigned (not let/const). {:?}", stmt
                );
                let (value, new_ty) = self.compile_expr(result, expr)?;
                let slot = result.vars.get(var);
                let (slot, oldty) = *unwrap!(
                    self,
                    slot,
                    "SetVar: var must be declared: {}",
                    var.log(self.pool)
                );
                assert_eq!(self, new_ty, oldty);
                result.push(Bc::Drop(slot));
                assert_eq!(self, value.count, slot.count);
                for i in 0..value.count {
                    result.push(Bc::Move {
                        from: value.index(i),
                        to: slot.index(i),
                    });
                }
            }
            Stmt::SetNamed(name, _) => err!(self, CErr::UndeclaredIdent(*name)),
            Stmt::DeclNamed { .. } | Stmt::SetNamed(_, _) => {
                ice!(self, "Scope resolution failed {}", stmt.log(self.pool))
            }
            Stmt::Noop => {}
            Stmt::DeclFunc(func) => {
                let func_id = self.program.add_func(func.clone());
            }
            s => todo!("Compile Stmt {:?}", s),
        }
        Ok(())
    }

    fn return_stack_slots(&mut self, f: FuncId) -> usize {
        // You must self.infer_types(f); before calling this
        let func = &self.program.funcs[f.0];
        let (_, ret) = func.ty.unwrap();
        self.program.slot_count(ret)
    }

    // TODO: make the indices always work out so you could just do it with a normal stack machine.
    //       and just use the slow linear types for debugging.
    fn compile_expr(
        &mut self,
        result: &mut FnBody<'p>,
        expr: &FatExpr<'p>,
    ) -> Res<'p, (StackRange, TypeId)> {
        result.last_loc = expr.loc;

        Ok(match expr.deref() {
            Expr::Closure(func) => {
                let id = self.program.add_func(*func.clone());
                self.ensure_compiled(&result.constants, id, result.when)?;
                let ty = self.program.func_type(id);
                (result.load_constant(Value::GetFn(id), ty), ty)
            }
            Expr::Call(f, arg) => {
                if let Expr::GetNamed(i) = f.as_ref().deref() {
                    if let Some(f) = self.lookup_unique_func(*i) {
                        let (mut arg, arg_ty_found) = self.compile_expr(result, arg)?;
                        // TODO: this fixes inline calls when no arguments. do better. support zero-sized types in general.
                        if arg.count == 0 {
                            arg = result.load_constant(
                                Value::Unit,
                                self.program.intern_type(TypeInfo::Unit),
                            );
                        }
                        // Note: f might not be compiled yet, we'll find out the first time we try to call it.
                        // But, we do need to know how many values it returns. If there's no type annotation, this might end up compiling the function to figure it out.
                        self.infer_types(&result.constants, f)?;
                        let func = &self.program.funcs[f.0];
                        let (arg_ty_expected, ret_ty_expected) = func.ty.unwrap();
                        // TODO: some huristic based on how many times called and how big the body is.
                        // TODO: pre-intern all these constants so its not a hash lookup everytime
                        let force_inline = func.has_tag(self.pool, "inline");
                        let deny_inline = func.has_tag(self.pool, "noinline");
                        assert!(
                            self,
                            !(force_inline && deny_inline),
                            "{f:?} is both @inline and @noinline"
                        );
                        let will_inline = force_inline;
                        let returns_type = self.program.is_type(ret_ty_expected, TypeInfo::Type); // TODO: even if only one val is a type

                        let cache_arg = if returns_type {
                            assert_eq!(
                                self, result.when,
                                ExecTime::Comptime,
                                "Cannot call function returning type at runtime."
                            );
                            let cache_arg = result.reserve_slots(1, arg_ty_expected);
                            result.push(Bc::CloneCreateTuple {
                                values: arg,
                                target: cache_arg.first,
                            });
                            Some(cache_arg)
                        } else {
                            None
                        };
                        let ret = result.reserve_slots(self.return_stack_slots(f), ret_ty_expected);
                        let func = &self.program.funcs[f.0];
                        if returns_type {
                            result.push(Bc::CallDirectMaybeCached { f, ret, arg });
                        } else if !func.capture_vars.is_empty() {
                            // TODO: check that you're calling from the same place as the definition.
                            assert!(self, !deny_inline, "capturing calls are always inlined.");
                            self.emit_capturing_call(result, arg, ret, f)?;
                        } else if will_inline {
                            self.emit_inline_call(result, arg, ret, f)?;
                        } else {
                            result.push(Bc::CallDirect { f, ret, arg });
                        }
                        if let Some(cache_arg) = cache_arg {
                            let f_arg_ret = result.reserve_slots(3, TypeId::unit());
                            result.push(Bc::LoadConstant {
                                slot: f_arg_ret.index(0),
                                value: Value::GetFn(f),
                            });
                            result.push(Bc::Move {
                                from: cache_arg.first,
                                to: f_arg_ret.index(1),
                            });
                            result.push(Bc::CloneCreateTuple {
                                values: ret,
                                target: f_arg_ret.index(2),
                            });
                            let nothing = result.reserve_slots(1, TypeId::unit());
                            // TODO: put next to for call.
                            result.push(Bc::CallBuiltin {
                                name: self.pool.intern("comptime_cache_insert"),
                                ret: nothing,
                                arg: f_arg_ret,
                            });
                            result.push(Bc::Drop(nothing));
                        }
                        return Ok((ret, ret_ty_expected));
                    } else if "if" == self.pool.get(*i) {
                        // TODO: treat this as a normal builtin but need to support general closures?
                        return self.emit_call_if(result, arg);
                    }
                    // else: fallthrough
                }

                let (arg, arg_ty_found) = self.compile_expr(result, arg)?;
                logln!("dynamic {}", f.log(self.pool));
                let (f, func_ty) = self.compile_expr(result, f)?;
                let ret_ty = if let TypeInfo::Fn(ty) = &self.program.types[func_ty.0] {
                    ty.returns
                } else {
                    // TODO
                    TypeId::any()
                };
                assert_eq!(self, f.count, 1);
                // TODO: not all function ptrs have 1 return value
                let ret = result.reserve_slots(1, TypeId::any());
                result.push(Bc::CallDynamic {
                    f: f.first,
                    ret,
                    arg,
                });
                (ret, ret_ty)
            }
            Expr::Block {
                body,
                result: value,
                locals,
            } => {
                for stmt in body {
                    self.compile_stmt(result, stmt)?;
                }
                let ret = self.compile_expr(result, value)?;

                for local in locals.as_ref().expect("resolve failed") {
                    if let Some((slot, ty)) = result.vars.remove(local) {
                        result.push(Bc::Drop(slot));
                    } else if VarType::Const == self.program.vars[local.1].kind {
                        assert!(
                            self,
                            result.vars.remove(local).is_none(),
                            "constants are not locals"
                        );
                    } else {
                        ice!(self, "Missing local {local:?}")
                    }
                }

                ret
            }
            Expr::ArrayLiteral(_) => todo!(),
            Expr::Tuple(values) => {
                let values: Res<'p, Vec<_>> = values
                    .iter()
                    .map(|v| self.compile_expr(result, v))
                    .collect();
                let values = values?;
                let required_slots: usize = values.iter().map(|(range, _)| range.count).sum();
                let types: Vec<_> = values.iter().map(|(_, ty)| *ty).collect();
                let ty = self.program.intern_type(TypeInfo::Tuple(types));

                let ret = result.reserve_slots(required_slots, ty);
                // TODO: they might already be consecutive
                let base = ret.first.0;
                let mut count = 0;
                for (v, ty) in values {
                    for i in 0..v.count {
                        result.push(Bc::Move {
                            from: StackOffset(v.first.0 + i),
                            to: StackOffset(base + count),
                        });
                        count += 1;
                    }
                }
                assert_eq!(self, count, ret.count);
                (ret, ty)
            }
            Expr::RefType(_) => todo!(),
            Expr::GetVar(var) => {
                if let Some((from, ty)) = result.vars.get(var).cloned() {
                    let to = result.reserve_slots(from.count, ty);
                    if from.count == 1 {
                        result.push(Bc::Clone {
                            from: from.first,
                            to: to.first,
                        });
                    } else {
                        result.push(Bc::CloneRange { from, to });
                    }
                    (to, ty)
                } else if let Some((value, ty)) = result.constants.get(var) {
                    let slot = result.reserve_slots(self.program.slot_count(ty), ty);
                    result.push(Bc::LoadConstant {
                        slot: slot.first,
                        value: value.clone(),
                    });
                    (slot, ty)
                } else {
                    println!("VARS: {:?}", result.vars);
                    println!("GLOBALS: {:?}", result.constants);
                    ice!(
                        self,
                        "Missing resolved variable {:?} '{}' at {:?}",
                        var,
                        self.pool.get(var.0),
                        expr.loc
                    )
                }
            }
            Expr::GetNamed(i) => {
                if let Some(func) = self.program.declarations.get(i) {
                    assert_eq!(self, func.len(), 1, "ambigous function reference");
                    let func = func[0];
                    self.ensure_compiled(&result.constants, func, ExecTime::Comptime)?;
                    let ty = self.program.func_type(func);
                    (result.load_constant(Value::GetFn(func), ty), ty)
                } else {
                    ice!(self, "Scope resolution failed {}", expr.log(self.pool));
                }
            }
            Expr::EnumLiteral(_) => todo!(),
            Expr::StructLiteral(fields) => {
                let values: Res<'p, Vec<_>> = fields
                    .iter()
                    .map(|field| self.cached_eval_expr(&result.constants, field.ty.clone()))
                    .collect();
                let fields: Vec<_> = values?
                    .into_iter()
                    .zip(fields.iter())
                    .map(|(v, f)| Value::Tuple {
                        container_type: TypeId::any(),
                        values: vec![Value::Symbol(f.name.0), v],
                    })
                    .collect();

                let map = Value::Tuple {
                    container_type: TypeId::any(),
                    values: fields,
                };
                let map = result.load_constant(map, TypeId::any()); // TODO
                (map, TypeId::any())
            }
            Expr::Value(value) => {
                let ty = self.program.type_of(value);
                (result.load_constant(value.clone(), ty), ty)
            }
            Expr::SuffixMacro(macro_name, arg) => {
                let name = self.pool.get(*macro_name);
                match name {
                    // TODO: make `let` deeply immutable so only const addr
                    // Note: arg is not evalutated
                    "addr" => match arg.deref().deref() {
                        Expr::GetVar(var) => {
                            if let Some((stack_slot, value_ty)) = result.vars.get(var).cloned() {
                                let ptr_ty = self.program.ptr_type(value_ty);
                                let addr_slot = result.reserve_slots(1, ptr_ty);
                                result.push(Bc::AbsoluteStackAddr {
                                    of: stack_slot,
                                    to: addr_slot.first,
                                });
                                (addr_slot, ptr_ty)
                            } else if result.constants.get(var).is_some() {
                                err!(self, "Took address of constant {}", var.log(self.pool))
                            } else {
                                ice!(self, "Missing var {} (in !addr)", var.log(self.pool))
                            }
                        }
                        &Expr::GetNamed(i) => return Err(self.error(CErr::UndeclaredIdent(i))),
                        _ => return Err(self.error(CErr::AddrRvalue(*arg.clone()))),
                    },
                    "type" => {
                        // Note: this does not evaluate the expression.
                        // TODO: warning if it has side effects.
                        let ty = self.type_of(result, arg)?;
                        let tyty = self.program.intern_type(TypeInfo::Type);
                        (result.load_constant(Value::Type(ty), tyty), tyty)
                    }
                    "assert_compile_error" => {
                        // TODO: this can still have side-effects on the vm state tho :(
                        let state = self.mark_state(result.clone());
                        let res = self.compile_expr(result, arg);
                        assert!(self, res.is_err());
                        mem::forget(res); // TODO: dont do this. but for now i like having my drop impl that prints it incase i forget  ot unwrap
                        *result = self.restore_state(state);
                        let ty = self.program.intern_type(TypeInfo::Unit);

                        (result.load_constant(Value::Unit, ty), ty)
                    }
                    _ => return Err(self.error(CErr::UndeclaredIdent(*macro_name))),
                }
            }
            Expr::FieldAccess(_, _) => todo!(),
        })
    }

    fn mark_state(&self, result: FnBody<'p>) -> StackHeights<'p> {
        StackHeights {
            value_stack: self.value_stack.len(),
            call_stack: self.call_stack.len(),
            debug_trace: self.debug_trace.len(),
            result,
        }
    }

    fn restore_state(&mut self, state: StackHeights<'p>) -> FnBody<'p> {
        drops(&mut self.value_stack, state.value_stack);
        drops(&mut self.call_stack, state.call_stack);
        drops(&mut self.debug_trace, state.debug_trace);
        state.result
    }

    fn resolve_function(&self, result: &FnBody<'p>, expr: &FatExpr<'p>) -> Option<FuncId> {
        match expr.deref() {
            Expr::GetNamed(i) => self.lookup_unique_func(*i),
            _ => None,
        }
    }

    fn type_of(&mut self, result: &FnBody<'p>, expr: &FatExpr<'p>) -> Res<'p, TypeId> {
        if let Some(ty) = expr.ty {
            return Ok(ty);
        }
        Ok(match expr.deref() {
            Expr::Value(v) => self.program.type_of(v),
            Expr::Call(f, arg) => {
                let fid = if let Some(f) = self.resolve_function(result, f) {
                    f
                } else {
                    ice!(self, "typecheck failed to resolve function expr {f:?}")
                };
                self.ensure_compiled(&result.constants, fid, ExecTime::Comptime)?;
                let (_, ret) = self.program.funcs[fid.0].ty.unwrap();
                ret
            }
            Expr::Block { result: e, .. } => self.type_of(result, e)?,
            Expr::ArrayLiteral(_) => todo!(),
            Expr::Tuple(_) => todo!(),
            Expr::RefType(_) => todo!(),
            Expr::EnumLiteral(_) => todo!(),
            Expr::StructLiteral(_) => todo!(),
            Expr::Closure(_) => todo!(),
            Expr::SuffixMacro(macro_name, arg) => {
                let name = self.pool.get(*macro_name);
                match name {
                    // TODO: make `let` deeply immutable so only const addr
                    "addr" => match arg.deref().deref() {
                        Expr::GetVar(var) => {
                            let (_, value_ty) = *result
                                .vars
                                .get(var)
                                .expect("Missing resolved var (TODO: addr of const?)");
                            self.program.intern_type(TypeInfo::Ptr(value_ty))
                        }
                        &Expr::GetNamed(i) => {
                            logln!(
                                "UNDECLARED IDENT: {} (in SuffixMacro::addr)",
                                self.pool.get(i)
                            );
                            return Err(self.error(CErr::UndeclaredIdent(i)));
                        }
                        _ => return Err(self.error(CErr::AddrRvalue(*arg.clone()))),
                    },
                    "type" => self.program.intern_type(TypeInfo::Type),
                    _ => return Err(self.error(CErr::UndeclaredIdent(*macro_name))),
                }
            }
            Expr::GetVar(var) => {
                if let Some((_, ty)) = result.vars.get(var).cloned() {
                    ty
                } else if let Some((_, ty)) = result.constants.get(var) {
                    ty
                } else {
                    ice!(self, "type check missing var {var:?}")
                }
            }
            Expr::GetNamed(_) => todo!(),
            Expr::FieldAccess(_, _) => todo!(),
        })
    }

    fn builtin_constant(&mut self, name: &str) -> Option<(Value, TypeId)> {
        use TypeInfo::*;
        let ty = match name {
            "i64" => Some(I64),
            "f64" => Some(F64),
            "Type" => Some(Type),
            "bool" => Some(Bool),
            "Any" => Some(Any),
            _ => None,
        };
        if let Some(ty) = ty {
            let ty = self.program.intern_type(ty);
            let tyty = self.program.intern_type(TypeInfo::Type);
            return Some((Value::Type(ty), tyty));
        }

        Some(match name {
            "true" => (Value::Bool(true), self.program.intern_type(TypeInfo::Bool)),
            "false" => (Value::Bool(false), self.program.intern_type(TypeInfo::Bool)),

            _ => return None,
        })
    }

    fn next_inst(&self) -> &Bc<'p> {
        let frame = self.call_stack.last().unwrap();
        let body = self
            .ready
            .get(frame.current_func.0)
            .unwrap()
            .as_ref()
            .unwrap();
        body.insts.get(frame.current_ip).unwrap()
    }

    fn current_fn_body(&self) -> &FnBody {
        let frame = self.call_stack.last().unwrap();
        let body = self.ready.get(frame.current_func.0).unwrap();
        body.as_ref().expect("jit current function")
    }

    // Resolve the lazy types for Arg and Ret
    fn infer_types(&mut self, constants: &SharedConstants<'p>, func: FuncId) -> Res<'p, ()> {
        match self.program.funcs[func.0].ty.clone() {
            LazyFnType::Pending { arg, ret } => {
                let state = DebugState::ResolveFnType(func, arg.clone(), ret.clone());
                self.push_state(&state);
                logln!("RESOLVE: Arg of {func:?}");
                let arg = match arg {
                    LazyType::Infer => TypeId::any(),
                    LazyType::PendingEval(e) => {
                        let value = self.cached_eval_expr(constants, e.clone())?;
                        self.to_type(value)?
                    }
                    LazyType::Finished(id) => id, // easy
                };
                // TODO: deal with comptime args that are allowed to be used in return type
                // TODO: copy-n-paste
                logln!("RESOLVE: Ret of {func:?}");
                let ret = match ret {
                    LazyType::Infer => todo!(),
                    LazyType::PendingEval(e) => {
                        let value = self.cached_eval_expr(constants, e.clone())?;
                        self.to_type(value)?
                    }
                    LazyType::Finished(id) => id, // easy
                };
                self.program.funcs[func.0].ty = LazyFnType::Finished(arg, ret);
                self.pop_state(state);
            }
            LazyFnType::Finished(_, _) => {} // easy
        }
        Ok(())
    }

    fn unit_to_type(&mut self) -> LazyFnType<'p> {
        LazyFnType::Finished(
            self.program.intern_type(TypeInfo::Unit),
            self.program.intern_type(TypeInfo::Type),
        )
    }

    fn cached_eval_expr(
        &mut self,
        constants: &SharedConstants<'p>,
        e: FatExpr<'p>,
    ) -> Res<'p, Value> {
        match e.deref() {
            Expr::Value(value) => return Ok(value.clone()),
            Expr::GetVar(var) => {
                // fast path for builtin type identifiers
                if let Some((value, _)) = constants.get(var) {
                    debug_assert_ne!(value, Value::Poison);
                    return Ok(value);
                }
                // fallthrough
            }
            Expr::Call(f, arg) => {
                // TODO: fast path for checking the generics cache
            }
            _ => {} // fallthrough
        }
        let state = DebugState::ComputeCached(e.clone());
        self.push_state(&state);
        let name = format!("@eval_{}@", self.anon_fn_counter);
        let fake_func: Func<'p> = Func {
            name: Some(self.pool.intern(&name)),
            ty: self.unit_to_type(),
            body: Some(e.clone()),
            arg_names: vec![],
            annotations: vec![],
            arg_vars: Some(vec![]),
            capture_vars: vec![],
            local_constants: Default::default(),
            loc: e.loc,
        };
        self.anon_fn_counter += 1;
        let func_id = self.program.add_func(fake_func);
        logln!(
            "Made anon: {func_id:?} = {}",
            self.program.funcs[func_id.0].log(self.pool)
        );
        let result = self.run(Some(constants), func_id, Value::Unit, ExecTime::Comptime)?;
        logln!(
            "COMPUTED: {} -> {:?} under {}",
            e.log(self.pool),
            result,
            self.program.funcs[func_id.0].log(self.pool)
        );
        self.pop_state(state);
        Ok(result)
    }

    #[track_caller]
    fn to_type(&mut self, value: Value) -> Res<'p, TypeId> {
        if let Value::Type(id) = value {
            Ok(id)
        } else if let Value::Tuple { values, .. } = value {
            let ty = if !values.is_empty() {
                let values: Res<'_, Vec<_>> = values.into_iter().map(|v| self.to_type(v)).collect();
                TypeInfo::Tuple(values?)
            } else {
                TypeInfo::Unit
            };
            Ok(self.program.intern_type(ty))
        } else if let Value::Unit = value {
            // This lets you use the literal `()` as a type (i dont parse it as a tuple because reasons).
            Ok(self.program.intern_type(TypeInfo::Unit))
        } else {
            Err(self.error(CErr::TypeError("Type", value)))
        }
    }

    #[track_caller]
    fn to_bool(&self, value: Value) -> Res<'p, bool> {
        if let Value::Bool(v) = value {
            Ok(v)
        } else {
            Err(self.error(CErr::TypeError("bool", value)))
        }
    }

    #[track_caller]
    fn to_seq(&self, value: Value) -> Res<'p, Vec<Value>> {
        match value {
            Value::Tuple {
                container_type,
                values,
            }
            | Value::Array {
                container_type,
                values,
            } => Ok(values),
            Value::Slice(values) => todo!(),
            _ => Err(self.error(CErr::TypeError("AnyTuple | AnyArray", value))),
        }
    }

    #[track_caller]
    fn to_triple(&self, value: Value) -> Res<'p, (Value, Value, Value)> {
        let values = self.to_seq(value)?;
        assert_eq!(self, values.len(), 3, "{:?}", values);
        Ok((values[0].clone(), values[1].clone(), values[2].clone()))
    }

    #[track_caller]
    fn to_func(&self, value: Value) -> Res<'p, FuncId> {
        if let Value::GetFn(id) = value {
            Ok(id)
        } else {
            Err(self.error(CErr::TypeError("AnyFunc", value)))
        }
    }

    #[track_caller]
    fn to_pair(&self, value: Value) -> Res<'p, (Value, Value)> {
        let values = self.to_seq(value)?;
        assert_eq!(self, values.len(), 2);
        Ok((values[0].clone(), values[1].clone()))
    }

    #[track_caller]
    fn split_to_pair(&self, value: Value) -> Res<'p, (Value, Value)> {
        let values = to_flat_seq(value);

        // TODO: sad cloning
        if values.len() == 2 {
            return Ok((values[0].clone(), values[1].clone()));
        }

        assert_eq!(self, values.len() % 2, 0);
        let first = (values[0..values.len() / 2]).to_vec();
        let second = (values[values.len() / 2..]).to_vec();

        Ok((
            Value::Tuple {
                container_type: TypeId::any(),
                values: first,
            },
            Value::Tuple {
                container_type: TypeId::any(),
                values: second,
            },
        ))
    }

    #[track_caller]
    fn to_stack_addr(&self, value: Value) -> Res<'p, StackAbsoluteRange> {
        if let Value::InterpAbsStackAddr(r) = value {
            Ok(r)
        } else {
            Err(self.error(CErr::TypeError("StackAddr", value)))
        }
    }

    #[track_caller]
    fn to_int(&self, value: Value) -> Res<'p, i64> {
        if let Value::I64(r) = value {
            Ok(r)
        } else {
            Err(self.error(CErr::TypeError("i64", value)))
        }
    }

    fn to_heap_ptr(&self, value: Value) -> Res<'p, (*mut InterpBox, usize, usize)> {
        if let Value::Heap {
            value,
            first,
            count,
        } = value
        {
            Ok((value, first, count))
        } else {
            Err(self.error(CErr::TypeError("Heap", value)))
        }
    }

    // TODO: macros for each builtin arg type cause this sucks.
    fn load_int_pair(&self, v: Value) -> Res<'p, (i64, i64)> {
        match v {
            Value::Tuple {
                container_type,
                mut values,
            } => {
                assert_eq!(self, values.len(), 2, "load_int_pair wrong arity");
                let a = replace(&mut values[0], Value::Poison);
                let b = replace(&mut values[1], Value::Poison);
                Ok((self.load_int(a)?, self.load_int(b)?))
            }
            v => err!(self, "load_int_pair {:?}", v),
        }
    }

    fn load_int(&self, v: Value) -> Res<'p, i64> {
        match v {
            Value::I64(i) => Ok(i),
            v => err!(self, "load_int {:?}", v),
        }
    }

    fn emit_call_if(
        &mut self,
        result: &mut FnBody<'p>,
        arg: &FatExpr<'p>,
    ) -> Result<(StackRange, TypeId), CompileError<'p>> {
        let ((cond, cond_ty), if_true, if_false) = if let Expr::Tuple(parts) = arg.deref() {
            let cond = self.compile_expr(result, &parts[0])?;
            let if_true = if let Expr::Closure(func) = parts[1].deref() {
                self.program.add_func(*func.clone())
            } else {
                ice!(self, "if args must be tuple");
            };
            let if_false = if let Expr::Closure(func) = parts[2].deref() {
                self.program.add_func(*func.clone())
            } else {
                ice!(self, "if args must be tuple");
            };
            (cond, if_true, if_false)
        } else {
            ice!(self, "if args must be tuple");
        };

        let arg = result.load_constant(Value::Unit, self.program.intern_type(TypeInfo::Unit));
        // TODO: if returning tuples
        let ret = result.reserve_slots(1, TypeId::any());

        let name = self.pool.intern("builtin:if");
        let branch_ip = result.push(Bc::DebugMarker("patch", name));
        let true_ip = result.insts.len();
        self.emit_capturing_call(result, arg, ret, if_true)?;
        let jump_over_false = result.push(Bc::DebugMarker("patch", name));
        let false_ip = result.insts.len();
        self.emit_capturing_call(result, arg, ret, if_false)?;

        result.insts[branch_ip] = Bc::JumpIf {
            // TODO: change to conditional so dont have to store the true_ip
            cond: cond.first,
            true_ip,
            false_ip,
        };
        result.insts[jump_over_false] = Bc::Goto {
            ip: result.insts.len(),
        };
        // TODO: check branches are same ty[e]
        Ok((ret, TypeId::any()))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ExecTime {
    Comptime,
    Runtime,
}

impl FnBody<'_> {
    fn reserve_slots(&mut self, count: usize, ty: TypeId) -> StackRange {
        if ty.is_any() {
            for _ in 0..count {
                self.slot_types.push(TypeId::any());
            }
        } else if count == 1 {
            self.slot_types.push(ty);
        } else {
            // TODO: expand from tuple
            for _ in 0..count {
                self.slot_types.push(TypeId::any());
            }
        }
        let range = StackRange {
            first: StackOffset(self.stack_slots),
            count,
        };
        self.stack_slots += count;
        range
    }

    fn load_constant(&mut self, value: Value, ty: TypeId) -> StackRange {
        let to = self.reserve_slots(1, ty);
        self.push(Bc::LoadConstant {
            slot: to.first,
            value,
        });
        to
    }
}

/// https://users.rust-lang.org/t/convenience-method-for-flipping-option-result-to-result-option/13695
fn invert<T, E>(x: Option<Result<T, E>>) -> Result<Option<T>, E> {
    x.map_or(Ok(None), |v| v.map(Some))
}

fn to_flat_seq(value: Value) -> Vec<Value> {
    match value {
        Value::Tuple {
            container_type,
            values,
        }
        | Value::Array {
            container_type,
            values,
        } => values.into_iter().flat_map(to_flat_seq).collect(),
        e => vec![e],
    }
}

impl<'p> Bc<'p> {
    // Used for inlining
    fn renumber(&mut self, stack_offset: usize, ip_offset: usize) {
        match self {
            Bc::CallDynamic { f, ret, arg } => {
                f.0 += stack_offset;
                ret.first.0 += stack_offset;
                arg.first.0 += stack_offset;
            }
            Bc::CallDirectMaybeCached { f, ret, arg } | Bc::CallDirect { f, ret, arg } => {
                ret.first.0 += stack_offset;
                arg.first.0 += stack_offset;
            }
            Bc::CallBuiltin { name, ret, arg } => {
                ret.first.0 += stack_offset;
                arg.first.0 += stack_offset;
            }
            Bc::LoadConstant { slot, value } => slot.0 += stack_offset,
            Bc::JumpIf {
                cond,
                true_ip,
                false_ip,
            } => {
                cond.0 += stack_offset;
                *true_ip += ip_offset;
                *false_ip += ip_offset;
            }
            Bc::Goto { ip } => *ip += ip_offset,
            Bc::CloneCreateTuple { values, target } | Bc::MoveCreateTuple { values, target } => {
                target.0 += stack_offset;
            }
            Bc::Drop(arg) | Bc::Ret(arg) => {
                arg.first.0 += stack_offset;
            }

            Bc::Move { from, to } | Bc::Clone { from, to } => {
                from.0 += stack_offset;
                to.0 += stack_offset;
            }
            Bc::CloneRange { from, to } | Bc::MoveRange { from, to } => {
                from.first.0 += stack_offset;
                to.first.0 += stack_offset;
            }
            Bc::ExpandTuple { from, to } => {
                from.0 += stack_offset;
                to.first.0 += stack_offset;
            }
            Bc::AbsoluteStackAddr { of, to } => {
                of.first.0 += stack_offset;
                to.0 += stack_offset;
            }
            Bc::DebugLine(_) | Bc::DebugMarker(_, _) => {}
        }
    }
}

fn flatten_value(values: Vec<Value>) -> Value {
    if values.len() == 1 {
        return values.into_iter().next().unwrap();
    }

    // TODO: flatten inner tuples
    Value::Tuple {
        container_type: TypeId::any(),
        values: values.into_iter().flat_map(expand_value).collect(),
    }
}

fn expand_value(value: Value) -> Vec<Value> {
    if let Value::Tuple { values, .. } = value {
        values.into_iter().flat_map(expand_value).collect()
    } else {
        vec![value]
    }
}

fn drops<T>(vec: &mut Vec<T>, new_len: usize) {
    for _ in 0..(vec.len() - new_len) {
        vec.pop();
    }
}

// TODO: This must be super fucking slow
#[derive(Debug, Clone, Default)]
pub struct SharedConstants<'p> {
    parents: Vec<Rc<SharedConstants<'p>>>,
    local: HashMap<Var<'p>, (Value, TypeId)>,
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

    pub fn insert(&mut self, k: Var<'p>, v: (Value, TypeId)) {
        self.local.insert(k, v);
    }

    pub fn bake(self) -> Rc<Self> {
        Rc::new(self)
    }
}

// TODO
pub trait InterpSend<'p> {
    fn get_type(interp: &mut Interp<'_, 'p>) -> TypeId;
    fn serialize(self) -> Value;
    fn deserialize(value: Value) -> Self;
}

// #[derive(Debug, InterpSend)]
// struct HelloWorld {
//     a: i64,
// }
