#![allow(clippy::wrong_self_convention)]
#![deny(unused_must_use)]
use std::marker::PhantomData;
use std::mem;
use std::rc::Rc;
use std::{collections::HashMap, mem::replace, ops::Deref, panic::Location};

use codemap::Span;

use crate::ast::{Annotation, FatStmt, Field, VarType};
use crate::logging::PoolLog;
use crate::{
    ast::{
        Expr, FatExpr, FnType, Func, FuncId, LazyFnType, LazyType, Program, Stmt, TypeId, TypeInfo,
        Var,
    },
    pool::{Ident, StringPool},
};

use crate::logging::{assert, assert_eq, bin_int, err, ice, logln, unwrap};

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
    fn offset(&self, offset: usize) -> StackOffset {
        debug_assert!(offset < self.count);
        StackOffset(self.first.0 + offset)
    }

    #[track_caller]
    fn single(&self) -> StackOffset {
        debug_assert_eq!(self.count, 1);
        self.first
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

#[derive(Debug, Clone)]
pub struct CallFrame<'p> {
    pub stack_base: StackAbsolute,
    current_func: FuncId,
    current_ip: usize,
    return_slot: StackAbsoluteRange,
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
    pub when: ExecTime,
    pub slot_types: Vec<TypeId>,
    pub func: FuncId,
    pub why: String,
    pub last_loc: Span,
    pub constants: SharedConstants<'p>,
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
    log_depth: usize,
    // Since there's a kinda confusing recursive structure for interpreting a program, it feels useful to keep track of where you are.
    pub debug_trace: Vec<DebugState<'p>>,
    pub anon_fn_counter: usize,
    pub assertion_count: usize,
    currently_inlining: Vec<FuncId>,
    pub prelude_length: usize,
    last_loc: Option<Span>,
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
    pub internal_loc: &'static Location<'static>,
    pub loc: Option<Span>,
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
    TypeCheck(TypeId, TypeId, &'static str),
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
            log_depth: 0,
            debug_trace: vec![],
            anon_fn_counter: 0,
            assertion_count: 0,
            currently_inlining: vec![],
            prelude_length: 0,
            last_loc: None,
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
            internal_loc: Location::caller(),
            loc: self.last_loc,
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
            return_slot: StackAbsoluteRange {
                first: StackAbsolute(0),
                count: 0,
            },
            is_rust_marker: true, // used for exiting the run loop. this is the only place we set it true.
            debug_name: self.pool.intern("@interp::run@"),
            when,
            constants,
        };
        self.ensure_compiled(&marker_callframe.constants, f, when)?;
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
        let _final_callframe = self.call_stack.pop().unwrap();
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
            self.update_debug();
            let i = self.next_inst();
            self.log_stack();
            logln!("I: {:?}", i.log(self.pool));
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
                        let ret = self.range_to_index(ret);
                        self.expand_maybe_tuple(prev.clone(), ret)?;
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
                    self.expand_maybe_tuple(value, abs)?;
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
                        frame.return_slot.first,
                        frame.return_slot.count
                    );

                    self.expand_maybe_tuple(value, frame.return_slot)?;

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
                        .map(|i| self.clone_slot(values.offset(i)))
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
                        let v = self.clone_slot(from.offset(i));
                        *self.get_slot_mut(to.offset(i)) = v;
                    }
                    self.bump_ip();
                }
                &Bc::MoveRange { from, to } => {
                    debug_assert_eq!(from.count, to.count);
                    for i in 0..from.count {
                        let v = self.take_slot(from.offset(i));
                        *self.get_slot_mut(to.offset(i)) = v;
                    }
                    self.bump_ip();
                }
                &Bc::ExpandTuple { from, to } => {
                    let tuple = self.take_slot(from);
                    let to = self.range_to_index(to);
                    self.expand_maybe_tuple(tuple, to)?;
                    self.bump_ip();
                }
                &Bc::Drop(slot) => {
                    assert_ne!(slot.count, 0);
                    for i in 0..slot.count {
                        let _ = self.take_slot(slot.offset(i));
                    }
                    self.bump_ip();
                }
                &Bc::AbsoluteStackAddr { of, to } => {
                    let ptr = self.range_to_index(of);
                    *self.get_slot_mut(to) = Value::InterpAbsStackAddr(ptr);
                    self.bump_ip();
                }
                &Bc::SlicePtr {
                    base,
                    offset,
                    count,
                    ret,
                } => {
                    let base = self.take_slot(base);
                    let res = self.slice_ptr(base, offset, count)?;
                    *self.get_slot_mut(ret) = res;
                    self.bump_ip();
                }
                &Bc::DerefPtr { from, to } => {
                    let arg = self.take_slot(from);
                    let value = self.deref_ptr(arg)?;
                    let ret = self.range_to_index(to);
                    self.expand_maybe_tuple(value, ret)?;
                    self.bump_ip();
                }
                Bc::DebugLine(_) | Bc::DebugMarker(_, _) => self.bump_ip(),
            }
        }
        self.pop_state(state);
        Ok(())
    }

    fn expand_maybe_tuple(&mut self, value: Value, target: StackAbsoluteRange) -> Res<'p, ()> {
        logln!("Expand {:?} TO {} slots", value, target.count);
        match target.count.cmp(&1) {
            std::cmp::Ordering::Equal => {
                self.value_stack[target.first.0] = value;
            }
            std::cmp::Ordering::Less => {
                todo!("zero argument return. probably just works but untested.")
            }
            std::cmp::Ordering::Greater => {
                let values = self.to_seq(value)?;
                assert_eq!(self, values.len(), target.count);
                let base = target.first.0;
                for (i, v) in values.into_iter().enumerate() {
                    assert_eq!(self, self.value_stack[base + i], Value::Poison);
                    self.value_stack[base + i] = v;
                }
            }
        }
        Ok(())
    }

    fn slice_ptr(&mut self, base: Value, offset: usize, count: usize) -> Res<'p, Value> {
        match base {
            Value::InterpAbsStackAddr(addr) => {
                assert!(self, count <= addr.count);
                Ok(Value::InterpAbsStackAddr(StackAbsoluteRange {
                    first: StackAbsolute(addr.first.0 + offset),
                    count,
                }))
            }
            Value::Heap {
                value,
                first,
                count,
            } => {
                let abs_first = first + count;
                let abs_last = abs_first + count;
                // Slicing operations are bounds checked.
                let data = unsafe { &*value };
                assert!(
                    self,
                    data.references > 0
                        && abs_first < data.values.len()
                        && abs_last <= data.values.len()
                );
                Ok(Value::Heap {
                    value,
                    first: first + offset,
                    count,
                })
            }
            _ => err!(self, "Wanted ptr found {:?}", base),
        }
    }

    fn deref_ptr(&mut self, arg: Value) -> Res<'p, Value> {
        match arg {
            Value::InterpAbsStackAddr(addr) => {
                if addr.count == 1 {
                    let value = self.value_stack[addr.first.0].clone();
                    assert_ne!(value, Value::Poison);
                    Ok(value)
                } else {
                    let values = &self.value_stack[addr.first.0..addr.first.0 + addr.count];
                    Ok(Value::Tuple {
                        container_type: TypeId::any(),
                        values: values.to_vec(),
                    })
                }
            }
            Value::Heap {
                value,
                first,
                count,
            } => {
                let data = unsafe { &*value };
                assert!(self, data.references > 0);
                Ok(if count == 1 {
                    let value = data.values[0].clone();
                    assert_ne!(value, Value::Poison);
                    value
                } else {
                    let values = &data.values[first..first + count];
                    Value::Tuple {
                        container_type: TypeId::any(),
                        values: values.to_vec(),
                    }
                })
            }
            _ => err!(self, "Wanted ptr found {:?}", arg),
        }
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
            "get" => self.deref_ptr(arg)?,
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
                                ptr.values[first + i] = entry;
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
            "len" => match arg {
                Value::InterpAbsStackAddr(addr) => Value::I64(addr.count as i64),
                Value::Heap { count, .. } => Value::I64(count as i64),
                _ => panic!("Wanted ptr found {:?}", arg),
            },
            "Ptr" => {
                // TODO: pointers shouldn't have a length
                let inner_ty = self.to_type(arg)?;
                let ty = self.program.intern_type(TypeInfo::Ptr(inner_ty));
                Value::Type(ty)
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
                    Value::Heap { value, first, .. } => {
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
                // println!("Fn: {:?}", arg);
                let (arg, ret) = self.to_pair(arg)?;
                let (arg, ret) = (self.to_type(arg)?, self.to_type(ret)?);
                let ty = self.program.intern_type(TypeInfo::Fn(FnType { arg, ret }));
                // println!("=> {}", self.program.log_type(ty));
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
            "Ty" => {
                if let Value::Type(ty) = arg {
                    assert!(
                        self,
                        false,
                        "Ty arg should be tuple of types not type {:?}",
                        self.program.log_type(ty)
                    );
                }
                print!("Ty: {:?}", arg);
                let ty = self.to_type(arg)?;
                // println!(" => {:?}", self.program.log_type(ty));
                Value::Type(ty)
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
        let return_slot = self.range_to_index(ret);
        let stack_base = self.value_stack.len(); // Our stack includes the argument but not the return slot.
        self.push_expanded(arg);
        let debug_name = self.program.funcs[f.0].get_name(self.pool);
        self.call_stack.push(CallFrame {
            stack_base: StackAbsolute(stack_base),
            current_func: f,
            current_ip: 0,
            return_slot,
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
            Value::Tuple { values, .. } => {
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
        let value = replace(self.get_slot_mut(slot), Value::Poison);
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

    pub fn empty_fn(&self, when: ExecTime, func: FuncId, loc: Span) -> FnBody<'p> {
        FnBody {
            insts: vec![],
            stack_slots: 0,
            vars: Default::default(),
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
            let mut result = self.empty_fn(when, FuncId(index + 10000000), func.loc);
            result.constants.parents.push(constants.clone().bake());
            let new_constants = func.local_constants.clone();

            let state = DebugState::EvalConstants(FuncId(index));
            self.push_state(&state);
            for stmt in new_constants {
                self.compile_stmt(&mut result, &stmt)?;
            }
            self.pop_state(state);
            constants.parents.push(result.constants.bake())
        }
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
        let func = &self.program.funcs[index];
        let (arg, _) = func.ty.unwrap();
        let mut result = self.empty_fn(when, FuncId(index), func.loc);
        result.constants = constants;
        let arg_range = result.reserve_slots(self.program, arg);
        let return_value = self.emit_body(&mut result, arg_range, FuncId(index))?;
        let func = &self.program.funcs[index];

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
        let arguments = func.arg_vars.as_ref().unwrap();
        if arguments.len() == 1 {
            // if there's one name, it refers to the whole tuple.
            let prev = result.vars.insert(arguments[0], (arg_range, arg));
            assert!(self, prev.is_none(), "overwrite arg?");
        } else if arguments.len() == arg_range.count {
            let types = self.program.tuple_types(arg).unwrap();
            // if they match, each element has its own name.
            for (i, var) in arguments.iter().enumerate() {
                // This always starts at 0 for normal functions, but for inlined closures, the args could be anywhere because we're sharing the parent's stack frame.
                let range = StackRange {
                    first: arg_range.offset(i),
                    count: 1,
                };
                let prev = result.vars.insert(*var, (range, types[i]));
                assert!(self, prev.is_none(), "overwrite arg?");
            }
        } else {
            // TODO: pattern match destructuring but for now you just cant refer to the arg.
            to_drop.push(arg_range);
        }
        let return_value = match func.body.as_ref() {
            Some(body) => {
                let (ret_val, found_ret_ty) = self.compile_expr(result, body, None)?;
                self.type_check_arg(found_ret_ty, ret, "bad return value")?;
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
                ret_val
            }
            None => {
                // Functions without a body are always builtins.
                // It's convient to give them a FuncId so you can put them in a variable,
                // but just force inline call.
                self.program.funcs[f.0].annotations.push(Annotation {
                    name: self.pool.intern("inline"),
                    args: None,
                });
                let ret = result.reserve_slots(self.program, ret);
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
        // This move ensures they end up at the base of the renumbered stack.
        let func = &self.program.funcs[f.0];
        let msg = "capturing calls are already inlined. what are you doing here?";
        assert_eq!(self, func.capture_vars.len(), 0, "{}", msg);
        let (arg_ty, _) = func.ty.unwrap();
        let stack_offset = result.stack_slots;
        let arg_slots = result.reserve_slots(self.program, arg_ty); // These are included in the new function's stack
        debug_assert_eq!(arg_slots.count, arg.count);
        result.push(Bc::MoveRange {
            from: arg,
            to: arg_slots,
        });
        let ip_offset = result.insts.len();
        let func = self.ready[f.0].as_ref().unwrap();
        // TODO: check for recusion somewhere.
        // TODO: put constants somewhere so dont have to clone them each time a function is inlined.
        result.stack_slots += func.stack_slots;
        result.slot_types.extend(func.slot_types.iter());
        let mut has_returned = false; // TODO: remove
        for (_, mut inst) in func.insts.iter().cloned().enumerate() {
            assert!(self, !has_returned); // TODO
            inst.renumber(stack_offset, ip_offset);
            if let Bc::Ret(return_value) = inst {
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

    // TODO: do the memo stuff here instead of my crazy other thing.
    // TODO: maybe call this @generic instead of @comptime? cause other things can be comptime
    // Return type is allowed to use args.
    fn emit_comptime_call(
        &mut self,
        result: &mut FnBody<'p>,
        f: FuncId,
        arg_expr: &FatExpr<'p>,
    ) -> Res<'p, Value> {
        let func = self.program.funcs[f.0].clone();
        let arg_ty = match &func.ty {
            &LazyFnType::Finished(arg, _) => arg,
            LazyFnType::Pending { arg, ret: _ } => match arg {
                LazyType::Infer => todo!(),
                LazyType::PendingEval(arg) => {
                    let arg =
                        self.cached_eval_expr(&result.constants, arg.clone(), TypeId::ty())?;
                    self.to_type(arg)?
                }
                &LazyType::Finished(arg) => arg,
            },
        };
        let arg_value = self.cached_eval_expr(&result.constants, arg_expr.clone(), arg_ty)?;
        let arg_ty = self.program.type_of(&arg_value);
        let ret = match &func.ty {
            &LazyFnType::Finished(arg, ret) => {
                self.type_check_arg(arg_ty, arg, "bad comtime arg")?;
                ret
            }
            LazyFnType::Pending { arg, ret } => {
                match arg {
                    LazyType::Infer => todo!(),
                    LazyType::PendingEval(arg) => {
                        let arg =
                            self.cached_eval_expr(&result.constants, arg.clone(), TypeId::ty())?;
                        let arg = self.to_type(arg)?;
                        self.type_check_arg(arg_ty, arg, "bad comtime arg")?;
                    }
                    &LazyType::Finished(arg) => {
                        self.type_check_arg(arg_ty, arg, "bad comtime arg")?;
                    }
                }
                match ret {
                    LazyType::Infer => todo!(),
                    LazyType::PendingEval(ret) => {
                        // Bind the arg into my own result so the ret calculation can use it.
                        let arguments = self.program.funcs[f.0].arg_vars.as_ref().unwrap();
                        if arguments.len() == 1 {
                            let prev = result.constants.insert(arguments[0], (arg_value, arg_ty));
                            // TODO: remove at the end so can do again.
                            assert!(self, prev.is_none(), "overwrite comptime arg?");
                        } else {
                            todo!()
                        }

                        let ret =
                            self.cached_eval_expr(&result.constants, ret.clone(), TypeId::ty())?;
                        self.to_type(ret)?
                    }
                    &LazyType::Finished(ret) => ret,
                }
            }
        };

        let result = self.cached_eval_expr(&result.constants, func.body.unwrap(), ret)?;
        let ty = self.program.type_of(&result);
        self.type_check_arg(ty, ret, "generic result")?;

        Ok(result)
    }

    fn compile_stmt(&mut self, result: &mut FnBody<'p>, stmt: &FatStmt<'p>) -> Res<'p, ()> {
        self.last_loc = Some(stmt.loc);
        match stmt.deref() {
            Stmt::Eval(expr) => {
                let (value, _) = self.compile_expr(result, expr, None)?;
                result.push(Bc::Drop(value));
            }
            Stmt::DeclVar {
                name,
                ty,
                value,
                dropping,
                kind,
            } => {
                let expected_ty = invert(
                    ty.clone()
                        .map(|ty| self.cached_eval_expr(&result.constants, ty, TypeId::ty())),
                )?
                .map(|ty| self.to_type(ty).unwrap());
                match kind {
                    VarType::Const => {
                        let mut value = match value {
                            Some(value) => self.cached_eval_expr(
                                &result.constants,
                                value.clone(),
                                expected_ty.unwrap_or(TypeId::any()),
                            )?,
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

                        if let Some(expected_ty) = expected_ty {
                            if self.program.types[expected_ty.0] == TypeInfo::Type {
                                // HACK. todo: general overloads for cast()
                                value = Value::Type(self.to_type(value)?)
                            }
                            let found_ty = self.program.type_of(&value);
                            self.type_check_eq(found_ty, expected_ty, "var decl")?;
                        }
                        let found_ty = self.program.type_of(&value);
                        result.constants.insert(*name, (value, found_ty));
                    }
                    VarType::Let | VarType::Var => {
                        let value = invert(
                            value
                                .as_ref()
                                .map(|expr| self.compile_expr(result, expr, expected_ty)),
                        )?;
                        self.last_loc = Some(stmt.loc);
                        let value = match (value, expected_ty) {
                            (None, Some(expected_ty)) => {
                                (result.reserve_slots(self.program, expected_ty), expected_ty)
                            }
                            (Some(value), None) => value,
                            (Some((value, ty)), Some(expected_ty)) => {
                                self.type_check_arg(ty, expected_ty, "var decl")?;
                                (value, ty)
                            }

                            (None, None) => {
                                err!(self, "{:?} decl with unknown type and no init", name)
                            }
                        };
                        let prev = result.vars.insert(*name, value);
                        assert!(self, prev.is_none(), "shadow is still new var");

                        // TODO: what if shadow is const? that would be more consistant if did it like rust.
                        if let Some(dropping) = dropping {
                            // Maybe should be like rust and dont call drop on the shadowed thing until the end of scope.
                            // It would be consistant and it mean you can reference its data if you do a chain of transforming something with the same name.
                            // But need to change my debugging check that everything was dropped.
                            // Actually if i did that just put them in the block's list instead of carefully taking them out which i did because i thought i wanted to egarly drop.
                            let (slot, _) =
                                unwrap!(self, result.vars.remove(dropping), "missing shadow");
                            result.push(Bc::Drop(slot));
                        }
                    }
                }
            }
            Stmt::SetVar(var, expr) => {
                let var_info = self.program.vars[var.1]; // TODO: the type here isn't set.
                assert_eq!(
                    self, var_info.kind,
                    VarType::Var,
                    "Only 'var' can be reassigned (not let/const). {:?}", stmt
                );
                let slot = result.vars.get(var);
                let (slot, oldty) = *unwrap!(
                    self,
                    slot,
                    "SetVar: var must be declared: {}",
                    var.log(self.pool)
                );

                let (value, new_ty) = self.compile_expr(result, expr, Some(oldty))?;
                self.type_check_arg(new_ty, oldty, "reassign var")?;
                result.push(Bc::Drop(slot));
                assert_eq!(self, value.count, slot.count);
                for i in 0..value.count {
                    result.push(Bc::Move {
                        from: value.offset(i),
                        to: slot.offset(i),
                    });
                }
            }
            Stmt::SetNamed(name, _) => err!(self, CErr::UndeclaredIdent(*name)),
            Stmt::DeclNamed { .. } => {
                ice!(self, "Scope resolution failed {}", stmt.log(self.pool))
            }
            Stmt::Noop => {}
            Stmt::DeclFunc(func) => {
                self.program.add_func(func.clone());
            }
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
        requested: Option<TypeId>,
    ) -> Res<'p, (StackRange, TypeId)> {
        result.last_loc = expr.loc;
        self.last_loc = Some(expr.loc);

        Ok(match expr.deref() {
            Expr::Closure(func) => {
                let id = self.program.add_func(*func.clone());
                self.ensure_compiled(&result.constants, id, result.when)?;
                result.load_constant_new(self.program, Value::GetFn(id))
            }
            Expr::Call(f, arg) => {
                if let Expr::GetNamed(i) = f.as_ref().deref() {
                    if let Some(f) = self.resolve_function(result, f) {
                        let func = &self.program.funcs[f.0];
                        let is_comptime = func.has_tag(self.pool, "comptime");
                        if is_comptime {
                            let ret = self.emit_comptime_call(result, f, arg)?;
                            return Ok(result.load_constant_new(self.program, ret));
                        }

                        let (mut arg, arg_ty_found) = self.compile_expr(result, arg, None)?;
                        // TODO: this fixes inline calls when no arguments. do better. support zero-sized types in general.
                        if arg.count == 0 {
                            arg = result.load_constant_new(self.program, Value::Unit).0;
                        }
                        // Note: f will be compiled differently depending on the calling convention.
                        // But, we do need to know how many values it returns. If there's no type annotation, this might end up compiling the function to figure it out.
                        self.infer_types(&result.constants, f)?;
                        let func = &self.program.funcs[f.0];
                        let (arg_ty_expected, ret_ty_expected) = func.ty.unwrap();
                        self.last_loc = Some(expr.loc); // TODO: have a stack so i dont have to keep doing this.
                        self.type_check_arg(arg_ty_found, arg_ty_expected, "bad arg")?;
                        debug_assert_eq!(
                            self.program.slot_count(arg_ty_found),
                            self.program.slot_count(arg_ty_expected)
                        );
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
                            let cache_arg = result.reserve_slots(self.program, TypeId::any());
                            result.push(Bc::CloneCreateTuple {
                                values: arg,
                                target: cache_arg.single(),
                            });
                            Some(cache_arg)
                        } else {
                            None
                        };
                        let ret = result.reserve_slots(self.program, ret_ty_expected);
                        assert_eq!(self, self.return_stack_slots(f), ret.count);
                        let func = &self.program.funcs[f.0];
                        if returns_type {
                            self.ensure_compiled(&result.constants, f, result.when)?;
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
                            let ty = self.program.intern_type(TypeInfo::Tuple(vec![
                                TypeId::any(),
                                TypeId::any(),
                                TypeId::any(),
                            ]));
                            let f_arg_ret = result.reserve_slots(self.program, ty);
                            debug_assert_eq!(f_arg_ret.count, 3);
                            result.push(Bc::LoadConstant {
                                slot: f_arg_ret.offset(0),
                                value: Value::GetFn(f),
                            });
                            result.push(Bc::Move {
                                from: cache_arg.single(),
                                to: f_arg_ret.offset(1),
                            });
                            result.push(Bc::CloneCreateTuple {
                                values: ret,
                                target: f_arg_ret.offset(2),
                            });
                            let nothing = result.reserve_slots_raw(self.program, 1, TypeId::unit());
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

                let (arg, arg_ty_found) = self.compile_expr(result, arg, None)?;
                let (f_slot, func_ty) = self.compile_expr(result, f, None)?;
                logln!(
                    "dynamic {} is {}",
                    f.log(self.pool),
                    self.program.log_type(func_ty)
                );
                if !func_ty.is_any() {
                    let ty = unwrap!(self, self.program.fn_ty(func_ty), "expected fn for call");
                    self.type_check_arg(arg_ty_found, ty.arg, "dyn call bad arg")?;
                }
                let ret_ty = if let TypeInfo::Fn(ty) = &self.program.types[func_ty.0] {
                    ty.ret
                } else {
                    logln!("WARNING: Called Any {:?}", expr.log(self.pool));
                    assert!(self, func_ty.is_any());
                    TypeId::any() // TODO
                };
                assert_eq!(self, f_slot.count, 1);
                let ret = result.reserve_slots(self.program, ret_ty);
                result.push(Bc::CallDynamic {
                    f: f_slot.first,
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
                let ret = self.compile_expr(result, value, requested)?;

                for local in locals.as_ref().expect("resolve failed") {
                    if let Some((slot, _ty)) = result.vars.remove(local) {
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
                debug_assert!(values.len() > 1, "no trivial tuples");
                let values: Res<'p, Vec<_>> = values
                    .iter()
                    .map(|v| self.compile_expr(result, v, None))
                    .collect();
                let values = values?;
                let types: Vec<_> = values.iter().map(|(_, ty)| *ty).collect();
                let ty = self.program.intern_type(TypeInfo::Tuple(types));

                let ret = result.reserve_slots(self.program, ty);
                // TODO: they might already be consecutive. kinda want to have something to profile before fixing.
                let base = ret.first.0;
                let mut count = 0;
                for (v, _) in values {
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
                    let to = result.reserve_slots(self.program, ty);
                    debug_assert_eq!(from.count, to.count);
                    if from.count == 1 {
                        result.push(Bc::Clone {
                            from: from.first,
                            to: to.first,
                        });
                    } else {
                        result.push(Bc::CloneRange { from, to });
                    }
                    (to, ty)
                } else if let Some((value, _)) = result.constants.get(var) {
                    result.load_constant_new(self.program, value)
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
                    result.load_constant_new(self.program, Value::GetFn(func))
                } else {
                    ice!(
                        self,
                        "Scope resolution failed {} (in Expr::GetNamed)",
                        expr.log(self.pool)
                    );
                }
            }
            Expr::EnumLiteral(_) => todo!(),
            Expr::Value(value) => result.load_constant_new(self.program, value.clone()),
            Expr::SuffixMacro(macro_name, arg) => {
                let name = self.pool.get(*macro_name);
                match name {
                    // TODO: make `let` deeply immutable so only const addr
                    // Note: arg is not evalutated
                    "addr" => self.addr_macro(result, arg)?,
                    "deref" => {
                        let (from, ptr_ty) = self.compile_expr(result, arg, requested)?;
                        let ty = unwrap!(self, self.program.unptr_ty(ptr_ty), "not ptr");
                        let to = result.reserve_slots(self.program, ty);
                        result.push(Bc::DerefPtr {
                            from: from.single(),
                            to,
                        });
                        (to, ty)
                    }
                    "type" => {
                        // Note: this does not evaluate the expression.
                        // TODO: warning if it has side effects.
                        let ty = self.type_of(result, arg)?;
                        result.load_constant_new(self.program, Value::Type(ty))
                    }
                    "assert_compile_error" => {
                        // TODO: this can still have side-effects on the vm state tho :(
                        let state = self.mark_state(result.clone());
                        let res = self.compile_expr(result, arg, None);
                        assert!(self, res.is_err());
                        mem::forget(res); // TODO: dont do this. but for now i like having my drop impl that prints it incase i forget  ot unwrap
                        *result = self.restore_state(state);
                        result.load_constant_new(self.program, Value::Unit)
                    }
                    "comptime_print" => {
                        println!("EXPR : {}", arg.log(self.pool));
                        let value =
                            self.cached_eval_expr(&result.constants, *arg.clone(), TypeId::any());
                        println!("VALUE: {:?}", value);
                        result.load_constant_new(self.program, Value::Unit)
                    }
                    "struct" => {
                        if let Expr::StructLiteralP(pattern) = arg.deref().deref() {
                            let names: Vec<_> = pattern.names.iter().map(|n| n.unwrap()).collect();
                            // TODO: why must this suck so bad
                            let types: Res<'p, Vec<_>> = pattern
                                .types
                                .iter()
                                .map(|ty| {
                                    self.cached_eval_expr(
                                        &result.constants,
                                        ty.clone().unwrap(),
                                        TypeId::ty(),
                                    )
                                })
                                .collect();
                            let types: Res<'p, Vec<_>> =
                                types?.into_iter().map(|ty| self.to_type(ty)).collect();
                            let types = types?;
                            let as_tuple = self.program.intern_type(TypeInfo::Tuple(types.clone()));
                            let mut fields = vec![];
                            let mut size = 0;
                            for (name, ty) in names.into_iter().zip(types.into_iter()) {
                                let count = self.program.slot_count(ty);
                                fields.push(Field {
                                    name,
                                    ty,
                                    first: size,
                                    count,
                                });
                                size += count;
                            }
                            let ty = TypeInfo::Struct {
                                fields,
                                size,
                                as_tuple,
                            };
                            let ty = Value::Type(self.program.intern_type(ty));
                            result.load_constant_new(self.program, ty)
                        } else {
                            err!(
                                self,
                                "expected map literal: .{{ name: Type, ... }} but found {:?}",
                                arg
                            );
                        }
                    }
                    _ => return Err(self.error(CErr::UndeclaredIdent(*macro_name))),
                }
            }
            Expr::FieldAccess(e, name) => {
                let (mut container_ptr, mut container_ptr_ty) = self.addr_macro(result, e)?;
                // Auto deref for nested place expressions.
                // TODO: i actually just want same depth in chains, not always deref all the way, you might want to do stuff with a &&T or whatever.
                let depth = self.program.ptr_depth(container_ptr_ty);
                if depth > 1 {
                    for _ in 0..(depth - 1) {
                        container_ptr_ty =
                            unwrap!(self, self.program.unptr_ty(container_ptr_ty), "");
                        let ret = result.reserve_slots(self.program, container_ptr_ty);
                        result.push(Bc::DerefPtr {
                            from: container_ptr.single(),
                            to: ret,
                        });
                        container_ptr = ret;
                    }
                }
                let container_ty =
                    unwrap!(self, self.program.unptr_ty(container_ptr_ty), "unreachable");
                if let TypeInfo::Struct { fields, .. } = &self.program.types[container_ty.0] {
                    for f in fields {
                        if f.name == *name {
                            let f = *f;
                            let ty = self.program.ptr_type(f.ty);
                            let ret = result.reserve_slots(self.program, ty);

                            result.push(Bc::SlicePtr {
                                base: container_ptr.single(),
                                offset: f.first,
                                count: f.count,
                                ret: ret.single(),
                            });
                            return Ok((ret, ty));
                        }
                    }
                    err!(
                        self,
                        "unknown name {} on {:?}",
                        self.pool.get(*name),
                        self.program.log_type(container_ty)
                    );
                } else {
                    err!(
                        self,
                        "only structs support field access but found {:?} which is {}",
                        e.log(self.pool),
                        self.program.log_type(container_ty)
                    );
                }
            }
            Expr::StructLiteralP(pattern) => {
                assert!(self, requested.is_some());
                let names: Vec<_> = pattern.names.iter().map(|n| n.unwrap()).collect();
                // TODO: why must this suck so bad
                let values: Option<_> = pattern.types.iter().cloned().collect();
                let values: Vec<FatExpr<'_>> = values.unwrap();
                if let TypeInfo::Struct {
                    fields, as_tuple, ..
                } = self.program.types[requested.unwrap().0].clone()
                {
                    assert_eq!(self, names.len(), values.len());
                    assert_eq!(self, fields.len(), values.len());
                    let all = names.into_iter().zip(values).zip(fields);
                    let mut values = vec![];
                    for ((name, value), field) in all {
                        assert_eq!(self, name, field.name);
                        let (value, found_ty) =
                            self.compile_expr(result, &value, Some(field.ty))?;
                        self.type_check_arg(found_ty, field.ty, "struct field")?;
                        values.push(value);
                    }

                    let ret = result.reserve_slots(self.program, as_tuple);
                    // TODO: they might already be consecutive. kinda want to have something to profile before fixing.
                    let base = ret.first.0;
                    let mut count = 0;
                    for v in values {
                        result.push(Bc::MoveRange {
                            from: v,
                            to: StackRange {
                                first: StackOffset(base + count),
                                count: v.count,
                            },
                        });
                        count += v.count;
                    }
                    assert_eq!(self, count, ret.count);
                    (ret, requested.unwrap())
                } else {
                    err!(self, "struct literal but expected {:?}", requested);
                }
            }
        })
    }

    fn addr_macro(
        &mut self,
        result: &mut FnBody<'p>,
        arg: &FatExpr<'p>,
    ) -> Res<'p, (StackRange, TypeId)> {
        match arg.deref().deref() {
            Expr::GetVar(var) => {
                if let Some((stack_slot, value_ty)) = result.vars.get(var).cloned() {
                    let ptr_ty = self.program.ptr_type(value_ty);
                    let addr_slot = result.reserve_slots(self.program, ptr_ty);
                    result.push(Bc::AbsoluteStackAddr {
                        of: stack_slot,
                        to: addr_slot.first,
                    });
                    Ok((addr_slot, ptr_ty))
                } else if result.constants.get(var).is_some() {
                    err!(self, "Took address of constant {}", var.log(self.pool))
                } else {
                    ice!(self, "Missing var {} (in !addr)", var.log(self.pool))
                }
            }
            Expr::SuffixMacro(macro_name, _) => {
                let name = self.pool.get(*macro_name);
                ice!(self, "Took address of macro {name} not supported")
            }
            // TODO: this is a bit weird but it makes place expressions work.
            Expr::FieldAccess(_, _) => self.compile_expr(result, arg, None),
            &Expr::GetNamed(i) => return Err(self.error(CErr::UndeclaredIdent(i))),
            _ => return Err(self.error(CErr::AddrRvalue(arg.clone()))),
        }
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

    fn resolve_function(&self, _result: &FnBody<'p>, expr: &FatExpr<'p>) -> Option<FuncId> {
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
            Expr::Call(f, _) => {
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
                    "deref" => {
                        let ptr_ty = self.type_of(result, arg)?;
                        unwrap!(self, self.program.unptr_ty(ptr_ty), "")
                    }
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
            Expr::StructLiteralP(_) => todo!(),
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

    // TOOD: @track_caller
    fn update_debug(&mut self) {
        let frame = self.call_stack.last().unwrap();
        let body = self
            .ready
            .get(frame.current_func.0)
            .unwrap()
            .as_ref()
            .unwrap();
        self.last_loc = body.debug.get(frame.current_ip).map(|i| i.src_loc);
    }

    fn current_fn_body(&self) -> &FnBody {
        let frame = self.call_stack.last().unwrap();
        let body = self.ready.get(frame.current_func.0).unwrap();
        body.as_ref().expect("jit current function")
    }

    // Resolve the lazy types for Arg and Ret
    fn infer_types(&mut self, constants: &SharedConstants<'p>, func: FuncId) -> Res<'p, FnType> {
        let f = &self.program.funcs[func.0];
        self.last_loc = Some(f.loc);
        match f.ty.clone() {
            LazyFnType::Pending { arg, ret } => {
                let state = DebugState::ResolveFnType(func, arg.clone(), ret.clone());
                self.push_state(&state);
                logln!("RESOLVE: Arg of {func:?}");
                let arg = match arg {
                    LazyType::Infer => TypeId::any(),
                    LazyType::PendingEval(e) => {
                        let value = self.cached_eval_expr(constants, e.clone(), TypeId::ty())?;
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
                        let value = self.cached_eval_expr(constants, e.clone(), TypeId::ty())?;
                        self.to_type(value)?
                    }
                    LazyType::Finished(id) => id, // easy
                };
                self.program.funcs[func.0].ty = LazyFnType::Finished(arg, ret);
                self.pop_state(state);
            }
            LazyFnType::Finished(_, _) => {} // easy
        }
        // TODO: go find everywhere that calls this and then immediatly unwraps.
        let (arg, ret) = self.program.funcs[func.0].ty.unwrap();
        Ok(FnType { arg, ret })
    }

    fn unit_to(&mut self, ret: TypeId) -> LazyFnType<'p> {
        LazyFnType::Finished(self.program.intern_type(TypeInfo::Unit), ret)
    }

    fn cached_eval_expr(
        &mut self,
        constants: &SharedConstants<'p>,
        e: FatExpr<'p>,
        ret_ty: TypeId,
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
            Expr::Call(_, _) => {
                // TODO: fast path for checking the generics cache
            }
            _ => {} // fallthrough
        }
        let state = DebugState::ComputeCached(e.clone());
        self.push_state(&state);
        let name = format!("@eval_{}@", self.anon_fn_counter);
        let fake_func: Func<'p> = Func {
            name: Some(self.pool.intern(&name)),
            ty: self.unit_to(ret_ty),
            body: Some(e.clone()),
            arg_names: vec![],
            annotations: vec![],
            arg_vars: Some(vec![]),
            capture_vars: vec![],
            local_constants: Default::default(),
            loc: e.loc,
            arg_loc: vec![],
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
            // TODO: fix horible special case. This causes infinite pain for tuple types because you need to put them in a const with a type annotation,
            //       or they're passed as multiple arguments.
            let ty = match values.len() {
                0 => TypeId::unit(),
                1 => self.to_type(values.into_iter().next().unwrap())?,
                _ => {
                    // println!("Type Tuple: {:?}", values);
                    let values: Res<'_, Vec<_>> =
                        values.into_iter().map(|v| self.to_type(v)).collect();
                    self.program.intern_type(TypeInfo::Tuple(values?))
                }
            };
            Ok(ty)
        } else if let Value::Unit = value {
            // This lets you use the literal `()` as a type (i dont parse it as a tuple because reasons). TODO: check if that still true with new parser
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
            Value::Tuple { values, .. } | Value::Array { values, .. } => Ok(values),
            Value::Slice(_) => todo!(),
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
            Value::Tuple { mut values, .. } => {
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

    // TODO: make this not a special case.
    fn emit_call_if(
        &mut self,
        result: &mut FnBody<'p>,
        arg: &FatExpr<'p>,
    ) -> Result<(StackRange, TypeId), CompileError<'p>> {
        let ((cond, _), if_true, if_false) = if let Expr::Tuple(parts) = arg.deref() {
            let cond = self.compile_expr(result, &parts[0], None)?;
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

        let true_ty = self.infer_types(&result.constants, if_true)?;
        let unit = self.program.intern_type(TypeInfo::Unit);
        let sig = "if(bool, fn(Unit) T, fn(Unit) T)";
        self.type_check_eq(true_ty.arg, unit, sig)?;
        let false_ty = self.infer_types(&result.constants, if_false)?;
        self.type_check_eq(false_ty.arg, unit, sig)?;
        self.type_check_eq(true_ty.ret, false_ty.ret, sig)?;

        // TODO: if returning tuples
        let ret = result.reserve_slots(self.program, true_ty.ret);

        let arg = result.load_constant_new(self.program, Value::Unit).0; // Note: before you start doing ip stuff!
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

        Ok((ret, true_ty.ret))
    }

    #[track_caller]
    fn type_check_eq(&self, found: TypeId, expected: TypeId, msg: &'static str) -> Res<'p, ()> {
        if found == expected {
            Ok(())
        } else {
            err!(self, CErr::TypeCheck(found, expected, msg))
        }
    }

    // #[track_caller]
    fn type_check_arg(&self, found: TypeId, expected: TypeId, msg: &'static str) -> Res<'p, ()> {
        if found == expected || found.is_any() || expected.is_any() {
            Ok(())
        } else {
            match (
                &self.program.types[found.0],
                &self.program.types[expected.0],
            ) {
                (TypeInfo::Tuple(f), TypeInfo::Tuple(e)) => {
                    if f.len() == e.len() {
                        let ok = f
                            .iter()
                            .zip(e.iter())
                            .all(|(f, e)| self.type_check_arg(*f, *e, msg).is_ok());
                        if ok {
                            return Ok(());
                        }
                    }
                }
                (&TypeInfo::Ptr(f), &TypeInfo::Ptr(e)) => {
                    if self.type_check_arg(f, e, msg).is_ok() {
                        return Ok(());
                    }
                }
                (TypeInfo::Tuple(_), TypeInfo::Type) | (TypeInfo::Type, TypeInfo::Tuple(_)) => {
                    return Ok(())
                }
                (&TypeInfo::Fn(f), &TypeInfo::Fn(e)) => {
                    if self.type_check_arg(f.arg, e.arg, msg).is_ok()
                        && self.type_check_arg(f.ret, e.ret, msg).is_ok()
                    {
                        return Ok(());
                    }
                }
                _ => {}
            }
            err!(self, CErr::TypeCheck(found, expected, msg))
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ExecTime {
    Comptime,
    Runtime,
}

impl<'p> FnBody<'p> {
    fn reserve_slots_raw(&mut self, program: &Program<'p>, count: usize, ty: TypeId) -> StackRange {
        let first = StackOffset(self.stack_slots);
        if ty.is_any() {
            // debug_assert_eq!(count, 1, "no any tuple");
            for _ in 0..count {
                self.slot_types.push(TypeId::any());
            }
            self.stack_slots += count;
        } else if count == 1 {
            self.slot_types.push(ty);
            self.stack_slots += count;
        } else {
            let types = program.tuple_types(ty).unwrap();
            let mut found = 0;
            for ty in types {
                found += self.reserve_slots(program, *ty).count;
            }
            debug_assert_eq!(found, count, "bad tuple size");
            // Note: don't bump self.stack_slots here.
        }
        StackRange { first, count }
    }

    fn reserve_slots(&mut self, program: &Program<'p>, ty: TypeId) -> StackRange {
        let count = program.slot_count(ty);
        self.reserve_slots_raw(program, count, ty)
    }

    fn load_constant_new(
        &mut self,
        program: &mut Program<'p>,
        value: Value,
    ) -> (StackRange, TypeId) {
        let ty = program.type_of(&value);
        let to = self.reserve_slots(program, ty);
        self.push(Bc::LoadConstant {
            slot: to.first,
            value,
        });
        (to, ty)
    }

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

/// https://users.rust-lang.org/t/convenience-method-for-flipping-option-result-to-result-option/13695
fn invert<T, E>(x: Option<Result<T, E>>) -> Result<Option<T>, E> {
    x.map_or(Ok(None), |v| v.map(Some))
}

fn to_flat_seq(value: Value) -> Vec<Value> {
    match value {
        Value::Tuple { values, .. } | Value::Array { values, .. } => {
            values.into_iter().flat_map(to_flat_seq).collect()
        }
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
            Bc::CallDirectMaybeCached { f: _, ret, arg } | Bc::CallDirect { f: _, ret, arg } => {
                ret.first.0 += stack_offset;
                arg.first.0 += stack_offset;
            }
            Bc::CallBuiltin { ret, arg, .. } => {
                ret.first.0 += stack_offset;
                arg.first.0 += stack_offset;
            }
            Bc::LoadConstant { slot, .. } => slot.0 += stack_offset,
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
            Bc::CloneCreateTuple { target, .. } | Bc::MoveCreateTuple { target, .. } => {
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
            Bc::ExpandTuple { from, to } | Bc::DerefPtr { from, to } => {
                from.0 += stack_offset;
                to.first.0 += stack_offset;
            }
            Bc::AbsoluteStackAddr { of, to } => {
                of.first.0 += stack_offset;
                to.0 += stack_offset;
            }
            Bc::SlicePtr {
                base, offset, ret, ..
            } => {
                base.0 += stack_offset;
                *offset += stack_offset;
                ret.0 += stack_offset;
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

    pub fn insert(&mut self, k: Var<'p>, v: (Value, TypeId)) -> Option<(Value, TypeId)> {
        self.local.insert(k, v)
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
