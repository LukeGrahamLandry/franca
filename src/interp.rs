#![allow(clippy::wrong_self_convention)]
use std::{
    collections::HashMap,
    fmt::{format, Debug},
    mem::replace,
    ops::Deref,
    panic::Location,
    ptr,
};

use crate::{
    ast::{
        Expr, FatExpr, FnType, Func, FuncId, LazyFnType, LazyType, Program, Stmt, TypeId, TypeInfo,
        Var,
    },
    pool::{Ident, StringPool},
};

macro_rules! bin_int {
    ($op:tt, $arg:expr, $res:expr) => {{
        let (a, b) = load_int_pair($arg);
        $res(a $op b)
    }};
}

pub enum LogTag {
    Parsing,
    InstLoop,
    Jitting,
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
    Fn(TypeId, usize),
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
struct StackOffset(usize);

impl StackOffset {
    fn to_range(&self) -> StackRange {
        StackRange {
            first: *self,
            count: 1,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
struct StackAbsolute(usize);

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
pub struct StackAbsoluteRange {
    first: StackAbsolute,
    count: usize,
}

#[derive(Copy, Clone)]
struct StackRange {
    first: StackOffset,
    count: usize,
}

impl StackRange {
    fn index(&self, offset: usize) -> StackOffset {
        assert!(offset < self.count);
        StackOffset(self.first.0 + offset)
    }
}

pub struct InterpBox {
    references: isize,
    values: Vec<Value>,
}

// TODO: smaller index sizes so can pack these?
enum Bc<'p> {
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
    // TODO: having memory is bad!
    CreateTuple {
        values: StackRange,
        target: StackOffset,
    },
    Ret(StackRange),
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
    ExpandTuple {
        from: StackOffset,
        to: StackRange,
    },
    Drop(StackRange),
    AbsoluteStackAddr {
        of: StackRange,
        to: StackOffset,
    },
}
#[derive(Debug, PartialEq, Clone, Copy)]
struct CallFrame<'p> {
    stack_base: StackAbsolute,
    current_func: FuncId,
    current_ip: usize,
    return_slot: StackAbsolute,
    return_count: usize,
    is_rust_marker: bool,
    debug_name: Ident<'p>,
    when: ExecTime,
}

struct FnBody<'p> {
    insts: Vec<Bc<'p>>,
    stack_slots: usize,
    vars: HashMap<Var<'p>, StackRange>, // TODO: use a vec
    arg_names: Vec<Option<Ident<'p>>>,
    when: ExecTime,
    slot_types: Vec<TypeId>,
    func: FuncId,
}

impl<'p> FnBody<'p> {
    fn push(&mut self, inst: Bc<'p>) -> usize {
        let ip = self.insts.len();
        self.insts.push(inst);
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
    pool: &'a StringPool<'p>,
    value_stack: Vec<Value>,
    call_stack: Vec<CallFrame<'p>>,
    program: &'a mut Program<'p>,
    ready: Vec<Option<FnBody<'p>>>,
    builtins: Vec<Ident<'p>>,
    log_depth: usize,
    // TODO: really need to have unique ids on expressions so im not just recursively hashing it and hoping for the best
    comptime_cache: HashMap<FatExpr<'p>, Value>,
    // Since there's a kinda confusing recursive structure for interpreting a program, it feels useful to keep track of where you are.
    debug_trace: Vec<DebugState<'p>>,
    anon_fn_counter: usize,
    pub assertion_count: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DebugState<'p> {
    OuterCall(FuncId, Value),
    JitToBc(FuncId, ExecTime),
    RunInstLoop(FuncId),
    ComputeCached(FatExpr<'p>),
    ResolveFnType(FuncId, LazyType<'p>, LazyType<'p>),
}

#[derive(Clone, Debug)]
pub struct CompileError<'p> {
    loc: &'static Location<'static>,
    reason: CErr<'p>,
    trace: Vec<DebugState<'p>>,
    value_stack: Vec<Value>,
    call_stack: Vec<CallFrame<'p>>,
}

#[derive(Clone, Debug)]
enum CErr<'p> {
    UndeclaredIdent(Ident<'p>),
    ComptimeCallAtRuntime,
    Ice(&'static str),
    LeakedValue,
    StackDepthLimit,
    AddrRvalue(FatExpr<'p>),
    TypeError(&'static str, Value),
}

// Just in case im stupid and forget to unwrap one somewhere.
// TODO: why am i not getting must use warnings from the result?
// TODO: i need better formating for errors because rn it prints the entire stack of poisons
//       the benifit of this drop even when no bugs is it puts the actual problem under my giant log message
impl<'p> Drop for CompileError<'p> {
    fn drop(&mut self) {
        println!("=============");
        println!("COMPILE ERROR: {:?} (Internal: {})", self.reason, self.loc);
        println!("=============");
    }
}

// TODO: rn these eat any calls. no overload checking.
// TODO: always put these at start of pool so can use indexes without hashmap lookup
//       IMPORTANT: interp should probably made the pool if i do that.
const BUILTINS: &[&str] = &[
    "add",
    "sub",
    "mul",
    "div",
    "eq",
    "ne",
    "lt",
    "gt",
    "ge",
    "le",
    "Tuple",
    "tuple",
    "assert_eq",
    "is_comptime",
    "get",
    "set",
    "is_uninit",
    "len",
    "Ptr",
    "is_oob_stack",
    "slice",
    "alloc",
    "free",
    "print",
];

// TODO: use this for op call builtin to avoid a runtime hashmap lookup.
//       but it seems you cant put this as a match left side so it doesnt help me anyway.
const fn builtin_i(name: &'static str) -> usize {
    // This is insane because iter.position isn't const.
    let mut i = 0;
    loop {
        if i == BUILTINS.len() {
            break;
        }
        // == isnt const
        if matches!(BUILTINS[i], name) {
            return i;
        }
        i += 1;
    }
    1
}

impl<'a, 'p> Interp<'a, 'p> {
    pub fn new(pool: &'a StringPool<'p>, program: &'a mut Program<'p>) -> Self {
        Self {
            pool,
            value_stack: vec![],
            call_stack: vec![],
            program,
            ready: vec![],
            builtins: BUILTINS.iter().map(|name| pool.intern(name)).collect(),
            log_depth: 0,
            comptime_cache: Default::default(),
            debug_trace: vec![],
            anon_fn_counter: 0,
            assertion_count: 0,
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
        assert_eq!(found, s);
    }

    #[track_caller]
    fn error(&self, reason: CErr<'p>) -> CompileError<'p> {
        CompileError {
            loc: Location::caller(),
            reason,
            trace: self.debug_trace.clone(),
            value_stack: self.value_stack.clone(),
            call_stack: self.call_stack.clone(),
        }
    }

    #[track_caller]
    fn assert(&self, cond: bool, on_err: impl FnOnce() -> CErr<'p>) -> Res<'p, ()> {
        if !cond {
            return Err(self.error(on_err()));
        }
        Ok(())
    }

    // TODO: make sure im `?` not dropping everywhere
    #[track_caller]
    fn assert_eq<T: PartialEq>(&self, a: &T, b: &T, on_err: impl Fn() -> CErr<'p>) -> Res<'p, ()> {
        self.assert(a == b, on_err)
    }

    #[track_caller]
    fn log_trace(&self) {
        logln!("=== TRACE ===");
        logln!("{}", Location::caller());
        let show_f = |func: FuncId| {
            format!(
                "f{}:{:?}:{}",
                func.0,
                self.program.funcs[func.0].get_name(self.pool),
                self.program.funcs[func.0].synth_name(self.pool)
            )
        };

        for (i, s) in self.debug_trace.iter().enumerate() {
            log!("{i}");
            match s {
                DebugState::OuterCall(f, arg) => {
                    log!("| Prep Interp | {} on val:{arg:?}", show_f(*f))
                }
                DebugState::JitToBc(f, when) => {
                    log!("| Jit Bytecode| {} for {:?}", show_f(*f), when)
                }
                DebugState::RunInstLoop(f) => log!("| Loop Insts  | {}", show_f(*f)),
                DebugState::ComputeCached(e) => log!("| Cache Eval  | {}", e.log(self.pool)),
                DebugState::ResolveFnType(f, arg, ret) => {
                    log!(
                        "| Resolve Type| {} is fn({}) {}",
                        show_f(*f),
                        arg.log(self.pool),
                        ret.log(self.pool)
                    )
                }
            }
            logln!(";")
        }
        logln!("=============");
    }

    pub fn add_declarations(&mut self, ast: Vec<Stmt<'p>>) -> Res<'p, ()> {
        for stmt in ast {
            match stmt {
                Stmt::DeclFunc(func) => {
                    self.program.add_func(func);
                }
                _ => panic!(
                    "Stmt {} is not supported at top level.",
                    stmt.log(self.pool)
                ),
            }
        }
        Ok(())
    }

    pub fn lookup_unique_func(&self, name: Ident<'p>) -> Option<FuncId> {
        self.program.declarations.get(&name).map(|decls| {
            assert_eq!(decls.len(), 1);
            decls[0]
        })
    }

    pub fn run(&mut self, f: FuncId, arg: Value, when: ExecTime) -> Res<'p, Value> {
        let state = DebugState::OuterCall(f, arg.clone());
        self.push_state(&state);
        let init_heights = (self.call_stack.len(), self.value_stack.len());

        // A fake callframe representing the calling rust program.
        let marker_callframe = CallFrame {
            stack_base: StackAbsolute(0),
            current_func: FuncId(0), // TODO: actually reserve 0 cause i do this a lot
            current_ip: 0,
            return_slot: StackAbsolute(0),
            return_count: 0,
            is_rust_marker: true, // used for exiting the run loop. this is the only place we set it true.
            debug_name: self.pool.intern("@interp::run@"),
            when,
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
        assert_ne!(result, Value::Poison);
        assert_eq!(self.value_stack.pop(), Some(Value::Poison));
        let final_callframe = self.call_stack.pop().unwrap();
        self.assert_eq(&final_callframe, &marker_callframe, || {
            CErr::Ice("bad frame")
        });
        let end_heights = (self.call_stack.len(), self.value_stack.len());
        self.assert(init_heights == end_heights, || CErr::Ice("bad stack size"));
        self.pop_state(state);
        Ok(result)
    }

    fn run_inst_loop(&mut self) -> Res<'p, ()> {
        let func = self.call_stack.last().unwrap().current_func;
        let state = DebugState::RunInstLoop(func);
        self.push_state(&state);
        loop {
            let i = self.next_inst();
            self.log_stack();
            logln!("I: {:?}", i);
            match i {
                &Bc::CallDirect { f, ret, arg } => {
                    // preincrement our ip because ret doesn't do it.
                    // this would be different if i was trying to do tail calls?
                    self.bump_ip();
                    let arg = self.take_slots(arg);
                    let when = self.call_stack.last().unwrap().when;
                    self.push_callframe(f, ret, arg, when)?;
                    self.log_callstack();
                    // don't bump ip here, we're in a new call frame.
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
                    self.log_callstack();
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
                    for _ in 0..size {
                        let value = self.value_stack.pop().unwrap();
                        debug_assert_eq!(value, Value::Poison)
                    }
                    // We don't increment the caller's ip, they did it on call.

                    self.log_callstack();
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
                    self.log_callstack();
                    // don't bump ip here, we're in a new call frame.
                }
                &Bc::CreateTuple { values, target } => {
                    let tuple = self.take_slots(values);
                    *self.get_slot_mut(target) = tuple;
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
                assert_eq!(values.len(), expected_count);
                let base = first.0;
                for (i, v) in values.into_iter().enumerate() {
                    // TODO: this needs to be a macro cause the cloning is a problem
                    self.assert_eq(&self.value_stack[base + i].clone(), &Value::Poison, || {
                        CErr::LeakedValue
                    });
                    self.value_stack[base + i] = v;
                }
            }
        }
        Ok(())
    }

    fn runtime_builtin(&mut self, name: &str, arg: Value) -> Res<'p, Value> {
        logln!("runtime_builtin: {name} {arg:?}");
        let value = match name {
            // Construct a tuple type from the arguments
            "Tuple" => {
                let types = self.to_seq(arg)?;
                let types: Res<'_, Vec<_>> = types.into_iter().map(|t| self.to_type(t)).collect();
                let ty = TypeInfo::Tuple(types?);
                let ty = self.program.intern_type(ty);
                Value::Type(ty)
            }
            "assert_eq" => {
                let (a, b) = self.split_to_pair(arg)?;
                assert_eq!(a, b, "runtime_builtin:assert_eq");
                self.assertion_count += 1; // sanity check for making sure tests actually ran
                Value::Unit
            }
            "is_comptime" => {
                assert_eq!(arg, Value::Unit);
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
                    assert!(data.references > 0);
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
                            assert_eq!(values.len(), addr.count);
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
                        if count == 1 {
                            ptr.values[first] = value;
                        } else {
                            let values = self.to_seq(value)?;
                            assert_eq!(values.len(), count);
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
            "Ptr" => Value::Type(self.program.intern_type(TypeInfo::Ptr(self.to_type(arg)?))),
            "is_oob_stack" => {
                let addr = self.to_stack_addr(arg)?;
                Value::Bool(addr.first.0 >= self.value_stack.len())
            }
            "slice" => {
                // last is not included
                let (addr, new_first, new_last) = self.to_triple(arg)?;
                let (new_first, new_last) = (self.to_int(new_first)?, self.to_int(new_last)?);
                assert!(new_first >= 0 && new_last >= 0 && new_first < new_last);
                match addr {
                    Value::InterpAbsStackAddr(addr) => {
                        assert!(
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
                assert!(count >= 0);
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
            "free" => {
                let (ty, count, ptr) = self.to_triple(arg)?;
                let (ty, count, (ptr, ptr_first, ptr_count)) = (
                    self.to_type(ty)?,
                    self.to_int(count)?,
                    self.to_heap_ptr(ptr)?,
                );
                let slots = count as usize * self.program.slot_count(ty);
                assert_eq!(ptr_first, 0);
                assert_eq!(ptr_count, slots);
                let ptr_val = unsafe { &*ptr };
                assert_eq!(ptr_val.references, 1);
                assert_eq!(ptr_val.values.len(), slots);
                let _ = unsafe { Box::from_raw(ptr) };
                Value::Unit
            }
            "print" => {
                println!("{:?}", arg);
                Value::Unit
            }
            _ => {
                if let Some(value) = runtime_builtin(name, arg.clone()) {
                    value
                } else {
                    return Err(self
                        .assert(false, || CErr::Ice("Known builtin is not implemented."))
                        .unwrap_err());
                }
            }
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
        self.ensure_compiled(f, when)?;
        assert!(
            self.ready[f.0].as_ref().is_some(),
            "ICE: ensure_compiled didn't work on {f:?}"
        );
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

    fn log_stack(&self) {
        log!("STACK ");
        let frame = self.call_stack.last().unwrap();
        for i in 0..frame.stack_base.0 {
            if self.value_stack[i] != Value::Poison {
                log!("({i}|{:?}), ", self.value_stack[i]);
            }
        }
        for i in frame.stack_base.0..self.value_stack.len() {
            if self.value_stack[i] != Value::Poison {
                let slot = i - frame.stack_base.0;
                log!("[{i}|${slot}|{:?}], ", self.value_stack[i]);
            }
        }
        logln!("END");
    }

    fn log_callstack(&self) {
        log!("CALLS ");
        for frame in &self.call_stack {
            log!("[{}], ", self.pool.get(frame.debug_name));
        }
        logln!("END");
    }

    fn bump_ip(&mut self) {
        let frame = self.call_stack.last_mut().unwrap();
        frame.current_ip += 1;
    }

    fn clone_slot(&self, slot: StackOffset) -> Value {
        let frame = self.call_stack.last().unwrap();
        let value = &self.value_stack[frame.stack_base.0 + slot.0];
        assert_ne!(value, &Value::Poison);
        value.clone()
    }

    fn take_slot(&mut self, slot: StackOffset) -> Value {
        let mut value = replace(self.get_slot_mut(slot), Value::Poison);
        assert_ne!(value, Value::Poison);
        value
    }

    /// If slot ranges over multiple, return them as a tuple.
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

    // why the fuck does result must use not warn me
    fn ensure_compiled(&mut self, FuncId(index): FuncId, when: ExecTime) -> Res<'p, ()> {
        if let Some(Some(_)) = self.ready.get(index) {
            return Ok(());
        }
        let state = DebugState::JitToBc(FuncId(index), when);
        self.push_state(&state);
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
        self.infer_types(FuncId(index));
        let mut func = self.program.funcs[index].clone();
        let (arg, ret) = func.ty.unwrap();
        let arg_slots = self.program.slot_count(arg);
        logln!("{:?} has arg {} slots", FuncId(index), arg_slots);
        assert_ne!(arg_slots, 0);
        let mut result = FnBody {
            insts: vec![],
            stack_slots: arg_slots,
            vars: Default::default(),
            arg_names: func.arg_names.clone(),
            when,
            slot_types: vec![TypeId::any(); arg_slots],
            func: FuncId(index),
        };
        let arg_range = StackRange {
            first: StackOffset(0),
            count: arg_slots,
        };
        let mut to_drop = vec![];
        if func.arg_names.len() == 1 {
            // if there's one name, it refers to the whole tuple.
            if let Some(name) = func.arg_names[0] {
                result.vars.insert(Var(name, 0), arg_range);
            } else {
                to_drop.push(arg_range);
            }
        } else if func.arg_names.len() == arg_slots {
            // if they match, each element has its own name.
            for (i, name) in func.arg_names.iter().enumerate() {
                let range = StackRange {
                    first: StackOffset(i),
                    count: 1,
                };
                if let Some(name) = func.arg_names[0] {
                    result.vars.insert(Var(name, 0), range);
                } else {
                    to_drop.push(range);
                }
            }
        } else {
            // TODO: pattern match destructuring but for now you just cant refer to the arg.
            to_drop.push(arg_range);
        }
        let body = func.body.as_ref().expect("fn body");
        let return_value = self.compile_expr(&mut result, body)?;

        // We're done with our arguments, get rid of them. Same for other vars.
        // TODO: once non-copy types are supported, this needs to get smarter because we might have moved out of our argument.
        for slot in result.vars.values() {
            result.insts.push(Bc::Drop(*slot));
        }
        for slot in to_drop {
            result.insts.push(Bc::Drop(slot));
        }

        result.insts.push(Bc::Ret(return_value));

        logln!("{:?}", result);
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

    pub fn write_jitted(&self) -> String {
        self.ready
            .iter()
            .zip(self.program.funcs.iter())
            .enumerate()
            .filter_map(|(i, (result, func))| {
                result
                    .as_ref()
                    .map(|result| format!("{}\n {:?}", func.log(self.pool), result))
            })
            .collect()
    }

    fn compile_stmt(&mut self, result: &mut FnBody<'p>, stmt: &Stmt<'p>) -> Res<'p, ()> {
        match stmt {
            Stmt::Eval(expr) => {
                let value = self.compile_expr(result, expr)?;
                result.push(Bc::Drop(value));
            }
            Stmt::DeclVar { name, ty, value } => {
                // TODO: scope.
                let value = invert(
                    value
                        .as_ref()
                        .map(|expr| self.compile_expr(result, expr.deref())),
                )?;
                let ty = invert(ty.clone().map(|ty| self.cached_eval_expr(ty)))?;
                let ty_slots = if let Some(ty) = ty {
                    Some(self.program.slot_count(self.to_type(ty)?))
                } else {
                    None
                };

                let value = match (value, ty_slots) {
                    (None, Some(slots)) => result.reserve_slots(slots, TypeId::any()),
                    (Some(value), None) => value,
                    (Some(value), Some(slots)) => {
                        if slots == value.count {
                            value
                        } else {
                            assert_eq!(value.count, 1);
                            let expanded = result.reserve_slots(slots, TypeId::any());
                            result.push(Bc::ExpandTuple {
                                from: value.first,
                                to: expanded,
                            });
                            expanded
                        }
                    }
                    // TODO: make this an error. dont guess.
                    (None, None) => result.reserve_slots(1, TypeId::any()),
                };
                let prev = result.vars.insert(Var(*name, 0), value);
                assert!(prev.is_none(), "TODO: redeclare and drop");
            }
            Stmt::SetNamed(name, expr) => {
                let value = self.compile_expr(result, expr)?;
                let slot = result.vars.get(&Var(*name, 0));
                let slot = *slot.expect("SetNamed: var must be declared");
                result.push(Bc::Drop(slot));
                assert_eq!(value.count, slot.count);
                for i in 0..value.count {
                    result.push(Bc::Move {
                        from: value.index(i),
                        to: slot.index(i),
                    });
                }
            }
            Stmt::SetNamed(_, _) => todo!(),
            Stmt::Noop => {}
            Stmt::DeclFunc(func) => {
                self.program.add_func(func.clone());
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
    fn compile_expr(&mut self, result: &mut FnBody<'p>, expr: &Expr<'p>) -> Res<'p, StackRange> {
        Ok(match expr {
            Expr::Closure(func) => {
                let id = self.program.add_func(*func.clone());
                self.ensure_compiled(id, result.when)?;
                let ty = self.program.intern_type(TypeInfo::Fn(FnType {
                    param: TypeId::any(),
                    returns: TypeId::any(),
                })); // TODO: infer here?
                result.load_constant(Value::GetFn(id), ty)
            }
            Expr::Call(f, arg) => {
                let arg = self.compile_expr(result, arg)?;

                if let Expr::GetNamed(i) = f.as_ref().deref() {
                    if let Some(f) = self.lookup_unique_func(*i) {
                        // Note: f might not be compiled yet, we'll find out the first time we try to call it.
                        // But, we do need to know how many values it returns. If there's no type annotation, this might end up compiling the function to figure it out.
                        self.infer_types(f);
                        let (arg_ty, ret_ty) = self.program.funcs[f.0].ty.unwrap();
                        if self.program.is_type(ret_ty, TypeInfo::Type) {
                            assert_eq!(
                                result.when,
                                ExecTime::Comptime,
                                "Cannot call function returning type at runtime."
                            );
                        }
                        let ret = result.reserve_slots(self.return_stack_slots(f), arg_ty);
                        result.insts.push(Bc::CallDirect { f, ret, arg });
                        return Ok(ret);
                    } else if self.builtins.contains(i) {
                        // TODO: this is ugly... other builtins might return tuples
                        let slots = if "tuple" == self.pool.get(*i) {
                            arg.count
                        } else {
                            1
                        };
                        let ret = result.reserve_slots(slots, TypeId::any());
                        result.insts.push(Bc::CallBuiltin { name: *i, ret, arg });
                        return Ok(ret);
                    } else if "if" == self.pool.get(*i) {
                        let unit = result
                            .load_constant(Value::Unit, self.program.intern_type(TypeInfo::Unit));
                        // TODO: if returning tuples
                        let ret = result.reserve_slots(1, TypeId::any());
                        assert_eq!(arg.count, 3);
                        let branch_ip = result.insts.len();
                        let true_ip = branch_ip + 1;
                        let false_ip = branch_ip + 4;
                        result.insts.push(Bc::JumpIf {
                            // TODO: change to conditional so dont have to store the true_ip
                            cond: arg.index(0),
                            true_ip,
                            false_ip,
                        });
                        let real_true_ip = result.push(Bc::CallDynamic {
                            f: arg.index(1),
                            ret,
                            arg: unit,
                        });
                        result.push(Bc::Drop(arg.index(2).to_range()));
                        result.push(Bc::Goto { ip: branch_ip + 6 });
                        let real_false_ip = result.push(Bc::CallDynamic {
                            f: arg.index(2),
                            ret,
                            arg: unit,
                        });
                        result.push(Bc::Drop(arg.index(1).to_range()));
                        assert_eq!(true_ip, real_true_ip);
                        assert_eq!(false_ip, real_false_ip);
                        return Ok(ret);
                    }
                    // else: fallthrough
                }

                logln!("dynamic {}", f.log(self.pool));
                let f = self.compile_expr(result, f)?;
                assert_eq!(f.count, 1);
                // TODO: not all function ptrs have 1 return value
                let ret = result.reserve_slots(1, TypeId::any());
                result.insts.push(Bc::CallDynamic {
                    f: f.first,
                    ret,
                    arg,
                });
                ret
            }
            Expr::Block(prelude, value) => {
                for stmt in prelude {
                    self.compile_stmt(result, stmt)?;
                }
                self.compile_expr(result, value)?
            }
            Expr::IfElse(_, _, _) => todo!(),
            Expr::Array(_) => todo!(),
            Expr::Tuple(values) => {
                let values: Res<'p, Vec<_>> = values
                    .iter()
                    .map(|v| self.compile_expr(result, v))
                    .collect();
                let values = values?;
                let required_slots: usize = values.iter().map(|range| range.count).sum();
                let ret = result.reserve_slots(required_slots, TypeId::any());
                // TODO: they might already be consecutive
                let base = ret.first.0;
                let mut count = 0;
                for v in values {
                    for i in 0..v.count {
                        result.insts.push(Bc::Move {
                            from: StackOffset(v.first.0 + i),
                            to: StackOffset(base + count),
                        });
                        count += 1;
                    }
                }
                assert_eq!(count, ret.count);
                ret
            }
            Expr::RefType(_) => todo!(),
            Expr::GetVar(v) => todo!(),
            Expr::GetNamed(i) => {
                if let Some((value, ty)) = self.builtin_constant(self.pool.get(*i)) {
                    result.load_constant(value, ty)
                } else if let Some(func) = self.program.declarations.get(i) {
                    assert_eq!(func.len(), 1, "ambigous function reference");
                    let func = func[0];
                    self.ensure_compiled(func, ExecTime::Comptime)?;
                    result.load_constant(Value::GetFn(func), TypeId::any())
                } else if let Some(index) = result.vars.get(&Var(*i, 0)) {
                    let from = *index;
                    let to = result.reserve_slots(from.count, TypeId::any());
                    if from.count == 1 {
                        result.insts.push(Bc::Clone {
                            from: from.first,
                            to: to.first,
                        });
                    } else {
                        result.insts.push(Bc::CloneRange { from, to });
                    }
                    to
                } else {
                    logln!("UNDECLARED IDENT: {} (in GetNamed)", self.pool.get(*i));
                    return Err(self.error(CErr::UndeclaredIdent(*i)));
                }
            }
            Expr::EnumLiteral(_) => todo!(),
            Expr::StructLiteral(_) => todo!(),
            Expr::Value(value) => {
                let to = result.reserve_slots(1, TypeId::any());
                result.insts.push(Bc::LoadConstant {
                    slot: to.first,
                    value: value.clone(),
                });
                to
            }
            Expr::SuffixMacro(macro_name, arg) => {
                let name = self.pool.get(*macro_name);
                match name {
                    "addr" => {
                        match arg.deref().deref() {
                            // TODO: copy-n-paste
                            Expr::GetNamed(i) => {
                                let stack_slot = if let Some(index) = result.vars.get(&Var(*i, 0)) {
                                    *index
                                } else {
                                    logln!(
                                        "UNDECLARED IDENT: {} (in SuffixMacro::addr)",
                                        self.pool.get(*i)
                                    );
                                    return Err(self.error(CErr::UndeclaredIdent(*i)));
                                };

                                let addr_slot = result.reserve_slots(1, TypeId::any());
                                result.push(Bc::AbsoluteStackAddr {
                                    of: stack_slot,
                                    to: addr_slot.first,
                                });
                                addr_slot
                            }
                            _ => return Err(self.error(CErr::AddrRvalue(*arg.clone()))),
                        }
                    }
                    _ => return Err(self.error(CErr::UndeclaredIdent(*macro_name))),
                }
            }
        })
    }

    fn builtin_constant(&mut self, name: &str) -> Option<(Value, TypeId)> {
        if let Some(ty) = builtin_type(name) {
            let ty = self.program.intern_type(ty);
            let tyty = self.program.intern_type(TypeInfo::Type);
            return Some((Value::Type(ty), tyty));
        }

        Some(match name {
            "unit" => (Value::Unit, TypeId::unit()),
            "true" => (Value::Bool(true), self.program.intern_type(TypeInfo::Bool)),
            "false" => (Value::Bool(false), self.program.intern_type(TypeInfo::Bool)),
            _ => return None,
        })
    }

    fn next_inst(&self) -> &Bc {
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
    fn infer_types(&mut self, func: FuncId) -> Res<'p, ()> {
        match self.program.funcs[func.0].ty.clone() {
            LazyFnType::Pending { arg, ret } => {
                let state = DebugState::ResolveFnType(func, arg.clone(), ret.clone());
                self.push_state(&state);
                logln!("RESOLVE: Ret of {func:?}");
                let ret = match ret {
                    LazyType::Infer => todo!(),
                    LazyType::PendingEval(e) => {
                        let value = self.cached_eval_expr(e.clone())?;
                        self.to_type(value)?
                    }
                    LazyType::Finished(id) => id, // easy
                };
                // TODO: copy-n-paste
                logln!("RESOLVE: Arg of {func:?}");
                let arg = match arg {
                    LazyType::Infer => todo!(),
                    LazyType::PendingEval(e) => {
                        let value = self.cached_eval_expr(e.clone())?;
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

    // TODO: fast path for builtin type identifiers
    fn cached_eval_expr(&mut self, e: FatExpr<'p>) -> Res<'p, Value> {
        if let Some(old_result) = self.comptime_cache.get(&e) {
            logln!("CACHED: {} -> {:?}", e.log(self.pool), old_result);
            return Ok(old_result.clone());
        }
        let state = DebugState::ComputeCached(e.clone());
        self.push_state(&state);
        let name = format!("@eval_{}@", self.anon_fn_counter);
        let fake_func: Func<'p> = Func {
            name: Some(self.pool.intern(&name)),
            ty: self.unit_to_type(),
            body: Some(e.clone()),
            arg_names: vec![None],
            annotations: vec![],
        };
        self.anon_fn_counter += 1;
        let func_id = self.program.add_func(fake_func);
        logln!(
            "Made anon: {func_id:?} = {}",
            self.program.funcs[func_id.0].log(self.pool)
        );
        let result = self.run(func_id, Value::Unit, ExecTime::Comptime)?;
        logln!(
            "COMPUTED: {} -> {:?} under {}",
            e.log(self.pool),
            result,
            self.program.funcs[func_id.0].log(self.pool)
        );
        self.comptime_cache.insert(e, result.clone());
        self.pop_state(state);
        Ok(result)
    }

    #[track_caller]
    fn to_type(&self, value: Value) -> Res<'p, TypeId> {
        if let Value::Type(id) = value {
            Ok(id)
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
        assert_eq!(values.len(), 3);
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
        assert_eq!(values.len(), 2);
        Ok((values[0].clone(), values[1].clone()))
    }

    #[track_caller]
    fn split_to_pair(&self, value: Value) -> Res<'p, (Value, Value)> {
        let values = to_flat_seq(value);

        // TODO: sad cloning
        if values.len() == 2 {
            return Ok((values[0].clone(), values[1].clone()));
        }

        assert_eq!(values.len() % 2, 0);
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
}

// TODO: macros for each builtin arg type cause this sucks.
fn load_int_pair(v: Value) -> (i64, i64) {
    match v {
        Value::Tuple {
            container_type,
            mut values,
        } => {
            assert_eq!(values.len(), 2, "load_int_pair wrong arity");
            let a = replace(&mut values[0], Value::Poison);
            let b = replace(&mut values[1], Value::Poison);
            (load_int(a), load_int(b))
        }
        v => panic!("load_int_pair {:?}", v),
    }
}

fn load_int(v: Value) -> i64 {
    match v {
        Value::I64(i) => i,
        v => panic!("load_int {:?}", v),
    }
}

impl Debug for StackOffset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.0)
    }
}

impl Debug for StackRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let args: Vec<_> = (self.first.0..(self.first.0 + self.count))
            .map(|i| format!("${}", i))
            .collect();
        let args = args.join(", ");
        write!(f, "({args})")
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ExecTime {
    Comptime,
    Runtime,
}

impl Debug for FnBody<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "=== Bytecode for {:?} at {:?} ===", self.func, self.when)?;
        writeln!(f, "TYPES: {:?}", &self.slot_types);
        for (i, bc) in self.insts.iter().enumerate() {
            writeln!(f, "{i}. {bc:?}");
        }
        writeln!(f, "===============")?;
        Ok(())
    }
}

impl Debug for Bc<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Bc::CallDynamic {
                f: func_slot,
                ret,
                arg,
            } => write!(f, "{ret:?} = call({func_slot:?}, {arg:?});")?,
            Bc::CallDirect { f: func, ret, arg } => {
                write!(f, "{ret:?} = call(f({:?}), {arg:?});", func.0)?
            }
            Bc::CallBuiltin { name, ret, arg } => {
                write!(f, "{ret:?} = builtin(i({}), {arg:?});", name.0)?
            }
            Bc::LoadConstant { slot, value } => write!(f, "{:?} = {:?};", slot, value)?,
            Bc::JumpIf {
                cond,
                true_ip,
                false_ip,
            } => write!(
                f,
                "if ({:?}) goto {} else goto {};",
                cond, true_ip, false_ip
            )?,
            Bc::Goto { ip } => write!(f, "goto {ip};",)?,
            Bc::CreateTuple { values, target } => {
                write!(f, "{target:?} = move{values:?};")?;
            }
            Bc::Ret(i) => write!(f, "return {i:?};")?,
            Bc::Clone { from, to } => write!(f, "{:?} = @clone({:?});", to, from)?,
            Bc::CloneRange { from, to } => write!(f, "{:?} = @clone({:?});", to, from)?,
            Bc::Move { from, to } => write!(f, "{:?} = move({:?});", to, from)?,
            Bc::ExpandTuple { from, to } => write!(f, "{:?} = move({:?});", to, from)?,
            Bc::Drop(i) => write!(f, "drop({:?});", i)?,
            Bc::AbsoluteStackAddr { of, to } => write!(f, "{:?} = @addr({:?});", to, of)?,
        }
        Ok(())
    }
}

// These are normal functions that don't need to do scary things to the compilation context.
fn runtime_builtin(name: &str, arg: Value) -> Option<Value> {
    let v = match name {
        "add" => bin_int!(+, arg, Value::I64),
        "sub" => bin_int!(-, arg, Value::I64),
        "mul" => bin_int!(*, arg, Value::I64),
        "div" => bin_int!(/, arg, Value::I64),
        "eq" => bin_int!(==, arg, Value::Bool),
        "ne" => bin_int!(!=, arg, Value::Bool),
        "gt" => bin_int!(>, arg, Value::Bool),
        "lt" => bin_int!(<, arg, Value::Bool),
        "ge" => bin_int!(>=, arg, Value::Bool),
        "le" => bin_int!(<=, arg, Value::Bool),
        "tuple" => {
            // This will become the `(a, b, c)` syntax.
            // It just gives you a way to express that you want to pass multiple things at once.
            // TODO: this is really inefficient. it boxes them from the stack and then writes them all back again?
            arg
        }
        _ => return None,
    };
    Some(v)
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
        self.insts.push(Bc::LoadConstant {
            slot: to.first,
            value,
        });
        to
    }
}

fn builtin_type(name: &str) -> Option<TypeInfo> {
    use TypeInfo::*;
    Some(match name {
        "Unit" => Unit,
        "i64" => I64,
        "f64" => F64,
        "Type" => Type,
        "bool" => Bool,
        _ => return None,
    })
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
