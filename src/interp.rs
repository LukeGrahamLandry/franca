#![allow(clippy::wrong_self_convention)]
use std::{
    collections::HashMap,
    fmt::{format, Debug},
    mem::replace,
    panic::Location,
};

use crate::{
    ast::{Expr, FnType, Func, FuncId, LazyFnType, LazyType, Program, Stmt, TypeId, TypeInfo, Var},
    pool::{Ident, StringPool},
};

macro_rules! bin_int {
    ($op:tt, $arg:expr, $res:expr) => {{
        let (a, b) = load_int_pair($arg);
        $res(a $op b)
    }};
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

#[derive(Debug, Copy, Clone, PartialEq)]
struct StackAbsolute(usize);

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
    Move {
        from: StackOffset,
        to: StackOffset,
    },
    Drop(StackRange),
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
    comptime_cache: HashMap<Expr<'p>, Value>,
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
    ComputeCached(Expr<'p>),
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
        println!("=== TRACE ===");
        println!("{}", Location::caller());
        let show_f = |func: FuncId| {
            format!(
                "f{}:{:?}:{}",
                func.0,
                self.program.funcs[func.0].get_name(self.pool),
                self.program.funcs[func.0].synth_name(self.pool)
            )
        };

        for (i, s) in self.debug_trace.iter().enumerate() {
            print!("{i}");
            match s {
                DebugState::OuterCall(f, arg) => {
                    print!("| Prep Interp | {} on val:{arg:?}", show_f(*f))
                }
                DebugState::JitToBc(f, when) => {
                    print!("| Jit Bytecode| {} for {:?}", show_f(*f), when)
                }
                DebugState::RunInstLoop(f) => print!("| Loop Insts  | {}", show_f(*f)),
                DebugState::ComputeCached(e) => print!("| Cache Eval  | {}", e.log(self.pool)),
            }
            println!(";")
        }
        println!("=============");
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
            count: 1,
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
            println!("I: {:?}", i);
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
                    *self.get_slot_mut(ret.first) = value;
                    self.bump_ip();
                }
                &Bc::Ret(slot) => {
                    self.log_callstack();
                    let value = self.take_slots(slot);
                    let frame = self.call_stack.pop().unwrap();
                    println!(
                        "return from {:?} --- {:?} --- to {:?} count:{}",
                        slot, value, frame.return_slot, frame.return_count
                    );

                    match frame.return_count.cmp(&1) {
                        std::cmp::Ordering::Equal => {
                            self.value_stack[frame.return_slot.0] = value;
                        }
                        std::cmp::Ordering::Less => {
                            todo!("zero argument return. probably just works but untested.")
                        }
                        std::cmp::Ordering::Greater => {
                            let values = self.to_seq(value)?;
                            assert_eq!(values.len(), frame.return_count);
                            let base = frame.return_slot.0;
                            for (i, v) in values.into_iter().enumerate() {
                                // TODO: this needs to be a macro cause the cloning is a problem
                                self.assert_eq(
                                    &self.value_stack[base + i].clone(),
                                    &Value::Poison,
                                    || CErr::LeakedValue,
                                );
                                self.value_stack[base + i] = v;
                            }
                        }
                    }

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
                    let f = self.to_func(f);
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
                &Bc::Drop(slot) => {
                    assert_ne!(slot.count, 0);
                    for i in 0..slot.count {
                        let _ = self.take_slot(slot.index(i));
                    }
                    self.bump_ip();
                }
            }
        }
        self.pop_state(state);
        Ok(())
    }

    fn runtime_builtin(&mut self, name: &str, arg: Value) -> Res<'p, Value> {
        println!("runtime_builtin: {name} {arg:?}");
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
                let (a, b) = self.to_pair(arg)?;
                assert_eq!(a, b, "runtime_builtin:assert_eq");
                self.assertion_count += 1; // sanity check for making sure tests actually ran
                Value::Unit
            }
            "is_comptime" => {
                assert_eq!(arg, Value::Unit);
                let when = self.call_stack.last().unwrap().when;
                Value::Bool(when == ExecTime::Comptime)
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
        print!("STACK ");
        let frame = self.call_stack.last().unwrap();
        for i in 0..frame.stack_base.0 {
            if self.value_stack[i] != Value::Poison {
                print!("({i}|{:?}), ", self.value_stack[i]);
            }
        }
        for i in frame.stack_base.0..self.value_stack.len() {
            if self.value_stack[i] != Value::Poison {
                let slot = i - frame.stack_base.0;
                print!("[{i}|${slot}|{:?}], ", self.value_stack[i]);
            }
        }
        println!("END");
    }

    fn log_callstack(&self) {
        print!("CALLS ");
        for frame in &self.call_stack {
            print!("[{}], ", self.pool.get(frame.debug_name));
        }
        println!("END");
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

        let mut func = self.program.funcs[index].clone();
        self.log_depth += 1;
        println!(
            "{} Start JIT: {:?} {}",
            "=".repeat(self.log_depth),
            FuncId(index),
            func.synth_name(self.pool)
        );
        println!("AST:");
        println!("{:?}", func.body.as_ref().map(|b| b.log(self.pool)));
        self.infer_types(FuncId(index));
        let mut result = FnBody {
            insts: vec![],
            stack_slots: func.arg_names.len(),
            vars: Default::default(),
            arg_names: func.arg_names.clone(),
            when,
            slot_types: vec![TypeId::any(); func.arg_names.len()],
            func: FuncId(index),
        };
        println!("{:?}", func);
        let body = func.body.as_ref().expect("fn body");
        let return_value = self.compile_expr(&mut result, body)?;

        // We're done with our arguments, get rid of them.
        // TODO: once non-copy types are supported, this needs to get smarter because we might have moved out of our argument.
        if !result.arg_names.is_empty() {
            result.insts.push(Bc::Drop(StackRange {
                first: StackOffset(0),
                count: result.arg_names.len(),
            }));
        }

        for slot in result.vars.values() {
            result.insts.push(Bc::Drop(*slot));
        }

        result.insts.push(Bc::Ret(return_value));

        println!("{:?}", result);
        self.ready[index] = Some(result);
        println!(
            "{} Done JIT: {:?} {}",
            "=".repeat(self.log_depth),
            FuncId(index),
            func.synth_name(self.pool)
        );
        self.log_depth -= 1;
        self.pop_state(state);
        Ok(())
    }

    pub fn write_jitted(&self) -> Vec<String> {
        self.ready
            .iter()
            .enumerate()
            .filter_map(|(i, result)| result.as_ref().map(|result| format!("{:?}", result)))
            .collect()
    }

    fn compile_stmt(&mut self, result: &mut FnBody<'p>, stmt: &Stmt<'p>) -> Res<'p, ()> {
        match stmt {
            Stmt::Eval(expr) => {
                let value = self.compile_expr(result, expr)?;
                result.push(Bc::Drop(value));
            }
            Stmt::DeclVar(name, expr) => {
                // TODO: scope.
                let value = self.compile_expr(result, expr)?;
                assert_eq!(value.count, 1, "TODO: tuple variables");
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

                if let Expr::GetNamed(i) = f.as_ref() {
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
                        // TODO: not all builtins have 1 return value
                        let ret = result.reserve_slots(1, TypeId::any());
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

                println!("dynamic {}", f.log(self.pool));
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
            Expr::GetVar(v) => {
                let ret = result.reserve_slots(1, TypeId::any());
                result.push(Bc::Clone {
                    from: result.vars.get(v).unwrap().first,
                    to: ret.first,
                });
                ret
            }
            Expr::GetNamed(i) => {
                if let Some(index) = result
                    .arg_names
                    .iter()
                    .flatten()
                    .position(|check| check == i)
                {
                    // TODO: call the right copy function. dont have redundant instructions for coyping
                    //       but it makes interp easier to debug if you always move when consuming
                    // Function arguments are at the beginning of our stack.
                    // They might be read more than once, so need to copy the value (interp uses linear types).
                    let to = result.reserve_slots(1, TypeId::any());
                    result.insts.push(Bc::Clone {
                        from: StackOffset(index),
                        to: to.first,
                    });
                    to
                } else if let Some((value, ty)) = self.builtin_constant(self.pool.get(*i)) {
                    result.load_constant(value, ty)
                } else if let Some(func) = self.program.declarations.get(i) {
                    assert_eq!(func.len(), 1, "ambigous function reference");
                    let func = func[0];
                    self.ensure_compiled(func, ExecTime::Comptime)?;
                    result.load_constant(Value::GetFn(func), TypeId::any())
                } else if let Some(index) = result.vars.get(&Var(*i, 0)) {
                    let from = *index;
                    assert_eq!(index.count, 1);
                    let to = result.reserve_slots(1, TypeId::any());
                    result.insts.push(Bc::Clone {
                        from: from.first,
                        to: to.first,
                    });
                    to
                } else {
                    println!("UNDECLARED IDENT: {}", self.pool.get(*i));
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
        })
    }

    fn builtin_constant(&mut self, name: &str) -> Option<(Value, TypeId)> {
        if let Some(ty) = builtin_type(name) {
            let ty = self.program.intern_type(ty);
            let tyty = self.program.intern_type(TypeInfo::Type);
            return Some((Value::Type(ty), tyty));
        }

        Some(match name {
            "unit" => (Value::Unit, self.program.intern_type(TypeInfo::Unit)),
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
        body.as_ref().unwrap_or_else(|| {
            self.write_jitted().iter().for_each(|f| println!("{}", f));
            self.log_trace();
            self.log_callstack();
            panic!("Forgot to jit current function? Forgot a return instruction?")
        })
    }

    // Resolve the lazy types for Arg and Ret
    fn infer_types(&mut self, func: FuncId) -> Res<'p, ()> {
        match self.program.funcs[func.0].ty.clone() {
            LazyFnType::Pending { arg, ret } => {
                println!("RESOLVE: Ret of {func:?}");
                let ret = match ret {
                    LazyType::Infer => todo!(),
                    LazyType::PendingEval(e) => {
                        let value = self.cached_eval_expr(e.clone())?;
                        self.to_type(value)?
                    }
                    LazyType::Finished(id) => id, // easy
                };
                // TODO: copy-n-paste
                println!("RESOLVE: Arg of {func:?}");
                let arg = match arg {
                    LazyType::Infer => todo!(),
                    LazyType::PendingEval(e) => {
                        let value = self.cached_eval_expr(e.clone())?;
                        self.to_type(value)?
                    }
                    LazyType::Finished(id) => id, // easy
                };
                self.program.funcs[func.0].ty = LazyFnType::Finished(arg, ret);
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
    fn cached_eval_expr(&mut self, e: Expr<'p>) -> Res<'p, Value> {
        if let Some(old_result) = self.comptime_cache.get(&e) {
            println!("CACHED: {} -> {:?}", e.log(self.pool), old_result);
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
        println!("Made anon: {func_id:?} = {name}");
        let result = self.run(func_id, Value::Unit, ExecTime::Comptime)?;
        println!("COMPUTED: {} -> {:?}", e.log(self.pool), result);
        self.comptime_cache.insert(e, result.clone());
        self.pop_state(state);
        Ok(result)
    }

    fn to_type(&self, value: Value) -> Res<'static, TypeId> {
        if let Value::Type(id) = value {
            Ok(id)
        } else {
            panic!("Expected Type found {:?}", value)
        }
    }

    fn to_bool(&self, value: Value) -> Res<'static, bool> {
        if let Value::Bool(v) = value {
            Ok(v)
        } else {
            panic!("Expected Bool found {:?}", value)
        }
    }

    fn to_seq(&self, value: Value) -> Res<'static, Vec<Value>> {
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
            _ => panic!("Expected Type found {:?}", value),
        }
    }

    fn to_triple(&self, value: Value) -> Res<'static, (Value, Value, Value)> {
        let values = self.to_seq(value)?;
        assert_eq!(values.len(), 3);
        Ok((values[0].clone(), values[1].clone(), values[2].clone()))
    }

    fn to_func(&self, value: Value) -> FuncId {
        if let Value::GetFn(id) = value {
            id
        } else {
            panic!("Expected Func found {:?}", value)
        }
    }

    fn to_pair(&self, value: Value) -> Res<'static, (Value, Value)> {
        let values = self.to_seq(value)?;
        assert_eq!(values.len(), 2);
        Ok((values[0].clone(), values[1].clone()))
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
                write!(f, "{target:?} = tuple{values:?};")?;
            }
            Bc::Ret(i) => write!(f, "return {i:?};")?,
            Bc::Clone { from, to } => write!(f, "{:?} = @clone({:?});", to, from)?,
            Bc::Move { from, to } => write!(f, "{:?} = move({:?});", to, from)?,
            Bc::Drop(i) => write!(f, "drop({:?});", i)?,
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
