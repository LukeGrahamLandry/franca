use std::{
    collections::HashMap,
    fmt::{format, Debug},
    mem::replace,
};

use crate::{
    ast::{Expr, Func, FuncId, LazyFnType, LazyType, Program, Stmt, TypeId, TypeInfo, Var},
    pool::{Ident, StringPool},
};

macro_rules! bin_int {
    ($op:tt, $arg:expr, $res:expr) => {{
        let (a, b) = load_int_pair($arg);
        $res(a $op b)
    }};
}

#[derive(Clone, PartialEq, Debug)]
pub enum Value {
    F64(f64),
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
impl Value {
    fn unwrap_type(&self) -> TypeId {
        if let &Value::Type(id) = self {
            id
        } else {
            panic!("Expected Type found {:?}", self)
        }
    }
}

#[derive(Copy, Clone)]
struct StackOffset(usize);

#[derive(Debug, Copy, Clone, PartialEq)]
struct StackAbsolute(usize);

#[derive(Copy, Clone)]
struct StackRange {
    first: StackOffset,
    count: usize,
}

// TODO: smaller index sizes so can pack these?
#[derive(Debug)]
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
struct CallFrame {
    stack_base: StackAbsolute,
    current_func: FuncId,
    current_ip: usize,
    return_slot: StackAbsolute,
    return_count: usize,
    is_rust_marker: bool,
}

struct FnBody<'p> {
    insts: Vec<Bc<'p>>,
    stack_slots: usize,
    vars: HashMap<Var, StackOffset>, // TODO: use a vec
    arg_names: Vec<Option<Ident<'p>>>,
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
    call_stack: Vec<CallFrame>,
    program: &'a mut Program<'p>,
    ready: Vec<Option<FnBody<'p>>>,
    builtins: Vec<Ident<'p>>,
    log_depth: usize,
}

// TODO: rn these eat any calls. no overload checking.
// TODO: always put these at start of pool so can use indexes without hashmap lookup
//       IMPORTANT: interp should probably made the pool if i do that.
const BUILTINS: &[&str] = &[
    "add", "sub", "mul", "div", "eq", "ne", "lt", "gt", "ge", "le",
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
        }
    }

    pub fn add_declarations(&mut self, ast: Vec<Stmt<'p>>) {
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
    }

    pub fn lookup_unique_func(&self, name: Ident<'p>) -> Option<FuncId> {
        self.program.declarations.get(&name).map(|decls| {
            assert_eq!(decls.len(), 1);
            decls[0]
        })
    }

    pub fn run(&mut self, f: FuncId, arg: Value) -> Value {
        let init_heights = (self.call_stack.len(), self.value_stack.len());

        // A fake callframe representing the calling rust program.
        let marker_callframe = CallFrame {
            stack_base: StackAbsolute(0),
            current_func: FuncId(0), // TODO: actually reserve 0 cause i do this a lot
            current_ip: 0,
            return_slot: StackAbsolute(0),
            return_count: 0,
            is_rust_marker: true, // used for exiting the run loop. this is the only place we set it true.
        };
        self.call_stack.push(marker_callframe);
        // TODO: typecheck
        self.value_stack.push(Value::Poison); // For the return value.
        let ret = StackRange {
            first: StackOffset(0),
            count: 1,
        };

        // Call the function
        self.push_callframe(f, ret, arg);
        self.run_inst_loop();

        // Give the return value to the caller.
        let result = self.take_slots(ret);

        // Sanity checks that we didn't mess anything up for the next guy.
        assert_ne!(result, Value::Poison);
        assert_eq!(self.value_stack.pop(), Some(Value::Poison));
        let final_callframe = self.call_stack.pop().unwrap();
        assert_eq!(final_callframe, marker_callframe);
        let end_heights = (self.call_stack.len(), self.value_stack.len());
        assert_eq!(init_heights, end_heights, "bad stack size");

        result
    }

    fn run_inst_loop(&mut self) {
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
                    self.push_callframe(f, ret, arg);

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
                    // TODO: use functions but then two frame lookups
                    let frame = self.call_stack.last_mut().unwrap();
                    let slot = frame.stack_base.0 + cond.0;
                    if let Value::Bool(cond) = self.value_stack[slot] {
                        let next_ip = if cond { true_ip } else { false_ip };
                        frame.current_ip = next_ip;
                    } else {
                        panic!("ICE: JumpIf on {:?}", cond)
                    }
                }
                &Bc::CallBuiltin { name, ret, arg } => {
                    let name = self.pool.get(name);
                    // Calling Convention: arguments passed to a function are moved out of your stack.
                    let arg = self.take_slots(arg);
                    let value = if let Some(value) = runtime_builtin(name, arg) {
                        value
                    } else {
                        panic!("Known builtin {:?} is not implemented.", name);
                    };
                    *self.get_slot_mut(ret.first) = value;
                    self.bump_ip();
                }
                &Bc::Ret(slot) => {
                    let value = self.take_slots(slot);
                    let frame = self.call_stack.pop().unwrap();
                    self.value_stack[frame.return_slot.0] = value;
                    // Release our stack space.
                    let size = self.value_stack.len() - frame.stack_base.0;
                    for _ in 0..size {
                        let value = self.value_stack.pop().unwrap();
                        debug_assert_eq!(value, Value::Poison)
                    }
                    // We don't increment the caller's ip, they did it on call.

                    if self.call_stack.last().unwrap().is_rust_marker {
                        break;
                    }
                }
                Bc::CallDynamic { .. } => todo!(), // Dont for get to inc ip first since ret doesn't
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
                    for i in slot.first.0..slot.count {
                        let _ = self.take_slot(StackOffset(i));
                    }
                    self.bump_ip();
                }
            }
        }
    }

    fn push_callframe(&mut self, f: FuncId, ret: StackRange, arg: Value) {
        self.ensure_compiled(f);
        // Calling Convention: arguments passed to a function are moved out of your stack.
        assert_eq!(ret.count, 1);
        let return_slot = self.slot_to_index(ret.first);
        self.value_stack.push(arg); // TODO: what if its a tuples
        self.call_stack.push(CallFrame {
            stack_base: StackAbsolute(self.value_stack.len() - 1), // Our stack includes the argument but not the return slot.
            current_func: f,
            current_ip: 0,
            return_slot,
            return_count: ret.count,
            is_rust_marker: false,
        });
        let empty = self.current_fn_body().stack_slots;
        for _ in 0..empty {
            self.value_stack.push(Value::Poison);
        }
    }

    fn log_stack(&self) {
        print!("|");
        let frame = self.call_stack.last().unwrap();
        for i in 0..frame.stack_base.0 {
            print!("({:?}), ", self.value_stack[i]);
        }
        for i in frame.stack_base.0..self.value_stack.len() {
            print!("[{:?}], ", self.value_stack[i]);
        }
        println!("|");
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
                container_type: TypeId(0), // TODO
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

    fn ensure_compiled(&mut self, FuncId(index): FuncId) {
        if let Some(Some(_)) = self.ready.get(index) {
            return;
        }
        while self.ready.len() <= index {
            self.ready.push(None);
        }

        let mut func = self.program.funcs[index].clone();
        self.log_depth += 1;
        println!(
            "{} Start JIT: {}",
            "=".repeat(self.log_depth),
            self.pool.get(func.name)
        );
        println!("AST:");
        println!("{:?}", func.body.as_ref().map(|b| b.log(self.pool)));
        self.infer_types(FuncId(index));
        let mut result = FnBody {
            insts: vec![],
            stack_slots: func.arg_names.len(),
            vars: Default::default(),
            arg_names: func.arg_names.clone(),
        };
        let return_value = self.compile_expr(&mut result, func.body.as_ref().unwrap());

        // We're done with our arguments, get rid of them.
        // TODO: once non-copy types are supported, this needs to get smarter because we might have moved out of our argument.
        result.insts.push(Bc::Drop(StackRange {
            first: StackOffset(0),
            count: result.arg_names.len(),
        }));

        result.insts.push(Bc::Ret(return_value));

        println!("{:?}", result);
        self.ready[index] = Some(result);
        println!(
            "{} Done JIT: {}",
            "=".repeat(self.log_depth),
            self.pool.get(func.name)
        );
        self.log_depth -= 1;
    }

    fn compile_stmt(&mut self, expr: Stmt) {
        todo!()
    }

    fn return_stack_slots(&mut self, f: FuncId) -> usize {
        self.infer_types(f);
        let func = &self.program.funcs[f.0];
        let (_, ret) = func.ty.unwrap();
        self.program.slot_count(ret)
    }

    fn compile_expr(&mut self, result: &mut FnBody<'p>, expr: &Expr<'p>) -> StackRange {
        match expr {
            Expr::Num(_) => todo!(),
            Expr::Call(f, arg) => {
                let arg = self.compile_expr(result, arg);
                if let Expr::GetNamed(i) = f.as_ref() {
                    if let Some(f) = self.lookup_unique_func(*i) {
                        // Note: f might not be compiled yet, we'll find out the first time we try to call it.
                        let ret = result.reserve_slots(self.return_stack_slots(f));
                        result.insts.push(Bc::CallDirect { f, ret, arg });
                        return ret;
                    } else if self.builtins.contains(i) {
                        // TODO: not all builtins have 1 return value
                        let ret = result.reserve_slots(1);
                        result.insts.push(Bc::CallBuiltin { name: *i, ret, arg });
                        return ret;
                    }
                    // else: fallthrough
                }

                println!("dynamic {}", f.log(self.pool));
                let f = self.compile_expr(result, f);
                assert_eq!(f.count, 1);
                // TODO: not all function ptrs have 1 return value
                let ret = result.reserve_slots(1);
                result.insts.push(Bc::CallDynamic {
                    f: f.first,
                    ret,
                    arg,
                });
                ret
            }
            Expr::Block(_, _) => todo!(),
            Expr::IfElse(_, _, _) => todo!(),
            Expr::Array(_) => todo!(),
            Expr::Tuple(values) => {
                let values: Vec<_> = values
                    .iter()
                    .map(|v| self.compile_expr(result, v))
                    .collect();
                let ret = result.reserve_slots(values.len());
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
            Expr::GetVar(v) => StackRange {
                // TODO clone
                first: *result.vars.get(v).unwrap(),
                count: 1,
            },
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
                    let to = result.reserve_slots(1);
                    result.insts.push(Bc::Clone {
                        from: StackOffset(index),
                        to: to.first,
                    });
                    to
                } else if let Some(ty) = builtin_type(self.pool.get(*i)) {
                    let to = result.reserve_slots(1);
                    let ty = self.program.intern_type(ty);
                    result.insts.push(Bc::LoadConstant {
                        slot: to.first,
                        value: Value::Type(ty),
                    });
                    to
                } else {
                    panic!("undeclared variable {}", self.pool.get(*i))
                }
            }
            Expr::EnumLiteral(_) => todo!(),
            Expr::StructLiteral(_) => todo!(),
        }
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
        let body = self
            .ready
            .get(frame.current_func.0)
            .expect("Forgot to jit current function?")
            .as_ref()
            .unwrap();
        body
    }

    // Resolve the lazy types for Arg and Ret
    fn infer_types(&mut self, func: FuncId) {
        match self.program.funcs[func.0].ty.clone() {
            LazyFnType::Pending { arg, ret } => {
                let ret = match ret {
                    LazyType::Infer => todo!(),
                    LazyType::PendingEval(e) => self.eval_expr(e.clone()).unwrap_type(),
                    LazyType::Finished(id) => id, // easy
                };
                // TODO: copy-n-paste
                let arg = match arg {
                    LazyType::Infer => todo!(),
                    LazyType::PendingEval(e) => self.eval_expr(e.clone()).unwrap_type(),
                    LazyType::Finished(id) => id, // easy
                };
                self.program.funcs[func.0].ty = LazyFnType::Finished(arg, ret);
            }
            LazyFnType::Finished(_, _) => {} // easy
        }
    }

    fn unit_to_type(&mut self) -> LazyFnType<'p> {
        LazyFnType::Finished(
            self.program.intern_type(TypeInfo::Unit),
            self.program.intern_type(TypeInfo::Type),
        )
    }

    // TODO: fast path for builtin type identifiers
    fn eval_expr(&mut self, e: Expr<'p>) -> Value {
        let fake_func: Func<'p> = Func {
            name: self.pool.intern("@interp@eval_expr@"),
            ty: self.unit_to_type(),
            body: Some(e),
            arg_names: vec![None],
        };
        let func_id = self.program.add_func(fake_func);
        self.ensure_compiled(func_id);
        self.run(func_id, Value::Unit)
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

impl Debug for FnBody<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "=== Func BC ===")?;
        for (i, bc) in self.insts.iter().enumerate() {
            write!(f, "{i}. ");
            match bc {
                Bc::CallDynamic { .. } => todo!(),
                Bc::CallDirect { f: func, ret, arg } => {
                    writeln!(f, "{ret:?} = call(f({:?}), {arg:?});", func.0)?
                }
                Bc::CallBuiltin { name, ret, arg } => {
                    writeln!(f, "{ret:?} = builtin(i({}), {arg:?});", name.0)?
                }
                Bc::LoadConstant { slot, value } => writeln!(f, "{:?} = {:?};", slot, value)?,
                Bc::JumpIf {
                    cond,
                    true_ip,
                    false_ip,
                } => writeln!(
                    f,
                    "if ({:?}) goto {} else goto {};",
                    cond, true_ip, false_ip
                )?,
                Bc::CreateTuple { values, target } => {
                    writeln!(f, "{target:?} = tuple{values:?};")?;
                }
                Bc::Ret(i) => writeln!(f, "return {i:?};")?,
                Bc::Clone { from, to } => writeln!(f, "{:?} = @clone({:?});", to, from)?,
                Bc::Move { from, to } => writeln!(f, "{:?} = move({:?});", to, from)?,
                Bc::Drop(i) => writeln!(f, "drop({:?});", i)?,
            }
        }
        writeln!(f, "===============")?;
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
        _ => return None,
    };
    Some(v)
}

impl FnBody<'_> {
    fn reserve_slots(&mut self, count: usize) -> StackRange {
        let range = StackRange {
            first: StackOffset(self.stack_slots),
            count,
        };
        self.stack_slots += count;
        range
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
