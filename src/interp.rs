use std::{
    collections::HashMap,
    fmt::{format, Debug},
    mem::replace,
};

use crate::{
    ast::{Expr, Func, FuncId, Program, Stmt, TypeId, Var},
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
    Unit,
    GetFn(FuncId),
    // This is unsed to represent a function's empty stack space.
    // Crash if you try to read one.
    Poison,
}

#[derive(Debug, Copy, Clone)]
struct StackOffset(usize);

#[derive(Debug, Copy, Clone)]
struct StackAbsolute(usize);

#[derive(Debug, Copy, Clone)]
enum IntrinsicFn {
    AddI64,
    SubI64,
    MulI64,
    DivI64,
    AddF64,
    SubF64,
    MulF64,
    DivF64,
    IsNan,
    EqI64,
    EqF64,
    EqBool,
    RawEqPtr,
    Malloc,
    Free,
    Transmute,
    Debug,
    Stride,
    CastF64I64,
    CastI64F64,
    SetPtr,
    GetPtr,
    Unreachable,
    System,
    CliArgs,
    ReadAllFile,
    WriteAllFile,
    GetTimestamp,
}

#[derive(Debug)]
enum Bc<'p> {
    CallDynamic {
        f: StackOffset,
        return_slot: StackOffset,
        arg_slot: StackOffset,
    },
    CallDirect {
        f: FuncId,
        return_slot: StackOffset,
        arg_slot: StackOffset,
    },
    CallBuiltin {
        name: Ident<'p>,
        return_slot: StackOffset,
        arg_slot: StackOffset,
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
        values: Vec<StackOffset>,
        target: StackOffset,
    },
    Ret(StackOffset),
    // Clone, Move, and Drop are for managing linear types.
    Clone {
        from: StackOffset,
        to: StackOffset,
    },
    Move {
        from: StackOffset,
        to: StackOffset,
    },
    Drop(StackOffset),
}

struct CallFrame {
    stack_base: StackAbsolute,
    current_func: FuncId,
    current_ip: usize,
    return_slot: StackAbsolute,
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
                Stmt::DeclFunc {
                    name,
                    return_type,
                    body,
                    arg_names,
                } => {
                    let id = FuncId(self.program.funcs.len());
                    self.program.funcs.push(Func {
                        name,
                        ty: TypeId(0), // TODO
                        body: body.unwrap(),
                        arg_names,
                    });
                    // TODO: add to func_lookup
                    assert!(self.program.declarations.get(&name).is_none(), "TODO");
                    self.program.declarations.insert(name, vec![id]);
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
        self.ensure_compiled(f);
        // TODO: typecheck
        self.value_stack.push(Value::Poison); // For the return value.
        self.value_stack.push(arg);
        self.call_stack.push(CallFrame {
            stack_base: StackAbsolute(1), // Our stack includes the argument but not the return slot.
            current_func: f,
            current_ip: 0,
            return_slot: StackAbsolute(0),
        });

        let empty = self.current_fn_body().stack_slots;
        for _ in 0..empty {
            self.value_stack.push(Value::Poison);
        }

        while self.value_stack.len() > 1 {
            let i = self.next_inst();
            println!("I: {:?}", i);
            match i {
                &Bc::CallDirect {
                    f,
                    return_slot,
                    arg_slot,
                } => todo!(),
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
                &Bc::CallBuiltin {
                    name,
                    return_slot,
                    arg_slot,
                } => {
                    println!("{:?}", self.value_stack);
                    let name = self.pool.get(name);
                    // Calling Convention: arguments passed to a function are moved out of your stack.
                    let arg = self.take_slot(arg_slot);
                    let value = match name {
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
                        _ => panic!("Known builtin {:?} is not implemented.", name),
                    };
                    *self.get_slot_mut(return_slot) = value;
                    println!("{:?}", self.value_stack);
                    self.bump_ip();
                }
                &Bc::Ret(slot) => {
                    let value = self.take_slot(slot);
                    let frame = self.call_stack.pop().unwrap();
                    self.value_stack[frame.return_slot.0] = value;
                    // Release our stack space.
                    let size = self.value_stack.len() - frame.stack_base.0;
                    for _ in 0..size {
                        let value = self.value_stack.pop().unwrap();
                        debug_assert_eq!(value, Value::Poison)
                    }
                    // We can't increment the caller's ip because the callstack might be empty now
                    // (if they were the original rust code that called run).
                    // In that case, the loop will break.
                }
                Bc::CallDynamic {
                    f,
                    return_slot,
                    arg_slot,
                } => todo!(), // Dont for get to inc ip first since ret doesn't
                Bc::CreateTuple { values, target } => {
                    let values = values.clone(); // TODO: this sucks but take needs unique access.
                    let target = *target;
                    println!("{:?}", self.value_stack);
                    println!("{:?}", values);
                    // Calling Convention: values are moved into a tuple.
                    let values: Vec<_> = values
                        .into_iter()
                        .map(|slot| self.take_slot(slot))
                        .collect();
                    let values = Value::Tuple {
                        container_type: TypeId(0), // TODO
                        values,
                    };
                    *self.get_slot_mut(target) = values;
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
                    let _ = self.take_slot(slot);
                    self.bump_ip();
                }
            }
        }
        // Give the return value to the caller.
        // Can't call take_slot because there isn't a stack frame.
        // This will change when comptime execution calls run recursively.
        assert_eq!(self.value_stack.len(), 1);
        assert!(self.call_stack.is_empty());
        let result = self.value_stack.pop().unwrap();
        debug_assert_ne!(result, Value::Poison);
        result
    }

    fn bump_ip(&mut self) {
        let frame = self.call_stack.last_mut().unwrap();
        frame.current_ip += 1;
    }

    fn clone_slot(&self, slot: StackOffset) -> Value {
        let frame = self.call_stack.last().unwrap();
        let value = &self.value_stack[frame.stack_base.0 + slot.0];
        debug_assert_ne!(value, &Value::Poison);
        value.clone()
    }

    fn take_slot(&mut self, slot: StackOffset) -> Value {
        let mut value = replace(self.get_slot_mut(slot), Value::Poison);
        debug_assert_ne!(value, Value::Poison);
        value
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

        let func = self.program.funcs[index].clone();
        self.log_depth += 1;
        println!(
            "{} Start JIT: {}",
            "=".repeat(self.log_depth),
            self.pool.get(func.name)
        );
        let mut result = FnBody {
            insts: vec![],
            stack_slots: 1,
            vars: Default::default(),
            arg_names: func.arg_names.clone(),
        };
        let return_value = self.compile_expr(&mut result, &func.body);

        // We're done with our arguments, get rid of them.
        for i in 0..result.arg_names.len() {
            // TODO: once non-copy types are supported, this needs to get smarter.
            result.insts.push(Bc::Drop(StackOffset(i)));
        }

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

    fn compile_expr(&mut self, result: &mut FnBody<'p>, expr: &Expr<'p>) -> StackOffset {
        match expr {
            Expr::Num(_) => todo!(),
            Expr::Call(f, arg) => {
                let arg_slot = self.compile_expr(result, arg);
                let return_slot = StackOffset(result.stack_slots);
                result.stack_slots += 1;
                if let Expr::GetNamed(i) = f.as_ref() {
                    if let Some(f) = self.lookup_unique_func(*i) {
                        // Note: f might not be compiled yet, we'll find out the first time we try to call it.
                        result.insts.push(Bc::CallDirect {
                            f,
                            return_slot,
                            arg_slot,
                        });
                        return return_slot;
                    } else if self.builtins.contains(i) {
                        result.insts.push(Bc::CallBuiltin {
                            name: *i,
                            return_slot,
                            arg_slot,
                        });
                        return return_slot;
                    }
                    // else: fallthrough
                }

                println!("dynamic {}", f.log(self.pool));
                let f = self.compile_expr(result, f);
                result.insts.push(Bc::CallDynamic {
                    f,
                    return_slot,
                    arg_slot,
                });
                return_slot
            }
            Expr::Block(_, _) => todo!(),
            Expr::IfElse(_, _, _) => todo!(),
            Expr::Array(_) => todo!(),
            Expr::Tuple(values) => {
                let values: Vec<_> = values
                    .iter()
                    .map(|v| self.compile_expr(result, v))
                    .collect();
                let target = StackOffset(result.stack_slots);
                result.stack_slots += 1;
                result.insts.push(Bc::CreateTuple { values, target });
                target
            }
            Expr::RefType(_) => todo!(),
            Expr::GetVar(v) => *result.vars.get(v).unwrap(),
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
                    let result_slot = StackOffset(result.stack_slots);
                    result.stack_slots += 1;
                    result.insts.push(Bc::Clone {
                        from: StackOffset(index),
                        to: result_slot,
                    });
                    result_slot
                } else {
                    panic!("undeclared variable {}", self.pool.get(*i))
                }
            }
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
            .unwrap()
            .as_ref()
            .unwrap();
        body
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

impl Debug for FnBody<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "=== Func BC ===")?;
        for (i, bc) in self.insts.iter().enumerate() {
            write!(f, "{i}. ");
            match bc {
                Bc::CallDynamic {
                    f,
                    return_slot,
                    arg_slot,
                } => todo!(),
                Bc::CallDirect {
                    f,
                    return_slot,
                    arg_slot,
                } => todo!(),
                Bc::CallBuiltin {
                    name,
                    return_slot,
                    arg_slot,
                } => writeln!(
                    f,
                    "${} = builtin(i({}), ${});",
                    return_slot.0, name.0, arg_slot.0
                )?,
                Bc::LoadConstant { slot, value } => writeln!(f, "${} = {:?};", slot.0, value)?,
                Bc::JumpIf {
                    cond,
                    true_ip,
                    false_ip,
                } => writeln!(
                    f,
                    "if (${}) goto {} else goto {};",
                    cond.0, true_ip, false_ip
                )?,
                Bc::CreateTuple { values, target } => {
                    let args: Vec<_> = values.iter().map(|i| format!("${}", i.0)).collect();
                    let args = args.join(", ");
                    writeln!(f, "${} = tuple({});", target.0, args)?;
                }
                Bc::Ret(i) => writeln!(f, "return ${};", i.0)?,
                Bc::Clone { from, to } => writeln!(f, "${} = @clone(${});", to.0, from.0)?,
                Bc::Move { from, to } => writeln!(f, "${} = move(${});", to.0, from.0)?,
                Bc::Drop(i) => writeln!(f, "drop(${});", i.0)?,
            }
        }
        writeln!(f, "===============")?;
        Ok(())
    }
}
