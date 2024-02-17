#![allow(clippy::wrong_self_convention)]
use std::{mem::replace, panic::Location};

use codemap::Span;

use crate::bc::*;
use crate::compiler::{CErr, ExecTime, Res};
use crate::logging::{outln, unwrap, PoolLog};
use crate::{
    ast::{FnType, FuncId, Program, TypeId, TypeInfo},
    pool::{Ident, StringPool},
};

use crate::logging::{assert, assert_eq, bin_int, err, ice, logln};

#[derive(Debug, Clone)]
pub struct CallFrame<'p> {
    pub stack_base: StackAbsolute,
    current_func: FuncId,
    current_ip: usize,
    return_slot: StackAbsoluteRange,
    is_rust_marker: bool,
    pub debug_name: Ident<'p>,
    when: ExecTime,
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
    pub assertion_count: usize,
    pub last_loc: Option<Span>,
}

impl<'a, 'p> Interp<'a, 'p> {
    pub fn new(pool: &'a StringPool<'p>, program: &'a mut Program<'p>) -> Self {
        Self {
            pool,
            value_stack: vec![],
            call_stack: vec![],
            program,
            ready: vec![],
            assertion_count: 0,
            last_loc: None,
        }
    }

    pub fn run(&mut self, f: FuncId, arg: Value, when: ExecTime) -> Res<'p, Value> {
        let init_heights = (self.call_stack.len(), self.value_stack.len());

        // A fake callframe representing the calling rust program.
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
        assert!(result != Value::Poison);
        assert!(self.value_stack.pop() == Some(Value::Poison));
        let _final_callframe = self.call_stack.pop().unwrap();
        // assert!(self, final_callframe == marker_callframe, "bad frame");
        let end_heights = (self.call_stack.len(), self.value_stack.len());
        assert!(init_heights == end_heights, "bad stack size");
        self.last_loc = None;
        Ok(result)
    }

    fn run_inst_loop(&mut self) -> Res<'p, ()> {
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
                &Bc::Load { from, to } => {
                    let arg = self.take_slot(from);
                    let value = self.deref_ptr(arg)?;
                    let ret = self.range_to_index(to);
                    self.expand_maybe_tuple(value, ret)?;
                    self.bump_ip();
                }
                &Bc::Store { from, to } => {
                    let value = self.take_slots(from);
                    let addr = self.take_slot(to);
                    self.set_ptr(addr, value)?;
                    self.bump_ip();
                }
                Bc::DebugLine(_) | Bc::DebugMarker(_, _) => self.bump_ip(),
                &Bc::TagCheck { enum_ptr, value } => {
                    let arg = self.clone_slot(enum_ptr);
                    let tag = self.slice_ptr(arg, 0, 1)?;
                    let tag = self.deref_ptr(tag)?;
                    assert_eq!(tag, Value::I64(value));
                    self.bump_ip();
                }
            }
        }
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
                assert_eq!(values.len(), target.count);
                let base = target.first.0;
                for (i, v) in values.into_iter().enumerate() {
                    assert_eq!(self.value_stack[base + i], Value::Poison);
                    self.value_stack[base + i] = v;
                }
            }
        }
        Ok(())
    }

    fn slice_ptr(&mut self, base: Value, offset: usize, count: usize) -> Res<'p, Value> {
        match base {
            Value::InterpAbsStackAddr(addr) => {
                assert!(count <= addr.count);
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
            _ => err!("Wanted ptr found {:?}", base),
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
                assert!(data.references > 0);
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
            _ => err!("Wanted ptr found {:?}", arg),
        }
    }

    pub fn runtime_builtin(&mut self, name: &str, arg: Value) -> Res<'p, Value> {
        logln!("runtime_builtin: {name} {arg:?}");
        let value = match name {
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
            "get" => self.deref_ptr(arg)?,
            "set" => {
                let (addr, value) = self.to_pair(arg)?;
                self.set_ptr(addr, value)?
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
            "len" => match arg {
                Value::InterpAbsStackAddr(addr) => Value::I64(addr.count as i64),
                Value::Heap { count, .. } => Value::I64(count as i64),
                _ => panic!("Wanted ptr found {:?}", arg),
            },
            "Ptr" => {
                let inner_ty = self.to_type(arg)?;
                Value::Type(self.program.ptr_type(inner_ty))
            }
            "Slice" => {
                let inner_ty = self.to_type(arg)?;
                Value::Type(self.program.slice_type(inner_ty))
            }
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
                    Value::Heap { value, first, .. } => {
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
            "dealloc" => {
                let (ty, ptr) = self.to_pair(arg)?;
                let (ty, (ptr, ptr_first, ptr_count)) = (self.to_type(ty)?, self.to_heap_ptr(ptr)?);
                let slots = ptr_count * self.program.slot_count(ty);
                assert_eq!(ptr_first, 0);
                let ptr_val = unsafe { &*ptr };
                assert_eq!(ptr_val.references, 1);
                assert_eq!(ptr_val.values.len(), slots);
                let _ = unsafe { Box::from_raw(ptr) };
                Value::Unit
            }
            "print" => {
                outln!("{}", arg);
                Value::Unit
            }
            "print_callstack" => {
                outln!("{}", self.log_callstack());
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
            // TODO: remove
            "tuple" => {
                // This will become the `(a, b, c)` syntax.
                // It just gives you a way to express that you want to pass multiple things at once.
                // TODO: this is really inefficient. it boxes them from the stack and then writes them all back again?
                arg
            }
            // TODO: remove. make sure tuple syntax always works first tho.
            "Ty" => {
                if let Value::Type(ty) = arg {
                    assert!(
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
            "Unique" => {
                let ty = TypeInfo::Unique(self.to_type(arg)?, self.program.types.len());
                Value::Type(self.program.intern_type(ty))
            }
            "tag_value" => {
                let (enum_ty, name) = self.to_pair(arg)?;
                let (enum_ty, name) = (self.to_type(enum_ty)?, self.to_int(name)?);
                let name = unwrap!(self.pool.upcast(name), "bad symbol");
                let cases = unwrap!(self.program.get_enum(enum_ty), "not enum");
                let index = cases.iter().position(|f| f.0 == name);
                let index = unwrap!(index, "bad case name");
                Value::I64(index as i64)
            }
            _ => ice!("Known builtin is not implemented. {}", name),
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
        let func = self.ready[f.0].as_ref();
        assert!(func.is_some(), "ICE: tried to call {f:?} but not compiled.");
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
        });
        let empty = self.current_fn_body().stack_slots; // TODO: does this count tuple args right?
        for _ in 0..empty {
            self.value_stack.push(Value::Poison);
        }
        // TODO: only comptime.
        if self.call_stack.len() > 1000 {
            err!(CErr::StackDepthLimit);
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

    // TOOD: @track_caller so you don't just get an error report on the forward declare of assert_eq all the time
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

    #[track_caller]
    fn to_type(&mut self, value: Value) -> Res<'p, TypeId> {
        if let Value::Type(id) = value {
            Ok(id)
        } else if let Value::Tuple { values, .. } = value {
            let values: Res<'_, Vec<_>> = values.into_iter().map(|v| self.to_type(v)).collect();
            Ok(self.program.tuple_of(values?))
        } else if let Value::Unit = value {
            // This lets you use the literal `()` as a type (i dont parse it as a tuple because reasons). TODO: check if that still true with new parser
            Ok(self.program.intern_type(TypeInfo::Unit))
        } else {
            err!(CErr::TypeError("Type", value))
        }
    }

    #[track_caller]
    fn to_bool(&self, value: Value) -> Res<'p, bool> {
        if let Value::Bool(v) = value {
            Ok(v)
        } else {
            err!(CErr::TypeError("bool", value))
        }
    }

    #[track_caller]
    fn to_seq(&self, value: Value) -> Res<'p, Vec<Value>> {
        match value {
            Value::Tuple { values, .. } => Ok(values),

            _ => err!(CErr::TypeError("AnyTuple | AnyArray", value)),
        }
    }

    #[track_caller]
    fn to_triple(&self, value: Value) -> Res<'p, (Value, Value, Value)> {
        let values = self.to_seq(value)?;
        assert_eq!(values.len(), 3, "arity {:?}", values);
        Ok((values[0].clone(), values[1].clone(), values[2].clone()))
    }

    #[track_caller]
    fn to_func(&self, value: Value) -> Res<'p, FuncId> {
        if let Value::GetFn(id) = value {
            Ok(id)
        } else {
            err!(CErr::TypeError("AnyFunc", value))
        }
    }

    #[track_caller]
    fn to_pair(&self, value: Value) -> Res<'p, (Value, Value)> {
        let values = self.to_seq(value)?;
        assert_eq!(values.len(), 2, "arity");
        Ok((values[0].clone(), values[1].clone()))
    }

    #[track_caller]
    fn split_to_pair(&self, value: Value) -> Res<'p, (Value, Value)> {
        let values = to_flat_seq(value);

        // TODO: sad cloning
        if values.len() == 2 {
            return Ok((values[0].clone(), values[1].clone()));
        }

        assert_eq!(values.len() % 2, 0, "arity");
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
            err!(CErr::TypeError("StackAddr", value))
        }
    }

    #[track_caller]
    fn to_int(&self, value: Value) -> Res<'p, i64> {
        if let Value::I64(r) = value {
            Ok(r)
        } else if let Value::Symbol(r) = value {
            // TODO: have a special unwrap method for this
            Ok(r as i64)
        } else {
            err!(CErr::TypeError("i64", value))
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
            err!(CErr::TypeError("Heap", value))
        }
    }

    // TODO: macros for each builtin arg type cause this sucks.
    fn load_int_pair(&self, v: Value) -> Res<'p, (i64, i64)> {
        match v {
            Value::Tuple { mut values, .. } => {
                assert_eq!(values.len(), 2, "load_int_pair wrong arity");
                let a = replace(&mut values[0], Value::Poison);
                let b = replace(&mut values[1], Value::Poison);
                Ok((self.load_int(a)?, self.load_int(b)?))
            }
            v => err!("load_int_pair {:?}", v),
        }
    }

    fn load_int(&self, v: Value) -> Res<'p, i64> {
        match v {
            Value::I64(i) => Ok(i),
            v => err!("load_int {:?}", v),
        }
    }

    fn set_ptr(&mut self, addr: Value, value: Value) -> Res<'p, Value> {
        Ok(match addr {
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
                assert!(ptr.references > 0 && first < ptr.values.len() && count < ptr.values.len());
                if count == 1 {
                    ptr.values[first] = value;
                } else {
                    let values = self.to_seq(value)?;
                    assert_eq!(values.len(), count);
                    for (i, entry) in values.into_iter().enumerate() {
                        ptr.values[first + i] = entry;
                    }
                }
                Value::Unit
            }
            _ => panic!("Wanted ptr found {:?}", addr),
        })
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

pub fn to_flat_seq(value: Value) -> Vec<Value> {
    match value {
        Value::Tuple { values, .. } => values.into_iter().flat_map(to_flat_seq).collect(),
        e => vec![e],
    }
}
