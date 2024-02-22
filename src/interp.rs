#![allow(clippy::wrong_self_convention)]
use std::env;
use std::mem::replace;
use std::process::Command;

use codemap::Span;
use interp_derive::InterpSend;

use crate::compiler::{CErr, ExecTime, Res};
use crate::ffi::InterpSend;
use crate::logging::{outln, unwrap, PoolLog};
use crate::{
    ast::{FnType, FuncId, Program, TypeId, TypeInfo},
    pool::{Ident, StringPool},
};
use crate::{bc::*, ffi};

use crate::logging::{
    assert, assert_eq, bin_int, err, ice, logln,
    LogTag::{ShowErr, ShowPrint},
};

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

    pub fn run(
        &mut self,
        f: FuncId,
        arg: Values,
        when: ExecTime,
        return_slot_count: usize,
    ) -> Res<'p, Values> {
        let _init_heights = (self.call_stack.len(), self.value_stack.len());

        // A fake callframe representing the calling rust program.
        let marker_callframe = CallFrame {
            stack_base: StackAbsolute(self.value_stack.len()),
            current_func: FuncId(0), // TODO: actually reserve 0 cause i do this a lot
            current_ip: 0,
            return_slot: StackAbsoluteRange {
                first: StackAbsolute(self.value_stack.len()),
                count: return_slot_count, // This matters even though it shouldn't. It's used for passing info to continuation
            },
            is_rust_marker: true, // used for exiting the run loop. this is the only place we set it true.
            debug_name: self.pool.intern("@interp::run@"),
            when,
        };
        self.call_stack.push(marker_callframe);
        // TODO: typecheck
        for _ in 0..return_slot_count {
            self.value_stack.push(Value::Poison);
        }
        let ret = StackRange {
            first: StackOffset(0),
            count: return_slot_count, // TODO
        };

        // Call the function
        self.push_callframe(f, ret, arg, when)?;
        self.run_continuation()
    }

    fn run_continuation(&mut self) -> Res<'p, Values> {
        if let Err(e) = self.run_inst_loop() {
            outln!(ShowErr, "{}", self.log_callstack());
            return Err(e);
        }

        // Give the return value to the caller.
        let frame = unwrap!(self.call_stack.last(), "");

        let ret = self.index_to_range(frame.return_slot);
        let result = self.take_slots(ret);

        // Sanity checks that we didn't mess anything up for the next guy.
        assert!(!result.is_poison());
        for _ in 0..ret.count {
            assert!(self.value_stack.pop() == Some(Value::Poison));
        }
        let _final_callframe = self.call_stack.pop().unwrap();
        // assert!(self, final_callframe == marker_callframe, "bad frame");
        // let end_heights = (self.call_stack.len(), self.value_stack.len());
        // assert!(init_heights == end_heights, "bad stack size");
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
                &Bc::LoadConstant { slot, value } => {
                    *self.get_slot_mut(slot) = value;
                    self.bump_ip();
                }
                &Bc::JumpIf {
                    cond,
                    true_ip,
                    false_ip,
                } => {
                    let cond = self.take_slot(cond);
                    let cond = self.to_bool(cond.into())?;
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
                    let abs = self.range_to_index(ret);
                    self.bump_ip();
                    // Note: at this point you have to be re-enterant because you might suspend to ask the compiler to do something.
                    let value = self.runtime_builtin(name, arg.clone(), Some(abs))?;
                    self.expand_maybe_tuple(value, abs)?;
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
                    debug_assert_eq!(frame.return_slot.count, slot.count);

                    self.expand_maybe_tuple(value, frame.return_slot)?;

                    // Release our stack space.
                    let size = self.value_stack.len() - frame.stack_base.0;
                    for i in 0..size {
                        let value = self.value_stack.pop().unwrap();
                        debug_assert_eq!(
                            value,
                            Value::Poison,
                            "{:?} was not empty\n{}",
                            StackOffset(size - i - 1),
                            self.log_callstack()
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
                    let f = self.to_func(f.into())?;
                    self.push_callframe(f, ret, arg, when)?;
                    logln!("{}", self.log_callstack());
                    // don't bump ip here, we're in a new call frame.
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
                    let res = self.raw_slice_ptr(base, offset, count)?;
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
                    let tag = self.raw_slice_ptr(arg, 0, 1)?;
                    let tag = self.deref_ptr(tag)?;
                    assert_eq!(tag, Values::One(Value::I64(value)));
                    self.bump_ip();
                }
                &Bc::CallC { f, ret, arg } => {
                    self.bump_ip();

                    #[cfg(not(feature = "interp_c_ffi"))]
                    {
                        err!("Interp c ffi is disabled.",)
                    }
                    #[cfg(feature = "interp_c_ffi")]
                    {
                        let f = self.take_slot(f);
                        let (ptr, f_ty) = self.to_c_func(f)?;
                        let arg = self.take_slots(arg);
                        let result = ffi::c::call(self.program, ptr, f_ty, arg)?;
                        *self.get_slot_mut(ret.single()) = result.single()?;
                    }
                }
            }
        }
        Ok(())
    }

    fn expand_maybe_tuple(&mut self, value: Values, target: StackAbsoluteRange) -> Res<'p, ()> {
        logln!("Expand {:?} TO {} slots", value, target.count);
        match target.count.cmp(&1) {
            std::cmp::Ordering::Equal => {
                self.value_stack[target.first.0] = value.single()?;
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

    // I care not for your stride!
    fn raw_slice_ptr(
        &mut self,
        base: Value,
        physical_offset: usize,
        physical_count: usize,
    ) -> Res<'p, Value> {
        match base {
            Value::InterpAbsStackAddr(addr) => {
                assert!(physical_count <= addr.count);
                Ok(Value::InterpAbsStackAddr(StackAbsoluteRange {
                    first: StackAbsolute(addr.first.0 + physical_offset),
                    count: physical_count,
                }))
            }
            Value::Heap {
                value,
                physical_first,
                stride,
                ..
            } => {
                let abs_first = physical_first + physical_offset;
                let abs_last = abs_first + physical_count;
                // Slicing operations are bounds checked.
                let data = unsafe { &*value };
                assert!(
                    data.references > 0
                        && (abs_first) < data.values.len()
                        && (abs_last) <= data.values.len()
                );
                Ok(Value::Heap {
                    value,
                    physical_first: abs_first,
                    physical_count,
                    stride,
                })
            }
            _ => err!("Wanted ptr found {:?}", base),
        }
    }

    fn deref_ptr(&mut self, arg: Value) -> Res<'p, Values> {
        match arg {
            Value::InterpAbsStackAddr(addr) => {
                let values = &self.value_stack[addr.first.0..addr.first.0 + addr.count];
                Ok(values.to_vec().into())
            }
            Value::Heap {
                value,
                physical_first: first,
                physical_count: count,
                ..
            } => {
                let data = unsafe { &*value };
                assert!(data.references > 0);
                let values = &data.values[first..first + count];
                Ok(values.to_vec().into())
            }
            _ => err!("Wanted ptr found {:?}", arg),
        }
    }

    pub fn runtime_builtin(
        &mut self,
        name: &str,
        arg: Values,
        ret_slot_for_suspend: Option<StackAbsoluteRange>,
    ) -> Res<'p, Values> {
        logln!("runtime_builtin: {name} {arg:?}");
        let value = match name {
            "panic" => {
                outln!(ShowErr, "{}", self.log_callstack());
                // TODO: let comptime panics get caught by !assert_compile_error
                if let Some(s) = String::deserialize_one(arg.clone()) {
                    outln!(ShowErr, "{s}");
                }
                outln!(ShowErr, "{arg:?}");
                err!("Program panicked: {arg:?}",)
            }
            "assert_eq" => {
                let (a, b) = self.to_pair(arg)?;
                assert_eq!(a, b, "runtime_builtin:assert_eq");
                self.assertion_count += 1; // sanity check for making sure tests actually ran
                Value::Unit.into()
            }
            "is_comptime" => {
                assert_eq!(arg, Values::One(Value::Unit));
                let when = self.call_stack.last().unwrap().when;
                Value::Bool(when == ExecTime::Comptime).into()
            }
            "is_uninit" => {
                let range = match arg {
                    Values::One(Value::InterpAbsStackAddr(addr)) => {
                        &self.value_stack[addr.first.0..addr.first.0 + addr.count]
                    }
                    Values::One(Value::Heap {
                        value,
                        physical_first: first,
                        physical_count: count,
                        ..
                    }) => {
                        let data = unsafe { &*value };
                        assert!(
                            data.references > 0
                                && (first) < data.values.len()
                                && (count) < data.values.len()
                        );
                        &data.values[(first)..(first + count)]
                    }
                    _ => err!("Wanted ptr found {:?}", arg),
                };
                let result = range.iter().all(|v| v == &Value::Poison);
                Value::Bool(result).into()
            }
            "raw_len" => match arg {
                Values::One(Value::InterpAbsStackAddr(addr)) => Value::I64(addr.count as i64),
                Values::One(Value::Heap { physical_count, .. }) => {
                    Value::I64(physical_count as i64)
                }
                _ => err!("Wanted ptr found {:?}", arg),
            }
            .into(),
            "size_of" => {
                let ty = self.to_type(arg)?;
                let stride = self.program.slot_count(ty);
                Value::I64(stride as i64).into()
            }
            "Ptr" => {
                let inner_ty = self.to_type(arg)?;
                Value::Type(self.program.ptr_type(inner_ty)).into()
            }
            "Slice" => {
                let inner_ty = self.to_type(arg)?;
                Value::Type(self.program.slice_type(inner_ty)).into()
            }
            "is_oob_stack" => {
                let addr = self.to_stack_addr(arg)?;
                Value::Bool(addr.first.0 >= self.value_stack.len()).into()
            }
            "raw_slice" => {
                // last is not included
                let (addr, new_first, new_last) = self.to_triple(arg)?;
                let (new_first, new_last) = (self.to_int(new_first)?, self.to_int(new_last)?);
                assert!(
                    new_first >= 0 && new_last >= 0 && new_first <= new_last,
                    "{new_first}..<{new_last}"
                );
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
                        physical_first: first,
                        stride,
                        ..
                    } => {
                        let abs_first = first + new_first as usize;
                        let abs_last = first + new_last as usize;
                        // Slicing operations are bounds checked.
                        let data = unsafe { &*value };
                        assert!(
                            data.references > 0
                                && abs_first < (data.values.len())
                                && abs_last <= (data.values.len()),
                            "[len={}]{abs_first}..<{abs_last}",
                            data.values.len()
                        );
                        Value::Heap {
                            value,
                            physical_first: abs_first,
                            physical_count: abs_last - abs_first,
                            stride,
                        }
                    }
                    _ => err!("Wanted ptr found {:?}", addr),
                }
                .into()
            }
            "alloc" => {
                let (ty, count) = self.to_pair(arg)?;
                let (ty, count) = (self.to_type(ty.into())?, self.to_int(count)?);
                let stride = self.program.slot_count(ty);
                assert!(count >= 0);
                let values = vec![Value::Poison; count as usize * stride];
                let value = Box::into_raw(Box::new(InterpBox {
                    references: 1,
                    values,
                }));
                Value::Heap {
                    value,
                    physical_first: 0,
                    physical_count: count as usize * stride,
                    stride,
                }
                .into()
            }
            "dump_ffi_types" => {
                let msg = self.program.dump_ffi_types();
                msg.serialize_one()
            }
            "dealloc" => {
                let (ty, ptr) = self.to_pair(arg)?;
                let (ty, (ptr, ptr_first, ptr_count, stride)) =
                    (self.to_type(ty.into())?, self.to_heap_ptr(ptr.into())?);
                assert_eq!(stride, self.program.slot_count(ty));
                assert_eq!(ptr_first, 0);
                let ptr_val = unsafe { &*ptr };
                assert_eq!(ptr_val.references, 1);
                // let slots = ptr_count * self.program.slot_count(ty);  // TODO: arrays of tuples should be flattened and then this makes sense for the check below.
                assert_eq!(ptr_val.values.len(), ptr_count);
                let _ = unsafe { Box::from_raw(ptr) };
                Value::Unit.into()
            }
            "print" => {
                outln!(ShowPrint, "{:?}", arg);
                Value::Unit.into()
            }
            "reflect_print" => {
                outln!(ShowPrint, "=== start print ===");
                self.reflect_print(arg, 0)?;
                outln!(ShowPrint, "=== end print ===");
                Value::Unit.into()
            }
            "print_callstack" => {
                outln!(ShowPrint, "{}", self.log_callstack());
                Value::Unit.into()
            }
            "Fn" => {
                // println!("Fn: {:?}", arg);
                let (arg, ret) = self.to_pair(arg)?;
                let (arg, ret) = (self.to_type(arg.into())?, self.to_type(ret.into())?);
                let ty = self.program.intern_type(TypeInfo::Fn(FnType { arg, ret }));
                // println!("=> {}", self.program.log_type(ty));
                Value::Type(ty).into()
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
            // TODO: remove. make sure tuple syntax always works first tho.
            "Ty" => {
                if let Values::One(Value::Type(ty)) = arg {
                    assert!(
                        false,
                        "Ty arg should be tuple of types not type {:?}",
                        self.program.log_type(ty)
                    );
                }
                print!("Ty: {:?}", arg);
                let ty = self.to_type(arg)?;
                // println!(" => {:?}", self.program.log_type(ty));
                Value::Type(ty).into()
            }
            "Unique" => {
                let ty = self.to_type(arg)?;
                Value::Type(self.program.unique_ty(ty)).into()
            }
            "tag_value" => {
                let (enum_ty, name) = self.to_pair(arg)?;
                let (enum_ty, name) = (self.to_type(enum_ty.into())?, self.to_int(name)?);
                let name = unwrap!(self.pool.upcast(name), "bad symbol");
                let cases = unwrap!(self.program.get_enum(enum_ty), "not enum");
                let index = cases.iter().position(|f| f.0 == name);
                let index = unwrap!(index, "bad case name");
                Value::I64(index as i64).into()
            }
            "system" => {
                self.fail_on_wasm("fn system")?;
                let mut arg = unwrap!(
                    Vec::<String>::deserialize_one(arg.clone()),
                    "expected Vec<string> not {arg:?}. TODO: support stack slices"
                )
                .into_iter();
                let cmd = unwrap!(arg.next(), "cmd");
                let mut cmd = Command::new(&cmd);
                for arg in arg {
                    cmd.arg(arg);
                }
                match cmd.output() {
                    Ok(output) => CmdResult {
                        status: output.status.code().unwrap(),
                        stdout: output.stdout,
                        stderr: output.stderr,
                    }
                    .serialize_one(),
                    Err(e) => err!("Error running {cmd:?}: {e:?}",),
                }
            }
            "puts" => {
                let arg = unwrap!(
                    String::deserialize_one(arg.clone()),
                    "expect str not {arg:?}"
                );
                outln!(ShowPrint, "{arg}");
                Value::Unit.into()
            }
            "cli_args" => {
                self.fail_on_wasm("fn cli_args")?;
                let mut args = env::args();
                args.next(); // The interpreter's exe.
                let args: Vec<_> = args.collect();
                args.serialize_one()
            }
            "type_id" => {
                let ty = self.to_type(arg)?;
                Value::I64(ty.0 as i64).into()
            }
            "infer_raw_deref_type" => {
                let name = self.pool.intern(name);
                self.suspend(
                    name,
                    arg,
                    unwrap!(ret_slot_for_suspend, "interp suspend but no ret slot"),
                )?
            }
            _ => ice!("Known builtin is not implemented. {}", name),
        };
        Ok(value)
    }

    fn suspend(
        &mut self,
        name: Ident<'p>,
        arg: Values,
        ret: StackAbsoluteRange,
    ) -> Res<'p, Values> {
        err!(CErr::InterpMsgToCompiler(name, arg, ret))
    }

    pub fn resume(&mut self, result: Values, ret: StackAbsoluteRange) -> Res<'p, Values> {
        self.expand_maybe_tuple(result, ret)?;
        self.run_continuation()
    }

    fn push_callframe(
        &mut self,
        f: FuncId,
        ret: StackRange,
        arg: Values,
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

    fn push_expanded(&mut self, arg: Values) {
        match arg {
            Values::One(arg) => self.value_stack.push(arg),
            Values::Many(values) => self.value_stack.extend(values),
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
        *value
    }

    #[track_caller]
    fn take_slot(&mut self, slot: StackOffset) -> Value {
        let value = replace(self.get_slot_mut(slot), Value::Poison);
        debug_assert_ne!(value, Value::Poison);
        value
    }

    /// If slot ranges over multiple, return them as a tuple.
    #[track_caller]
    fn take_slots(&mut self, slot: StackRange) -> Values {
        if slot.count == 0 {
            Value::Unit.into()
        } else if slot.count == 1 {
            self.take_slot(slot.first).into()
        } else {
            let mut values = vec![];
            for i in 0..slot.count {
                values.push(self.take_slot(StackOffset(slot.first.0 + i)))
            }

            values.into()
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

    fn index_to_range(&mut self, slot: StackAbsoluteRange) -> StackRange {
        let frame = self.call_stack.last().unwrap();
        StackRange {
            first: StackOffset(frame.stack_base.0 + slot.first.0),
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
    pub fn to_type(&mut self, value: Values) -> Res<'p, TypeId> {
        match value {
            Values::One(Value::Unit) => Ok(self.program.intern_type(TypeInfo::Unit)),
            Values::One(Value::Type(id)) => Ok(id),
            Values::Many(values) => {
                let values: Res<'_, Vec<_>> =
                    values.into_iter().map(|v| self.to_type(v.into())).collect();
                Ok(self.program.tuple_of(values?))
            }
            _ => {
                err!(CErr::TypeError("Type", value))
            }
        }
    }

    #[track_caller]
    fn to_bool(&self, value: Values) -> Res<'p, bool> {
        if let Values::One(Value::Bool(v)) = value {
            Ok(v)
        } else {
            err!(CErr::TypeError("bool", value))
        }
    }

    #[track_caller]
    fn to_seq(&self, value: Values) -> Res<'p, Vec<Value>> {
        Ok(value.vec())
    }

    #[track_caller]
    fn to_triple(&self, value: Values) -> Res<'p, (Value, Value, Value)> {
        let values = value.vec();
        assert_eq!(values.len(), 3, "arity {:?}", values);
        Ok((values[0], values[1], values[2]))
    }

    #[track_caller]
    fn to_func(&self, value: Values) -> Res<'p, FuncId> {
        if let Values::One(Value::GetFn(id)) = value {
            Ok(id)
        } else {
            err!(CErr::TypeError("AnyFunc", value))
        }
    }

    #[track_caller]
    fn to_pair(&self, value: Values) -> Res<'p, (Value, Value)> {
        let values = self.to_seq(value)?;
        assert_eq!(values.len(), 2, "arity {:?}", values);
        Ok((values[0], values[1]))
    }

    #[track_caller]
    fn to_stack_addr(&self, value: Values) -> Res<'p, StackAbsoluteRange> {
        if let Values::One(Value::InterpAbsStackAddr(r)) = value {
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
            err!(CErr::TypeError("i64", value.into()))
        }
    }

    fn to_heap_ptr(&self, value: Values) -> Res<'p, (*mut InterpBox, usize, usize, usize)> {
        if let Values::One(Value::Heap {
            value,
            physical_first: first,
            physical_count: count,
            stride,
        }) = value
        {
            Ok((value, first, count, stride))
        } else {
            err!(CErr::TypeError("Heap", value))
        }
    }

    // TODO: macros for each builtin arg type cause this sucks.
    fn load_int_pair(&self, v: Values) -> Res<'p, (i64, i64)> {
        match v {
            Values::Many(mut values) => {
                assert_eq!(values.len(), 2, "load_int_pair wrong arity {:?}", values);
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

    fn to_c_func(&self, v: Value) -> Res<'p, (usize, FnType)> {
        match v {
            Value::CFnPtr { ptr, ty } => Ok((ptr, ty)),
            v => err!("to_c_func {:?}", v),
        }
    }

    fn set_ptr(&mut self, addr: Value, value: Values) -> Res<'p, Value> {
        Ok(match addr {
            Value::InterpAbsStackAddr(addr) => {
                // TODO: call drop if not poison
                // Note: the slots you're setting to are allowed to contain poison
                if addr.count == 1 {
                    self.value_stack[addr.first.0] = value.single()?;
                } else {
                    let values: Vec<Value> = value.into();
                    assert_eq!(values.len(), addr.count);
                    for (i, entry) in values.into_iter().enumerate() {
                        self.value_stack[addr.first.0 + i] = entry;
                    }
                }
                Value::Unit
            }
            Value::Heap {
                value: ptr_value,
                physical_first: first,
                physical_count: count,
                ..
            } => {
                // Slicing operations are bounds checked.
                let ptr = unsafe { &mut *ptr_value };
                assert!(
                    ptr.references > 0
                        && first < ptr.values.len()
                        && (first + count) <= ptr.values.len()
                );
                let values: Vec<_> = value.into();
                assert_eq!(values.len(), count);
                for (i, entry) in values.into_iter().enumerate() {
                    ptr.values[first + i] = entry;
                }
                Value::Unit
            }
            _ => err!("Wanted ptr found {:?}", addr),
        })
    }

    fn reflect_print(&mut self, arg: Values, mut depth: usize) -> Res<'p, ()> {
        for mut v in arg.vec() {
            loop {
                outln!(ShowPrint, "{}{}", "=".repeat(depth), v);
                match v {
                    Value::InterpAbsStackAddr(slot) => {
                        if slot.count == 1 {
                            v = self.deref_ptr(v)?.single()?;
                        } else {
                            for i in 0..slot.count {
                                self.reflect_print(
                                    Value::InterpAbsStackAddr(StackAbsoluteRange {
                                        first: StackAbsolute(slot.first.0 + i),
                                        count: 1,
                                    })
                                    .into(),
                                    depth + 2,
                                )?;
                            }
                            break;
                        }
                    }
                    Value::Heap {
                        value,
                        physical_first: first,
                        physical_count: count,
                        stride,
                    } => {
                        let values = unsafe { &mut *value };
                        outln!(
                            ShowPrint,
                            "{}{:?}",
                            "=".repeat(depth),
                            &values.values[first..(first + count)]
                        );
                        if (count % stride) == 0 && count != stride {
                            for i in 0..(count / stride) {
                                self.reflect_print(
                                    Value::Heap {
                                        value,
                                        physical_first: first + (i * stride),
                                        physical_count: stride,
                                        stride,
                                    }
                                    .into(),
                                    depth + 2,
                                )?;
                            }
                        }

                        break;
                    }
                    _ => break,
                }
                depth += 2;
            }
        }
        Ok(())
    }

    fn fail_on_wasm(&self, name: &str) -> Res<'p, ()> {
        if cfg!(target_arch = "wasm32") {
            err!("Operation {name:?} suported on wasm target.",)
        }
        Ok(())
    }
}

#[derive(Debug, Clone, InterpSend)]
pub struct CmdResult {
    status: i32,
    stdout: Vec<u8>,
    stderr: Vec<u8>,
}
