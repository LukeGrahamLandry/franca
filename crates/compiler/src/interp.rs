#![allow(clippy::wrong_self_convention)]

use std::env;
use std::mem::replace;
use std::process::Command;

use codemap::Span;
use interp_derive::InterpSend;

use crate::ast::Flag;
use crate::compiler::{CErr, CompileError, ExecTime, Executor, Res, ToBytes};
use crate::emit_bc::EmitBc;
use crate::ffi::InterpSend;
use crate::logging::LogTag::ShowPrint;
use crate::logging::{unwrap, PoolLog};
use crate::outln;
use crate::{assert, assert_eq, err, logln};
use crate::{
    ast::{FnType, FuncId, Program, TypeId, TypeInfo},
    export_ffi,
    pool::{Ident, StringPool},
};
use crate::{bc::*, ffi};

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

#[derive(Clone)]
pub struct Interp<'p> {
    pub pool: &'p StringPool<'p>,
    pub value_stack: Vec<Value>,
    pub call_stack: Vec<CallFrame<'p>>,
    pub ready: BcReady<'p>,
    pub last_loc: Option<Span>,
    pub messages: Vec<StackAbsoluteRange>,
    pub ffi_stack: Vec<Values>,
}

impl<'p> Interp<'p> {
    pub fn new(pool: &'p StringPool<'p>) -> Self {
        Self {
            pool,
            value_stack: vec![],
            call_stack: vec![],
            ready: BcReady::default(),
            last_loc: None,
            messages: vec![],
            ffi_stack: vec![],
        }
    }

    pub fn run(&mut self, f: FuncId, arg: Values, when: ExecTime, return_slot_count: usize, program: &mut Program<'p>) -> Res<'p, Values> {
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
        self.push_callframe(f, ret, arg, when, program)?;
        self.run_continuation(program)
    }

    fn run_continuation(&mut self, program: &mut Program<'p>) -> Res<'p, Values> {
        self.run_inst_loop(program)?;

        // Give the return value to the caller.
        let frame = unwrap!(self.call_stack.last(), "");

        let ret = self.index_to_range(frame.return_slot);
        let result = self.take_slots(ret);

        // Sanity checks that we didn't mess anything up for the next guy.
        assert!(!result.is_poison());
        for _ in 0..ret.count {
            assert_eq!(self.value_stack.pop(), Some(Value::Poison));
        }
        let _final_callframe = self.call_stack.pop().unwrap();
        // assert!(self, final_callframe == marker_callframe, "bad frame");
        // let end_heights = (self.call_stack.len(), self.value_stack.len());
        // assert!(init_heights == end_heights, "bad stack size");
        self.last_loc = None;
        Ok(result)
    }

    fn run_inst_loop(&mut self, program: &mut Program<'p>) -> Res<'p, ()> {
        loop {
            self.update_debug();
            let i = self.next_inst();
            self.log_stack();

            logln!("I: {:?}", i.log(self.pool));
            match i {
                Bc::MarkContiguous(_) | Bc::LastUse(_) => {
                    self.bump_ip();
                }
                Bc::NoCompile => err!("Ran empty function",),
                Bc::Unreachable => err!("Entered unreachable code",),
                &Bc::CallDirect { f, ret, arg } => {
                    // preincrement our ip because ret doesn't do it.
                    // this would be different if i was trying to do tail calls?
                    self.bump_ip();
                    let arg = self.take_slots(arg);
                    let when = self.call_stack.last().unwrap().when;
                    self.push_callframe(f, ret, arg, when, program)?;
                    logln!("{}", self.log_callstack());
                    // don't bump ip here, we're in a new call frame.
                }
                &Bc::CallSplit { ct, rt, ret, arg } => {
                    self.bump_ip();
                    let arg = self.take_slots(arg);

                    let when = self.call_stack.last().unwrap().when;
                    let f = if when == ExecTime::Runtime { rt } else { ct };

                    // TODO: this is super ugly recreation of all the other calling types. need to factor out calling convention better.
                    if let Some(addr) = program[f].comptime_addr {
                        let ty = program[f].unwrap_ty();
                        let comp_ctx = program[f].has_tag(Flag::Ct);
                        let ptr = addr as usize;
                        let result = ffi::c::call(program, ptr, ty, arg, comp_ctx)?;
                        *self.get_slot_mut(ret.single()) = result.single()?;
                    } else if program[f].body.is_none() {
                        let abs = self.range_to_index(ret);
                        let name = self.pool.get(program[f].name);
                        let value = self.runtime_builtin(name, arg.clone(), Some(abs), program)?;
                        self.expand_maybe_tuple(value, abs)?;
                    } else {
                        self.push_callframe(f, ret, arg, when, program)?;
                    }
                }
                &Bc::LoadConstant { slot, value } => {
                    *self.get_slot_mut(slot) = value;
                    self.bump_ip();
                }
                &Bc::JumpIf { cond, true_ip, false_ip } => {
                    let cond = self.take_slot(cond);
                    let cond = Values::One(cond).to_bool()?;
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
                    let value = self.runtime_builtin(name, arg.clone(), Some(abs), program)?;
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
                    let f = Values::One(f).to_func()?;
                    self.push_callframe(f, ret, arg, when, program)?;
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
                &Bc::SlicePtr { base, offset, count, ret } => {
                    let base = self.take_slot(base);
                    let res = self.raw_slice_ptr(base, offset, count)?;
                    *self.get_slot_mut(ret) = res;
                    self.bump_ip();
                }
                // A trivial improvement for load/store would be connecting the parts, so you don't allocate the intermediate vec,
                // but I really don't want to bother working on this because the asm stuff is so much cooler.
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
                &Bc::CallC { f, ret, arg, ty, comp_ctx } => {
                    let f = self.take_slot(f);
                    let arg = self.take_slots(arg);

                    // TODO: HACK! just want to test function pointers dynamic dispatch. this wont work on asm!
                    //      this goes away when i properly implement GetNativeFnPtr
                    if let Some(f) = f.to_func() {
                        self.bump_ip(); // pre-bump
                        let when = self.call_stack.last().unwrap().when;
                        self.push_callframe(f, ret, arg, when, program)?;
                        // don't bump ip here, we're in a new call frame.
                        continue;
                    }

                    self.bump_ip();
                    let ptr = f.to_int()?.to_bytes() as usize;
                    let result = ffi::c::call(program, ptr, ty, arg, comp_ctx)?;
                    *self.get_slot_mut(ret.single()) = result.single()?;
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
                let values = value.to_seq()?;
                assert_eq!(values.len(), target.count);
                let base = target.first.0;
                for (i, v) in values.into_iter().enumerate() {
                    // I think this is fine.
                    // it happens now that i added expr result pointers so it skips a copy when setting var to return value (ret uses this but MoveRange doesn't)
                    // assert_eq!(self.value_stack[base + i], Value::Poison, "write {v:?} to abs slot {}", base + i);
                    self.value_stack[base + i] = v;
                }
            }
        }
        Ok(())
    }

    // I care not for your stride!
    fn raw_slice_ptr(&mut self, base: Value, physical_offset: usize, physical_count: usize) -> Res<'p, Value> {
        match base {
            Value::InterpAbsStackAddr(addr) => {
                assert!(physical_count <= addr.count);
                Ok(Value::InterpAbsStackAddr(StackAbsoluteRange {
                    first: StackAbsolute(addr.first.0 + physical_offset),
                    count: physical_count,
                }))
            }
            Value::Heap { value, physical_first, .. } => {
                let abs_first = physical_first + physical_offset;
                let abs_last = abs_first + physical_count;
                // Slicing operations are bounds checked.
                let data = unsafe { &*value };
                assert!(data.references > 0 && (abs_first) < data.values.len() && (abs_last) <= data.values.len());
                Ok(Value::Heap {
                    value,
                    physical_first: abs_first,
                    physical_count,
                })
            }
            // Real raw pointers
            // But now there's this weird thing where this is measured in bytes but the others are in stack slots.
            Value::I64(addr) => Ok(Value::I64(addr + physical_offset as i64)),
            _ => err!("Wanted ptr found {:?}", base),
        }
    }

    pub fn deref_ptr(&mut self, arg: Value) -> Res<'p, Values> {
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
            Value::I64(addr) => unsafe {
                let ptr = addr as usize as *mut i64;
                let value = *ptr;
                Ok(Values::One(Value::I64(value)))
            },
            _ => err!("Wanted ptr found {:?}", arg),
        }
    }

    pub fn runtime_builtin(
        &mut self,
        name: &str,
        arg: Values,
        ret_slot_for_suspend: Option<StackAbsoluteRange>,
        program: &mut Program<'p>,
    ) -> Res<'p, Values> {
        logln!("runtime_builtin: {name} {arg:?}");
        let value = match name {
            "panic" => {
                let msg = if let Some(s) = String::deserialize_one(arg.clone()) {
                    s
                } else {
                    format!("{arg:?}")
                };
                err!("Program panicked: \n{msg}\nAt {}", self.log_callstack())
            }
            "assert_eq" => {
                let (a, b) = arg.to_pair()?;
                assert_eq!(a, b, "runtime_builtin:assert_eq");
                program.assertion_count += 1; // sanity check for making sure tests actually ran
                Value::Unit.into()
            }
            "is_comptime" => {
                assert_eq!(arg, Values::One(Value::Unit));
                let when = self.call_stack.last().unwrap().when;
                Value::Bool(when == ExecTime::Comptime).into()
            }
            "is_uninit" => {
                let range = match arg {
                    Values::One(Value::InterpAbsStackAddr(addr)) => &self.value_stack[addr.first.0..addr.first.0 + addr.count],
                    Values::One(Value::Heap {
                        value,
                        physical_first: first,
                        physical_count: count,
                        ..
                    }) => {
                        let data = unsafe { &*value };
                        assert!(data.references > 0 && (first) < data.values.len() && (count) < data.values.len());
                        &data.values[(first)..(first + count)]
                    }
                    _ => err!("Wanted ptr found {:?}", arg),
                };
                let result = range.iter().all(|v| v == &Value::Poison);
                Value::Bool(result).into()
            }
            "size_of" => {
                let ty = program.to_type(arg)?;
                let stride = self.size_of(program, ty);
                Value::I64(stride as i64).into()
            }
            "is_oob_stack" => {
                let addr = arg.to_stack_addr()?;
                Value::Bool(addr.first.0 >= self.value_stack.len()).into()
            }
            "raw_slice" => {
                // last is not included
                let (addr, new_first, new_last) = arg.to_triple()?;
                let (new_first, new_last) = (new_first.to_int()?, new_last.to_int()?);
                assert!(new_first >= 0 && new_last >= 0 && new_first <= new_last, "{new_first}..<{new_last}");
                match addr {
                    Value::InterpAbsStackAddr(addr) => {
                        assert!((new_first as usize) < addr.count && (new_last as usize) <= addr.count);
                        Value::InterpAbsStackAddr(StackAbsoluteRange {
                            first: StackAbsolute(addr.first.0 + new_first as usize),
                            count: (new_last - new_first) as usize,
                        })
                    }
                    Value::Heap {
                        value,
                        physical_first: first,
                        ..
                    } => {
                        let abs_first = first + new_first as usize;
                        let abs_last = first + new_last as usize;
                        // Slicing operations are bounds checked.
                        let data = unsafe { &*value };
                        assert!(
                            data.references > 0
                                && abs_first <= (data.values.len())  // TODO: wrong in general but lets you have zero count
                                && abs_last <= (data.values.len()),
                            "[len={}]{abs_first}..<{abs_last}",
                            data.values.len()
                        );
                        Value::Heap {
                            value,
                            physical_first: abs_first,
                            physical_count: (new_last - new_first) as usize,
                        }
                    }
                    _ => err!("Wanted ptr found {:?}", addr),
                }
                .into()
            }
            "alloc" => {
                let (ty, count) = arg.to_pair()?;
                let (ty, count) = (program.to_type(ty.into())?, count.to_int()?);
                let stride = self.size_of(program, ty);
                assert!(count >= 0);
                let values = vec![Value::Poison; count as usize * stride];
                let value = Box::into_raw(Box::new(InterpBox {
                    references: 1,
                    values,
                    is_constant: false,
                }));
                Value::Heap {
                    value,
                    physical_first: 0,
                    physical_count: count as usize * stride,
                }
                .into()
            }
            "dump_ffi_types" => {
                let msg = program.dump_ffi_types();
                msg.serialize_one()
            }
            "dealloc" => {
                let (ty, ptr, count) = arg.to_triple()?;
                let (ty, count, (ptr, ptr_first, ptr_count)) = (program.to_type(ty.into())?, count.to_int()?, Values::One(ptr).to_heap_ptr()?);
                assert_eq!(self.size_of(program, ty) * count as usize, ptr_count);
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
            "Fn" => {
                // println!("Fn: {:?}", arg);
                let (arg, ret) = arg.to_pair()?;
                let (arg, ret) = (program.to_type(arg.into())?, program.to_type(ret.into())?);
                let ty = program.intern_type(TypeInfo::Fn(FnType { arg, ret }));
                // println!("=> {}", self.program.log_type(ty));
                Value::Type(ty).into()
            }
            "FnPtr" => {
                // println!("Fn: {:?}", arg);
                let (arg, ret) = arg.to_pair()?;
                let (arg, ret) = (program.to_type(arg.into())?, program.to_type(ret.into())?);
                let ty = program.intern_type(TypeInfo::FnPtr(FnType { arg, ret }));
                // println!("=> {}", self.program.log_type(ty));
                Value::Type(ty).into()
            }
            // TODO: remove. make sure tuple syntax always works first tho.
            "Ty" => {
                if let Values::One(Value::Type(ty)) = arg {
                    assert!(false, "Ty arg should be tuple of types not type {:?}", program.log_type(ty));
                }
                // print!("Ty: {:?}", arg);
                let ty = program.to_type(arg)?;
                // println!(" => {:?}", self.program.log_type(ty));
                Value::Type(ty).into()
            }
            "system" => {
                self.fail_on_wasm("fn system")?;
                let mut arg = unwrap!(
                    Vec::<String>::deserialize_one(arg.clone()),
                    "expected Vec<string> not {arg:?}. TODO: support stack slices"
                )
                .into_iter();
                let cmd = unwrap!(arg.next(), "cmd");
                let mut cmd = Command::new(cmd);
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
                let arg = unwrap!(String::deserialize_one(arg.clone()), "expect str not {arg:?}");
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
            "IntType" => {
                let (bit_count, signed) = arg.to_pair()?;
                let (bit_count, signed) = (bit_count.to_int()?, Values::One(signed).to_bool()?);
                let ty = program.intern_type(TypeInfo::Int(crate::ast::IntType { bit_count, signed }));
                Value::Type(ty).into()
            }
            "clone_const" => {
                let (ptr, first, count) = arg.to_heap_ptr()?;
                let ptr = unsafe { &mut *ptr };
                assert!(ptr.is_constant, "clone_const but not const, you could just mutate it");
                let values = ptr.values[first..(first + count)].to_vec();
                Value::new_box(values, false).into()
            }
            "str" => {
                let arg = unwrap!(Ident::deserialize_one(arg.clone()), "expected symbol found {arg:?}");
                let s = self.pool.get(arg).to_string();
                s.serialize_one()
            }
            #[cfg(target_arch = "aarch64")]
            "copy_to_mmap_exec" => {
                let (ptr, _len) = arg.to_pair()?;
                let arg = self.deref_ptr(ptr)?;
                let arg: Vec<_> = arg.vec().into_iter().map(|v| v.to_int().unwrap() as u32).collect();
                let (map, code) = export_ffi::copy_to_mmap_exec(arg);
                let map: usize = Box::leak(map) as *const memmap2::Mmap as usize;
                Values::Many(vec![Value::I64(map as i64), Value::I64(code as i64)])
            }
            "literal_ast" => {
                let (ty, ptr) = arg.to_pair()?;
                let val = self.deref_ptr(ptr)?;
                let mut v = val.vec();
                v.insert(0, ty); // aaaaaa
                let arg = Values::Many(v);

                // aaaaa
                self.suspend(
                    Flag::Literal_Ast.ident(),
                    arg,
                    unwrap!(ret_slot_for_suspend, "interp suspend but no ret slot"),
                )?
            }
            _ => {
                // TODO: since this nolonger checks if its an expected name, you get worse error messages.
                let name = self.pool.intern(name);
                self.suspend(name, arg, unwrap!(ret_slot_for_suspend, "interp suspend but no ret slot"))?
            }
        };
        Ok(value)
    }

    // This does not spark joy...
    // I think I'll be spending a while reading about algebreic effects.
    fn suspend(&mut self, name: Ident<'p>, arg: Values, ret: StackAbsoluteRange) -> Res<'p, Values> {
        self.messages.push(ret);
        err!(CErr::InterpMsgToCompiler(name, arg, ret))
    }

    pub fn resume(&mut self, result: Values, ret: StackAbsoluteRange, program: &mut Program<'p>) -> Res<'p, Values> {
        self.expand_maybe_tuple(result, ret)?;
        self.run_continuation(program)
    }

    fn push_callframe(&mut self, f: FuncId, ret: StackRange, arg: Values, when: ExecTime, program: &Program<'p>) -> Res<'p, ()> {
        let func = self.ready[f].as_ref();
        assert!(func.is_some(), "ICE: tried to call {f:?} but not compiled.");
        // Calling Convention: arguments passed to a function are moved out of your stack.
        let return_slot = self.range_to_index(ret);
        let stack_base = self.value_stack.len(); // Our stack includes the argument but not the return slot.
        self.push_expanded(arg);
        let debug_name = program[f].get_name(self.pool);
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
        debug_assert_ne!(value, Value::Poison, "{}", self.log_callstack());
        value
    }

    /// If slot ranges over multiple, return them as a tuple.
    #[track_caller]
    pub fn take_slots(&mut self, slot: StackRange) -> Values {
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
        let body = self.ready[frame.current_func].as_ref().unwrap();
        body.insts.get(frame.current_ip).unwrap()
    }

    fn update_debug(&mut self) {
        let frame = self.call_stack.last().unwrap();
        let body = self.ready[frame.current_func].as_ref().unwrap();
        // TOOD: @track_caller so you don't just get an error report on the forward declare of assert_eq all the time. HACK:
        if !matches!(body.insts[frame.current_ip], Bc::CallBuiltin { .. }) {
            self.last_loc = body.debug.get(frame.current_ip).map(|i| i.src_loc);
        }
    }

    fn current_fn_body(&self) -> &FnBody {
        let frame = self.call_stack.last().unwrap();
        self.ready[frame.current_func].as_ref().expect("jit current function")
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
                assert!(ptr.references > 0 && first < ptr.values.len() && (first + count) <= ptr.values.len());
                if ptr.is_constant {
                    err!("Illegal mutation of baked constant",)
                }
                let values: Vec<_> = value.into();
                assert_eq!(values.len(), count);
                for (i, entry) in values.into_iter().enumerate() {
                    ptr.values[first + i] = entry;
                }
                Value::Unit
            }
            Value::I64(addr) => unsafe {
                let ptr = addr as usize as *mut i64;
                let value = value.single()?.to_int()?;
                *ptr = value;
                Value::Unit
            },
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
                    } => {
                        let values = unsafe { &mut *value };
                        outln!(ShowPrint, "{}{:?}", "=".repeat(depth), &values.values[first..(first + count)]);

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

impl<'p> Executor<'p> for Interp<'p> {
    type SavedState = (usize, usize);

    fn compile_func(&mut self, program: &Program<'p>, f: FuncId) -> Res<'p, ()> {
        let init_heights = (self.call_stack.len(), self.value_stack.len());
        let result = EmitBc::compile(program, &mut self.ready, f);
        if result.is_ok() {
            let end_heights = (self.call_stack.len(), self.value_stack.len());
            assert_eq!(init_heights, end_heights, "bad stack size");
        }
        result
    }

    fn run_func(&mut self, program: &mut Program<'p>, f: FuncId, arg: Values, when: ExecTime) -> Res<'p, Values> {
        let ret = program[f].unwrap_ty().ret;
        assert!(!ret.is_any() && !ret.is_unknown());
        let size = self.size_of(program, ret);
        self.run(f, arg, when, size, program)
    }

    fn run_continuation(&mut self, program: &mut Program<'p>, response: Values) -> Res<'p, Values> {
        let ret = unwrap!(self.messages.pop(), "nothing to resume");
        self.resume(response, ret, program)
    }

    fn size_of(&mut self, program: &Program<'p>, ty: TypeId) -> usize {
        self.ready.sizes.slot_count(program, ty)
    }

    fn is_ready(&self, f: FuncId) -> bool {
        self.ready.ready.len() > f.0 && self.ready[f].is_some()
    }

    fn dump_repr(&self, _program: &Program<'p>, f: FuncId) -> String {
        self.ready[f]
            .as_ref()
            .map(|func| func.log(self.pool))
            .unwrap_or_else(|| String::from("NOT READY"))
    }

    fn tag_error(&self, err: &mut CompileError<'p>) {
        err.value_stack = self.value_stack.clone();
        err.call_stack = self.log_callstack();
        err.loc = self.last_loc.or(err.loc);
    }

    fn mark_state(&self) -> Self::SavedState {
        (self.call_stack.len(), self.value_stack.len())
    }

    fn restore_state(&mut self, (cs, vs): Self::SavedState) {
        drops(&mut self.call_stack, cs);
        drops(&mut self.value_stack, vs);
    }

    fn deref_ptr_pls(&mut self, slot: Value) -> Res<'p, Values> {
        self.deref_ptr(slot)
    }

    fn to_interp(self: Box<Self>) -> Option<Interp<'p>> {
        Some(*self)
    }
}

fn drops<T>(vec: &mut Vec<T>, new_len: usize) {
    for _ in 0..(vec.len() - new_len) {
        vec.pop();
    }
}

impl<'p> Values {
    #[track_caller]
    fn to_bool(self) -> Res<'p, bool> {
        if let Values::One(Value::Bool(v)) = self {
            Ok(v)
        } else {
            err!(CErr::TypeError("bool", self))
        }
    }

    #[track_caller]
    fn to_seq(self) -> Res<'p, Vec<Value>> {
        Ok(self.vec())
    }

    #[track_caller]
    fn to_triple(self) -> Res<'p, (Value, Value, Value)> {
        let values = self.vec();
        assert_eq!(values.len(), 3, "arity {:?}", values);
        Ok((values[0], values[1], values[2]))
    }

    #[track_caller]
    fn to_func(self) -> Res<'p, FuncId> {
        if let Values::One(Value::GetFn(id)) = self {
            Ok(id)
        } else {
            err!(CErr::TypeError("AnyFunc", self))
        }
    }

    #[track_caller]
    pub fn to_pair(self) -> Res<'p, (Value, Value)> {
        let values = self.to_seq()?;
        assert_eq!(values.len(), 2, "arity {:?}", values);
        Ok((values[0], values[1]))
    }

    #[track_caller]
    fn to_stack_addr(self) -> Res<'p, StackAbsoluteRange> {
        if let Values::One(Value::InterpAbsStackAddr(r)) = self {
            Ok(r)
        } else {
            err!(CErr::TypeError("StackAddr", self))
        }
    }

    fn to_heap_ptr(self) -> Res<'p, (*mut InterpBox, usize, usize)> {
        if let Values::One(Value::Heap {
            value,
            physical_first: first,
            physical_count: count,
        }) = self
        {
            Ok((value, first, count))
        } else {
            err!(CErr::TypeError("Heap", self))
        }
    }
}

impl<'p> Value {
    #[track_caller]
    pub fn to_int(self) -> Res<'p, i64> {
        if let Value::I64(r) = self {
            Ok(r)
        } else if let Value::Symbol(r) = self {
            // TODO: have a special unwrap method for this
            Ok(r as i64)
        } else {
            err!(CErr::TypeError("i64", self.into()))
        }
    }
}
