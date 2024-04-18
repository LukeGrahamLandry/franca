//! TODO: i know this is remarkably bad codegen.
//! @c_call means https://en.wikipedia.org/wiki/Calling_convention#ARM_(A64)

#![allow(non_upper_case_globals)]
#![allow(unused)]

use crate::ast::{Flag, FnType, Func, FuncId, TypeId};
use crate::bc::{Bc, BcReady, InterpBox, StackOffset, StackRange, Value, Values};
use crate::compiler::{Compile, ExecTime, Res};
use crate::{ast::Program, bc::FnBody};
use crate::{bootstrap_gen::*, unwrap};
use crate::{err, logging::PoolLog};
use std::arch::asm;
use std::cell::UnsafeCell;
use std::mem::transmute;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
struct SpOffset(usize);

struct BcToAsm<'z, 'p, 'a> {
    compile: &'z mut Compile<'a, 'p>,
    slots: Vec<Option<SpOffset>>,
    open_slots: Vec<(SpOffset, usize)>,
    next_slot: SpOffset,
    f: FuncId,
    wip: Vec<FuncId>,
    when: ExecTime, // make recursion work
}

const x0: i64 = 0;
const x1: i64 = 1;
const x2: i64 = 2;
const x3: i64 = 3;
const x4: i64 = 4;
const x5: i64 = 5;
// const x6: i64 = 6;
// const x7: i64 = 7;
const x8: i64 = 8;
// const x9: i64 = 9;
/// c_call: "intra-procedure-call scratch register"
const x16: i64 = 16;
/// c_call: "intra-procedure-call scratch register"
const x17: i64 = 17;

const x21: i64 = 21;
const fp: i64 = 29;
const lr: i64 = 30;
const sp: i64 = 31;
// const W32: i64 = 0b0;
const X64: i64 = 0b1;

pub fn emit_aarch64<'p>(compile: &mut Compile<'_, 'p>, f: FuncId, when: ExecTime) -> Res<'p, ()> {
    compile.aarch64.reserve(compile.program.funcs.len());
    let mut a = BcToAsm::new(compile, when);
    a.compile(f)?;
    assert!(a.wip.is_empty());
    Ok(())
}

macro_rules! cc_reg {
    ($self:expr, $slots:expr, $skip_units:expr, $first_int:expr, $is_int:expr, $is_float:expr) => {{
        let mut floats = 0;
        let mut ints = $first_int;
        #[allow(clippy::redundant_closure_call)]
        for i in $slots {
            let slot = StackOffset(i);
            let ty = $self.slot_type(slot);
            if $skip_units && ty.is_unit() {
                continue;
            }
            if ty == TypeId::f64() {
                $is_float(floats, slot);
                floats += 1;
            } else {
                $is_int(ints as i64, slot);
                ints += 1;
            }
        }
    }};
}

impl<'z, 'p, 'a> BcToAsm<'z, 'p, 'a> {
    fn new(compile: &'z mut Compile<'a, 'p>, when: ExecTime) -> Self {
        Self {
            compile,
            slots: vec![],
            open_slots: vec![],
            next_slot: SpOffset(0),
            f: FuncId(0), // TODO: bad
            wip: vec![],
            when,
        }
    }

    // TODO: i cant keep copy pasting this shape. gonna cry.
    fn compile(&mut self, f: FuncId) -> Res<'p, ()> {
        self.compile.aarch64.reserve(f.0);
        if self.compile.aarch64.get_fn(f).is_some() || self.wip.contains(&f) {
            return Ok(());
        }
        self.compile.aarch64.make_write();

        self.wip.push(f);
        if let Some(template) = self.compile.program[f].any_reg_template {
            // TODO: want to use asm instead of interp so will need to compile.
            //       but really it would be better to have the separation of compiling for host vs target so I could cross compile once I support other architectures.
            // self.compile(template)?;
        } else if self.compile.program[f].comptime_addr.is_some() && self.compile.program[f].jitted_code.is_none() {
            // println!("Skip asm for {f:?}");
        } else {
            let callees = self.compile.program[f].wip.as_ref().unwrap().callees.clone();
            for c in callees {
                // TODO: change
                if c.1 != ExecTime::Comptime {
                    self.compile(c.0)?;
                }
            }

            let func = &self.compile.program[f];
            if let Some(insts) = func.jitted_code.as_ref() {
                // TODO: i dont like that the other guy leaked the box for the jitted_code ptr. i'd rather everything share the one Jitted instance.
                // TODO: this needs to be aware of the distinction between comptime and runtime target.
                for op in insts {
                    self.compile.aarch64.push(*op as i64);
                }
            } else {
                self.bc_to_asm(f)?;
            }
            self.compile.aarch64.save_current(f);
        }

        self.wip.retain(|c| c != &f);
        Ok(())
    }

    #[track_caller]
    fn slot_type(&self, slot: StackOffset) -> TypeId {
        self.compile.ready[self.f].as_ref().unwrap().slot_types[slot.0]
    }

    fn bc_to_asm(&mut self, f: FuncId) -> Res<'p, ()> {
        let func = self.compile.ready[f].as_ref().unwrap();
        // println!("{}", func.log(self.compile.program.pool));
        let ff = &self.compile.program[func.func];
        let is_flat_call = ff.has_tag(Flag::Flat_Call);
        self.f = f;
        self.next_slot = SpOffset(0);
        self.slots.clear();
        self.slots.extend(vec![None; func.stack_slots]);
        self.open_slots.clear();
        let is_c_call = ff.has_tag(Flag::C_Call);

        self.compile.aarch64.push(sub_im(X64, sp, sp, 16, 0));
        self.compile.aarch64.push(stp_so(X64, fp, lr, sp, 0)); // save our return address
        self.compile.aarch64.push(add_im(X64, fp, sp, 0, 0)); // Note: normal mov encoding can't use sp
        self.compile.aarch64.push(brk(0));
        let reserve_stack = self.compile.aarch64.prev();

        let mut release_stack = vec![];
        let arg_range = func.arg_range;
        // if is_c_call {
        assert!(func.arg_range.count <= 8, "c_call only supports 8 arguments. TODO: pass on stack");

        let mut flat_result = None;

        if is_flat_call {
            // (x0=compiler, x1=arg_ptr, x2=arg_len, x3=ret_ptr, x4=ret_len)
            assert!(!is_c_call);
            let ret_size = self.compile.ready.sizes.slot_count(self.compile.program, ff.finished_ret.unwrap());

            // Runtime check that caller agrees on type sizes.
            // TODO: This is not nessisary if we believe in our hearts that there are no compiler bugs...
            assert!(arg_range.count < (1 << 12));
            assert!(ret_size < (1 << 12));
            self.compile.aarch64.push(cmp_im(X64, x2, arg_range.count as i64, 0));
            self.compile.aarch64.push(b_cond(2, CmpFlags::EQ as i64)); // TODO: do better
            self.compile.aarch64.push(brk(456));
            self.compile.aarch64.push(cmp_im(X64, x4, ret_size as i64, 0));
            self.compile.aarch64.push(b_cond(2, CmpFlags::EQ as i64)); // TODO: do better
            self.compile.aarch64.push(brk(456));

            // Save the result pointer.
            flat_result = Some(self.next_slot);
            self.compile.aarch64.push(str_uo(X64, x3, sp, self.next_slot.0 as i64 / 8));
            self.next_slot = SpOffset(self.next_slot.0 + 8);

            // Copy arguments into their stack slots
            let on_stack = self.find_many(arg_range, true);
            for i in 0..arg_range.count {
                self.compile.aarch64.push(ldr_uo(X64, x5, x1, i as i64));
                self.set_slot(x5, arg_range.offset(i));
            }
        } else {
            assert!(
                !ff.has_tag(Flag::Ct),
                "compiler context is implicitly passed as first argument for @ct builtins."
            );
            self.compile.program[func.func].add_tag(Flag::C_Call); // Make sure we don't try to emit as @flat_call later
                                                                   // TODO: not skipping unit fixes allow_create debug check for enum init when payload is unit.
                                                                   //       in general need to just handle unit better so im not special casing everywhere.
            cc_reg!(self, arg_range, false, 0, |ints, slot| self.set_slot(ints, slot), |floats, slot| self
                .set_slot_f(floats, slot));
        }

        let mut patch_cbz = vec![];
        let mut patch_b = vec![];

        let func = self.compile.ready[f].as_ref().unwrap();
        for i in 0..func.insts.len() {
            let inst = &(self.compile.ready[f].as_ref().unwrap().insts[i].clone());
            self.compile.aarch64.mark_next_ip();
            match inst {
                &Bc::MarkContiguous(slots, _) => {
                    self.find_many(slots, true);
                }
                &Bc::LastUse(slots) => {
                    self.release_many(slots);
                }
                Bc::NoCompile => unreachable!(),
                Bc::CallDynamic { .. } => todo!(),
                &Bc::CallSplit { rt, ct, ret, arg } => {
                    let f = if self.when == ExecTime::Comptime { ct } else { rt };
                    self.call_direct(f, ret, arg)?;
                }
                &Bc::CallDirect { f, ret, arg } => {
                    self.call_direct(f, ret, arg)?;
                }
                Bc::CallBuiltin { name, .. } => todo!("{}", self.compile.program.pool.get(*name)),
                Bc::LoadConstant { slot, value } => match value {
                    &Value::F64(n) => {
                        // Don't care that its a float since we always write it to memory anyway.
                        self.load_imm(x0, n);
                        self.set_slot(x0, *slot);
                    }
                    Value::I64(n) => {
                        self.load_imm(x0, u64::from_le_bytes(n.to_le_bytes()));
                        self.set_slot(x0, *slot);
                    }
                    // These only make sense during comptime execution, but they're also really just numbers.
                    Value::OverloadSet(i) | Value::GetFn(FuncId(i)) => {
                        self.load_imm(x0, *i as u64);
                        self.set_slot(x0, *slot);
                    }
                    Value::Symbol(i) | Value::Type(TypeId(i)) => {
                        self.load_imm(x0, *i as u64);
                        self.set_slot(x0, *slot);
                    }
                    &Value::Bool(b) => {
                        self.compile.aarch64.push(movz(X64, x0, b as i64, 0));
                        self.set_slot(x0, *slot);
                    }
                    Value::Unit => {}
                    Value::Poison => todo!(),
                    Value::InterpAbsStackAddr(_) => todo!(),
                    &Value::Heap {
                        value,
                        physical_first,
                        physical_count,
                    } => {
                        let ty = self.slot_type(*slot);
                        assert!(
                            self.compile.program.unptr_ty(ty).is_some(),
                            "Expected Value::Heap to be ptr, not {:?}",
                            self.compile.program.log_type(ty)
                        );
                        // TODO: this needs to be different when I actually want to emit an executable.
                        let ptr = self.compile.aarch64.constants.copy_heap(value, physical_first, physical_count);
                        self.load_imm(x0, ptr as u64);
                        self.set_slot(x0, *slot);
                    }
                    &Value::GetNativeFnPtr(f) => {
                        // TODO: use adr+adrp instead of an integer.
                        // TODO: do linker-ish things to allow forward references.
                        //       actually the way i do that rn is with the dispatch table so could just use that for now.
                        let ptr = unwrap!(self.compile.aarch64.get_fn(f), "GetNativeFnPtr not compiled yet. TODO: mutual recursion.");
                        self.load_imm(x0, ptr.as_ptr() as u64);
                        self.set_slot(x0, *slot);
                    }
                    Value::SplitFunc { ct, rt } => todo!(),
                },
                &Bc::JumpIf { cond, true_ip, false_ip } => {
                    self.get_slot(x0, cond);
                    self.compile.aarch64.push(brk(0));
                    assert_eq!(true_ip, i + 1);
                    patch_cbz.push((self.compile.aarch64.prev(), false_ip));
                }
                &Bc::Goto { ip } => {
                    self.compile.aarch64.push(brk(0));
                    patch_b.push((self.compile.aarch64.prev(), ip));
                }
                &Bc::Ret(slots) => {
                    if is_flat_call {
                        assert!(!is_c_call);
                        // Retrive result address
                        self.compile.aarch64.push(ldr_uo(X64, x0, sp, flat_result.unwrap().0 as i64 / 8));
                        for i in 0..slots.count {
                            self.get_slot(x1, slots.offset(i));
                            self.compile.aarch64.push(str_uo(X64, x1, x0, i as i64));
                        }
                    } else {
                        match slots.count {
                            0 => {}
                            1..=7 => {
                                cc_reg!(self, slots, true, 0, |ints, slot| self.get_slot(ints, slot), |floats, slot| self
                                    .get_slot_f(floats, slot));
                            }
                            _ => err!("c_call only supports 7 return value. TODO: structs",),
                        }
                    }

                    // Leave holes for stack fixup code.
                    self.compile.aarch64.push(brk(0));
                    let a = self.compile.aarch64.prev();
                    self.compile.aarch64.push(brk(0));
                    let b = self.compile.aarch64.prev();
                    self.compile.aarch64.push(brk(0));
                    release_stack.push((a, b, self.compile.aarch64.prev()));
                    // Do the return
                    self.compile.aarch64.push(ret(()));
                }
                // Note: drop is on the value, we might immediately write to that slot and someone might have a pointer to it.
                Bc::Drop(_) | Bc::DebugMarker(_, _) | Bc::DebugLine(_) => {}
                &Bc::Move { from, to } => {
                    // TODO: it might be part of a struct?
                    // if !self.slot_is_var(from) && !self.slot_is_var(to) {
                    //     self.slots[to.0] = self.slots[from.0].take();
                    // } else {
                    self.get_slot(x0, from);
                    self.set_slot(x0, to);
                    // }
                }
                &Bc::Clone { from, to } => {
                    self.get_slot(x0, from);
                    self.set_slot(x0, to);
                }
                Bc::CloneRange { from, to } | Bc::MoveRange { from, to } => {
                    for i in 0..from.count {
                        let to_unit = self.slot_type(StackOffset(to.first.0 + i)).is_unit();
                        let from_unit = self.slot_type(StackOffset(from.first.0 + i)).is_unit();
                        if from_unit && to_unit {
                            continue;
                        }
                        if from_unit && !to_unit {
                            self.load_imm(x0, 123); // HACK for enum padding.
                            self.set_slot(x0, StackOffset(to.first.0 + i));
                            continue;
                        }
                        self.get_slot(x0, StackOffset(from.first.0 + i));
                        self.set_slot(x0, StackOffset(to.first.0 + i));
                    }
                }
                &Bc::AbsoluteStackAddr { of, to } => {
                    let offset = self.find_many(of, false);
                    self.compile.aarch64.push(add_im(X64, x0, sp, offset.0 as i64, 0));
                    self.set_slot(x0, to);
                }
                &Bc::SlicePtr { base, offset, ret, .. } => {
                    self.get_slot(x0, base); // x0 = ptr
                    self.compile.aarch64.push(add_im(X64, x0, x0, (offset * 8) as i64, 0)); // x0 += offset * size_of(i64)
                    self.set_slot(x0, ret);
                }
                &Bc::Load { from, to } => {
                    self.get_slot(x0, from); // x0 = ptr
                    for i in 0..to.count {
                        self.compile.aarch64.push(ldr_uo(X64, x1, x0, i as i64)); // x1 = *x0[i]
                        self.set_slot(x1, StackOffset(to.first.0 + i)); // out = x1
                    }
                }
                &Bc::Store { to, from } => {
                    self.get_slot(x0, to); // x0 = ptr
                    for i in 0..from.count {
                        self.get_slot(x1, StackOffset(from.first.0 + i)); // x1 = val
                        self.compile.aarch64.push(str_uo(X64, x1, x0, i as i64));
                        // *x0[i] = val
                    }
                }
                &Bc::TagCheck { enum_ptr, value } => {
                    self.get_slot(x0, enum_ptr); // x0 = ptr
                    self.compile.aarch64.push(ldr_uo(X64, x0, x0, 0)); // x0 = *x0 = tag
                    self.compile.aarch64.push(cmp_im(X64, x0, value, 0));
                    self.compile.aarch64.push(b_cond(2, CmpFlags::EQ as i64)); // TODO: do better
                    self.compile.aarch64.push(brk(456));
                }
                Bc::CallC { f, arg, ret, ty, comp_ctx } => {
                    assert!(!*comp_ctx, "flat call needs special handling");
                    self.get_slot(x16, *f);
                    self.dyn_c_call(*arg, *ret, *ty, *comp_ctx, |s| s.compile.aarch64.push(br(x16, 1)));
                    self.release_one(*f);
                }
                Bc::Unreachable => {
                    self.compile.aarch64.push(brk(123));
                }
            }
        }
        for (inst, false_ip) in patch_cbz {
            let offset = self.compile.aarch64.offset_words_inst_ip(inst, false_ip);
            self.compile.aarch64.patch(inst, cbz(X64, signed_truncate(offset, 19), x0));
        }
        for (from_inst, to_ip) in patch_b {
            let dist = self.compile.aarch64.offset_words_inst_ip(from_inst, to_ip);
            debug_assert_ne!(dist, 0, "while(1);");
            self.compile.aarch64.patch(from_inst, b(signed_truncate(dist, 26), 0));
        }

        let mut slots = self.next_slot.0; //self.compile.ready[self.f].as_ref().unwrap().stack_slots * 8;
        assert!(slots < 4096, "not enough bits to refer to all slots");
        if slots % 16 != 0 {
            slots += 16 - (slots % 16); // play by the rules
        }
        self.compile.aarch64.patch(reserve_stack, sub_im(X64, sp, sp, slots as i64, 0));
        for (fst, snd, thd) in release_stack {
            self.compile.aarch64.patch(fst, add_im(X64, sp, fp, 0, 0)); // Note: normal mov encoding can't use sp
            self.compile.aarch64.patch(snd, ldp_so(X64, fp, lr, sp, 0)); // get our return address
            self.compile.aarch64.patch(thd, add_im(X64, sp, sp, 16, 0));
        }
        Ok(())
    }

    fn set_slot(&mut self, src_reg: i64, slot: StackOffset) {
        let offset = self.find_one(slot, true);
        self.compile.aarch64.push(str_uo(X64, src_reg, sp, offset.0 as i64 / 8));
    }

    #[track_caller]
    fn get_slot(&mut self, dest_reg: i64, slot: StackOffset) {
        let offset = self.find_one(slot, false);
        self.compile.aarch64.push(ldr_uo(X64, dest_reg, sp, offset.0 as i64 / 8));
    }

    fn set_slot_f(&mut self, src_reg: i64, slot: StackOffset) {
        let offset = self.find_one(slot, true);
        self.compile.aarch64.push(f_str_uo(X64, src_reg, sp, offset.0 as i64 / 8));
    }

    #[track_caller]
    fn get_slot_f(&mut self, dest_reg: i64, slot: StackOffset) {
        let offset = self.find_one(slot, false);
        self.compile.aarch64.push(f_ldr_uo(X64, dest_reg, sp, offset.0 as i64 / 8));
    }

    fn find_many(&mut self, slot: StackRange, allow_create: bool) -> SpOffset {
        if slot.count == 1 {
            return self.find_one(slot.single(), allow_create);
        }
        if let Some(slot) = self.slots[slot.first.0] {
            return slot;
        }

        debug_assert!(allow_create);
        for (i, &(_, size)) in self.open_slots.iter().enumerate().rev() {
            if size == slot.count * 8 {
                let mut found = self.open_slots.remove(i);
                let out = found.0;
                for i in slot {
                    debug_assert!(self.slots[i].is_none());
                    self.slots[i] = Some(found.0);
                    found.0 .0 += 8;
                }
                return out;
            }
        }

        let made = self.next_slot;
        for i in slot {
            debug_assert!(self.slots[i].is_none());
            self.slots[i] = Some(self.next_slot);
            self.next_slot.0 += 8;
        }
        made
    }

    fn release_many(&mut self, slot: StackRange) {
        if slot.count == 1 {
            self.release_one(slot.single())
        } else {
            let mut first = None;
            let mut prev: Option<SpOffset> = None;
            for i in slot {
                let r = self.slots[i].take().unwrap();
                if first.is_none() {
                    first = Some(r);
                } else {
                    assert_eq!(prev.unwrap().0 + 8, r.0)
                }
                prev = Some(r);
            }
            self.open_slots.push((first.unwrap(), slot.count * 8));
        }
    }

    #[track_caller]
    fn find_one(&mut self, slot: StackOffset, allow_create: bool) -> SpOffset {
        if let Some(slot) = self.slots[slot.0] {
            slot
        } else if let Some((made, size)) = self.open_slots.pop() {
            debug_assert!(allow_create, "!allow_create {slot:?}");
            if size > 8 {
                self.open_slots.push((SpOffset(made.0 + 8), size - 8));
            }
            self.slots[slot.0] = Some(made);
            made
        } else {
            debug_assert!(allow_create, "!allow_create {slot:?}");
            let made = self.next_slot;
            self.slots[slot.0] = Some(made);
            self.next_slot.0 += 8;
            made
        }
    }

    fn release_one(&mut self, slot: StackOffset) {
        if let Some(slot) = self.slots[slot.0].take() {
            self.open_slots.push((slot, 8));
        }
    }

    fn load_imm(&mut self, reg: i64, mut value: u64) {
        let bottom = u16::MAX as u64;
        self.compile.aarch64.push(movz(X64, reg, (value & bottom) as i64, 0));
        value >>= 16;
        if value != 0 {
            for shift in 1..4 {
                self.compile.aarch64.push(movk(X64, reg, (value & bottom) as i64, shift));
                value >>= 16;
                if value == 0 {
                    break;
                }
            }
        }
    }

    fn dyn_c_call(&mut self, arg: StackRange, ret: StackRange, _: FnType, comp_ctx: bool, do_call: impl FnOnce(&mut Self)) {
        let reg_offset = if comp_ctx { 1 } else { 0 }; // for secret args like comp_ctx
        assert!(arg.count <= 7, "indirect c_call only supports 7 arguments. TODO: pass on stack");
        assert!(ret.count <= 7, "indirect c_call only supports 7 returns. TODO: pass on stack");

        cc_reg!(self, arg, true, reg_offset, |ints, slot| self.get_slot(ints, slot), |floats, slot| self
            .get_slot_f(floats, slot));

        if comp_ctx {
            // TODO: this has gotta be UB
            let c = self.compile as *const Compile as u64;
            let p = &self.compile.program as *const &mut Program as u64;
            debug_assert_eq!(c, p, "need repr c");
            self.load_imm(x0, c);
        }

        do_call(self);
        self.release_many(arg); // TODO: this assumes they're not variables? are we sure args always make a copy? apperently since this works.

        cc_reg!(self, ret, true, 0, |ints, slot| self.set_slot(ints, slot), |floats, slot| self
            .set_slot_f(floats, slot));
    }

    // TODO: i want to do this though asm but that means i need to bootstrap more stuff.
    fn emit_any_reg(&mut self, template: FuncId, registers: Vec<i64>) -> Vec<i64> {
        todo!()
        // let mut out = Vec::<i64>::new();
        // extern "C" fn consume(ops: &mut Vec<i64>, op: i64) {
        //     ops.push(op);
        // }
        // let arg = (((&mut out) as *mut Vec<i64> as usize, consume as usize), registers).serialize_one();
        // self.compile.ready.run(template, arg, ExecTime::Comptime, 1, self.compile.program).unwrap();
        // out
    }

    fn _emit_any_reg(&mut self, template: FuncId, registers: Vec<i64>) -> Vec<i64> {
        todo!()
        // let f = self.compile.aarch64.get_fn(template).unwrap().as_ptr();
        // // TODO: this relies on my slice layout. use ffi types.
        // type TemplateFn = extern "C" fn(&mut Vec<i64>, extern "C" fn(&mut Vec<i64>, i64), *const i64, usize);
        // let f: TemplateFn = unsafe { transmute(f) };
        // let mut out = Vec::<i64>::new();
        // extern "C" fn consume(ops: &mut Vec<i64>, op: i64) {
        //     ops.push(op);
        // }
        // self.compile.aarch64.make_exec();
        // let indirect_fns = self.compile.aarch64.get_dispatch();
        // unsafe {
        //     asm!(
        //     "mov x21, {fns}",
        //     fns = in(reg) indirect_fns,
        //     out("x21") _
        //     );
        // }
        // f(&mut out, consume, registers.as_ptr(), registers.len());
        // self.compile.aarch64.make_write();
        // // TODO: cache these so I don't have to keep doing sys-calls to toggle the permissions every call?
        // out
    }

    fn slot_is_var(&self, slot: StackOffset) -> bool {
        self.compile.ready[self.f].as_ref().unwrap().slot_is_var.get(slot.0)
    }

    fn call_direct(&mut self, f: FuncId, ret: StackRange, arg: StackRange) -> Res<'p, ()> {
        let target = &self.compile.program[f];
        let target_c_call = target.has_tag(Flag::C_Call);
        let comp_ctx = target.has_tag(Flag::Ct);
        if let Some(template) = target.any_reg_template {
            // TODO: this is just a POC
            let registers = vec![0, 1, 0];
            let ops = self.emit_any_reg(template, registers);
            for i in 0..arg.count {
                if self.slot_type(StackOffset(arg.first.0 + i)).is_unit() {
                    continue;
                }
                self.get_slot(i as i64, StackOffset(arg.first.0 + i));
            }
            self.release_many(arg);
            assert_eq!(ret.count, 1);
            for op in ops {
                // println!("{op:#05x}, ");
                self.compile.aarch64.push(op);
            }

            if !self.slot_type(ret.single()).is_unit() {
                self.set_slot(x0, ret.single());
            }
        } else if target.has_tag(Flag::Flat_Call) {
            // (compiler, arg_ptr, arg_len_i64s, ret_ptr, ret_len_i64s)
            assert!(comp_ctx, "Flat call is only supported for calling into the compiler");
            assert!(!target.has_tag(Flag::C_Call), "multiple calling conventions doesn't make sense");

            let addr = target
                .comptime_addr
                .unwrap_or_else(|| self.compile.aarch64.get_fn(f).unwrap().as_ptr() as u64);
            let arg_offset = self.find_many(arg, false);
            let ret_offset = self.find_many(ret, true);
            let c = self.compile as *const Compile as u64;
            self.load_imm(x0, c);
            debug_assert!(arg_offset.0 < 4096 && ret_offset.0 < 4096);
            self.compile.aarch64.push(add_im(X64, x1, sp, arg_offset.0 as i64, 0));
            self.load_imm(x2, arg.count as u64);
            self.compile.aarch64.push(add_im(X64, x3, sp, ret_offset.0 as i64, 0));
            self.load_imm(x4, ret.count as u64);
            self.branch_with_link(f);
            self.release_many(arg); // Note: release after to make sure they don't alias ret which might not be what the callee is expecting (even tho it would be fine for current uses).
        } else {
            self.dyn_c_call(arg, ret, target.unwrap_ty(), comp_ctx, |s| s.branch_with_link(f));
            self.compile.program[f].add_tag(Flag::C_Call); // Make sure we don't try to emit as @flat_call later
        }
        Ok(())
    }

    fn branch_with_link(&mut self, f: FuncId) {
        // If we already emitted the target function, can just branch there directly.
        // This covers the majority of cases because I try to handle callees first.
        // Note: checking this before comptime_addr means we handle inline asm as a normal function.
        if let Some(bytes) = self.compile.aarch64.get_fn(f) {
            let mut offset = bytes.as_ptr() as i64 - self.compile.aarch64.get_current() as i64;
            debug_assert!(offset % 4 == 0, "instructions are u32");
            offset /= 4;
            assert!(offset.abs() < (1 << 25), "can't jump that far"); // TODO: use adr/adrp
            offset = signed_truncate(offset, 26);
            self.compile.aarch64.push(b(offset, 1));
            return;
        }

        // TODO: maybe its prefereable to use the dispatch table even when its const because then runtime code could setup the pointers with dlopen,
        //       and could reuse the same code for comptime and runtime.
        if let Some(bytes) = self.compile.program[f].comptime_addr {
            debug_assert!(self.compile.program[f].jitted_code.is_none(), "inline asm should be emitted normally");
            let mut offset = bytes as i64 - self.compile.aarch64.get_current() as i64;
            debug_assert!(offset % 4 == 0, "instructions are u32");
            offset /= 4;

            // If its a comptime_addr into the compiler, (which happens a lot because of assert_eq and tag_value),
            //     aslr might be our friend and just put the mmaped pages near where it originally loaded the compiler.
            if offset.abs() < (1 << 25) {
                offset = signed_truncate(offset, 26);
                self.compile.aarch64.push(b(offset, 1));
                return;
            } else {
                // If its a comptime_addr into libc, its probably far away, but that's fine, we're not limited to one instruction.
                let f = &self.compile.program[f];
                self.load_imm(x16, bytes);
                // fall though to finish the indirect call
            }
        } else {
            // It's a function we haven't emitted yet, so we don't know where to jump to.
            // The normal solution would be punt and let the linker deal with it or go back later and patch it ourselves.
            // But for now, I just spend a register on having a dispatch table and do an indirect call through that.
            // TODO: have a mapping. funcs take up slots even if never indirect called.
            assert!(f.0 < 4096);
            self.compile.aarch64.push(ldr_uo(X64, x16, x21, f.0 as i64));
        }

        self.compile.aarch64.push(br(x16, 1))
    }
}

use crate::ffi::InterpSend;
#[cfg(target_arch = "aarch64")]
pub use jit::Jitted;

#[derive(Default)]
pub struct ConstBytes {
    /// Safety: the asm code will alias these pointers so be careful. TODO: does it have to be a *const [UnsafeCell<i64>]?
    constants: Vec<UnsafeCell<Vec<i64>>>,
}

impl ConstBytes {
    pub fn store_to_ints<'a>(&mut self, values: impl Iterator<Item = &'a Value>) -> Vec<i64> {
        let mut out = vec![];
        for value in values {
            self.write_int_copy(value, &mut out);
        }
        out
    }

    pub fn store<'a>(&mut self, values: impl Iterator<Item = &'a Value>) -> *const u8 {
        let mut out = self.store_to_ints(values);
        let ptr = out.as_ptr() as *const u8;
        self.constants.push(UnsafeCell::new(out));
        ptr
    }

    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    pub fn copy_heap(&mut self, value: *mut InterpBox, first: usize, count: usize) -> *const u8 {
        let value = unsafe { &*value };
        // No longer true since now i do seialization stuff so it might have been mutable before... idk. the whole thing's a giant hack
        // assert!(value.is_constant, "wont be shared by reference so only makes sense for constants?");
        let values = &value.values[first..first + count];
        self.store(values.iter())
    }

    pub fn write_int_copy(&mut self, value: &Value, out: &mut Vec<i64>) {
        match value {
            Value::SplitFunc { ct, rt } => todo!(),
            Value::F64(_) => todo!(),
            &Value::I64(i) => out.push(i),
            &Value::Bool(i) => out.push(i as i64),
            &Value::OverloadSet(i) | &Value::GetFn(FuncId(i)) => out.push(i as i64),
            &Value::Symbol(i) | &Value::Type(TypeId(i)) => out.push(i as i64),
            Value::Unit => out.push(0), // TODO
            Value::Poison | Value::InterpAbsStackAddr(_) => unreachable!(),
            Value::GetNativeFnPtr(_) => todo!(),
            &Value::Heap {
                value,
                physical_first,
                physical_count,
            } => {
                let ptr = self.copy_heap(value, physical_first, physical_count);
                out.push(ptr as usize as i64);
            }
        }
    }
}

//#[cfg(target_arch = "aarch64")]
pub mod jit {
    use crate::ast::FuncId;
    use crate::bc_to_asm::ConstBytes;
    use crate::bootstrap_gen::brk;
    use std::cell::UnsafeCell;
    use std::ptr::null;
    use std::slice;

    pub struct Jitted {
        map_mut: Option<memmap2::MmapMut>,
        map_exec: Option<memmap2::Mmap>,
        /// This is redundant but is the pointer used for actually calling functions and there aren't that many bits in the instruction,
        /// so I don't want to spend one doubling to skip lengths.
        dispatch: Vec<*const u8>,
        ranges: Vec<*const [u8]>,
        current_start: *const u8,
        next: *mut u8,
        ip_to_inst: Vec<*const u8>,
        pub constants: ConstBytes,
    }

    // TODO: https://community.arm.com/arm-community-blogs/b/architectures-and-processors-blog/posts/caches-and-self-modifying-code
    impl Jitted {
        pub fn new(bytes: usize) -> Self {
            let mut map = memmap2::MmapOptions::new().len(bytes).map_anon().unwrap();
            assert_eq!(map.as_ptr() as usize % 4, 0, "alignment's fucked");
            Self {
                current_start: map.as_ptr(),
                next: map.as_mut_ptr(),
                map_mut: Some(map),
                map_exec: None,
                dispatch: vec![],
                ranges: vec![],
                ip_to_inst: vec![],
                constants: ConstBytes::default(),
            }
        }

        pub fn bytes(&self) -> &[u8] {
            let map = self.map_mut.as_ref().unwrap();
            let len = unsafe { self.next.offset_from(map.as_ptr()) } as usize;
            &map[0..len]
        }

        pub fn reserve(&mut self, func_count: usize) {
            assert!(func_count < (1 << 12), "TODO: not enough bits for indirect call");
            for _ in self.dispatch.len()..func_count {
                self.dispatch.push(null());
                self.ranges.push(&[] as *const [u8])
            }
        }

        pub fn get_dispatch(&self) -> *const *const u8 {
            debug_assert!(self.map_exec.is_some(), "dont try to use the dispatch table while writing.");
            self.dispatch.as_ptr()
        }

        pub fn get_fn(&self, f: FuncId) -> Option<*const [u8]> {
            let ptr = self.ranges[f.0];
            if ptr.len() == 0 {
                None
            } else {
                Some(ptr)
            }
        }

        pub fn mark_next_ip(&mut self) {
            self.ip_to_inst.push(self.next);
        }

        pub fn offset_words_ip_ip(&mut self, from_ip: usize, to_ip: usize) -> i64 {
            unsafe { self.ip_to_inst[to_ip].offset_from(self.ip_to_inst[from_ip]) as i64 / 4 }
        }

        pub fn offset_words_inst_ip(&self, from_inst: usize, to_ip: usize) -> i64 {
            unsafe { (self.ip_to_inst[to_ip].offset_from(self.current_start) as i64 / 4) - (from_inst as i64) }
        }

        pub fn prev(&self) -> usize {
            unsafe { (self.next as *const u32).offset_from(self.current_start as *const u32) as usize - 1 }
        }

        pub fn patch(&mut self, inst_index: usize, inst_value: i64) {
            unsafe {
                let ptr = (self.current_start as *mut u32).add(inst_index);
                debug_assert_eq!(*ptr, brk(0) as u32, "unexpected patch");
                *ptr = inst_value as u32
            };
        }

        pub fn push(&mut self, inst: i64) {
            let map = self.map_mut.as_ref().expect("No push while exec.");
            unsafe {
                debug_assert!(map.as_ptr().add(map.len()) > self.next, "OOB");
                *(self.next as *mut u32) = inst as u32;
                self.next = self.next.add(4);
            }
        }

        pub fn save_current(&mut self, f: FuncId) {
            debug_assert!(self.map_mut.is_some());
            unsafe {
                // TODO: make sure there's not an off by one thing here.
                let range = slice::from_raw_parts(self.current_start, self.next.offset_from(self.current_start) as usize);
                self.dispatch[f.0] = self.current_start;
                self.ranges[f.0] = range as *const [u8];
                self.ip_to_inst.clear();
                debug_assert_eq!(self.current_start as usize % 4, 0);
                self.current_start = self.next;
            }
        }

        pub fn make_exec(&mut self) {
            if let Some(map) = self.map_mut.take() {
                self.map_exec = Some(map.make_exec().unwrap())
            }
        }

        pub fn make_write(&mut self) {
            if let Some(map) = self.map_exec.take() {
                self.map_mut = Some(map.make_mut().unwrap())
            }
        }

        pub fn get_current(&self) -> *const u8 {
            self.next
        }
    }
}

/// https://developer.arm.com/documentation/100076/0100/A32-T32-Instruction-Set-Reference/Condition-Codes/Condition-code-suffixes-and-related-flags?lang=en
#[derive(Debug, Copy, Clone)]
enum CmpFlags {
    EQ = 0b0000,
}

// There must be a not insane way to do this but i gave up and read the two's complement wikipedia page.
/// Convert an i64 to an i<bit_count> with the (64-<bit_count>) leading bits 0.
fn signed_truncate(mut x: i64, bit_count: i64) -> i64 {
    debug_assert!(x > -(1 << (bit_count - 1)) && (x < (1 << (bit_count - 1))));
    let mask = (1 << bit_count) - 1;
    if x < 0 {
        x *= -1;
        x = !x;
        x += 1;
        x &= mask;
    }
    x
}

// impl<'a, 'p> Executor for  {
// }
