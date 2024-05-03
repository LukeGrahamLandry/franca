//! TODO: i know this is remarkably bad codegen.
//! #c_call means https://en.wikipedia.org/wiki/Calling_convention#ARM_(A64)

#![allow(non_upper_case_globals)]
#![allow(unused)]

use crate::ast::{Flag, FnType, Func, FuncId, TypeId, TypeInfo};
use crate::bc::{Bc, BcReady, Value, Values};
use crate::compiler::{add_unique, Compile, ExecTime, Res};
use crate::{ast::Program, bc::FnBody};
use crate::{bootstrap_gen::*, unwrap, TRACE_ASM};
use crate::{err, logging::PoolLog};
use std::arch::asm;
use std::cell::UnsafeCell;
use std::fmt::Debug;
use std::fs;
use std::mem::transmute;
use std::process::Command;

// I'm using u16 everywhere cause why not, extra debug mode check might catch putting a stupid big number in there. that's 65k bytes, 8k words, the uo instructions can only do 4k words.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
struct SpOffset(u16);

struct BcToAsm<'z, 'p, 'a> {
    compile: &'z mut Compile<'a, 'p>,
    vars: Vec<Option<SpOffset>>,
    stack: Vec<Val>,
    free_reg: Vec<i64>,
    open_slots: Vec<(SpOffset, u16)>,
    next_slot: SpOffset,
    f: FuncId,
    wip: Vec<FuncId>,
    when: ExecTime, // make recursion work
    debug_if_saved_stack: Vec<Option<Vec<Val>>>,
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum Val {
    Increment { reg: i64, offset_bytes: u16 }, // not a pointer derefernce, just adding a static number to the value in the register.
    Literal(i64),
    Spill(SpOffset),
}

impl Debug for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Increment { reg, offset_bytes } => write!(f, "x{reg} + {offset_bytes}"),
            Val::Literal(x) => write!(f, "{x}"),
            Val::Spill(slot) => write!(f, "[sp, {}]", slot.0),
        }
    }
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

impl<'z, 'p, 'a> BcToAsm<'z, 'p, 'a> {
    fn new(compile: &'z mut Compile<'a, 'p>, when: ExecTime) -> Self {
        Self {
            compile,
            vars: Default::default(),
            stack: Default::default(),
            open_slots: vec![],
            next_slot: SpOffset(0),
            f: FuncId::from_index(0), // TODO: bad
            wip: vec![],
            when,
            free_reg: vec![],
            debug_if_saved_stack: vec![],
        }
    }

    // TODO: i cant keep copy pasting this shape. gonna cry.
    fn compile(&mut self, f: FuncId) -> Res<'p, ()> {
        self.compile.aarch64.reserve(f.as_index());
        if self.compile.aarch64.get_fn(f).is_some() || self.wip.contains(&f) {
            return Ok(());
        }
        self.compile.aarch64.make_write();

        self.wip.push(f);
        if let Some(template) = self.compile.program[f].any_reg_template {
            todo!()
        } else if let Some(addr) = self.compile.program[f].comptime_addr {
            self.compile.aarch64.dispatch[f.as_index()] = addr as *const u8;
        } else {
            let callees = self.compile.program[f].wip.as_ref().unwrap().callees.clone();
            for c in callees {
                if c.1 == self.when || c.1 == ExecTime::Both {
                    self.compile(c.0)?;
                }
            }

            let func = &self.compile.program[f];
            if let Some(insts) = func.jitted_code.as_ref() {
                // TODO: i dont like that the other guy leaked the box for the jitted_code ptr. i'd rather everything share the one Jitted instance.
                // TODO: this needs to be aware of the distinction between comptime and runtime target.
                for op in insts {
                    let op = *op as i64;
                    self.compile.aarch64.push(op);
                }
                self.compile.aarch64.save_current(f);
            } else {
                self.bc_to_asm(f)?;
                self.compile.aarch64.save_current(f);
                // if cfg!(feature = "llvm_dis_debug") {
                let asm = self.compile.aarch64.get_fn(f).unwrap();
                if TRACE_ASM {
                    let asm = unsafe { &*asm };
                    let hex: String = asm
                        .iter()
                        .copied()
                        .array_chunks::<4>()
                        .map(|b| format!("{:#02x} {:#02x} {:#02x} {:#02x} ", b[0], b[1], b[2], b[3]))
                        .collect();
                    let path = "target/latest_log/temp.asm".to_string();
                    fs::write(&path, hex).unwrap();
                    let dis = String::from_utf8(Command::new("llvm-mc").arg("--disassemble").arg(&path).output().unwrap().stdout).unwrap();
                    debugln!("{dis}");
                }
                // }
            }
        }

        self.wip.retain(|c| c != &f);
        Ok(())
    }

    fn bc_to_asm(&mut self, f: FuncId) -> Res<'p, ()> {
        let a = self.compile.program[f].finished_arg.unwrap();
        let r = self.compile.program[f].finished_ret.unwrap();
        // TODO: i could change the ret_limit if i knew only called at runtime because internal abi doesn't have to match c but should just add explicit cc stuff less hackily -- May 2
        if self.compile.ready.sizes.slot_count(self.compile.program, a) >= 7 || self.compile.ready.sizes.slot_count(self.compile.program, r) > 1 {
            debug_assert!(!self.compile.program[f].has_tag(Flag::C_Call),);
            debug_assert!(self.compile.program[f].comptime_addr.is_none());
            // my cc can do 8 returns in the arg regs but my ffi with compiler can't
            // TODO: my c_Call can;t handle agragates
            self.compile.program[f].add_tag(Flag::Flat_Call);
            self.compile.program[f].add_tag(Flag::Ct);
        }
        let ff = &self.compile.program[f];
        let is_flat_call = ff.has_tag(Flag::Flat_Call);
        debugln!(
            "=== {f:?} {} flat:{is_flat_call} ===",
            self.compile.pool.get(self.compile.program[f].name)
        );
        debugln!("{}", self.compile.program[f].body.as_ref().unwrap().log(self.compile.pool));
        let is_c_call = ff.has_tag(Flag::C_Call);
        let has_ct = ff.has_tag(Flag::Ct);
        let arg_ty = ff.finished_arg.unwrap();
        let ret_ty = ff.finished_ret.unwrap();
        let arg_size = self.compile.slot_count(arg_ty);
        let ret_size = self.compile.slot_count(ret_ty);
        let func = self.compile.ready[f].as_ref().unwrap();
        self.f = f;
        self.next_slot = SpOffset(0);
        self.vars.clear();
        self.vars.extend(vec![None; func.vars.len()]);
        self.open_slots.clear();
        self.debug_if_saved_stack.clear();
        self.debug_if_saved_stack.extend(vec![None; func.if_debug_count as usize]);

        self.compile.aarch64.push(sub_im(X64, sp, sp, 16, 0));
        self.compile.aarch64.push(stp_so(X64, fp, lr, sp, 0)); // save our return address
        self.compile.aarch64.push(add_im(X64, fp, sp, 0, 0)); // Note: normal mov encoding can't use sp
        self.compile.aarch64.push(brk(0));
        let reserve_stack = self.compile.aarch64.prev();

        let mut release_stack = vec![];

        let mut flat_result = None;

        self.free_reg.clear();
        // The code expects arguments on the virtual stack (the first thing it does might be save them to variables but that's not my problem).
        if is_flat_call {
            // (x0=compiler, x1=arg_ptr, x2=arg_len, x3=ret_ptr, x4=ret_len)
            assert!(!is_c_call);

            // Runtime check that caller agrees on type sizes.
            // TODO: This is not nessisary if we believe in our hearts that there are no compiler bugs...
            assert!(arg_size < (1 << 12));
            assert!(ret_size < (1 << 12));
            self.compile.aarch64.push(cmp_im(X64, x2, arg_size as i64, 0));
            self.compile.aarch64.push(b_cond(2, CmpFlags::EQ as i64)); // TODO: do better
            self.compile.aarch64.push(brk(0xbad0));
            self.compile.aarch64.push(cmp_im(X64, x4, ret_size as i64, 0));
            self.compile.aarch64.push(b_cond(2, CmpFlags::EQ as i64)); // TODO: do better
            self.compile.aarch64.push(brk(0xbad0));

            // Save the result pointer.
            flat_result = Some(self.next_slot);
            self.store_u64(x3, sp, self.next_slot.0);
            self.next_slot.0 += 8;

            // Copy arguments into their stack slots.
            // This is the same as the load instruction. We have a pointer and we want to splat it out onto the virtual stack.
            // TODO: better would be allowing storing deref offsets on the stack so loads could be suspended. -- May 1
            self.reset_free_reg();
            self.free_reg.retain(|r| *r != x1);
            self.stack.push(Val::Increment { reg: x1, offset_bytes: 0 });
            // TODO: this is really dumb. it should just refer to them in mmoery if the arg is big because rn it will spill onto its own stack anyway.
            self.emit_load(arg_size);
        } else {
            assert!(arg_size <= 7, "c_call only supports 7 arguments. TODO: pass on stack");
            assert!(!has_ct, "compiler context is implicitly passed as first argument for #ct builtins.");
            self.compile.program[func.func].add_tag(Flag::C_Call); // Make sure we don't try to emit as #flat_call later

            self.ccall_reg_to_stack(arg_size);

            // Any registers not containing args can be used.
            for i in arg_size..8 {
                self.free_reg.push(i as i64);
            }
        }

        let mut patch_cbz = vec![]; // (patch_idx, jump_idx, register)
        let mut patch_b = vec![];

        debugln!("entry: ({:?})", self.stack);
        debug_assert_eq!(self.stack.len() as u16, arg_size);
        let func = self.compile.ready[f].as_ref().unwrap();
        for i in 0..func.insts.len() {
            let inst = &(self.compile.ready[f].as_ref().unwrap().insts[i].clone());
            self.compile.aarch64.mark_next_ip();
            match inst {
                &Bc::LastUse { id } => {
                    let slot = self.vars[id as usize].take();
                    let slot = slot.unwrap();
                    let ty = self.compile.ready[f].as_ref().unwrap().vars[id as usize];
                    let count = self.compile.slot_count(ty);
                    self.open_slots.push((slot, count * 8)); // TODO: keep this sorted by count?
                }
                Bc::NoCompile => unreachable!("{}", self.compile.program[self.f].log(self.compile.pool)),
                &Bc::CallSplit { rt, ct } => {
                    let f = if self.when == ExecTime::Comptime { ct } else { rt };
                    self.call_direct(f)?;
                }
                &Bc::CallDirect { f } => {
                    self.call_direct(f)?;
                }
                &Bc::PushConstant { value } => self.stack.push(Val::Literal(value)),
                &Bc::GetNativeFnPtr(f) => {
                    // TODO: use adr+adrp instead of an integer.
                    // TODO: do linker-ish things to allow forward references.
                    //       actually the way i do that rn is with the dispatch table so could just use that for now.

                    if let Some(ptr) = self.compile.aarch64.get_fn(f) {
                        self.stack.push(Val::Literal(ptr.as_ptr() as i64));
                    } else if let Some(addr) = self.compile.program[f].comptime_addr {
                        self.stack.push(Val::Literal(addr as i64));
                    } else {
                        assert!(f.as_index() < 4096);
                        let reg = self.get_free_reg();
                        self.compile.aarch64.push(ldr_uo(X64, reg, x21, f.as_index() as i64));
                        self.stack.push(Val::Increment { reg, offset_bytes: 0 })
                    }
                }
                &Bc::JumpIf { true_ip, false_ip } => {
                    let cond = self.pop_to_reg();
                    self.compile.aarch64.push(brk(0));
                    assert_eq!(true_ip as usize, i + 1);
                    patch_cbz.push((self.compile.aarch64.prev(), false_ip, cond));
                    self.drop_reg(cond);
                }
                &Bc::Goto { ip } => {
                    self.compile.aarch64.push(brk(0));
                    patch_b.push((self.compile.aarch64.prev(), ip));
                }
                &Bc::Ret => {
                    if is_flat_call {
                        assert!(!is_c_call);
                        let working = self.get_free_reg();
                        // Retrive result address
                        self.load_u64(working, sp, flat_result.unwrap().0);
                        // We have the values on the virtual stack, they want them at some address, that's the same as my store instruction.
                        self.stack.push(Val::Increment {
                            reg: working,
                            offset_bytes: 0,
                        });
                        self.emit_store(ret_size);
                    } else {
                        // We have the values on virtual stack and want them in r0-r7, that's the same as making a call.
                        self.stack_to_ccall_reg(ret_size)
                    }
                    debug_assert!(self.stack.is_empty());

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
                &Bc::AddrVar { id } => {
                    if let Some(slot) = self.vars[id as usize] {
                        self.stack.push(Val::Increment {
                            reg: sp,
                            offset_bytes: slot.0,
                        });
                    } else {
                        let ty = self.compile.ready[f].as_ref().unwrap().vars[id as usize];
                        let count = self.compile.slot_count(ty);
                        let slot = self.create_slots(count);
                        self.vars[id as usize] = Some(slot);
                        self.stack.push(Val::Increment {
                            reg: sp,
                            offset_bytes: slot.0,
                        });
                    }
                }
                // Note: we do this statically, it wont get actually applied to a register until its needed because loads/stores have an offset immediate.
                &Bc::IncPtr { offset } => match self.stack.last_mut().unwrap() {
                    Val::Increment { reg, offset_bytes: prev } => {
                        *prev += offset * 8;
                    }
                    Val::Literal(v) => {
                        *v += offset as i64 * 8;
                    }
                    Val::Spill(_) => {
                        // TODO: should it hold the add statically? rn it does the load but doesn't need to restore because it just unspills it.
                        let (reg, offset_bytes) = self.pop_to_reg_with_offset();
                        self.stack.push(Val::Increment {
                            reg,
                            offset_bytes: offset_bytes + offset,
                        })
                    }
                },
                &Bc::Load { slots } => self.emit_load(slots),
                &Bc::Store { slots } => self.emit_store(slots),
                &Bc::TagCheck { expected } => {
                    // TODO: this leaks a register if it was literal but it probably never will be -- May 1
                    let (reg, offset_bytes) = self.peek_to_reg_with_offset(); // enum_ptr. can't stomp!
                    let working = self.get_free_reg();
                    debug_assert_eq!(offset_bytes % 8, 0);
                    debug_assert!(offset_bytes / 8 < (1 << 12));
                    self.load_u64(working, reg, offset_bytes); // working = *ptr

                    self.compile.aarch64.push(cmp_im(X64, working, expected as i64, 0));
                    self.compile.aarch64.push(b_cond(2, CmpFlags::EQ as i64)); // TODO: do better
                    self.compile.aarch64.push(brk(0xbeef));
                    self.drop_reg(working);
                    // don't drop <reg>, we just peeked it
                }
                Bc::CallFnPtr { ty, comp_ctx } => {
                    assert!(!*comp_ctx, "flat call needs special handling");
                    self.dyn_c_call(*ty, *comp_ctx, |s| {
                        // dyn_c_call will have popped the args, so now stack is just the pointer to call
                        let reg = s.pop_to_reg(); // TODO: i'd rather be able to specify that this be x16 so you know its not part of the cc. -- May 1
                        s.compile.aarch64.push(br(reg, 1));
                        s.drop_reg(reg);
                    });
                }
                Bc::Unreachable => {
                    self.compile.aarch64.push(brk(123));
                }
                Bc::Pop { slots } => {
                    debug_assert!(self.stack.len() >= *slots as usize);
                    for _ in 0..*slots {
                        let r = self.pop_to_reg(); // TODO: dont bother actually making the value, just return the reg -- May 1
                        self.drop_reg(r);
                    }
                }
                // TODO: dont use vars
                #[cfg(debug_assertions)]
                &Bc::EndIf { index, slots } => match &self.debug_if_saved_stack[index as usize] {
                    Some(prev) => {
                        assert_eq!(&self.stack, prev)
                    }
                    None => {
                        self.debug_if_saved_stack[index as usize] = Some(self.stack.clone());
                    }
                },
                #[cfg(not(debug_assertions))]
                &Bc::EndIf { .. } => {}
            }

            debugln!("{i} {inst:?} => {:?} | free: {:?}", self.stack, self.free_reg);
        }
        for (inst, false_ip, reg) in patch_cbz {
            let offset = self.compile.aarch64.offset_words_inst_ip(inst, false_ip as usize);
            debug_assert!(reg < 32);
            self.compile.aarch64.patch(inst, cbz(X64, signed_truncate(offset, 19), reg));
        }
        for (from_inst, to_ip) in patch_b {
            let dist = self.compile.aarch64.offset_words_inst_ip(from_inst, to_ip as usize);
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

    // TODO: floats -- May 1
    fn stack_to_ccall_reg(&mut self, slots: u16) {
        for i in 0..(slots as usize) {
            let stack_index = self.stack.len() - (slots as usize) + i;
            if let Val::Increment { reg, offset_bytes } = self.stack[stack_index] {
                if reg == i as i64 {
                    if offset_bytes != 0 {
                        self.compile.aarch64.push(add_im(X64, i as i64, reg, offset_bytes as i64, 0));
                    }

                    // if we happen to already be in the right register, cool, don't worry about it.
                    debug!("|x{} already| ", i);
                    continue;
                }
            }

            if let Some(want) = self.free_reg.iter().position(|r| *r as usize == i) {
                self.free_reg.remove(want);
            } else if let Some(used) = self.stack.iter().position(|r| r.reg().map(|(r, _)| r as usize == i).unwrap_or(false)) {
                // The one we want is already used by something on the v-stack; swap it with a random free one. TODO: put in the right place if you can.
                let (old_reg, offset_bytes) = self.stack[used].reg().unwrap();
                debug_assert_ne!(old_reg, sp); // encoding but unreachable
                let reg = self.get_free_reg();
                self.compile.aarch64.push(mov(X64, reg, old_reg));
                self.stack[used] = Val::Increment { reg, offset_bytes };
                // Now x{i} is free and we'll use it below.
            } else {
                panic!("TODO: x{i} is not free. stack is {:?}. free is {:?}", self.stack, self.free_reg);
            }

            debug!("|(x{}) <- ({:?})| ", i, self.stack[stack_index]);
            match self.stack[stack_index] {
                Val::Increment { reg, offset_bytes } => {
                    debug_assert_ne!(reg as usize, i);
                    // even if offset_bytes is 0, we need to move it to the right register. and add can encode that mov even if reg is sp.
                    self.compile.aarch64.push(add_im(X64, i as i64, reg, offset_bytes as i64, 0));
                }
                Val::Literal(x) => self.load_imm(i as i64, x as u64),
                Val::Spill(slot) => self.load_u64(i as i64, sp, slot.0),
            }
        }
        debugln!();
        self.stack.truncate(self.stack.len() - slots as usize);
    }

    // TODO: floats -- May 1
    fn ccall_reg_to_stack(&mut self, slots: u16) {
        for i in 0..slots {
            self.stack.push(Val::Increment {
                reg: i as i64,
                offset_bytes: 0,
            });
        }
    }

    /// <ptr:1> -> <?:n>
    fn emit_load(&mut self, slots: u16) {
        debug_assert!(!self.stack.is_empty());
        debug!("LOAD: [{:?}] to ", self.stack.last().unwrap());
        // TODO: really you want to suspend this too because the common case is probably that the next thing is a store but thats harder to think about so just gonna start with the dumb thing i was doing before -- May 1
        let (reg, mut offset_bytes) = self.pop_to_reg_with_offset(); // get the ptr

        debug_assert_eq!(offset_bytes % 8, 0);
        for _ in 0..slots {
            let working = self.get_free_reg();
            self.load_u64(working, reg, offset_bytes);
            // self.store_u64(working, sp, saved.0 as u16);
            let v = Val::Increment {
                reg: working,
                offset_bytes: 0,
            };
            self.stack.push(v);
            debug!("({:?}), ", self.stack.last().unwrap());
            offset_bytes += 8;
            // saved.0 += 8;
        }
        debugln!();
        debug_assert!(offset_bytes / 8 < (1 << 12));
        self.drop_reg(reg);
    }

    /// <?:n> <ptr:1> -> _
    fn emit_store(&mut self, slots: u16) {
        debug_assert!(self.stack.len() > slots as usize);
        debugln!(
            "STORE: ({:?}) to [{:?}]",
            &self.stack[self.stack.len() - slots as usize - 1..self.stack.len() - 1],
            self.stack.last().unwrap()
        );
        let (reg, mut offset_bytes) = self.pop_to_reg_with_offset();
        // Note: we're going backwards: popping off the stack and storing right to left.
        for i in (0..slots).rev() {
            let o = offset_bytes + (i * 8);
            debug!("| [x{reg} + {o}] <- ({:?}) |", self.stack.last().unwrap());
            let val = self.pop_to_reg();
            self.store_u64(val, reg, o);
            self.drop_reg(val);
        }
        self.drop_reg(reg);
        debugln!();
    }

    fn load_u64(&mut self, dest_reg: i64, src_addr_reg: i64, offset_bytes: u16) {
        debug_assert_ne!(dest_reg, sp);
        self.compile.aarch64.push(ldr_uo(X64, dest_reg, src_addr_reg, (offset_bytes / 8) as i64));
        debug_assert!(offset_bytes / 8 < (1 << 12));
    }

    fn store_u64(&mut self, src_reg: i64, dest_addr_reg: i64, offset_bytes: u16) {
        if src_reg == sp {
            // TODO: this is weird. can only happen for exactly the sp, not an offset from it. so i guess somewhere else is saving an add, maybe its fine. -- May 2
            let reg = self.get_free_reg();
            self.compile.aarch64.push(add_im(X64, reg, sp, 0, 0)); // not mov!
            self.store_u64(reg, dest_addr_reg, offset_bytes);
            self.drop_reg(reg);
            return;
        }
        self.compile.aarch64.push(str_uo(X64, src_reg, dest_addr_reg, (offset_bytes / 8) as i64));
        debug_assert!(offset_bytes / 8 < (1 << 12));
    }

    fn reset_free_reg(&mut self) {
        self.free_reg.clear();
        self.free_reg.extend([0, 1, 2, 3, 4, 5, 6, 7, 8]);
    }

    fn spill_abi_stompable(&mut self) {
        for i in 0..self.stack.len() {
            self.try_spill(i);
        }
        debugln!();
    }

    fn try_spill(&mut self, i: usize) -> bool {
        let v = self.stack[i];
        if let Val::Increment { reg, offset_bytes } = v {
            if reg == sp {
                return false;
            }

            // Note: this assumes we don't need to preserve the value in the reg other than for this one v-stack slot.
            let slot = self.create_slots(1);
            debug!("(spill ({reg:?} + {offset_bytes}) -> [sp, {}]) ", slot.0);
            if offset_bytes != 0 {
                self.compile.aarch64.push(add_im(X64, reg, reg, offset_bytes as i64, 0));
            }

            self.store_u64(reg, sp, slot.0);
            self.drop_reg(reg);
            self.stack[i] = Val::Spill(slot);
            return true;
        }

        false
    }

    fn get_free_reg(&mut self) -> i64 {
        if let Some(r) = self.free_reg.pop() {
            debug_assert_ne!(r, sp);
            r
        } else {
            let mut i = 0;
            while i < self.stack.len() && !self.try_spill(i) {
                i += 1;
            }
            if i == self.stack.len() {
                panic!("spill to stack failed");
            }
            self.free_reg.pop().unwrap()
        }
    }

    fn drop_reg(&mut self, reg: i64) {
        if reg != sp {
            debug_assert!(!self.free_reg.contains(&reg), "drop r{reg} -> {:?}", self.free_reg);
            self.free_reg.push(reg);
        }
    }

    fn pop_to_reg(&mut self) -> i64 {
        match self.stack.pop().unwrap() {
            Val::Increment { mut reg, offset_bytes } => {
                if offset_bytes > 0 {
                    debug_assert!(offset_bytes < (1 << 12), "TODO: not enough bits");
                    let out = if reg == sp { self.get_free_reg() } else { reg };
                    self.compile.aarch64.push(add_im(X64, out, reg, offset_bytes as i64, 0));
                    reg = out;
                }
                reg
            }
            Val::Literal(x) => {
                let r = self.get_free_reg();
                self.load_imm(r, u64::from_le_bytes(x.to_le_bytes()));
                r
            }
            Val::Spill(slot) => {
                let r = self.get_free_reg();
                self.load_u64(r, sp, slot.0);
                self.open_slots.push((slot, 8));
                r
            }
        }
    }

    fn peek_to_reg_with_offset(&mut self) -> (i64, u16) {
        match self.stack.last().cloned().unwrap() {
            Val::Increment { reg, offset_bytes } => (reg, offset_bytes),
            Val::Literal(x) => {
                let r = self.get_free_reg();
                self.load_imm(r, u64::from_le_bytes(x.to_le_bytes()));
                (r, 0)
            }
            Val::Spill(slot) => {
                let r = self.get_free_reg();
                self.load_u64(r, sp, slot.0);
                (r, 0)
            }
        }
    }

    fn pop_to_reg_with_offset(&mut self) -> (i64, u16) {
        let res = self.peek_to_reg_with_offset();
        self.stack.pop().unwrap();
        res
    }

    fn create_slots(&mut self, count: u16) -> SpOffset {
        for (i, &(_, size)) in self.open_slots.iter().enumerate().rev() {
            if size == count * 8 {
                return self.open_slots.remove(i).0;
            }
        }

        let made = self.next_slot;
        self.next_slot.0 += count * 8;
        made
    }

    // TODO: this should check if adr is close enough since it statically knows the ip cause we're jitting.
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

    /// <arg:n> -> <ret:m>
    fn dyn_c_call(&mut self, f_ty: FnType, comp_ctx: bool, do_call: impl FnOnce(&mut Self)) {
        let reg_offset = if comp_ctx { 1 } else { 0 }; // for secret args like comp_ctx
        let arg_count = self.compile.slot_count(f_ty.arg);
        let ret_count = self.compile.slot_count(f_ty.ret);
        assert!(
            (arg_count + reg_offset) <= 7,
            "indirect c_call only supports 7 arguments. TODO: pass on stack"
        );
        assert!(ret_count <= 7, "indirect c_call only supports 7 returns. TODO: pass on stack");

        if comp_ctx {
            let first_arg_index = self.stack.len() - arg_count as usize;
            // TODO: this has gotta be UB
            let c = self.compile as *const Compile as u64;
            let p = &self.compile.program as *const &mut Program as u64;
            debug_assert_eq!(c, p, "need repr c");
            // put it on the stack as though it were a normal argument so the cc assigner doesn't need a special case
            self.stack.insert(first_arg_index, Val::Literal(c as i64));
        }

        self.stack_to_ccall_reg(arg_count + reg_offset);
        self.spill_abi_stompable();
        do_call(self);
        self.ccall_reg_to_stack(ret_count);
        for i in ret_count..7 {
            add_unique(&mut self.free_reg, i as i64); // now the extras are usable again.
        }
    }

    fn call_direct(&mut self, f: FuncId) -> Res<'p, ()> {
        let target = &self.compile.program[f];
        debug_assert!(target.any_reg_template.is_none());
        let target_c_call = target.has_tag(Flag::C_Call);
        let target_flat_call = target.has_tag(Flag::Flat_Call);
        let comp_ctx = target.has_tag(Flag::Ct);
        let f_ty = target.unwrap_ty();

        if target_flat_call {
            debugln!("flat_call");
            // (compiler, arg_ptr, arg_len_i64s, ret_ptr, ret_len_i64s)
            assert!(comp_ctx, "Flat call is only supported for calling into the compiler");
            assert!(!target_c_call, "multiple calling conventions doesn't make sense");

            let addr = target.comptime_addr.or_else(|| self.compile.aarch64.get_fn(f).map(|v| v.as_ptr() as u64));

            let arg_count = self.compile.slot_count(f_ty.arg);
            let ret_count = self.compile.slot_count(f_ty.ret);
            let arg_offset = self.create_slots(arg_count);
            // TODO: super simple data flow lookahead so if you're about to store the whole thing to a var, use that as the address.
            let ret_offset = self.create_slots(ret_count);
            debug_assert!(self.stack.len() as u16 >= arg_count, "{}", arg_count);
            self.stack.push(Val::Increment {
                reg: sp,
                offset_bytes: arg_offset.0,
            });
            self.emit_store(arg_count);

            // Do this way up here before putting things in x0-4
            self.spill_abi_stompable();

            let c = self.compile as *const Compile as u64;
            self.load_imm(x0, c);
            debug_assert!(arg_offset.0 < 4096 && ret_offset.0 < 4096);
            self.compile.aarch64.push(add_im(X64, x1, sp, arg_offset.0 as i64, 0));
            self.load_imm(x2, arg_count as u64);
            self.compile.aarch64.push(add_im(X64, x3, sp, ret_offset.0 as i64, 0));
            self.load_imm(x4, ret_count as u64);
            self.branch_with_link(f);

            for i in 0..7 {
                add_unique(&mut self.free_reg, i as i64); // now the extras are usable again.
            }

            self.stack.push(Val::Increment {
                reg: sp,
                offset_bytes: ret_offset.0,
            });
            self.emit_load(ret_count);

            // TODO: leaking slots -- May 1
            // self.release_many(arg); // Note: release after to make sure they don't alias ret which might not be what the callee is expecting (even tho it would be fine for current uses).
        } else {
            debugln!("c_call");
            self.compile.program[f].add_tag(Flag::C_Call); // Make sure we don't try to emit as #flat_call later
            self.dyn_c_call(f_ty, comp_ctx, |s| s.branch_with_link(f));
        }
        Ok(())
    }

    const DO_BASIC_ASM_INLINE: bool = true;

    fn branch_with_link(&mut self, f: FuncId) {
        if Self::DO_BASIC_ASM_INLINE {
            // TODO: save result on the function so dont have to recheck every time?
            if let Some(code) = &self.compile.program[f].jitted_code {
                if self.compile.program[f].has_tag(Flag::One_Ret_Pic) {
                    // TODO: HACK: for no-op casts, i have two rets because I can't have single element tuples.
                    if code.len() == 2 && code[0] as i64 == ret(()) && code[1] as i64 == ret(()) {
                        return;
                    }
                    for op in &code[0..code.len() - 1] {
                        debug_assert_ne!(*op as i64, ret(()));
                        self.compile.aarch64.push(*op as i64);
                    }
                    debug_assert_eq!(*code.last().unwrap() as i64, ret(()));
                    return;
                }
            }
        }

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
            assert!(f.as_index() < 4096);
            self.compile.aarch64.push(ldr_uo(X64, x16, x21, f.as_index() as i64));
        }

        self.compile.aarch64.push(br(x16, 1))
    }
}

impl Val {
    fn reg(&self) -> Option<(i64, u16)> {
        match self {
            &Val::Increment { reg, offset_bytes } => Some((reg, offset_bytes)),
            Val::Literal(_) => None,
            Val::Spill(_) => None,
        }
    }
}

use crate::ffi::InterpSend;
#[cfg(target_arch = "aarch64")]
pub use jit::Jitted;

pub fn store_to_ints<'a>(values: impl Iterator<Item = &'a Value>) -> Vec<i64> {
    let mut out = vec![];
    for value in values {
        out.push(value.as_raw_int());
    }
    out
}

pub fn store<'a>(values: impl Iterator<Item = &'a Value>) -> *const u8 {
    let mut out = store_to_ints(values);
    out.into_raw_parts().0 as *const u8
}

impl Value {
    pub fn as_raw_int(&self) -> i64 {
        match self {
            Value::F64(f) => i64::from_le_bytes(f.to_le_bytes()),
            &Value::I64(i) => i,
            &Value::Bool(i) => i as i64,
            &Value::OverloadSet(i) => i.as_raw(),
            &Value::GetFn(i) => i.as_raw(),
            &Value::Symbol(i) => i as i64,
            &Value::Type(ty) => ty.as_raw(),
            Value::Unit => 0, // TODO
            &Value::Heap(ptr) => ptr as usize as i64,
            Value::SplitFunc { ct, rt } => todo!(),
            &Value::GetNativeFnPtr(i) => {
                // TODO: not sure if we want to preserve the id or use the actual address
                i.as_raw()
                // todo!()
            }
        }
    }
}

//#[cfg(target_arch = "aarch64")]
pub mod jit {
    use crate::ast::FuncId;
    use crate::bootstrap_gen::brk;
    use crate::{JITTED_PAGE, STATS};
    use std::cell::UnsafeCell;
    use std::ptr::null;
    use std::slice;

    pub struct Jitted {
        map_mut: Option<memmap2::MmapMut>,
        map_exec: Option<memmap2::Mmap>,
        /// This is redundant but is the pointer used for actually calling functions and there aren't that many bits in the instruction,
        /// so I don't want to spend one doubling to skip lengths.
        pub dispatch: Vec<*const u8>,
        ranges: Vec<*const [u8]>,
        current_start: *const u8,
        next: *mut u8,
        old: *mut u8,
        ip_to_inst: Vec<*const u8>,
        pub low: usize,
        pub high: usize,
    }

    // TODO: https://community.arm.com/arm-community-blogs/b/architectures-and-processors-blog/posts/caches-and-self-modifying-code
    impl Jitted {
        pub fn new(bytes: usize) -> Self {
            let mut map = memmap2::MmapOptions::new().len(bytes).map_anon().unwrap();
            assert_eq!(map.as_ptr() as usize % 4, 0, "alignment's fucked");
            unsafe { JITTED_PAGE = (map.as_ptr() as usize, bytes) }

            Self {
                current_start: map.as_ptr(),
                low: map.as_mut_ptr() as usize,
                high: map.as_mut_ptr() as usize + bytes,
                next: map.as_mut_ptr(),
                old: map.as_mut_ptr(),
                map_mut: Some(map),
                map_exec: None,
                dispatch: vec![],
                ranges: vec![],
                ip_to_inst: vec![],
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
            let ptr = self.ranges[f.as_index()];
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
                self.dispatch[f.as_index()] = self.current_start;
                self.ranges[f.as_index()] = range as *const [u8];
                self.ip_to_inst.clear();
                debug_assert_eq!(self.current_start as usize % 4, 0);
                // just a marker to make sure someone doesn't fall off the end of the function by forgetting to return.
                // also you can see it in the debugger disassembly so you can tell if you emitted a trash instruction inside a function or you're off the end in uninit memory. -- Apr 25
                self.push(brk(0xDEAD));
                self.current_start = self.next;
            }
        }

        pub fn make_exec(&mut self) {
            if let Some(map) = self.map_mut.take() {
                self.map_exec = Some(map.make_exec().unwrap());
                unsafe { STATS.jit_mprotect += 1 };
            }
        }

        pub fn make_write(&mut self) {
            if let Some(map) = self.map_exec.take() {
                self.map_mut = Some(map.make_mut().unwrap());
                unsafe { STATS.jit_mprotect += 1 };
            }
        }

        pub fn get_current(&self) -> *const u8 {
            self.next
        }

        // Returns the range of memory we've written since the last call.
        pub fn bump_dirty(&mut self) -> (*mut u8, *mut u8) {
            let dirty = (self.old, self.next);
            self.old = self.next;
            dirty
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
