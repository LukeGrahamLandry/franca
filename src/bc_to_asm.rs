//! TODO: i know this is remarkably bad codegen.
//! #c_call means https://en.wikipedia.org/wiki/Calling_convention#ARM_(A64)

#![allow(non_upper_case_globals)]
#![allow(unused)]

use crate::ast::{CallConv, Flag, FnType, Func, FuncId, TypeId, TypeInfo};
use crate::bc::{BbId, Bc, FloatMask, Value, Values};
use crate::compiler::{add_unique, Compile, ExecTime, Res};
use crate::reflect::BitSet;
use crate::{ast::Program, bc::FnBody};
use crate::{bootstrap_gen::*, unwrap};
use crate::{err, logging::PoolLog};
use std::arch::asm;
use std::cell::UnsafeCell;
use std::fmt::Debug;
use std::fs;
use std::mem::transmute;
use std::process::Command;

const ZERO_DROPPED_REG: bool = false;
const ZERO_DROPPED_SLOTS: bool = false;
pub const TRACE_ASM: bool = false;

// I'm using u16 everywhere cause why not, extra debug mode check might catch putting a stupid big number in there. that's 65k bytes, 8k words, the uo instructions can only do 4k words.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
struct SpOffset(u16);

#[derive(PartialEq, Eq, Clone, Copy)]
enum Val {
    Increment { reg: i64, offset_bytes: u16 }, // not a pointer derefernce, just adding a static number to the value in the register.
    Literal(i64),
    Spill(SpOffset),
    FloatReg(i64),
}

impl Debug for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Increment { reg, offset_bytes } => write!(f, "x{reg} + {offset_bytes}"),
            Val::Literal(x) => write!(f, "{x}"),
            Val::Spill(slot) => write!(f, "[sp, {}]", slot.0),
            Val::FloatReg(x) => write!(f, "v{x}"),
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

pub fn emit_aarch64<'p>(compile: &mut Compile<'_, 'p>, f: FuncId, when: ExecTime, body: &FnBody<'p>) -> Res<'p, ()> {
    debug_assert!(!compile.program[f].asm_done, "ICE: tried to double compile?");
    compile.aarch64.reserve(compile.program.funcs.len());
    let mut a = BcToAsm::new(compile, when, body);
    a.compile(f)?;
    assert!(a.wip.is_empty());
    compile.program[f].asm_done = true;
    Ok(())
}

struct BcToAsm<'z, 'p, 'a> {
    compile: &'z mut Compile<'a, 'p>,
    vars: Vec<Option<SpOffset>>,
    next_slot: SpOffset,
    f: FuncId,
    wip: Vec<FuncId>, // make recursion work
    when: ExecTime,
    flat_result: Option<SpOffset>,
    patch_cbz: Vec<(*const u8, BbId, i64)>,
    patch_b: Vec<(*const u8, BbId)>,
    release_stack: Vec<(*const u8, *const u8, *const u8)>,
    state: BlockState,
    block_ips: Vec<Option<*const u8>>,
    clock: u16,
    markers: Vec<(String, usize)>,
    log_asm_bc: bool,
    body: &'z FnBody<'p>,
}

#[derive(Default, Clone)]
struct BlockState {
    stack: Vec<Val>,
    free_reg: Vec<i64>,
    open_slots: Vec<(SpOffset, u16, u16)>,
}

impl<'z, 'p, 'a> BcToAsm<'z, 'p, 'a> {
    fn new(compile: &'z mut Compile<'a, 'p>, when: ExecTime, body: &'z FnBody<'p>) -> Self {
        Self {
            compile,
            vars: Default::default(),
            next_slot: SpOffset(0),
            f: FuncId::from_index(0), // TODO: bad
            wip: vec![],
            when,
            flat_result: None,
            patch_cbz: vec![], // (patch_idx, jump_idx, register)
            patch_b: vec![],
            release_stack: vec![],
            state: Default::default(),
            block_ips: vec![],
            clock: 0,
            markers: vec![],
            log_asm_bc: false,
            body,
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
            let func = &self.compile.program[f];
            if let Some(insts) = func.jitted_code.as_ref() {
                // TODO: i dont like that the other guy leaked the box for the jitted_code ptr. i'd rather everything share the one Jitted instance.
                // TODO: this needs to be aware of the distinction between comptime and runtime target.
                for op in insts {
                    let op = *op as i64;
                    self.compile.aarch64.push(op);
                }
                self.compile.aarch64.save_current(f);
                if func.cc.unwrap() == CallConv::OneRetPic {
                    self.compile.program[f].aarch64_stack_bytes = Some(0);
                }
            } else {
                if TRACE_ASM {
                    println!();
                    println!("=== Bytecode for {f:?}: {} ===", self.compile.program.pool.get(func.name));
                    for (b, insts) in self.body.blocks.iter().enumerate() {
                        println!("[b{b}({})]: ({} incoming)", insts.arg_slots, insts.incoming_jumps);
                        for (i, op) in insts.insts.iter().enumerate() {
                            println!("    {i}. {op:?}");
                        }
                    }
                    println!("===")
                }

                self.log_asm_bc = func.has_tag(Flag::Log_Asm_Bc);
                self.bc_to_asm(f)?;
                self.compile.aarch64.save_current(f);
                // if cfg!(feature = "llvm_dis_debug") {
                let asm = self.compile.aarch64.get_fn(f).unwrap();

                let func = &self.compile.program[f];
                if TRACE_ASM || self.log_asm_bc || func.has_tag(Flag::Log_Asm) {
                    let asm = unsafe { &*self.compile.aarch64.ranges[f.as_index()] };
                    let hex: String = asm
                        .iter()
                        .copied()
                        .array_chunks::<4>()
                        .map(|b| format!("{:#02x} {:#02x} {:#02x} {:#02x} ", b[0], b[1], b[2], b[3]))
                        .collect();
                    let path = "target/latest_log/temp.asm".to_string();
                    fs::write(&path, hex).unwrap();
                    let dis = String::from_utf8(Command::new("llvm-mc").arg("--disassemble").arg(&path).output().unwrap().stdout).unwrap();
                    println!();
                    println!("=== Asm for {f:?}: {} ===", self.compile.program.pool.get(func.name));

                    let mut it = dis.split('\n');
                    it.nth(1);
                    for (i, line) in it.enumerate() {
                        for (s, offset) in &self.markers {
                            if *offset == i {
                                println!("{s}");
                            }
                        }
                        println!("{line}");
                    }
                    println!("===")
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

        let ff = &self.compile.program[f];
        let is_flat_call = ff.cc == Some(CallConv::Flat);
        debugln!(
            "=== {f:?} {} flat:{is_flat_call} ===",
            self.compile.pool.get(self.compile.program[f].name)
        );
        debugln!("{}", self.compile.program[f].body.as_ref().unwrap().log(self.compile.pool));

        let arg_ty = ff.finished_arg.unwrap();
        let ret_ty = ff.finished_ret.unwrap();
        let arg_size = self.compile.slot_count(arg_ty);
        let ret_size = self.compile.slot_count(ret_ty);
        let func = self.body;
        self.f = f;
        self.next_slot = SpOffset(0);
        self.vars.clear();
        self.vars.extend(vec![None; func.vars.len()]);
        self.state.open_slots.clear();
        self.patch_cbz.clear();
        self.patch_b.clear();
        self.release_stack.clear();
        self.block_ips.clear();
        self.markers.clear();
        self.clock = 0;
        let block_count = self.body.blocks.len();
        self.block_ips.extend(vec![None; block_count]);
        self.state = Default::default();

        self.compile.aarch64.mark_start(f);
        self.compile.aarch64.push(sub_im(X64, sp, sp, 16, 0));
        self.compile.aarch64.push(stp_so(X64, fp, lr, sp, 0)); // save our return address
        self.compile.aarch64.push(add_im(X64, fp, sp, 0, 0)); // Note: normal mov encoding can't use sp
        self.compile.aarch64.push(brk(0));
        let reserve_stack = self.compile.aarch64.prev();

        self.flat_result = None;

        // The code expects arguments on the virtual stack (the first thing it does might be save them to variables but that's not my problem).
        let cc = unwrap!(self.compile.program[func.func].cc, "ICE: missing calling convention");
        match cc {
            CallConv::Flat => {
                // (x0=compiler, x1=arg_ptr, x2=arg_len, x3=ret_ptr, x4=ret_len)
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
                self.flat_result = Some(self.next_slot);
                self.store_u64(x3, sp, self.next_slot.0);
                self.next_slot.0 += 8;

                // Copy arguments into their stack slots.
                // This is the same as the load instruction. We have a pointer and we want to splat it out onto the virtual stack.
                // TODO: better would be allowing storing deref offsets on the stack so loads could be suspended. -- May 1
                self.reset_free_reg();
                self.state.free_reg.retain(|r| *r != x1);
                self.state.stack.push(Val::Increment { reg: x1, offset_bytes: 0 });
                // TODO: this is really dumb. it should just refer to them in mmoery if the arg is big because rn it will spill onto its own stack anyway.
                self.emit_load(arg_size);
            }
            CallConv::Arg8Ret1 => {
                debug_assert!(arg_size <= 7, "c_call only supports 7 arguments. TODO: pass on stack");
                let floats = self.compile.program.float_mask_one(arg_ty);
                self.ccall_reg_to_stack(arg_size, floats);

                let float_count = floats.count_ones();
                let int_count = arg_size as u32 - float_count;

                // Any registers not containing args can be used.
                for i in int_count..8 {
                    self.state.free_reg.push(i as i64);
                }
            }
            CallConv::Inline | CallConv::OneRetPic | CallConv::Arg8Ret1Ct => unreachable!("unsupported cc {cc:?}"),
        }

        debugln!("entry: ({:?})", self.state.stack);
        debug_assert_eq!(self.state.stack.len() as u16, arg_size);
        self.emit_block(0, false)?;

        for (inst, false_ip, reg) in self.patch_cbz.drain(..) {
            let false_ip = self.block_ips[false_ip.0 as usize].unwrap();
            let offset = self.compile.aarch64.offset_words(inst, false_ip);
            debug_assert!(reg < 32);
            debug_assert_ne!(offset, 0, "!if ice: while(1);");
            self.compile.aarch64.patch(inst, cbz(X64, signed_truncate(offset, 19), reg));
        }
        for (from_inst, to_ip) in self.patch_b.drain(0..) {
            let to_ip = self.block_ips[to_ip.0 as usize].unwrap();
            let dist = self.compile.aarch64.offset_words(from_inst, to_ip);
            debug_assert_ne!(dist, 0, "while(1);");
            self.compile.aarch64.patch(from_inst, b(signed_truncate(dist, 26), 0));
        }

        let mut slots = self.next_slot.0; //self.compile.ready[self.f].as_ref().unwrap().stack_slots * 8;
        assert!(slots < 4096, "not enough bits to refer to all slots");
        if slots % 16 != 0 {
            slots += 16 - (slots % 16); // play by the rules
        }
        self.compile.aarch64.patch(reserve_stack, sub_im(X64, sp, sp, slots as i64, 0));
        for (fst, snd, thd) in self.release_stack.drain(0..) {
            self.compile.aarch64.patch(fst, add_im(X64, sp, fp, 0, 0)); // Note: normal mov encoding can't use sp
            self.compile.aarch64.patch(snd, ldp_so(X64, fp, lr, sp, 0)); // get our return address
            self.compile.aarch64.patch(thd, add_im(X64, sp, sp, 16, 0));
        }
        self.compile.program[f].aarch64_stack_bytes = Some(slots);
        Ok(())
    }

    fn emit_block(&mut self, b: usize, args_not_vstacked: bool) -> Res<'p, ()> {
        if self.block_ips[b].is_some() {
            return Ok(());
        }
        self.block_ips[b] = Some(self.compile.aarch64.next);
        let f = self.f;
        let block = &self.body.blocks[b];

        let slots = block.arg_slots;
        let mask = block.arg_float_mask;
        self.clock = block.clock;
        // debug_assert_eq!(block.height, 0);
        // TODO: handle normal args here as well.
        if args_not_vstacked {
            self.state.free_reg.clear();
            let int_count = slots as u32 - mask.count_ones();
            if slots > 0 {
                self.ccall_reg_to_stack(slots, mask);
            }
            for i in int_count..8 {
                self.drop_reg(i as i64);
            }
        }
        let func = self.body;
        let mut is_done = false;
        for i in 0..func.blocks[b].insts.len() {
            // TOOD: hack
            if is_done {
                break;
            }
            // debug_assert!(!is_done);
            let block = &self.body.blocks[b];
            let inst = block.insts[i];
            let mask = block.arg_float_mask;
            is_done = self.emit_inst(b, inst, i)?;
            debugln!("{b}:{i} {inst:?} => {:?} | free: {:?}", self.state.stack, self.state.free_reg);
        }
        debug_assert!(is_done);
        Ok(())
    }

    fn emit_inst(&mut self, b: usize, inst: Bc, i: usize) -> Res<'p, bool> {
        if TRACE_ASM || self.log_asm_bc {
            let ins = self
                .compile
                .aarch64
                .offset_words(self.compile.aarch64.current_start, self.compile.aarch64.next)
                - 1;

            self.markers.push((format!("[{b}:{i}] {inst:?}: {:?}", self.state.stack), ins as usize));
        }
        match inst {
            Bc::AddrFnResult => {
                if let Some(slot) = self.flat_result {
                    self.state.stack.push(Val::Spill(slot));
                } else {
                    todo!() // x8
                }
            }
            Bc::Noop => {}
            Bc::LastUse { id } => {
                // TODO: I this doesn't work because if blocks are depth first now, not in order,
                //       so the var can be done after they rejoin and then the other branch thinks that slot is free
                //       and puts something else in it. -- May 6
                let slot = self.vars[id as usize]; // .take();
                let slot = slot.unwrap();
                let ty = self.body.vars[id as usize];
                let count = self.compile.slot_count(ty);
                self.drop_slot(slot, count * 8);
            }
            Bc::NoCompile => unreachable!("{}", self.compile.program[self.f].log(self.compile.pool)),
            Bc::CallSplit { rt, ct } => {
                //     let f = if self.when == ExecTime::Comptime { ct } else { rt };
                //     self.call_direct(f)?;
                todo!()
            }
            Bc::PushConstant { value } => self.state.stack.push(Val::Literal(value)),
            Bc::GetNativeFnPtr(f) => {
                // TODO: use adr+adrp instead of an integer. but should do that in load_imm so it always happens.

                if let Some(ptr) = self.compile.aarch64.get_fn(f) {
                    self.state.stack.push(Val::Literal(ptr as i64));
                } else if let Some(addr) = self.compile.program[f].comptime_addr {
                    self.state.stack.push(Val::Literal(addr as i64));
                } else {
                    assert!(f.as_index() < 4096);
                    let reg = self.get_free_reg();
                    // you don't really need to do this but i dont trust it cause im not following the calling convention
                    self.load_imm(x21, self.compile.aarch64.dispatch.as_ptr() as u64); // NOTE: this means you can't ever resize
                    self.compile.aarch64.push(ldr_uo(X64, reg, x21, f.as_index() as i64));
                    self.state.stack.push(Val::Increment { reg, offset_bytes: 0 })
                }
            }
            Bc::JumpIf { true_ip, false_ip, slots } => {
                let cond = self.pop_to_reg();
                let mask = self.body.blocks[true_ip.0 as usize].arg_float_mask;
                debug_assert_eq!(slots, 0); // self.stack_to_ccall_reg(slots, mask);
                self.spill_abi_stompable();
                self.compile.aarch64.push(brk(0));

                // branch if zero so true before false
                self.patch_cbz.push((self.compile.aarch64.prev(), false_ip, cond));
                // we only do one branch so true block must be directly after the check.
                // this is the only shape of flow graph that its possible to generate with my ifs/whiles.
                debug_assert!(self.block_ips[true_ip.0 as usize].is_none());

                self.drop_reg(cond);
                let state = self.state.clone();
                self.emit_block(true_ip.0 as usize, true);
                self.state = state;
                self.emit_block(false_ip.0 as usize, true);

                return Ok(true);
            }
            Bc::Goto { ip, slots } => {
                let block = &self.body.blocks[ip.0 as usize];
                debug_assert_eq!(slots, block.arg_slots);
                if block.incoming_jumps == 1 {
                    debug_assert!(self.block_ips[ip.0 as usize].is_none());
                    self.emit_block(ip.0 as usize, false);
                } else {
                    let mask = block.arg_float_mask;
                    self.stack_to_ccall_reg(slots, mask);
                    self.spill_abi_stompable();
                    if self.block_ips[ip.0 as usize].is_some() {
                        self.compile.aarch64.push(brk(0));
                        self.patch_b.push((self.compile.aarch64.prev(), ip));
                    } else {
                        // If we haven't emitted it yet, it will be right after us, so just fall through.
                        self.emit_block(ip.0 as usize, true);
                    }
                }
                return Ok(true);
            }
            Bc::CallDirect { f, tail } => {
                let target = &self.compile.program[f];
                debug_assert!(target.any_reg_template.is_none());
                let comp_ctx = match self.compile.program[f].cc.unwrap() {
                    CallConv::Arg8Ret1Ct => true,
                    CallConv::Arg8Ret1 | CallConv::OneRetPic => false,
                    _ => err!("expected c_call",),
                };
                let f_ty = target.unwrap_ty();

                // TODO: use with_link for tail calls. need to "Leave holes for stack fixup code." like below
                // TODO: if you already know the stack height of the callee, you could just fixup to that and jump in past the setup code. but lets start simple.
                self.dyn_c_call(f_ty, comp_ctx, |s| {
                    if tail {
                        s.emit_stack_fixup();
                    }
                    s.branch_func(f, !tail)
                });
                return Ok(tail);
            }
            Bc::CallDirectFlat { f } => {
                self.call_direct_flat(f);
            }
            Bc::Ret => {
                let ff = &self.compile.program[self.f];
                let cc = ff.cc.unwrap();
                let ret_ty = ff.finished_ret.unwrap();
                let arg_size = self.compile.slot_count(ff.finished_arg.unwrap());
                let ret_size = self.compile.slot_count(ret_ty);
                match cc {
                    CallConv::Arg8Ret1 => {
                        // We have the values on virtual stack and want them in r0-r7, that's the same as making a call.
                        let floats = self.compile.program.float_mask_one(ret_ty);
                        self.stack_to_ccall_reg(ret_size, floats)
                    }
                    CallConv::Flat => {
                        // We now require the bytecode to deal with putting values in the result address.
                        // so nothing to do here.
                    }
                    CallConv::Inline | CallConv::OneRetPic | CallConv::Arg8Ret1Ct => unreachable!("unsupported cc {cc:?}"),
                }
                // debug_assert!(self.state.stack.is_empty()); // todo

                self.emit_stack_fixup();
                // Do the return
                self.compile.aarch64.push(ret(()));
                return Ok(true);
            }
            Bc::AddrVar { id } => {
                if let Some(slot) = self.vars[id as usize] {
                    self.state.stack.push(Val::Increment {
                        reg: sp,
                        offset_bytes: slot.0,
                    });
                } else {
                    let ty = self.body.vars[id as usize];
                    let count = self.compile.slot_count(ty);
                    let slot = self.create_slots(count);
                    self.vars[id as usize] = Some(slot);
                    self.state.stack.push(Val::Increment {
                        reg: sp,
                        offset_bytes: slot.0,
                    });
                }
            }
            // Note: we do this statically, it wont get actually applied to a register until its needed because loads/stores have an offset immediate.
            Bc::IncPtr { offset } => match self.state.stack.last_mut().unwrap() {
                Val::Increment { reg, offset_bytes: prev } => {
                    *prev += offset * 8;
                }
                Val::Literal(v) => {
                    *v += offset as i64 * 8;
                }
                Val::Spill(_) => {
                    // TODO: should it hold the add statically? rn it does the load but doesn't need to restore because it just unspills it.
                    let (reg, offset_bytes) = self.pop_to_reg_with_offset();
                    self.state.stack.push(Val::Increment {
                        reg,
                        offset_bytes: offset_bytes + (offset * 8),
                    })
                }
                Val::FloatReg(_) => err!("don't try to gep a float",),
            },
            Bc::Load { slots } => self.emit_load(slots),
            Bc::StorePost { slots } => self.emit_store(slots),
            Bc::StorePre { slots } => {
                let ptr = self.state.stack.remove(self.state.stack.len() - slots as usize - 1);
                self.state.stack.push(ptr);
                self.emit_store(slots);
            }
            Bc::TagCheck { expected } => {
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
                // TODO: tail call
                assert!(!comp_ctx, "flat call needs special handling");
                self.dyn_c_call(ty, comp_ctx, |s| {
                    // dyn_c_call will have popped the args, so now stack is just the pointer to call
                    // TODO: this is for sure a bug!!!! make a test that calls something with lots of argument through a dynamic function pointer.
                    let reg = s.pop_to_reg(); // TODO: i'd rather be able to specify that this be x16 so you know its not part of the cc. -- May 1
                    s.compile.aarch64.push(br(reg, 1));
                    s.drop_reg(reg);
                });
            }
            Bc::Unreachable => {
                self.compile.aarch64.push(brk(0xbabe));

                return Ok(true);
            }
            Bc::Pop { slots } => {
                debug_assert!(self.state.stack.len() >= slots as usize);
                for _ in 0..slots {
                    match self.state.stack.pop().unwrap() {
                        Val::Increment { reg, .. } => self.drop_reg(reg),
                        Val::Literal(_) => {}
                        Val::Spill(slot) => self.drop_slot(slot, 8),
                        Val::FloatReg(_) => todo!(),
                    }
                }
            }
            Bc::Dup => {
                let val = *self.state.stack.last().unwrap();
                match val {
                    Val::Increment { reg, offset_bytes } => {
                        if reg == sp {
                            self.state.stack.push(val);
                        } else {
                            let new = self.get_free_reg();
                            self.compile.aarch64.push(mov(X64, new, reg));
                            self.state.stack.push(Val::Increment { reg: new, offset_bytes });
                        }
                    }
                    Val::Literal(_) | Val::Spill(_) => self.state.stack.push(val),
                    Val::FloatReg(_) => todo!(),
                }
            }
            Bc::CopyToFrom { slots } => {
                if slots <= 4 {
                    let (from, from_offset) = self.pop_to_reg_with_offset();
                    let (to, to_offset) = self.pop_to_reg_with_offset();
                    let temp = self.get_free_reg();
                    for i in 0..slots {
                        self.load_u64(temp, from, from_offset + i * 8);
                        self.store_u64(temp, to, to_offset + i * 8);
                    }
                    self.drop_reg(from);
                    self.drop_reg(to);
                    self.drop_reg(temp);
                } else {
                    self.state.stack.push(Val::Literal(slots as i64 * 8));
                    self.stack_to_ccall_reg(3, 0);
                    self.spill_abi_stompable();
                    let addr = libc::memcpy as *const u8;
                    // let offset = self.compile.aarch64.offset_words(self.compile.aarch64.next, addr);
                    self.load_imm(x17, addr as u64);
                    self.compile.aarch64.push(br(x17, 1));
                    for i in 0..8 {
                        add_unique(&mut self.state.free_reg, i as i64);
                    }
                }
            }
        }

        Ok(false)
    }

    // TODO: refactor this. its a problem that im afraid of it! -- May 8
    #[track_caller]
    fn stack_to_ccall_reg(&mut self, slots: u16, float_mask: u32) {
        debug_assert!((slots as u32 - float_mask.count_ones()) < 8);
        debug_assert!(self.state.stack.len() >= slots as usize);
        let mut next_int = 0;
        let mut next_float = 0;

        for slot_index in 0..(slots as usize) {
            debug_assert_eq!(slot_index, next_int + next_float);
            let f = (float_mask >> (slots - slot_index as u16 - 1)) & 1 == 1;
            let stack_index = self.state.stack.len() - (slots as usize) + slot_index;

            if f {
                if let Val::FloatReg(have) = self.state.stack[stack_index] {
                    if have == next_float as i64 {
                        // its already where we want it.
                        self.state.stack[stack_index] = Val::Literal(0); // just make sure we dont see it again later.
                        next_float += 1;
                        continue;
                    }
                }

                debug!("|(v{}) <- ({:?})| ", next_float, self.state.stack[stack_index]);

                for i in 0..self.state.stack.len() {
                    if let Val::FloatReg(x) = self.state.stack[i] {
                        if x == next_float as i64 {
                            // Someone else already has the one we want, so spill that to the stack since i don't have fmov encoding. TODO
                            let worked = self.try_spill(i, true);
                            assert!(worked);
                        }
                    }
                }

                match self.state.stack[stack_index] {
                    Val::Increment { reg, offset_bytes } => {
                        debug_assert_eq!(offset_bytes, 0, "dont GEP a float");
                        debug_assert_ne!(reg, sp, "dont fmov the stack pointer");
                        // TODO: fmov encoding.
                        let worked = self.try_spill(stack_index, false);
                        assert!(worked);
                    }
                    Val::Literal(x) => {
                        // TODO: fmov encoding.
                        let reg = self.get_free_reg();
                        self.load_imm(reg, u64::from_le_bytes(i64::to_le_bytes(x)));
                        self.state.stack[stack_index] = Val::Increment { reg, offset_bytes: 0 };
                        let worked = self.try_spill(stack_index, false);
                        assert!(worked);
                    }
                    Val::Spill(_) => {}
                    Val::FloatReg(_) => {
                        // TODO: fmov encoding.
                        let worked = self.try_spill(stack_index, true);
                        assert!(worked);
                    }
                }

                let Val::Spill(slot) = self.state.stack[stack_index] else {
                    unreachable!()
                };
                self.compile.aarch64.push(f_ldr_uo(X64, next_float as i64, sp, slot.0 as i64 / 8));
                self.drop_slot(slot, 8);

                next_float += 1;
                continue;
            }

            if let Val::Increment { reg, offset_bytes } = self.state.stack[stack_index] {
                if reg == next_int as i64 {
                    if offset_bytes != 0 {
                        self.compile.aarch64.push(add_im(X64, reg, reg, offset_bytes as i64, 0));
                    }

                    // if we happen to already be in the right register, cool, don't worry about it.
                    debug!("|x{} already| ", reg);
                    self.state.free_reg.retain(|r| reg != *r);
                    self.state.stack[stack_index] = Val::Literal(0); // make sure we dont try to spill it later
                    next_int += 1;
                    continue;
                }
            }

            if let Some(want) = self.state.free_reg.iter().position(|r| *r as usize == next_int) {
                self.state.free_reg.remove(want);
            } else if let Some(used) = self
                .state
                .stack
                .iter()
                .position(|r| r.reg().map(|(r, _)| r as usize == next_int).unwrap_or(false))
            {
                // The one we want is already used by something on the v-stack; swap it with a random free one. TODO: put in the right place if you can.
                let (old_reg, offset_bytes) = self.state.stack[used].reg().unwrap();
                debug_assert_ne!(old_reg, sp); // encoding but unreachable
                let reg = self.get_free_reg();
                self.compile.aarch64.push(mov(X64, reg, old_reg));
                self.state.stack[used] = Val::Increment { reg, offset_bytes };
                // Now x{i} is free and we'll use it below.
            } else {
                panic!(
                    "TODO: x{next_int} is not free. stack is {:?}. free is {:?}",
                    self.state.stack, self.state.free_reg
                );
            }

            debug!("|(x{}) <- ({:?})| ", next_int, self.state.stack[stack_index]);
            match self.state.stack[stack_index] {
                Val::Increment { reg, offset_bytes } => {
                    debug_assert_ne!(reg as usize, next_int);
                    // even if offset_bytes is 0, we need to move it to the right register. and add can encode that mov even if reg is sp.
                    self.compile.aarch64.push(add_im(X64, next_int as i64, reg, offset_bytes as i64, 0));
                }
                Val::Literal(x) => self.load_imm(next_int as i64, x as u64),
                Val::Spill(slot) => self.load_u64(next_int as i64, sp, slot.0),
                Val::FloatReg(_) => todo!(),
            }
            self.state.free_reg.retain(|r| next_int as i64 != *r);
            // TODO: why can't i do this ? feels like this has gotta be a lurking bug -- May 8
            // self.state.stack[stack_index] = Val::Literal(0); // make sure we dont try to spill it later
            next_int += 1;
        }
        debugln!();
        self.state.stack.truncate(self.state.stack.len() - slots as usize);
        debug_assert_eq!(next_float as u32, float_mask.count_ones());
        debug_assert_eq!(next_int as u32, slots as u32 - float_mask.count_ones());
    }

    // TODO: floats -- May 1
    fn ccall_reg_to_stack(&mut self, slots: u16, float_mask: u32) {
        debug_assert!((slots as u32 - float_mask.count_ones()) < 8);
        let mut next_float = 0;
        let mut next_int = 0;
        for i in 0..slots {
            debug_assert_eq!(i, (next_int + next_float) as u16);
            let f = (float_mask >> (slots - i - 1)) & 1 == 1;
            let v = if f {
                Val::FloatReg(next_float)
            } else {
                self.state.free_reg.retain(|r| *r != next_int);
                Val::Increment {
                    reg: next_int,
                    offset_bytes: 0,
                }
            };
            *(if f { &mut next_float } else { &mut next_int }) += 1;
            self.state.stack.push(v);
        }

        debug_assert_eq!(next_float as u32, float_mask.count_ones());
        debug_assert_eq!(next_int as u32, slots as u32 - float_mask.count_ones());
    }

    /// <ptr:1> -> <?:n>
    fn emit_load(&mut self, slots: u16) {
        debug_assert!(!self.state.stack.is_empty());
        debug!("LOAD: [{:?}] to ", self.state.stack.last().unwrap());
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
            self.state.stack.push(v);
            debug!("({:?}), ", self.state.stack.last().unwrap());
            offset_bytes += 8;
            // saved.0 += 8;
        }
        debugln!();
        debug_assert!(offset_bytes / 8 < (1 << 12));
        self.drop_reg(reg);
    }

    /// <?:n> <ptr:1> -> _
    #[track_caller]
    fn emit_store(&mut self, slots: u16) {
        debug_assert!(self.state.stack.len() > slots as usize, "want store {slots} slots");
        debugln!(
            "STORE: ({:?}) to [{:?}]",
            &self.state.stack[self.state.stack.len() - slots as usize - 1..self.state.stack.len() - 1],
            self.state.stack.last().unwrap()
        );
        let (reg, mut offset_bytes) = self.pop_to_reg_with_offset();
        // Note: we're going backwards: popping off the stack and storing right to left.
        for i in (0..slots).rev() {
            let o = offset_bytes + (i * 8);
            let v = self.state.stack.last().unwrap();
            debug!("| [x{reg} + {o}] <- ({:?}) |", v);
            if let &Val::FloatReg(r) = v {
                self.state.stack.pop().unwrap();
                self.compile.aarch64.push(f_str_uo(X64, r, reg, o as i64 / 8))
            } else {
                let val = self.pop_to_reg();
                self.store_u64(val, reg, o);
                self.drop_reg(val);
            }
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
        self.state.free_reg.clear();
        self.state.free_reg.extend([0, 1, 2, 3, 4, 5, 6, 7]);
    }

    fn spill_abi_stompable(&mut self) {
        for i in 0..self.state.stack.len() {
            self.try_spill(i, true);
        }
        debugln!();
    }

    // do_floats:false if you're just trying to free up a gpr, not saving for a call.
    fn try_spill(&mut self, i: usize, do_floats: bool) -> bool {
        let v = self.state.stack[i];
        if let Val::Increment { reg, offset_bytes } = v {
            if reg == sp {
                return false;
            }

            // Note: this assumes we don't need to preserve the value in the reg other than for this one v-stack slot.
            let slot = self.create_slots(1);
            debug!("(spill (x{reg:?} + {offset_bytes}) -> [sp, {}]) ", slot.0);
            if offset_bytes != 0 {
                self.compile.aarch64.push(add_im(X64, reg, reg, offset_bytes as i64, 0));
            }

            self.store_u64(reg, sp, slot.0);
            self.drop_reg(reg);
            self.state.stack[i] = Val::Spill(slot);
            return true;
        }

        if do_floats {
            if let Val::FloatReg(freg) = v {
                let slot = self.create_slots(1);
                self.compile.aarch64.push(f_str_uo(X64, freg, sp, slot.0 as i64 / 8));
                self.state.stack[i] = Val::Spill(slot);
                return true;
            }
        }

        false
    }

    fn get_free_reg(&mut self) -> i64 {
        if let Some(r) = self.state.free_reg.pop() {
            debug_assert_ne!(r, sp);
            r
        } else {
            let mut i = 0;
            while i < self.state.stack.len() && !self.try_spill(i, false) {
                i += 1;
            }
            if i == self.state.stack.len() {
                panic!("spill to stack failed");
            }
            self.state.free_reg.pop().unwrap()
        }
    }

    fn drop_reg(&mut self, reg: i64) {
        if reg != sp {
            debug_assert!(!self.state.free_reg.contains(&reg), "drop r{reg} -> {:?}", self.state.free_reg);
            self.state.free_reg.push(reg);
            if ZERO_DROPPED_REG {
                self.load_imm(reg, 0);
            }
        }
    }

    fn pop_to_reg(&mut self) -> i64 {
        match self.state.stack.pop().unwrap() {
            Val::Increment { mut reg, offset_bytes } => {
                if offset_bytes > 0 {
                    let out = if reg == sp { self.get_free_reg() } else { reg };
                    if offset_bytes < (1 << 12) {
                        self.compile.aarch64.push(add_im(X64, out, reg, offset_bytes as i64, 0));
                    } else {
                        self.compile.aarch64.push(add_im(X64, out, reg, (offset_bytes >> 12) as i64, 1));
                        self.compile.aarch64.push(add_im(X64, out, reg, offset_bytes as i64 & ((1 << 12) - 1), 0));
                    }
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
                self.drop_slot(slot, 8);
                r
            }
            Val::FloatReg(_) => todo!(),
        }
    }

    fn peek_to_reg_with_offset(&mut self) -> (i64, u16) {
        match self.state.stack.last().cloned().unwrap() {
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
            Val::FloatReg(_) => todo!(),
        }
    }

    fn pop_to_reg_with_offset(&mut self) -> (i64, u16) {
        let res = self.peek_to_reg_with_offset();
        self.state.stack.pop().unwrap();
        res
    }

    fn create_slots(&mut self, count: u16) -> SpOffset {
        for (i, &(_, size, clock)) in self.state.open_slots.iter().enumerate() {
            if self.clock >= clock && size == count * 8 {
                return self.state.open_slots.remove(i).0;
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
                let part = value & bottom;
                if part != 0 {
                    self.compile.aarch64.push(movk(X64, reg, part as i64, shift));
                }
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
            let first_arg_index = self.state.stack.len() - arg_count as usize;
            // TODO: this has gotta be UB
            let c = self.compile as *const Compile as u64;
            let p = &self.compile.program as *const &mut Program as u64;
            debug_assert_eq!(c, p, "need repr c");
            // put it on the stack as though it were a normal argument so the cc assigner doesn't need a special case
            self.state.stack.insert(first_arg_index, Val::Literal(c as i64));
        }

        // TODO: if i always just recompute liek this, dont bother putting it on the bc ibstructions -- May 3
        let floats = self.compile.program.float_mask_one(f_ty.arg);
        self.stack_to_ccall_reg(arg_count + reg_offset, floats);
        self.spill_abi_stompable();
        do_call(self);
        let floats = self.compile.program.float_mask_one(f_ty.ret);
        self.ccall_reg_to_stack(ret_count, floats);

        let float_count = floats.count_ones();
        let int_count = ret_count as u32 - float_count;

        for i in int_count..7 {
            add_unique(&mut self.state.free_reg, i as i64); // now the extras are usable again.
        }
    }

    // stack must be [<ret_ptr>, <arg_ptr>]. bc needs to deal with loading/storing stuff from memory.
    fn call_direct_flat(&mut self, f: FuncId) -> Res<'p, ()> {
        let target = &self.compile.program[f];
        debug_assert!(target.any_reg_template.is_none());
        debug_assert_eq!(target.cc, Some(CallConv::Flat));
        let f_ty = target.unwrap_ty();

        debugln!("flat_call");
        // (compiler, arg_ptr, arg_len_i64s, ret_ptr, ret_len_i64s)

        let addr = target.comptime_addr.or_else(|| self.compile.aarch64.get_fn(f).map(|v| v as u64));

        let arg_ptr = self.state.stack.pop().unwrap();
        let ret_ptr = self.state.stack.pop().unwrap();
        let arg_count = self.compile.slot_count(f_ty.arg);
        let ret_count = self.compile.slot_count(f_ty.ret);

        let c = self.compile as *const Compile as i64;
        self.state.stack.push(Val::Literal(c));
        self.state.stack.push(arg_ptr);
        self.state.stack.push(Val::Literal(arg_count as i64));
        self.state.stack.push(ret_ptr);
        self.state.stack.push(Val::Literal(ret_count as i64));

        self.stack_to_ccall_reg(5, 0);
        self.spill_abi_stompable();
        self.branch_func(f, true);

        for i in 0..7 {
            add_unique(&mut self.state.free_reg, i as i64); // now the extras are usable again.
        }

        Ok(())
    }

    const DO_BASIC_ASM_INLINE: bool = true;

    // TODO: use with_link for tail calls.
    // !with_link === tail
    fn branch_func(&mut self, f: FuncId, with_link: bool) {
        if Self::DO_BASIC_ASM_INLINE {
            // TODO: save result on the function so dont have to recheck every time?
            if let Some(code) = &self.compile.program[f].jitted_code {
                if self.compile.program[f].cc == Some(CallConv::OneRetPic) {
                    // TODO: HACK: for no-op casts, i have two rets because I can't have single element tuples.
                    if code.len() == 2 && code[0] as i64 == ret(()) && code[1] as i64 == ret(()) {
                        if !with_link {
                            // If you're trying to do a tail call to fn add, that means emit the add instruction and then return.
                            self.compile.aarch64.push(ret(()));
                        }
                        return;
                    }
                    for op in &code[0..code.len() - 1] {
                        debug_assert_ne!(*op as i64, ret(()));
                        self.compile.aarch64.push(*op as i64);
                    }
                    debug_assert_eq!(*code.last().unwrap() as i64, ret(()));
                    if !with_link {
                        // If you're trying to do a tail call to fn add, that means emit the add instruction and then return.
                        self.compile.aarch64.push(ret(()));
                    }
                    return;
                }
            }
        }

        // If we already emitted the target function, can just branch there directly.
        // This covers the majority of cases because I try to handle callees first.
        // Note: checking this before comptime_addr means we handle inline asm as a normal function.
        if let Some(bytes) = self.compile.aarch64.get_fn(f) {
            let mut offset = bytes as i64 - self.compile.aarch64.get_current() as i64;
            debug_assert!(offset % 4 == 0, "instructions are u32 but {offset}%4");
            offset /= 4;
            // TODO: use adr/adrp
            if offset.abs() < (1 << 25) {
                offset = signed_truncate(offset, 26);
                self.compile.aarch64.push(b(offset, with_link as i64));
                return;
            }
        }

        // TODO: maybe its prefereable to use the dispatch table even when its const because then runtime code could setup the pointers with dlopen,
        //       and could reuse the same code for comptime and runtime.
        if let Some(bytes) = self.compile.program[f].comptime_addr {
            debug_assert!(self.compile.program[f].jitted_code.is_none(), "inline asm should be emitted normally");
            let mut offset = bytes as i64 - self.compile.aarch64.get_current() as i64;
            debug_assert!(offset % 4 == 0, "instructions are u32 but {offset}%4");
            offset /= 4;

            // If its a comptime_addr into the compiler, (which happens a lot because of assert_eq and tag_value),
            //     aslr might be our friend and just put the mmaped pages near where it originally loaded the compiler.
            if offset.abs() < (1 << 25) {
                offset = signed_truncate(offset, 26);
                self.compile.aarch64.push(b(offset, with_link as i64));
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
            // you don't really need to do this but i dont trust it cause im not following the calling convention
            self.load_imm(x21, self.compile.aarch64.dispatch.as_ptr() as u64); // NOTE: this means you can't ever resize
            self.compile.aarch64.push(ldr_uo(X64, x16, x21, f.as_index() as i64));
        }

        self.compile.aarch64.push(br(x16, with_link as i64))
    }

    fn drop_slot(&mut self, slot: SpOffset, bytes: u16) {
        self.state.open_slots.push((slot, bytes, self.clock)); // TODO: keep this sorted by count?
        if ZERO_DROPPED_SLOTS {
            // todo this wont work now that i try to use x17
            self.load_imm(x17, 0);
            for i in 0..(bytes / 8) {
                self.store_u64(x17, sp, slot.0 + (i * 8));
            }
        }
    }

    fn emit_stack_fixup(&mut self) {
        self.compile.aarch64.push(brk(0));
        let a = self.compile.aarch64.prev();
        self.compile.aarch64.push(brk(0));
        let b = self.compile.aarch64.prev();
        self.compile.aarch64.push(brk(0));
        self.release_stack.push((a, b, self.compile.aarch64.prev()));
    }
}

impl Val {
    fn reg(&self) -> Option<(i64, u16)> {
        match self {
            &Val::Increment { reg, offset_bytes } => Some((reg, offset_bytes)),
            Val::Literal(_) => None,
            Val::Spill(_) => None,
            Val::FloatReg(_) => todo!(),
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
            &Value::Label(return_from) => return_from.as_raw(),
            Value::Unit => 0, // TODO
            &Value::Heap(ptr) => ptr as usize as i64,
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
        pub ranges: Vec<*const [u8]>,
        pub current_start: *const u8,
        pub next: *mut u8,
        old: *mut u8,
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
                dispatch: Vec::with_capacity(99999), // Dont ever resize!
                ranges: vec![],
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

        pub fn get_fn(&self, f: FuncId) -> Option<*const u8> {
            if self.dispatch[f.as_index()].is_null() {
                None
            } else {
                Some(self.dispatch[f.as_index()])
            }
        }

        #[allow(clippy::not_unsafe_ptr_arg_deref)]
        pub fn offset_words(&self, from_ip: *const u8, to_ip: *const u8) -> i64 {
            unsafe { (to_ip.offset_from(from_ip) as i64 / 4) }
        }

        pub fn prev(&self) -> *const u8 {
            unsafe { self.next.offset(-4) }
        }

        pub fn patch(&mut self, ip: *const u8, inst_value: i64) {
            unsafe {
                let ptr = ip as *mut u32;
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

        // Recursion shouldn't have to slowly lookup the start address.
        pub fn mark_start(&mut self, f: FuncId) {
            debug_assert_eq!(self.current_start as usize % 4, 0);
            self.dispatch[f.as_index()] = self.current_start;
        }

        pub fn save_current(&mut self, f: FuncId) {
            debug_assert!(self.map_mut.is_some());
            unsafe {
                // TODO: make sure there's not an off by one thing here.
                let range = slice::from_raw_parts(self.current_start, self.next.offset_from(self.current_start) as usize);
                self.dispatch[f.as_index()] = self.current_start;
                self.ranges[f.as_index()] = range as *const [u8];
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
