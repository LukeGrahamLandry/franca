//! TODO: i know this is remarkably bad codegen.
//! @c_call means https://en.wikipedia.org/wiki/Calling_convention#ARM_(A64)

#![allow(non_upper_case_globals)]

use crate::ast::{FnType, FuncId, TypeId};
use crate::bc::{Bc, StackOffset, StackRange, Value};
use crate::compiler::Res;
use crate::experiments::bootstrap_gen::*;
use crate::interp::Interp;
use crate::logging::{err, PoolLog};
use crate::{ast::Program, bc::FnBody};

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct SpOffset(usize);

pub struct BcToAsm<'z, 'a, 'p> {
    pub program: &'z Program<'p>,
    pub interp: &'z Interp<'a, 'p>,
    pub asm: Jitted,
    pub slots: Vec<Option<SpOffset>>,
    pub open_slots: Vec<(SpOffset, usize)>,
    pub next_slot: SpOffset,
    f: FuncId,
    slot_types: Option<&'z [TypeId]>
}

const x0: i64 = 0;
const x1: i64 = 1;
// const x2: i64 = 2;
// const x3: i64 = 3;
// const x4: i64 = 4;
// const x5: i64 = 5;
// const x6: i64 = 6;
// const x7: i64 = 7;
// const x8: i64 = 8;
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

impl<'z, 'a, 'p> BcToAsm<'z, 'a, 'p> {
    pub fn new(interp: &'z Interp<'a, 'p>, program: &'z Program<'p>) -> Self {
        Self {
            program,
            interp,
            asm: Jitted::new(1<<26),  // Its just virtual memory right? I really don't want to ever run out of space and need to change the address.
            slots: vec![],
            open_slots: vec![],
            next_slot: SpOffset(0),
            f: FuncId(0),  // TODO: bad
            slot_types: None
        }
    }

    // TODO: i cant keep copy pasting this shape. gonna cry.
    pub fn compile(&mut self, f: FuncId) -> Res<'p, ()> {
        self.asm.reserve(f.0);
        if self.asm.get_fn(f).is_some() {
            return Ok(());
        }
        self.asm.make_write();

        let callees = self.program.funcs[f.0]
            .wip
            .as_ref()
            .unwrap()
            .callees
            .clone();
        for c in callees {
            self.compile(c)?;
        }

        let func = &self.program.funcs[f.0];
        if let Some(insts) = func.jitted_code.as_ref() {
            // TODO: i dont like that the other guy leaked the box for the jitted_code ptr. i'd rather everything share the one Jitted instance.
            // TODO: this needs to be aware of the distinction between comptime and runtime target.
            for op in insts {
                self.asm.push(*op as i64);
            }
        } else {
            self.bc_to_asm(self.interp.ready[f.0].as_ref().unwrap())?;
        }
        self.asm.save_current(f);
        Ok(())
    }

    fn bc_to_asm(&mut self, func: &'z FnBody<'p>) -> Res<'p, ()> {
        println!("{}", func.log(self.program.pool));
        let ff = &self.program.funcs[func.func.0];
        self.slot_types = Some(&func.slot_types);
        self.f = func.func;
        self.next_slot = SpOffset(0);
        self.slots.clear();
        self.slots.extend(vec![None; func.stack_slots]);
        self.open_slots.clear();
        let is_c_call = ff.has_tag(self.program.pool, "c_call");
        let slots = if func.stack_slots % 2 == 0 {
            func.stack_slots + 2
        } else {
            func.stack_slots + 1 + 2
        };
        // TODO: always put this at 0 and just offset everyone else
        let SAVED_RET: i64 = (slots - 2) as i64;
        println!("SAVED_RET {}", SAVED_RET * 8);
        assert!(slots < 63, "range for stp/ldp");
        self.asm.push(brk(0));
        let reserve_stack = self.asm.prev();
        let mut release_stack = vec![];
        self.asm.push(stp_so(X64, fp, lr, sp, SAVED_RET));  // save our return address
        if is_c_call {
            assert!(
                func.arg_range.count <= 8,
                "c_call only supports 8 arguments. TODO: pass on stack"
            );
            for i in func.arg_range {
                if func.slot_types[i].is_unit() {
                    continue
                }
                self.set_slot((i - func.arg_range.first.0) as i64, StackOffset(i));
            }
        }

        let mut patch_cbz = vec![];
        let mut patch_b = vec![];

        for (i, inst) in func.insts.iter().enumerate() {
            self.asm.mark_next_ip();
            match inst {
                &Bc::LastUse(slots) => {
                    self.release_many(slots);
                }
                Bc::NoCompile => unreachable!(),
                Bc::CallDynamic { .. } => todo!(),
                Bc::CallDirect { f, ret, arg } => {
                    let target = &self.program.funcs[f.0];
                    let target_c_call = target.has_tag(self.program.pool, "c_call");
                    if target_c_call {
                        // if let Some(bytes) = self.asm.get_fn(*f) {
                        //     // TODO: shoyld only do this for comptime functions. for runtime, want to be able to squash everything down.
                        //     //       or maybe should have two Jitted. full seperation between comptime and runtime and compile everything twice,
                        //     //       since need to allow that for cross compiling anyway.
                        //     // Would be interesting to always use the indirect because then you could do super powerful things
                        //     // for mixin patching other code. redirect any call. tho couldn't rely on that cause it might inline.
                        //     // also feels gross. but i've kinda just invented a super heavy handed context pointer which some languages
                        //     // do for allocators/logging anyway
                        //     todo!("if we already emitted the function, don't need to do an indirect call, can just branch there since we know the offset")
                        // } else {
                            assert!(f.0 < 512);
                            self.asm.push(ldr_uo(X64, x16, x21, f.0 as i64));
                            self.dyn_c_call(x16, *arg, *ret, target.unwrap_ty());
                        // }
                    } else {
                        todo!()
                    }
                }
                Bc::CallBuiltin { .. } => todo!(),
                Bc::LoadConstant { slot, value } => match value {
                    Value::F64(_) => todo!(),
                    Value::I64(n) => {
                        self.load_imm(x0, u64::from_le_bytes(n.to_le_bytes()));
                        self.set_slot(x0, *slot);
                    }
                    // These only make sense during comptime execution, but they're also really just numbers.
                    Value::OverloadSet(i)
                    | Value::GetFn(FuncId(i))
                    | Value::Type(TypeId(i))
                    | Value::Symbol(i) => {
                        self.load_imm(x0, *i as u64);
                        self.set_slot(x0, *slot);
                    }
                    &Value::Bool(b) => {
                        self.asm.push(movz(X64, x0, b as i64, 0));
                        self.set_slot(x0, *slot);
                    },
                    Value::Unit => {}
                    Value::Poison => todo!(),
                    Value::InterpAbsStackAddr(_) => todo!(),
                    Value::Heap { .. } => todo!(),
                },
                &Bc::JumpIf { cond, true_ip, false_ip } => {
                    self.get_slot(x0, cond);
                    self.asm.push(brk(0));
                    assert_eq!(true_ip, i + 1);
                    patch_cbz.push((self.asm.prev(), false_ip));
                },
                &Bc::Goto { ip } => {
                    self.asm.push(brk(0));
                    patch_b.push((self.asm.prev(), ip));
                },
                Bc::Ret(slot) => {
                    if is_c_call {
                        match slot.count {
                            0 => {}
                            1 => {
                                self.get_slot(x0, slot.single());
                            }
                            _ => err!("c_call only supports one return value. TODO: structs",),
                        }
                        dbg!(SAVED_RET);
                        self.asm.push(ldp_so(X64, fp, lr, sp, SAVED_RET));  // get our return address
                        self.asm.push(brk(0));
                        release_stack.push(self.asm.prev());
                        self.asm.push(ret(()));
                    } else {
                        todo!()
                    }
                }
                // Note: drop is on the value, we might immediately write to that slot and someone might have a pointer to it.
                Bc::Drop(_) | Bc::DebugMarker(_, _) | Bc::DebugLine(_) => {}
                &Bc::Move { from, to } => {
                    if !func.slot_is_var.get(from.0) && !func.slot_is_var.get(to.0) {
                        self.slots[to.0] = self.slots[from.0].take();
                    } else {
                        self.get_slot(x0, from);
                        self.set_slot(x0, to);
                    }
                }
                &Bc::Clone { from, to } => {
                    self.get_slot(x0, from);
                    self.set_slot(x0, to);
                }
                Bc::CloneRange { from, to } | Bc::MoveRange { from, to } => {
                    for i in 0..from.count {
                        self.get_slot(x0, StackOffset(from.first.0 + i));
                        self.set_slot(x0, StackOffset(to.first.0 + i));
                    }
                }
                &Bc::AbsoluteStackAddr { of, to } => {
                    let offset = self.find_many(of, false);
                    self.asm.push(add_im(X64, x0, sp, offset.0 as i64, 0));
                    self.set_slot(x0, to);
                },
                &Bc::SlicePtr { base, offset, ret, .. } => {
                    self.get_slot(x0, base);  // x0 = ptr
                    self.asm.push(add_im(X64, x0, x0, (offset * 8) as i64, 0)); // x0 += offset * size_of(i64)
                    self.set_slot(x0, ret);
                },
                &Bc::Load { from, to } => {
                    assert_eq!(to.count, 1);
                    self.get_slot(x0, from);  // x0 = ptr
                    self.asm.push(ldr_uo(X64, x0, x0, 0)); // x0 = *x0
                    self.set_slot(x0, to.single());  // out = x0
                },
                &Bc::TagCheck { enum_ptr, value } => {
                    self.get_slot(x0, enum_ptr);  // x0 = ptr
                    self.asm.push(ldr_uo(X64, x0, x0, 0)); // x0 = *x0 = tag
                    self.asm.push(cmp_im(X64, x0, value, 0));
                    self.asm.push(b_cond(9999, CmpFlags::NE as i64)); // TODO: do better than just hoping to jump into garbage
                },
                Bc::Store { to, from } => {
                    assert_eq!(from.count, 1);
                    self.get_slot(x0, from.single());  // x0 = val
                    self.get_slot(x1, *to);  // x1 = ptr
                    self.asm.push(str_uo(X64, x0, x1, 0));  // *x1 = x0
                }
                Bc::CallC { f, arg, ret, ty, comp_ctx } => {
                    self.get_slot(x16, *f);
                    self.dyn_c_call(x16, *arg, *ret, *ty);
                    self.release_one(*f);
                    assert!(!*comp_ctx);
                },
            }
        }
        for (inst, false_ip) in patch_cbz {
            let offset = self.asm.offset_words_inst_ip(inst, false_ip);
            self.asm.patch(inst, cbz(X64, signed_truncate(offset, 19), x0));
        }
        for (from_inst, to_ip) in patch_b {
            let dist = self.asm.offset_words_inst_ip(from_inst, to_ip);
            debug_assert_ne!(dist, 0, "while(1);");
            self.asm.patch(from_inst, b(signed_truncate(dist, 26), 0));
        }

        self.asm.patch(reserve_stack, sub_im(X64, sp, sp, (slots * 8) as i64, 0));
        for inst in release_stack {
            self.asm.patch(inst, add_im(X64, sp, sp, (slots * 8) as i64, 0));
        }
        // panic!();
        Ok(())
    }

    fn set_slot(&mut self, src_reg: i64, slot: StackOffset) {
        let offset = self.find_one(slot, true);
        self.asm.push(str_uo(X64, src_reg, sp, offset.0 as i64 / 8));
    }

    fn get_slot(&mut self, dest_reg: i64, slot: StackOffset) {
        let offset = self.find_one(slot, false);
        self.asm.push(ldr_uo(X64, dest_reg, sp, offset.0 as i64 / 8));
    }

    fn find_many(&mut self, slot: StackRange, allow_create: bool) -> SpOffset {
        if slot.count == 1 {
            return self.find_one(slot.single(), allow_create);
        }
        if let Some(slot) = self.slots[slot.first.0] {
            return slot
        }

        debug_assert!(allow_create);
        for (i, &(_, size)) in self.open_slots.iter().enumerate().rev() {
            if size == slot.count * 8 {
                let found = self.open_slots.remove(i);
                self.slots[slot.first.0] = Some(found.0);
                return found.0;
            }
        }

        let made = self.next_slot;
        self.slots[slot.first.0] = Some(made);
        self.next_slot.0 += 8;
        made
    }

    fn release_many(&mut self, slot: StackRange) {
        if let Some(r) = self.slots[slot.first.0].take() {
            self.open_slots.push((r, slot.count * 8));
        }
    }

    fn find_one(&mut self, slot: StackOffset, allow_create: bool) -> SpOffset {
        if let Some(slot) = self.slots[slot.0] {
            slot
        } else if let Some((made, size)) = self.open_slots.pop() {
            debug_assert!(allow_create);
            if size > 8 {
                self.open_slots.push((SpOffset(made.0 + 8), size - 8));
            }
            self.slots[slot.0] = Some(made);
            made
        } else {
            debug_assert!(allow_create);
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
        self.asm.push(movz(X64, reg, (value & bottom) as i64, 0));
        value >>= 16;
        if value != 0 {
            for shift in 1..4 {
                self.asm.push(movk(X64, reg, (value & bottom) as i64, shift));
                value >>= 16;
                if value == 0 {
                    break
                }
            }
        }
    }

    fn dyn_c_call(&mut self, f_addr_reg: i64, arg: StackRange, ret: StackRange, _: FnType) {
        assert!(
            arg.count <= 7,
            "indirect c_call only supports 7 arguments. TODO: pass on stack"
        );
        for i in 0..arg.count {
            if self.slot_types.unwrap()[arg.first.0 + i].is_unit() {
                continue
            }
            self.get_slot(i as i64, StackOffset(arg.first.0 + i));
        }
        // The symptom of not resetting sp is you get a stack overflow which is odd.
        // maybe it keeps calling me in a loop because rust loses where it put its link register.
        self.asm.push(br(f_addr_reg, 1));
        // TODO: you cant call release many because it assumes they were made in one chunk
        for i in arg {
            self.release_one(StackOffset(i));
        }
        assert_eq!(ret.count, 1);
        self.set_slot(x0, ret.single());
    }
}

#[allow(unused)]
#[cfg(target_arch = "aarch64")]
mod tests {
    use super::{BcToAsm, Jitted};
    use crate::{ast::{garbage_loc, Program}, compiler::{Compile, ExecTime, Res}, interp::Interp, LIB, logging::{err, ice, unwrap}, make_toplevel, parse::Parser, pool::StringPool, scope::ResolveScope};
    use codemap::CodeMap;
    use std::{arch::asm, fs, mem::transmute};
    use std::process::Command;
    use std::ptr::addr_of;
    use crate::ast::SuperSimple;
    use crate::export_ffi::get_special_functions;

    fn jit_main<Arg, Ret>(test_name: &str, src: &str, f: impl FnOnce(extern "C" fn(Arg) -> Ret)) -> Res<'static, ()> {
        let pool = Box::leak(Box::<StringPool>::default());
        let mut codemap = CodeMap::new();
        let mut stmts = vec![];
        let mut libs: Vec<_> = LIB
            .iter()
            .map(|(name, code)| codemap.add_file(name.to_string(), code.to_string()))
            .collect();
        libs.insert(3, codemap.add_file("special".into(), get_special_functions()));  // TODO: order independent name resolution
        libs.push(codemap.add_file("main_file".into(), src.to_string()));
        for l in libs {
            stmts.extend(Parser::parse(l, pool).unwrap());
        }
        let mut global = make_toplevel(pool, garbage_loc(), stmts);
        let vars = ResolveScope::of(&mut global, pool);
        let mut program = Program::new(vars, pool);
        let mut comp = Compile::new(pool, &mut program, Interp::new(pool));
        comp.add_declarations(global)?;
        let name = pool.intern("main");
        let main = unwrap!(comp.lookup_unique_func(name), "");
        comp.compile(main, ExecTime::Runtime)?;

        let mut asm = BcToAsm::new(&comp.executor, &mut program);
        asm.asm.reserve(asm.program.funcs.len());
        asm.compile(main)?;

        if cfg!(feature = "llvm_dis_debug") {
            let hex: String = asm.asm.bytes()
                .iter()
                .copied()
                .array_chunks::<4>()
                .map(|b| format!("{:#02x} {:#02x} {:#02x} {:#02x} ", b[0], b[1], b[2], b[3]))
                .collect();
            let path = format!("target/latest_log/asm/{test_name}.asm");
            fs::write(&path, hex).unwrap();
            let dis = String::from_utf8(Command::new("llvm-mc").arg("--disassemble").arg(&path).output().unwrap().stdout).unwrap();
            fs::write(&path, dis);
        }

        asm.asm.make_exec();
        let code = asm.asm.get_fn(main).unwrap().as_ptr();
        let code: extern "C" fn(Arg) -> Ret = unsafe { transmute(code) };
        let indirect_fns = asm.asm.get_dispatch();
        unsafe {
            asm!(
                "mov x21, {fns}",
                fns = in(reg) indirect_fns,
                // I'm hoping this is how I declare that I intend to clobber the register.
                // https://doc.rust-lang.org/reference/inline-assembly.html
                // "[...] the contents of the register to be discarded at the end of the asm code"
                // I imagine that means they just don't put it anywhere, not that they zero it for spite reasons.
                out("x21") _
            );
        }
        f(code);
        Ok(())
    }

    macro_rules! simple {
        ($name:ident, $arg:expr, $ret:expr, $src:expr) => {
            #[test]
            fn $name () {
                jit_main(stringify!($name), $src, |f| {
                    let ret: i64 = f($arg);
                    assert_eq!(ret, $ret);
                })
                .unwrap();
            }
        };
    }

    simple!(trivial, (), 42, "@c_call fn main() i64 = { 42 }");
    simple!(trivial_indirect, (), 42, "@c_call fn get_42() i64 = { 42 } @c_call fn main() i64 = { get_42() }");
    simple!(simple_ifa, true, 123, "@c_call fn main(a: bool) i64 = { (a, fn()=123, fn=456)!if }");
    simple!(simple_ifb, false, 456, "@c_call fn main(a: bool) i64 = { (a, fn()=123, fn=456)!if }");
    simple!(math, 5, 20, "@c_call fn main(a: i64) i64 = { add(a, 15) }");
    simple!(simple_while, 4, 10, r#"
        @c_call fn main(n: i64) i64 = {
            var a = 0;
            (fn()=ne(n, 0), fn()={
                a = add(a, n);
                n = sub(n, 1);
            })!while;
            a
        }"#
    );
    simple!(var_addr, 3, 10, r#"
        @c_call fn main(n: i64) i64 = {
            var a = n;
            var b = 0;
            let a_ptr = a!addr;
            let b_ptr = b!addr;
            b_ptr[] = add(a_ptr[], 7);
            b
        }"#
    );
    simple!(fields, 3, 10, r#"
        @c_call fn main(n: i64) i64 = {
            const A = .{ a: i64, b: i64 }!struct;
            var a: A = .{ a: n, b: 0 };
            a.b[] = add(a.a[], 7);
            a.b[]
        }"#
    );
    simple!(varient, 3, 10, r#"
        @c_call fn main(n: i64) i64 = {
            const A = .{ a: i64, b: i64 }!enum;
            var a: A = .{ a: n };
            add(a.a[], 7)
        }"#
    );
    simple!(nested, (), 91, r#"
        @c_call fn main() i64 = {
            add(add(add(add(add(add(add(add(add(1, 2), 3), 4), add(5, 6)), 7), 8), add(9, add(10, 11))), 12), 13)
        }"#
    );

    #[test]
    fn use_ptr() {
        jit_main("use_ptr",
            r#"@c_call fn main(a: Ptr(i64), b: Ptr(i64)) i64 = {
                   b[] = add(a[], 1);
                   add(b[], 4)
                }"#,
            |f| {
                let mut a = 5;
                let mut b = 0;
                let ret: i64 = f((addr_of!(a), addr_of!(b)));
                assert_eq!(ret, 10);
                assert_eq!(a, 5);
                assert_eq!(b, 6);
            },
        )
            .unwrap();
    }

    #[test]
    fn ffi_ptr() {
        jit_main("ffi_ptr",
            r#"@c_call fn main(a: Ptr(SuperSimple)) i64 = {
                   let c = sub(a.b[], a.a[]);
                   a.a[] = 1;
                   a.b[] = 2;
                   c
                }"#,
            |f| {
                let mut a = SuperSimple {
                    a: 57,
                    b: 77,
                };
                let ret: i64 = f((addr_of!(a)));
                assert_eq!(ret, 20);
                assert_eq!(a.a, 1);
                assert_eq!(a.b, 2);
            },
        )
            .unwrap();
    }
}

#[cfg(target_arch = "aarch64")]
pub use jit::Jitted;
use crate::experiments::aarch64::{CmpFlags, signed_truncate};

#[cfg(target_arch = "aarch64")]
pub mod jit {
    use std::ptr::null;
    use std::slice;
    use crate::ast::FuncId;
    use crate::experiments::bootstrap_gen::brk;

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
    }

    // TODO: https://community.arm.com/arm-community-blogs/b/architectures-and-processors-blog/posts/caches-and-self-modifying-code
    impl Jitted {
        pub fn new(bytes: usize) -> Self {
            let mut map = memmap2::MmapOptions::new()
                .len(bytes)
                .map_anon()
                .unwrap();
            assert_eq!(map.as_ptr() as usize % 4, 0, "alignment's fucked");
            Self {
                current_start: map.as_ptr(),
                next: map.as_mut_ptr(),
                map_mut: Some(map),
                map_exec: None,
                dispatch: vec![],
                ranges: vec![],
                ip_to_inst: vec![],
            }
        }

        pub fn bytes(&self) -> &[u8] {
            let map = self.map_mut.as_ref().unwrap();
            let len = unsafe {self.next.offset_from(map.as_ptr())} as usize;
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
            unsafe {
                (self.next as *const u32).offset_from(self.current_start as *const u32) as usize - 1
            }
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
    }
}
