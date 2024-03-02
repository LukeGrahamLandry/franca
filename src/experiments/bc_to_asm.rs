//! TODO: i know this is remarkably bad codegen.
//! @c_call means https://en.wikipedia.org/wiki/Calling_convention#ARM_(A64)

use std::mem;
use std::ptr::null;

use crate::ast::{FuncId, TypeId};
use crate::bc::{Bc, Value};
use crate::compiler::Res;
use crate::experiments::bootstrap_gen::*;
use crate::interp::Interp;
use crate::logging::{err, PoolLog};
use crate::{ast::Program, bc::FnBody};

use super::builtins;

struct BcToAsm<'z, 'a, 'p> {
    program: &'z Program<'p>,
    interp: &'z Interp<'a, 'p>,
    ready: Vec<*const u8>,
    mmaps: Vec<Option<Box<memmap2::Mmap>>>,
    asm: Vec<i64>,
}

const x0: i64 = 0;
const x1: i64 = 1;
const x2: i64 = 2;
const x3: i64 = 3;
const x4: i64 = 4;
const x5: i64 = 5;
const x6: i64 = 6;
const x7: i64 = 7;
const x8: i64 = 8;
const x9: i64 = 9;
const x21: i64 = 21;
const fp: i64 = 29;
const lr: i64 = 30;
const sp: i64 = 31;
const W32: i64 = 0b0;
const X64: i64 = 0b1;

impl<'z, 'a, 'p> BcToAsm<'z, 'a, 'p> {
    // TODO: i cant keep copy pasting this shape. gonna cry.
    pub fn compile(&mut self, f: FuncId) -> Res<'p, ()> {
        while self.ready.len() <= f.0 {
            self.ready.push(null());
            self.mmaps.push(None);
        }
        if !self.ready[f.0].is_null() {
            return Ok(());
        }

        let callees = self.program.funcs[f.0]
            .wip
            .as_ref()
            .unwrap()
            .callees
            .clone();
        for c in callees {
            self.compile(c)?;
        }
        self.bc_to_asm(self.interp.ready[f.0].as_ref().unwrap())?;
        let ops: Vec<u32> = mem::take(&mut self.asm)
            .into_iter()
            .map(|op| op as u32)
            .collect();
        let map = builtins::copy_to_mmap_exec(ops.into());
        self.ready[f.0] = map.1;
        self.mmaps[f.0] = Some(map.0.to_box());
        Ok(())
    }

    fn bc_to_asm(&mut self, func: &FnBody<'p>) -> Res<'p, ()> {
        println!("{}", func.log(self.program.pool));
        let ff = &self.program.funcs[func.func.0];
        let is_c_call = ff.has_tag(self.program.pool, "c_call");
        let slots = if func.stack_slots % 2 == 0 {
            func.stack_slots + 2
        } else {
            func.stack_slots + 1 + 2
        };
        assert!(slots < 63, "range for stp/ldp");
        self.asm.push(sub_im(X64, sp, sp, (slots * 8) as i64, 0));
        self.asm.push(stp_so(X64, fp, lr, sp, (slots - 2) as i64));  // save our return address
        if is_c_call {
            assert!(
                func.arg_range.count <= 8,
                "c_call only supports 8 arguments. TODO: pass on stack"
            );
            assert_eq!(func.arg_range.first.0, 0);
            for i in 0..func.arg_range.count {
                self.asm.push(str_uo(X64, i as i64, sp, i as i64));
            }
        }
        for (i, inst) in func.insts.iter().enumerate() {
            match inst {
                Bc::CallDynamic { .. } => todo!(),
                Bc::CallDirect { f, ret, arg } => {
                    let target = &self.program.funcs[f.0];
                    let target_c_call = target.has_tag(self.program.pool, "c_call");
                    if target_c_call {
                        assert!(
                            arg.count <= 7,
                            "indirect c_call only supports 7 arguments. TODO: pass on stack"
                        );
                        for i in 0..arg.count {
                            self.asm
                                .push(ldr_uo(X64, i as i64, sp, (arg.first.0 + i) as i64));
                        }
                        assert!(f.0 < 512);
                        self.asm.push(ldr_uo(X64, x7, x21, f.0 as i64));
                        // this symptom of not resetting sp is you get a stack overflow which is odd.
                        // maybe it keeps calling me in a loop because rust loses where it put its link register.
                        self.asm.push(br(x7, 1));
                        assert_eq!(ret.count, 1);
                        self.asm.push(str_uo(X64, x0, sp, ret.first.0 as i64));
                    } else {
                        todo!()
                    }
                }
                Bc::CallBuiltin { .. } => todo!(),
                Bc::LoadConstant { slot, value } => match value {
                    Value::F64(_) => todo!(),
                    Value::I64(n) => {
                        self.asm.push(movz(X64, x0, *n, 0));
                        self.asm.push(str_uo(X64, x0, sp, slot.0 as i64));
                    }
                    // These only make sense during comptime execution but they're also really just numbers.
                    Value::OverloadSet(i)
                    | Value::GetFn(FuncId(i))
                    | Value::Type(TypeId(i))
                    | Value::Symbol(i) => {
                        self.asm.push(movz(X64, x0, *i as i64, 0));
                        self.asm.push(str_uo(X64, x0, sp, slot.0 as i64));
                    }
                    Value::Bool(_) => todo!(),
                    Value::Unit => {}
                    Value::Poison => todo!(),
                    Value::InterpAbsStackAddr(_) => todo!(),
                    Value::Heap { .. } => todo!(),
                    Value::CFnPtr { .. } => todo!(),
                },
                Bc::JumpIf { .. } => todo!(),
                Bc::Goto { .. } => todo!(),
                Bc::Ret(slot) => {
                    if is_c_call {
                        match slot.count {
                            0 => {}
                            1 => {
                                self.asm.push(ldr_uo(X64, x0, sp, slot.first.0 as i64));
                            }
                            _ => err!("c_call only supports one return value. TODO: structs",),
                        }
                        self.asm.push(ldp_so(X64, fp, lr, sp, (slots - 2) as i64));  // get our return address
                        self.asm.push(add_im(X64, sp, sp, (slots * 8) as i64, 0));
                        self.asm.push(ret(()));
                    } else {
                        todo!()
                    }
                }
                Bc::AbsoluteStackAddr { .. } => todo!(),
                Bc::Drop(_) | Bc::DebugMarker(_, _) | Bc::DebugLine(_) => {}
                Bc::Clone { from, to } | Bc::Move { from, to } => {
                    self.asm.push(ldr_uo(X64, x0, sp, (from.0 + i) as i64));
                    self.asm.push(str_uo(X64, x0, sp, (to.0 + i) as i64));
                }
                Bc::CloneRange { from, to } | Bc::MoveRange { from, to } => {
                    for i in 0..from.count {
                        self.asm
                            .push(ldr_uo(X64, x0, sp, (from.first.0 + i) as i64));
                        self.asm.push(str_uo(X64, x0, sp, (to.first.0 + i) as i64));
                    }
                }
                Bc::SlicePtr { .. } => todo!(),
                Bc::Load { .. } => todo!(),
                Bc::TagCheck { .. } => todo!(),
                Bc::Store { .. } => todo!(),
                Bc::CallC { .. } => todo!(),
            }
        }
        Ok(())
    }
}

#[allow(unused)]
#[cfg(target_arch = "aarch64")]
mod tests {
    use super::{BcToAsm, Value};
    use crate::{
        ast::{garbage_loc, FatStmt, Program},
        compiler::{Compile, ExecTime, Res},
        interp::Interp,
        logging::{err, ice, unwrap},
        make_toplevel,
        parse::Parser,
        pool::StringPool,
        scope::ResolveScope,
    };
    use codemap::CodeMap;
    use std::{arch::asm, mem::transmute};

    fn jit_main<Arg, Ret>(src: &str, f: impl FnOnce(extern "C" fn(Arg) -> Ret)) -> Res<'_, ()> {
        let pool = Box::leak(Box::<StringPool>::default());
        let mut codemap = CodeMap::new();
        let file = codemap.add_file("main_file".into(), src.to_string());
        let stmts = Parser::parse(file.clone(), pool).unwrap();
        let mut global = make_toplevel(pool, garbage_loc(), stmts);
        let vars = ResolveScope::of(&mut global, pool);
        let mut program = Program::new(vars, pool);
        let mut comp = Compile::new(pool, &mut program, Interp::new(pool));
        comp.add_declarations(global)?;
        let name = pool.intern("main");
        let main = unwrap!(comp.lookup_unique_func(name), "");
        comp.compile(main, ExecTime::Runtime)?;

        let mut asm = BcToAsm {
            interp: &comp.executor,
            program: &mut program,
            ready: vec![],
            asm: vec![],
            mmaps: vec![],
        };
        asm.compile(main)?;

        let code = asm.ready[main.0];
        assert!(!code.is_null());
        let code: extern "C" fn(Arg) -> Ret = unsafe { transmute(code) };
        let indirect_fns = asm.ready.as_ptr();
        // TODO!!!!!! dont just clobber shit
        unsafe {
            asm!(
                "mov x21, {fns}",
                fns = in(reg) indirect_fns
            );
        }
        f(code);
        Ok(())
    }

    #[test]
    fn trivial() {
        jit_main("const i64; @c_call fn main() i64 = { 42 }", |f| {
            let ret: i64 = f(());
            assert_eq!(ret, 42);
        })
        .unwrap();
    }

    #[test]
    fn trivial_indirect() {
        jit_main(
            "const i64; @c_call fn get_42() i64 = { 42 } @c_call fn main() i64 = { get_42() }",
            |f| {
                let ret: i64 = f(());
                assert_eq!(ret, 42);
            },
        )
        .unwrap();
    }
}
