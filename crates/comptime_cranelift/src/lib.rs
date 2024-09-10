//! TODO:
//! - tail calls
//! - backtrace needs to deal with pointer authentication
//! - soemthing makes it jump into garbage when you run too many tests
//! - signed math on smaller int types needs work because i always zero extend everything.
//! - cranelift is broken https://github.com/bytecodealliance/wasmtime/issues/8852
//!   "I just noticed that the assertion failure is unrelated to StructArgument. [...] I'm not sure when this assertion fires though."
//!

mod bc;
mod ds;
use std::{
    collections::HashMap,
    ptr::{slice_from_raw_parts, slice_from_raw_parts_mut},
};

use bc::{BackendImportVTable, Bc, BigOption, FnBody, FuncId, Prim, PrimSig, SelfHosted};
use codegen::control::ControlPlane;
use cranelift::{
    codegen::{
        ir::{
            types::{F64, I32, I64, I8},
            ArgumentExtension, ArgumentPurpose, StackSlot,
        },
        settings::Flags,
    },
    prelude::*,
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use ds::{extend_options, pops, BitSet};
use types::{F32, I16};

#[no_mangle]
pub extern "C" fn franca_comptime_cranelift_init(
    data: SelfHosted,
    vtable: *const BackendImportVTable,
    dispatch_ptr: i64,
) -> Box<JittedCl<JITModule>> {
    let isa = cranelift_native::builder().unwrap();
    let mut flags = settings::builder();
    flags.set("preserve_frame_pointers", "true").unwrap();
    flags.set("unwind_info", "false").unwrap();
    flags.set("enable_pinned_reg", "true").unwrap();

    // TODO: want to let comptime control settings...
    // flags.set("opt_level", "speed").unwrap();
    let isa = isa.finish(Flags::new(flags));

    let calls = cranelift_module::default_libcall_names();
    let mut builder = JITBuilder::with_isa(isa.unwrap(), calls);
    builder.symbol_lookup_fn(Box::new(|name| match name {
        "memcpy" => Some(my_memcpy as *const u8),
        _ => None,
    }));
    let module = JITModule::new(builder);
    Box::new(JittedCl {
        module,
        funcs: vec![],
        pending: vec![],
        data,
        vtable,
        dispatch_ptr,
    })
}

#[no_mangle]
pub extern "C" fn franca_comptime_cranelift_emit(
    cl: &mut JittedCl<JITModule>,
    f: FuncId,
    body: &'static FnBody,
    compile_ctx_ptr: i64,
    log_ir: bool,
    log_asm: bool,
) {
    assert!(!cl.pending.contains(&f));
    cl.pending.push(f);
    let ctx = cl.module.make_context();
    let mut e = Emit {
        body,
        blocks: vec![],
        stack: vec![],
        vars: vec![],
        f,
        block_done: BitSet::empty(),
        ssa_vars: Default::default(),
        compile_ctx_ptr,
        cl,
    };
    e.emit_func(f, FunctionBuilderContext::new(), ctx, log_ir, log_asm);
    // cl.module.finalize_definitions().unwrap();
    // let ff = cl.funcs[f.as_index()].unwrap();
    // let addr = cl.module.get_finalized_function(ff);
    // unsafe { ((*cl.vtable).put_jitted_function)(cl.data, f, addr as usize) };
}

#[no_mangle]
pub extern "C" fn franca_comptime_cranelift_flush(cl: &mut JittedCl<JITModule>) {
    if cl.pending.is_empty() {
        return;
    }
    cl.module.finalize_definitions().unwrap();
    for f in cl.pending.drain(0..) {
        let ff = cl.funcs[f.as_index()].unwrap();
        let addr = cl.module.get_finalized_function(ff);
        unsafe { ((*cl.vtable).put_jitted_function)(cl.data, f, addr as usize) };
    }
}

pub struct JittedCl<M: Module> {
    pub module: M,
    funcs: Vec<Option<cranelift_module::FuncId>>,
    // can't just use funcs[i].is_some because it might just be a forward declaration.
    pending: Vec<FuncId>,
    data: SelfHosted,
    vtable: *const BackendImportVTable,
    dispatch_ptr: i64,
}

fn my_memcpy(dest: *mut u8, src: *const u8, len: usize) {
    unsafe {
        let dest = &mut *slice_from_raw_parts_mut(dest, len);
        let src = &*slice_from_raw_parts(src, len);
        dest.copy_from_slice(src)
    }
}

struct Emit<'z, M: Module> {
    cl: &'z mut JittedCl<M>,
    body: &'static FnBody,
    blocks: Vec<Block>,
    block_done: BitSet,
    stack: Vec<Value>,
    ssa_vars: HashMap<u16, Value>,
    vars: Vec<StackSlot>,
    compile_ctx_ptr: i64,
    f: FuncId,
}

impl<'z, M: Module> Emit<'z, M> {
    fn emit_func(
        &mut self,
        f: FuncId,
        mut builder_ctx: FunctionBuilderContext,
        mut ctx: codegen::Context,
        log_ir: bool,
        log_asm: bool,
    ) {
        self.f = f;
        ctx.func.signature = self.make_sig(self.body.signeture);

        let (name, linkage) = (format!("FN{}", f.as_index()), Linkage::Export);
        let id = self
            .cl
            .module
            .declare_function(&name, linkage, &ctx.func.signature)
            .unwrap();
        extend_options(&mut self.cl.funcs, f.as_index());
        self.cl.funcs[f.as_index()] = Some(id);

        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);
        self.emit_body(&mut builder);
        if log_ir {
            println!("=== Cranelift IR for {} ===", f.as_index());
            println!("{}", builder.func.display());
            println!("===");
        }
        builder.finalize();
        self.cl.module.define_function(id, &mut ctx).unwrap();

        if log_asm {
            ctx.want_disasm = true;
            let code = ctx
                .compile_stencil(self.cl.module.isa(), &mut ControlPlane::default())
                .unwrap();
            println!("=== asm for {} ===", f.as_index());
            println!("{}", code.vcode.unwrap());
        }

        self.cl.module.clear_context(&mut ctx);
    }

    fn emit_body(&mut self, builder: &mut FunctionBuilder) {
        self.vars.clear();
        for ty in &self.body.vars {
            let v = builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                ty.size as u32,
                8,
            )); // TODO: is last arg align?
            self.vars.push(v);
        }

        self.blocks.clear();
        self.block_done.clear();
        self.ssa_vars.clear();

        // println!("{} {:?}", self.program.pool.get(self.program[self.f].name), self.body.signeture);
        for (i, block) in self.body.blocks.iter().enumerate() {
            self.blocks.push(builder.create_block());
            if i != 0 {
                for p in block.arg_prims {
                    let cl_ty = primitive(*p);
                    builder.append_block_param(*self.blocks.last().unwrap(), cl_ty);
                }
            }
        }
        builder.append_block_params_for_function_params(self.blocks[0]);

        self.emit_block(0, builder);

        builder.seal_all_blocks();
    }

    fn emit_block(&mut self, b: usize, builder: &mut FunctionBuilder) {
        if self.block_done.get(b) {
            return;
        }
        self.block_done.set(b);

        let block = &self.body.blocks[b];
        builder.switch_to_block(self.blocks[b]);
        let args = builder.block_params(self.blocks[b]);
        for i in 0..block.arg_prims.len() {
            let v = args[i];
            self.stack.push(v)
        }
        if b == 0 {
            let sig = self.body.signeture;
            debug_assert_eq!(sig.arg_slots(), block.arg_prims.len());
        }

        for inst in &block.insts {
            match *inst {
                Bc::Nop => {}
                Bc::SaveSsa { id, .. } => {
                    let prev = self.ssa_vars.insert(id, self.stack.pop().unwrap());
                    debug_assert!(prev.is_none());
                }
                Bc::LoadSsa { id } => self.stack.push(*self.ssa_vars.get(&id).unwrap()),
                Bc::GetCompCtx => {
                    let v = builder.ins().iconst(I64, self.compile_ctx_ptr);
                    self.stack.push(v);
                }
                Bc::CallDirect { f, tail, sig } => {
                    let sig = self.body.sig_payloads[sig as usize];
                    let args = &self.stack[self.stack.len() - sig.arg_slots()..self.stack.len()];

                    // I want to allow mixing backends so you could ask for better optimisation for specific comptime functions
                    // without slowing compilation for all of them. So when trying to call something, check if my asm backend has it.
                    // Note: check cl funcs first because ptrs go in the asm dispatch table anyway.
                    // TODO: actually forward declare if none to make mutual recursion work.
                    let call = if let Some(&Some(func_id)) = self.cl.funcs.get(f.as_index()) {
                        let callee = self.cl.module.declare_func_in_func(func_id, builder.func);
                        if tail {
                            // TODO: have to deal with floats.
                            // builder.ins().return_call(callee, args);
                            // TODO: cranelift wants to do real tailcalls which changes the abi.
                            let call = builder.ins().call(callee, args);
                            if sig.no_return {
                                builder.ins().trap(TrapCode::UnreachableCodeReached);
                                break;
                            }
                            let ret = builder.inst_results(call).to_vec();
                            builder.ins().return_(&ret);
                            break;
                        }
                        builder.ins().call(callee, args)
                    } else {
                        let addr =
                            unsafe { ((*self.cl.vtable).get_jitted_function)(self.cl.data, f) };
                        let callee = match addr {
                            BigOption::Some(addr) => builder.ins().iconst(I64, addr as i64),
                            BigOption::None => {
                                // TODO: HACK: this is really stupid. this is how my asm does it but cl has relocation stuff so just have to forward declare the funcid.
                                //       its just annoying because then i need to make sure i know if im supposed to be declaring
                                //       or jsut waiting on it to be done by a different backend if i want to allow you mixing them    -- May 16
                                let dispatch = builder.ins().iconst(I64, self.cl.dispatch_ptr);
                                builder.ins().load(
                                    I64,
                                    MemFlags::new(),
                                    dispatch,
                                    f.as_index() as i32 * 8,
                                )
                            }
                        };
                        let cl_sig = self.make_sig(sig);
                        let sl_sig = builder.import_signature(cl_sig);
                        let args =
                            &self.stack[self.stack.len() - sig.arg_slots()..self.stack.len()];

                        let call = builder.ins().call_indirect(sl_sig, callee, args);
                        if tail {
                            if sig.no_return {
                                builder.ins().trap(TrapCode::UnreachableCodeReached);
                                break;
                            }
                            // I guess we don't trust rustc to agree with cranelift about what a tail call means?
                            // and they want to be fancier than me about skipping adjusting sp?
                            // TODO: track whether this was a required tail call or just a friendly one and maybe give an error?
                            let ret = builder.inst_results(call).to_vec();
                            builder.ins().return_(&ret);
                            break;
                        }
                        call
                    };

                    let ret_val = builder.inst_results(call);
                    pops(&mut self.stack, sig.arg_slots());
                    for v in ret_val {
                        self.stack.push(*v);
                    }
                }
                Bc::CallFnPtr { sig, .. } => {
                    let sig = self.body.sig_payloads[sig as usize];
                    let cl_sig = builder.import_signature(self.make_sig(sig));
                    let first_arg = self.stack.len() - sig.arg_slots();
                    let callee = self.stack[first_arg - 1];
                    let args = &self.stack[first_arg..self.stack.len()];
                    let call = builder.ins().call_indirect(cl_sig, callee, args);
                    pops(&mut self.stack, sig.arg_slots() + 1);
                    let ret_val = builder.inst_results(call);
                    if sig.no_return {
                        builder.ins().trap(TrapCode::UnreachableCodeReached);
                        break;
                    }
                    for v in ret_val {
                        self.stack.push(*v);
                    }
                }
                Bc::PushConstant { value, ty } => match ty {
                    Prim::P64 | Prim::I8 | Prim::I16 | Prim::I32 | Prim::I64 => {
                        self.stack.push(builder.ins().iconst(primitive(ty), value))
                    }
                    Prim::F64 => {
                        let v = builder.ins().iconst(I64, value);
                        self.stack
                            .push(builder.ins().bitcast(F64, MemFlags::new(), v));
                    }
                    Prim::F32 => {
                        let v = builder.ins().iconst(I32, value);
                        self.stack
                            .push(builder.ins().bitcast(F32, MemFlags::new(), v));
                    }
                },
                Bc::PushGlobalAddr { id } => {
                    let value = unsafe { ((*self.cl.vtable).get_jit_addr)(self.cl.data, id) };
                    self.stack.push(builder.ins().iconst(I64, value as i64))
                }
                Bc::JumpIf {
                    true_ip,
                    false_ip,
                    slots,
                } => {
                    debug_assert_eq!(
                        slots, 0,
                        "this is probably fine. but not tested cause emit_bc never does it"
                    );
                    let cond = self.stack.pop().unwrap();
                    let t = self.blocks[true_ip.0 as usize];
                    let f = self.blocks[false_ip.0 as usize];
                    let args = &self.stack[self.stack.len() - slots as usize..self.stack.len()];
                    builder.ins().brif(cond, t, args, f, args);
                    pops(&mut self.stack, slots as usize);
                    let stack = self.stack.clone();
                    self.emit_block(true_ip.0 as usize, builder);
                    self.stack = stack;
                    self.emit_block(false_ip.0 as usize, builder);
                    break;
                }
                Bc::Goto { ip, slots } => {
                    let b = self.blocks[ip.0 as usize];
                    let args = &self.stack[self.stack.len() - slots as usize..self.stack.len()];
                    builder.ins().jump(b, args);
                    pops(&mut self.stack, slots as usize);
                    self.emit_block(ip.0 as usize, builder);
                    break;
                }
                Bc::Ret0 => {
                    builder.ins().return_(&[]);
                    break;
                }
                Bc::Ret1(_) => {
                    self.emit_return(builder, 1);
                    break;
                }
                Bc::Ret2(_) => {
                    self.emit_return(builder, 2);
                    break;
                }
                Bc::GetNativeFnPtr(f) => {
                    if let Some(&Some(id)) = self.cl.funcs.get(f.as_index()) {
                        let id = self.cl.module.declare_func_in_func(id, builder.func);
                        self.stack.push(builder.ins().func_addr(I64, id));
                    } else {
                        // :copy-paste
                        let addr =
                            unsafe { ((*self.cl.vtable).get_jitted_function)(self.cl.data, f) };
                        match addr {
                            BigOption::Some(addr) => {
                                self.stack.push(builder.ins().iconst(I64, addr as i64));
                            }
                            BigOption::None => {
                                let dispatch = builder.ins().iconst(I64, self.cl.dispatch_ptr);
                                let addr = builder.ins().load(
                                    I64,
                                    MemFlags::new(),
                                    dispatch,
                                    f.as_index() as i32 * 8,
                                );
                                self.stack.push(addr);
                            }
                        }
                    }
                }
                Bc::Load { ty } => {
                    let addr = self.stack.pop().unwrap();
                    let v = builder.ins().load(primitive(ty), MemFlags::new(), addr, 0);
                    self.stack.push(v);
                }
                Bc::StorePost { .. } => {
                    let addr = self.stack.pop().unwrap();
                    let v = self.stack.pop().unwrap();
                    builder.ins().store(MemFlags::new(), v, addr, 0);
                }
                Bc::StorePre { .. } => {
                    let v = self.stack.pop().unwrap();
                    let addr = self.stack.pop().unwrap();
                    builder.ins().store(MemFlags::new(), v, addr, 0);
                }
                Bc::AddrVar { id } => {
                    if let Some(&ptr) = self.ssa_vars.get(&id) {
                        self.stack.push(ptr);
                    } else {
                        let slot = self.vars[id as usize];
                        self.stack.push(builder.ins().stack_addr(I64, slot, 0));
                    }
                }
                Bc::IncPtrBytes { bytes } => {
                    let ptr = self.stack.pop().unwrap();
                    let offset = builder.ins().iconst(I64, bytes as i64);
                    let ptr = builder.ins().bitcast(I64, MemFlags::new(), ptr);
                    let res = builder.ins().iadd(ptr, offset);
                    let res = builder.ins().bitcast(I64, MemFlags::new(), res);
                    self.stack.push(res);
                }

                Bc::Unreachable => {
                    builder.ins().trap(TrapCode::UnreachableCodeReached);
                    break;
                }
                Bc::NoCompile => panic!("tried to compile a NoCompile block"),
                Bc::LastUse { .. } => {}
                Bc::PeekDup(skip) => {
                    self.stack
                        .push(self.stack[self.stack.len() - skip as usize - 1]);
                }
                Bc::Snipe(skip) => {
                    self.stack.remove(self.stack.len() - skip as usize - 1);
                }
                Bc::CopyBytesToFrom { bytes } => {
                    let from = self.stack.pop().unwrap();
                    let to = self.stack.pop().unwrap();
                    let size = builder.ins().iconst(I64, bytes as i64);
                    let config = self.cl.module.target_config();
                    builder.call_memcpy(config, to, from, size)
                }
                Bc::Intrinsic(op) => {
                    macro_rules! inst2 {
                        ($s:expr, $builder:expr, $name:ident) => {{
                            let rhs = $s.stack.pop().unwrap();
                            let lhs = $s.stack.pop().unwrap();
                            let out = $builder.ins().$name(lhs, rhs);
                            $s.stack.push(out);
                        }};
                    }

                    macro_rules! icmp {
                        ($s:expr, $builder:expr, $name:ident) => {{
                            let rhs = $s.stack.pop().unwrap();
                            let lhs = $s.stack.pop().unwrap();
                            let out = $builder.ins().icmp(IntCC::$name, lhs, rhs);
                            $s.stack.push(out);
                        }};
                    }

                    macro_rules! fcmp {
                        ($s:expr, $builder:expr, $name:ident) => {{
                            let rhs = $s.stack.pop().unwrap();
                            let lhs = $s.stack.pop().unwrap();
                            let out = $builder.ins().fcmp(FloatCC::$name, lhs, rhs);
                            $s.stack.push(out);
                        }};
                    }

                    match op {
                        bc::Intrinsic::Add => inst2!(self, builder, iadd),
                        bc::Intrinsic::Sub => inst2!(self, builder, isub),
                        bc::Intrinsic::Mul => inst2!(self, builder, imul),
                        bc::Intrinsic::Div => inst2!(self, builder, sdiv),
                        bc::Intrinsic::UDiv => inst2!(self, builder, udiv),
                        bc::Intrinsic::FAdd => inst2!(self, builder, fadd),
                        bc::Intrinsic::FSub => inst2!(self, builder, fsub),
                        bc::Intrinsic::FMul => inst2!(self, builder, fmul),
                        bc::Intrinsic::FDiv => inst2!(self, builder, fdiv),
                        bc::Intrinsic::ShiftLeft => inst2!(self, builder, ishl),
                        bc::Intrinsic::ShiftRightLogical => inst2!(self, builder, ushr),
                        bc::Intrinsic::ShiftRightArithmetic => inst2!(self, builder, sshr),
                        bc::Intrinsic::BitAnd => inst2!(self, builder, band),
                        bc::Intrinsic::BitOr => inst2!(self, builder, bor),
                        bc::Intrinsic::BitXor => inst2!(self, builder, bxor),
                        bc::Intrinsic::Eq => icmp!(self, builder, Equal),
                        bc::Intrinsic::Ne => icmp!(self, builder, NotEqual),
                        bc::Intrinsic::Lt => icmp!(self, builder, SignedLessThan),
                        bc::Intrinsic::Gt => icmp!(self, builder, SignedGreaterThan),
                        bc::Intrinsic::Le => icmp!(self, builder, SignedLessThanOrEqual),
                        bc::Intrinsic::Ge => icmp!(self, builder, SignedGreaterThanOrEqual),
                        bc::Intrinsic::ULt => icmp!(self, builder, UnsignedLessThan),
                        bc::Intrinsic::UGt => icmp!(self, builder, UnsignedGreaterThan),
                        bc::Intrinsic::ULe => icmp!(self, builder, UnsignedLessThanOrEqual),
                        bc::Intrinsic::UGe => icmp!(self, builder, UnsignedGreaterThanOrEqual),
                        // TODO: are these ordered or unordered?
                        bc::Intrinsic::FEq => fcmp!(self, builder, Equal),
                        bc::Intrinsic::FNe => fcmp!(self, builder, NotEqual),
                        bc::Intrinsic::FLt => fcmp!(self, builder, LessThan),
                        bc::Intrinsic::FGt => fcmp!(self, builder, GreaterThan),
                        bc::Intrinsic::FLe => fcmp!(self, builder, LessThanOrEqual),
                        bc::Intrinsic::FGe => fcmp!(self, builder, GreaterThanOrEqual),
                        bc::Intrinsic::BitNot => {
                            let v = self.stack.pop().unwrap();
                            self.stack.push(builder.ins().bnot(v))
                        }
                        bc::Intrinsic::IntToFloatValue => {
                            let v = self.stack.pop().unwrap();
                            self.stack.push(builder.ins().fcvt_from_sint(F64, v))
                        }
                        bc::Intrinsic::FloatToIntValue => {
                            let v = self.stack.pop().unwrap();
                            self.stack.push(builder.ins().fcvt_to_sint_sat(I64, v))
                        }
                        bc::Intrinsic::_Unused => unreachable!("unused intrinsic"),
                        bc::Intrinsic::IntToFloatBits => {
                            let v = self.stack.pop().unwrap();
                            self.stack
                                .push(builder.ins().bitcast(F64, MemFlags::new(), v))
                        }
                        bc::Intrinsic::FloatToIntBits => {
                            let v = self.stack.pop().unwrap();
                            self.stack
                                .push(builder.ins().bitcast(I64, MemFlags::new(), v))
                        }
                        bc::Intrinsic::ShrinkFloat => {
                            let v = self.stack.pop().unwrap();
                            self.stack.push(builder.ins().fdemote(F32, v))
                        }
                        bc::Intrinsic::GrowFloat => {
                            let v = self.stack.pop().unwrap();
                            self.stack.push(builder.ins().fpromote(F64, v))
                        }
                        bc::Intrinsic::IntToPtr => (),
                        bc::Intrinsic::PtrToInt => (),
                        bc::Intrinsic::SignExtend32To64 => {
                            let v = self.stack.pop().unwrap();
                            self.stack.push(builder.ins().sextend(I64, v))
                        }
                        bc::Intrinsic::Trunc64To32 => {
                            let v = self.stack.pop().unwrap();
                            self.stack.push(builder.ins().ireduce(I32, v))
                        }
                        bc::Intrinsic::Trunc32To16 | bc::Intrinsic::Trunc64To16 => {
                            let v = self.stack.pop().unwrap();
                            self.stack.push(builder.ins().ireduce(I16, v))
                        }
                        bc::Intrinsic::Trunc64To8
                        | bc::Intrinsic::Trunc32To8
                        | bc::Intrinsic::Trunc16To8 => {
                            let v = self.stack.pop().unwrap();
                            self.stack.push(builder.ins().ireduce(I8, v))
                        }
                        bc::Intrinsic::ZeroExtend32To64
                        | bc::Intrinsic::ZeroExtend16To64
                        | bc::Intrinsic::ZeroExtend8To64 => {
                            let v = self.stack.pop().unwrap();
                            self.stack.push(builder.ins().uextend(I64, v))
                        }
                        bc::Intrinsic::ZeroExtend16To32 | bc::Intrinsic::ZeroExtend8To32 => {
                            let v = self.stack.pop().unwrap();
                            self.stack.push(builder.ins().uextend(I32, v))
                        }
                        bc::Intrinsic::ZeroExtend8To16 => {
                            let v = self.stack.pop().unwrap();
                            self.stack.push(builder.ins().uextend(I16, v))
                        }
                    }
                }
                Bc::Switch(idx) => {
                    // TODO: thier jump table thing isn't quite what i want because its densly packed starting at zero,
                    //       mine might be non-sequential? but this seems sad

                    let data = &self.body.switch_payloads[idx as usize];
                    let default = data.last().expect("switch to have cases");
                    assert!(default.value == -1, "expected default case");
                    let cases = &data[0..data.len() - 1];

                    let value = self.stack.pop().unwrap();

                    let mut next_block = builder.create_block();
                    for case in cases {
                        let check = builder.ins().iconst(I64, case.value);
                        let cond = builder.ins().icmp(IntCC::Equal, value, check);
                        builder.ins().brif(
                            cond,
                            self.blocks[case.block.0 as usize],
                            &[],
                            next_block,
                            &[],
                        );
                        let stack = self.stack.clone();
                        self.emit_block(case.block.0 as usize, builder);
                        self.stack = stack;
                        builder.switch_to_block(next_block);
                        next_block = builder.create_block();
                    }
                    builder
                        .ins()
                        .jump(self.blocks[default.block.0 as usize], &[]);
                    self.emit_block(default.block.0 as usize, builder);
                    builder.switch_to_block(next_block);
                    builder.ins().trap(TrapCode::UnreachableCodeReached);
                    break;
                }
            }
        }
    }

    fn make_sig(&mut self, sign: PrimSig) -> Signature {
        let mut sig = self.cl.module.make_signature();
        if sign.first_arg_is_indirect_return {
            let value_type = primitive(Prim::P64);
            sig.params.push(AbiParam {
                value_type,
                purpose: ArgumentPurpose::StructReturn,
                extension: ArgumentExtension::None,
            });
        }

        for ty in sign.args {
            let ty = *ty;
            // TODO: sign extend once I use small int types? but really caller should always do it I think.
            sig.params.push(AbiParam {
                value_type: primitive(ty),
                purpose: ArgumentPurpose::Normal,
                extension: ArgumentExtension::None,
            });
        }

        let make_ret = |sig: &mut Signature, ty: Prim| {
            // TODO: sign extend once I use small int types?
            sig.returns.push(AbiParam {
                value_type: primitive(ty),
                purpose: ArgumentPurpose::Normal,
                extension: ArgumentExtension::None,
            });
        };
        if let BigOption::Some(r) = sign.ret1 {
            make_ret(&mut sig, r);
        }
        if let BigOption::Some(r) = sign.ret2 {
            make_ret(&mut sig, r);
        }

        sig
    }

    fn emit_return(&mut self, builder: &mut FunctionBuilder, count: u16) {
        let args = &self.stack[self.stack.len() - count as usize..self.stack.len()];
        builder.ins().return_(args);
        pops(&mut self.stack, count as usize);
    }
}

fn primitive(prim: Prim) -> Type {
    match prim {
        Prim::I8 => I8,
        Prim::I16 => I16,
        Prim::I32 => I32,
        Prim::P64 | Prim::I64 => I64,
        Prim::F64 => F64,
        Prim::F32 => F32,
    }
}
