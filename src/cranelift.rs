//! TODO:
//! - tail calls
//! - backtrace needs to deal with pointer authentication
//! - soemthing makes it jump into garbage when you run too many tests
//! - signed math on smaller int types needs work because i always zero extend everything.

use std::{collections::HashMap, mem};

use crate::export_ffi::BigResult::*;
use cranelift::{
    codegen::{
        control::ControlPlane,
        ir::{
            stackslot::StackSize,
            types::{F64, I32, I64, I8},
            ArgumentExtension, ArgumentPurpose, StackSlot,
        },
        settings::Flags,
        CodegenError,
    },
    prelude::*,
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, DataId, Linkage, Module, ModuleError};
use cranelift_object::{ObjectBuilder, ObjectModule, ObjectProduct};
use types::{F32, I16};

use crate::{
    ast::{CallConv, Flag, FuncId, FuncImpl, Program},
    bc::{is_float, BakedVar, BasicBlock, Bc, FnBody, Prim, PrimSig},
    bc_to_asm::Jitted,
    compiler::{CErr, Compile, CompileError, ExecStyle, Res},
    emit_bc::{emit_bc, empty_fn_body},
    err, extend_options,
    logging::make_err,
    pops, unwrap, BitSet,
};

pub struct JittedCl<M: Module> {
    pub module: M,
    funcs: Vec<Option<cranelift_module::FuncId>>,
    // can't just use funcs[i].is_some because it might just be a forward declaration.
    funcs_done: BitSet,
    pending: Vec<FuncId>,
}

pub fn emit_cl_exe<'p>(comp: &mut Compile<'_, 'p>, f: FuncId) -> Res<'p, ObjectProduct> {
    let isa = cranelift_native::builder().unwrap();
    let mut flags = settings::builder();
    flags.set("preserve_frame_pointers", "true").unwrap();
    flags.set("unwind_info", "false").unwrap();
    // flags.set("enable_pinned_reg", "true").unwrap();

    // TODO: want to let comptime control settings...
    flags.set("opt_level", "speed").unwrap();
    let isa = isa.finish(Flags::new(flags));

    let calls = cranelift_module::default_libcall_names();
    let builder = ObjectBuilder::new(isa.unwrap(), "program", calls).unwrap();

    let module = ObjectModule::new(builder);
    let mut m = JittedCl {
        module,
        funcs: vec![],
        funcs_done: BitSet::empty(),
        pending: vec![],
    };

    fn emit<'p>(comp: &mut Compile<'_, 'p>, m: &mut JittedCl<ObjectModule>, f: FuncId) -> Res<'p, ()> {
        for callee in comp.program[f].callees.clone() {
            emit(comp, m, callee)?;
        }
        // TODO: mutual callees

        if comp.program[f].has_tag(Flag::Ct) {
            return Ok(()); // TODO: why am i trying to call Unique?
        }
        if m.funcs_done.get(f.as_index()) {
            return Ok(());
        }

        println!("do {}", comp.program.pool.get(comp.program[f].name));
        if comp.program[f].body.cranelift_emit().is_some() {
            crate::cranelift::emit_cl_intrinsic(comp.program, m, f)?;
        } else if let FuncImpl::Normal(_) = comp.program[f].body {
            if comp.program[f].cc.unwrap() != CallConv::Inline {
                let body = emit_bc(comp, f, ExecStyle::Aot)?;
                emit_cl_inner(comp.program, m, None, &body, f, 0)?;
            }
        } else if comp.program[f].body.comptime_addr().is_some() {
            let body = emit_bc(comp, f, ExecStyle::Aot)?;
            emit_cl_inner(comp.program, m, None, &body, f, 0)?;
        }
        m.funcs_done.set(f.as_index());

        Ok(())
    }
    emit(comp, &mut m, f)?;

    Ok(m.module.finish())
}

pub fn emit_cl<'p>(compile: &mut Compile<'_, 'p>, body: &FnBody<'p>, f: FuncId) -> Res<'p, ()> {
    let ctx = compile as *mut Compile as usize;
    emit_cl_inner(compile.program, &mut compile.cranelift, Some(&mut compile.aarch64), body, f, ctx)
}

fn emit_cl_inner<'p, M: Module>(
    program: &Program<'p>,
    cl: &mut JittedCl<M>,
    asm: Option<&mut Jitted>,
    body: &FnBody<'p>,
    f: FuncId,
    compile_ctx_ptr: usize,
) -> Res<'p, ()> {
    if cl.funcs_done.get(f.as_index()) {
        return Ok(());
    }
    let ctx = cl.module.make_context();

    let mut e = Emit {
        body,
        blocks: vec![],
        stack: vec![],
        vars: vec![],
        f: FuncId::from_index(0),
        block_done: BitSet::empty(),
        ssa_vars: Default::default(),
        compile_ctx_ptr,
        program,
        asm,
        cl,
    };
    e.emit_func(f, FunctionBuilderContext::new(), ctx)?;
    cl.funcs_done.insert(f.as_index(), true);
    Ok(())
}

pub(crate) fn emit_cl_intrinsic<'p, M: Module>(program: &mut Program<'p>, cl: &mut JittedCl<M>, f: FuncId) -> Res<'p, ()> {
    let mut body = empty_fn_body(program, f, ExecStyle::Aot)?;
    body.func = f;
    let arg = program[f].finished_arg.unwrap();
    let arg_prims = program.as_primatives(arg);
    let arg = program.get_info(arg);
    body.blocks.push(BasicBlock {
        arg_prims,
        insts: vec![Bc::CallDirect {
            f,
            tail: true,
            sig: body.signeture,
        }],
        arg_slots: arg.size_slots,
        arg_float_mask: arg.float_mask,
        incoming_jumps: 0,
        clock: 0,
        height: 0,
    });
    emit_cl_inner(program, cl, None, &body, f, 0)
}

impl Default for JittedCl<JITModule> {
    fn default() -> Self {
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
            "memcpy" => Some(libc::memcpy as *const u8),
            _ => None,
        }));
        let module = JITModule::new(builder);
        JittedCl {
            module,
            funcs: vec![],
            funcs_done: BitSet::empty(),
            pending: vec![],
        }
    }
}

impl JittedCl<JITModule> {
    pub fn flush_pending_defs(&mut self, aarch64: &mut Jitted) -> Res<'static, ()> {
        if self.pending.is_empty() {
            return Ok(());
        }
        self.module.finalize_definitions().unwrap();
        for f in self.pending.drain(0..) {
            aarch64.extend_blanks(f);
            let ff = self.funcs[f.as_index()].unwrap();
            let addr = self.module.get_finalized_function(ff);
            aarch64.dispatch[f.as_index()] = addr;
        }
        Ok(())
    }
}

struct Emit<'z, 'p, M: Module> {
    compile_ctx_ptr: usize, // TODO: HACK. should really pass this through correctly as an argument.
    program: &'z Program<'p>,
    asm: Option<&'z mut Jitted>, // TODO: kinda hack. other side should be able to do too
    cl: &'z mut JittedCl<M>,
    body: &'z FnBody<'p>,
    blocks: Vec<Block>,
    block_done: BitSet,
    stack: Vec<Value>,
    ssa_vars: HashMap<u16, Value>,

    vars: Vec<StackSlot>,
    f: FuncId,
}

impl<'z, 'p, M: Module> Emit<'z, 'p, M> {
    fn emit_func(&mut self, f: FuncId, mut builder_ctx: FunctionBuilderContext, mut ctx: codegen::Context) -> Res<'p, ()> {
        self.f = f;
        ctx.func.signature = self.make_sig(self.body.signeture);

        let is_dynamic = matches!(self.program[f].body, FuncImpl::ComptimeAddr(_));
        let (name, linkage) = if is_dynamic {
            // cranelift-object does the underscore for me
            (self.program.pool.get(self.program[f].name).to_string(), Linkage::Import)
        } else if self.program[f].name == Flag::Main.ident() && self.asm.is_none() {
            // HACK
            // entry point for exe
            (String::from("main"), Linkage::Export)
        } else {
            (format!("FN{}", f.as_index()), Linkage::Export)
        };
        let id = Into::<Res<_>>::into(self.cl.module.declare_function(&name, linkage, &ctx.func.signature).map_err(wrap))?;
        extend_options(&mut self.cl.funcs, f.as_index());
        self.cl.funcs[f.as_index()] = Some(id);

        if is_dynamic {
            return Ok(());
        }
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);
        self.emit_body(&mut builder)?;
        if self.program[f].has_tag(Flag::Log_Ir) {
            println!("=== Cranelift IR for {f:?}: {} ===", self.program.pool.get(self.program[f].name));
            println!("{}", builder.func.display());
            println!("===");
        }
        builder.finalize();
        Into::<Res<_>>::into(self.cl.module.define_function(id, &mut ctx).map_err(wrap))?;

        if self.program[f].has_tag(Flag::Log_Asm) {
            ctx.want_disasm = true;
            let code = Into::<Res<_>>::into(ctx.compile_stencil(self.cl.module.isa(), &mut ControlPlane::default()).map_err(wrapg))?;
            println!("=== asm for {f:?} {} ===", self.program.pool.get(self.program[f].name));
            println!("{}", code.vcode.unwrap());
        }

        self.cl.module.clear_context(&mut ctx);
        self.cl.pending.push(f);

        Ok(())
    }

    fn emit_body(&mut self, builder: &mut FunctionBuilder) -> Res<'p, ()> {
        self.vars.clear();
        for ty in &self.body.vars {
            let slots = self.program.slot_count(*ty);
            let v = builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, (slots * 8) as StackSize));
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
                    let cl_ty = match p {
                        Prim::I8 | Prim::I16 | Prim::I32 | Prim::I64 | Prim::P64 => I64,
                        Prim::F64 => F64,
                        Prim::F32 => F32,
                    };
                    builder.append_block_param(*self.blocks.last().unwrap(), cl_ty);
                }
            }
        }
        builder.append_block_params_for_function_params(self.blocks[0]);

        self.emit_block(0, builder)?;

        for (b, bl) in self.blocks.iter().enumerate() {
            if self.block_done.get(b) {
                builder.seal_block(*bl);
            }
        }
        Ok(())
    }

    fn emit_block(&mut self, b: usize, builder: &mut FunctionBuilder) -> Res<'p, ()> {
        if self.block_done.get(b) {
            return Ok(());
        }
        self.block_done.set(b);

        let block = &self.body.blocks[b];
        builder.switch_to_block(self.blocks[b]);
        let args = builder.block_params(self.blocks[b]);
        debug_assert_eq!(args.len(), block.arg_slots as usize, "b:{b}");
        for i in 0..block.arg_slots {
            let v = args[i as usize];
            self.stack.push(v)
        }
        if b == 0 {
            let sig = self.body.signeture;
            debug_assert_eq!(sig.arg_slots, block.arg_slots);
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
                    let v = builder.ins().iconst(I64, self.compile_ctx_ptr as i64);
                    self.stack.push(v);
                }
                Bc::CallDirect { f, tail, sig } => {
                    let args = &self.stack[self.stack.len() - sig.arg_slots as usize..self.stack.len()];
                    if let Some(&emit) = self.program[f].body.cranelift_emit() {
                        let emit: CfEmit = unsafe { mem::transmute(emit) };
                        let v = emit(builder, args);
                        pops(&mut self.stack, sig.arg_slots as usize);
                        if sig.ret_slots == 0 {
                            // Unit is zero sized.
                            if tail {
                                builder.ins().return_(&[]);
                                break;
                            } else {
                                continue;
                            }
                        } else {
                            self.stack.push(v);
                            debug_assert_eq!(sig.ret_slots, 1);
                            if tail {
                                builder.ins().return_(&[v]);
                                break;
                            } else {
                                continue;
                            }
                        }
                    }

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
                        let callee = self
                            .asm
                            .as_mut()
                            .and_then(|a| a.get_fn(f))
                            .map(|a| a as u64)
                            .map(|addr| builder.ins().iconst(I64, addr as i64))
                            .unwrap_or_else(|| {
                                if let Some(asm) = &mut self.asm {
                                    // TODO: HACK: this is really stupid. this is how my asm does it but cl has relocation stuff so just have to forward declare the funcid.
                                    //       its just annoying because then i need to make sure i know if im supposed to be declaring
                                    //       or jsut waiting on it to be done by a different backend if i want to allow you mixing them    -- May 16
                                    asm.extend_blanks(f);
                                    asm.pending_indirect.push(f);
                                    let dispatch = builder.ins().iconst(I64, asm.get_dispatch() as i64);
                                    builder.ins().load(I64, MemFlags::new(), dispatch, f.as_index() as i32 * 8)
                                } else {
                                    todo!()
                                }
                            });

                        let cl_sig = self.make_sig(sig);
                        let sl_sig = builder.import_signature(cl_sig);
                        let args = &self.stack[self.stack.len() - sig.arg_slots as usize..self.stack.len()];

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
                    pops(&mut self.stack, sig.arg_slots as usize);
                    for v in ret_val {
                        self.stack.push(*v);
                    }
                }
                Bc::CallFnPtr { sig, .. } => {
                    let cl_sig = builder.import_signature(self.make_sig(sig));
                    let first_arg = self.stack.len() - sig.arg_slots as usize;
                    let callee = self.stack[first_arg - 1];
                    let args = &self.stack[first_arg..self.stack.len()];
                    let call = builder.ins().call_indirect(cl_sig, callee, args);
                    pops(&mut self.stack, sig.arg_slots as usize + 1);
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
                    Prim::P64 | Prim::I8 | Prim::I16 | Prim::I32 | Prim::I64 => self.stack.push(builder.ins().iconst(I64, value)),
                    Prim::F64 => {
                        let v = builder.ins().iconst(I64, value);
                        self.stack.push(builder.ins().bitcast(F64, MemFlags::new(), v));
                    }
                    Prim::F32 => {
                        let v = builder.ins().iconst(I32, value);
                        self.stack.push(builder.ins().bitcast(F32, MemFlags::new(), v));
                    }
                },
                Bc::PushGlobalAddr { id } => match self.program.baked.get(id).0 {
                    BakedVar::Bytes(bytes) => {
                        let id: DataId = Into::<Res<_>>::into(self.cl.module.declare_anonymous_data(false, false).map_err(wrap))?;
                        let mut data = DataDescription::new();
                        data.define(bytes.to_owned().into_boxed_slice());
                        Into::<Res<_>>::into(self.cl.module.define_data(id, &data).map_err(wrap))?;
                        let local = self.cl.module.declare_data_in_func(id, builder.func);
                        self.stack.push(builder.ins().global_value(I64, local));
                    }
                    _ => todo!(),
                },
                Bc::JumpIf { true_ip, false_ip, slots } => {
                    debug_assert_eq!(slots, 0, "this is probably fine. but not tested cause emit_bc never does it");
                    let cond = self.stack.pop().unwrap();
                    let t = self.blocks[true_ip.0 as usize];
                    let f = self.blocks[false_ip.0 as usize];
                    let args = &self.stack[self.stack.len() - slots as usize..self.stack.len()];
                    builder.ins().brif(cond, t, args, f, args);
                    pops(&mut self.stack, slots as usize);
                    let stack = self.stack.clone();
                    self.emit_block(true_ip.0 as usize, builder)?;
                    self.stack = stack;
                    self.emit_block(false_ip.0 as usize, builder)?;
                    break;
                }
                Bc::Goto { ip, slots } => {
                    let b = self.blocks[ip.0 as usize];
                    let args = &self.stack[self.stack.len() - slots as usize..self.stack.len()];
                    builder.ins().jump(b, args);
                    pops(&mut self.stack, slots as usize);
                    self.emit_block(ip.0 as usize, builder)?;
                    break;
                }
                Bc::Ret0 => {
                    builder.ins().return_(&[]);
                    break;
                }
                Bc::Ret1(prim) => {
                    self.emit_return(builder, 1);
                    break;
                }
                Bc::Ret2((a, b)) => {
                    self.emit_return(builder, 2);
                    break;
                }
                Bc::GetNativeFnPtr(f) => {
                    if let Some(&Some(id)) = self.cl.funcs.get(f.as_index()) {
                        let id = self.cl.module.declare_func_in_func(id, builder.func);
                        self.stack.push(builder.ins().func_addr(I64, id));
                    } else if let Some(asm) = &mut self.asm {
                        let addr = asm.get_fn(f).map(|a| a as i64);
                        let addr = unwrap!(addr, "fn not ready");
                        self.stack.push(builder.ins().iconst(I64, addr));
                    } else {
                        todo!()
                    }
                }
                Bc::Load { ty } => {
                    let addr = self.stack.pop().unwrap();
                    let v = builder.ins().load(primitive(ty), MemFlags::new(), addr, 0);
                    let v = match ty {
                        Prim::I8 | Prim::I16 | Prim::I32 => builder.ins().uextend(I64, v),
                        Prim::P64 | Prim::I64 => v,
                        Prim::F64 | Prim::F32 => v,
                    };
                    self.stack.push(v);
                }
                Bc::StorePost { ty } => {
                    let addr = self.stack.pop().unwrap();
                    let v = self.stack.pop().unwrap();
                    let v = match ty {
                        Prim::I8 | Prim::I16 | Prim::I32 => builder.ins().ireduce(primitive(ty), v),
                        Prim::F32 | Prim::P64 | Prim::I64 | Prim::F64 => v,
                    };
                    builder.ins().store(MemFlags::new(), v, addr, 0);
                }
                Bc::StorePre { ty } => {
                    let v = self.stack.pop().unwrap();
                    let v = match ty {
                        Prim::I8 | Prim::I16 | Prim::I32 => builder.ins().ireduce(primitive(ty), v),
                        Prim::F32 | Prim::P64 | Prim::I64 | Prim::F64 => v,
                    };
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
                Bc::NoCompile => err!("NoCompile",),
                Bc::LastUse { .. } => {}
                Bc::PeekDup(skip) => {
                    self.stack.push(self.stack[self.stack.len() - skip as usize - 1]);
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
            }
        }
        Ok(())
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
            let mut ty = *ty;
            if matches!(ty, Prim::I8 | Prim::I16 | Prim::I32) {
                ty = Prim::I64;
            }
            // TODO: sign extend once I use small int types? but really caller should always do it I think.
            sig.params.push(AbiParam {
                value_type: primitive(ty),
                purpose: ArgumentPurpose::Normal,
                extension: ArgumentExtension::None,
            });
        }

        let make_ret = |sig: &mut Signature, mut ty: Prim| {
            if matches!(ty, Prim::I8 | Prim::I16 | Prim::I32) {
                ty = Prim::I64;
            }
            // TODO: sign extend once I use small int types?
            sig.returns.push(AbiParam {
                value_type: primitive(ty),
                purpose: ArgumentPurpose::Normal,
                extension: ArgumentExtension::None,
            });
        };
        if let Some(r) = sign.ret1 {
            make_ret(&mut sig, r);
        }
        if let Some(r) = sign.ret2 {
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

#[track_caller]
fn wrap(e: ModuleError) -> Box<CompileError<'static>> {
    make_err(CErr::Fatal(match e {
        ModuleError::Compilation(CodegenError::Verifier(e)) => format!("cranelift: {}", e),
        _ => format!("cranelift: {:?}", e),
    }))
}

#[track_caller]
fn wrapg(e: cranelift::codegen::CodegenError) -> Box<CompileError<'static>> {
    make_err(CErr::Fatal(format!("cranelift: {:?}", e)))
}

macro_rules! inst {
    ($name:ident) => {
        |builder: &mut FunctionBuilder, v: &[Value]| builder.ins().$name(v[0], v[1])
    };
}

macro_rules! icmp {
    ($name:ident) => {
        |builder: &mut FunctionBuilder, v: &[Value]| {
            let cond = builder.ins().icmp(IntCC::$name, v[0], v[1]);
            builder.ins().uextend(I64, cond)
        }
    };
}

macro_rules! fcmp {
    ($name:ident) => {
        |builder: &mut FunctionBuilder, v: &[Value]| {
            let cond = builder.ins().fcmp(FloatCC::$name, v[0], v[1]);
            builder.ins().uextend(I64, cond)
        }
    };
}

pub type CfEmit = fn(&mut FunctionBuilder, &[Value]) -> Value;

// TODO: still emit these as real functions if you take a pointer to one.
pub const BUILTINS: &[(&str, CfEmit)] = &[
    ("fn add(_: i64, __: i64) i64;", inst!(iadd)),
    ("fn sub(_: i64, __: i64) i64;", inst!(isub)),
    ("fn mul(_: i64, __: i64) i64;", inst!(imul)),
    ("fn div(_: i64, __: i64) i64;", inst!(sdiv)),
    ("fn eq(_: i64, __: i64) bool;", icmp!(Equal)),
    ("fn ne(_: i64, __: i64) bool;", icmp!(NotEqual)),
    ("fn lt(_: i64, __: i64) bool;", icmp!(SignedLessThan)),
    ("fn gt(_: i64, __: i64) bool;", icmp!(SignedGreaterThan)),
    ("fn le(_: i64, __: i64) bool;", icmp!(SignedLessThanOrEqual)),
    ("fn ge(_: i64, __: i64) bool;", icmp!(SignedGreaterThanOrEqual)),
    ("fn add(_: f64, __: f64) f64;", inst!(fadd)),
    ("fn sub(_: f64, __: f64) f64;", inst!(fsub)),
    ("fn mul(_: f64, __: f64) f64;", inst!(fmul)),
    ("fn div(_: f64, __: f64) f64;", inst!(fdiv)),
    ("fn eq(_: f64, __: f64) bool;", fcmp!(Equal)),
    ("fn ne(_: f64, __: f64) bool;", fcmp!(NotEqual)),
    ("fn lt(_: f64, __: f64) bool;", fcmp!(LessThan)),
    ("fn gt(_: f64, __: f64) bool;", fcmp!(GreaterThan)),
    ("fn le(_: f64, __: f64) bool;", fcmp!(LessThanOrEqual)),
    ("fn ge(_: f64, __: f64) bool;", fcmp!(GreaterThanOrEqual)),
    ("fn bit_or(_: i64, __: i64) i64;", inst!(bor)),
    ("fn bit_and(_: i64, __: i64) i64;", inst!(band)),
    ("fn shift_left(_: i64, __: i64) i64;", inst!(ishl)),
    ("fn offset(_: rawptr, bytes: i64) rawptr;", inst!(iadd)),
    ("fn bit_not(_: i64) i64;", |builder: &mut FunctionBuilder, v: &[Value]| {
        builder.ins().bnot(v[0])
    }),
    // it seems this matches what i do.
    // https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/codegen/src/isa/aarch64/inst/emit.rs#L2183
    ("fn int(_: f64) i64;", |builder: &mut FunctionBuilder, v: &[Value]| {
        builder.ins().fcvt_to_sint_sat(I64, v[0])
    }),
    ("fn typeid_to_int(_: Type) i64;", |_: &mut FunctionBuilder, v: &[Value]| v[0]),
    ("fn float(_: i64) f64;", |builder: &mut FunctionBuilder, v: &[Value]| {
        builder.ins().fcvt_from_sint(F64, v[0])
    }),
    ("fn bitcast(_: i64) f64;", |builder: &mut FunctionBuilder, v: &[Value]| {
        builder.ins().bitcast(F64, MemFlags::new(), v[0])
    }),
    ("fn bitcast(_: f64) i64;", |builder: &mut FunctionBuilder, v: &[Value]| {
        builder.ins().bitcast(I64, MemFlags::new(), v[0])
    }),
    ("fn cast(_: f32) f64;", |builder: &mut FunctionBuilder, v: &[Value]| {
        builder.ins().fpromote(F64, v[0])
    }),
    ("fn cast(_: f64) f32;", |builder: &mut FunctionBuilder, v: &[Value]| {
        builder.ins().fdemote(F32, v[0])
    }),
];
