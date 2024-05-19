//! TODO:
//! - tail calls
//! - backtrace needs to deal with pointer authentication
//! - soemthing makes it jump into garbage when you run too many tests

use std::mem;

use cranelift::{
    codegen::{
        control::ControlPlane,
        ir::{
            stackslot::StackSize,
            types::{F64, I64, I8},
            ArgumentExtension, ArgumentPurpose, StackSlot,
        },
        settings::Flags,
        CodegenError,
    },
    prelude::*,
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module, ModuleError};

use crate::{
    ast::{CallConv, Flag, FnType, FuncId, FuncImpl, Program, TypeId, TypeInfo},
    bc::{BasicBlock, Bc, FnBody},
    bc_to_asm::Jitted,
    compiler::{CErr, Compile, CompileError, Res},
    emit_bc::EmitBc,
    err, extend_options,
    logging::make_err,
    reflect::BitSet,
    unwrap,
};

pub struct JittedCl {
    pub module: JITModule,
    funcs: Vec<Option<cranelift_module::FuncId>>,
    // can't just use funcs[i].is_some because it might just be a forward declaration.
    funcs_done: BitSet,
    pending: Vec<FuncId>,
}

pub fn emit_cl<'p>(compile: &mut Compile<'_, 'p>, body: &FnBody<'p>, f: FuncId) -> Res<'p, ()> {
    if compile.cranelift.funcs_done.get(f.as_index()) {
        return Ok(());
    }
    let ctx = compile.cranelift.module.make_context();

    let mut flat_sig = Signature::new(compile.cranelift.module.target_config().default_call_conv);
    flat_sig.params.push(AbiParam::new(I64));
    flat_sig.params.push(AbiParam::new(I64));
    flat_sig.params.push(AbiParam::new(I64));
    flat_sig.params.push(AbiParam::new(I64));
    flat_sig.params.push(AbiParam::new(I64));

    compile.last_loc = Some(compile.program[f].loc);
    let is_flat = compile.program[f].cc.unwrap() == CallConv::Flat;
    let mut e = Emit {
        body,
        blocks: vec![],
        stack: vec![],
        indirect_ret_addr: None,
        vars: vec![],
        f: FuncId::from_index(0),
        block_done: BitSet::empty(),
        flat_arg_addr: None,
        flat_args_already_offset: vec![],
        compile_ctx_ptr: compile as *mut Compile as usize,
        program: compile.program,
        asm: &mut compile.aarch64,
        cl: &mut compile.cranelift,
        flat_sig,
        is_flat,
    };
    e.emit_func(f, FunctionBuilderContext::new(), ctx)?;
    compile.cranelift.funcs_done.insert(f.as_index(), true);
    Ok(())
}

pub(crate) fn emit_cl_intrinsic<'p>(compile: &mut Compile<'_, 'p>, _ptr: usize, f: FuncId) -> Res<'p, ()> {
    let mut body = EmitBc::empty_fn(compile.program, f);
    body.func = f;
    let arg = compile.program[f].finished_arg.unwrap();
    let arg = compile.program.get_info(arg);
    body.blocks.push(BasicBlock {
        insts: vec![Bc::CallDirect { f, tail: true }],
        arg_slots: arg.size_slots,
        arg_float_mask: arg.float_mask,
        incoming_jumps: 0,
        clock: 0,
        height: 0,
    });
    emit_cl(compile, &body, f)
}

impl Default for JittedCl {
    fn default() -> Self {
        let isa = cranelift_native::builder().unwrap();
        let mut flags = settings::builder();
        flags.set("preserve_frame_pointers", "true").unwrap();
        flags.set("unwind_info", "false").unwrap();

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

impl JittedCl {
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

struct Emit<'z, 'p> {
    compile_ctx_ptr: usize, // TODO: HACK. should really pass this through correctly as an argument. seperate which flat calls really need it.
    program: &'z mut Program<'p>,
    asm: &'z mut Jitted, // TODO: kinda hack. other side should be able to do too
    cl: &'z mut JittedCl,
    body: &'z FnBody<'p>,
    blocks: Vec<Block>,
    block_done: BitSet,
    stack: Vec<Value>,
    indirect_ret_addr: Option<Value>,
    flat_arg_addr: Option<Value>,
    flat_args_already_offset: Vec<Value>,
    vars: Vec<StackSlot>,
    f: FuncId,
    flat_sig: Signature,
    is_flat: bool,
}

impl<'z, 'p> Emit<'z, 'p> {
    fn emit_func(&mut self, f: FuncId, mut builder_ctx: FunctionBuilderContext, mut ctx: codegen::Context) -> Res<'p, ()> {
        self.f = f;
        let f_ty = self.program[f].unwrap_ty();

        let name = format!("FN{}", f.as_index());
        ctx.func.signature = if self.is_flat {
            self.flat_sig.clone() // TODO: if i have to clone anyway, maybe dont make it upfront?
        } else {
            assert!(self.program[f].cc.unwrap() != CallConv::Arg8Ret1Ct);
            self.make_sig(f_ty, true, false)
        };

        let id = self
            .cl
            .module
            .declare_function(&name, Linkage::Export, &ctx.func.signature)
            .map_err(wrap)?;
        extend_options(&mut self.cl.funcs, f.as_index());
        self.cl.funcs[f.as_index()] = Some(id);

        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);
        self.emit_body(&mut builder)?;
        if self.program[f].has_tag(Flag::Log_Ir) {
            println!("=== Cranelift IR for {f:?}: {} ===", self.program.pool.get(self.program[f].name));
            println!("{}", builder.func.display());
            println!("===");
        }
        builder.finalize();
        self.cl.module.define_function(id, &mut ctx).map_err(wrap)?;

        if self.program[f].has_tag(Flag::Log_Asm) {
            ctx.want_disasm = true;
            let code = ctx.compile_stencil(self.cl.module.isa(), &mut ControlPlane::default()).map_err(wrapg)?;
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
        self.flat_args_already_offset.clear();
        if self.is_flat {
            let entry = builder.create_block();
            builder.switch_to_block(entry);

            builder.append_block_params_for_function_params(entry);
            self.indirect_ret_addr = Some(builder.block_params(entry)[3]);
            self.flat_arg_addr = Some(builder.block_params(entry)[1]);

            for block in &self.body.blocks {
                self.blocks.push(builder.create_block());
                for _ in 0..block.arg_slots {
                    builder.append_block_param(*self.blocks.last().unwrap(), I64);
                }
            }
            builder.ins().jump(self.blocks[0], &[]);
            builder.seal_block(entry);
        } else {
            // TODO: copy-paste
            for (i, block) in self.body.blocks.iter().enumerate() {
                self.blocks.push(builder.create_block());
                if i != 0 {
                    for _ in 0..block.arg_slots {
                        builder.append_block_param(*self.blocks.last().unwrap(), I64);
                    }
                }
            }
            builder.append_block_params_for_function_params(self.blocks[0]);
            self.indirect_ret_addr = None;
            self.flat_arg_addr = None;
        }

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
        if b == 0 && !self.is_flat {
            let arg = self.program.get_info(self.program[self.f].finished_arg.unwrap());
            debug_assert_eq!(arg.size_slots, block.arg_slots);
            self.cast_ret_from_float(builder, arg.size_slots, arg.float_mask);
        }

        for inst in &block.insts {
            match *inst {
                Bc::GetCompCtx => {
                    let v = builder.ins().iconst(I64, self.compile_ctx_ptr as i64);
                    self.stack.push(v);
                }
                Bc::NameFlatCallArg { id, offset } => {
                    let Some(ptr) = self.flat_arg_addr else { err!("not flat call",) };
                    debug_assert_eq!(id as usize, self.flat_args_already_offset.len());
                    let offset = builder.ins().iconst(I64, offset as i64 * 8);
                    let ptr = builder.ins().iadd(ptr, offset);
                    self.flat_args_already_offset.push(ptr);
                }
                Bc::CallDirect { f, tail } => {
                    let f_ty = self.program[f].unwrap_ty();
                    let (mut arg, ret) = self.program.get_infos(f_ty);
                    if self.program[f].cc.unwrap() == CallConv::Arg8Ret1Ct {
                        // Note: don't have to adjust float mask for comp_ctx because its added on the left where the bit mask is already zeros
                        arg.size_slots += 1;
                    }
                    self.cast_args_to_float(builder, arg.size_slots, arg.float_mask);

                    let args = &self.stack[self.stack.len() - arg.size_slots as usize..self.stack.len()];
                    if let FuncImpl::EmitCranelift(emit) = self.program[f].body {
                        let emit: CfEmit = unsafe { mem::transmute(emit) };
                        let v = emit(builder, args);
                        pops(&mut self.stack, arg.size_slots as usize);
                        if ret.size_slots == 0 {
                            // Unit is zero sized.
                            if tail {
                                builder.ins().return_(&[]);
                                break;
                            } else {
                                continue;
                            }
                        } else {
                            self.stack.push(v);
                            debug_assert_eq!(ret.size_slots, 1);
                            self.cast_ret_from_float(builder, ret.size_slots, ret.float_mask);
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

                    let ty = self.program[f].unwrap_ty();
                    // Note: check cl funcs first because ptrs go in the asm dispatch table anyway.
                    // TODO: actually forward declare if none to make mutual recursion work.
                    let call = if let Some(&Some(func_id)) = self.cl.funcs.get(f.as_index()) {
                        let callee = self.cl.module.declare_func_in_func(func_id, builder.func);
                        if tail {
                            // TODO: have to deal with floats.
                            // builder.ins().return_call(callee, args);
                            // TODO: cranelift wants to do real tailcalls which changes the abi.
                            let call = builder.ins().call(callee, args);
                            if ty.ret.is_never() {
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
                            .get_fn(f)
                            .map(|a| a as u64)
                            .map(|addr| builder.ins().iconst(I64, addr as i64))
                            .unwrap_or_else(|| {
                                // TODO: HACK: this is really stupid. this is how my asm does it but cl has relocation stuff so just have to forward declare the funcid.
                                //       its just annoying because then i need to make sure i know if im supposed to be declaring
                                //       or jsut waiting on it to be done by a different backend if i want to allow you mixing them    -- May 16
                                self.asm.extend_blanks(f);
                                self.asm.pending_indirect.push(f);
                                let dispatch = builder.ins().iconst(I64, self.asm.get_dispatch() as i64);
                                builder.ins().load(I64, MemFlags::new(), dispatch, f.as_index() as i32 * 8)
                            });

                        let comp_ctx = self.program[f].cc.unwrap() == CallConv::Arg8Ret1Ct;
                        let sig = self.make_sig(ty, false, comp_ctx);
                        let sig = builder.import_signature(sig);
                        let args = &self.stack[self.stack.len() - arg.size_slots as usize..self.stack.len()];

                        let call = builder.ins().call_indirect(sig, callee, args);
                        if tail {
                            if ty.ret.is_never() {
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
                    pops(&mut self.stack, arg.size_slots as usize);
                    for v in ret_val {
                        self.stack.push(*v);
                    }
                    self.cast_ret_from_float(builder, ret_val.len() as u16, ret.float_mask);
                }
                Bc::CallDirectFlat { f } => {
                    let f_ty = &self.program[f].unwrap_ty();
                    // (compiler, arg_ptr, arg_len_i64s, ret_ptr, ret_len_i64)
                    let c = self.compile_ctx_ptr as i64;
                    let c = builder.ins().iconst(I64, c);
                    let arg_ptr = self.stack.pop().unwrap();
                    let arg_count = self.program.slot_count(f_ty.arg);
                    let arg_count = builder.ins().iconst(I64, arg_count as i64);
                    let ret_ptr = self.stack.pop().unwrap();
                    let ret_count = self.program.slot_count(f_ty.ret);
                    let ret_count = builder.ins().iconst(I64, ret_count as i64);

                    let args = &[c, arg_ptr, arg_count, ret_ptr, ret_count];
                    // flat_call result goes into a variable somewhere, already setup by bc. so don't worry about return value here.
                    // need to check cl.funcs first because we put our functions in .dispatch too.
                    if let Some(&Some(func_id)) = self.cl.funcs.get(f.as_index()) {
                        let callee = self.cl.module.declare_func_in_func(func_id, builder.func);
                        builder.ins().call(callee, args);
                    } else {
                        let callee = self
                            .asm
                            .get_fn(f)
                            .map(|a| a as u64)
                            .map(|addr| builder.ins().iconst(I64, addr as i64))
                            .unwrap_or_else(|| {
                                // TODO: HACK: this is really stupid. this is how my asm does it but cl has relocation stuff so just have to forward declare the funcid.
                                //       its just annoying because then i need to make sure i know if im supposed to be declaring
                                //       or jsut waiting on it to be done by a different backend if i want to allow you mixing them    -- May 16
                                self.asm.extend_blanks(f);
                                self.asm.pending_indirect.push(f);
                                let dispatch = builder.ins().iconst(I64, self.asm.get_dispatch() as i64);
                                builder.ins().load(I64, MemFlags::new(), dispatch, f.as_index() as i32 * 8)
                            });
                        let sig = builder.import_signature(self.flat_sig.clone());
                        builder.ins().call_indirect(sig, callee, args);
                    }
                    if f_ty.ret.is_never() {
                        builder.ins().trap(TrapCode::UnreachableCodeReached);
                        break;
                    }
                }
                Bc::CallFnPtr { ty, comp_ctx } => {
                    assert!(!comp_ctx);
                    let sig = builder.import_signature(self.make_sig(ty, false, comp_ctx));
                    let (arg, ret) = self.program.get_infos(ty);
                    self.cast_args_to_float(builder, arg.size_slots, arg.float_mask);
                    let first_arg = self.stack.len() - arg.size_slots as usize;
                    let callee = self.stack[first_arg - 1];
                    let args = &self.stack[first_arg..self.stack.len()];
                    let call = builder.ins().call_indirect(sig, callee, args);
                    pops(&mut self.stack, arg.size_slots as usize + 1);
                    let ret_val = builder.inst_results(call);
                    for v in ret_val {
                        self.stack.push(*v);
                    }
                    self.cast_ret_from_float(builder, ret_val.len() as u16, ret.float_mask);
                }
                Bc::PushConstant { value } => self.stack.push(builder.ins().iconst(I64, value)),
                Bc::JumpIf { true_ip, false_ip, slots } => {
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
                Bc::Ret => {
                    if self.indirect_ret_addr.is_some() {
                        // flat_call so we must have already put the values there.
                        debug_assert!(self.flat_arg_addr.is_some());
                        builder.ins().return_(&[]);
                    } else {
                        let ret = self.program.get_info(self.program[self.f].finished_ret.unwrap());
                        self.cast_args_to_float(builder, ret.size_slots, ret.float_mask);
                        let args = &self.stack[self.stack.len() - ret.size_slots as usize..self.stack.len()];
                        builder.ins().return_(args);
                        pops(&mut self.stack, ret.size_slots as usize);
                    }
                    break;
                }
                Bc::GetNativeFnPtr(f) => {
                    if let Some(&Some(id)) = self.cl.funcs.get(f.as_index()) {
                        let id = self.cl.module.declare_func_in_func(id, builder.func);
                        self.stack.push(builder.ins().func_addr(I64, id));
                    } else {
                        let addr = self.asm.get_fn(f).map(|a| a as i64);
                        let addr = unwrap!(addr, "fn not ready");
                        self.stack.push(builder.ins().iconst(I64, addr));
                    }
                }
                Bc::Load { slots } => {
                    debug_assert_ne!(slots, 0);
                    let addr = self.stack.pop().unwrap();
                    for s in 0..slots {
                        let v = builder.ins().load(I64, MemFlags::new(), addr, s as i32 * 8);
                        self.stack.push(v);
                    }
                }
                Bc::StorePost { slots } => {
                    debug_assert_ne!(slots, 0);
                    let addr = self.stack.pop().unwrap();
                    for s in 0..slots {
                        let v = self.stack[self.stack.len() - slots as usize + s as usize];
                        builder.ins().store(MemFlags::new(), v, addr, s as i32 * 8);
                    }
                    pops(&mut self.stack, slots as usize);
                }
                Bc::StorePre { slots } => {
                    debug_assert_ne!(slots, 0);
                    let addr = self.stack[self.stack.len() - slots as usize - 1];
                    for s in 0..slots {
                        let v = self.stack[self.stack.len() - slots as usize + s as usize];
                        builder.ins().store(MemFlags::new(), v, addr, s as i32 * 8);
                    }
                    pops(&mut self.stack, slots as usize + 1);
                }
                Bc::AddrVar { id } => {
                    if let Some(&ptr) = self.flat_args_already_offset.get(id as usize) {
                        self.stack.push(ptr);
                    } else {
                        let slot = self.vars[id as usize];
                        self.stack.push(builder.ins().stack_addr(I64, slot, 0));
                    }
                }
                Bc::IncPtr { offset } => {
                    let ptr = self.stack.pop().unwrap();
                    let offset = builder.ins().iconst(I64, offset as i64 * 8);
                    let ptr = builder.ins().bitcast(I64, MemFlags::new(), ptr);
                    let res = builder.ins().iadd(ptr, offset);
                    let res = builder.ins().bitcast(I64, MemFlags::new(), res);
                    self.stack.push(res);
                }
                Bc::Pop { slots } => {
                    pops(&mut self.stack, slots as usize);
                }
                Bc::TagCheck { expected: _ } => {} // TODO: !!!
                Bc::Unreachable => {
                    builder.ins().trap(TrapCode::UnreachableCodeReached);
                    break;
                }
                Bc::NoCompile => err!("NoCompile",),
                Bc::LastUse { .. } | Bc::Noop => {}
                Bc::AddrFnResult => self.stack.push(self.indirect_ret_addr.unwrap()),
                Bc::Dup => {
                    self.stack.push(*self.stack.last().unwrap());
                }
                Bc::CopyToFrom { slots } => {
                    let from = self.stack.pop().unwrap();
                    let to = self.stack.pop().unwrap();
                    if slots < 4 {
                        for s in 0..slots {
                            let v = builder.ins().load(I64, MemFlags::new(), from, s as i32 * 8);
                            builder.ins().store(MemFlags::new(), v, to, s as i32 * 8);
                        }
                    } else {
                        let size = builder.ins().iconst(I64, slots as i64 * 8);
                        let config = self.cl.module.target_config();
                        builder.call_memcpy(config, to, from, size)
                    }
                }
            }
        }
        Ok(())
    }

    fn make_type(&mut self, ty: TypeId) -> Type {
        let ty = self.program.raw_type(ty);
        match &self.program[ty] {
            TypeInfo::Unknown => todo!(),
            TypeInfo::Never => todo!(),
            TypeInfo::F64 => F64,
            TypeInfo::FnPtr(_)
            | TypeInfo::Label(_)
            | TypeInfo::Type
            | TypeInfo::Unit
            | TypeInfo::OverloadSet
            | TypeInfo::Int(_)
            | TypeInfo::Fn(_)
            | TypeInfo::Scope
            | TypeInfo::Bool => I64,
            TypeInfo::Tuple(_) | TypeInfo::Struct { .. } | TypeInfo::Tagged { .. } => I64,
            TypeInfo::Ptr(_) | TypeInfo::VoidPtr => I64,
            TypeInfo::Enum { .. } | TypeInfo::Unique(_, _) | TypeInfo::Named(_, _) => unreachable!(),
        }
    }

    fn make_sig(&mut self, t: FnType, _internal: bool, comp_ctx: bool) -> Signature {
        let mut sig = self.cl.module.make_signature();
        // if internal {
        //     sig.call_conv = cranelift::codegen::isa::CallConv::Tail; // i guess you can't say thing for ffi ones?
        // }
        if comp_ctx {
            // TODO: should bring this all the way out into bc eventually
            sig.params.push(AbiParam {
                value_type: I64,
                purpose: ArgumentPurpose::Normal,
                extension: ArgumentExtension::None,
            })
        }

        // TODO: this is wrong. you can have nested things. i pass (Str, Str) as (*i,i,*i,i).
        //       but that disagrees with c calling convention so need to have another round of clarifying that.
        fn push(s: &mut Emit, arg: TypeId, sig: &mut Signature) {
            let arg = s.program.raw_type(arg);
            if let TypeInfo::Tagged { .. } = &s.program[arg] {
                for _ in 0..s.program.slot_count(arg) {
                    sig.params.push(AbiParam {
                        value_type: I64,
                        purpose: ArgumentPurpose::Normal,
                        extension: ArgumentExtension::None,
                    })
                }
            } else if let Some(types) = s.program.tuple_types(arg) {
                let types = types.to_vec();
                for t in types {
                    let t = s.program.raw_type(t);
                    push(s, t, sig);
                }
            } else if let TypeInfo::Unit = &s.program[arg] {
            } else {
                let extension = if arg == TypeId::f64() {
                    ArgumentExtension::None
                } else {
                    ArgumentExtension::Sext
                };
                sig.params.push(AbiParam {
                    value_type: s.make_type(arg),
                    purpose: ArgumentPurpose::Normal,
                    extension,
                })
            }
        }

        push(self, t.arg, &mut sig);

        // TODO: unit. TODO: multiple returns tuple?
        let ret = self.program.raw_type(t.ret);
        if !ret.is_never() && !ret.is_unit() {
            // TODO: ArgumentPurpose::StructReturn for real c abi. dont just slots==1 because never (and eventually unit) are 0
            let extension = if ret == TypeId::f64() {
                ArgumentExtension::None
            } else {
                ArgumentExtension::Sext
            };
            sig.returns.push(AbiParam {
                value_type: self.make_type(ret),
                purpose: ArgumentPurpose::Normal,
                extension,
            });
        }

        sig
    }

    // TODO: #ct
    fn cast_args_to_float(&mut self, builder: &mut FunctionBuilder, slots: u16, float_mask: u32) {
        // TODO: this is super dumb. I should really just track types through the whole thing properly.
        for slot_index in 0..slots as usize {
            let is_float = (float_mask >> (slots - slot_index as u16 - 1)) & 1 == 1;
            let s = self.stack.len() - (slots as usize) + slot_index;
            if is_float {
                self.stack[s] = builder.ins().bitcast(F64, MemFlags::new(), self.stack[s]);
            }
        }
    }

    fn cast_ret_from_float(&mut self, builder: &mut FunctionBuilder, slots: u16, float_mask: u32) {
        // TODO: this is super dumb. I should really just track types through the whole thing properly.
        debug_assert!(slots <= self.stack.len() as u16);
        for slot_index in 0..slots as usize {
            let is_float = (float_mask >> (slots - slot_index as u16 - 1)) & 1 == 1;
            let s = self.stack.len() - (slots as usize) + slot_index;
            if is_float {
                self.stack[s] = builder.ins().bitcast(I64, MemFlags::new(), self.stack[s]);
            }
        }
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

fn pops<T>(v: &mut Vec<T>, count: usize) {
    for _ in 0..count {
        v.pop().unwrap();
    }
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
    ("fun eq(_: i64, __: i64) bool;", icmp!(Equal)),
    ("fn ne(_: i64, __: i64) bool;", icmp!(NotEqual)),
    ("fn lt(_: i64, __: i64) bool;", icmp!(SignedLessThan)),
    ("fn gt(_: i64, __: i64) bool;", icmp!(SignedGreaterThan)),
    ("fn le(_: i64, __: i64) bool;", icmp!(SignedLessThanOrEqual)),
    ("fn ge(_: i64, __: i64) bool;", icmp!(SignedGreaterThanOrEqual)),
    ("fn add(_: f64, __: f64) f64;", inst!(fadd)),
    ("fn sub(_: f64, __: f64) f64;", inst!(fsub)),
    ("fn mul(_: f64, __: f64) f64;", inst!(fmul)),
    ("fn div(_: f64, __: f64) f64;", inst!(fdiv)),
    ("fun eq(_: f64, __: f64) bool;", fcmp!(Equal)),
    ("fn ne(_: f64, __: f64) bool;", fcmp!(NotEqual)),
    ("fn lt(_: f64, __: f64) bool;", fcmp!(LessThan)),
    ("fn gt(_: f64, __: f64) bool;", fcmp!(GreaterThan)),
    ("fn le(_: f64, __: f64) bool;", fcmp!(LessThanOrEqual)),
    ("fn ge(_: f64, __: f64) bool;", fcmp!(GreaterThanOrEqual)),
    ("fn bit_or(_: i64, __: i64) i64;", inst!(bor)),
    ("fn bit_and(_: i64, __: i64) i64;", inst!(band)),
    ("fn shift_left(_: i64, __: i64) i64;", inst!(ishl)),
    ("fun offset(_: rawptr, bytes: i64) rawptr;", inst!(iadd)),
    ("fn bit_not(_: i64) i64;", |builder: &mut FunctionBuilder, v: &[Value]| {
        builder.ins().bnot(v[0])
    }),
    ("fn load(_: *u8) u8;", |builder: &mut FunctionBuilder, v: &[Value]| {
        builder.ins().uload8(I64, MemFlags::new(), v[0], 0)
    }),
    ("fn store(_: *u8, val: u8) Unit;", |builder: &mut FunctionBuilder, v: &[Value]| {
        let val = builder.ins().ireduce(I8, v[1]);
        builder.ins().store(MemFlags::new(), val, v[0], 0);
        builder.ins().iconst(I64, 0)
    }),
    ("fn ptr_to_int(_: rawptr) i64;", |_: &mut FunctionBuilder, v: &[Value]| v[0]),
    ("fn int_to_ptr(_: i64) rawptr;", |_: &mut FunctionBuilder, v: &[Value]| v[0]),
    // it seems this matches what i do.
    // https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/codegen/src/isa/aarch64/inst/emit.rs#L2183
    ("fun int(_: f64) i64;", |builder: &mut FunctionBuilder, v: &[Value]| {
        builder.ins().fcvt_to_sint_sat(I64, v[0])
    }),
    ("fn typeid_to_int(_: Type) i64;", |_: &mut FunctionBuilder, v: &[Value]| v[0]),
    ("fn load(_: *Unit) Unit;", |builder: &mut FunctionBuilder, _: &[Value]| {
        builder.ins().iconst(I64, 0)
    }),
    ("fn store(_: *Unit, val: Unit) Unit;", |builder: &mut FunctionBuilder, _: &[Value]| {
        builder.ins().iconst(I64, 0)
    }),
];
