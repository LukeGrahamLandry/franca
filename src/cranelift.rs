use std::mem;

use cranelift::{
    codegen::{
        ir::{
            stackslot::StackSize,
            types::{F64, I64, R64},
            ArgumentExtension, ArgumentPurpose, StackSlot,
        },
        settings::Flags,
        CodegenError,
    },
    frontend::FuncInstBuilder,
    prelude::*,
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module, ModuleError};

use crate::{
    ast::{Flag, FnType, FuncId, TypeId, TypeInfo},
    bc::{Bc, FnBody},
    compiler::{CErr, Compile, CompileError, Res},
    err, extend_options,
    logging::{make_err, PoolLog},
    reflect::BitSet,
};

pub struct JittedCl {
    module: JITModule,
    funcs: Vec<Option<cranelift_module::FuncId>>,
}

pub fn emit_cl<'p>(compile: &mut Compile<'_, 'p>, body: &FnBody<'p>, f: FuncId) -> Res<'p, ()> {
    let ctx = compile.cranelift.module.make_context();
    let mut e = Emit {
        compile,
        body,
        blocks: vec![],
        stack: vec![],
        indirect_ret_addr: None,
        vars: vec![],
        f: FuncId::from_index(0),
        block_done: BitSet::empty(),
    };
    e.emit_func(f, FunctionBuilderContext::new(), ctx)?;
    Ok(())
}

impl Default for JittedCl {
    fn default() -> Self {
        let isa = cranelift_native::builder().unwrap();
        let flags = settings::builder();
        // TODO: want to let comptime control settings...
        let isa = isa.finish(Flags::new(flags));
        let builder = JITBuilder::with_isa(isa.unwrap(), cranelift_module::default_libcall_names());
        let module = JITModule::new(builder);
        JittedCl { module, funcs: vec![] }
    }
}

impl JittedCl {
    pub fn get_ptr(&mut self, f: FuncId) -> Option<*const u8> {
        self.module.finalize_definitions().unwrap();
        self.funcs[f.as_index()].map(|f| self.module.get_finalized_function(f))
    }
}

struct Emit<'z, 'a, 'p> {
    compile: &'z mut Compile<'a, 'p>,
    body: &'z FnBody<'p>,
    blocks: Vec<Block>,
    block_done: BitSet,
    stack: Vec<Value>,
    indirect_ret_addr: Option<Value>,
    vars: Vec<StackSlot>,
    f: FuncId,
}

impl<'z, 'a, 'p> Emit<'z, 'a, 'p> {
    fn emit_func(&mut self, f: FuncId, mut builder_ctx: FunctionBuilderContext, mut ctx: codegen::Context) -> Res<'p, ()> {
        self.f = f;
        self.compile.last_loc = Some(self.compile.program[f].loc);
        let f_ty = self.compile.program[f].unwrap_ty();
        ctx.func.signature = self.make_sig(f_ty, true);

        let linkage = if self.compile.program[f].comptime_addr.is_some() {
            Linkage::Import
        } else {
            Linkage::Export
        };

        let name = format!("FN{}", f.as_index());
        let id = self
            .compile
            .cranelift
            .module
            .declare_function(&name, linkage, &ctx.func.signature)
            .map_err(wrap)?;

        if let Some(addr) = self.compile.program[f].comptime_addr {
        } else {
            let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);
            self.emit_body(&mut builder)?;
            if self.compile.program[f].has_tag(Flag::Log_Cl) {
                println!(
                    "=== Cranelift IR for {f:?}: {} ===",
                    self.compile.program.pool.get(self.compile.program[f].name)
                );
                println!("{}", builder.func);
                println!("===");
            }
            builder.finalize();
            self.compile.cranelift.module.define_function(id, &mut ctx).map_err(wrap)?;
        }

        extend_options(&mut self.compile.cranelift.funcs, f.as_index());
        self.compile.cranelift.funcs[f.as_index()] = Some(id);
        self.compile.cranelift.module.clear_context(&mut ctx);

        Ok(())
    }

    fn emit_body(&mut self, builder: &mut FunctionBuilder) -> Res<'p, ()> {
        self.vars.clear();
        for ty in &self.body.vars {
            let slots = self.compile.slot_count(*ty);
            let v = builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, (slots * 8) as StackSize));
            self.vars.push(v);
        }

        self.blocks.clear();
        self.block_done.clear();
        for _ in 0..self.body.blocks.len() {
            self.blocks.push(builder.create_block());
        }

        builder.append_block_params_for_function_params(self.blocks[0]);
        let f_ty = self.compile.program[self.f].unwrap_ty();
        if self.compile.slot_count(f_ty.ret) > 1 {
            self.indirect_ret_addr = Some(*builder.block_params(self.blocks[0]).last().unwrap());
        } else {
            self.indirect_ret_addr = None;
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
        debug_assert_eq!(args.len(), block.arg_slots as usize);
        for i in 0..block.arg_slots {
            let v = args[i as usize];
            self.stack.push(v)
        }

        for inst in &block.insts {
            match *inst {
                Bc::CallDirect { f, tail } => {
                    let f_ty = self.compile.program[f].unwrap_ty();
                    let slots = self.compile.slot_count(f_ty.arg);
                    let args = &self.stack[self.stack.len() - slots as usize..self.stack.len()];
                    if let Some(emit) = self.compile.program[f].cl_emit_fn_ptr {
                        let emit: CfEmit = unsafe { mem::transmute(emit) };
                        let v = emit(builder.ins(), args);
                        pops(&mut self.stack, slots as usize);
                        self.stack.push(v);
                        if tail {
                            builder.ins().return_(&[v]);
                            break;
                        } else {
                            continue;
                        }
                    }

                    // I want to allow mixing backends so you could ask for better optimisation for specific comptime functions
                    // without slowing compilation for all of them. So when trying to call something, check if my asm backend has it.
                    let addr = self.compile.program[f]
                        .comptime_addr
                        .or_else(|| self.compile.aarch64.get_fn(f).map(|a| a as u64));

                    let call = if let Some(addr) = addr {
                        let callee = builder.ins().iconst(I64, addr as i64);
                        let ty = self.compile.program[f].unwrap_ty();
                        let sig = self.make_sig(ty, false);
                        let sig = builder.import_signature(sig);
                        let args = &self.stack[self.stack.len() - slots as usize..self.stack.len()];

                        let call = builder.ins().call_indirect(sig, callee, args);
                        if tail {
                            // I guess we don't trust rustc to agree with cranelift about what a tail call means?
                            // and they want to be fancier than me about skipping adjusting sp?
                            let ret = builder.inst_results(call).to_vec();
                            builder.ins().return_(&ret);
                            break;
                        }
                        call
                    } else {
                        let func_id = self.compile.cranelift.funcs[f.as_index()].unwrap();
                        let callee = self.compile.cranelift.module.declare_func_in_func(func_id, builder.func);
                        if tail {
                            builder.ins().return_call(callee, args);
                            break;
                        }
                        builder.ins().call(callee, args)
                    };

                    let ret = builder.inst_results(call);
                    pops(&mut self.stack, slots as usize);
                    for v in ret {
                        self.stack.push(*v);
                    }
                }
                Bc::CallDirectFlat { f } => todo!(),
                Bc::CallSplit { ct, rt } => todo!(),
                Bc::CallFnPtr { ty, comp_ctx } => todo!(),
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
                    let slots = self.compile.slot_count(self.compile.program[self.f].finished_ret.unwrap());
                    let args = &self.stack[self.stack.len() - slots as usize..self.stack.len()];
                    if let Some(ret_addr) = self.indirect_ret_addr {
                        for s in 0..slots {
                            let v = self.stack[self.stack.len() - slots as usize + s as usize];
                            builder.ins().store(MemFlags::new(), v, ret_addr, s as i32 * 8);
                        }
                        builder.ins().return_(&[]);
                    } else {
                        builder.ins().return_(args);
                    }
                    pops(&mut self.stack, slots as usize);
                    break;
                }
                Bc::GetNativeFnPtr(_) => todo!(),
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
                    let slot = self.vars[id as usize];
                    self.stack.push(builder.ins().stack_addr(R64, slot, 0));
                }
                Bc::IncPtr { offset } => {
                    let ptr = self.stack.pop().unwrap();
                    let offset = builder.ins().iconst(I64, offset as i64 * 8);
                    let ptr = builder.ins().bitcast(I64, MemFlags::new(), ptr);
                    let res = builder.ins().iadd(ptr, offset);
                    let res = builder.ins().bitcast(R64, MemFlags::new(), res);
                    self.stack.push(res);
                }
                Bc::Pop { slots } => {
                    pops(&mut self.stack, slots as usize);
                }
                Bc::TagCheck { expected } => todo!(),
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
                        let config = self.compile.cranelift.module.target_config();
                        builder.call_memcpy(config, to, from, size)
                    }
                }
            }
        }
        Ok(())
    }

    fn make_type(&mut self, ty: TypeId) -> Type {
        let ty = self.compile.program.raw_type(ty);
        match &self.compile.program[ty] {
            TypeInfo::Unknown | TypeInfo::Any => todo!(),
            TypeInfo::Never => todo!(),
            TypeInfo::F64 => F64,
            TypeInfo::FnPtr(_)
            | TypeInfo::Label(_)
            | TypeInfo::Type
            | TypeInfo::Unit
            | TypeInfo::OverloadSet
            | TypeInfo::Int(_)
            | TypeInfo::Fn(_)
            | TypeInfo::Bool => I64,
            TypeInfo::Tuple(_) => F64,
            TypeInfo::Struct { .. } | TypeInfo::Tagged { .. } => R64,
            TypeInfo::Unique(_, _) | TypeInfo::Named(_, _) => unreachable!(),

            TypeInfo::Ptr(_) | TypeInfo::VoidPtr => R64,
            TypeInfo::Scope => todo!(),
        }
    }

    fn make_sig(&mut self, t: FnType, internal: bool) -> Signature {
        let mut sig = self.compile.cranelift.module.make_signature();
        if internal {
            sig.call_conv = cranelift::codegen::isa::CallConv::Tail; // i guess you can't say thing for ffi ones?
        }
        let arg = self.compile.program.raw_type(t.arg);
        if let TypeInfo::Tuple(types) = self.compile.program[arg].clone() {
            for t in types {
                let slots = self.compile.slot_count(t);
                let purpose = if slots == 1 {
                    ArgumentPurpose::Normal
                } else {
                    ArgumentPurpose::StructArgument(slots as u32 * 8)
                };
                sig.params.push(AbiParam {
                    value_type: self.make_type(t),
                    purpose,
                    extension: ArgumentExtension::Sext,
                })
            }
        } else {
            let slots = self.compile.slot_count(arg);
            let purpose = if slots == 1 {
                ArgumentPurpose::Normal
            } else {
                ArgumentPurpose::StructArgument(slots as u32 * 8)
            };
            sig.params.push(AbiParam {
                value_type: self.make_type(arg),
                purpose,
                extension: ArgumentExtension::Sext,
            })
        }
        // TODO: unit. TODO: multiple returns tuple?
        let ret = self.compile.program.raw_type(t.ret);
        let slots = self.compile.slot_count(ret);
        if slots == 1 {
            sig.returns.push(AbiParam {
                value_type: self.make_type(ret),
                purpose: ArgumentPurpose::Normal,
                extension: ArgumentExtension::Sext,
            });
        } else {
            sig.params.push(AbiParam {
                value_type: R64,
                purpose: ArgumentPurpose::StructReturn,
                extension: ArgumentExtension::Sext,
            })
        };

        sig
    }
}

fn wrap(e: ModuleError) -> Box<CompileError<'static>> {
    make_err(CErr::Fatal(match e {
        ModuleError::Compilation(CodegenError::Verifier(e)) => format!("cranelift: {}", e),
        _ => format!("cranelift: {:?}", e),
    }))
}

fn pops<T>(v: &mut Vec<T>, count: usize) {
    for _ in 0..count {
        v.pop().unwrap();
    }
}

macro_rules! inst {
    ($name:ident) => {
        |builder: FuncInstBuilder, v: &[Value]| builder.$name(v[0], v[1])
    };
}

macro_rules! icmp {
    ($name:ident) => {
        |builder: FuncInstBuilder, v: &[Value]| builder.icmp(IntCC::$name, v[0], v[1])
    };
}

macro_rules! fcmp {
    ($name:ident) => {
        |builder: FuncInstBuilder, v: &[Value]| builder.fcmp(FloatCC::$name, v[0], v[1])
    };
}

pub type CfEmit = fn(FuncInstBuilder, &[Value]) -> Value;

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
    ("fun offset(_: rawptr, bytes: i64) i64;", inst!(iadd)),
    ("fn bit_not(_: i64) i64;", |builder: FuncInstBuilder, v: &[Value]| builder.bnot(v[0])),
];
