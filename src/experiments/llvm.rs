#![allow(clippy::missing_safety_doc)]
#![allow(unused)]

use std::{
    collections::HashMap,
    ffi::{c_uint, CStr, CString},
    mem::MaybeUninit,
    num::NonZeroU8,
    ptr::null,
};

use llvm_sys::{
    analysis::{LLVMVerifierFailureAction, LLVMVerifyFunction, LLVMVerifyModule},
    core::*,
    execution_engine::{
        LLVMAddModule, LLVMCreateJITCompilerForModule, LLVMDisposeExecutionEngine, LLVMExecutionEngineGetErrMsg, LLVMExecutionEngineRef,
        LLVMFindFunction, LLVMGetFunctionAddress, LLVMGetGlobalValueAddress, LLVMLinkInMCJIT, LLVMRecompileAndRelinkFunction,
    },
    ir_reader::LLVMParseIRInContext,
    linker::LLVMLinkModules2,
    prelude::*,
    target::{LLVM_InitializeNativeAsmPrinter, LLVM_InitializeNativeTarget},
    target_machine::LLVMGetTargetMachineTarget,
    transforms::{
        ipo::LLVMAddGlobalOptimizerPass, pass_builder::LLVMRunPasses, pass_manager_builder::LLVMPassManagerBuilderSetOptLevel,
        scalar::LLVMAddReassociatePass,
    },
};

use crate::{
    ast::{Flag, FnType, FuncId, Program, TypeId, TypeInfo},
    bc::{Bc, StackOffset, Value},
    compiler::Res,
    interp::Interp,
    logging::{err, unwrap, PoolLog},
    pool::Ident,
};

pub struct JittedLlvm {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    inline_asm_module: LLVMModuleRef,
    execution_engine: LLVMExecutionEngineRef,
    builder: LLVMBuilderRef,
    types: Vec<Option<LLVMTypeRef>>,
    functions: Vec<Option<(LLVMValueRef, CString, bool)>>,
    i32_ty: LLVMTypeRef,
}

impl JittedLlvm {
    /// Note: this will leak memory unless you call `release` but that has safety rules so I don't want to impl drop.
    pub fn new<'p>(name_in: &str, program: &mut Program) -> Res<'p, JittedLlvm> {
        unsafe {
            let context = LLVMContextCreate();
            let llvm_ir = program.emit_inline_llvm_ir();
            let len = llvm_ir.len(); // TODO: include null?

            // Not using the ___Copy one but I immediatly call parseIR_ so assuming it pulls everything it needs out of the string, it's fine to not outlive the stack frame.
            let llvm_ir = null_terminated(llvm_ir); // dont inline this in the expression cause it drops it too soon
            let llvm_ir = LLVMCreateMemoryBufferWithMemoryRange(llvm_ir.as_ptr(), len, EMPTY, LLVMBool::from(false));

            let inline_asm_module = return_ref(|out, err| LLVMParseIRInContext(context, llvm_ir, out, err))?;
            verify_module(inline_asm_module);

            // Fixes: Unable to find target for this triple (no targets are registered)
            assert_eq!(LLVM_InitializeNativeTarget(), 0);
            // Fixes: LLVM ERROR: Target does not support MC emission!
            assert_eq!(LLVM_InitializeNativeAsmPrinter(), 0);
            // Fixes: JIT has not been linked in.
            LLVMLinkInMCJIT();
            let execution_engine = return_ref(|out, err| LLVMCreateJITCompilerForModule(out, inline_asm_module, 2, err))?;

            let name = null_terminate(name_in);
            // Yes this is a lot of fucking around that seems pointless but you can't emit new code into a module created with LLVMParseIRInContext.
            // If you try there's no error (it passes verify), but asking the jit for pointers to newly emitted functions just gives you null.
            // Its weird cause it works if the module is empty at the beginning, its just if it has any functions, then it locks you out from making more.
            // Which really sucks cause then i cant do inline ir dynamiclly? well i guess just create a new module every time.
            // fuck i bet it cant inline across modules which kinda kills my whole plan of using functions for primatives.
            // TODO: maybe i just need to call LLVMLinkModules2 at the end? seems to just segsev but maybe i did it too early or wrong way around, idc rn.
            let module = LLVMModuleCreateWithNameInContext(name.as_ptr(), context);
            LLVMAddModule(execution_engine, module);

            let mut this = Self {
                execution_engine,
                builder: LLVMCreateBuilderInContext(context),
                context,
                inline_asm_module,
                module,
                types: vec![],
                functions: vec![],
                i32_ty: LLVMInt32TypeInContext(context),
            };

            for f in program.inline_llvm_ir.clone() {
                let name = null_terminate(&format!("FN{}", f.0));
                let ty = program.func_type(f);
                let ty = program.fn_ty(ty).unwrap();
                let ty = this.get_function_type(program, ty);
                // Note: not LLVMGetNamedFunction on inline_asm_module! we're declaring that we plan on importing into the normal module.
                let value = unsafe { LLVMAddFunction(this.module, name.as_ptr(), ty) };
                extend_options(&mut this.functions, f.0);
                this.functions[f.0] = Some((value, name, true));
            }

            Ok(this)
        }
    }

    fn decl_function(&mut self, program: &mut Program, f: FuncId) -> LLVMValueRef {
        extend_options(&mut self.functions, f.0);
        if let Some((f, _, _)) = self.functions[f.0] {
            return f;
        }

        let name = null_terminate(&format!("FN{}", f.0));
        let ty = program.func_type(f);
        let ty = program.fn_ty(ty).unwrap();
        let ty = self.get_function_type(program, ty);
        unsafe {
            let func = unsafe { LLVMAddFunction(self.module, name.as_ptr(), ty) };
            assert_ne!(func as usize, 0);
            self.functions[f.0] = Some((func, name, false));
            func
        }
    }

    fn get_fn(&mut self, f: FuncId) -> Option<LLVMValueRef> {
        extend_options(&mut self.functions, f.0);
        self.functions[f.0].as_ref().and_then(|(f, _, ready)| {
            if *ready {
                assert_ne!(*f as usize, 0);
                Some(*f)
            } else {
                None
            }
        })
    }

    pub fn get_fn_jitted(&mut self, f: FuncId) -> Option<*const u8> {
        let value = self.get_fn(f)?;
        let name = self.functions[f.0].as_ref().unwrap().1.as_ptr();
        let ptr = unsafe { LLVMGetFunctionAddress(self.execution_engine, name) as usize as *const u8 };
        assert_ne!(ptr as usize, 0);
        Some(ptr)
    }

    fn finish_fn(&mut self, f: FuncId) {
        let func = self.functions[f.0].as_mut().unwrap();
        func.2 = true;

        unsafe {
            // TODO: where does message go??
            let failed = LLVMVerifyFunction(func.0, LLVMVerifierFailureAction::LLVMPrintMessageAction);
            if failed != 0 {
                panic!("Failed llvm verify! {f:?}.");
            }
        }
    }

    /// This is more complicated than it sounds
    /// - Don't call this for comptime Fn(_) because you want to be able to pass around the indexes when talking to the compiler.
    /// - Tuples get pulled up to be represented as multiple arguments.
    ///   TODO: its weird that structs are treated differently but you need to be able to do whatever c does for ffi.
    ///         should have a @repr(C) so you can opt in. but then also some huristic for when to not bother trying to represent big tuples in registers?
    ///         or its fine, just spill them like you would if someone manually wrote that function, just seems like extra regalloc work.
    fn get_function_type(&mut self, program: &mut Program, ty: FnType) -> LLVMTypeRef {
        let mut arg = if let TypeInfo::Tuple(fields) = &program.types[ty.arg.0] {
            fields.clone().iter().map(|ty| self.get_type(program, *ty)).collect()
        } else {
            vec![self.get_type(program, ty.arg)]
        };
        let ret = self.get_type(program, ty.ret);
        unsafe { LLVMFunctionType(ret, arg.as_mut_ptr(), arg.len() as c_uint, LLVMBool::from(false)) }
    }

    fn get_type(&mut self, program: &mut Program, ty: TypeId) -> LLVMTypeRef {
        extend_options(&mut self.types, ty.0);
        if let Some(ty) = self.types[ty.0] {
            return ty;
        }

        let result = unsafe {
            match &program.types[ty.0] {
                TypeInfo::Unknown | TypeInfo::Any | TypeInfo::Never | TypeInfo::F64 => todo!("llvm type: {}", program.log_type(ty)),
                // TODO: special case Unit but need different type for enum padding. for returns unit should be LLVMVoidTypeInContext(self.context)
                TypeInfo::Fn(_) | TypeInfo::Unit | TypeInfo::Type | TypeInfo::Int(_) => LLVMInt64TypeInContext(self.context),
                TypeInfo::Bool => LLVMInt1TypeInContext(self.context),
                &TypeInfo::FnPtr(ty) => self.get_function_type(program, ty),
                &TypeInfo::Ptr(inner) => {
                    let inner = self.get_type(program, inner);
                    LLVMPointerType(inner, c_uint::from(0u16))
                }
                &TypeInfo::Struct { as_tuple, .. } => self.get_type(program, as_tuple),
                TypeInfo::Tuple(fields) => {
                    let mut fields: Vec<_> = fields.clone().iter().map(|ty| self.get_type(program, *ty)).collect();
                    LLVMStructType(fields.as_mut_ptr(), fields.len() as c_uint, LLVMBool::from(false))
                }
                TypeInfo::Enum { cases } => todo!(),
                &TypeInfo::Unique(ty, _) | &TypeInfo::Named(ty, _) => self.get_type(program, ty), // TOOD: carry forward struct names?
                TypeInfo::VoidPtr => LLVMPointerTypeInContext(self.context, c_uint::from(0u16)),
            }
        };
        self.types[ty.0] = Some(result);
        result
    }

    /// # Safety
    /// Invalidates jitted function pointers.
    pub unsafe fn release(&self) {
        LLVMDisposeBuilder(self.builder);
        LLVMDisposeExecutionEngine(self.execution_engine);
        // causes SEGSEV. Execution engine owns the module I guess?
        // LLVMDisposeModule(self.module);
        // LLVMDisposeModule(self.inline_asm_module);
        LLVMContextDispose(self.context);
    }
}

fn null_terminated(s: String) -> CString {
    debug_assert_ne!(s.bytes().last(), Some(0));
    let mut s = std::convert::Into::<Vec<u8>>::into(s);
    s.push(0); // why the fuck isnt this a function bro
    CString::from_vec_with_nul(s).unwrap()
}

pub struct BcToLlvm<'z, 'a, 'p> {
    program: &'z mut Program<'p>,
    interp: &'z mut Interp<'a, 'p>,
    llvm: JittedLlvm,
    f: FuncId,
    wip: Vec<FuncId>, // makes recursion work
    slots: Vec<Option<LLVMValueRef>>,
    blocks: HashMap<usize, LLVMBasicBlockRef>,
}

impl<'z, 'a, 'p> BcToLlvm<'z, 'a, 'p> {
    pub fn new(interp: &'z mut Interp<'a, 'p>, program: &'z mut Program<'p>) -> Self {
        Self {
            llvm: JittedLlvm::new("franca", program).unwrap(),
            program,
            interp,
            f: FuncId(0), // TODO: bad
            wip: vec![],
            slots: vec![],
            blocks: Default::default(),
        }
    }

    // TODO: i cant keep copy pasting this shape. gonna cry.
    pub fn compile(&mut self, f: FuncId) -> Res<'p, ()> {
        if self.llvm.get_fn_jitted(f).is_some() || self.wip.contains(&f) {
            return Ok(());
        }
        self.wip.push(f);

        if let Some(template) = self.program.funcs[f.0].any_reg_template {
        } else if self.program.funcs[f.0].comptime_addr.is_some() {
            // TODO
        } else {
            let callees = self.program.funcs[f.0].wip.as_ref().unwrap().callees.clone();
            for c in callees {
                self.compile(c)?;
            }

            let func = &self.program.funcs[f.0];
            if let Some(insts) = func.jitted_code.as_ref() {
                todo!()
            } else {
                self.bc_to_asm(f)?;
            }
            self.llvm.finish_fn(f);
        }

        self.wip.retain(|c| c != &f);
        Ok(())
    }

    #[track_caller]
    fn slot_type(&self, slot: StackOffset) -> TypeId {
        self.interp.ready[self.f.0].as_ref().unwrap().slot_types[slot.0]
    }

    // TODO: change name? make this whole thing a trait so i dont have to keep writing the shitty glue.
    fn bc_to_asm(&mut self, f: FuncId) -> Res<'p, ()> {
        unsafe {
            self.f = f;
            let ff = &self.program.funcs[f.0];
            let is_c_call = ff.has_tag(Flag::C_Call); // TODO
            let llvm_f = self.llvm.decl_function(self.program, f);
            let func = self.interp.ready[f.0].as_ref().unwrap();
            println!("{}", func.log(self.program.pool));
            self.slots.clear();
            self.slots.extend(vec![None; func.stack_slots]);
            self.blocks.clear();

            // TODO: iterate BitSet using the magic intrinsics.
            for b in 0..func.insts.len() {
                if func.jump_targets.get(b) {
                    let name = null_terminate(&format!(".IP{}", b));
                    self.blocks.insert(b, LLVMAppendBasicBlock(llvm_f, name.as_ptr()));
                }
            }

            LLVMPositionBuilderAtEnd(self.llvm.builder, *self.blocks.get(&0).unwrap());

            for i in 0..func.stack_slots {
                if func.slot_is_var.get(i) {
                    let name = null_terminate(&format!(".VAR{}", i));
                    let ty = self.llvm.get_type(self.program, func.slot_types[i]);
                    let ptr = LLVMBuildAlloca(self.llvm.builder, ty, name.as_ptr()); // TODO: sometimes tuples need consecutive addresses.
                    self.slots[i] = Some(ptr);
                }
            }

            let arg_range = func.arg_range;
            // if arg_range.count != 1 {
            //     todo!()
            // }
            for i in arg_range {
                // Note: SEGSEGV here means you told llvm the wrong type for llvm_f.
                let arg_index = (i - arg_range.first.0) as c_uint;
                let value = LLVMGetParam(llvm_f, arg_index);
                self.write_slot(StackOffset(i), value);
            }

            let func = self.interp.ready[f.0].as_ref().unwrap();
            let mut block_finished = true;
            for i in 0..func.insts.len() {
                let func = &self.interp.ready[f.0].as_ref().unwrap();
                if func.jump_targets.get(i) {
                    let block = *self.blocks.get(&(i)).unwrap();
                    // Fallthrough (false branch of an if)
                    if !block_finished {
                        LLVMBuildBr(self.llvm.builder, block);
                    }

                    LLVMPositionBuilderAtEnd(self.llvm.builder, block);
                    block_finished = false;
                }
                assert!(!block_finished);

                let inst = &(func.insts[i].clone());
                match inst {
                    Bc::NoCompile => unreachable!(),
                    Bc::CallDynamic { .. } => todo!(),
                    &Bc::CallDirect { f, ret, arg } => {
                        let ty = self.program.func_type(f);
                        let ty = self.program.fn_ty(ty).unwrap();
                        assert!(
                            !matches!(self.program.types[ty.arg.0], TypeInfo::Struct { .. }),
                            "TODO: do struct args get flattened like tuples? ffi? enums?"
                        );
                        let ty = self.func_type(f);
                        let function = self.llvm.get_fn(f).unwrap();
                        let mut args = arg.into_iter().map(|i| self.read_slot(StackOffset(i))).collect::<Vec<_>>();
                        let value = LLVMBuildCall2(self.llvm.builder, ty, function, args.as_mut_ptr(), args.len() as c_uint, EMPTY);
                        self.write_slot(ret.single(), value);
                    }
                    Bc::CallBuiltin { name, .. } => todo!("{}", self.program.pool.get(*name)),
                    &Bc::LoadConstant { slot, value } => {
                        let mut ty = self.llvm_type(slot);
                        let value = match value {
                            Value::I64(n) => LLVMConstInt(ty, u64::from_le_bytes(n.to_le_bytes()), LLVMBool::from(false)),
                            // Fn has to be int because the only time you have them is at comptime where the index is whats important.
                            Value::Type(TypeId(n)) | Value::OverloadSet(n) | Value::GetFn(FuncId(n)) | Value::Symbol(n) => {
                                LLVMConstInt(ty, n as u64, LLVMBool::from(false))
                            }
                            Value::GetNativeFnPtr(ff) => {
                                ty = self.func_type(ff);
                                unwrap!(self.llvm.get_fn(f), "GetNativeFnPtr on uncompiled {f:?}")
                            }
                            Value::Bool(b) => LLVMConstInt(ty, b as u64, LLVMBool::from(false)),
                            Value::Unit => LLVMConstInt(ty, 0, LLVMBool::from(false)),
                            Value::F64(_) => todo!(),
                            Value::Poison | Value::InterpAbsStackAddr(_) => err!("intrp constant in llvm",),
                            Value::Heap {
                                value,
                                physical_first,
                                physical_count,
                            } => todo!(),
                        };
                        self.write_slot(slot, value);
                    }
                    &Bc::JumpIf { cond, true_ip, false_ip } => {
                        let cond = self.read_slot(cond);
                        LLVMBuildCondBr(
                            self.llvm.builder,
                            cond,
                            *self.blocks.get(&true_ip).unwrap(),
                            *self.blocks.get(&false_ip).unwrap(),
                        );
                        block_finished = true;
                    }
                    &Bc::Goto { ip } => {
                        LLVMBuildBr(self.llvm.builder, *self.blocks.get(&ip).unwrap());
                        block_finished = true;
                    }
                    &Bc::Ret(slot) => {
                        if slot.count == 1 {
                            let slot = self.read_slot(slot.first);
                            LLVMBuildRet(self.llvm.builder, slot);
                        } else {
                            todo!()
                        }
                        block_finished = true;
                    }
                    Bc::LastUse(_) | Bc::Drop(_) | Bc::DebugMarker(_, _) | Bc::DebugLine(_) => {}
                    &Bc::Move { from, to } | &Bc::Clone { from, to } => {
                        let v = self.read_slot(from);
                        self.write_slot(to, v);
                    }
                    Bc::CloneRange { from, to } | Bc::MoveRange { from, to } => {
                        for i in 0..from.count {
                            let from = StackOffset(i + from.first.0);
                            let to = StackOffset(i + to.first.0);
                            let v = self.read_slot(from);
                            self.write_slot(to, v);
                        }
                    }
                    &Bc::AbsoluteStackAddr { of, to } => {
                        for slot in of {
                            assert!(self.slot_is_var(StackOffset(slot)));
                        }
                        if of.count == 1 {
                            let ptr = self.slots[of.first.0].unwrap();
                            self.write_slot(to, ptr);
                        } else {
                            todo!()
                        }
                    }
                    &Bc::SlicePtr { base, offset, ret, .. } => {
                        let ty = self.slot_type(base);
                        let ty = unwrap!(self.program.unptr_ty(ty), "not ptr");
                        assert!(matches!(self.program.types[ty.0], TypeInfo::Struct { .. }));
                        let ty = self.llvm.get_type(self.program, ty);
                        let ptr = self.read_slot(base);
                        // The first one is because you're going through the pointer.
                        let mut index_values = vec![
                            LLVMConstInt(self.llvm.i32_ty, 0, LLVMBool::from(false)),
                            LLVMConstInt(self.llvm.i32_ty, offset as u64, LLVMBool::from(false)),
                        ];
                        let field_ptr_value =
                            LLVMBuildGEP2(self.llvm.builder, ty, ptr, index_values.as_mut_ptr(), index_values.len() as c_uint, EMPTY);
                        self.write_slot(ret, field_ptr_value);
                    }
                    &Bc::Load { from, to } => {
                        let ty = self.llvm_type(to.single());
                        let ptr = self.read_slot(from);
                        let val = LLVMBuildLoad2(self.llvm.builder, ty, ptr, EMPTY);
                        self.write_slot(to.single(), val);
                    }
                    &Bc::Store { to, from } => {
                        let ptr = self.read_slot(to);
                        let val = self.read_slot(from.single());
                        LLVMBuildStore(self.llvm.builder, val, ptr);
                    }
                    &Bc::TagCheck { enum_ptr, value } => {
                        let ptr = self.read_slot(enum_ptr);
                        todo!()
                    }
                    &Bc::CallC { f, arg, ret, ty, comp_ctx } => {
                        let ty = self.llvm.get_function_type(self.program, ty);
                        todo!()
                    }
                }
            }

            Ok(())
        }
    }

    fn read_slot(&mut self, slot: StackOffset) -> LLVMValueRef {
        if self.slot_is_var(slot) {
            let ptr = self.slots[slot.0].unwrap();
            let ty = self.slot_type(slot);
            let ty = self.llvm.get_type(self.program, ty);
            unsafe { LLVMBuildLoad2(self.llvm.builder, ty, ptr, EMPTY) }
        } else {
            self.slots[slot.0].unwrap()
        }
    }

    fn write_slot(&mut self, slot: StackOffset, value: LLVMValueRef) {
        if self.slot_is_var(slot) {
            let ptr = self.slots[slot.0].unwrap();
            unsafe { LLVMBuildStore(self.llvm.builder, value, ptr) };
        } else {
            assert!(self.slots[slot.0].is_none(), "{slot:?}");
            self.slots[slot.0] = Some(value)
        }
    }

    fn slot_is_var(&self, slot: StackOffset) -> bool {
        self.interp.ready[self.f.0].as_ref().unwrap().slot_is_var.get(slot.0)
    }

    fn llvm_type(&mut self, slot: StackOffset) -> LLVMTypeRef {
        let ty = self.slot_type(slot);
        self.llvm.get_type(self.program, ty)
    }

    fn func_type(&mut self, ff: FuncId) -> LLVMTypeRef {
        let ff = self.program.func_type(ff);
        let ff = self.program.fn_ty(ff).unwrap();
        self.llvm.get_function_type(self.program, ff)
    }
}

pub fn null_terminate(bytes: &str) -> CString {
    let bytes: Vec<_> = Vec::from(bytes).into_iter().map(|b| NonZeroU8::new(b).unwrap()).collect();
    CString::from(bytes)
}

pub fn extend_options<T>(v: &mut Vec<Option<T>>, index: usize) {
    if v.len() > index {
        return;
    }

    let count = index - v.len() + 1;
    v.reserve(count);
    for _ in 0..count {
        v.push(None);
    }
}

#[allow(unused)]
mod tests {
    use crate::ast::{Flag, FuncId, SuperSimple, TargetArch};
    use crate::experiments::arena::Arena;
    use crate::experiments::emit_ir::EmitIr;
    use crate::experiments::tests::jit_test;
    use crate::{
        ast::{garbage_loc, Program},
        compiler::{Compile, ExecTime, Res},
        interp::Interp,
        logging::{err, ice, unwrap},
        make_toplevel,
        parse::Parser,
        pool::StringPool,
        scope::ResolveScope,
    };
    use codemap::CodeMap;
    use llvm_sys::analysis::{LLVMVerifierFailureAction, LLVMVerifyModule};
    use llvm_sys::core::{LLVMDisposeBuilder, LLVMDisposeMessage, LLVMPrintModuleToString};
    use llvm_sys::error::{LLVMCreateStringError, LLVMGetErrorMessage};
    use llvm_sys::execution_engine::{LLVMGetFunctionAddress, LLVMGetGlobalValueAddress, LLVMRunFunction};
    use std::ffi::CStr;
    use std::mem::MaybeUninit;
    use std::process::Command;
    use std::ptr::{addr_of, null};
    use std::{arch::asm, fs, mem::transmute};

    use super::{print_module, verify_module, BcToLlvm};

    // TODO: this is an ugly copy paste
    fn jit_main<Arg, Ret>(test_name: &str, src: &str, f: impl FnOnce(extern "C" fn(Arg) -> Ret)) -> Res<'static, ()> {
        let pool = Box::leak(Box::<StringPool>::default());
        let mut codemap = CodeMap::new();
        let file = codemap.add_file("main_file".to_string(), format!("#include_std(\"core.fr\");{src}"));
        let user_span = file.span;
        let stmts = Parser::parse(&mut codemap, file.clone(), pool).unwrap();
        let mut global = make_toplevel(pool, garbage_loc(), stmts);
        let vars = ResolveScope::of(&mut global, pool);
        let mut program = Program::new(vars, pool, TargetArch::Interp, TargetArch::Llvm);
        let mut comp = Compile::new(pool, &mut program, Interp::new(pool));
        comp.add_declarations(global, Flag::TopLevel.ident(), None)?;
        let main = unwrap!(comp.program.find_unique_func(Flag::Main.ident()), "");
        comp.compile(main, ExecTime::Runtime)?;

        let mut asm = BcToLlvm::new(&mut comp.executor, &mut program);

        asm.compile(main)?;

        if cfg!(feature = "dis_debug") {
            let debug_path = format!("target/latest_log/asm/{test_name}");
            fs::create_dir_all(&debug_path).unwrap();
            unsafe {
                print_module(asm.llvm.module, |dis| {
                    fs::write(format!("{debug_path}/program_llvm_ir.txt"), dis).unwrap()
                });
            }
        }

        verify_module(asm.llvm.module);

        let code = asm.llvm.get_fn_jitted(main).unwrap();
        let code: extern "C" fn(Arg) -> Ret = unsafe { transmute(code) };
        f(code);
        unsafe {
            asm.llvm.release();
        }
        Ok(())
    }

    jit_test!(jit_main);
}

fn verify_module<'p>(module: LLVMModuleRef) -> Res<'p, ()> {
    unsafe { checked_llvm(|msg| LLVMVerifyModule(module, LLVMVerifierFailureAction::LLVMPrintMessageAction, msg))? }
    Ok(())
}

/// # Safety
/// `f` must return non zero only if it has initilized its parameter to an llvm string.
unsafe fn checked_llvm<'p>(f: impl FnOnce(*mut *mut ::libc::c_char) -> LLVMBool) -> Res<'p, ()> {
    let mut err = MaybeUninit::uninit();
    let failed = f(err.as_mut_ptr());
    if failed != 0 {
        let err = MaybeUninit::assume_init(err);
        let msg = CStr::from_ptr(err).to_str().unwrap().to_string();
        LLVMDisposeMessage(err);
        err!("LLVM Error: {}", msg)
    }
    Ok(())
}

/// # Safety
/// `f` must uphold contract of `checked_llvm` and also return zero only if it has initilized its parameter.
unsafe fn return_ref<'p, T>(f: impl FnOnce(*mut T, *mut *mut ::libc::c_char) -> LLVMBool) -> Res<'p, T> {
    let mut out = MaybeUninit::uninit();
    checked_llvm(|err| f(out.as_mut_ptr(), err))?;
    Ok(out.assume_init())
}

/// # Safety
/// `module` must be valid.
unsafe fn print_module<T>(module: LLVMModuleRef, f: impl FnOnce(&str) -> T) -> T {
    let ir_str = LLVMPrintModuleToString(module);
    let dis = CStr::from_ptr(ir_str).to_str().unwrap(); // Note a copy, can't dispose before write.
    let out = f(dis);
    LLVMDisposeMessage(ir_str);
    out
}

const EMPTY: *const libc::c_char = &[0i8] as *const libc::c_char;
