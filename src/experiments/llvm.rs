#![allow(clippy::missing_safety_doc)]
#![allow(unused)]

use std::{
    collections::HashMap,
    ffi::{c_uint, CStr, CString},
    mem::MaybeUninit,
    num::NonZeroU8,
};

use llvm_sys::{
    core::{
        LLVMAddFunction, LLVMAppendBasicBlock, LLVMBuildAlloca, LLVMBuildBr, LLVMBuildCall2, LLVMBuildCondBr, LLVMBuildLoad2, LLVMBuildRet,
        LLVMBuildStore, LLVMConstInt, LLVMContextCreate, LLVMContextDispose, LLVMCreateBuilderInContext, LLVMCreateMemoryBufferWithMemoryRange,
        LLVMDisposeMessage, LLVMDisposeModule, LLVMFunctionType, LLVMGetNamedGlobal, LLVMGetParam, LLVMInt1TypeInContext, LLVMInt64TypeInContext,
        LLVMModuleCreateWithNameInContext, LLVMPointerType, LLVMPointerTypeInContext, LLVMPositionBuilderAtEnd, LLVMSetTarget, LLVMStructType,
    },
    execution_engine::{LLVMCreateJITCompilerForModule, LLVMDisposeExecutionEngine, LLVMExecutionEngineRef, LLVMGetFunctionAddress, LLVMLinkInMCJIT},
    ir_reader::LLVMParseIRInContext,
    prelude::*,
    target::{LLVM_InitializeNativeAsmPrinter, LLVM_InitializeNativeTarget},
    target_machine::LLVMGetTargetMachineTarget,
    transforms::ipo::LLVMAddGlobalOptimizerPass,
};

use crate::{
    ast::{Flag, FuncId, Program, TypeId, TypeInfo},
    bc::{Bc, StackOffset, Value},
    compiler::Res,
    interp::Interp,
    logging::PoolLog,
    pool::Ident,
};

pub struct JittedLlvm {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    execution_engine: LLVMExecutionEngineRef,
    builder: LLVMBuilderRef,
    types: Vec<Option<LLVMTypeRef>>,
    functions: Vec<Option<(LLVMValueRef, bool)>>,
}

impl JittedLlvm {
    pub fn new(name: &str, program: &mut Program) -> JittedLlvm {
        unsafe {
            let context = LLVMContextCreate();
            let name = null_terminate(name);
            let mut llvm_ir = std::convert::Into::<Vec<u8>>::into(program.emit_inline_llvm_ir());
            let len = llvm_ir.len(); // TODO: include the null?
            llvm_ir.push(0); // why the fuck isnt this a function bro
            let llvm_ir = CString::from_vec_with_nul(llvm_ir).unwrap();

            let llvm_ir = LLVMCreateMemoryBufferWithMemoryRange(llvm_ir.as_ptr(), len, name.as_ptr(), LLVMBool::from(false));

            let mut err = MaybeUninit::uninit();
            let mut module = MaybeUninit::uninit();

            let failed = LLVMParseIRInContext(context, llvm_ir, module.as_mut_ptr(), err.as_mut_ptr());
            if failed != 0 {
                let err = err.assume_init();
                let msg = CStr::from_ptr(err).to_str().unwrap().to_string();
                LLVMDisposeMessage(err);
                panic!("{}", msg);
            }
            let module = module.assume_init();

            let mut execution_engine = MaybeUninit::uninit();

            // Fixes: Unable to find target for this triple (no targets are registered)
            assert_eq!(LLVM_InitializeNativeTarget(), 0);
            // Fixes: LLVM ERROR: Target does not support MC emission!
            assert_eq!(LLVM_InitializeNativeAsmPrinter(), 0);
            // Fixes: JIT has not been linked in.
            LLVMLinkInMCJIT();
            let failed = LLVMCreateJITCompilerForModule(execution_engine.as_mut_ptr(), module, 2, err.as_mut_ptr());

            if failed != 0 {
                // TODO: factor out
                let err = err.assume_init();
                let msg = CStr::from_ptr(err).to_str().unwrap().to_string();
                LLVMDisposeMessage(err);
                panic!("{}", msg);
            }

            Self {
                execution_engine: execution_engine.assume_init(),
                builder: LLVMCreateBuilderInContext(context),
                context,
                module,
                types: vec![],
                functions: vec![],
            }
        }
    }

    pub fn decl_function(&mut self, program: &mut Program, f: FuncId) -> LLVMValueRef {
        extend_options(&mut self.functions, f.0);
        if let Some((f, _)) = self.functions[f.0] {
            return f;
        }

        let name = null_terminate(&format!("FN{}", f.0));
        let ty = program.func_type(f);
        let ty = self.get_type(program, ty);
        let func = unsafe { LLVMAddFunction(self.module, name.as_ptr(), ty) };
        self.functions[f.0] = Some((func, false));
        func
    }

    pub fn get_fn(&mut self, f: FuncId) -> Option<LLVMValueRef> {
        extend_options(&mut self.functions, f.0);
        self.functions[f.0].and_then(|(f, ready)| if ready { Some(f) } else { None })
    }

    pub fn get_fn_jitted(&mut self, f: FuncId) -> Option<*const u8> {
        self.get_fn(f)?;
        let name = null_terminate(&format!("FN{}", f.0));
        unsafe { Some(self.get_named_fn_ptr(&name)) }
    }

    fn finish_fn(&mut self, f: FuncId) {
        self.functions[f.0].as_mut().unwrap().1 = true;
    }

    pub fn get_type(&mut self, program: &mut Program, ty: TypeId) -> LLVMTypeRef {
        extend_options(&mut self.types, ty.0);
        if let Some(ty) = self.types[ty.0] {
            return ty;
        }

        let result = unsafe {
            match &program.types[ty.0] {
                TypeInfo::Unknown | TypeInfo::Any | TypeInfo::Never | TypeInfo::F64 => todo!("llvm type: {}", program.log_type(ty)),
                // TODO: special case Unit but need different type for enum padding. for returns unit should be LLVMVoidTypeInContext(self.context)
                TypeInfo::Unit | TypeInfo::Type | TypeInfo::Int(_) => LLVMInt64TypeInContext(self.context),
                TypeInfo::Bool => LLVMInt1TypeInContext(self.context),
                &TypeInfo::Fn(ty) | &TypeInfo::FnPtr(ty) => {
                    let mut arg = vec![self.get_type(program, ty.arg)]; // TODO: represent tuple as multiple arguments.
                    let ret = self.get_type(program, ty.ret);
                    LLVMFunctionType(ret, arg.as_mut_ptr(), arg.len() as c_uint, LLVMBool::from(false))
                }
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

    pub unsafe fn get_named_fn_ptr(&self, func_name: &CStr) -> *const u8 {
        LLVMGetFunctionAddress(self.execution_engine, func_name.as_ptr()) as usize as *const u8
    }

    pub unsafe fn release(&self) {
        // this used to cause a segsev, idk
        LLVMDisposeExecutionEngine(self.execution_engine);
        LLVMDisposeModule(self.module);
        LLVMContextDispose(self.context);
    }
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
            llvm: JittedLlvm::new("franca", program),
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

    fn bc_to_asm(&mut self, f: FuncId) -> Res<'p, ()> {
        unsafe {
            self.f = f;
            let ff = &self.program.funcs[f.0];
            if ff.llvm_ir.is_some() {
                let name = null_terminate(&format!("FN{}", f.0));
                let value = LLVMGetNamedGlobal(self.llvm.module, name.as_ptr());
                self.llvm.functions[f.0] = Some((value, true));
                return Ok(());
            }
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
            if arg_range.count != 1 {
                todo!()
            }
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
                        let ty = self.llvm.get_type(self.program, ty);
                        let function = self.llvm.get_fn(f).unwrap();
                        let empty = CString::from(vec![]); // TODO: hoist
                        let mut args = arg.into_iter().map(|i| self.read_slot(StackOffset(i))).collect::<Vec<_>>();
                        let value = LLVMBuildCall2(self.llvm.builder, ty, function, args.as_mut_ptr(), args.len() as c_uint, empty.as_ptr());
                        self.write_slot(ret.single(), value);
                    }
                    Bc::CallBuiltin { name, .. } => todo!("{}", self.program.pool.get(*name)),
                    &Bc::LoadConstant { slot, value } => {
                        let ty = self.slot_type(slot);
                        let ty = self.llvm.get_type(self.program, ty);
                        let value = match value {
                            Value::I64(n) => LLVMConstInt(ty, u64::from_le_bytes(n.to_le_bytes()), LLVMBool::from(false)),
                            // TODO: type if Fn has to be int because the only time you have them is at comptime where the index is whats important. so need seperate typeof for fn
                            Value::GetFn(FuncId(n)) | Value::Symbol(n) => LLVMConstInt(ty, n as u64, LLVMBool::from(false)),
                            Value::Unit => LLVMConstInt(ty, 0, LLVMBool::from(false)), //TODO
                            _ => todo!(),
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
                        todo!()
                    }
                    &Bc::Load { from, to } => {
                        let ptr = self.read_slot(from);
                        todo!()
                    }
                    &Bc::Store { to, from } => {
                        let ptr = self.read_slot(to);
                        todo!()
                    }
                    &Bc::TagCheck { enum_ptr, value } => {
                        let ptr = self.read_slot(enum_ptr);
                        todo!()
                    }
                    Bc::CallC { f, arg, ret, ty, comp_ctx } => {
                        todo!()
                    }
                }
            }

            Ok(())
        }
    }

    fn read_slot(&mut self, slot: StackOffset) -> LLVMValueRef {
        if self.slot_is_var(slot) {
            let empty = CString::from(vec![]); // TODO: hoist
            let ptr = self.slots[slot.0].unwrap();
            let ty = self.slot_type(slot);
            let ty = self.llvm.get_type(self.program, ty);
            unsafe { LLVMBuildLoad2(self.llvm.builder, ty, ptr, empty.as_ptr()) }
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
    use crate::ast::{Flag, SuperSimple, TargetArch};
    use crate::experiments::arena::Arena;
    use crate::experiments::emit_ir::EmitIr;
    use crate::experiments::tests::jit_test;
    use crate::export_ffi::get_special_functions;
    use crate::{
        ast::{garbage_loc, Program},
        compiler::{Compile, ExecTime, Res},
        interp::Interp,
        logging::{err, ice, unwrap},
        make_toplevel,
        parse::Parser,
        pool::StringPool,
        scope::ResolveScope,
        LIB,
    };
    use codemap::CodeMap;
    use llvm_sys::analysis::{LLVMVerifierFailureAction, LLVMVerifyModule};
    use llvm_sys::core::{LLVMDisposeMessage, LLVMPrintModuleToString};
    use llvm_sys::execution_engine::LLVMGetFunctionAddress;
    use std::ffi::CStr;
    use std::mem::MaybeUninit;
    use std::process::Command;
    use std::ptr::addr_of;
    use std::{arch::asm, fs, mem::transmute};

    use super::BcToLlvm;

    // TODO: this is an ugly copy paste
    fn jit_main<Arg, Ret>(test_name: &str, src: &str, f: impl FnOnce(extern "C" fn(Arg) -> Ret)) -> Res<'static, ()> {
        let pool = Box::leak(Box::<StringPool>::default());
        let mut codemap = CodeMap::new();
        let mut stmts = vec![];
        let mut libs: Vec<_> = LIB
            .iter()
            .map(|(name, code)| codemap.add_file(name.to_string(), code.to_string()))
            .collect();
        libs.insert(3, codemap.add_file("special".into(), get_special_functions())); // TODO: order independent name resolution
        libs.push(codemap.add_file("main_file".into(), src.to_string()));
        for l in libs {
            stmts.extend(Parser::parse(l, pool).unwrap());
        }
        let mut global = make_toplevel(pool, garbage_loc(), stmts);
        let vars = ResolveScope::of(&mut global, pool);
        let mut program = Program::new(vars, pool, TargetArch::Interp, TargetArch::Llvm);
        let mut comp = Compile::new(pool, &mut program, Interp::new(pool));
        comp.add_declarations(global, Flag::TopLevel.ident(), None)?;
        let main = unwrap!(comp.lookup_unique_func(Flag::Main.ident()), "");
        comp.compile(main, ExecTime::Runtime)?;

        let mut asm = BcToLlvm::new(&mut comp.executor, &mut program);

        asm.compile(main)?;

        if cfg!(feature = "dis_debug") {
            let debug_path = format!("target/latest_log/asm/{test_name}");
            fs::create_dir_all(&debug_path).unwrap();
            unsafe {
                let ir_str = LLVMPrintModuleToString(asm.llvm.module);
                let dis = CStr::from_ptr(ir_str).to_str().unwrap(); // Note a copy, can't dispose before write.
                fs::write(format!("{debug_path}/llvm_ir.txt"), dis).unwrap();
                LLVMDisposeMessage(ir_str);
            }
        }

        unsafe {
            let mut msg = MaybeUninit::uninit();
            let failed = LLVMVerifyModule(asm.llvm.module, LLVMVerifierFailureAction::LLVMPrintMessageAction, msg.as_mut_ptr());
            if failed != 0 {
                let msg = msg.assume_init();
                let msg_str = CStr::from_ptr(msg).to_str().unwrap(); // Note a copy, can't dispose before write.
                panic!("Failed llvm verify! \n{}.", msg_str);
                LLVMDisposeMessage(msg);
            }
        }

        let code = asm.llvm.get_fn_jitted(main).unwrap();
        let code: extern "C" fn(Arg) -> Ret = unsafe { transmute(code) };
        f(code);
        Ok(())
    }

    jit_test!(jit_main);
}
