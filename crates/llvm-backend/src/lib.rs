#![allow(clippy::missing_safety_doc)]
#![allow(unused)]

use std::{
    collections::HashMap,
    ffi::{c_uint, CStr, CString},
    mem::MaybeUninit,
    num::NonZeroU8,
    ptr::{null, null_mut},
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
    LLVMAttributeFunctionIndex, LLVMAttributeIndex,
};

use compiler::{
    ast::{Flag, FnType, FuncId, Program, TypeId, TypeInfo},
    bc::{Bc, BcReady, StackOffset, Value},
    bc_to_asm::ConstBytes,
    compiler::{Compile, ExecTime, Res},
    err, extend_options,
    logging::PoolLog,
    pool::Ident,
    reflect::BitSet,
    unwrap,
};

pub struct JittedLlvm {
    context: LLVMContextRef,
    pub module: LLVMModuleRef,
    inline_asm_module: LLVMModuleRef,
    execution_engine: LLVMExecutionEngineRef,
    builder: LLVMBuilderRef,
    types: Vec<Option<LLVMTypeRef>>,
    functions: Vec<Option<(LLVMValueRef, CString, bool)>>,
    i32_ty: LLVMTypeRef,
    ptr_ty: *mut llvm_sys::LLVMType,
    constants: ConstBytes,
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
                ptr_ty: LLVMPointerTypeInContext(context, c_uint::from(0u16)),
                constants: ConstBytes::default(),
            };

            for f in program.inline_llvm_ir.clone() {
                let name = null_terminate(&format!("FN{}", f.as_index()));
                let ty = program.func_type(f);
                let ty = program.fn_ty(ty).unwrap();
                let ty = this.get_function_type(program, ty, false);
                // Note: not LLVMGetNamedFunction on inline_asm_module! we're declaring that we plan on importing into the normal module.
                let value = unsafe { LLVMAddFunction(this.module, name.as_ptr(), ty) };
                extend_options(&mut this.functions, f.as_index());
                this.functions[f.as_index()] = Some((value, name, true));
            }

            Ok(this)
        }
    }

    fn decl_function(&mut self, program: &mut Program, f: FuncId) -> LLVMValueRef {
        extend_options(&mut self.functions, f.as_index());
        if let Some((f, _, _)) = self.functions[f.as_index()] {
            return f;
        }

        let target = &program[f];
        let comp_ctx = target.has_tag(Flag::Ct);
        let name = null_terminate(&format!("FN{}", f.as_index()));
        let ty = program.func_type(f);
        let ty = program.fn_ty(ty).unwrap();
        let noreturn = ty.ret.is_never(); // TODO: Unique$Never?
        let ty = self.get_function_type(program, ty, comp_ctx);
        unsafe {
            let func = unsafe { LLVMAddFunction(self.module, name.as_ptr(), ty) };
            if noreturn {
                // LLVMCreateStringAttribute(C, K, KLength, V, VLength)
                // LLVMAddAttributeAtIndex(func, LLVMAttributeFunctionIndex, A);
            }

            assert_ne!(func as usize, 0);
            self.functions[f.as_index()] = Some((func, name, false));
            func
        }
    }

    fn get_fn(&mut self, f: FuncId) -> Option<LLVMValueRef> {
        extend_options(&mut self.functions, f.as_index());
        self.functions[f.as_index()].as_ref().and_then(|(f, _, ready)| {
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
        let name = self.functions[f.as_index()].as_ref().unwrap().1.as_ptr();
        let ptr = unsafe { LLVMGetFunctionAddress(self.execution_engine, name) as usize as *const u8 };
        if ptr.is_null() {
            return None; // TODO: does get_fn ever lie?
        }
        assert_ne!(ptr as usize, 0, "Null fn {f:?}");
        Some(ptr)
    }

    fn finish_fn(&mut self, f: FuncId) {
        let func = self.functions[f.as_index()].as_mut().unwrap();
        func.2 = true;

        // unsafe {
        //     // TODO: where does message go?? makes this a bit useless since module verify actually gives me the err
        //     let failed = LLVMVerifyFunction(func.0, LLVMVerifierFailureAction::LLVMPrintMessageAction);
        //     if failed != 0 {
        //         panic!("Failed llvm verify! {f:?}.");
        //     }
        // }
    }

    /// This is more complicated than it sounds
    /// - Don't call this for comptime Fn(_) because you want to be able to pass around the indexes when talking to the compiler.
    /// - Tuples get pulled up to be represented as multiple arguments.
    ///   TODO: its weird that structs are treated differently but you need to be able to do whatever c does for ffi.
    ///         should have a @repr(C) so you can opt in. but then also some huristic for when to not bother trying to represent big tuples in registers?
    ///         or its fine, just spill them like you would if someone manually wrote that function, just seems like extra regalloc work.
    fn get_function_type(&mut self, program: &mut Program, ty: FnType, comp_ctx: bool) -> LLVMTypeRef {
        let raw_arg = program.raw_type(ty.arg);
        let mut arg = if let TypeInfo::Tuple(fields) = &program[raw_arg] {
            fields.clone().iter().map(|ty| self.get_type(program, *ty)).collect()
        } else {
            vec![self.get_type(program, ty.arg)]
        };
        if comp_ctx {
            arg.insert(0, self.ptr_ty); // TODO: cri
        }
        let noreturn = ty.ret.is_never();
        let ret = self.get_type(program, ty.ret);
        unsafe { LLVMFunctionType(ret, arg.as_mut_ptr(), arg.len() as c_uint, LLVMBool::from(false)) }
    }

    fn get_type(&mut self, program: &mut Program, ty: TypeId) -> LLVMTypeRef {
        extend_options(&mut self.types, ty.as_index());
        if let Some(ty) = self.types[ty.as_index()] {
            return ty;
        }

        let result = unsafe {
            match &program[ty] {
                TypeInfo::F64 => LLVMDoubleTypeInContext(self.context),
                TypeInfo::Unknown | TypeInfo::Any => todo!("llvm type: {}", program.log_type(ty)),
                // TODO: special case Unit but need different type for enum padding. for returns unit should be LLVMVoidTypeInContext(self.context)
                TypeInfo::OverloadSet | TypeInfo::Fn(_) | TypeInfo::Never | TypeInfo::Unit | TypeInfo::Type | TypeInfo::Int(_) => {
                    LLVMInt64TypeInContext(self.context)
                }
                TypeInfo::Bool => LLVMInt1TypeInContext(self.context),
                &TypeInfo::FnPtr(ty) => self.get_function_type(program, ty, false),
                &TypeInfo::Struct { as_tuple, .. } => self.get_type(program, as_tuple),
                TypeInfo::Tuple(fields) => {
                    let mut fields: Vec<_> = fields.clone().iter().map(|ty| self.get_type(program, *ty)).collect();
                    LLVMStructType(fields.as_mut_ptr(), fields.len() as c_uint, LLVMBool::from(false))
                }
                TypeInfo::Enum { cases } => todo!(),
                TypeInfo::Scope => todo!(),
                &TypeInfo::Unique(ty, _) | &TypeInfo::Named(ty, _) => self.get_type(program, ty), // TOOD: carry forward struct names?
                // They want to get rid of non-opaque pointers anyway: https://llvm.org/docs/OpaquePointers.html
                TypeInfo::Ptr(_) | TypeInfo::VoidPtr => self.ptr_ty,
            }
        };
        self.types[ty.as_index()] = Some(result);
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

    fn const_ptr(&self, ptr: *const u8) -> *mut llvm_sys::LLVMValue {
        unsafe {
            debug_assert!(!ptr.is_null());
            let ptr = LLVMConstInt(LLVMInt64TypeInContext(self.context), ptr as u64, LLVMBool::from(false));
            LLVMConstIntToPtr(ptr, self.ptr_ty)
        }
    }
}

fn null_terminated(s: String) -> CString {
    debug_assert_ne!(s.bytes().last(), Some(0));
    let mut s = std::convert::Into::<Vec<u8>>::into(s);
    s.push(0); // why the fuck isnt this a function bro
    CString::from_vec_with_nul(s).unwrap()
}

pub struct BcToLlvm<'z, 'p, 'a> {
    pub compile: &'z mut Compile<'a, 'p>,
    pub llvm: JittedLlvm,
    f: FuncId,
    wip: Vec<FuncId>, // makes recursion work
    slots: Vec<Option<LLVMValueRef>>,
    val_is_ptr: BitSet,
    blocks: HashMap<usize, LLVMBasicBlockRef>,
}

impl<'z, 'p, 'a> BcToLlvm<'z, 'p, 'a> {
    pub fn new(compile: &'z mut Compile<'a, 'p>) -> Self {
        Self {
            llvm: JittedLlvm::new("franca", compile.program).unwrap(),
            compile,
            f: FuncId::from_index(0), // TODO: bad
            wip: vec![],
            slots: vec![],
            blocks: Default::default(),
            val_is_ptr: BitSet::empty(),
        }
    }

    // TODO: i cant keep copy pasting this shape. gonna cry.
    pub fn compile(&mut self, f: FuncId) -> Res<'p, ()> {
        if self.llvm.get_fn_jitted(f).is_some() || self.wip.contains(&f) {
            return Ok(());
        }
        self.wip.push(f);

        println!(
            "{f:?} {} {}",
            self.compile.pool.get(self.compile.program[f].name),
            self.compile.program[f].has_tag(Flag::Ct)
        );

        if let Some(template) = self.compile.program[f].any_reg_template {
            todo!()
        } else if let Some(addr) = self.compile.program[f].comptime_addr {
            // TODO: shouldn't need this since i change at the callsite anyway
            let ptr = self.llvm.const_ptr(addr as *const u8);
            self.llvm.functions[f.as_index()] = Some((ptr, CString::new(String::new()).unwrap(), true));
        } else {
            let callees = self.compile.program[f].wip.as_ref().unwrap().callees.clone();
            for (c, when) in callees {
                // TODO: this will change
                if when != ExecTime::Comptime {
                    self.compile(c)?;
                }
            }

            let func = &self.compile.program[f];
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
        let ty = self.compile.ready[self.f].as_ref().unwrap().slot_types[slot.0];
        self.compile.program.raw_type(ty)
    }

    // TODO: change name? make this whole thing a trait so i dont have to keep writing the shitty glue.
    fn bc_to_asm(&mut self, f: FuncId) -> Res<'p, ()> {
        unsafe {
            self.f = f;
            let ff = &self.compile.program[f];
            let is_c_call = ff.has_tag(Flag::C_Call); // TODO
            let is_flat_call = ff.has_tag(Flag::Flat_Call); // TODO
                                                            // debug_assert!(!is_flat_call);
            let llvm_f = self.llvm.decl_function(self.compile.program, f);
            let func = self.compile.ready[f].as_ref().unwrap();
            // println!("{}", func.log(self.compile.program.pool));
            self.slots.clear();
            self.slots.extend(vec![None; func.stack_slots]);
            self.blocks.clear();
            self.val_is_ptr = func.slot_is_var.clone();

            // TODO: iterate BitSet using the magic intrinsics.
            for b in 0..func.insts.len() {
                if func.jump_targets.get(b) {
                    let name = null_terminate(&format!(".IP{}", b));
                    self.blocks.insert(b, LLVMAppendBasicBlock(llvm_f, name.as_ptr()));
                }
            }
            LLVMPositionBuilderAtEnd(self.llvm.builder, *self.blocks.get(&0).unwrap());
            let int = LLVMInt64TypeInContext(self.llvm.context);
            for b in 0..func.insts.len() {
                if let Bc::MarkContiguous(slot, ty) = func.insts[b] {
                    let name = null_terminate(&format!(".ALLOC{}", slot.first.0));
                    // Not using the 'ty' from bc because I'm not consistant about representing structs.
                    // GEP uses multiple indexes for nesting instead of flattening them.
                    let ty = LLVMArrayType(int, slot.count as u32);
                    let ptr = LLVMBuildAlloca(self.llvm.builder, ty, name.as_ptr());
                    for i in slot {
                        let offset = i - slot.first.0;
                        // The first one is because you're going through the pointer.
                        let mut index_values = vec![
                            LLVMConstInt(self.llvm.i32_ty, 0, LLVMBool::from(false)),
                            LLVMConstInt(self.llvm.i32_ty, offset as u64, LLVMBool::from(false)),
                        ];
                        // https://llvm.org/docs/GetElementPtr.html
                        let field_ptr_value =
                            LLVMBuildInBoundsGEP2(self.llvm.builder, ty, ptr, index_values.as_mut_ptr(), index_values.len() as c_uint, EMPTY);
                        self.slots[i] = Some(field_ptr_value);
                        self.val_is_ptr.set(i);
                    }
                }
            }

            for i in 0..func.stack_slots {
                if func.slot_is_var.get(i) && self.slots[i].is_none() {
                    let name = null_terminate(&format!(".VAR{}", i));
                    let ty = self.llvm.get_type(self.compile.program, func.slot_types[i]);
                    let ptr = LLVMBuildAlloca(self.llvm.builder, ty, name.as_ptr());
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

            let func = self.compile.ready[f].as_ref().unwrap();
            let mut block_finished = true;
            let mut dead_code = false; // HACK to deal with my weird 'unreachable'
            for i in 0..func.insts.len() {
                let func = &self.compile.ready[f].as_ref().unwrap();
                if func.jump_targets.get(i) {
                    dead_code = false;
                    let block = *self.blocks.get(&(i)).unwrap();
                    // Fallthrough (false branch of an if)
                    if !block_finished {
                        LLVMBuildBr(self.llvm.builder, block);
                    }

                    LLVMPositionBuilderAtEnd(self.llvm.builder, block);
                    block_finished = false;
                }
                if dead_code {
                    continue; // HACK
                }
                assert!(!block_finished, "{i}");

                let inst = &(func.insts[i].clone());
                match inst {
                    Bc::MarkContiguous(_, _) => {}
                    Bc::NoCompile => unreachable!(),
                    Bc::Unreachable => {
                        LLVMBuildUnreachable(self.llvm.builder);
                        block_finished = true;
                        // TODO: I guess my dumb 'comptime if' eliminating the codegen but still putting out the branch,
                        //       still sometimes puts out the branch over else even tho its unreachable so llvm (rightly) complains about multiple terminators.
                        dead_code = true;
                    }
                    &Bc::CallDirect { f, ret, arg } => {
                        self.call_direct(f, ret, arg)?;
                    }
                    &Bc::CallSplit { rt, ret, arg, .. } => {
                        // TODO: update this when we support comptime here.
                        self.call_direct(rt, ret, arg)?;
                    }
                    &Bc::LoadConstant { slot, value } => {
                        let ty = self.slot_type(slot);
                        let is_ptr = matches!(self.compile.program[ty], TypeInfo::Ptr(_));
                        let mut ty = self.llvm_type(slot);
                        let value = match value {
                            Value::SplitFunc { ct, rt } => todo!(),
                            Value::I64(n) => {
                                // TODO: this fixes calling comptime_addr fns who were in as const ints. ir says null when i told it a function type maybe? need to figure this out
                                ty = LLVMInt64TypeInContext(self.llvm.context); // HACK
                                let val = LLVMConstInt(ty, u64::from_le_bytes(n.to_le_bytes()), LLVMBool::from(false));
                                if is_ptr {
                                    LLVMBuildIntToPtr(self.llvm.builder, val, self.llvm.ptr_ty, EMPTY)
                                } else {
                                    val
                                }
                            }
                            // Fn has to be int because the only time you have them is at comptime where the index is whats important.
                            Value::OverloadSet(n) => LLVMConstInt(ty, n as u64, LLVMBool::from(false)),
                            Value::Type(n) => LLVMConstInt(ty, n.as_raw() as u64, LLVMBool::from(false)),

                            Value::GetFn(n) => LLVMConstInt(ty, n.as_raw() as u64, LLVMBool::from(false)),
                            Value::Symbol(n) => LLVMConstInt(ty, n as u64, LLVMBool::from(false)),
                            Value::GetNativeFnPtr(ff) => {
                                ty = self.func_type(ff);
                                unwrap!(self.llvm.get_fn(f), "GetNativeFnPtr on uncompiled {f:?}")
                            }
                            Value::Bool(b) => LLVMConstInt(ty, b as u64, LLVMBool::from(false)),
                            Value::Unit => LLVMConstInt(ty, 0, LLVMBool::from(false)),
                            Value::F64(bits) => {
                                let f = f64::from_bits(bits);
                                LLVMConstReal(LLVMDoubleTypeInContext(self.llvm.context), f)
                            }
                            Value::Heap {
                                value,
                                physical_first,
                                physical_count,
                            } => {
                                // TODO: this needs to be different when I actually want to emit an executable.
                                let ptr = self.llvm.constants.copy_heap(value, physical_first, physical_count);
                                self.llvm.const_ptr(ptr)
                            }
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
                    Bc::LastUse(_) | Bc::DebugMarker(_, _) | Bc::DebugLine(_) => {}
                    &Bc::Clone { from, to } => {
                        let v = self.read_slot(from);
                        self.write_slot(to, v);
                    }
                    Bc::CloneRange { from, to } => {
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
                        // TODO: for structs, this would be the GEP of the first field. do i need to tell it it means the whole thing somehow?
                        let ptr = self.slots[of.first.0].unwrap();

                        self.write_slot(to, ptr);
                    }
                    &Bc::SlicePtr { base, offset, ret, .. } => {
                        let ty = self.slot_type(base);
                        let ty = unwrap!(self.compile.program.unptr_ty(ty), "not ptr");
                        let size = self.compile.ready.sizes.slot_count(self.compile.program, ty);
                        debug_assert!(offset <= size);
                        let ty = LLVMArrayType(int, size as u32);
                        let ptr = self.read_slot(base);
                        // The first one is because you're going through the pointer.
                        let mut index_values = vec![
                            LLVMConstInt(self.llvm.i32_ty, 0, LLVMBool::from(false)),
                            LLVMConstInt(self.llvm.i32_ty, offset as u64, LLVMBool::from(false)),
                        ];
                        // https://llvm.org/docs/GetElementPtr.html
                        let field_ptr_value =
                            LLVMBuildInBoundsGEP2(self.llvm.builder, ty, ptr, index_values.as_mut_ptr(), index_values.len() as c_uint, EMPTY);
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
                        let int = LLVMInt64TypeInContext(self.llvm.context);
                        let tag_val = LLVMBuildLoad2(self.llvm.builder, int, ptr, EMPTY);
                        let expect_val = LLVMConstInt(int, value as u64, 0);

                        let name = null_terminate(&format!(".CHECK{}", i));
                        let pass = LLVMAppendBasicBlock(llvm_f, name.as_ptr());
                        let name = null_terminate(&format!(".FAIL{}", i));
                        let fail = LLVMAppendBasicBlock(llvm_f, name.as_ptr());
                        let cond = LLVMBuildICmp(self.llvm.builder, llvm_sys::LLVMIntPredicate::LLVMIntEQ, tag_val, expect_val, EMPTY);
                        LLVMBuildCondBr(self.llvm.builder, cond, pass, fail);

                        LLVMPositionBuilderAtEnd(self.llvm.builder, fail);

                        // TODO: !!!! need to actually crash. i think unreachable is just UB !!!!
                        LLVMBuildUnreachable(self.llvm.builder);

                        LLVMPositionBuilderAtEnd(self.llvm.builder, pass);
                    }
                    &Bc::CallFnPtr { f, arg, ret, ty, comp_ctx } => {
                        let ty = self.llvm.get_function_type(self.compile.program, ty, comp_ctx);
                        let f = self.read_slot(f);
                        let f = LLVMBuildIntToPtr(self.llvm.builder, f, self.llvm.ptr_ty, EMPTY);
                        self.do_call(f, ty, arg, ret, comp_ctx);
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
            let ty = self.llvm.get_type(self.compile.program, ty);
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
        self.val_is_ptr.get(slot.0)
    }

    fn llvm_type(&mut self, slot: StackOffset) -> LLVMTypeRef {
        let ty = self.slot_type(slot);
        self.llvm.get_type(self.compile.program, ty)
    }

    fn func_type(&mut self, ff: FuncId) -> LLVMTypeRef {
        let target = &self.compile.program[ff];
        let comp_ctx = target.has_tag(Flag::Ct);

        let ff = self.compile.program.func_type(ff);
        let ff = self.compile.program.fn_ty(ff).unwrap();
        self.llvm.get_function_type(self.compile.program, ff, comp_ctx)
    }

    fn do_call(
        &mut self,
        function: *mut llvm_sys::LLVMValue,
        ty: *mut llvm_sys::LLVMType,
        arg: compiler::bc::StackRange,
        ret: compiler::bc::StackRange,
        comp_ctx: bool,
    ) {
        unsafe {
            debug_assert!(LLVMIsNull(function) == 0);
            let mut args = arg.into_iter().map(|i| self.read_slot(StackOffset(i))).collect::<Vec<_>>();
            if comp_ctx {
                let ptr = self.llvm.const_ptr(self.compile as *mut Compile as usize as *const u8); // TODO: this has gotta be UB
                args.insert(0, ptr); // TODO: cri
            }
            let value = LLVMBuildCall2(self.llvm.builder, ty, function, args.as_mut_ptr(), args.len() as c_uint, EMPTY);
            self.write_slot(ret.single(), value);
        }
    }

    fn call_direct(&mut self, f: FuncId, ret: compiler::bc::StackRange, arg: compiler::bc::StackRange) -> Res<'p, ()> {
        let ty = self.compile.program.func_type(f);
        let ty = self.compile.program.fn_ty(ty).unwrap();
        assert!(
            !matches!(self.compile.program[ty.arg], TypeInfo::Struct { .. }),
            "TODO: do struct args get flattened like tuples? ffi? enums?"
        );
        let ty = self.func_type(f);

        let target = &self.compile.program[f];
        let comp_ctx = target.has_tag(Flag::Ct);
        // TODO: this has to change for actually emitting
        let function = if let Some(addr) = target.comptime_addr {
            debug_assert_eq!(addr % 4, 0);
            // TODO: do i need some sort of ptr to function cast?
            self.llvm.const_ptr(addr as *const u8)
        } else {
            self.llvm.get_fn(f).unwrap()
        };
        self.do_call(function, ty, arg, ret, comp_ctx);
        Ok(())
    }
}

pub fn null_terminate(bytes: &str) -> CString {
    let bytes: Vec<_> = Vec::from(bytes).into_iter().map(|b| NonZeroU8::new(b).unwrap()).collect();
    CString::from(bytes)
}

#[allow(clippy::not_unsafe_ptr_arg_deref)]
pub fn verify_module<'p>(module: LLVMModuleRef) -> Res<'p, ()> {
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
