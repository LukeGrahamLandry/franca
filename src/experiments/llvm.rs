#![allow(clippy::missing_safety_doc)]

use std::{
    ffi::{CStr, CString},
    mem::MaybeUninit,
    num::NonZeroU8,
};

use llvm_sys::{
    core::{LLVMContextCreate, LLVMContextDispose, LLVMDisposeMessage, LLVMDisposeModule, LLVMModuleCreateWithNameInContext},
    execution_engine::{LLVMCreateJITCompilerForModule, LLVMDisposeExecutionEngine, LLVMExecutionEngineRef, LLVMGetFunctionAddress, LLVMLinkInMCJIT},
    prelude::*,
    target::{LLVM_InitializeNativeAsmPrinter, LLVM_InitializeNativeTarget},
};

pub struct JittedLlvm {
    context: LLVMContextRef,
    module: LLVMModuleRef,
    execution_engine: LLVMExecutionEngineRef,
}

impl JittedLlvm {
    pub unsafe fn new(name: &str) -> JittedLlvm {
        let context = LLVMContextCreate();
        let name = null_terminate(name);
        let module = LLVMModuleCreateWithNameInContext(name.as_ptr(), context);

        let mut execution_engine = MaybeUninit::uninit();
        let mut err = MaybeUninit::uninit();

        // Fixes: Unable to find target for this triple (no targets are registered)
        assert_eq!(LLVM_InitializeNativeTarget(), 0);
        // Fixes: LLVM ERROR: Target does not support MC emission!
        assert_eq!(LLVM_InitializeNativeAsmPrinter(), 0);
        // Fixes: JIT has not been linked in.
        LLVMLinkInMCJIT();
        let failed = LLVMCreateJITCompilerForModule(execution_engine.as_mut_ptr(), module, 0, err.as_mut_ptr());

        if failed != 0 {
            let err = err.assume_init();
            let msg = CStr::from_ptr(err).to_str().unwrap().to_string();
            LLVMDisposeMessage(err);
            panic!("{}", msg);
        }

        let execution_engine = execution_engine.assume_init();

        Self {
            context,
            module,
            execution_engine,
        }
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

pub fn null_terminate(bytes: &str) -> CString {
    let bytes: Vec<_> = Vec::from(bytes).into_iter().map(|b| NonZeroU8::new(b).unwrap()).collect();
    CString::from(bytes)
}
