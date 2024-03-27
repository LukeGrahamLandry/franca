#![feature(allocator_api)]
#![feature(const_refs_to_cell)]
#![feature(ptr_metadata)]
#![feature(iter_array_chunks)]
#![feature(vec_into_raw_parts)]
#![feature(slice_ptr_len)]
#![feature(slice_ptr_get)]
#![feature(closure_track_caller)]
// bro if you can tell you could compile it more efficiently why don't you just compile it more efficiently
#![allow(clippy::format_collect)]
#![feature(pointer_is_aligned)]
extern crate core;

use std::mem::ManuallyDrop;
use std::{fs, sync::atomic::Ordering};

use ast::FuncId;
use bc::Value;
use codemap::{CodeMap, Span};
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle};
use compiler::{BoxedExec, CErr, Res};
use pool::StringPool;

macro_rules! mut_replace {
    ($value:expr, $f:expr) => {{
        let temp = mem::take(&mut $value);
        #[allow(clippy::redundant_closure_call)]
        let (temp, out) = $f(temp)?;
        $value = temp;
        out
    }};
}

pub mod ast;
pub mod bc;
pub mod compiler;
pub mod emit_bc;
pub mod experiments;
pub mod export_ffi;
pub mod ffi;
pub mod interp;
pub mod lex;
pub mod logging;
#[cfg(feature = "lsp")]
pub mod lsp;
pub mod overloading;
pub mod parse;
pub mod pool;
pub mod scope;

use crate::{
    ast::{Expr, FatExpr, FatStmt, Flag, Func, Program, TargetArch, TypeId, EXPR_COUNT},
    compiler::{Compile, CompileError, ExecTime},
    logging::{
        get_logs, log_tag_info, save_logs,
        LogTag::{ShowErr, *},
    },
    parse::Parser,
    scope::ResolveScope,
};

macro_rules! test_file {
    ($case:ident) => {
        #[test]
        fn $case() {
            crate::logging::init_logs(&[ShowErr]);

            let pool = Box::leak(Box::<StringPool>::default());

            let res = run_main(
                pool,
                fs::read_to_string(format!("../../tests/{}.fr", stringify!($case))).unwrap(),
                Value::I64(3145192),
                Value::I64(3145192),
                Some(&stringify!($case)),
                Box::new(crate::interp::Interp::new(pool)),
                true,
                Box::new(crate::interp::Interp::new(pool)),
            );
            if !res {
                panic!("Test Failed")
            }
        }
    };
}

test_file!(basic);
test_file!(structs);
test_file!(generics);
test_file!(overloading);
test_file!(closures);
test_file!(ffi);
test_file!(collections);
test_file!(macros);
test_file!(aarch64_jit);
test_file!(backpassing);
test_file!(dispatch);
test_file!(modules);

pub fn load_program<'p>(comp: &mut Compile<'_, 'p>, src: &str) -> Res<'p, FuncId> {
    // TODO: this will get less dumb when I have first class modules.
    let file = comp
        .program
        .codemap
        .add_file("main_file".to_string(), format!("#include_std(\"prelude.fr\");\n{src}"));
    let user_span = file.span;
    let parsed = match Parser::parse(&mut comp.program.codemap, file.clone(), comp.pool) {
        Ok(s) => s,
        Err(e) => {
            return Err(CompileError {
                internal_loc: e.loc,
                loc: Some(e.diagnostic[0].spans[0].span),
                reason: CErr::Diagnostic(e.diagnostic),
                trace: String::new(),
                value_stack: vec![],
                call_stack: String::new(),
            })
        }
    };

    let mut global = make_toplevel(comp.pool, user_span, parsed.stmts);
    let current = comp.add_module(Flag::TopLevel.ident(), None)?;
    global.module = Some(current);
    ResolveScope::of(&mut global, comp, parsed.directives)?;
    comp.compile_module(global)
}

// If it's just a cli that's going to immediately exit, you can set leak=true and not bother walking the tree to free everything at the end.
// I should really just use arenas for everything.
#[allow(clippy::too_many_arguments)]
pub fn run_main<'a: 'p, 'p>(
    pool: &'a StringPool<'p>,
    src: String,
    arg: Value,
    expect: Value,
    save: Option<&str>,
    runtime_executor: BoxedExec<'a>,
    leak: bool,
    comptime_executor: BoxedExec<'a>,
) -> bool {
    log_tag_info();
    // let start = timestamp();
    let mut program = Program::new(pool, TargetArch::Interp, TargetArch::Interp);
    let mut comp = Compile::new(pool, &mut program, runtime_executor, comptime_executor);
    let result = load_program(&mut comp, &src);

    // damn turns out defer would maybe be a good idea

    match result {
        Err(e) => {
            log_err(&comp, e, save);
            return false;
        }
        Ok(_toplevel) => {
            match comp.program.find_unique_func(Flag::Main.ident()) {
                None => {
                    outln!(ShowErr, "'fn main' NOT FOUND");
                    log_dbg(&comp, save);
                    return false;
                }
                Some(f) => {
                    match comp.compile(f, ExecTime::Runtime) {
                        Err(e) => {
                            log_err(&comp, e, save);
                            return false;
                        }
                        Ok(_) => {
                            // let end = timestamp();
                            // let seconds = end - start;
                            // outln!(ShowPrint, "===============");
                            // outln!(ShowPrint,
                            //     "Frontend (parse+comptime+bytecode) finished.\n   - {lines} (non comment) lines in {seconds:.5} seconds ({:.0} lines per second).",
                            //     lines as f64 / seconds
                            // );
                            // outln!(ShowPrint, "===============");
                            // let start = timestamp();
                            match comp.run(f, arg.into(), ExecTime::Runtime) {
                                Err(e) => {
                                    log_err(&comp, e, save);
                                    return false;
                                }
                                Ok(result) => {
                                    // let end = timestamp();
                                    // let seconds = end - start;
                                    // outln!(ShowPrint, "===============");
                                    // outln!(ShowPrint, "Interpreter finished running main() in {seconds:.5} seconds.");
                                    debug_assert_eq!(result, expect.into());
                                    // TODO: change this when i add assert(bool)
                                    let assertion_count = src.split("assert_eq(").count() - 1;
                                    // debug so dont crash in web if not using my system of one run per occurance.
                                    debug_assert_eq!(comp.program.assertion_count, assertion_count, "vm missed assertions?");
                                    outln!(
                                        ShowPrint,
                                        "   - {assertion_count} assertions passed. {} comptime evaluations.",
                                        comp.anon_fn_counter
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
    } // TODO: this is dereanged. put it in a function so you can just use ? to call log_err

    outln!(ShowPrint, "===============");
    log_dbg(&comp, save);
    println!("Created {} AST nodes.", EXPR_COUNT.load(Ordering::Acquire));
    if leak {
        let _ = ManuallyDrop::new(comp);
        let _ = ManuallyDrop::new(program);
    }
    true
}

fn log_dbg(comp: &Compile<'_, '_>, save: Option<&str>) {
    outln!(Bytecode, "{}", comp.runtime_executor.log(comp.pool));
    if let Some(id) = comp.program.find_unique_func(Flag::Main.ident()) {
        outln!(FinalAst, "{}", comp.program.log_finished_ast(id));
    }

    println!("{}", get_logs(ShowPrint));
    println!("{}", get_logs(ShowErr));
    if let Some(path) = save {
        let folder = &format!("target/latest_log/{path}");
        fs::create_dir_all(folder).unwrap();
        save_logs(folder);
        println!("Wrote log to {folder:?}");
    }
    println!("===============================");
}

fn log_err<'p>(interp: &Compile<'_, 'p>, e: CompileError<'p>, save: Option<&str>) {
    outln!(ShowPrint, "ERROR");
    if let CErr::Diagnostic(diagnostic) = &e.reason {
        emit_diagnostic(&interp.program.codemap, diagnostic);
    } else if let Some(loc) = e.loc {
        let diagnostic = vec![Diagnostic {
            level: Level::Error,
            message: e.reason.log(interp.program, interp.pool),
            code: None,
            spans: vec![SpanLabel {
                span: loc,
                label: None,
                style: SpanStyle::Primary,
            }],
        }];
        emit_diagnostic(&interp.program.codemap, &diagnostic);
    } else {
        outln!(ShowErr, "{}", e.reason.log(interp.program, interp.pool));
    }

    outln!(ShowErr, "Internal: {}", e.internal_loc);
    outln!(ShowErr, "{}", e.trace);
    outln!(ShowErr, "{}", e.call_stack);
    log_dbg(interp, save);
}

fn emit_diagnostic(codemap: &CodeMap, diagnostic: &[Diagnostic]) {
    for d in diagnostic {
        println!("{}", d.message);
    }
    if cfg!(target_arch = "wasm32") {
        let mut out = vec![];
        let mut emitter = Emitter::vec(&mut out, Some(codemap));
        emitter.emit(diagnostic);
        drop(emitter);
        outln!(
            ShowErr,
            "{}",
            String::from_utf8(out).unwrap_or_else(|_| "ICE: diagnostic was not valid utf8".into())
        );
    } else {
        let mut emitter = Emitter::stderr(ColorConfig::Auto, Some(codemap));
        emitter.emit(diagnostic);
    }
}

pub fn make_toplevel<'p>(pool: &StringPool<'p>, user_span: Span, stmts: Vec<FatStmt<'p>>) -> Func<'p> {
    let name = pool.intern("@toplevel@");
    let body = Some(FatExpr::synthetic(
        Expr::Block {
            body: stmts,
            result: Box::new(FatExpr::synthetic(Expr::unit(), user_span)),
            locals: None,
        },
        user_span,
    ));

    let (g_arg, g_ret) = Func::known_args(TypeId::unit(), TypeId::unit(), user_span);
    Func::new(name, g_arg, g_ret, body, user_span, true)
}

// TODO: its unfortunate that ifdefing this out means i don't get ide here.
/// C ABI callable from js when targeting wasm.
#[cfg(target_arch = "wasm32")]
pub mod web {
    #![allow(clippy::missing_safety_doc)]
    use crate::logging::{init_logs_flag, save_logs};

    use crate::bc::Value;
    use crate::interp::Interp;
    use crate::logging::{outln, LogTag::*};
    use crate::pool::StringPool;
    use crate::run_main;
    use std::alloc::{alloc, Layout};
    use std::ffi::{c_char, CStr, CString};
    use std::ptr::slice_from_raw_parts;

    static mut POOL: Option<StringPool> = None;

    /// len does NOT include null terminator.
    #[no_mangle]
    pub unsafe extern "C" fn run(log_flag: u64, input_query: *const u8, len: usize) {
        let s = format!("Running with log={log_flag:?}");
        unsafe { crate::web::console_log(s.as_ptr(), s.len()) };

        init_logs_flag(log_flag);
        if POOL.is_none() {
            POOL = Some(StringPool::default());
        }
        let pool = POOL.as_ref().unwrap();

        let src = &*slice_from_raw_parts(input_query, len);
        let src = match String::from_utf8(src.to_vec()) {
            Ok(src) => src,
            Err(e) => {
                outln!(ShowErr, "{:?}", e);
                return;
            }
        };
        run_main(pool, src, Value::Unit, Value::Unit, None, Interp::new(pool));
        save_logs("");
    }

    /// len DOES include null terminator
    #[no_mangle]
    pub unsafe extern "C" fn alloc_str(len: usize) -> *mut u8 {
        alloc(Layout::array::<u8>(len).unwrap())
    }

    #[no_mangle]
    pub unsafe extern "C" fn drop_c_str(ptr: *mut c_char) {
        let _ = CString::from_raw(ptr);
    }

    /// result does NOT include null terminator.
    #[no_mangle]
    pub unsafe extern "C" fn c_str_len(ptr: *mut c_char) -> usize {
        CStr::from_ptr(ptr).to_bytes().len()
    }

    extern "C" {
        pub fn console_log(ptr: *const u8, len: usize);
        pub fn timestamp() -> f64;
        pub fn show_log(tag: usize, ptr: *const u8, len: usize);
    }
}

pub fn timestamp() -> f64 {
    #[cfg(target_arch = "wasm32")]
    {
        let t = unsafe { web::timestamp() };
        t / 1000.0
    }

    #[cfg(not(target_arch = "wasm32"))]
    {
        use std::time::SystemTime;
        SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_secs_f64()
    }
}

#[cfg(feature = "mir")]
#[test]
fn test_mir() {
    use mir_sys::*;
    unsafe {
        let ctx = _MIR_init();
        c2mir_init(ctx);
        // c2mir_compile(ctx, ops, getc_func, getc_data, source_name, output_file);
        c2mir_finish(ctx);
    }
}

macro_rules! impl_index_imm {
    ($container:ty, $idx:ty, $elem:ty, $field:ident) => {
        impl<'p> std::ops::Index<$idx> for $container {
            type Output = $elem;

            fn index(&self, index: $idx) -> &Self::Output {
                &self.$field[index.0 as usize]
            }
        }
    };
}

macro_rules! impl_index {
    ($container:ty, $idx:ty, $elem:ty, $field:ident) => {
        $crate::impl_index_imm!($container, $idx, $elem, $field);

        impl<'p> std::ops::IndexMut<$idx> for $container {
            fn index_mut(&mut self, index: $idx) -> &mut Self::Output {
                &mut self.$field[index.0]
            }
        }
    };
}
pub(crate) use impl_index;
pub(crate) use impl_index_imm;

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
