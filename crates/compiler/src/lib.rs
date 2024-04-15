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
#![feature(backtrace_frames)]
extern crate core;

use std::env;
use std::mem::ManuallyDrop;
use std::path::PathBuf;
use std::{fs, sync::atomic::Ordering};

use ast::FuncId;
use bc::Value;
use codemap::{CodeMap, Span};
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle};
use compiler::{CErr, Res};
use export_ffi::STDLIB_PATH;
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
pub mod bc_to_asm;
pub mod bootstrap_gen;
pub mod compiler;
pub mod crc;
pub mod emit_bc;
pub mod emit_rust;
pub mod export_ffi;
pub mod ffi;
pub mod interp;
pub mod lex;
#[cfg(feature = "llvm")]
pub mod llvm;
pub mod logging;
#[cfg(feature = "lsp")]
pub mod lsp;
pub mod overloading;
pub mod parse;
pub mod pool;
pub mod reflect;
pub mod scope;

use crate::logging::PoolLog;
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

// I'd rather include it in the binary but I do this so I don't have to wait for the compiler to recompile every time I change the lib
// (maybe include_bytes in a seperate crate would make it better)
// I also like that users can put the lib somewhere an edit it for thier program. I dont want the compiler to just force its blessed version.
// But I also don't want it to be like c where you just get whatever the system happens to have.
pub fn find_std_lib() -> bool {
    fn check(mut p: PathBuf) -> bool {
        p.push("lib");
        p.push("franca_stdlib_1.fr");
        if p.exists() {
            p.pop();
            let mut path = STDLIB_PATH.lock().unwrap();
            *path = Some(p);
            return true;
        }
        false
    }

    // if a project wants to supply its own version, that should take priority.
    if let Ok(mut p) = env::current_dir() {
        if check(p.clone()) {
            return true;
        }
        p.push("franca");
        if check(p.clone()) {
            return true;
        }
        p.pop();
        p.push("vendor/franca");
        if check(p.clone()) {
            return true;
        }
    }

    if let Ok(mut p) = env::current_exe() {
        p.pop();
        p.push("franca");
        if check(p.clone()) {
            return true;
        }
        // exe might be in franca/target/release/franca or franca/target/debug/deps/compiler-21be1aa281dbe5d6, so go up
        for _ in 0..5 {
            p.pop();
            if check(p.clone()) {
                return true;
            }
        }
    }

    false
}

pub fn load_program<'p>(comp: &mut Compile<'_, 'p>, src: &str) -> Res<'p, (FuncId, usize)> {
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
    let f = comp.compile_module(global)?;
    Ok((f, parsed.lines))
}

// If it's just a cli that's going to immediately exit, you can set leak=true and not bother walking the tree to free everything at the end.
// I should really just use arenas for everything.
#[allow(clippy::too_many_arguments)]
pub fn run_main<'a: 'p, 'p>(pool: &'a StringPool<'p>, src: String, mut arg: Value, mut expect: Value, save: Option<&str>, leak: bool) -> bool {
    log_tag_info();
    let start = timestamp();
    let mut program = Program::new(pool, TargetArch::Interp, TargetArch::Interp);
    let mut comp = Compile::new(pool, &mut program);
    let result = load_program(&mut comp, &src);

    // damn turns out defer would maybe be a good idea

    match result {
        Err(e) => {
            log_err(&comp, e, save);
            return false;
        }
        Ok((_, lines)) => {
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
                            let end = timestamp();
                            let seconds = end - start;
                            outln!(Perf, "===============");
                            outln!(Perf,
                                "Frontend (parse+comptime+bytecode) finished.\n   - {lines} (non comment) lines in {seconds:.5} seconds ({:.0} lines per second).",
                                lines as f64 / seconds
                            );
                            outln!(Perf, "===============");
                            let start = timestamp();
                            if comp.program[f].finished_ret == Some(TypeId::unit()) && comp.program[f].finished_arg == Some(TypeId::unit()) {
                                arg = Value::Unit;
                                expect = Value::Unit;
                            }
                            match comp.run(f, arg.into(), ExecTime::Runtime) {
                                Err(e) => {
                                    log_err(&comp, e, save);
                                    return false;
                                }
                                Ok(result) => {
                                    let end = timestamp();
                                    let seconds = end - start;
                                    outln!(Perf, "===============");
                                    outln!(Perf, "Interpreter finished running main() in {seconds:.5} seconds.");
                                    debug_assert_eq!(result, expect.into());
                                    // TODO: change this when i add assert(bool)
                                    let assertion_count = src.split("assert_eq(").count() - 1;
                                    // debug so dont crash in web if not using my system of one run per occurance.
                                    debug_assert_eq!(comp.program.assertion_count, assertion_count, "vm missed assertions?");
                                    outln!(
                                        Perf,
                                        "   - {} assertions passed. {} comptime evaluations.",
                                        comp.program.assertion_count,
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

    outln!(Perf, "===============");
    log_dbg(&comp, save);
    outln!(Perf, "Created {} AST nodes.", EXPR_COUNT.load(Ordering::Acquire));
    if leak {
        let _ = ManuallyDrop::new(comp);
        let _ = ManuallyDrop::new(program);
    }
    true
}

pub fn log_dbg(comp: &Compile<'_, '_>, save: Option<&str>) {
    for b in comp.ready.ready.iter().flatten() {
        outln!(Bytecode, "{}", b.log(comp.pool));
    }

    if let Some(id) = comp.program.find_unique_func(Flag::Main.ident()) {
        outln!(FinalAst, "{}", comp.program.log_finished_ast(id));
    }

    println!("{}", get_logs(ShowPrint));
    let err = get_logs(ShowErr);
    if !err.is_empty() {
        println!("{err}");
    }

    if let Some(path) = save {
        let folder = &format!("target/latest_log/{path}");
        fs::create_dir_all(folder).unwrap();
        save_logs(folder);
        outln!(Perf, "Wrote log to {folder:?}");
    }
    outln!(Perf, "===============================");
}

pub fn log_err<'p>(interp: &Compile<'_, 'p>, e: CompileError<'p>, save: Option<&str>) {
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

    if cfg!(feature = "trace_errors") {
        outln!(ShowErr, "Internal: {}", e.internal_loc.unwrap());
    }
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

pub fn timestamp() -> f64 {
    use std::time::SystemTime;
    SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_secs_f64()
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
