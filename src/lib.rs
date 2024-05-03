#![feature(ptr_metadata)]
#![feature(iter_array_chunks)]
#![feature(vec_into_raw_parts)]
#![feature(slice_ptr_len)]
#![feature(slice_ptr_get)]
#![feature(closure_track_caller)]
#![feature(sync_unsafe_cell)]
// bro if you can tell you could compile it more efficiently why don't you just compile it more efficiently
#![allow(clippy::format_collect)]
extern crate core;

struct MyAllocator;

pub const ARENA_SIZE: usize = 1 << 30;
pub const COMPTIME_STACK_SIZE: usize = 1 << 20;

thread_local! {
     pub static MEM: Cell<*mut u8> = Cell::new(unsafe{libc::mmap(
         null_mut(),
         ARENA_SIZE,
         libc::PROT_WRITE | libc::PROT_READ,
         libc::MAP_ANON | libc::MAP_PRIVATE,
         -1,
         0,
     )} as *mut u8);
     pub static COMPTIME_STACK: Cell<*mut u8> = Cell::new(unsafe{libc::mmap(
         null_mut(),
         COMPTIME_STACK_SIZE,
         libc::PROT_WRITE | libc::PROT_READ,
         libc::MAP_ANON | libc::MAP_PRIVATE,
         -1,
         0,
     )} as *mut u8);
}

unsafe impl GlobalAlloc for MyAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let ptr = MEM.get();
        let ptr = ptr.add(ptr.align_offset(layout.align()));
        MEM.set(ptr.add(layout.size()));
        ptr
    }

    unsafe fn dealloc(&self, _: *mut u8, _: Layout) {
        // leak it
    }
}

// Apr 29. this makes (release mode) tests ~20% faster.
#[global_allocator]
static GLOBAL: MyAllocator = MyAllocator;

pub type Map<K, V> = rustc_hash::FxHashMap<K, V>;

pub const TRACE_ASM: bool = false;

#[macro_export]
macro_rules! debug {
    ($($arg:tt)*) => {{
        if $crate::TRACE_ASM {
            print!($($arg)*);
        }
    }};
}

#[macro_export]
macro_rules! debugln {
    ($($arg:tt)*) => {{
        if $crate::TRACE_ASM  {
            println!($($arg)*);
        }
    }};
}

use std::alloc::GlobalAlloc;
use std::alloc::Layout;
use std::arch::asm;
use std::cell::Cell;
use std::env;
use std::fs;
use std::mem::{transmute, ManuallyDrop};
use std::path::PathBuf;
use std::ptr::null_mut;

use ast::FuncId;
use ast::ScopeId;
use bc::Value;
use codemap::{CodeMap, Span};
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle};
use compiler::{CErr, Res};
use export_ffi::STDLIB_PATH;
use lex::Lexer;
use pool::StringPool;

macro_rules! mut_replace {
    ($value:expr, $f:expr) => {{
        let temp = mem::take(&mut $value);
        #[allow(clippy::redundant_closure_call)]
        let res = $f(temp);
        if res.is_err() {
            println!("WARNING: mut_replace hit err. something will be lost!! {}. {res:?}", Location::caller());
        }
        let (temp, out) = res?;
        $value = temp;
        out
    }};
}

pub mod ast;
pub mod bc;
pub mod bc_to_asm;
pub mod bootstrap_gen;
pub mod compiler;
pub mod emit_bc;
pub mod emit_rust;
pub mod export_ffi;
pub mod ffi;
pub mod lex;
pub mod logging;
pub mod overloading;
pub mod parse;
pub mod pool;
pub mod reflect;
pub mod scope;

use crate::bc_to_asm::emit_aarch64;
use crate::logging::{init_logs_flag, PoolLog};
use crate::{
    ast::{Expr, FatExpr, FatStmt, Flag, Func, Program, TargetArch, TypeId},
    compiler::{Compile, CompileError, ExecTime},
    logging::{
        get_logs, log_tag_info, save_logs,
        LogTag::{ShowErr, *},
    },
    parse::Parser,
    scope::ResolveScope,
};

#[derive(Debug)]
pub struct Stats {
    pub ast_expr_nodes: usize,
    pub fn_body_resolve: usize,
    pub make_lit_fn: usize,
    pub parser_queue: usize,
    pub parser_did: usize,
    pub jit_mprotect: usize,
}

pub static mut STATS: Stats = Stats {
    ast_expr_nodes: 0,
    fn_body_resolve: 0,
    make_lit_fn: 0,
    parser_queue: 0,
    jit_mprotect: 0,
    parser_did: 0,
};

/*
macro_rules! include_std {
    ($name:expr) => {
        (concat!($name, ".fr"), include_str!(concat!("../../../lib/", $name, ".fr")))
    };
}

pub const INCLUDE_STD: &[(&str, &str)] = &[
    include_std!("core"),
    include_std!("ast"),
    include_std!("fmt"),
    include_std!("list"),
    include_std!("map"),
    include_std!("mem"),
    include_std!("option"),
    include_std!("pointers"),
    include_std!("prelude"),
    include_std!("macros"),
    include_std!("testing"),
    include_std!("slice"),
    include_std!("system"),
    include_std!("codegen/aarch64/basic"),
    include_std!("codegen/aarch64/basic.gen"),
    include_std!("codegen/aarch64/instructions"),
    include_std!("codegen/aarch64/unwind"),
    include_std!("codegen/bf/instructions"),
    include_std!("codegen/llvm/basic"),
    include_std!("codegen/wasm/basic"),
    include_std!("codegen/wasm/instructions"),
];
*/

pub static mut STACK_START: usize = 0;
pub static mut JITTED_PAGE: (usize, usize) = (0, 0);
pub static mut MY_CONST_DATA: (usize, usize) = (0, 0);
pub static mut STACK_MIN: usize = usize::max_value();
pub static mut COMPILER_CTX_PTR: usize = 0;
pub static mut MMAP_ARENA_START: usize = 0;

pub static MY_STRING: &str = "Hello World";

#[inline(never)]
pub fn where_am_i() {
    let marker = 0;
    println!("ADDR OF RUST FUNCTION: {}", where_am_i as usize);
    println!("ADDR OF RUST CONSTANT DATA: {}", MY_STRING.as_ptr() as usize);
    println!(
        "ADDR OF RUST STACK: {} to {} (min: {})",
        unsafe { STACK_START },
        &marker as *const i32 as usize,
        unsafe { STACK_MIN }
    );
    println!("COMPILER_CTX_PTR: {}", unsafe { COMPILER_CTX_PTR });
    println!("ADDR OF MMAP ARENA: {} to {} ", unsafe { MMAP_ARENA_START }, MEM.get() as usize);
    println!("ADDR OF MMAP JITTED: {} to {}", unsafe { JITTED_PAGE.0 }, unsafe {
        JITTED_PAGE.0 + JITTED_PAGE.1
    });
    println!("ADDR OF MMAP CONST DATA: {} to {}", unsafe { MY_CONST_DATA.0 }, unsafe {
        MY_CONST_DATA.0 + MY_CONST_DATA.1
    });
}

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

    let p = PathBuf::from("/Users/luke/Documents/mods/infered");
    if check(p) {
        return true;
    }

    false
}

pub fn load_program<'p>(comp: &mut Compile<'_, 'p>, src: &str) -> Res<'p, FuncId> {
    unsafe { COMPILER_CTX_PTR = comp as *const Compile as usize };
    // TODO: this will get less dumb when I have first class modules.
    let file = comp
        .parsing
        .codemap
        .add_file("main_file".to_string(), format!("#include_std(\"core.fr\");\n{src}"));
    let user_span = file.span;
    let lex = Lexer::new(file.clone(), comp.program.pool, file.span);
    let parsed = match Parser::parse_stmts(&mut comp.parsing, lex, comp.pool) {
        Ok(s) => s,
        Err(e) => {
            return Err(CompileError {
                internal_loc: e.loc,
                loc: Some(e.diagnostic[0].spans[0].span),
                reason: CErr::Diagnostic(e.diagnostic),
                trace: String::new(),
            })
        }
    };

    let mut global = make_toplevel(comp.pool, user_span, parsed);
    ResolveScope::run(&mut global, comp, ScopeId::from_index(0))?;
    let f = comp.compile_top_level(global)?;
    Ok(f)
}

// If it's just a cli that's going to immediately exit, you can set leak=true and not bother walking the tree to free everything at the end.
// I should really just use arenas for everything.
#[allow(clippy::too_many_arguments)]
pub fn run_main<'a: 'p, 'p>(pool: &'a StringPool<'p>, src: String, save: Option<&str>, leak: bool) -> bool {
    let beg = MEM.get();
    init_logs_flag(0x0); //init_logs_flag(0xFFFFFFFFF);
    log_tag_info();
    let start = timestamp();
    let mut program = Program::new(pool, TargetArch::Aarch64, TargetArch::Aarch64);
    let mut comp = Compile::new(pool, &mut program);
    let result = load_program(&mut comp, &src);

    // damn turns out defer would maybe be a good idea

    match result {
        Err(e) => {
            log_err(&comp, e, save);
            return false;
        }
        Ok(_) => {
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
                            if let Err(e) = emit_aarch64(&mut comp, f, ExecTime::Runtime) {
                                log_err(&comp, e, save);
                                return false;
                            }
                            let end = timestamp();
                            let seconds = end - start;
                            outln!(Perf, "===============");
                            println!(
                                //Perf,
                                "Compilation (parse+comptime+bytecode+asm) finished in {seconds:.3} seconds",
                            );
                            outln!(Perf, "===============");
                            let start = timestamp();

                            comp.aarch64.reserve(comp.program.funcs.len()); // Need to allocate for all, not just up to the one being compiled because backtrace gets the len of array from the program func count not from asm.
                            comp.aarch64.make_exec();
                            comp.flush_cpu_instruction_cache();
                            let code = comp.aarch64.get_fn(f).unwrap().as_ptr();

                            let code: extern "C-unwind" fn(i64) -> i64 = unsafe { transmute(code) };
                            let indirect_fns = comp.aarch64.get_dispatch();
                            unsafe {
                                asm!(
                                "mov x21, {fns}",
                                fns = in(reg) indirect_fns,
                                // I'm hoping this is how I declare that I intend to clobber the register.
                                // https://doc.rust-lang.org/reference/inline-assembly.html
                                // "[...] the contents of the register to be discarded at the end of the asm code"
                                // I imagine that means they just don't put it anywhere, not that they zero it for spite reasons.
                                out("x21") _
                                );
                            }
                            code(0);

                            let end = timestamp();
                            let seconds = end - start;
                            outln!(Perf, "===============");
                            outln!(Perf, "Finished running main() in {seconds:.5} seconds.");
                        }
                    }
                }
            }
        }
    } // TODO: this is dereanged. put it in a function so you can just use ? to call log_err

    outln!(Perf, "===============");
    log_dbg(&comp, save);
    if leak {
        let _ = ManuallyDrop::new(comp);
        let _ = ManuallyDrop::new(program);
    }

    let end = MEM.get();
    println!("Used {} KB of memory.", (end as usize - beg as usize) / 1000);

    true
}

pub fn log_dbg(comp: &Compile<'_, '_>, save: Option<&str>) {
    for b in comp.ready.ready.iter().flatten() {
        outln!(Bytecode, "{}", b.log(comp.pool));
    }

    outln!(FinalAst, "============= BELOW IS ALL INCLUDING COMPTIME================");
    for (i, f) in comp.program.funcs.iter().enumerate() {
        if !f.evil_uninit {
            outln!(FinalAst, "{:?} {}", FuncId::from_index(i), f.log(comp.pool));
        }
    }

    let log_scope = |s: ScopeId| {
        let scope = &comp[s];
        let pad = format!("{}({}) ", "=".repeat(scope.depth), scope.depth);
        outln!(
            Consts,
            "{pad}fn '{}'; scope {} has parent {} (b: {}); specializations: {:?};",
            comp.pool.get(scope.name),
            s.as_index(),
            scope.parent.as_index(),
            scope.block_in_parent,
            scope.funcs
        );

        let mut s = pad.clone();
        for (i, b) in scope.vars.iter().enumerate() {
            s += &format!("({} -> {i}) ", b.parent);
        }
        outln!(Consts, "{s};");

        let mut consts = scope.constants.iter().collect::<Vec<_>>();
        consts.sort_by(|(a, _), (b, _)| a.1.cmp(&b.1));
        for (var, (e, ty)) in consts {
            outln!(Consts, "{pad}- {}: {} = {};", var.log(comp.pool), ty.log(comp.pool), e.log(comp.pool),);
        }
    };

    for s in 0..comp.scopes.len() {
        log_scope(ScopeId::from_index(s));
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

    if cfg!(feature = "trace_errors") {
        outln!(ShowErr, "Internal: {}", e.internal_loc.unwrap());
    }
    if let CErr::Diagnostic(diagnostic) = &e.reason {
        emit_diagnostic(&interp.parsing.codemap, diagnostic);
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
        emit_diagnostic(&interp.parsing.codemap, &diagnostic);
    } else {
        outln!(ShowErr, "{}", e.reason.log(interp.program, interp.pool));
    }

    outln!(ShowErr, "{}", e.trace);
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
            resolved: None,
            body: stmts,
            result: Box::new(FatExpr::synthetic_ty(
                Expr::Value { value: Value::Unit.into() },
                user_span,
                TypeId::unit(),
            )),
        },
        user_span,
    ));

    let (g_arg, g_ret) = Func::known_args(TypeId::unit(), TypeId::unit(), user_span);
    Func::new(name, g_arg, g_ret, body, user_span, true, false)
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
                &self.$field[index.as_index()]
            }
        }
    };
}

macro_rules! impl_index {
    ($container:ty, $idx:ty, $elem:ty, $field:ident) => {
        $crate::impl_index_imm!($container, $idx, $elem, $field);

        impl<'p> std::ops::IndexMut<$idx> for $container {
            fn index_mut(&mut self, index: $idx) -> &mut Self::Output {
                &mut self.$field[index.as_index()]
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
