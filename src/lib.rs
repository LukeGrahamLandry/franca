use std::{fs, path::PathBuf};

use bc::Value;
use codemap::CodeMap;
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle};
use pool::StringPool;

pub mod ast;
pub mod bc;
pub mod compiler;
pub mod ffi;
pub mod interp;
pub mod lex;
pub mod logging;
pub mod parse;
pub mod pool;
pub mod scope;

use crate::{
    ast::{Expr, FatExpr, FatStmt, Func, LazyFnType, Program, TypeId},
    bc::SharedConstants,
    compiler::{Compile, CompileError, ExecTime},
    logging::{outln, PoolLog},
    parse::Parser,
    scope::ResolveScope,
};

static LIB: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/lib/interp_builtins.txt"
));

macro_rules! test_file {
    ($case:ident) => {
        #[test]
        fn $case() {
            let pool = Box::leak(Box::<StringPool>::default());

            assert!(run_main(
                pool,
                fs::read_to_string(format!("tests/{}.txt", stringify!($case))).unwrap(),
                Value::I64(0),
                Value::I64(0),
                Some(&stringify!($case)),
            )
            .is_some());
        }
    };
}

test_file!(basic);
test_file!(structs);
test_file!(generics);

pub fn run_main<'a: 'p, 'p>(
    pool: &'a StringPool<'p>,
    src: String,
    arg: Value,
    expect: Value,
    save: Option<&str>,
) -> Option<String> {
    let start = timestamp();
    let mut codemap = CodeMap::new();
    let lib = codemap.add_file("lib/interp_builtins.txt".into(), LIB.to_string());
    let code = codemap.add_file("main_file".into(), src.clone());
    let mut stmts = Vec::<FatStmt<'p>>::new();

    let mut parse = |file| match Parser::parse(file, pool) {
        Ok(new) => {
            stmts.extend(new);
            true
        }
        Err(e) => {
            outln!("Parse error (Internal: {})", e.loc);

            emit_diagnostic(&codemap, &e.diagnostic);
            false
        }
    };

    if !parse(lib.clone()) || !parse(code) {
        return None;
    }

    let mut global = Func {
        annotations: vec![],
        name: Some(pool.intern("@toplevel@")),
        ty: LazyFnType::Finished(TypeId::any(), TypeId::any()),
        body: Some(FatExpr::synthetic(
            Expr::Block {
                body: stmts,
                result: Box::new(FatExpr::synthetic(Expr::Value(Value::Unit), lib.span)),
                locals: None,
            },
            lib.span,
        )),
        arg_names: vec![],
        arg_vars: None,
        capture_vars: vec![],
        local_constants: vec![],
        loc: lib.span,
        arg_loc: vec![],
    };

    let vars = ResolveScope::of(&mut global);
    let mut program = Program::new(vars, pool);
    let mut interp = Compile::new(pool, &mut program);
    // damn turns out defer would maybe be a good idea
    let result = interp.add_declarations(&SharedConstants::default(), global);
    fn log_err<'p>(codemap: CodeMap, interp: &mut Compile<'_, 'p>, e: CompileError<'p>) {
        let diagnostic = vec![Diagnostic {
            level: Level::Error,
            message: e.reason.log(interp.interp.program, interp.pool),
            code: None,
            spans: vec![SpanLabel {
                span: e.loc.unwrap(),
                label: None,
                style: SpanStyle::Primary,
            }],
        }];
        emit_diagnostic(&codemap, &diagnostic);
        outln!("Internal: {}", e.internal_loc);
        outln!("{}", e.trace);
    }

    if let Err(e) = result {
        log_err(codemap, &mut interp, e);
        return None;
    } else {
        let toplevel = interp.lookup_unique_func(pool.intern("@toplevel@"));
        let id = toplevel.unwrap();
        let constants = interp.interp.ready[id.0]
            .as_ref()
            .unwrap()
            .constants
            .clone();
        let name = pool.intern("main");
        match interp.lookup_unique_func(name) {
            None => {
                outln!("FN {name:?} = 'MAIN' NOT FOUND");
            }
            Some(f) => {
                match interp.compile(Some(&constants), f, ExecTime::Runtime) {
                    Err(e) => {
                        log_err(codemap, &mut interp, e);
                        return None;
                    }
                    Ok(_) => {
                        let end = timestamp();
                        let seconds = end - start;
                        let lines = format!("{}\n{}", LIB, src)
                            .split('\n')
                            .filter(|s| !s.split("//").next().unwrap().is_empty())
                            .count();

                        outln!("===============");
                        outln!(
                            "Frontend (parse+comptime+bytecode) finished.\n   - {lines} (non comment/empty) lines in {seconds:.5} seconds ({:.0} lines per second).",
                            lines as f64 / seconds
                        );
                        let inst_count: usize = interp
                            .interp
                            .ready
                            .iter()
                            .flatten()
                            .map(|func| func.insts.len())
                            .sum();
                        outln!(
                            "   - Generated {inst_count} instructions ({:.0} i/sec).",
                            inst_count as f64 / seconds
                        );

                        outln!("===============");
                        let start = timestamp();
                        match interp.run(f, arg.clone(), ExecTime::Runtime) {
                            Err(e) => {
                                log_err(codemap, &mut interp, e);
                                return None;
                            }
                            Ok(result) => {
                                let end = timestamp();
                                let seconds = end - start;
                                outln!("===============");
                                outln!(
                                    "Interpreter finished running main() in {seconds:.5} seconds."
                                );
                                debug_assert_eq!(result, expect);
                                // TODO: change this when i add assert(bool)
                                let assertion_count = src.split("assert_eq(").count() - 1;
                                // debug so dont crash in web if not using my system of one run per occurance.
                                debug_assert_eq!(
                                    interp.interp.assertion_count, assertion_count,
                                    "vm missed assertions?"
                                );
                                outln!(
                                    "   - {assertion_count} assertions passed. {} comptime evaluations.",
                                    interp.anon_fn_counter
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    if cfg!(feature = "some_log") {
        if let Some(path) = save {
            let path = PathBuf::from(format!("target/latest_log/{path}/interp.log"));
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(
                &path,
                format!("{}\nAt {:?}", interp.interp.log(pool), timestamp()),
            )
            .unwrap();
            outln!("Wrote log to {:?}", path);
        }
    }
    outln!("===============");
    Some(interp.interp.log(pool))
}

fn emit_diagnostic(codemap: &CodeMap, diagnostic: &[Diagnostic]) {
    if cfg!(target_arch = "wasm32") {
        let mut out = vec![];
        let mut emitter = Emitter::vec(&mut out, Some(codemap));
        emitter.emit(diagnostic);
        drop(emitter);
        outln!(
            "{}",
            String::from_utf8(out).unwrap_or_else(|_| "ICE: diagnostic was not valid utf8".into())
        );
    } else {
        let mut emitter = Emitter::stderr(ColorConfig::Auto, Some(codemap));
        emitter.emit(diagnostic);
    }
}

// TODO: its unfortunate that ifdefing this out means i don't get ide here.
/// C ABI callable from js when targeting wasm.
#[cfg(target_arch = "wasm32")]
pub mod web {
    #![allow(clippy::missing_safety_doc)]
    use crate::bc::Value;
    use crate::logging::outln;
    use crate::pool::StringPool;
    use crate::run_main;
    use std::alloc::{alloc, Layout};
    use std::ffi::{c_char, CStr, CString};
    use std::ptr::slice_from_raw_parts;

    static mut POOL: Option<StringPool> = None;

    /// len does NOT include null terminator.
    #[no_mangle]
    pub unsafe extern "C" fn run(input_query: *const u8, len: usize) {
        if POOL.is_none() {
            POOL = Some(StringPool::default());
        }
        let pool = POOL.as_ref().unwrap();

        let src = &*slice_from_raw_parts(input_query, len);
        let src = match String::from_utf8(src.to_vec()) {
            Ok(src) => src,
            Err(e) => {
                outln!("{:?}", e);
                return;
            }
        };
        let res = run_main(pool, src, Value::Unit, Value::Unit, None);
        flush_console();
        if let Some(s) = res {
            show_bc(s.as_ptr(), s.len());
        }
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

    // TODO: should just use Write for my logging stuff so this could be less clunky.
    // TODO: this means you won't get output if I don't catch an error and fully panic but I want that to not happen anyway.
    //       ice should always tel you waht it was trying to do because i care about debugging the compiler.
    //       but also infinite loops and you have to wait for the program to finish before you get anything.
    //       but i dont have loops yet so who cares.

    /// Printing one line is slower than parsing my example so buffering seems important.
    static mut CONSOLE: Vec<String> = vec![];

    pub fn flush_console() {
        unsafe {
            // This is overkill because printing is slow anyway but I want fewer calls between wasm and js.
            let len = CONSOLE.iter().map(|s| s.len() + 1).sum();
            let mut out = String::with_capacity(len);
            for msg in CONSOLE.drain(0..) {
                out.push_str(&msg);
                out.push('\n');
            }
            console_log(out.as_ptr(), out.len());
        }
    }

    pub fn push_console(msg: String) {
        // Safety: There's only one thread.
        unsafe {
            CONSOLE.push(msg);
        }
    }

    extern "C" {
        pub fn console_log(ptr: *const u8, len: usize);
        pub fn timestamp() -> f64;
        pub fn show_bc(ptr: *const u8, len: usize);
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
        SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_secs_f64()
    }
}
