use std::fs;

use bc::Value;
use codemap::CodeMap;
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle};
use pool::StringPool;

macro_rules! mut_replace {
    ($value:expr, $f:expr) => {{
        let temp = mem::take(&mut $value);
        let (temp, out) = $f(temp)?;
        $value = temp;
        out
    }};
}

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
    ast::{Expr, FatExpr, FatStmt, Func, Program, TypeId},
    compiler::{Compile, CompileError, ExecTime},
    logging::{get_logs, outln, save_logs, LogTag::{ShowErr, *}, PoolLog},
    parse::Parser,
    scope::ResolveScope,
};

macro_rules! stdlib {
    ($name:expr) => {
        (
            $name,
            include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/lib/", $name, ".txt")),
        )
    };
}

static LIB: &[(&str, &str)] = &[stdlib!("interp"), stdlib!("collections"), stdlib!("system")];

macro_rules! test_file {
    ($case:ident) => {
        #[test]
        fn $case() {
            crate::logging::init_logs(&[ShowErr]);
            
            let pool = Box::leak(Box::<StringPool>::default());

            assert!(run_main(
                pool,
                fs::read_to_string(format!("tests/{}.txt", stringify!($case))).unwrap(),
                Value::I64(3145192),
                Value::I64(3145192),
                Some(&stringify!($case)),
            ));
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

pub fn run_main<'a: 'p, 'p>(
    pool: &'a StringPool<'p>,
    src: String,
    arg: Value,
    expect: Value,
    save: Option<&str>,
) -> bool {
    let start = timestamp();
    let mut codemap = CodeMap::new();
    let mut stmts = Vec::<FatStmt<'p>>::new();

    // TODO: this will get less dumb when I have first class modules.
    let mut libs: Vec<_> = LIB
        .iter()
        .map(|(name, code)| codemap.add_file(name.to_string(), code.to_string()))
        .collect();
    libs.push(codemap.add_file("main_file".into(), src.clone()));
    let user_span = libs.last().unwrap().span;
    for file in &libs {
        match Parser::parse(file.clone(), pool) {
            Ok(new) => {
                stmts.extend(new);
            }
            Err(e) => {
                outln!(ShowErr, "Parse error (Internal: {})", e.loc);

                emit_diagnostic(&codemap, &e.diagnostic);
                return false;
            }
        }
    }

    let name = pool.intern("@toplevel@");
    let body = Some(FatExpr::synthetic(
        Expr::Block {
            body: stmts,
            result: Box::new(FatExpr::synthetic(Expr::Value(Value::Unit), user_span)),
            locals: None,
        },
        user_span,
    ));

    let (g_arg, g_ret) = Func::known_args(TypeId::unit(), TypeId::unit(), user_span);
    let mut global = Func::new(name, g_arg, g_ret, body, user_span, true);

    let vars = ResolveScope::of(&mut global, pool);
    let mut program = Program::new(vars, pool);
    let mut interp = Compile::new(pool, &mut program);
    // damn turns out defer would maybe be a good idea
    let result = interp.add_declarations(global);

    fn log_dbg(interp: &Compile, save: Option<&str>) {
        outln!(Bytecode, "{}", interp.interp.log(interp.pool));
        let name = interp.pool.intern("main");
        if let Some(id) = interp.lookup_unique_func(name) {
            outln!(FinalAst, "{}", interp.interp.program.log_finished_ast(id));
        }
        
        println!("{}", get_logs(ShowPrint));
        println!("{}", get_logs(ShowErr));
        if let Some(path) = save {
            let folder = &format!("target/latest_log/{path}");
            fs::create_dir_all(folder).unwrap();
            save_logs(folder);
            println!( "Wrote log to {folder:?}");
        }
        println!("===============================");
    }

    fn log_err<'p>(
        codemap: CodeMap,
        interp: &mut Compile<'_, 'p>,
        e: CompileError<'p>,
        save: Option<&str>,
    ) {
        if let Some(loc) = e.loc {
            let diagnostic = vec![Diagnostic {
                level: Level::Error,
                message: e.reason.log(interp.interp.program, interp.pool),
                code: None,
                spans: vec![SpanLabel {
                    span: loc,
                    label: None,
                    style: SpanStyle::Primary,
                }],
            }];
            emit_diagnostic(&codemap, &diagnostic);
        } else {
            outln!(
                ShowErr,
                "{}",
                e.reason.log(interp.interp.program, interp.pool)
            );
        }

        outln!(ShowErr, "Internal: {}", e.internal_loc);
        outln!(ShowErr, "{}", e.trace);
        log_dbg(interp, save);
    }

    match result {
        Err(e) => {
            log_err(codemap, &mut interp, e, save);
            return false;
        }
        Ok(_toplevel) => {
            let name = pool.intern("main");
            match interp.lookup_unique_func(name) {
                None => {
                    outln!(ShowErr, "FN {name:?} = 'MAIN' NOT FOUND");
                    let decls = interp
                        .interp
                        .program
                        .declarations
                        .keys()
                        .map(|n| pool.get(*n).to_string())
                        .collect::<Vec<String>>();
                    outln!(ShowErr, "Decls: {decls:?}");
                    log_dbg(&interp, save);
                    return false;
                }
                Some(f) => {
                    match interp.compile(f, ExecTime::Runtime) {
                        Err(e) => {
                            log_err(codemap, &mut interp, e, save);
                            return false;
                        }
                        Ok(_) => {
                            let end = timestamp();
                            let seconds = end - start;
                            let lib: String = LIB.iter().map(|(_, code)| *code).collect();
                            let lines = format!("{}\n{}", lib, src)
                                .split('\n')
                                .filter(|s| !s.split("//").next().unwrap().is_empty())
                                .count();

                            outln!(ShowPrint, "===============");
                            outln!(ShowPrint, 
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
                            outln!(ShowPrint, 
                                "   - Generated {inst_count} instructions ({:.0} i/sec).",
                                inst_count as f64 / seconds
                            );

                            outln!(ShowPrint, "===============");
                            let start = timestamp();
                            match interp.run(f, arg.clone(), ExecTime::Runtime) {
                                Err(e) => {
                                    log_err(codemap, &mut interp, e, save);
                                    return false;
                                }
                                Ok(result) => {
                                    let end = timestamp();
                                    let seconds = end - start;
                                    outln!(ShowPrint, "===============");
                                    outln!(ShowPrint, 
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
                                    outln!(ShowPrint, 
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
    }

    outln!(ShowPrint, "===============");
    log_dbg(&interp, save);
    true
}

fn emit_diagnostic(codemap: &CodeMap, diagnostic: &[Diagnostic]) {
    if cfg!(target_arch = "wasm32") {
        let mut out = vec![];
        let mut emitter = Emitter::vec(&mut out, Some(codemap));
        emitter.emit(diagnostic);
        drop(emitter);
        outln!(ShowErr,
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
    use crate::logging::{init_logs_flag, save_logs};

    use crate::bc::Value;
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
        run_main(pool, src, Value::Unit, Value::Unit, None);
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
        SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_secs_f64()
    }
}
