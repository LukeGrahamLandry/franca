use std::{fs, path::PathBuf};

use codemap::CodeMap;
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle};
use interp::Value;
use pool::StringPool;

pub mod ast;
pub mod interp;
pub mod lex;
pub mod logging;
pub mod parse;
pub mod pool;
pub mod scope;

use crate::{
    ast::{Expr, FatExpr, FatStmt, Func, LazyFnType, Program, TypeId},
    interp::{CompileError, ExecTime, Interp, SharedConstants},
    logging::{outln, PoolLog},
    parse::Parser,
    scope::ResolveScope,
};

static LIB: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/lib/interp_builtins.txt"
));

pub fn run_main<'a: 'p, 'p>(
    pool: &'a StringPool<'p>,
    src: String,
    arg: Value,
    expect: Value,
    save: Option<&str>,
) {
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
        return;
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
    let mut program = Program {
        vars,
        ..Default::default()
    };
    let mut interp = Interp::new(pool, &mut program);
    // damn turns out defer would maybe be a good idea
    let result = interp.add_declarations(&SharedConstants::default(), global);
    fn log_err<'p>(codemap: CodeMap, interp: &mut Interp<'_, 'p>, e: CompileError<'p>) {
        let diagnostic = vec![Diagnostic {
            level: Level::Error,
            message: e.reason.log(interp.program, interp.pool),
            code: None,
            spans: vec![SpanLabel {
                span: e.loc.unwrap(),
                label: None,
                style: SpanStyle::Primary,
            }],
        }];
        emit_diagnostic(&codemap, &diagnostic);
        outln!("{}", e.trace);
    }

    if let Err(e) = result {
        log_err(codemap, &mut interp, e);
    } else {
        let toplevel = interp.lookup_unique_func(pool.intern("@toplevel@"));
        let id = toplevel.unwrap();
        let constants = interp.ready[id.0].as_ref().unwrap().constants.clone();
        let name = pool.intern("main");
        match interp.lookup_unique_func(name) {
            None => {
                outln!("FN {name:?} = 'MAIN' NOT FOUND");
            }
            Some(f) => {
                let result = interp.run(Some(&constants), f, arg.clone(), ExecTime::Runtime);
                match result {
                    Err(e) => log_err(codemap, &mut interp, e),
                    Ok(result) => {
                        let end = timestamp();
                        assert_eq!(result, expect);
                        // TODO: change this when i add assert(bool)
                        let assertion_count = src.split("assert_eq(").count() - 1;
                        assert_eq!(
                            interp.assertion_count, assertion_count,
                            "vm missed assertions?"
                        );
                        outln!(
                            "{assertion_count} assertions passed. {} comptime evaluations.",
                            interp.anon_fn_counter
                        );
                        let seconds = end - start;
                        let lines = format!("{}\n{}", LIB, src)
                            .split('\n')
                            .filter(|s| !s.split("//").next().unwrap().is_empty())
                            .count();
                        outln!(
                            "Finished {lines} (non comment/empty) lines in {seconds:.5} seconds ({:.0} lines per second).",
                            lines as f64 / seconds
                        );
                        let inst_count: usize = interp
                            .ready
                            .iter()
                            .flatten()
                            .map(|func| func.insts.len())
                            .sum();
                        outln!(
                            "Generated {inst_count} instructions ({:.0} i/sec).",
                            inst_count as f64 / seconds
                        );
                    }
                }
            }
        }
    }

    if cfg!(feature = "some_log") {
        if let Some(path) = save {
            let path = PathBuf::from(format!("target/latest_log/{path}/interp.log"));
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(&path, format!("{}\nAt {:?}", interp.log(pool), timestamp())).unwrap();
            outln!("Wrote log to {:?}", path);
        }
    }
}

fn emit_diagnostic(codemap: &CodeMap, diagnostic: &[Diagnostic]) {
    if cfg!(target_arch = "wasm32") {
        let mut out = vec![];
        let mut emitter = Emitter::vec(&mut out, Some(codemap));
        emitter.emit(diagnostic);
        drop(emitter);
        outln!("{}", String::from_utf8(out).unwrap());
    } else {
        let mut emitter = Emitter::stderr(ColorConfig::Auto, Some(codemap));
        emitter.emit(diagnostic);
    }
}

/// C ABI callable from js when targeting wasm.
pub mod web {
    #![allow(clippy::missing_safety_doc)]
    use crate::interp::Value;
    use crate::logging::outln;
    use crate::pool::StringPool;
    use crate::run_main;
    use std::alloc::{alloc, Layout};
    use std::ffi::{c_char, CStr, CString};
    use std::ptr::slice_from_raw_parts;

    /// len does NOT include null terminator.
    #[no_mangle]
    pub unsafe extern "C" fn run(input_query: *const u8, len: usize) {
        let src = &*slice_from_raw_parts(input_query, len);
        let src = match String::from_utf8(src.to_vec()) {
            Ok(src) => src,
            Err(e) => {
                outln!("{:?}", e);
                return;
            }
        };
        let pool = Box::leak(Box::<StringPool>::default());
        run_main(pool, src, Value::Unit, Value::Unit, None);
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

    #[cfg(target_arch = "wasm32")]
    extern "C" {
        pub fn console_log(ptr: *const u8, len: usize);
        pub fn timestamp() -> f64;
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
