#![allow(unused)]

use std::{env, fs, io::read_to_string, path::PathBuf, time::Instant};

use ast::{Expr, FatExpr, Func, LazyFnType, Program, TypeId};
use codemap::CodeMap;
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle};
use interp::Value;
use scope::ResolveScope;

use crate::{
    ast::FatStmt,
    interp::{ExecTime, Interp, SharedConstants},
    logging::{logln, PoolLog},
    parse::Parser,
    pool::StringPool,
};

mod ast;
mod interp;
mod lex;
mod logging;
mod parse;
mod pool;
mod scope;

fn main() {
    let pool = Box::leak(Box::<StringPool>::default());
    if let Some(name) = env::args().nth(1) {
        run_main(
            pool,
            fs::read_to_string(format!("tests/{name}.txt")).unwrap(),
            Value::I64(0),
            Value::I64(0),
            Some(&name),
        );
        return;
    }

    for case in fs::read_dir("tests").unwrap() {
        let case = case.unwrap();
        println!("TEST: {}", case.file_name().to_str().unwrap());
        run_main(
            pool,
            fs::read_to_string(case.path()).unwrap(),
            Value::I64(0),
            Value::I64(0),
            Some(
                case.file_name()
                    .to_str()
                    .unwrap()
                    .strip_suffix(".txt")
                    .unwrap(),
            ),
        );
    }
}

fn run_main<'a: 'p, 'p>(
    pool: &'a StringPool<'p>,
    src: String,
    arg: Value,
    expect: Value,
    save: Option<&str>,
) {
    let prelude = fs::read_to_string("lib/interp_builtins.txt").unwrap();
    let start = Instant::now();
    let mut codemap = CodeMap::new();
    let lib = codemap.add_file("lib/interp_builtins.txt".into(), prelude.clone());
    let code = codemap.add_file("main_file".into(), src.clone());
    let mut stmts = Vec::<FatStmt<'p>>::new();
    let mut parse = |file| match Parser::parse(&codemap, file, pool, false) {
        Ok(new) => {
            stmts.extend(new);
            true
        }
        Err(e) => {
            println!("Parse error (Internal: {})", e.loc);
            let mut emitter = Emitter::stderr(ColorConfig::Auto, Some(&codemap));
            emitter.emit(&e.diagnostic);
            false
        }
    };

    if !parse(lib.clone()) {
        return;
    }
    if !parse(code) {
        return;
    }

    let mut global = Func {
        annotations: vec![],
        name: Some(pool.intern("@toplevel@")),
        ty: LazyFnType::Finished(TypeId::any(), TypeId::any()),
        body: Some(FatExpr::synthetic(
            ast::Expr::Block {
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

    logln!("{}", global.log(pool));

    let vars = ResolveScope::of(&mut global);
    let mut program = Program {
        vars,
        ..Default::default()
    };
    let mut interp = Interp::new(pool, &mut program);
    // damn turns out defer would maybe be a good idea
    let result = interp.add_declarations(&SharedConstants::default(), global);
    if result.is_ok() {
        let toplevel = interp.lookup_unique_func(pool.intern("@toplevel@"));
        let id = toplevel.unwrap();
        let constants = interp.ready[id.0].as_ref().unwrap().constants.clone();
        let name = pool.intern("main");
        if let Some(f) = interp.lookup_unique_func(name) {
            let result = interp.run(Some(&constants), f, arg.clone(), ExecTime::Runtime);
            if let Ok(result) = result {
                let end = Instant::now();
                logln!("{arg:?} -> {result:?}");
                assert_eq!(result, expect);
                // TODO: change this when i add assert(bool)
                let assertion_count = src.split("assert_eq(").count() - 1;
                assert_eq!(
                    interp.assertion_count, assertion_count,
                    "vm missed assertions?"
                );
                println!(
                    "{assertion_count} assertions passed. {} comptime evaluations.",
                    interp.anon_fn_counter
                );
                let seconds = (end - start).as_secs_f32();
                let lines = format!("{}\n{}", prelude, src)
                    .split('\n')
                    .filter(|s| !s.split("//").next().unwrap().is_empty())
                    .count();
                println!(
                        "Finished {lines} (non comment/empty) lines in {seconds:.5} seconds ({:.0} lines per second).",
                        lines as f32 / seconds
                    );
                let inst_count: usize = interp
                    .ready
                    .iter()
                    .flatten()
                    .map(|func| func.insts.len())
                    .sum();
                println!(
                    "Generated {inst_count} instructions ({:.0} per second).",
                    inst_count as f32 / seconds
                );
            } else {
                let e = result.unwrap_err();
                let d = vec![Diagnostic {
                    level: Level::Error,
                    message: e.reason.log(interp.program, pool),
                    code: None,
                    spans: vec![SpanLabel {
                        span: e.loc.unwrap(),
                        label: None,
                        style: SpanStyle::Primary,
                    }],
                }];
                let mut emitter = Emitter::stderr(ColorConfig::Auto, Some(&codemap));
                emitter.emit(&d);
                println!("{}", e.trace);
            }
        } else {
            println!("FN {name:?} = 'MAIN' NOT FOUND");
        }
    } else {
        let e = result.unwrap_err();
        let d = vec![Diagnostic {
            level: Level::Error,
            message: e.reason.log(interp.program, pool),
            code: None,
            spans: vec![SpanLabel {
                span: e.loc.unwrap(),
                label: None,
                style: SpanStyle::Primary,
            }],
        }];
        let mut emitter = Emitter::stderr(ColorConfig::Auto, Some(&codemap));
        emitter.emit(&d);
        println!("{}", e.trace);
    }

    #[cfg(feature = "some_log")]
    if let Some(path) = save {
        let path = PathBuf::from(format!("target/latest_log/{path}/interp.log"));
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(
            &path,
            format!("{}\nAt {:?}", interp.log(&pool), Instant::now()),
        )
        .unwrap();
        println!("Wrote log to {:?}", path);
    }
    println!("=================================");
}
