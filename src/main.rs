#![allow(unused)]

use std::{env, fs, io::read_to_string, path::PathBuf, time::Instant};

use ast::{Expr, FatExpr, Func, LazyFnType, Program, TypeId};
use interp::Value;
use scope::ResolveScope;
use tree_sitter::Parser;

use crate::{
    interp::{ExecTime, Interp, SharedConstants},
    logging::{logln, PoolLog},
    parse::WalkParser,
    pool::StringPool,
};

mod ast;
mod interp;
mod logging;
mod parse;
mod pool;
mod scope;

macro_rules! check_op {
    ($name:expr, $op:tt) => {{
        let cases = [5, 10, -5];
        for n in cases {
            let src = format!("fn main(n: i64) i64 = {{ {0}(n, n) }}", $name);
            run_main(&src, Value::I64(n), Value::I64(n $op n), None);
            let src = format!("fn main(n: i64) bool = {{ {0}({0}(n, n), n) }}", $name);
                run_main(&src, Value::I64(n), Value::I64((n $op n) $op n), None);
        }
    }};
}

macro_rules! check_cmp {
    ($name:expr, $op:tt) => {{
        let cases = [5, 10, -5];
        for n in cases {
            let src = format!("fn main(n: i64) bool = {{ {0}(n, n) }}", $name);
            run_main(&src, Value::I64(n), Value::Bool(n $op n), None);
        }
    }};
}

fn main() {
    run_main(
        &fs::read_to_string("tests/basic.txt").unwrap(),
        Value::I64(0),
        Value::I64(0),
        Some("target/latest_log/interp.txt"),
    );
}

#[test]
fn tests_txt() {
    run_main(
        &fs::read_to_string("tests/basic.txt").unwrap(),
        Value::I64(0),
        Value::I64(0),
        None,
    );
}

// TODO: since operators are traits, i probably dont need to use a macro for this
#[test]
fn interp_math() {
    check_op!("add", +);
    check_op!("sub", -);
    check_op!("mul", *);
    check_op!("div", /);
    check_cmp!("eq", ==);
    check_cmp!("ne", !=);
    check_cmp!("gt", >);
    check_cmp!("lt", <);
    check_cmp!("le", <=);
    check_cmp!("ge", >=);
}

// TODO: when i add inference, need to check for recursion. err: Recursive functions must specify return type.

#[test]
fn call_user_fn() {
    run_main(
        r#"
            fn main(n: i64) i64 = { add(other(n), n) }
            fn other(n: i64) i64 = { add(n, n) }
            "#,
        Value::I64(5),
        Value::I64(15),
        None,
    );
}

#[test]
fn return_tuple() {
    run_main(
        r#"
                        fn main(n: i64) Tuple(i64, i64) = { tuple(n, add(n, 1)) }
                        "#,
        Value::I64(3),
        Value::Tuple {
            container_type: ast::TypeId::any(), // TODO
            values: vec![Value::I64(3), Value::I64(4)],
        },
        None,
    );
}

#[test]
fn passing_tuple_to_multiargs() {
    run_main(
        r#"
                    fn main(n: i64) i64 = { add(add(two(unit)), n) }
                    fn two(u: Unit) Tuple(i64, i64) = { tuple(1, 2) }
                    "#,
        Value::I64(3),
        Value::I64(6),
        None,
    );
}

fn run_main(src: &str, arg: Value, expect: Value, save: Option<&str>) {
    let prelude = &fs::read_to_string("lib/interp_builtins.txt").unwrap();
    let full_src = format!("{}\n{}", prelude, src); // TODO: this means wrong line numbers

    let start = Instant::now();
    let pool = StringPool::default();
    let mut p = Parser::new();
    p.set_language(tree_sitter_inferd::language());
    let stmts = WalkParser::parse(p, &full_src, &pool);

    let mut global = Func {
        annotations: vec![],
        name: Some(pool.intern("@toplevel@")),
        ty: LazyFnType::Finished(TypeId::any(), TypeId::any()),
        body: Some(FatExpr::synthetic(ast::Expr::Block {
            body: stmts,
            result: Box::new(FatExpr::synthetic(Expr::Value(Value::Unit))),
            locals: None,
        })),
        arg_names: vec![],
        arg_vars: None,
        capture_vars: vec![],
        local_constants: vec![],
    };

    logln!("{}", global.log(&pool));

    let vars = ResolveScope::of(&mut global);
    let mut program = Program {
        vars,
        ..Default::default()
    };
    let mut interp = Interp::new(&pool, &mut program);
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
                let lines = full_src
                    .split('\n')
                    .filter(|s| !s.split("//").next().unwrap().is_empty())
                    .count();
                println!(
                        "Finished {lines} (non comment/empty) lines in {seconds:.5} seconds ({:.0} lines per second).",
                        lines as f32 / seconds
                    );
            } else {
                println!("{:?}", result.unwrap_err());
            }
        } else {
            println!("FN {name:?} = 'MAIN' NOT FOUND");
        }
    } else {
        println!("{:?}", result.unwrap_err());
    }

    #[cfg(feature = "some_log")]
    if let Some(path) = save {
        let path = PathBuf::from(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(
            &path,
            format!("{}\nAt {:?}", interp.log(&pool), Instant::now()),
        )
        .unwrap();
        println!("Wrote log to {:?}", path);
    }
}
