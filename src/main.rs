#![allow(unused)]

use std::time::Instant;

use ast::Program;
use interp::Value;
use tree_sitter::Parser;

use crate::{
    interp::{ExecTime, Interp},
    parse::WalkParser,
    pool::StringPool,
};

macro_rules! log {
    // Using cfg!(...) instead of #[cfg(...)] to avoid unused var warnings.
    ($($arg:tt)*) => {{
        if cfg!(feature = "logging") {
            print!($($arg)*);
        }
    }};
}
macro_rules! logln {
    // Using cfg!(...) instead of #[cfg(...)] to avoid unused var warnings.
    ($($arg:tt)*) => {{
        if cfg!(feature = "logging") {
            println!($($arg)*);
        }
    }};
}
pub(crate) use log;
pub(crate) use logln;

mod ast;
mod interp;
mod parse;
mod pool;
mod scope;

macro_rules! check_op {
    ($name:expr, $op:tt) => {{
        let cases = [5, 10, -5];
        for n in cases {
            let src = format!("fn main(n: i64) i64 = {{ {0}(n, n) }}", $name);
            run_main(&src, Value::I64(n), Value::I64(n $op n));
            let src = format!("fn main(n: i64) bool = {{ {0}({0}(n, n), n) }}", $name);
                run_main(&src, Value::I64(n), Value::I64((n $op n) $op n));
        }
    }};
}

macro_rules! check_cmp {
    ($name:expr, $op:tt) => {{
        let cases = [5, 10, -5];
        for n in cases {
            let src = format!("fn main(n: i64) bool = {{ {0}(n, n) }}", $name);
            run_main(&src, Value::I64(n), Value::Bool(n $op n));
        }
    }};
}

fn main() {
    run_tests_txt();
    // let src = include_str!("lib/builtins.txt");
    // let src = r#"
    //     fn get(arr: &Array(T), i: i64) &T;
    //     fn swap(ptr: &T, new: T) T = {
    //         let old = get(ptr);
    //         set(ptr, new);
    //         old
    //     }
    //     call(fn(a: Int) = { a });
    //     // fn call(fn(Int));
    //     // call(fn = { $0 });
    // "#;
}

#[test]
fn tests_txt() {
    run_tests_txt();
}

fn run_tests_txt() {
    run_main(include_str!("tests.txt"), Value::I64(0), Value::I64(0));
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
    );
}

fn run_main(src: &str, arg: Value, expect: Value) {
    let start = Instant::now();
    let mut p = Parser::new();
    p.set_language(tree_sitter_inferd::language());
    let pool = StringPool::default();
    let ast = WalkParser::parse(p, src, &pool);
    let mut program = Program::default();
    let mut interp = Interp::new(&pool, &mut program);
    interp.add_declarations(ast);
    let f = interp.lookup_unique_func(pool.intern("main")).unwrap();
    let result = interp.run(f, arg.clone(), ExecTime::Runtime).unwrap();
    logln!("{arg:?} -> {result:?}");
    assert_eq!(result, expect);
    interp.write_jitted().iter().for_each(|f| logln!("{}", f));
    // TODO: change this when i add assert(bool)
    let assertion_count = src.split("assert_eq(").count() - 1;
    assert_eq!(interp.assertion_count, assertion_count);
    program.log_cached_types();
    println!("{assertion_count} assertions passed.");
    let end = Instant::now();
    let seconds = (end - start).as_secs_f32();
    let lines = src
        .split('\n')
        .filter(|s| !s.split("//").next().unwrap().is_empty())
        .count();
    println!(
        "Finished {lines} (non comment/empty) lines in {seconds:.5} seconds ({:.0} lines per second).",
        lines as f32 / seconds
    );
}
