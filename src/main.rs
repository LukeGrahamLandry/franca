#![allow(unused)]

use ast::Program;
use interp::Value;
use tree_sitter::Parser;

use crate::{interp::Interp, parse::WalkParser, pool::StringPool};

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
    run_main(
        "fn main(n: i64) = { add(add(n, n), n)}",
        Value::I64(5),
        Value::I64(15),
    );
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

fn run_main(src: &str, arg: Value, expect: Value) {
    let mut p = Parser::new();
    p.set_language(tree_sitter_inferd::language());
    let pool = StringPool::default();
    let ast = WalkParser::parse(p, src, &pool);
    let mut program = Program::default();
    let mut interp = Interp::new(&pool, &mut program);
    interp.add_declarations(ast);
    let f = interp.lookup_unique_func(pool.intern("main")).unwrap();
    let result = interp.run(f, arg.clone());
    println!("{arg:?} -> {result:?}");
    assert_eq!(result, expect);
}
