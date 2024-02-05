#![allow(unused)]

use ast::Program;
use interp::Value;
use tree_sitter::Parser;

use crate::{
    interp::{ExecTime, Interp},
    parse::WalkParser,
    pool::StringPool,
};

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
    // run_main(
    //     r#"
    //         fn main(n: int()) int() = { add(add(n, n), n) }
    //         @comptime fn int() Type = { i64 }
    //         "#,
    //     Value::I64(5),
    //     Value::I64(15),
    // );

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

#[test]
fn call_in_type_annotation() {
    run_main(
        r#"
                fn main(n: int(unit)) int(unit) = { add(add(n, n), n) }
                @comptime fn int(u: Unit) Type = { i64 }
                "#,
        Value::I64(5),
        Value::I64(15),
    );
}

#[test]
fn simple_if() {
    run_main(
        r#"  fn main(n: i64) i64 = { if(eq(n, 1), fn(a: i64) i64 = { 5 }, fn(a: i64) i64 = { 10) }) }  "#,
        Value::I64(1),
        Value::I64(5),
    );
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
    let result = interp.run(f, arg.clone(), ExecTime::Runtime).unwrap();
    println!("{arg:?} -> {result:?}");
    assert_eq!(result, expect);
    interp.write_jitted().iter().for_each(|f| println!("{}", f));
    program.log_cached_types();
}
