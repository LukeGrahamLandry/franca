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

fn main() {
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
    let src = r#"
        fn main(n: i64) i64 = { add(n, n) }
    "#;
    assert_eq!(
        main_int_to_int("fn main(n: i64) i64 = { add(n, n) }", Value::I64(5)),
        Value::I64(10)
    );
    // assert_eq!(
    //     main_int_to_int("fn main(n: i64) i64 = add(n, n);", Value::I64(5)),
    //     Value::I64(10)
    // );
}

fn main_int_to_int(src: &str, arg: Value) -> Value {
    let mut p = Parser::new();
    p.set_language(tree_sitter_inferd::language());
    let pool = StringPool::default();
    let ast = WalkParser::parse(p, src, &pool);
    let mut program = Program::default();
    let mut interp = Interp::new(&pool, &mut program);
    interp.add_declarations(ast);
    let f = interp.lookup_unique_func(pool.intern("main")).unwrap();
    let result = interp.run(f, arg);
    println!("R: {result:?}");
    result
}
