#![allow(unused)]

use tree_sitter::Parser;

use crate::{interp::Interp, parse::WalkParser, pool::StringPool};

mod ast;
mod interp;
mod parse;
mod pool;

fn main() {
    let mut p = Parser::new();
    p.set_language(tree_sitter_inferd::language());
    // let src = include_str!("lib/builtins.txt");
    let src = r#"
        fn get(arr: &Array(T), i: i64) &T;
        fn swap(ptr: &T, new: T) T = {
            let old = get(ptr);
            set(ptr, new);
            old
        }
        
        call(fn(a: Int) = { a });
        
        // fn call(fn(Int));
        // call(fn = { $0 });
    "#;
    let pool = StringPool::default();
    let p = WalkParser::parse(p, src, &pool);
    let interp = Interp::new(&pool);
}
