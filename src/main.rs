#![allow(unused)]

use tree_sitter::Parser;

mod ast;
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
        
        fn call(fn(Int));
    "#;
    let tree = p.parse(src, None).unwrap();

    println!("{}", tree.root_node().to_sexp());
}
