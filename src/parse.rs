use tree_sitter::{Language, Node, Parser, Tree, TreeCursor};

use crate::{
    ast::{Expr, Stmt},
    pool::StringPool,
};

pub struct WalkParser<'a, 'p> {
    pool: &'a StringPool<'p>,
    src: &'a [u8],
}

impl<'a, 'p> WalkParser<'a, 'p> {
    pub fn parse(mut p: Parser, src: &'p str, pool: &'a StringPool<'p>) {
        let tree = p.parse(src, None).unwrap();
        let mut p = WalkParser {
            pool,
            src: src.as_bytes(),
        };
        let mut tree = tree.walk();
        tree.goto_first_child();
        let mut stmts = vec![];
        stmts.push(p.parse_stmt(&mut tree));
        while tree.goto_next_sibling() {
            stmts.push(p.parse_stmt(&mut tree));
        }
        for s in stmts {
            println!("{}", s.log(p.pool))
        }
    }

    fn parse_stmt(&mut self, cursor: &mut TreeCursor) -> Stmt<'p> {
        let node = cursor.node();
        let mut cursor = node.walk();
        println!("Parse Stmt {}", node.to_sexp());
        if node.kind() == "func_def" {
            let mut entries = node.children_by_field_name("name", &mut cursor);
            let result = entries.next().unwrap();
            assert!(entries.next().is_none());
            let name = result.utf8_text(self.src).unwrap();
            let name = self.pool.intern(name);
            drop(entries);

            println!("Parse Stmt {}", node.to_sexp());

            let mut entries = node.children_by_field_name("proto", &mut cursor);
            let node = entries.next().unwrap();
            assert!(entries.next().is_none());

            let mut cursor = node.walk();

            let mut entries = node.children_by_field_name("return_type", &mut cursor);
            let return_type = entries.next().map(|result| self.parse_expr(result.walk()));
            assert!(entries.next().is_none());

            Stmt::DeclFunc {
                name,
                return_type,
                body: None,
            }
        } else if node.kind() == "call_expr" {
            let f = node.child(0).unwrap();
            let f = Box::new(self.parse_expr(f.walk()));
            let arg = node.child(1).unwrap();
            let arg = Box::new(self.parse_expr(arg.walk()));

            Stmt::Eval(Expr::Call(f, arg))
        } else {
            todo!("Wanted Stmt found {:?}", node)
        }
    }

    fn parse_expr(&mut self, mut cursor: TreeCursor) -> Expr<'p> {
        let node = cursor.node();
        println!("{:?}", cursor.node().to_sexp());
        match node.kind() {
            "identifier" => {
                let name = node.utf8_text(self.src).unwrap();
                let name = self.pool.intern(name);
                Expr::GetVar(name)
            }
            "type_expr" => {
                let child = node.child(0).unwrap();
                let kind = child.kind();
                if kind == "&" {
                    let inner = node.child(1).unwrap();
                    Expr::RefType(Box::new(self.parse_expr(inner.walk())))
                } else {
                    todo!("Unknown typeexpr kind {kind}");
                }
            }
            "tupple" => {
                let mut cursor = node.walk();
                let args = node
                    .children(&mut cursor)
                    .filter(|child| child.kind() != "(" && child.kind() != ")") // TODO: wtf
                    .map(|child| self.parse_expr(child.walk()));
                Expr::Tuple(args.collect())
            }
            "closure_expr" => {
                let mut entries = node.children_by_field_name("proto", &mut cursor);
                let node = entries.next().unwrap();
                assert!(entries.next().is_none());

                let mut cursor = node.walk();

                let mut entries = node.children_by_field_name("return_type", &mut cursor);
                let return_type = entries.next().map(|result| self.parse_expr(result.walk()));
                assert!(entries.next().is_none());

                // Stmt::DeclFunc {
                //     name,
                //     return_type,
                //     body: None,
                // }
                // Expr::Func()
                todo!()
            }
            _ => todo!("parse expr for {}: {:?}", node.kind(), node.to_sexp()),
        }
    }

    // fn find_one(&mut self, cursor: &mut TreeCursor, field_name: &str) {
    //     let node = cursor.node();
    //     println!("Parse Stmt {}", node.to_sexp());
    //     let mut entries = node.children_by_field_name(field_name, cursor);
    //     let result = entries.next().unwrap();
    //     assert!(entries.next().is_none());
    //     result
    // }
}
