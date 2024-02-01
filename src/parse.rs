use tree_sitter::{Language, Node, Parser, Tree, TreeCursor};

use crate::{
    ast::{Expr, Stmt},
    pool::{Ident, StringPool},
};

pub struct WalkParser<'a, 'p> {
    pool: &'a StringPool<'p>,
    src: &'a [u8],
}

impl<'a, 'p> WalkParser<'a, 'p> {
    pub fn parse(mut p: Parser, src: &'p str, pool: &'a StringPool<'p>) -> Vec<Stmt<'p>> {
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
        for s in &stmts {
            println!("finished stmt: {}", s.log(p.pool))
        }
        stmts
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

            println!("Parse func_def {}", node.to_sexp());

            let mut entries = node.children_by_field_name("proto", &mut cursor);
            let proto = entries.next().unwrap();
            assert!(entries.next().is_none());

            let mut cursor = proto.walk();

            let mut entries = proto.children_by_field_name("return_type", &mut cursor);
            let return_type = entries.next().map(|result| self.parse_expr(result.walk()));
            assert!(entries.next().is_none());
            drop(entries);

            let mut arg_names: Vec<Option<Ident>> = vec![];
            let mut args = proto.children_by_field_name("params", &mut cursor);
            for arg in args {
                println!("Arg: {}", arg.to_sexp());
                let (names, ty) = self.parse_binding(arg);
                arg_names.push(names);
            }

            let mut cursor = node.walk();

            let mut entries = node.children_by_field_name("body", &mut cursor);
            let body = if let Some(body) = entries.next() {
                println!("body {:?}", body.to_sexp());
                let mut cursor = body.walk();
                let mut stmts = body.children_by_field_name("body", &mut cursor);
                assert!(stmts.next().is_none()); // TODO.
                drop(stmts);

                let mut entries = body.children_by_field_name("result", &mut cursor);
                let body = if let Some(result) = entries.next() {
                    println!("Return {:?}", result.to_sexp());
                    Some(self.parse_expr(result.walk()))
                } else {
                    None
                };
                assert!(entries.next().is_none());
                body
            } else {
                None
            };
            assert!(entries.next().is_none());

            Stmt::DeclFunc {
                name,
                return_type,
                body,
                arg_names,
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
        println!("Parse Expr: {:?}", cursor.node().to_sexp());
        match node.kind() {
            "identifier" => {
                let name = node.utf8_text(self.src).unwrap();
                let name = self.pool.intern(name);
                Expr::GetNamed(name)
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
                    .filter(|child| {
                        child.kind() != "(" && child.kind() != ")" && child.kind() != ","
                    }) // TODO: wtf
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
            "call_expr" => {
                let f = node.child(0).unwrap();
                let args = node.child(1).unwrap();
                let f = self.parse_expr(f.walk());
                let args = self.parse_expr(args.walk());
                Expr::Call(Box::new(f), Box::new(args))
            }
            "names" => {
                let mut cursor = node.walk();
                let names: Vec<_> = node
                    .children(&mut cursor)
                    .map(|result| self.parse_expr(result.walk()))
                    .collect();
                assert_eq!(names.len(), 1);
                names[0].clone()
            }
            _ => todo!("parse expr for {}: {:?}", node.kind(), node.to_sexp()),
        }
    }

    fn parse_binding(&mut self, arg: Node<'_>) -> (Option<Ident<'p>>, Option<Expr<'p>>) {
        assert_eq!(arg.kind(), "binding_type");
        let mut cursor = arg.walk();

        let name = self.parse_expr(arg.child(0).unwrap().walk());
        arg.child(1).map(|result| assert_eq!(result.kind(), ":")); // TODO
        let ty = arg.child(2).map(|result| self.parse_expr(result.walk()));
        if let Expr::GetNamed(name) = name {
            (Some(name), ty)
        } else {
            panic!("expected argument name found {}", name.log(self.pool));
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
