use std::rc::Rc;

use tree_sitter::{Language, Node, Parser, Tree, TreeCursor};

// TODO: this sucks ass. very possible im just using the api wrong because i didn't read the docs, i just looked at the functions i could call.
//       or maybe the anti-parser-generator people were right.
//       one thing i dont understand: they always say you need a manually written one for good error reporting,
//       but are syntax errors really the thing you care about?maybe im just brainwashed by rust being super agressive about other stuff.

use crate::{
    ast::{Expr, Func, LazyFnType, LazyType, Stmt},
    interp::Value,
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
        match node.kind() {
            "func_def" => {
                let func = self.parse_func(node);
                Stmt::DeclFunc(func)
            }
            "call_expr" => {
                let f = node.child(0).unwrap();
                let f = Box::new(self.parse_expr(f.walk()));
                let arg = node.child(1).unwrap();
                let arg = Box::new(self.parse_expr(arg.walk()));

                Stmt::Eval(Expr::Call(f, arg))
            }
            "declare" => {
                self.assert_literal(node.child(0).unwrap(), "let");
                let names = self.parse_expr(node.child(1).unwrap().walk());
                println!("{:?}", names);
                self.assert_literal(node.child(2).unwrap(), "=");
                let value = self.parse_expr(node.child(3).unwrap().walk());
                println!("{:?}", value);
                self.assert_literal(node.child(4).unwrap(), ";");
                let name = match names {
                    Expr::GetNamed(i) => i,
                    _ => todo!("assign to {names:?}"),
                };
                Stmt::DeclVar(name, Box::new(value))
            }
            "assign" => {
                let names = self.parse_expr(node.child(0).unwrap().walk());
                self.assert_literal(node.child(1).unwrap(), "=");
                let value = self.parse_expr(node.child(2).unwrap().walk());
                self.assert_literal(node.child(3).unwrap(), ";");
                let name = match names {
                    Expr::GetNamed(i) => i,
                    _ => todo!("assign to {names:?}"),
                };
                Stmt::SetNamed(name, value)
            }
            s => todo!("Wanted Stmt found {s}: {:?}", node),
        }
    }

    fn assert_literal(&self, node: Node, expected: &str) {
        let name = node.utf8_text(self.src).unwrap();
        assert_eq!(name, expected)
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
            "tuple" => {
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
                let func = self.parse_func(node);
                assert!(func.body.is_some());
                Expr::Closure(Box::new(func))
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
            "number" => {
                let text = node.utf8_text(self.src).unwrap();
                match text.parse::<i64>() {
                    Ok(i) => Expr::Value(Value::I64(i)),
                    Err(e) => todo!("{:?}", e),
                }
            }
            _ => todo!("parse expr for {}: {:?}", node.kind(), node.to_sexp()),
        }
    }

    fn parse_binding(&mut self, arg: Node<'_>) -> (Option<Ident<'p>>, Option<Expr<'p>>) {
        assert_eq!(arg.kind(), "binding_type");
        let mut cursor = arg.walk();

        let name = self.parse_expr(arg.child(0).unwrap().walk());
        if let Some(result) = arg.child(1) {
            assert_eq!(result.kind(), ":")
        }

        let ty = arg.child(2).map(|result| self.parse_expr(result.walk()));
        if let Expr::GetNamed(name) = name {
            (Some(name), ty)
        } else {
            panic!("expected argument name found {}", name.log(self.pool));
        }
    }

    fn parse_func(&mut self, node: Node) -> Func<'p> {
        let mut cursor = node.walk();
        let mut entries = node.children_by_field_name("name", &mut cursor);
        let name = entries.next().map(|result| {
            let name = result.utf8_text(self.src).unwrap();
            let name = self.pool.intern(name);
            name
        });
        assert!(entries.next().is_none());
        drop(entries);

        println!("Parse func_def {}", node.to_sexp());

        let mut entries = node.children_by_field_name("proto", &mut cursor);
        let proto = entries.next().unwrap();
        assert!(entries.next().is_none());

        let mut cursor = proto.walk();

        // TODO: its actially down in the tree somehwere
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

            let body_stmts: Vec<_> = stmts
                .map(|stmt| self.parse_stmt(&mut stmt.walk()))
                .collect();

            let mut entries = body.children_by_field_name("result", &mut cursor);
            let result = if let Some(result) = entries.next() {
                println!("Return {:?}", result.to_sexp());
                Some(self.parse_expr(result.walk()))
            } else {
                None
            };
            assert!(entries.next().is_none());

            if body_stmts.is_empty() {
                result
            } else {
                let result = result.unwrap_or_else(|| Expr::Value(Value::Unit));
                Some(Expr::Block(body_stmts, Box::new(result)))
            }
        } else {
            None
        };
        assert!(entries.next().is_none());

        let mut cursor = node.walk();
        let mut entries = node.children_by_field_name("annotation", &mut cursor);
        let annotation: Option<Ident<'p>> = if let Some(annotation) = entries.next() {
            println!("TODO annotation {:?}", annotation.to_sexp());
            None
        } else {
            None
        };
        Func {
            name,
            ty: LazyFnType::of(None, return_type),
            body,
            arg_names,
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
