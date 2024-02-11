use std::{ops::Deref, rc::Rc};

use codemap::{CodeMap, Span, SpanLoc};
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle};
use tree_sitter::{Language, Node, Parser, Point, Tree, TreeCursor};

// TODO: this sucks ass. very possible im just using the api wrong because i didn't read the docs, i just looked at the functions i could call.
//       or maybe the anti-parser-generator people were right.
//       one thing i dont understand: they always say you need a manually written one for good error reporting,
//       but are syntax errors really the thing you care about?maybe im just brainwashed by rust being super agressive about other stuff.
// I FUCKING TAKE IT BACK I HATE TREE SITTER SO MUCH

use crate::{
    ast::{
        Annotation, Expr, FatExpr, FatStmt, Field, Func, Known, LazyFnType, LazyType, Stmt, TypeId,
        VarInfo, VarType,
    },
    interp::Value,
    logging::PoolLog,
    pool::{Ident, StringPool},
    scope::ResolveScope,
};

#[macro_use]
use crate::logging::{logln, log};

pub struct WalkParser<'a, 'p> {
    pool: &'a StringPool<'p>,
    src: &'a [u8],
    expr_id: usize,
}

fn print_but_not_fucking_stupid(
    child_id: usize,
    src: &[u8],
    depth: usize,
    node: Node,
    limit: usize,
) {
    #[cfg(not(feature = "spam_log"))]
    {
        return;
    }

    log!("{}({child_id}): {}", "=".repeat(depth * 2), node.kind());
    if node.kind() == "identifier" || node.kind() == "names" {
        log!(" |{}|", node.utf8_text(src).unwrap());
    }
    if node.start_position().row != node.end_position().row {
        logln!(
            "   [Lines {} to {}]",
            node.start_position().row + 1,
            node.end_position().row + 1
        );
    } else {
        logln!("   [Line {}]", node.start_position().row + 1);
    }

    if limit == 0 {
        return;
    }
    for (i, child) in node.children(&mut node.walk()).enumerate() {
        print_but_not_fucking_stupid(i, src, depth + 1, child, limit - 1);
    }
}

fn print_for_error(child_id: usize, src: &[u8], depth: usize, node: Node) {
    if node.has_error() {
        print!("{}({child_id}): {}", "=".repeat(depth * 2), node.kind());
        if node.kind() == "identifier" || node.kind() == "names" || node.is_error() {
            print!(" |{}|", node.utf8_text(src).unwrap());
        }
        if node.start_position().row != node.end_position().row {
            print!(
                "   [Lines {} to {},  ",
                node.start_position().row + 1,
                node.end_position().row + 1
            );
        } else {
            print!("   [Line {},  ", node.start_position().row + 1);
        }
        println!("Col {}]", node.start_position().column + 1);
    }

    for (i, child) in node.children(&mut node.walk()).enumerate() {
        print_for_error(i, src, depth + 1, child);
    }
}

impl<'a, 'p> WalkParser<'a, 'p> {
    pub fn parse(mut p: Parser, src: &'p str, pool: &'a StringPool<'p>) -> Vec<FatStmt<'p>> {
        logln!("SRC:\n{src}");
        let tree = p.parse(src, None).unwrap();
        logln!("PARSE:\n{}", tree.root_node().to_sexp());
        print_but_not_fucking_stupid(0, src.as_bytes(), 1, tree.root_node(), 9999);
        let mut p = WalkParser {
            pool,
            src: src.as_bytes(),
            expr_id: 0,
        };

        let mut tree = tree.walk();
        tree.goto_first_child();
        let mut stmts = vec![];
        stmts.push(p.parse_stmt(&mut tree));
        while tree.goto_next_sibling() {
            stmts.push(p.parse_stmt(&mut tree));
        }

        for s in &stmts {
            logln!("finished stmt: {}", s.log(pool))
        }

        stmts
    }

    fn parse_stmt(&mut self, cursor: &mut TreeCursor) -> FatStmt<'p> {
        let node = cursor.node();
        assert_eq!(node.kind(), "statement");

        let mut cursor = node.walk();
        let mut entries = node.children_by_field_name("annotation", &mut cursor);

        let mut annotations = vec![];
        for annotation in entries {
            self.check_err(annotation);
            self.assert_literal(annotation.child(0).unwrap(), "@");
            let name = annotation.child(1).unwrap();
            let name = name.utf8_text(self.src).unwrap();
            let name = self.pool.intern(name);
            let args = annotation.child(2).map(|args| self.parse_expr(args.walk()));
            annotations.push(Annotation { name, args })
        }

        let stmt = self.parse_stmt_inner(&mut cursor);
        stmt.fat_with(annotations)
    }

    fn parse_stmt_inner(&mut self, cursor: &mut TreeCursor) -> Stmt<'p> {
        let node = cursor.node();
        let mut cursor = node.walk();
        // logln!("Parse Stmt {}", node.to_sexp());
        logln!("PARSE STMT:");
        print_but_not_fucking_stupid(0, self.src, 0, node, 2);
        let stmt = match node.kind() {
            "func_def" => {
                let func = self.parse_func(node);
                Stmt::DeclFunc(func)
            }
            "expr" => Stmt::Eval(self.parse_expr(cursor)),
            "declare" => {
                let qualifier = node.child_by_field_name("kind").unwrap();
                let qualifier = qualifier.utf8_text(self.src).unwrap();
                let qualifier = match qualifier {
                    "let" => VarType::Let,
                    "var" => VarType::Var,
                    "const" => VarType::Const,
                    _ => panic!("Expected let/var/const but found {:?}", qualifier),
                };
                let binding = self.parse_binding(node.child(1).unwrap());

                let value = node
                    .child_by_field_name("value")
                    .map(|value| self.parse_expr(value.walk()));

                Stmt::DeclNamed {
                    name: binding.0.expect("binding name"),
                    ty: binding.1,
                    value,
                    kind: qualifier,
                }
            }
            "assign" => {
                let names = self.parse_expr(node.child(0).unwrap().walk());
                self.assert_literal(node.child(1).unwrap(), "=");
                let value = self.parse_expr(node.child(2).unwrap().walk());
                let name = match names.deref() {
                    Expr::GetNamed(i) => i,
                    _ => todo!("assign to {names:?}"),
                };
                Stmt::SetNamed(*name, value)
            }
            ";" => Stmt::Noop,
            s => {
                if let Some(parent) = node.parent() {
                    logln!("ERROR PARENT:");
                    logln!("{}", parent.utf8_text(self.src).unwrap());
                    logln!("ERROR TEXT:");
                    logln!("{}", node.utf8_text(self.src).unwrap());
                }
                todo!("Wanted Stmt found {s}: {:?}\n {}", node, node.to_sexp())
            }
        };
        logln!("GOT STMT: \n-{stmt:?} \n-{}", stmt.log(self.pool));
        logln!("================");
        stmt
    }

    fn assert_literal(&self, node: Node, expected: &str) {
        let name = node.utf8_text(self.src).unwrap();
        assert_eq!(name, expected);
    }

    /// <Expr>!<?Ident>
    fn parse_expr(&mut self, mut cursor: TreeCursor) -> FatExpr<'p> {
        let node = cursor.node();
        self.check_err(node);
        logln!("PARSE EXPR");
        print_but_not_fucking_stupid(0, self.src, 0, node, 3);
        logln!("=============");
        if node.is_error() || node.is_missing() || node.has_error() {
            print_for_error(0, self.src, 0, node);
            panic!(
                "PARSE ERROR: {}\n===\n{}\n===",
                node.start_position(),
                node.utf8_text(self.src).unwrap()
            );
        }
        let expr = if node.kind() == "expr" {
            let expr = self.parse_expr_inner(node.child(0).unwrap().walk());

            let mut bang = node
                .children(&mut cursor)
                .filter(|n| n.kind() == "suffix_macro");
            let macro_node = bang.next();
            logln!("suffix macro is {:?}", macro_node);
            let macro_name = macro_node.map(|result| {
                self.assert_literal(result.child(0).unwrap(), "!");
                self.parse_expr(result.child(1).unwrap().walk())
            });
            assert!(bang.next().is_none());

            match macro_name {
                Some(name_expr) => {
                    if let &Expr::GetNamed(i) = name_expr.deref() {
                        self.expr(
                            Expr::SuffixMacro(i, Box::new(expr)),
                            name_expr.loc,
                            Known::Maybe,
                        )
                    } else {
                        panic!("Suffix macro must be an identifier not {name_expr:?}")
                    }
                }
                None => expr,
            }
        } else {
            // TODO: try to never get here because it probably means there's somewhere you cant do `expr!thing`.
            self.parse_expr_inner(cursor)
        };
        logln!("FOUND EXPR: {:?}", expr.log(self.pool));
        logln!("=============");
        expr
    }

    /// Expr
    fn parse_expr_inner(&mut self, mut cursor: TreeCursor) -> FatExpr<'p> {
        let node = cursor.node();
        logln!("Parse Expr: {:?}", cursor.node().to_sexp());
        match node.kind() {
            "identifier" => self.expr(
                Expr::GetNamed(self.parse_ident(node)),
                node.start_position(),
                Known::Maybe,
            ),
            "type_expr" => {
                let child = node.child(0).unwrap();
                let kind = child.kind();
                if kind == "&" {
                    let inner = node.child(1).unwrap();
                    let e = Expr::RefType(Box::new(self.parse_expr(inner.walk())));
                    self.expr(e, node.start_position(), Known::Foldable)
                } else if kind == "tuple" {
                    self.parse_tuple(node) // TODO: this shouldnt be a type_expr branch
                } else {
                    todo!("Unknown typeexpr kind {kind}");
                }
            }
            "tuple" => self.parse_tuple(node),
            "closure_expr" => {
                let func = self.parse_func(node);
                assert!(func.body.is_some(), "CLOSURE MISSING BODY: \n{:?}", func);
                self.expr(
                    Expr::Closure(Box::new(func)),
                    node.start_position(),
                    Known::Foldable,
                )
            }
            "call_expr" => {
                let f = node.child(0).unwrap();
                let args = node.child(1).unwrap();
                let f = self.parse_expr(f.walk());
                let args = self.parse_expr(args.walk());
                self.expr(
                    Expr::Call(Box::new(f), Box::new(args)),
                    node.start_position(),
                    Known::Maybe,
                )
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
                    Ok(i) => self.expr(
                        Expr::Value(Value::I64(i)),
                        node.start_position(),
                        Known::Foldable,
                    ),
                    Err(e) => todo!("{:?}", e),
                }
            }
            "block" => {
                let mut cursor = node.walk();
                let mut stmts = node.children_by_field_name("body", &mut cursor);

                let body_stmts: Vec<_> = stmts
                    .map(|stmt| self.parse_stmt(&mut stmt.walk()))
                    .collect();

                let mut entries = node.children_by_field_name("result", &mut cursor);
                let result = if let Some(result) = entries.next() {
                    logln!("Return {:?}", result.to_sexp());
                    Some(self.parse_expr(result.walk()))
                } else {
                    None
                };
                assert!(entries.next().is_none());

                if body_stmts.is_empty() {
                    result.unwrap_or_else(|| {
                        self.expr(
                            Expr::Value(Value::Unit),
                            node.end_position(),
                            Known::Foldable,
                        )
                    })
                } else {
                    let result = result.unwrap_or_else(|| {
                        self.expr(
                            Expr::Value(Value::Unit),
                            node.end_position(),
                            Known::Foldable,
                        )
                    });
                    self.expr(
                        Expr::Block {
                            body: body_stmts,
                            result: Box::new(result),
                            locals: None,
                        },
                        node.start_position(),
                        Known::Maybe,
                    )
                }
            }
            "map_expr" => {
                let mut fields = vec![];
                for child in node.children(&mut node.walk()) {
                    if child.kind() == "field_decl" {
                        let (name, ty) = self.parse_binding(child.child(0).unwrap());
                        fields.push(Field {
                            name: name.unwrap(),
                            ty: ty.unwrap(),
                        })
                    }
                }
                self.expr(
                    Expr::StructLiteral(fields),
                    node.start_position(),
                    Known::Maybe,
                )
            }
            _ => todo!("parse expr for {}", node.kind()),
        }
    }

    fn parse_binding(&mut self, arg: Node<'_>) -> (Option<Ident<'p>>, Option<FatExpr<'p>>) {
        assert_eq!(arg.kind(), "binding_type");
        logln!("do_parse_binding");
        print_but_not_fucking_stupid(0, self.src, 0, arg, 2);
        let mut cursor = arg.walk();

        let name = self.parse_expr(arg.child(0).unwrap().walk());
        if let Some(result) = arg.child(1) {
            assert_eq!(result.kind(), ":")
        }

        let ty = arg.child(2).map(|result| self.parse_expr(result.walk()));
        logln!("ty: {ty:?}");
        if let Expr::GetNamed(name) = name.deref() {
            (Some(*name), ty)
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
        let mut arg_types: Vec<Option<FatExpr>> = vec![];
        let mut args = proto
            .children_by_field_name("params", &mut cursor)
            .filter(|arg| arg.kind() != ",");
        for arg in args {
            logln!("Arg: {}", arg.to_sexp());
            let (names, ty) = self.parse_binding(arg);
            arg_names.push(names);
            arg_types.push(ty);
        }

        let mut cursor = node.walk();

        let mut entries = node.children_by_field_name("body", &mut cursor);
        let body = if let Some(body) = entries.next() {
            logln!("FUNCTION BODY");
            print_but_not_fucking_stupid(0, self.src, 0, body, 3);
            logln!("=============");

            Some(self.parse_expr(body.walk()))
        } else {
            None
        };
        assert!(entries.next().is_none());

        let any_type_expr = self.expr(
            Expr::Value(Value::Type(TypeId::any())),
            node.start_position(),
            Known::Foldable,
        );

        let arg = match arg_types.len() {
            // Note: this is the *type* `Unit`, NOT the *value* `unit`
            0 => Some(any_type_expr),
            1 => arg_types.into_iter().next().unwrap(),
            _ => Some(
                self.expr(
                    Expr::Tuple(
                        arg_types
                            .into_iter()
                            .map(|ty| ty.unwrap_or_else(|| any_type_expr.clone()))
                            .collect(),
                    ),
                    node.start_position(),
                    Known::Foldable,
                ),
            ),
        };

        let func = Func {
            name,
            ty: LazyFnType::of(arg, return_type),
            body,
            arg_names,
            annotations: vec![],
            arg_vars: None,
            capture_vars: vec![],
            local_constants: Default::default(),
        };
        logln!("GOT FUNC: {}", func.log(self.pool));
        func
    }

    fn expr(&mut self, expr: Expr<'p>, loc: Point, known: Known) -> FatExpr<'p> {
        self.expr_id += 1;
        FatExpr {
            expr,
            loc,
            id: self.expr_id,
            ty: None,
            known,
        }
    }

    fn parse_tuple(&mut self, node: Node<'_>) -> FatExpr<'p> {
        let mut cursor = node.walk();
        let args = node
            .children(&mut cursor)
            .filter(|child| child.kind() != "(" && child.kind() != ")" && child.kind() != ",") // TODO: wtf
            .map(|child| self.parse_expr(child.walk()));
        let e = Expr::Tuple(args.collect());
        self.expr(e, node.start_position(), Known::Maybe)
    }

    fn parse_ident(&self, node: Node<'_>) -> Ident<'p> {
        let name = node.utf8_text(self.src).unwrap();
        self.pool.intern(name)
    }

    fn check_err(&self, node: Node<'_>) {
        if node.has_error() {
            self.error(node);
            panic!("Parse Error")
        }
    }

    fn error(&self, node: Node<'_>) {
        println!("PARSE ERROR");
        println!("========================");
        print_for_error(0, self.src, 0, node);
        println!("========================");

        let mut codemap = CodeMap::new();
        let file = codemap.add_file(
            "src".to_owned(),
            String::from_utf8(self.src.to_owned()).unwrap(),
        );

        let d = Diagnostic {
            level: Level::Error,
            message: "Parse Error".to_owned(),
            code: None,
            spans: collect_errs(file.span, node)
                .into_iter()
                .map(|span| SpanLabel {
                    span,
                    label: None,
                    style: SpanStyle::Primary,
                })
                .collect(),
        };
        let mut emitter = Emitter::stderr(ColorConfig::Auto, Some(&codemap));
        emitter.emit(&[d]);
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

fn collect_errs(root: Span, node: Node<'_>) -> Vec<Span> {
    let mut errs = if node.is_error() {
        let range = node.byte_range();
        return vec![root.subspan(range.start as u64, range.end as u64)];
    } else {
        vec![]
    };

    for child in node.children(&mut node.walk()) {
        errs.extend(collect_errs(root, child));
    }
    errs
}
