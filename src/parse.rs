//! Convert a stream of tokens into ASTs.
#![deny(unused_must_use)]

use std::{fmt::Debug, ops::Deref, panic::Location, sync::Arc};

use codemap::{File, Span};
use codemap_diagnostic::{Diagnostic, Level, SpanLabel, SpanStyle};

use crate::ast::{Binding, Name, TypeId, Var};
use crate::{
    ast::{Annotation, Expr, FatExpr, FatStmt, Func, Known, LazyType, Pattern, Stmt},
    bc::Value,
    lex::{Lexer, Token, TokenType},
    logging::{outln, LogTag::Parsing, PoolLog},
    pool::{Ident, StringPool},
};
use TokenType::*;

pub struct Parser<'a, 'p> {
    pool: &'p StringPool<'p>,
    expr_id: usize,
    lexer: Lexer<'a, 'p>,
    spans: Vec<Span>,
}

type Res<T> = Result<T, ParseErr>;

#[derive(Debug)]
pub struct ParseErr {
    pub loc: &'static Location<'static>,
    pub diagnostic: Vec<Diagnostic>,
}

impl<'a, 'p> Parser<'a, 'p> {
    pub fn parse(file: Arc<File>, pool: &'p StringPool<'p>) -> Res<Vec<FatStmt<'p>>> {
        outln!(Parsing, "\n######################################\n### START FILE: {} \n######################################\n", file.name());

        let mut p = Parser {
            pool,
            lexer: Lexer::new(file.source(), pool, file.span),
            expr_id: 0,
            spans: vec![],
        };

        p.start_subexpr();
        let mut stmts: Vec<FatStmt<'p>> = vec![];
        while p.peek() != Eof {
            stmts.push(p.parse_stmt()?);
        }
        let _full = p.end_subexpr();
        debug_assert!(p.spans.is_empty(), "leaked parse loc context");

        Ok(stmts)
    }

    fn parse_expr(&mut self) -> Res<FatExpr<'p>> {
        let modifiers = [
            Amp,
            Star,
            Question,
            DoubleSquare,
            DoubleSquigle,
            Bang,
            UpArrow,
        ];
        let mut found = vec![];
        while modifiers.contains(&self.peek()) {
            found.push(self.pop());
        }

        let prefix = self.parse_expr_inner()?;
        self.maybe_parse_suffix(prefix)
    }

    fn parse_expr_inner(&mut self) -> Res<FatExpr<'p>> {
        match self.peek() {
            // TODO: use no body as type expr?
            // No name, require body, optional (args).
            Fn => {
                self.start_subexpr();
                let loc = self.eat(Fn)?;
                let name = if let Symbol(i) = self.peek() {
                    self.pop();
                    // return Err(self.error_next("Fn expr must not have name".into()));
                    Some(i)
                } else {
                    None
                };
                // Args are optional so you can do `if(a, fn=b, fn=c)`
                let arg = if self.maybe(LeftParen) {
                    let a = self.parse_args()?;
                    self.eat(RightParen)?;
                    a
                } else {
                    Pattern::empty(loc)
                };

                let ret = if Equals != self.peek() {
                    LazyType::PendingEval(self.parse_expr()?)
                } else {
                    LazyType::Infer
                };
                self.eat(Equals)?;
                let body = self.parse_expr()?;

                let name = name.unwrap_or_else(|| {
                    let mut name = body.expr.log(self.pool);
                    name.truncate(25);
                    self.pool.intern(&name)
                });

                let func = Func::new(name, arg, ret, Some(body), loc, false);
                Ok(self.expr(Expr::Closure(Box::new(func))))
            }
            LeftSquiggle => {
                self.start_subexpr();
                self.eat(LeftSquiggle)?;
                let mut body = vec![];
                while self.peek() != RightSquiggle {
                    body.push(self.parse_stmt()?);
                }

                let result = if let Some(s) = body.last() {
                    if let Stmt::Eval(_) = s.deref() {
                        if let Stmt::Eval(e) = body.pop().unwrap().stmt {
                            e
                        } else {
                            unreachable!()
                        }
                    } else {
                        self.start_subexpr();
                        self.expr(Expr::unit())
                    }
                } else {
                    self.start_subexpr();
                    self.expr(Expr::unit())
                };

                self.eat(RightSquiggle)?;
                Ok(self.expr(Expr::Block {
                    body,
                    result: Box::new(result),
                    locals: None,
                }))
            }
            DotLeftSquiggle => {
                self.start_subexpr();
                self.eat(DotLeftSquiggle)?;
                let pattern = self.parse_args()?;
                self.eat(RightSquiggle)?;

                Ok(self.expr(Expr::StructLiteralP(pattern)))
            }
            LeftParen => self.parse_tuple(),
            Number(f) => {
                self.start_subexpr();
                self.pop();
                Ok(self.expr(Expr::Value {
                    ty: TypeId::i64(),
                    value: Value::I64(f).into(),
                }))
            }
            Symbol(i) => {
                self.start_subexpr();
                self.pop();
                Ok(self.expr(Expr::GetNamed(i)))
            }
            Quoted(i) => {
                self.start_subexpr();
                self.pop();
                // TODO: maybe its wrong to both putting everything in the pool
                Ok(self.expr(Expr::String(i)))
            }
            // TODO: allow this as a raw statement, currently it will get parsed as an annotation on a noop.
            // TODO: should i allow the payload to be statement too?
            // TODO: could make arg optional but then there's wierd stuff like `@no_arg (expected, target)` being seen as one thing.
            //       i dont want to make parsing depend on how the macro was declared so you could opt out of the arg and allow that.
            At => {
                self.start_subexpr();
                self.eat(At)?;
                let name = self.ident()?;
                let arg = Box::new(self.parse_tuple()?);
                let target = Box::new(self.parse_expr()?);
                Ok(self.expr(Expr::PrefixMacro {
                    name: Var(name, 0),
                    arg,
                    target,
                }))
            }
            _ => Err(self.expected("Expr === 'fn' or '{' or '(' or '\"' or Num or Ident...")),
        }
    }

    fn maybe_parse_suffix(&mut self, mut prefix: FatExpr<'p>) -> Res<FatExpr<'p>> {
        loop {
            prefix = match self.peek() {
                LeftParen => {
                    self.start_subexpr();
                    let arg = self.parse_tuple()?;
                    self.expr(Expr::Call(Box::new(prefix), Box::new(arg)))
                }
                Bang => {
                    self.start_subexpr();
                    self.eat(Bang)?;
                    let name = self.ident()?;
                    self.expr(Expr::SuffixMacro(name, Box::new(prefix)))
                }
                Dot => {
                    self.start_subexpr();
                    self.eat(Dot)?;
                    let name = self.ident()?;
                    self.expr(Expr::FieldAccess(Box::new(prefix), name))
                }
                DoubleSquare => {
                    self.start_subexpr();
                    self.eat(DoubleSquare)?;
                    self.expr(Expr::SuffixMacro(
                        self.pool.intern("deref"),
                        Box::new(prefix),
                    ))
                }
                LeftSquare => {
                    self.eat(LeftSquare)?;
                    // TODO: a[b] or a[b] = c
                    //       new plan is treat those the same and have the `=` statement cope with all place expr stuff.
                    return Err(self.todo("index expr"));
                }
                _ => return Ok(prefix),
            };
        }
    }

    fn parse_tuple(&mut self) -> Res<FatExpr<'p>> {
        self.start_subexpr();
        self.eat(LeftParen)?;
        let args = self.comma_sep_expr()?;
        self.eat(RightParen)?;
        Ok(match args.len() {
            0 => self.expr(Expr::unit()),
            1 => {
                self.end_subexpr();
                args.into_iter().next().unwrap()
            }
            _ => self.expr(Expr::Tuple(args)),
        })
    }

    fn parse_stmt(&mut self) -> Res<FatStmt<'p>> {
        self.start_subexpr();
        let annotations = self.parse_annotations()?;
        let stmt = match self.peek() {
            // Require name, optional body.
            Fn => {
                let loc = self.lexer.nth(0).span;
                self.eat(Fn)?;
                let name = self.ident()?;

                self.eat(LeftParen)?;
                let arg = self.parse_args()?;
                self.eat(RightParen)?;

                let ret = if Equals != self.peek() && Semicolon != self.peek() {
                    LazyType::PendingEval(self.parse_expr()?)
                } else {
                    LazyType::Infer
                };

                let body = match self.peek() {
                    Semicolon => {
                        self.eat(Semicolon)?;
                        None
                    }
                    Equals => {
                        self.eat(Equals)?;
                        Some(self.parse_expr()?)
                    }
                    _ => return Err(self.expected("'='Expr for fn body OR ';' for ffi decl.")),
                };

                let func = Func::new(name, arg, ret, body, loc, true);
                Stmt::DeclFunc(func)
            }
            Qualifier(kind) => {
                self.pop();
                let binding = self.parse_type_binding()?;
                let value = if Equals == self.peek() {
                    self.eat(Equals)?;
                    Some(self.parse_expr()?)
                } else {
                    None
                };
                // TODO: allow multiple bindings
                let (name, ty) = match binding.name {
                    Name::Ident(i) => (i, binding.ty),
                    Name::Var(_) => unreachable!(),
                    Name::None => panic!("var decl needs name. {:?}", binding.ty),
                };

                self.eat(Semicolon)?;
                // TODO: this could just carry the pattern forward.
                Stmt::DeclNamed {
                    name,
                    ty,
                    value,
                    kind,
                }
            }
            Semicolon => {
                self.eat(Semicolon)?;
                Stmt::Noop
            }
            _ => {
                let e = self.parse_expr()?;
                let s = if self.maybe(Equals) {
                    let value = self.parse_expr()?;
                    Stmt::Set { place: e, value }
                } else {
                    // Note: don't eat the semicolon so it shows up as noop for last stmt in block loop.
                    Stmt::Eval(e)
                };
                if !matches!(self.peek(), Semicolon | RightSquiggle) {
                    return Err(self.expected("';' (discard) or '}' (return) after expr stmt"));
                }
                s
            }
        };
        Ok(self.stmt(annotations, stmt))
    }

    // | @name(args) |
    fn parse_annotations(&mut self) -> Res<Vec<Annotation<'p>>> {
        let mut annotations = vec![];
        while let At = self.peek() {
            self.eat(At)?;
            let name = self.ident()?;
            let args = if LeftParen == self.peek() {
                Some(self.parse_tuple()?)
            } else {
                None
            };
            annotations.push(Annotation { name, args });
        }
        Ok(annotations)
    }

    /// `Expr, Expr, ` ends at `)`
    fn comma_sep_expr(&mut self) -> Res<Vec<FatExpr<'p>>> {
        let mut values: Vec<FatExpr<'p>> = vec![];

        if RightParen != self.peek() {
            loop {
                values.push(self.parse_expr()?);
                match self.peek() {
                    Comma => {
                        self.eat(Comma)?;
                        if RightParen == self.peek() {
                            break;
                        }
                    }
                    RightParen => {
                        // No trailing comma is fine
                        break;
                    }
                    _ => {
                        return Err(self.expected("',' or ')' after tuple element"));
                    }
                }
            }
        }

        Ok(values)
    }

    // TODO: its a bit weird that im using a pattern for both of those.
    //       its good for struct decl to be pattern that the instantiation has to match.
    //       and i like struct decl being just an instantiation of a map of types so i think this is good.
    //       do i like letting you do pattern matchy things in struct decls? That's kinda a cool side effect.
    /// Used for fn sig args and also struct literals.
    /// `Names: Expr, Names: Expr` ends with ')' or '}' or missing comma
    fn parse_args(&mut self) -> Res<Pattern<'p>> {
        let mut args = Pattern::empty(self.lexer.nth(0).span);

        loop {
            match self.peek() {
                RightParen | RightSquiggle => break,
                Comma => return Err(self.expected("Expr")),
                _ => {
                    args.bindings.push(self.parse_type_binding()?);
                    if Comma == self.peek() {
                        // inner and optional trailing
                        self.eat(Comma)?;
                    } else {
                        // No trailing comma is fine
                        break;
                    }
                }
            }
        }

        while RightParen != self.peek() && RightSquiggle != self.peek() {
            args.bindings.push(self.parse_type_binding()?);
            if Comma == self.peek() {
                // inner and optional trailing
                self.eat(Comma)?;
            } else {
                // No trailing comma is fine
                break;
            }
        }

        if args.bindings.is_empty() {
            args.bindings.push(Binding {
                name: Name::None,
                ty: LazyType::Finished(TypeId::unit()),
            });
        }

        Ok(args)
    }

    // TODO: rn just one ident but support tuple for pattern matching
    /// `Names ':' ?Expr`
    fn parse_type_binding(&mut self) -> Res<Binding<'p>> {
        let name = self.ident()?;
        let types = if Colon == self.peek() {
            self.pop();
            LazyType::PendingEval(self.parse_expr()?)
        } else {
            LazyType::Infer
        };
        Ok(Binding {
            name: Name::Ident(name),
            ty: types,
        })
    }

    #[track_caller]
    fn ident(&mut self) -> Res<Ident<'p>> {
        if let TokenType::Symbol(i) = self.peek() {
            self.pop();
            Ok(i)
        } else {
            Err(self.expected("Ident"))
        }
    }

    #[track_caller]
    fn maybe(&mut self, ty: TokenType) -> bool {
        if self.peek() == ty {
            self.pop();
            true
        } else {
            false
        }
    }

    #[track_caller]
    fn eat(&mut self, ty: TokenType) -> Res<Span> {
        if self.peek() == ty {
            let t = self.pop();
            Ok(t.span)
        } else {
            Err(self.expected(&format!("{ty:?}")))
        }
    }

    // start a new subexpression
    // this must be called once for every .expr because that calls end
    fn start_subexpr(&mut self) {
        self.spans.push(self.lexer.nth(0).span);
    }

    // return the span of the working expression but also add it to the previous one.
    #[track_caller]
    fn end_subexpr(&mut self) -> Span {
        let done = self.spans.pop().unwrap();
        if let Some(prev) = self.spans.last_mut() {
            *prev = prev.merge(done);
        }
        done
    }

    // get a token and track its span on the working expression
    #[track_caller]
    fn pop(&mut self) -> Token<'p> {
        let t = self.lexer.next();
        let prev = self.spans.last().unwrap();
        *self.spans.last_mut().unwrap() = prev.merge(t.span);
        t
    }

    fn peek(&mut self) -> TokenType<'p> {
        self.lexer.nth(0).kind
    }

    #[track_caller]
    fn expr(&mut self, expr: Expr<'p>) -> FatExpr<'p> {
        outln!(
            Parsing,
            "{}({}) EXPR {}",
            "=".repeat(self.spans.len() * 2),
            self.spans.len(),
            expr.log(self.pool)
        );
        self.expr_id += 1;
        FatExpr {
            expr,
            loc: self.end_subexpr(),
            id: self.expr_id,
            ty: TypeId::unknown(),
            known: Known::Maybe,
        }
    }

    #[track_caller]
    fn stmt(&mut self, annotations: Vec<Annotation<'p>>, stmt: Stmt<'p>) -> FatStmt<'p> {
        outln!(
            Parsing,
            "{}({}) STMT {:?} {}",
            "=".repeat(self.spans.len() * 2),
            self.spans.len(),
            annotations
                .iter()
                .map(|a| self.pool.get(a.name))
                .collect::<Vec<_>>(),
            stmt.log(self.pool)
        );
        FatStmt {
            stmt,
            annotations,
            loc: self.end_subexpr(),
        }
    }

    #[track_caller]
    fn todo(&mut self, msg: &str) -> ParseErr {
        self.error_next(format!("Not yet implemented: {msg}."))
    }

    #[track_caller]
    fn error_next(&mut self, message: String) -> ParseErr {
        let token = self.lexer.next();
        let last = if self.spans.len() < 2 {
            *self.spans.last().unwrap()
        } else {
            self.spans[self.spans.len() - 1]
        };
        ParseErr {
            loc: Location::caller(),
            diagnostic: vec![Diagnostic {
                level: Level::Error,
                message,
                code: None,
                spans: vec![
                    SpanLabel {
                        span: last,
                        label: None,
                        style: SpanStyle::Secondary,
                    },
                    SpanLabel {
                        span: token.span,
                        label: Some(String::from("next")),
                        style: SpanStyle::Primary,
                    },
                ],
            }],
        }
    }

    #[track_caller]
    fn expected(&mut self, msg: &str) -> ParseErr {
        let s = format!("Expected: {msg} but found {:?}", self.peek());
        self.error_next(s)
    }
}
