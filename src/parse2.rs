#![deny(unused_must_use)]

use std::{fmt::Debug, ops::Deref, panic::Location, sync::Arc};

use codemap::{CodeMap, File, Span, SpanLoc};
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle};

use crate::{
    ast::{
        Annotation, Expr, FatExpr, FatStmt, Field, Func, Known, LazyFnType, LazyType, Pattern,
        Stmt, TypeId, VarInfo, VarType,
    },
    interp::Value,
    lex::{Lexer, Token, TokenType},
    logging::PoolLog,
    pool::{Ident, StringPool},
    scope::ResolveScope,
};
use TokenType::*;
#[macro_use]
use crate::logging::{logln, log};

pub struct Parser<'a, 'p> {
    pool: &'p StringPool<'p>,
    src: &'a str,
    expr_id: usize,
    lexer: Lexer<'a, 'p>,
    file: Arc<File>,
    codemap: &'a CodeMap,
    spans: Vec<Span>,
    track_err: bool,
}

type Res<T> = Result<T, ParseErr>;

#[derive(Debug)]
pub struct ParseErr {
    pub loc: &'static Location<'static>,
    pub diagnostic: Vec<Diagnostic>,
}

impl<'a, 'p> Parser<'a, 'p> {
    pub fn parse(
        codemap: &'a CodeMap,
        file: Arc<File>,
        pool: &'p StringPool<'p>,
        track_err: bool,
    ) -> Res<Vec<FatStmt<'p>>> {
        let mut p = Parser {
            file: file.clone(),
            pool,
            lexer: Lexer::new(file.source(), pool, file.span),
            src: file.source(),
            expr_id: 0,
            codemap,
            spans: vec![],
            track_err,
        };

        p.start_subexpr();
        let mut stmts: Vec<FatStmt<'p>> = vec![];
        while p.peek() != Eof {
            stmts.push(p.parse_stmt()?);
            if Semicolon == p.peek() {
                p.eat(Semicolon)?;
            }
        }
        let full = p.end_subexpr();

        for s in &stmts {
            logln!("finished stmt: {}", s.log(pool))
        }

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
                if let Symbol(_) = self.peek() {
                    return Err(self.error_next("Fn expr must not have name".into()));
                }
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

                Ok(self.expr(Expr::Closure(Box::new(Func {
                    annotations: vec![],
                    name: None,
                    ty: LazyFnType::Pending {
                        arg: arg.clone().make_ty(),
                        ret,
                    },
                    body: Some(body),
                    arg_names: vec![],
                    arg_loc: vec![],
                    arg_vars: None,
                    capture_vars: vec![],
                    local_constants: vec![],
                    loc,
                }))))
            }
            LeftSquiggle => {
                self.start_subexpr();
                self.eat(LeftSquiggle)?;
                let mut body = vec![];
                while self.peek() != RightSquiggle {
                    body.push(self.parse_stmt()?);
                    // TODO: make sure stmt doesnt eat the semicolon
                    if !self.maybe(Semicolon) {
                        break;
                    }
                }
                let result = if RightSquiggle == self.peek() {
                    self.start_subexpr();
                    self.expr(Expr::Value(Value::Unit))
                } else {
                    self.parse_expr()?
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
                Ok(self.expr(Expr::Value(Value::I64(f))))
            }
            Symbol(i) => {
                self.start_subexpr();
                self.pop();
                Ok(self.expr(Expr::GetNamed(i)))
            }
            Quoted(i) => Err(self.todo("quoted string")),
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
                LeftSquare => {
                    self.eat(LeftSquare)?;
                    // a[b] or a[b] = c
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
        Ok(self.expr(Expr::Tuple(args)))
    }

    fn parse_stmt(&mut self) -> Res<FatStmt<'p>> {
        self.start_subexpr();
        let annotations = self.parse_annotations()?;
        let stmt = match self.peek() {
            // Require name, optional body.
            Fn => {
                let loc = self.lexer.nth(0).span;
                self.start_subexpr();
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

                let body = match self.pop().kind {
                    Semicolon => None,
                    Equals => Some(self.parse_expr()?),
                    _ => return Err(self.expected("'='Expr for fn body OR ';' for ffi decl.")),
                };

                Stmt::DeclFunc(Func {
                    annotations: vec![],
                    name: Some(name),
                    ty: LazyFnType::Pending {
                        arg: arg.clone().make_ty(),
                        ret,
                    },
                    body,
                    arg_names: vec![],
                    arg_loc: vec![],
                    arg_vars: None,
                    capture_vars: vec![],
                    local_constants: vec![],
                    loc,
                })
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
                assert!(binding.names.len() == 1 && binding.types.len() <= 1);
                // TODO: this could just carry the pattern forward.
                Stmt::DeclNamed {
                    name: binding.names[0],
                    ty: binding.types.first().unwrap().clone(),
                    value,
                    kind,
                }
            }
            Semicolon => Stmt::Noop,
            _ => {
                let e = self.parse_expr()?;
                if self.maybe(Equals) {
                    if let Expr::GetNamed(name) = e.deref() {
                        let value = self.parse_expr()?;
                        Stmt::SetNamed(*name, value)
                    } else {
                        return Err(self.todo("left of assign not plain ident"));
                    }
                } else {
                    Stmt::Eval(e)
                }
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

        while RightParen != self.peek() {
            values.push(self.parse_expr()?);
            if Comma == self.peek() {
                // inner and optional trailing
                self.eat(Comma)?;
            } else {
                // No trailing comma is fine
                break;
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
                    args.then(self.parse_type_binding()?);
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
            args.then(self.parse_type_binding()?);
            if Comma == self.peek() {
                // inner and optional trailing
                self.eat(Comma)?;
            } else {
                // No trailing comma is fine
                break;
            }
        }

        Ok(args)
    }

    // TODO: rn just one ident but support tuple for pattern matching
    /// `Names ':' ?Expr`
    fn parse_type_binding(&mut self) -> Res<Pattern<'p>> {
        let loc = self.lexer.nth(0).span;
        let name = self.ident()?;
        let types = if Colon == self.peek() {
            self.pop();
            vec![Some(self.parse_expr()?)]
        } else {
            vec![None]
        };
        Ok(Pattern {
            names: vec![name],
            types,
            loc,
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
        self.expr_id += 1;
        FatExpr {
            expr,
            loc: self.end_subexpr(),
            id: self.expr_id,
            ty: None,
            known: Known::Maybe,
        }
    }

    #[track_caller]
    fn stmt(&mut self, annotations: Vec<Annotation<'p>>, stmt: Stmt<'p>) -> FatStmt<'p> {
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
