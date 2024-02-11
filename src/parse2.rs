use std::{fmt::Debug, panic::Location, sync::Arc};

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
                p.eat(Semicolon);
            }
        }
        let full = p.end_subexpr();

        for s in &stmts {
            logln!("finished stmt: {}", s.log(pool))
        }

        Ok(stmts)
    }

    fn parse_expr(&mut self) -> Res<FatExpr<'p>> {
        let prefix = self.parse_expr_inner()?;
        self.maybe_parse_suffix(prefix)
    }

    fn parse_expr_inner(&mut self) -> Res<FatExpr<'p>> {
        match self.peek() {
            LeftSquiggle => {
                self.eat(LeftSquiggle);
                Err(self.todo()) // ("Struct or block.")
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
            _ => Err(self.expected("Expr === '{' or '(' or Num or Ident...")),
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
            Fn => {
                self.pop();
                return Err(self.todo());
            }
            Qualifier(kind) => {
                self.pop();
                let binding = self.parse_type_binding()?;
                let value = if Equals == self.peek() {
                    self.eat(Equals);
                    Some(self.parse_expr()?)
                } else {
                    None
                };
                assert!(binding.names.len() == 1 && binding.types.len() <= 1);
                Stmt::DeclNamed {
                    name: binding.names[0],
                    ty: binding.types.first().cloned(),
                    value,
                    kind,
                }
            }
            Semicolon => Stmt::Noop,
            _ => return Err(self.expected("Ident or fn/const/var/let or Expr")),
        };
        Ok(self.stmt(annotations, Stmt::Noop))
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

    // '(' | Expr, Expr, | ')'
    fn comma_sep_expr(&mut self) -> Res<Vec<FatExpr<'p>>> {
        let mut values: Vec<FatExpr<'p>> = vec![];

        while RightParen != self.peek() {
            values.push(self.parse_expr()?);
            if Comma == self.peek() {
                // inner and optional trailing
                self.eat(Comma);
            } else {
                // No trailing comma is fine
                break;
            }
        }

        Err(self.todo())
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
    fn eat(&mut self, ty: TokenType) -> Res<()> {
        if self.peek() == ty {
            self.pop();
            Ok(())
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

    fn stmt(&mut self, annotations: Vec<Annotation<'p>>, stmt: Stmt<'p>) -> FatStmt<'p> {
        FatStmt {
            stmt,
            annotations,
            loc: self.end_subexpr(),
        }
    }

    #[track_caller]
    fn todo(&mut self) -> ParseErr {
        self.error_next("Not yet implemented.".into())
    }

    #[track_caller]
    fn error_next(&mut self, message: String) -> ParseErr {
        let token = self.lexer.next();
        ParseErr {
            loc: Location::caller(),
            diagnostic: vec![Diagnostic {
                level: Level::Error,
                message,
                code: None,
                spans: vec![
                    SpanLabel {
                        span: token.span,
                        label: None,
                        style: SpanStyle::Primary,
                    },
                    SpanLabel {
                        span: *self.spans.last().unwrap(),
                        label: None,
                        style: SpanStyle::Secondary,
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

    fn parse_type_binding(&mut self) -> Res<Pattern<'p>> {
        let name = self.ident()?;
        let types = if Colon == self.peek() {
            self.pop();
            vec![self.parse_expr()?]
        } else {
            vec![]
        };
        Ok(Pattern {
            names: vec![name],
            types,
        })
    }
}
