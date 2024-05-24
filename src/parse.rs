//! Convert a stream of tokens into ASTs.
use std::collections::HashSet;
use std::{fmt::Debug, mem, ops::Deref, sync::Arc};

use codemap::{CodeMap, File, Span};
use codemap_diagnostic::{Diagnostic, Level, SpanLabel, SpanStyle};

use crate::ast::{Binding, Flag, Name, TypeId, VarType};
use crate::bc::Values;
use crate::compiler::{CErr, CompileError, Res};
use crate::export_ffi::get_include_std;
use crate::STATS;
use crate::{
    ast::{Annotation, Expr, FatExpr, FatStmt, Func, LazyType, Pattern, Stmt},
    lex::{Lexer, Token, TokenType},
    logging::PoolLog,
    pool::{Ident, StringPool},
};
use TokenType::*;

pub struct Parser<'a, 'p> {
    pool: &'p StringPool<'p>,
    lexer: Lexer<'a, 'p>,
    spans: Vec<Span>,
    ctx: &'a mut ParseTasks<'p>,
}

#[derive(Debug, Clone)]
pub enum ParseFile<'p> {
    PendingStmts(Arc<File>, Span),
    PendingExpr(Arc<File>, Span),
    ParsedStmts(Vec<FatStmt<'p>>),
    ParsedExpr(FatExpr<'p>),
    Err(Box<CompileError<'p>>),
    DoneExpr,
    DoneStmt,
    Wip,
}

/// # Safety
/// The main thread only reads before 'tasks[next]' and the parse thread only writes to 'tasks[next]' or push to end.
/// Because of resizing the vec, the lock allows only one of A) main thread reading B) parse thread pushing.
/// 'fn handle' doesn't worry about resize because there's only one parser thread.
/// (i know its not faster than std locks, i just find it entertaining)
pub struct ParseTasks<'p> {
    pub pool: &'p StringPool<'p>,
    pub codemap: CodeMap,
    tasks: Vec<ParseFile<'p>>,
    already_loaded: HashSet<Ident<'p>>,
}

unsafe impl<'p> Sync for ParseTasks<'p> {}
unsafe impl<'p> Send for ParseTasks<'p> {}

pub static mut ANON_BODY_AS_NAME: bool = false;

impl<'p> ParseTasks<'p> {
    pub fn new(pool: &'p StringPool<'p>) -> Self {
        Self {
            pool,
            codemap: CodeMap::new(),
            tasks: Default::default(),
            already_loaded: Default::default(),
        }
    }

    pub fn wait_for_stmts(&mut self, name: usize) -> Res<'p, Vec<FatStmt<'p>>> {
        match &mut self.tasks[name] {
            ParseFile::PendingStmts(file, span) => {
                let lex = Lexer::new(file.clone(), self.pool, *span);
                let res = Parser::parse_stmts(self, lex, self.pool);
                match res {
                    Ok(stmts) => {
                        unsafe { STATS.parser_did += 1 };
                        // println!("{:?}\n=====", stmts.iter().map(|v| v.log(self.pool)).collect::<Vec<_>>());
                        self.tasks[name] = ParseFile::Wip; // stmts are single use it seems.
                        Ok(stmts)
                    }
                    Err(e) => {
                        self.tasks[name] = ParseFile::Err(e.clone());
                        Err(e)
                    }
                }
            }
            ParseFile::ParsedStmts(_) => unreachable!(),
            ParseFile::Err(e) => Err(e.clone()),
            _ => todo!(),
        }
    }

    pub fn wait_for_expr(&mut self, name: usize) -> Res<'p, FatExpr<'p>> {
        match &mut self.tasks[name] {
            ParseFile::PendingExpr(file, span) => {
                let lex = Lexer::new(file.clone(), self.pool, *span);
                let res = Parser::parse_expr_outer(self, lex, self.pool);
                match res {
                    Ok(stmts) => {
                        unsafe { STATS.parser_did += 1 };
                        // println!("{}\n=====", stmts.log(self.pool));
                        self.tasks[name] = ParseFile::ParsedExpr(stmts.clone());
                        Ok(stmts)
                    }
                    Err(e) => {
                        self.tasks[name] = ParseFile::Err(e.clone());
                        Err(e)
                    }
                }
            }
            ParseFile::ParsedExpr(v) => Ok(v.clone()),
            ParseFile::Err(e) => Err(e.clone()),
            _ => todo!(),
        }
    }

    pub fn add_task(&mut self, is_expr: bool, file: Arc<File>, span: Span) -> usize {
        unsafe { STATS.parser_queue += 1 };
        if is_expr {
            self.tasks.push(ParseFile::PendingExpr(file, span));
        } else {
            self.tasks.push(ParseFile::PendingStmts(file, span));
        }
        self.tasks.len() - 1
    }
}

impl<'a, 'p> Parser<'a, 'p> {
    pub fn parse_stmts(ctx: &'a mut ParseTasks<'p>, lexer: Lexer<'a, 'p>, pool: &'p StringPool<'p>) -> Res<'p, Vec<FatStmt<'p>>> {
        let mut p = Parser {
            pool,
            lexer,
            spans: vec![],
            ctx,
        };

        p.start_subexpr();
        let mut stmts: Vec<FatStmt<'p>> = vec![];
        while p.peek() != Eof {
            stmts.push(p.parse_stmt()?);
        }
        let _ = p.end_subexpr();
        debug_assert!(p.spans.is_empty(), "leaked parse loc context");
        unsafe {
            STATS.inflated_lexer_lines += p.lexer.raw_lines;
            STATS.skipped_lexer_lines += p.lexer.skipped_lines;
        };
        Ok(stmts)
    }

    pub fn parse_expr_outer(ctx: &'a mut ParseTasks<'p>, lexer: Lexer<'a, 'p>, pool: &'p StringPool<'p>) -> Res<'p, FatExpr<'p>> {
        let mut p = Parser {
            pool,
            lexer,
            spans: vec![],
            ctx,
        };

        p.start_subexpr();
        let expr = p.parse_expr()?;
        let _ = p.end_subexpr();
        debug_assert!(p.spans.is_empty(), "leaked parse loc context");
        unsafe {
            STATS.inflated_lexer_lines += p.lexer.raw_lines;
            STATS.skipped_lexer_lines += p.lexer.skipped_lines;
        };

        Ok(expr)
    }

    fn parse_expr(&mut self) -> Res<'p, FatExpr<'p>> {
        match self.peek() {
            Star => self.prefix_operator(Star, Flag::Operator_Star_Prefix),
            Question => self.prefix_operator(Question, Flag::Operator_Question_Prefix),
            // UpArrow => self.prefix_operator(UpArrow, Flag::Operator_Up_Arrow_Prefix),
            // Amp => self.prefix_operator(Amp, Flag::Operator_Ampersand_Prefix),
            _ => {
                let prefix = self.parse_expr_inner()?;
                self.maybe_parse_suffix(prefix)
            }
        }
    }

    fn prefix_operator(&mut self, tok: TokenType, op: Flag) -> Res<'p, FatExpr<'p>> {
        self.eat(tok)?;
        self.start_subexpr();
        self.start_subexpr();
        let ptr = self.expr(Expr::GetNamed(op.ident()));
        let inner = self.parse_expr()?;
        Ok(self.expr(Expr::Call(Box::new(ptr), Box::new(inner))))
    }

    fn fn_def_signeture(&mut self, loc: Span) -> Res<'p, (Option<Ident<'p>>, Pattern<'p>, LazyType<'p>, Vec<Annotation<'p>>)> {
        let name = if let Symbol(i) = self.peek() {
            self.pop();
            // return Err(self.error_next("Fn expr must not have name".into()));
            Some(i)
        } else {
            None
        };
        // Args are optional so you can do `if(a, fn=b, fn=c)`
        let mut no_paren = false;
        let mut arg = if self.maybe(LeftParen) {
            let a = self.parse_args()?;
            self.eat(RightParen)?;
            a
        } else {
            no_paren = true;
            Pattern::empty(loc)
        };
        arg.if_empty_add_unit();

        let ret = if Equals != self.peek() && Semicolon != self.peek() && Pipe != self.peek() && FatRightArrow != self.peek() && Hash != self.peek() {
            if name.is_none() && no_paren {
                return Err(self.error_next(String::from("'fn <Expr> =' can't treat Expr as ret type. pls specify name or args.")));
            }
            LazyType::PendingEval(self.parse_expr()?)
        } else {
            LazyType::Infer
        };
        let ann = self.parse_annotations()?;

        Ok((name, arg, ret, ann))
    }

    fn anon_fn_name(&self, expr: &FatExpr<'p>) -> Ident<'p> {
        if unsafe { ANON_BODY_AS_NAME } {
            let mut name = expr.log(self.pool);
            name.truncate(25);
            self.pool.intern(&name)
        } else {
            Flag::Anon.ident()
        }
    }

    fn parse_expr_inner(&mut self) -> Res<'p, FatExpr<'p>> {
        match self.peek() {
            // TODO: use no body as type expr?
            // Optional name, require body, optional (args).
            Fn => {
                self.start_subexpr();
                let loc = self.eat(Fn)?;
                let (name, arg, ret, ann) = self.fn_def_signeture(loc)?;

                let capturing = if self.maybe(Equals) {
                    false
                } else if self.maybe(FatRightArrow) {
                    true
                } else {
                    return Err(self.expected("'=' or '=>' to indicate function body"));
                };

                let body = self.parse_expr()?;

                // TODO: if you dont do this, you could have basically no contention on the pool if lex/parse/compile were all on seperate threads. -- Apr 26
                let name = name.unwrap_or_else(|| self.anon_fn_name(&body));

                let mut func = Func::new(name, arg, ret, Some(body), loc, capturing);
                func.annotations = ann;
                Ok(self.expr(Expr::Closure(Box::new(func))))
            }
            FatRightArrow => {
                self.start_subexpr();
                let loc = self.eat(FatRightArrow)?;
                let body = self.parse_expr()?;

                let name = self.anon_fn_name(&body);

                let func = Func::new(name, Pattern::empty(loc), LazyType::Infer, Some(body), loc, true);
                Ok(self.expr(Expr::Closure(Box::new(func))))
            }
            Fun => Err(self.error_next("use 'fn' for lambda expression. 'fun' means public which doesn't make sense for an expression".to_string())),
            LeftSquiggle => {
                self.start_subexpr();
                self.eat(LeftSquiggle)?;
                let expr = self.parse_block_until_squiggle()?;
                self.eat(RightSquiggle)?;
                Ok(expr)
            }
            DotLeftSquiggle => Err(self.error_next(String::from("Use '(Name: Value)' instead of '.{ Name: Value }'"))),
            LeftParen => self.parse_tuple(),
            Number(f) => {
                self.start_subexpr();
                self.pop();
                let mut e = self.expr(Expr::Value { value: Values::One(f) });
                e.ty = TypeId::i64();
                Ok(e)
            }
            Float(f) => {
                self.start_subexpr();
                self.pop();
                let mut e = self.expr(Expr::Value {
                    value: (f.to_bits() as i64).into(),
                });
                e.ty = TypeId::f64();
                Ok(e)
            }
            BinaryNum { bit_count, value } => {
                if bit_count > 64 {
                    return Err(self.error_next(String::from("TODO: support >64 bit literals.")));
                }
                // TODO: this is ugly
                self.start_subexpr();
                self.start_subexpr();
                self.start_subexpr();
                self.start_subexpr();
                self.pop();
                let v = i64::from_le_bytes((value).to_le_bytes());
                let mut v = self.expr(Expr::Value { value: (v).into() });
                v.ty = TypeId::i64();
                let mut bits = self.expr(Expr::Value {
                    value: (bit_count as i64).into(),
                });
                bits.ty = TypeId::i64();
                let pair = self.expr(Expr::Tuple(vec![bits, v]));
                let call = self.expr(Expr::SuffixMacro(Flag::From_Bit_Literal.ident(), Box::new(pair)));
                Ok(call)
            }
            Symbol(i) => {
                self.start_subexpr();
                self.pop();
                Ok(self.expr(Expr::GetNamed(i)))
            }
            // TODO: maybe its wrong to both putting everything in the pool
            Quoted { s, escapes } => {
                self.start_subexpr();
                self.pop();
                let mut e = self.expr(Expr::String(s));
                if escapes {
                    self.start_subexpr();
                    self.start_subexpr();
                    let f = Box::new(self.expr(Expr::GetNamed(Flag::__String_Escapes.ident())));
                    e = self.expr(Expr::Call(f, Box::new(e)));
                }
                Ok(e)
            }
            // TODO: allow this as a raw statement, currently it will get parsed as an annotation on a noop.
            // TODO: should i allow the payload to be statement too?
            // TODO: could make arg optional but then there's wierd stuff like `@no_arg (expected, target)` being seen as one thing.
            //       i dont want to make parsing depend on how the macro was declared so you could opt out of the arg and allow that.
            // TODO: annotations on anon function exprs. Currently you can't make one #c_call because stmt and expr macros are parsed differently.
            At => {
                self.start_subexpr();
                self.eat(At)?;
                // Not normal parse_expr here. want '@f(a)t' to parse as '@(f)(a) t' not '@(f(a))t'
                let handler = match self.peek() {
                    TokenType::Symbol(name) => {
                        self.start_subexpr();
                        self.pop();
                        self.expr(Expr::GetNamed(name))
                    }
                    TokenType::LeftParen => {
                        self.eat(LeftParen)?;
                        let res = self.parse_expr()?;
                        self.eat(RightParen)?;
                        res
                    }
                    _ => return Err(self.expected("ident or '(' for macro expr (arbitrary expr must be wrapped in parens)")),
                };

                let arg = if self.peek() == TokenType::LeftParen {
                    Box::new(self.parse_tuple()?)
                } else {
                    // TODO: its a bit fragile with the threads stuff now?
                    //        hangs if you forget this so unwrap and that thread crashes.
                    //        cause its not waiting on a mutex, just the counter never goes up. -- Apr 28
                    self.start_subexpr();
                    Box::new(self.raw_unit())
                };

                let target = match self.peek() {
                    Semicolon | Comma | RightParen | RightSquiggle | Dot | RightAngle => {
                        // target is optional.
                        self.start_subexpr();
                        Box::new(self.raw_unit())
                    }
                    _ => Box::new(self.parse_expr()?),
                };
                Ok(self.expr(Expr::PrefixMacro {
                    handler: Box::new(handler),
                    arg,
                    target,
                }))
            }
            Dot => {
                self.start_subexpr();
                self.start_subexpr();
                self.eat(Dot)?;
                let name = self.ident()?;
                let e = self.expr(Expr::GetNamed(name));
                Ok(self.expr(Expr::SuffixMacro(Flag::Contextual_Field.ident(), Box::new(e))))
            }
            LeftAngle => {
                self.start_subexpr();
                self.eat(LeftAngle)?;
                let e = self.parse_expr()?;
                self.eat(RightAngle)?;
                Ok(self.expr(Expr::SuffixMacro(Flag::Unquote.ident(), Box::new(e))))
            }
            Quote => {
                self.start_subexpr();
                self.eat(Quote)?;
                let e = self.parse_expr()?;
                self.eat(Quote)?;
                Ok(self.expr(Expr::SuffixMacro(Flag::Quote.ident(), Box::new(e))))
            }
            DoubleColon => {
                self.start_subexpr();
                self.eat(DoubleColon)?;
                let e = self.parse_expr()?;
                Ok(self.expr(Expr::SuffixMacro(Flag::Const_Eval.ident(), Box::new(e))))
            }
            Bang => {
                // !name === ()!name
                self.start_subexpr();
                self.start_subexpr();
                self.eat(Bang)?;
                let name = self.ident()?;
                let prefix = Box::new(self.raw_unit());
                Ok(self.expr(Expr::SuffixMacro(name, prefix)))
            }
            _ => Err(self.expected("Expr === 'fn' or '{' or '(' or '\"' or '@' or Num or Ident...")),
        }
    }

    fn maybe_parse_suffix(&mut self, mut prefix: FatExpr<'p>) -> Res<'p, FatExpr<'p>> {
        loop {
            prefix = match self.peek() {
                LeftParen => {
                    self.start_subexpr();
                    let arg = self.parse_tuple()?;
                    self.expr_call(prefix, arg)
                }
                Dollar => {
                    self.start_subexpr();
                    self.eat(Dollar)?;
                    let arg = self.parse_expr()?;
                    self.expr_call(prefix, arg)
                }
                LeftSquiggle => {
                    self.start_subexpr();
                    self.pop(); // {
                    let loc = self.next_span();
                    let (name, arg, ret, ann) = self.fn_def_signeture(loc)?;
                    self.eat(Pipe)?;

                    self.start_subexpr();
                    let callback = self.parse_block_until_squiggle()?;
                    self.eat(RightSquiggle)?;

                    let name = name.unwrap_or_else(|| self.anon_fn_name(&callback));
                    let mut callback = Func::new(name, arg, ret, Some(callback), *self.spans.last().unwrap(), true);
                    callback.annotations = ann;
                    let callback = self.expr(Expr::Closure(Box::new(callback)));

                    self.push_arg(&mut prefix, callback)?;
                    prefix
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
                    match self.peek() {
                        Symbol(name) => {
                            self.pop();
                            self.expr(Expr::FieldAccess(Box::new(prefix), name))
                        }
                        LeftParen => {
                            self.eat(LeftParen)?;
                            let index = Box::new(self.parse_expr()?);
                            self.eat(RightParen)?;
                            self.expr(Expr::TupleAccess {
                                ptr: Box::new(prefix),
                                index,
                            })
                        }

                        _ => return Err(self.expected(".name or .(index)")),
                    }
                }
                DoubleSquare => {
                    self.start_subexpr();
                    self.eat(DoubleSquare)?;
                    self.expr(Expr::SuffixMacro(Flag::Deref.ident(), Box::new(prefix)))
                }
                LeftSquare => {
                    self.start_subexpr();
                    self.eat(LeftSquare)?;
                    let index = self.parse_expr()?;
                    self.eat(RightSquare)?;
                    self.bin_named_macro(Flag::Operator_Index, prefix, index)
                }
                Amp => {
                    self.start_subexpr();
                    self.eat(Amp)?;
                    self.expr(Expr::SuffixMacro(Flag::Addr.ident(), Box::new(prefix)))
                }
                _ => return Ok(prefix),
            };
        }
    }

    fn parse_tuple(&mut self) -> Res<'p, FatExpr<'p>> {
        self.start_subexpr();
        self.eat(LeftParen)?;
        if self.maybe(RightParen) {
            return Ok(self.raw_unit());
        }
        let mut args: Vec<FatExpr<'p>> = vec![];
        loop {
            args.push(self.parse_expr()?);
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
                Colon | Equals => {
                    // The last thing was actually a name for a named argument
                    let value = if self.maybe(Colon) {
                        if self.peek() == Equals {
                            // default arg with infered type
                            LazyType::Infer
                        } else {
                            // we're in a function header and that was a type
                            LazyType::PendingEval(self.parse_expr()?)
                        }
                    } else {
                        // It's gonna be Name = Value like for @enum(T) S or named arguments
                        LazyType::Infer
                    };
                    let Expr::GetNamed(name) = args.pop().unwrap().expr else {
                        return Err(self.expected("Ident before ':'/'=' in argument pattern"));
                    };
                    let default = if self.maybe(Equals) { Some(self.parse_expr()?) } else { None };
                    let first = Binding {
                        name: Name::Ident(name),
                        ty: value,
                        default,
                        kind: VarType::Var,
                    };
                    self.maybe(Comma);
                    let mut named = if RightParen == self.peek() {
                        // Maybe there was only one named argument.
                        Pattern::empty(*self.spans.last().unwrap())
                    } else {
                        // Otherwise, switch to parsing a pattern.
                        let mut a = self.parse_args()?;
                        a.if_empty_add_unit(); // TODO: kinda HACK that i can't call it in the other branch but everywhere else I do.
                        a
                    };
                    // Add in the that first argument
                    named.bindings.insert(0, first);
                    if args.is_empty() {
                        // All arguments were named, don't have to do anything extra.
                        self.eat(RightParen)?;
                        return Ok(self.expr(Expr::StructLiteralP(named)));
                    } else {
                        return Err(self.expected("TODO: some named some positional"));
                    }
                }
                _ => {
                    return Err(self.expected("',' or ')' or ':' after tuple element"));
                }
            }
        }
        self.eat(RightParen)?;

        Ok(match args.len() {
            0 => self.raw_unit(),
            1 => {
                self.end_subexpr();
                args.into_iter().next().unwrap()
            }
            _ => self.expr(Expr::Tuple(args)),
        })
    }

    fn parse_stmt(&mut self) -> Res<'p, FatStmt<'p>> {
        let stmt = self.parse_stmt_inner()?;
        // TODO: if you're sad that this check is slow... fix the ambiguity problem...
        if !stmt.annotations.is_empty() && !matches!(stmt.stmt, Stmt::DeclFunc(_)) {
            for s in &stmt.annotations {
                match Flag::try_from(s.name) {
                    Ok(Flag::Pub) => {}
                    _ => {
                        // TODO: error in wrong place.
                        return Err(self.error_next(
                            "TODO: unknown stmt annotation would be ignored. for now you can wrap the expression in brackets to invoke the macro as an expression".to_string(),
                        ));
                    }
                }
            }
        }
        Ok(stmt)
    }

    fn fn_stmt(&mut self) -> Res<'p, Stmt<'p>> {
        let loc = self.next_span();
        match self.pop().kind {
            Fn | Fun => {}
            _ => return Err(self.expected("fn or fun")),
        }

        let (name, arg, ret, ann) = self.fn_def_signeture(loc)?;
        if name.is_none() {
            // TODO: msg in wrong place
            return Err(self.expected("fn expr to have name"));
        }

        let body = match self.peek() {
            Semicolon => {
                // TODO: return Err(self.expected("function body")), and have @import for the compiler ffi things
                self.eat(Semicolon)?;
                None
            }
            Equals => {
                self.eat(Equals)?;

                if self.peek() == LeftSquiggle {
                    self.start_subexpr();
                    let span = self.lexer.skip_to_closing_squigle();
                    let i = self.ctx.add_task(true, self.lexer.src.clone(), span);
                    let e = Expr::GetParsed(i);
                    Some(self.expr(e))
                } else {
                    Some(self.parse_expr()?)
                }
            }
            _ => return Err(self.expected("'='Expr for fn body OR ';' for ffi decl.")),
        };

        let mut func = Func::new(name.unwrap(), arg, ret, body, loc, false);
        func.annotations = ann;
        Ok(Stmt::DeclFunc(Box::new(func)))
    }

    fn parse_stmt_inner(&mut self) -> Res<'p, FatStmt<'p>> {
        self.start_subexpr();
        let mut annotations = self.parse_annotations()?;
        let stmt = match self.peek() {
            // Require name, optional body.
            Fn => self.fn_stmt()?,
            Fun => {
                annotations.push(Annotation {
                    name: Flag::Pub.ident(),
                    args: None,
                });
                self.fn_stmt()?
            }
            Qualifier(kind) => {
                self.pop();
                let binding = self.parse_type_binding(false)?;
                // TODO: allow multiple bindings
                let (name, ty) = match binding.name {
                    Name::Ident(i) => (i, binding.ty),
                    Name::Var(_) => unreachable!(),
                    Name::None => panic!("var decl needs name. {:?}", binding.ty),
                };

                let s = match self.peek() {
                    Equals => {
                        self.eat(Equals)?;
                        let value = self.parse_expr()?;
                        // interestinly, its fine without requiring this semicolon. it was like that for a while and there was only one place it was missing.
                        self.eat(Semicolon)?;
                        Stmt::DeclNamed { name, ty, value, kind }
                    }
                    Semicolon => {
                        // I think this is better but then I can't use @import the current way.
                        return Err(
                            self.error_next("binding requires a value (use unsafe '()!uninitilized' if thats what you really want)".to_string())
                        );
                    }
                    LeftArrow => {
                        self.eat(LeftArrow)?;
                        let mut call = self.parse_expr()?;
                        self.start_subexpr();
                        let mut arg = Pattern::empty(*self.spans.last().unwrap());
                        arg.bindings.push(Binding {
                            name: Name::Ident(name),
                            ty,
                            default: None,
                            kind,
                        });
                        self.eat(Semicolon)?;

                        self.start_subexpr();
                        let callback = self.parse_block_until_squiggle()?;
                        // Note: we leave the closing squiggle because the outer code is expecting to be inside a block.
                        let name = self.anon_fn_name(&callback);
                        let callback = Func::new(name, arg, LazyType::Infer, Some(callback), *self.spans.last().unwrap(), true);
                        let callback = self.expr(Expr::Closure(Box::new(callback)));

                        self.push_arg(&mut call, callback)?;
                        Stmt::Eval(call)
                    }
                    _ => return Err(self.expected("';' or '<-' or '=' after declaration.")),
                };

                s
            }
            Semicolon => {
                self.eat(Semicolon)?;
                Stmt::Noop
            }

            Hash => {
                self.eat(Hash)?;
                let name = self.ident()?;

                if name == Flag::Include_Std.ident() {
                    self.eat(LeftParen)?;
                    let TokenType::Quoted { s: name, escapes } = self.peek() else {
                        return Err(self.expected("quoted path"));
                    };
                    assert!(!escapes, "TODO: string escapes in include");
                    self.pop();
                    self.eat(RightParen)?;
                    self.eat(Semicolon)?;
                    if self.ctx.already_loaded.insert(name) {
                        let name = self.pool.get(name);
                        let Some(src) = get_include_std(name) else {
                            return Err(self.expected("known path for #include_std"));
                        };
                        let file = self.ctx.codemap.add_file(name.to_string(), src);
                        let s = file.span;
                        Stmt::ExpandParsedStmts(self.ctx.add_task(false, file, s))
                    } else {
                        // don't load the same file twice.
                        Stmt::Noop
                    }
                } else {
                    return Err(self.error_next("reserved for stmt directives".to_string()));
                }
            }
            Symbol(name) => match self.lexer.nth(1).kind {
                // TODO: it would be nicer here if DoubleColon was two tokens but i use it for const eval operator as well.
                DoubleColon => {
                    self.pop();
                    self.pop();
                    let value = self.parse_expr()?;
                    self.eat(Semicolon)?;
                    Stmt::DeclNamed {
                        name,
                        ty: LazyType::Infer,
                        value,
                        kind: VarType::Const,
                    }
                }
                Colon => {
                    self.pop();
                    self.pop();
                    let ty = match self.peek() {
                        Colon | Equals => LazyType::Infer,
                        _ => LazyType::PendingEval(self.parse_expr()?),
                    };

                    let kind = match self.peek() {
                        Colon => VarType::Const,
                        Equals => VarType::Var,
                        _ => return Err(self.expected("':' for const or '=' for var after variable declaration")),
                    };
                    self.pop();

                    if self.peek() == Semicolon {
                        return Err(
                            self.error_next("binding requires a value (use unsafe '()!uninitilized' if thats what you really want)".to_string())
                        );
                    }
                    let value = self.parse_expr()?;
                    self.eat(Semicolon)?;
                    Stmt::DeclNamed { name, ty, value, kind }
                }
                _ => {
                    let e = self.parse_expr()?;
                    self.after_expr_stmt(e)?
                }
            },
            _ => {
                let e = self.parse_expr()?;
                self.after_expr_stmt(e)?
            }
        };
        Ok(self.stmt(annotations, stmt))
    }

    fn after_expr_stmt(&mut self, e: FatExpr<'p>) -> Res<'p, Stmt<'p>> {
        let s = match self.peek() {
            Equals => {
                self.pop();
                let value = self.parse_expr()?;
                Stmt::Set { place: e, value }
            }
            EqOp(op) => {
                self.pop();
                self.start_subexpr();
                let target = self.parse_expr()?;
                let e = self.bin_named_macro(op, e, target);
                Stmt::Eval(e)
            }
            _ => {
                // Note: don't eat the semicolon so it shows up as noop for last stmt in block loop.
                Stmt::Eval(e)
            }
        };
        if !matches!(self.peek(), Semicolon | RightSquiggle) {
            return Err(self.expected("';' (discard) or '}' (return) after expr stmt"));
        }
        Ok(s)
    }

    pub fn bin_named_macro(&mut self, handler: Flag, arg: FatExpr<'p>, target: FatExpr<'p>) -> FatExpr<'p> {
        self.start_subexpr();
        let handler = Box::new(self.expr(Expr::GetNamed(handler.ident())));
        self.expr(Expr::PrefixMacro {
            handler,
            arg: Box::new(arg),
            target: Box::new(target),
        })
    }

    // | @name(args) |
    fn parse_annotations(&mut self) -> Res<'p, Vec<Annotation<'p>>> {
        let mut annotations = vec![];
        while let Hash = self.peek() {
            if let Symbol(name) = self.lexer.nth(1).kind {
                // HACK
                if name == Flag::Include_Std.ident() {
                    break;
                }
            }
            self.eat(Hash)?;
            let name = self.ident()?;
            let args = if LeftParen == self.peek() { Some(self.parse_tuple()?) } else { None };
            annotations.push(Annotation { name, args });
        }
        Ok(annotations)
    }

    // TODO: its a bit weird that im using a pattern for both of those.
    //       its good for struct decl to be pattern that the instantiation has to match.
    //       and i like struct decl being just an instantiation of a map of types so i think this is good.
    //       do i like letting you do pattern matchy things in struct decls? That's kinda a cool side effect.
    /// Used for fn sig args and also struct literals.
    /// `Names: Expr, Names: Expr` ends with ')' or '}' or missing comma
    fn parse_args(&mut self) -> Res<'p, Pattern<'p>> {
        let mut args = Pattern::empty(self.next_span());

        loop {
            match self.peek() {
                RightParen | RightSquiggle => break,
                Comma => return Err(self.expected("Expr")),
                _ => {
                    args.bindings.push(self.parse_type_binding(true)?);
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

        Ok(args)
    }

    // TODO: rn just one ident but support tuple for pattern matching
    /// `Names ':' ?Expr`
    fn parse_type_binding(&mut self, allow_default: bool) -> Res<'p, Binding<'p>> {
        let kind = if let Qualifier(kind) = self.peek() {
            self.pop();
            kind
        } else {
            VarType::Let
        };
        let name = self.ident()?;
        let types = if self.maybe(Colon) {
            if self.peek() == Equals && allow_default {
                LazyType::Infer
            } else {
                LazyType::PendingEval(self.parse_expr()?)
            }
        } else {
            LazyType::Infer
        };
        let default = if allow_default && Equals == self.peek() {
            self.pop();
            Some(self.parse_expr()?)
        } else {
            None
        };

        Ok(Binding {
            name: Name::Ident(name),
            ty: types,
            default,
            kind,
        })
    }

    #[track_caller]
    fn ident(&mut self) -> Res<'p, Ident<'p>> {
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
    fn eat(&mut self, ty: TokenType) -> Res<'p, Span> {
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
        let s = self.next_span();
        self.spans.push(s);
    }

    fn next_span(&mut self) -> Span {
        self.lexer.nth(0).span
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
        unsafe {
            STATS.ast_expr_nodes_all += 1;
            STATS.ast_expr_nodes_parser_only += 1;
        }

        FatExpr {
            expr,
            loc: self.end_subexpr(),
            ty: TypeId::unknown,
            done: false,
        }
    }

    fn raw_unit(&mut self) -> FatExpr<'p> {
        let mut e = self.expr(Expr::Value { value: Values::Unit });
        e.ty = TypeId::unit;
        e
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
    fn error_next(&mut self, message: String) -> Box<CompileError<'p>> {
        let token = self.lexer.next();
        let last = if self.spans.len() < 2 {
            *self.spans.last().unwrap()
        } else {
            self.spans[self.spans.len() - 1]
        };

        let internal_loc = Some(std::panic::Location::caller());
        let diagnostic = vec![Diagnostic {
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
        }];
        Box::new(CompileError {
            internal_loc,
            loc: Some(last),
            reason: CErr::Diagnostic(diagnostic),
            trace: String::new(),
        })
    }

    #[track_caller]
    fn expected(&mut self, msg: &str) -> Box<CompileError<'p>> {
        let s = format!("Expected: {msg} but found {:?}", self.peek());
        self.error_next(s)
    }

    /// does NOT consume the closing squiggle
    fn parse_block_until_squiggle(&mut self) -> Res<'p, FatExpr<'p>> {
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
                self.raw_unit()
            }
        } else {
            self.start_subexpr();
            self.raw_unit()
        };
        Ok(self.expr(Expr::Block {
            body,
            result: Box::new(result),
            ret_label: None,
            hoisted_constants: false,
        }))
    }

    fn expr_call(&mut self, prefix: FatExpr<'p>, mut arg: FatExpr<'p>) -> FatExpr<'p> {
        let Expr::FieldAccess(first, name) = prefix.expr else {
            return self.expr(Expr::Call(Box::new(prefix), Box::new(arg)));
        };
        // Dot call syntax sugar.
        // TODO: I want this to just call push_arg
        self.start_subexpr();
        let f = self.expr(Expr::GetNamed(name));
        match arg.expr {
            Expr::Tuple(mut parts) => {
                parts.insert(0, *first);
                arg.expr = Expr::Tuple(parts);
                self.expr(Expr::Call(Box::new(f), Box::new(arg)))
            }
            // Parser flattened empty tuples.
            Expr::Value { value: Values::Unit, .. } => self.expr(Expr::Call(Box::new(f), first)),
            // Tuple parser eagerly falttened single tuples so we have to undo that here.
            // TODO: have it work woth named arguments. Need to support mixing named and positional.
            _ => {
                self.start_subexpr();
                let arg = self.expr(Expr::Tuple(vec![*first, arg]));
                self.expr(Expr::Call(Box::new(f), Box::new(arg)))
            }
        }
    }

    fn push_arg(&mut self, call: &mut FatExpr<'p>, callback: FatExpr<'p>) -> Res<'p, ()> {
        if let Expr::Call(_, old_arg) = &mut call.expr {
            match &mut old_arg.expr {
                Expr::Tuple(parts) => parts.push(callback),
                // Parser flattened empty tuples.
                Expr::Value { value: Values::Unit, .. } => old_arg.expr = callback.expr,
                _ => {
                    old_arg.expr = Expr::Tuple(vec![mem::take(old_arg), callback]);
                }
            }
        } else if let Expr::FieldAccess(first, name) = &mut call.expr {
            // This is simpler than the version in expr_call because we already know the arg we're pushing is just a closure (not a tuple)
            debug_assert!(matches!(callback.expr, Expr::Closure(_)));
            self.start_subexpr();
            self.start_subexpr();
            self.start_subexpr();
            let f = self.expr(Expr::GetNamed(*name));
            let arg = self.expr(Expr::Tuple(vec![mem::take(first), callback]));
            *call = self.expr(Expr::Call(Box::new(f), Box::new(arg)));
        } else {
            self.start_subexpr();
            // It might just be a function access. like 'while {| _ } {| _ }' should be valid.
            let f = mem::take(call);
            *call = self.expr(Expr::Call(Box::new(f), Box::new(callback)))
        }
        Ok(())
    }
}
