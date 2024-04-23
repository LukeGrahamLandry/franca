//! Convert a stream of tokens into ASTs.
use std::{fmt::Debug, mem, ops::Deref, panic::Location, sync::Arc};

use codemap::{CodeMap, File, Span};
use codemap_diagnostic::{Diagnostic, Level, SpanLabel, SpanStyle};

use crate::ast::{Binding, Flag, Name, TypeId, VarType};
use crate::bc::Values;
use crate::export_ffi::get_include_std;
use crate::{
    ast::{Annotation, Expr, FatExpr, FatStmt, Func, LazyType, Pattern, Stmt},
    bc::Value,
    lex::{Lexer, Token, TokenType},
    logging::{LogTag::Parsing, PoolLog},
    pool::{Ident, StringPool},
};
use crate::{outln, STATS};
use TokenType::*;

pub struct Parser<'a, 'p> {
    pool: &'p StringPool<'p>,
    expr_id: usize,
    lexer: Vec<Lexer<'a, 'p>>,
    spans: Vec<Span>,
    codemap: &'a mut CodeMap,
    lines: usize,
}

type Res<T> = Result<T, ParseErr>;

#[derive(Debug)]
pub struct ParseErr {
    pub loc: Option<&'static Location<'static>>,
    pub file: Arc<File>,
    pub diagnostic: Vec<Diagnostic>,
}

pub struct Parsed<'p> {
    pub stmts: Vec<FatStmt<'p>>,
    pub lines: usize,
}

impl<'a, 'p> Parser<'a, 'p> {
    pub fn parse(codemap: &'a mut CodeMap, file: Arc<File>, pool: &'p StringPool<'p>) -> Res<Parsed<'p>> {
        outln!(
            Parsing,
            "\n######################################\n### START FILE: {} \n######################################\n",
            file.name()
        );

        let mut p = Parser {
            pool,
            lexer: vec![Lexer::new(file.clone(), pool, file.span)], // TODO
            expr_id: 0,
            spans: vec![],
            codemap,
            lines: 0,
        };

        p.start_subexpr();
        let mut stmts: Vec<FatStmt<'p>> = vec![];
        while p.peek() != Eof {
            stmts.push(p.parse_stmt()?);
        }
        let _full = p.end_subexpr();
        debug_assert!(p.spans.is_empty(), "leaked parse loc context");
        let lex = p.lexer.pop().unwrap();
        p.lines += lex.line - lex.comment_lines;

        Ok(Parsed { stmts, lines: p.lines })
    }

    fn parse_expr(&mut self) -> Res<FatExpr<'p>> {
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

    fn prefix_operator(&mut self, tok: TokenType, op: Flag) -> Res<FatExpr<'p>> {
        self.eat(tok)?;
        self.start_subexpr();
        self.start_subexpr();
        let ptr = self.expr(Expr::GetNamed(op.ident()));
        let inner = self.parse_expr()?;
        Ok(self.expr(Expr::Call(Box::new(ptr), Box::new(inner))))
    }

    fn fn_def_signeture(&mut self, loc: Span) -> Res<(Option<Ident<'p>>, Pattern<'p>, LazyType<'p>)> {
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

        let ret = if Equals != self.peek() && Semicolon != self.peek() && Pipe != self.peek() {
            assert!(
                name.is_some() || !no_paren,
                "'fn <Expr> =' can't treat Expr as ret type. pls specify name or args."
            );
            LazyType::PendingEval(self.parse_expr()?)
        } else {
            LazyType::Infer
        };
        Ok((name, arg, ret))
    }

    fn parse_expr_inner(&mut self) -> Res<FatExpr<'p>> {
        match self.peek() {
            // TODO: use no body as type expr?
            // Optional name, require body, optional (args).
            Fn => {
                self.start_subexpr();
                let loc = self.eat(Fn)?;
                let (name, arg, ret) = self.fn_def_signeture(loc)?;
                self.eat(Equals)?;
                let body = self.parse_expr()?;

                let name = name.unwrap_or_else(|| {
                    let mut name = body.expr.log(self.pool);
                    name.truncate(25);
                    self.pool.intern(&name)
                });

                let func = Func::new(name, arg, ret, Some(body), loc, false, true);
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
                Ok(self.expr(Expr::Value {
                    ty: TypeId::i64(),
                    value: Value::I64(f).into(),
                }))
            }
            Float(f) => {
                self.start_subexpr();
                self.pop();
                Ok(self.expr(Expr::Value {
                    ty: TypeId::f64(),
                    value: Value::F64(f.to_bits()).into(),
                }))
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
                let v = self.expr(Expr::int(v));
                let bits = self.expr(Expr::int(bit_count as i64));
                let pair = self.expr(Expr::Tuple(vec![bits, v]));
                let call = self.expr(Expr::SuffixMacro(Flag::From_Bit_Literal.ident(), Box::new(pair)));
                Ok(call)
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
            // TODO: annotations on anon function exprs. Currently you can't make one @c_call because stmt and expr macros are parsed differently.
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
                if self.peek() != TokenType::LeftParen {
                    return Err(self.expected("'(' for macro arg. (TODO: treat that as single arg?)"));
                }

                let arg = Box::new(self.parse_tuple()?);

                let target = match self.peek() {
                    Semicolon | Comma | RightParen | RightSquiggle | Dot | RightAngle => {
                        // target is optional.
                        self.start_subexpr();
                        Box::new(self.expr(Expr::unit()))
                    }
                    _ => Box::new(self.parse_expr()?),
                };
                Ok(self.expr(Expr::PrefixMacro {
                    handler: Box::new(handler),
                    arg,
                    target,
                }))
            }
            Dot => Err(self.error_next(String::from("leading '.' is reserved for inferred type enum constants."))),
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
            _ => Err(self.expected("Expr === 'fn' or '{' or '(' or '\"' or '@' or Num or Ident...")),
        }
    }

    fn maybe_parse_suffix(&mut self, mut prefix: FatExpr<'p>) -> Res<FatExpr<'p>> {
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
                    let (name, arg, ret) = self.fn_def_signeture(loc)?;
                    self.eat(Pipe)?;

                    self.start_subexpr();
                    let callback = self.parse_block_until_squiggle()?;
                    self.eat(RightSquiggle)?;

                    let name = name.unwrap_or_else(|| {
                        let mut name = callback.expr.log(self.pool);
                        name.truncate(25);
                        name = name.replace('\n', "~");
                        self.pool.intern(&name)
                    });
                    let callback = Func::new(name, arg, ret, Some(callback), *self.spans.last().unwrap(), false, true);
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
                    let name = self.ident()?;
                    self.expr(Expr::FieldAccess(Box::new(prefix), name))
                }
                DoubleSquare => {
                    self.start_subexpr();
                    self.eat(DoubleSquare)?;
                    self.expr(Expr::SuffixMacro(Flag::Deref.ident(), Box::new(prefix)))
                }
                LeftSquare => {
                    self.start_subexpr();
                    self.eat(LeftSquare)?;
                    let index = Box::new(self.parse_expr()?);
                    self.eat(RightSquare)?;
                    self.expr(Expr::Index {
                        ptr: Box::new(prefix),
                        index,
                    })
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

    fn parse_tuple(&mut self) -> Res<FatExpr<'p>> {
        self.start_subexpr();
        self.eat(LeftParen)?;
        if self.maybe(RightParen) {
            return Ok(self.expr(Expr::unit()));
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
                        LazyType::PendingEval(self.parse_expr()?)
                    } else {
                        // It's gonna be Name = Value like for @enum(T) S
                        LazyType::Infer
                    };
                    let name = if let Expr::GetNamed(name) = args.pop().unwrap().expr {
                        name
                    } else {
                        return Err(self.expected("Ident before ':' in argument pattern"));
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
            0 => self.expr(Expr::unit()),
            1 => {
                self.end_subexpr();
                args.into_iter().next().unwrap()
            }
            _ => self.expr(Expr::Tuple(args)),
        })
    }

    fn parse_stmt(&mut self) -> Res<FatStmt<'p>> {
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

    fn fn_stmt(&mut self, may_capture: bool) -> Res<Stmt<'p>> {
        let loc = self.next_span();
        match self.pop().kind {
            Fn | Fun => {}
            _ => return Err(self.expected("fn or fun")),
        }

        let (name, arg, ret) = self.fn_def_signeture(loc)?;
        if name.is_none() {
            // TODO: msg in wrong place
            return Err(self.expected("fn expr to have name"));
        }

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

        let func = Func::new(name.unwrap(), arg, ret, body, loc, true, may_capture);
        Ok(Stmt::DeclFunc(func))
    }

    fn parse_stmt_inner(&mut self) -> Res<FatStmt<'p>> {
        self.start_subexpr();
        let mut annotations = self.parse_annotations()?;
        let stmt = match self.peek() {
            // Require name, optional body.
            Fn => self.fn_stmt(true)?,
            Fun => {
                annotations.push(Annotation {
                    name: Flag::Pub.ident(),
                    args: None,
                });
                self.fn_stmt(false)?
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
                        Stmt::DeclNamed {
                            name,
                            ty,
                            value: Some(value),
                            kind,
                        }
                    }
                    Semicolon => {
                        // I think this is better but then I can't use @import the current way.
                        // return Err(self.error_next("name binding requires initial value".to_string());
                        self.eat(Semicolon)?;
                        Stmt::DeclNamed { name, ty, value: None, kind }
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
                        let name = Flag::Backpass.ident();
                        let callback = Func::new(name, arg, LazyType::Infer, Some(callback), *self.spans.last().unwrap(), false, true);
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

            IncludeStd => {
                self.eat(IncludeStd)?;
                self.eat(LeftParen)?;
                if let TokenType::Quoted(name) = self.peek() {
                    self.pop();
                    self.eat(RightParen)?;
                    self.eat(Semicolon)?;
                    let name = self.pool.get(name);
                    if let Some(src) = get_include_std(name) {
                        self.end_subexpr();
                        let file = self.codemap.add_file(name.to_string(), src);
                        self.lexer.push(Lexer::new(file.clone(), self.pool, file.span)); // TODO
                        return self.parse_stmt();
                    } else {
                        return Err(self.expected("known path for #include_std"));
                    }
                } else {
                    return Err(self.expected("quoted path"));
                }
            }
            Symbol(_name) => {
                let lex = self.lexer.last_mut().unwrap();
                if matches!(lex.nth(1).kind, TokenType::Colon | TokenType::DoubleColon) {
                    return Err(self.error_next("reserved for name := value and name :: value".to_string()));
                }
                let e = self.parse_expr()?;
                self.after_expr_stmt(e)?
            }
            _ => {
                let e = self.parse_expr()?;
                self.after_expr_stmt(e)?
            }
        };
        Ok(self.stmt(annotations, stmt))
    }

    fn after_expr_stmt(&mut self, e: FatExpr<'p>) -> Res<Stmt<'p>> {
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
        Ok(s)
    }

    // | @name(args) |
    fn parse_annotations(&mut self) -> Res<Vec<Annotation<'p>>> {
        let mut annotations = vec![];
        while let At = self.peek() {
            self.eat(At)?;
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
    fn parse_args(&mut self) -> Res<Pattern<'p>> {
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
    fn parse_type_binding(&mut self, allow_default: bool) -> Res<Binding<'p>> {
        let kind = if let Qualifier(kind) = self.peek() {
            self.pop();
            kind
        } else {
            VarType::Var // TODO: default to let?
        };
        let name = self.ident()?;
        let types = if Colon == self.peek() {
            self.pop();
            LazyType::PendingEval(self.parse_expr()?)
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
        let s = self.next_span();
        self.spans.push(s);
    }

    fn next_span(&mut self) -> Span {
        self.lexer.last_mut().unwrap().nth(0).span
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
        let t = self.lexer.last_mut().unwrap().next();
        let prev = self.spans.last().unwrap();
        if t.kind == TokenType::Eof && self.lexer.len() > 1 {
            self.lexer.pop();
            return self.pop();
        }
        *self.spans.last_mut().unwrap() = prev.merge(t.span);
        t
    }

    fn peek(&mut self) -> TokenType<'p> {
        let kind = self.lexer.last_mut().unwrap().nth(0).kind;
        if kind == TokenType::Eof && self.lexer.len() > 1 {
            let lex = self.lexer.pop().unwrap();
            self.lines += lex.line - lex.comment_lines;
            self.peek()
        } else {
            kind
        }
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

        unsafe {
            STATS.ast_expr_nodes += 1;
        }

        FatExpr {
            expr,
            loc: self.end_subexpr(),
            ty: TypeId::unknown(),
        }
    }

    #[track_caller]
    fn stmt(&mut self, annotations: Vec<Annotation<'p>>, stmt: Stmt<'p>) -> FatStmt<'p> {
        outln!(
            Parsing,
            "{}({}) STMT {:?} {}",
            "=".repeat(self.spans.len() * 2),
            self.spans.len(),
            annotations.iter().map(|a| self.pool.get(a.name)).collect::<Vec<_>>(),
            stmt.log(self.pool)
        );
        FatStmt {
            stmt,
            annotations,
            loc: self.end_subexpr(),
        }
    }

    #[track_caller]
    fn error_next(&mut self, message: String) -> ParseErr {
        let lex = self.lexer.last_mut().unwrap();
        let token = lex.next();
        let last = if self.spans.len() < 2 {
            *self.spans.last().unwrap()
        } else {
            self.spans[self.spans.len() - 1]
        };
        ParseErr {
            loc: if cfg!(feature = "trace_errors") {
                Some(std::panic::Location::caller())
            } else {
                None
            },
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
            file: lex.src.clone(),
        }
    }

    #[track_caller]
    fn expected(&mut self, msg: &str) -> ParseErr {
        let s = format!("Expected: {msg} but found {:?}", self.peek());
        self.error_next(s)
    }

    /// does NOT consume the closing squiggle
    fn parse_block_until_squiggle(&mut self) -> Res<FatExpr<'p>> {
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
        Ok(self.expr(Expr::Block {
            resolved: false,
            body,
            result: Box::new(result),
            locals: None,
        }))
    }

    fn expr_call(&mut self, prefix: FatExpr<'p>, mut arg: FatExpr<'p>) -> FatExpr<'p> {
        // Dot call syntax sugar.
        if let Expr::FieldAccess(first, name) = prefix.expr {
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
                Expr::Value {
                    value: Values::One(Value::Unit),
                    ..
                } => self.expr(Expr::Call(Box::new(f), first)),
                // Tuple parser eagerly falttened single tuples so we have to undo that here.
                // TODO: have it work woth named arguments. Need to support mixing named and positional.
                _ => {
                    self.start_subexpr();
                    let arg = self.expr(Expr::Tuple(vec![*first, arg]));
                    self.expr(Expr::Call(Box::new(f), Box::new(arg)))
                }
            }
        } else {
            self.expr(Expr::Call(Box::new(prefix), Box::new(arg)))
        }
    }

    fn push_arg(&mut self, call: &mut FatExpr<'p>, callback: FatExpr<'p>) -> Res<()> {
        if let Expr::Call(_, old_arg) = &mut call.expr {
            match &mut old_arg.expr {
                Expr::Tuple(parts) => parts.push(callback),
                // Parser flattened empty tuples.
                Expr::Value {
                    value: Values::One(Value::Unit),
                    ..
                } => old_arg.expr = callback.expr,
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
