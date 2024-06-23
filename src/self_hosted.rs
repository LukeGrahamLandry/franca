use std::{
    marker::PhantomData,
    mem::{transmute, ManuallyDrop},
};

use codemap::CodeMap;

use crate::{
    ast::{FatExpr, FatStmt, Flag, TypeId},
    compiler::{CErr, Res},
    err,
    ffi::InterpSend,
};

#[cfg(not(feature = "self_hosted"))]
use crate::{
    lex::Lexer,
    parse::{ParseFile, ParseTasks, Parser},
    pool::StringPool,
};

use crate::export_ffi::BigResult::*;

#[cfg(feature = "self_hosted")]
pub struct SelfHosted<'p> {
    pool: *mut (),
    codemap: *mut (),
    parser: *mut (),
    a: PhantomData<&'p u8>,
}

#[cfg(feature = "self_hosted")]
#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct Span {
    low: u32,
    high: u32,
}

#[cfg(not(feature = "self_hosted"))]
pub use codemap::Span;

#[cfg(feature = "self_hosted")]
#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct ParseErr<'p> {
    pub span: Span,
    pub msg: &'p str,
}

use crate::export_ffi::BigResult;

#[cfg(feature = "self_hosted")]
#[allow(improper_ctypes)]
#[link(name = "franca")]
extern "C" {
    fn init_self_hosted<'p>() -> SelfHosted<'p>;
    fn finish_pending<'p>(s: *mut (), id: usize) -> BigResult<FatExpr<'p>, ParseErr<'p>>;
    fn finish_pending_stmts<'p>(s: *mut (), id: usize) -> BigResult<Vec<FatStmt<'p>>, ParseErr<'p>>;
    fn insert_owned<'p>(s: *mut (), s: &[u8]) -> Ident<'p>;
    fn get<'p>(s: *mut (), s: Ident<'p>) -> &'p [u8];
    fn get_c_str(s: *mut (), s: Ident) -> *const u8;
    fn add_file(s: *mut (), name: &str, content: &str) -> (i64, i64); // Span
    fn source_slice(s: *mut (), span_low: u32, span_high: u32) -> *const str;
    fn push_parse(s: *mut (), src: &str, span_low: u32, span_high: u32) -> usize;
}

#[cfg(feature = "self_hosted")]
impl<'p> SelfHosted<'p> {
    pub fn intern(&self, s: &str) -> Ident<'p> {
        let s = s.as_bytes().to_vec();
        // TODO: my version of the pool doesn't want to own things. :LEAK
        unsafe { insert_owned(self.pool, s.leak()) }
    }

    pub fn get(&self, s: Ident<'p>) -> &'p str {
        unsafe { std::str::from_utf8_unchecked(get(self.pool, s)) }
    }

    pub fn get_c_str(&self, s: Ident<'p>) -> *const u8 {
        unsafe { get_c_str(self.pool, s) }
    }

    pub fn wait_for_expr(&self, id: usize) -> Res<'p, FatExpr<'p>> {
        // TODO: HACK because the franca side doesn't know how to clone.
        //       wrong allocator! this only works because i just leak all the memory! -- Jun 23
        let e = ManuallyDrop::new(unsafe { finish_pending(self.parser, id) });
        let e = ManuallyDrop::into_inner(e.clone());
        match e {
            Ok(t) => Ok(t),
            Err(t) => err!("{:?}", t),
        }
    }

    pub fn wait_for_stmts(&self, id: usize) -> Res<'p, Vec<FatStmt<'p>>> {
        // TODO: HACK because the franca side doesn't know how to clone.
        //       wrong allocator! this only works because i just leak all the memory! -- Jun 23
        let e = ManuallyDrop::new(unsafe { finish_pending_stmts(self.parser, id) });
        let e = ManuallyDrop::into_inner(e.clone());
        match e {
            Ok(t) => Ok(t),
            Err(t) => err!("{:?}", t),
        }
    }
    pub fn add_file(&self, name: String, content: String) -> Span {
        // TODO: my version of the codemap doesn't want to own things. :LEAK
        unsafe {
            let s = add_file(self.codemap, name.leak(), content.leak());
            Span {
                low: s.0 as u32,
                high: s.1 as u32,
            }
        }
    }
    pub fn source_slice(&self, span: Span) -> &'p str {
        unsafe { &*source_slice(self.codemap, span.low, span.high) }
    }

    pub fn add_task(&mut self, is_expr: bool, span: Span) -> usize {
        unsafe {
            println!("add task {span:?}");
            let src = source_slice(self.codemap, span.low, span.high);
            push_parse(self.parser, unsafe { &*src }, span.low, span.high)
        }
    }

    pub fn lookup_filename(&self, span: Span) -> &'p str {
        todo!()
    }

    pub(crate) fn print_diagnostic(&self, e: crate::compiler::CompileError<'p>) {
        // TODO
        println!("{e:?}");
    }
}

#[cfg(feature = "self_hosted")]
impl<'p> Default for SelfHosted<'p> {
    fn default() -> Self {
        unsafe { init_self_hosted() }
    }
}

#[cfg(not(feature = "self_hosted"))]
pub struct SelfHosted<'p> {
    pool: &'p StringPool<'p>,
    codemap: CodeMap,
    pub parser: ParseTasks<'p>,
}

#[cfg(not(feature = "self_hosted"))]
impl<'p> SelfHosted<'p> {
    pub fn intern(&self, s: &str) -> Ident<'p> {
        self.pool.intern(s)
    }

    pub fn get(&self, s: Ident<'p>) -> &'p str {
        self.pool.get(s)
    }

    pub fn get_c_str(&self, s: Ident<'p>) -> *const u8 {
        self.pool.get_c_str(s)
    }

    pub fn wait_for_expr(&mut self, id: usize) -> Res<'p, FatExpr<'p>> {
        match &mut self.parser.tasks[id] {
            &mut ParseFile::PendingExpr(span) => {
                let code = self.source_slice(span);
                let lex = Lexer::new(code, self.pool, span);
                let res = Parser::parse_expr_outer(self, lex);
                match res {
                    Ok(stmts) => {
                        // println!("{:?}\n=====", stmts.iter().map(|v| v.log(self.pool)).collect::<Vec<_>>());
                        self.parser.tasks[id] = ParseFile::ParsedExpr(stmts.clone());
                        Ok(stmts)
                    }
                    Err(e) => {
                        self.parser.tasks[id] = ParseFile::Err(e.clone());
                        Err(e)
                    }
                }
            }
            ParseFile::ParsedStmts(_) => unreachable!(),
            ParseFile::Err(e) => Err(e.clone()),
            ParseFile::ParsedExpr(v) => Ok(v.clone()),
            e => todo!("{e:?}'"),
        }
    }

    pub fn wait_for_stmts(&mut self, id: usize) -> Res<'p, Vec<FatStmt<'p>>> {
        match &mut self.parser.tasks[id] {
            &mut ParseFile::PendingStmts(span) => {
                let code = self.source_slice(span);
                let lex = Lexer::new(code, self.pool, span);
                let res = Parser::parse_stmts(self, lex);
                match res {
                    Ok(stmts) => {
                        // println!("{:?}\n=====", stmts.iter().map(|v| v.log(self.pool)).collect::<Vec<_>>());
                        self.parser.tasks[id] = ParseFile::Wip; // stmts are single use it seems.
                        Ok(stmts)
                    }
                    Err(e) => {
                        self.parser.tasks[id] = ParseFile::Err(e.clone());
                        Err(e)
                    }
                }
            }
            ParseFile::PendingExpr(_) => unreachable!(),
            ParseFile::Err(e) => Err(e.clone()),
            _ => todo!(),
        }
    }

    pub fn add_file(&mut self, name: String, source: String) -> Span {
        self.codemap.add_file(name, source).span
    }

    pub fn source_slice(&self, span: Span) -> &'p str {
        unsafe { transmute(self.codemap.find_file(span.high()).source_slice(span)) }
    }

    pub fn lookup_filename(&mut self, span: Span) -> &'p str {
        unsafe { transmute(self.codemap.find_file(span.high()).name()) }
    }

    pub(crate) fn print_diagnostic(&self, e: crate::compiler::CompileError<'p>) {
        // TODO
        println!("{e:?}");
        // if let CErr::Diagnostic(diagnostic) = &e.reason {
        //     emit_diagnostic(&self.codemap, diagnostic);
        // } else if let Some(loc) = e.loc {
        //     let diagnostic = vec![Diagnostic {
        //         level: Level::Error,
        //         message: e.reason.log(self.program, self.pool),
        //         code: None,
        //         spans: vec![SpanLabel {
        //             span: loc,
        //             label: None,
        //             style: SpanStyle::Primary,
        //         }],
        //     }];
        //     emit_diagnostic(&self.codemap, &diagnostic);
        // } else {
        //     println!("{}", e.reason.log(self.program, interp.pool));
        // }
    }

    pub fn add_task(&mut self, is_expr: bool, span: Span) -> usize {
        self.parser.add_task(is_expr, span)
    }
}

#[cfg(not(feature = "self_hosted"))]
impl<'p> Default for SelfHosted<'p> {
    fn default() -> Self {
        let pool = Box::leak(Box::default());
        Self {
            pool,
            codemap: CodeMap::new(),
            parser: ParseTasks::new(pool),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Ident<'pool>(pub u32, pub PhantomData<&'pool str>);

impl Flag {
    pub const fn ident<'p>(self) -> Ident<'p> {
        Ident(self as u32, PhantomData)
    }
}

impl<'p> Ident<'p> {
    pub fn null() -> Ident<'p> {
        Ident(0, PhantomData)
    }
}

impl std::fmt::Debug for Ident<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "S{}", self.0)
    }
}

impl<'p> InterpSend<'p> for Ident<'p> {
    fn get_type_key() -> u128 {
        // i dare you to change the generic to Self
        unsafe { std::mem::transmute(std::any::TypeId::of::<Ident>()) }
    }

    fn create_type(_: &mut crate::ast::Program) -> crate::ast::TypeId {
        TypeId::ident
    }

    fn get_or_create_type(_: &mut crate::ast::Program) -> crate::ast::TypeId {
        TypeId::ident
    }

    fn get_type(_: &crate::ast::Program) -> crate::ast::TypeId {
        TypeId::ident
    }

    fn name() -> String {
        "Symbol".to_string()
    }
}
