use std::mem::{transmute, ManuallyDrop};

use codemap::{CodeMap, Span};

use crate::{
    ast::{FatExpr, FatStmt},
    compiler::{CErr, Res},
    lex::Lexer,
    parse::{ParseFile, ParseTasks, Parser},
    pool::{Ident, StringPool},
};

use crate::export_ffi::BigResult::*;

#[cfg(feature = "self_hosted")]
pub struct SelfHosted<'p> {
    pool: *mut (),
    codemap: *mut (),
    parser: *mut (),
    a: marker::PhantomData::PhantomData<&'p u8>,
}

#[cfg(feature = "self_hosted")]
#[allow(improper_ctypes)]
#[link(name = "franca")]
extern "C" {
    fn init_self_hosted<'p>() -> SelfHosted<'p>;
    fn finish_pending<'p>(s: *mut (), id: usize) -> Res<'p, FatExpr<'p>>;
    fn insert_owned<'p>(s: *mut (), s: &[u8]) -> Ident<'p>;
    fn get<'p>(s: *mut (), s: Ident<'p>) -> &'p [u8];
    fn add_file(s: *mut (), name: &str, content: &str) -> Span;
    fn source_slice(s: *mut (), span: Span) -> *const str;
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
        todo!()
    }

    pub fn wait_for_expr(&self, id: usize) -> Res<'p, FatExpr<'p>> {
        // TODO: HACK because the franca side doesn't know how to clone.
        //       wrong allocator! this only works because i just leak all the memory! -- Jun 23
        let e = ManuallyDrop::new(unsafe { finish_pending(self.parser, id) });
        ManuallyDrop::into_inner(e.clone())
    }

    pub fn add_file(&self, name: String, content: String) -> Span {
        // TODO: my version of the codemap doesn't want to own things. :LEAK
        unsafe { add_file(self.codemap, name.leak(), content.leak()) }
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
    pub pool: &'p StringPool<'p>,
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
