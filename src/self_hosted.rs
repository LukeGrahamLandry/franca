use std::{
    marker::PhantomData,
    mem::{transmute, ManuallyDrop},
};

use crate::{
    ast::{FatExpr, FatStmt, Flag, Func, LazyType, Pattern, TypeId},
    compiler::{CErr, Compile, Res},
    err,
    export_ffi::{BigOption, ImportVTable},
    ffi::InterpSend,
};

use crate::export_ffi::BigResult::*;

pub struct SelfHosted<'p> {
    pub pool: *mut (),
    codemap: *mut (),
    parser: *mut (),
    _arena: *mut (),
    a: PhantomData<&'p u8>,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct Span {
    pub(crate) low: u32,
    pub(crate) high: u32,
}

#[derive(Clone, Copy, Debug)]
#[repr(C)]
pub struct ParseErr<'p> {
    pub span: Span,
    pub msg: &'p str,
}

use crate::export_ffi::BigResult;

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
    pub(crate) fn log_stmt(pool: *mut (), s: &FatStmt) -> *const str;
    pub(crate) fn log_expr(pool: *mut (), s: &FatExpr) -> *const str;
    pub(crate) fn log_pattern(pool: *mut (), s: &Pattern) -> *const str;
    pub(crate) fn log_func(pool: *mut (), s: &Func) -> *const str;
    pub(crate) fn log_lazy_type(pool: *mut (), s: &LazyType) -> *const str;
    pub fn self_hosted_main(vtable: *const ImportVTable);
    pub(crate) fn get_include_std(arg: *mut Compile, name: &str) -> BigOption<usize>; // TODO: calling convention!!
    pub(crate) fn emit_llvm();
    fn show_error_line(codemap: *mut (), span_low: u32, span_high: u32);
}

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
        let e = unsafe { finish_pending_stmts(self.parser, id) };
        match e {
            Ok(t) => Ok(t),
            Err(t) => {
                unsafe {
                    show_error_line(self.codemap, t.span.low, t.span.high);
                }
                err!("{:?}", t)
            }
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

    pub fn add_task(&mut self, _: bool, span: Span) -> usize {
        unsafe {
            // println!("add task {span:?}");
            let src = source_slice(self.codemap, span.low, span.high);
            push_parse(self.parser, &*src, span.low, span.high)
        }
    }

    pub fn lookup_filename(&self, span: Span) -> &'p str {
        // TODO
        "unknownfiletodo"
    }

    pub(crate) fn print_diagnostic(&self, e: crate::compiler::CompileError<'p>) {
        unsafe {
            if let Some(loc) = e.loc {
                show_error_line(self.codemap, loc.low, loc.high);
            }
        }
        // TODO
        println!("{e:?}");
    }
}

impl<'p> Default for SelfHosted<'p> {
    fn default() -> Self {
        unsafe { init_self_hosted() }
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
