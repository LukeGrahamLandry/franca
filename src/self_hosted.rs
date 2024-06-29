use std::{marker::PhantomData, mem::ManuallyDrop, ptr::addr_of};

use crate::{
    ast::{FatExpr, FatStmt, Flag, Func, LazyType, Pattern, ScopeId, TypeId, Var},
    compiler::{CErr, Compile, Res, Scope},
    err,
    export_ffi::{BigOption, ImportVTable, IMPORT_VTABLE},
    ffi::InterpSend,
    logging::make_err,
};

use crate::export_ffi::BigResult::*;

pub struct SelfHosted<'p> {
    pub pool: *mut (),
    pub codemap: *mut (),
    parser: *mut (),
    _arena: *mut (),
    scopes: *mut (),
    vtable: *const ImportVTable,
    a: PhantomData<&'p u8>,
}

pub struct Scopes<'p> {
    pub scopes: Vec<Scope<'p>>,
    pub next_var: u32,
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
    pub(crate) fn emit_llvm();
    pub fn show_error_line(codemap: *mut (), span_low: u32, span_high: u32);

    fn put_constant(scopes: *mut (), name: Var, value: FatExpr, ty: LazyType);
    fn get_var_type(scopes: *mut (), v: Var) -> BigOption<TypeId>;
    fn put_var_type(scopes: *mut (), v: Var, ty: TypeId) -> bool;
    fn get_constant<'p>(scopes: *mut (), name: Var<'p>) -> BigOption<&'p mut (FatExpr<'p>, LazyType<'p>)>;
    fn find_constant_in_scope<'p>(scopes: *mut (), s: ScopeId, name: Ident<'p>) -> BigOption<Var<'p>>;
    fn dup_var<'p>(scopes: *mut (), old: Var<'p>) -> Var<'p>;
    pub fn resolve_root<'p>(compiler: &mut SelfHosted<'p>, func: &mut Func<'p>, scope: ScopeId) -> BigResult<ParseErr<'p>, ()>;
    pub fn resolve_sign<'p>(compiler: &mut SelfHosted<'p>, func: &mut Func<'p>) -> BigResult<ParseErr<'p>, ()>;
    pub fn resolve_body<'p>(compiler: &mut SelfHosted<'p>, func: &mut Func<'p>) -> BigResult<ParseErr<'p>, ()>;
    // For unquoting, initial will be None, but for inlining closures, it will be the return variable.
    fn renumber_expr<'p>(compiler: &mut SelfHosted<'p>, expr: &mut FatExpr<'p>, initial: BigOption<(Var<'p>, Var<'p>)>);
    fn maybe_renumber_and_dup_scope<'p>(compiler: &mut SelfHosted<'p>, new_func: &mut Func<'p>) -> BigResult<ParseErr<'p>, ()>;

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
            Err(t) => {
                let mut e = make_err(CErr::Fatal(t.msg.to_string()));
                e.loc = Some(t.span);
                Err(e)
            }
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

    pub(crate) fn print_diagnostic(&self, e: crate::compiler::CompileError<'p>) {
        unsafe {
            if let Some(loc) = e.loc {
                show_error_line(self.codemap, loc.low, loc.high);
            }
        }
        // TODO
        println!("{e:?}");
    }

    pub(crate) fn put_constant(&mut self, name: crate::ast::Var<'p>, value: FatExpr<'p>, ty: LazyType<'p>) {
        unsafe { put_constant(self.scopes, name, value, ty) }
    }

    pub fn get_var_type(&self, v: Var) -> BigOption<TypeId> {
        unsafe { get_var_type(self.scopes, v) }
    }

    pub(crate) fn put_var_type(&mut self, v: Var<'p>, ty: TypeId) {
        unsafe { put_var_type(self.scopes, v, ty) };
    }

    pub(crate) fn get_constant(&mut self, name: Var<'p>) -> BigOption<&mut (FatExpr<'p>, LazyType<'p>)> {
        unsafe { get_constant(self.scopes, name) }
    }

    pub(crate) fn find_in_scope(&self, s: ScopeId, name: Ident<'p>) -> BigOption<Var<'p>> {
        unsafe { find_constant_in_scope(self.scopes, s, name) }
    }

    pub(crate) fn maybe_renumber_and_dup_scope(&mut self, new_func: &mut Func<'p>) -> BigResult<(), Box<crate::compiler::CompileError<'p>>> {
        match unsafe { maybe_renumber_and_dup_scope(self, new_func) } {
            Ok(_) => Ok(()),
            Err(e) => err!("{e:?}",),
        }
    }

    pub(crate) fn renumber_expr(&mut self, expr: &mut FatExpr<'p>, initial: BigOption<(Var<'p>, Var<'p>)>) {
        unsafe { renumber_expr(self, expr, initial) }
    }

    pub(crate) fn dup_var(&mut self, old: Var<'p>) -> Var<'p> {
        unsafe { dup_var(self.scopes, old) }
    }
}

impl<'p> Default for SelfHosted<'p> {
    fn default() -> Self {
        let mut temp = unsafe { init_self_hosted() };
        temp.vtable = addr_of!(IMPORT_VTABLE);
        temp
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
