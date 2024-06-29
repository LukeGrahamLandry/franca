use std::{
    marker::PhantomData,
    mem::{self, ManuallyDrop},
    ptr::addr_of,
};

use crate::{
    ast::{FatExpr, FatStmt, Flag, FnFlag, Func, LazyType, Pattern, ScopeId, TypeId, Var, WalkAst},
    compiler::{CErr, Compile, Res, Scope},
    err,
    export_ffi::{BigOption, ImportVTable, IMPORT_VTABLE},
    ffi::InterpSend,
    logging::make_err,
    Map,
};

use crate::export_ffi::BigResult::*;

pub struct SelfHosted<'p> {
    pub pool: *mut (),
    pub codemap: *mut (),
    parser: *mut (),
    _arena: *mut (),
    #[cfg(not(feature = "self_scope"))]
    pub scopes: Option<Box<Scopes<'p>>>, // small option because its a nullable pointer currently. TEMP
    #[cfg(feature = "self_scope")]
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
    pub(crate) fn get_include_std(arg: *mut Compile, name: &str) -> BigOption<usize>; // TODO: calling convention!!
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

    #[cfg(not(feature = "self_scope"))]
    pub fn new_scope(&mut self, parent: ScopeId, name: Ident<'p>, block_in_parent: usize) -> ScopeId {
        let scopes = self.scopes.as_mut().unwrap();
        let depth = if scopes.scopes.is_empty() {
            0
        } else {
            scopes.scopes[parent.as_index()].depth + 1
        }; // HACK
        scopes.scopes.push(Scope {
            parent,
            constants: Default::default(),
            vars: Default::default(),
            depth,
            name,
            block_in_parent,
            rt_types: Default::default(),
        });
        ScopeId::from_index(scopes.scopes.len() - 1)
    }

    #[cfg(not(feature = "self_scope"))]
    fn dup_scope(&mut self, prev: ScopeId, mapping: Map<Var<'p>, Var<'p>>) -> ScopeId {
        let scopes = self.scopes.as_mut().unwrap();
        let old_scope = &scopes.scopes[prev.as_index()];
        let p = old_scope.parent;
        let bp = old_scope.block_in_parent;
        let n = old_scope.name;
        let id = self.new_scope(p, n, bp);
        let scopes = self.scopes.as_mut().unwrap();
        let old_scope = &scopes.scopes[prev.as_index()];
        let old_constants = old_scope.constants.clone();
        // TODO: just take the part you need. rn this copys more and more every time! -- May 29
        //       i think this is still true -- Jun 3
        let old_vars = old_scope.vars.clone();
        // can't use the copy directly because need to rehash
        for (k, v) in old_constants {
            if let Some(new) = mapping.get(&k) {
                scopes.scopes[id.as_index()].constants.insert(*new, v);
            }
        }

        for mut block in old_vars {
            block.vars.retain_mut(|k| {
                if let Some(new) = mapping.get(k) {
                    *k = *new;
                    true
                } else {
                    false
                }
            });
            scopes.scopes[id.as_index()].vars.push(block);
        }
        id
    }

    #[cfg(not(feature = "self_scope"))]
    pub(crate) fn put_constant(&mut self, name: crate::ast::Var<'p>, value: FatExpr<'p>, ty: LazyType<'p>) {
        let scopes = self.scopes.as_mut().unwrap();
        scopes.scopes[name.scope.as_index()].constants.insert(name, (value, ty));
    }

    #[cfg(not(feature = "self_scope"))]
    pub fn get_var_type(&self, v: Var) -> BigOption<TypeId> {
        let scopes = self.scopes.as_ref().unwrap();
        scopes.scopes[v.scope.as_index()].rt_types.get(&v).copied().into()
    }

    #[cfg(not(feature = "self_scope"))]
    pub(crate) fn put_var_type(&mut self, name: Var<'p>, ty: TypeId) {
        let scopes = self.scopes.as_mut().unwrap();
        scopes.scopes[name.scope.as_index()].rt_types.insert(name, ty);
    }

    #[cfg(not(feature = "self_scope"))]
    pub(crate) fn get_constant(&mut self, name: Var<'p>) -> BigOption<&mut (FatExpr<'p>, LazyType<'p>)> {
        let scopes = self.scopes.as_mut().unwrap();
        scopes.scopes[name.scope.as_index()].constants.get_mut(&name).into()
    }

    #[cfg(not(feature = "self_scope"))]
    pub(crate) fn find_in_scope(&self, s: ScopeId, name: Ident<'p>) -> BigOption<Var<'p>> {
        let scopes = self.scopes.as_ref().unwrap();
        scopes.scopes[s.as_index()].constants.keys().find(|v| v.name == name).copied().into()
    }

    #[cfg(not(feature = "self_scope"))]
    pub(crate) fn maybe_renumber_and_dup_scope(&mut self, new_func: &mut Func<'p>) -> BigResult<(), Box<crate::compiler::CompileError<'p>>> {
        use FnFlag::*;
        // Closures always resolve up front, so they need to renumber the clone.
        // TODO: HACK but closures get renumbered when inlined anyway, so its just the const args that matter. im just being lazy and doing the whole thing redundantly -- May 9
        // normal functions with const args havent had their body resolved yet so don't have to deal with it, we only resolve on the clone.
        // the special case for generics is when args can reference previous ones so they have to resolve sign earlier.
        let needs_renumber = new_func.get_flag(AllowRtCapture) || new_func.get_flag(Generic);
        if needs_renumber {
            // :push_type_when_create_new_var // TODO
            let mut mapping = Default::default();
            let mut renumber = crate::ast::RenumberVars {
                vars: self.scopes.as_ref().unwrap().next_var,
                mapping: &mut mapping,
            };
            renumber.func(new_func);
            let v = renumber.vars;
            if new_func.get_flag(Generic) && !new_func.get_flag(ResolvedBody) {
                // the sign has already been resolved so we need to renumber before binding arguments.
                // however, the body hasn't been resolved yet, so we can't just renumber in place.
                // instead, remap the sign as normal and then, insert a new scope containing the remapped variables,
                // and use that as the starting point when we resolve the body of new new function.
                // that way when it iterates up the scopes to resolve names, it will see our remapped shadows instead of the original.
                // this allows #generic argument types that reference the values of other argument.
                // but doing this is a bit creepy because the Var.scope isn't updated to the new one,
                // so they get inserted back in the old one's constants/rt_types again later.
                // that's why its fine when we can't find a remap for something in constants or vars.      -- May 29
                debug_assert!(new_func.get_flag(ResolvedSign));
                debug_assert!(!new_func.get_flag(AllowRtCapture));
                new_func.assert_body_not_resolved()?;
                // TODO: not true for 'name :: fn() = {}' exprs because they don't stop.
                // debug_assert!(
                //     !matches!(body.expr, Expr::Block { .. }),
                //     "Block should be GetParsed because body is not resolved yet. "
                // );
                let id = self.dup_scope(new_func.scope.unwrap(), mapping);
                new_func.scope = BigOption::Some(id);
            }
            self.scopes.as_mut().unwrap().next_var = v;
        } else {
            new_func.assert_body_not_resolved()?;
        }
        Ok(())
    }

    #[cfg(not(feature = "self_scope"))]
    pub(crate) fn renumber_expr(&mut self, expr: &mut FatExpr<'p>, initial: BigOption<(Var<'p>, Var<'p>)>) {
        let mut mapping = Map::<Var, Var>::default();
        if let BigOption::Some((old, new)) = initial {
            mapping.insert(old, new);
        }
        let mut ctx = crate::ast::RenumberVars {
            vars: self.scopes.as_ref().unwrap().next_var,
            mapping: &mut mapping,
        };
        ctx.expr(expr);
        self.scopes.as_mut().unwrap().next_var = ctx.vars;
    }

    #[cfg(not(feature = "self_scope"))]
    pub(crate) fn dup_var(&mut self, old: Var<'p>) -> Var<'p> {
        let mut new = old;
        new.id = self.scopes.as_mut().unwrap().next_var;
        self.scopes.as_mut().unwrap().next_var += 1;
        new
    }

    #[cfg(feature = "self_scope")]
    pub(crate) fn put_constant(&mut self, name: crate::ast::Var<'p>, value: FatExpr<'p>, ty: LazyType<'p>) {
        unsafe { put_constant(self.scopes, name, value, ty) }
    }

    #[cfg(feature = "self_scope")]
    pub fn get_var_type(&self, v: Var) -> BigOption<TypeId> {
        unsafe { get_var_type(self.scopes, v) }
    }

    #[cfg(feature = "self_scope")]
    pub(crate) fn put_var_type(&mut self, v: Var<'p>, ty: TypeId) {
        unsafe { put_var_type(self.scopes, v, ty) };
    }

    #[cfg(feature = "self_scope")]
    pub(crate) fn get_constant(&mut self, name: Var<'p>) -> BigOption<&mut (FatExpr<'p>, LazyType<'p>)> {
        unsafe { get_constant(self.scopes, name) }
    }

    #[cfg(feature = "self_scope")]
    pub(crate) fn find_in_scope(&self, s: ScopeId, name: Ident<'p>) -> BigOption<Var<'p>> {
        unsafe { find_constant_in_scope(self.scopes, s, name) }
    }

    #[cfg(feature = "self_scope")]
    pub(crate) fn maybe_renumber_and_dup_scope(&mut self, new_func: &mut Func<'p>) -> BigResult<(), Box<crate::compiler::CompileError<'p>>> {
        match unsafe { maybe_renumber_and_dup_scope(self, new_func) } {
            Ok(_) => Ok(()),
            Err(e) => err!("{e:?}",),
        }
    }

    #[cfg(feature = "self_scope")]
    pub(crate) fn renumber_expr(&mut self, expr: &mut FatExpr<'p>, initial: BigOption<(Var<'p>, Var<'p>)>) {
        unsafe { renumber_expr(self, expr, initial) }
    }

    #[cfg(feature = "self_scope")]
    pub(crate) fn dup_var(&mut self, old: Var<'p>) -> Var<'p> {
        unsafe { dup_var(self.scopes, old) }
    }
}

impl<'p> Default for SelfHosted<'p> {
    fn default() -> Self {
        let mut temp = unsafe { init_self_hosted() };

        #[cfg(not(feature = "self_scope"))]
        {
            mem::forget(temp.scopes); // TODO: remove. temp while there are two impls that disagree on what this is. -- Jun 28
            temp.scopes = Some(Box::new(Scopes { scopes: vec![], next_var: 0 }));
        }

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
