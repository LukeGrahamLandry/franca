use std::{
    collections::HashMap,
    default, mem,
    ops::{Deref, DerefMut},
};

use crate::{
    ast::{Expr, FatExpr, Func, LazyFnType, LazyType, Stmt, TypeId, Var, VarInfo, VarType},
    interp::Value,
    pool::Ident,
};

#[derive(Default, Debug)]
pub struct ResolveScope<'p> {
    next_var: usize,
    scopes: Vec<Vec<Var<'p>>>,
    track_captures_before_scope: Vec<usize>,
    captures: Vec<Var<'p>>,
    info: Vec<VarInfo>,
    local_constants: Vec<Vec<Stmt<'p>>>,
}

impl<'p> ResolveScope<'p> {
    // TODO: we know when we're at the top level where order doesn't matter, so should prescan for decls?
    //       then functions could be delt with here too.
    // TODO: will need to keep some state about this for macros that want to add vars?
    // TODO: instead of doing this up front, should do this tree walk at the same time as the interp is doing it?
    pub fn of(stmts: &mut Func<'p>) -> Vec<VarInfo> {
        let mut resolver = ResolveScope::default();

        resolver.push_scope(true);
        resolver.resolve_func(stmts);
        let (globals, _) = resolver.pop_scope();

        assert!(resolver.scopes.is_empty(), "ICE: unmatched scopes");
        resolver.info
    }

    fn resolve_func(&mut self, func: &mut Func<'p>) {
        self.local_constants.push(Default::default());
        self.push_scope(true);
        for name in func.arg_names.iter().flatten() {
            self.decl_var(name);
            self.info.push(VarInfo {
                ty: TypeId::any(),
                kind: VarType::Var,
            });
        }
        match &mut func.ty {
            LazyFnType::Finished(_, _) => {}
            LazyFnType::Pending { arg, ret } => {
                match arg {
                    LazyType::Infer => {}
                    LazyType::PendingEval(ty) => self.resolve_expr(ty),
                    LazyType::Finished(_) => {}
                }
                match ret {
                    LazyType::Infer => {}
                    LazyType::PendingEval(ty) => self.resolve_expr(ty),
                    LazyType::Finished(_) => {}
                }
            }
        }
        self.push_scope(false);
        if let Some(body) = &mut func.body {
            self.resolve_expr(body)
        }
        let (outer_locals, _) = self.pop_scope();
        assert!(outer_locals.is_empty(), "function needs block");

        let (args, captures) = self.pop_scope();
        func.arg_vars = Some(args);
        let capures = captures.unwrap();
        // Now check which things we captured from *our* parent.
        for c in &capures {
            self.find_var(&c.0); // This adds it back to self.captures if needed
        }
        func.capture_vars = capures;
        func.local_constants = self.local_constants.pop().unwrap();
    }

    fn resolve_stmt(&mut self, stmt: &mut Stmt<'p>) {
        match stmt {
            Stmt::DeclNamed {
                name,
                ty,
                value,
                kind,
            } => {
                if let Some(ty) = ty {
                    self.resolve_expr(ty);
                }
                if let Some(value) = value {
                    self.resolve_expr(value);
                }
                let (old, new) = self.decl_var(name);
                self.info.push(VarInfo {
                    ty: TypeId::any(),
                    kind: *kind,
                });
                let decl = Stmt::DeclVar {
                    name: new,
                    ty: mem::replace(ty, Some(FatExpr::null())),
                    value: mem::replace(value, Some(FatExpr::null())),
                    dropping: old,
                    kind: *kind,
                };
                if *kind == VarType::Const {
                    self.local_constants.last_mut().unwrap().push(decl);
                    *stmt = Stmt::Noop;
                } else {
                    *stmt = decl;
                }
            }
            Stmt::SetNamed(name, e) => {
                self.resolve_expr(e);
                let var = self.find_var(name).expect("undeclared var");
                let value = mem::replace(e, FatExpr::null());
                *stmt = Stmt::SetVar(var, value);
            }
            Stmt::Noop => {}
            Stmt::Eval(e) => self.resolve_expr(e),
            Stmt::DeclFunc(func) => {
                self.resolve_func(func);
                self.local_constants
                    .last_mut()
                    .unwrap()
                    .push(mem::replace(stmt, Stmt::Noop));
            }
            Stmt::DeclVar { .. } | Stmt::SetVar(_, _) => {
                unreachable!("added by this pass {stmt:?}")
            }
        }
    }

    fn resolve_expr(&mut self, expr: &mut FatExpr<'p>) {
        match expr.deref_mut() {
            Expr::Call(f, arg) => {
                self.resolve_expr(f);
                self.resolve_expr(arg);
            }
            Expr::Block {
                body,
                result,
                locals,
            } => {
                assert!(locals.is_none());
                self.push_scope(false);
                for stmt in body {
                    self.resolve_stmt(stmt);
                }
                self.resolve_expr(result);
                let (vars, _) = self.pop_scope();
                mem::replace(locals, Some(vars));
            }
            Expr::ArrayLiteral(values) | Expr::Tuple(values) => {
                for value in values {
                    self.resolve_expr(value);
                }
            }
            Expr::SuffixMacro(_, e) | Expr::RefType(e) => self.resolve_expr(e),
            Expr::Closure(func) => self.resolve_func(func),
            Expr::Value(_) => {}
            Expr::GetNamed(name) => {
                if let Some(var) = self.find_var(name) {
                    mem::replace(expr.deref_mut(), Expr::GetVar(var));
                }
                // else it might be a global, like a function with overloading, or undeclared. We'll find out later.
            }
            Expr::StructLiteral(fields) => {
                for field in fields {
                    self.resolve_expr(&mut field.ty);
                }
            }
            Expr::EnumLiteral(_) => todo!(),
            Expr::GetVar(_) => unreachable!("added by this pass {expr:?}"),
        }
    }

    fn find_var(&mut self, name: &Ident<'p>) -> Option<Var<'p>> {
        let boundery = *self.track_captures_before_scope.last().unwrap();
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(found) = scope.iter().position(|v| v.0 == *name) {
                let v = scope[found];
                if i < boundery
                    && !self.captures.contains(&v)
                    && self.info[v.1].kind != VarType::Const
                {
                    // We got it from our parent function.
                    self.captures.push(v);
                }
                return Some(v);
            }
        }
        None
    }

    #[must_use]
    fn decl_var(&mut self, name: &Ident<'p>) -> (Option<Var<'p>>, Var<'p>) {
        let var = Var(*name, self.next_var);
        self.next_var += 1;
        let current = self.scopes.last_mut().unwrap();
        // Only checking current scope for something to drop.
        let old = if let Some(found) = current.iter().position(|v| v.0 == *name) {
            Some(mem::replace(&mut current[found], var))
        } else {
            current.push(var);
            None
        };
        (old, var)
    }

    fn push_scope(&mut self, track_captures: bool) {
        self.track_captures_before_scope.push(if track_captures {
            self.scopes.len()
        } else {
            *self.track_captures_before_scope.last().unwrap()
        });
        self.scopes.push(Default::default());
    }

    #[must_use]
    fn pop_scope(&mut self) -> (Vec<Var<'p>>, Option<Vec<Var<'p>>>) {
        let boundery = self.track_captures_before_scope.pop().unwrap();
        let vars = self.scopes.pop().unwrap();
        let captures = if boundery == self.scopes.len() {
            Some(mem::take(&mut self.captures))
        } else {
            None
        };
        (vars, captures)
    }
}
