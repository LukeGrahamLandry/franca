use std::{
    collections::HashMap,
    default, mem,
    ops::{Deref, DerefMut},
};

use crate::{
    ast::{Expr, FatExpr, Func, Stmt, Var},
    pool::Ident,
};

#[derive(Default)]
pub struct ResolveScope<'p> {
    next_var: usize,
    scopes: Vec<Vec<Var<'p>>>,
}

impl<'p> ResolveScope<'p> {
    // TODO: we know when we're at the top level where order doesn't matter, so should prescan for decls?
    //       then functions could be delt with here too.
    // TODO: will need to keep some state about this for macros that want to add vars?
    // TODO: instead of doing this up front, should do this tree walk at the same time as the interp is doing it?
    pub fn of(stmts: &mut [Stmt]) {
        let mut resolver = ResolveScope::default();

        resolver.push_scope();
        for s in stmts {
            resolver.resolve_stmt(s);
        }
        let globals = resolver.pop_scope();

        assert!(resolver.scopes.is_empty(), "ICE: unmatched scopes")
    }

    fn resolve_func(&mut self, func: &mut Func<'p>) {
        self.push_scope();
        for name in func.arg_names.iter().flatten() {
            self.decl_var(name);
        }
        self.push_scope();
        if let Some(body) = &mut func.body {
            self.resolve_expr(body)
        }
        self.pop_scope();

        func.arg_vars = Some(self.pop_scope());
    }

    fn resolve_stmt(&mut self, stmt: &mut Stmt<'p>) {
        match stmt {
            Stmt::DeclNamed { name, ty, value } => {
                if let Some(value) = value {
                    self.resolve_expr(value);
                }
                let (old, new) = self.decl_var(name);
                *stmt = Stmt::DeclVar {
                    name: new,
                    ty: mem::replace(ty, Some(FatExpr::null())),
                    value: mem::replace(value, Some(FatExpr::null())),
                    dropping: old,
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
            Stmt::DeclFunc(func) => self.resolve_func(func),
            Stmt::DeclVar { .. } | Stmt::SetVar(_, _) | Stmt::EndScope(_) => {
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
                self.push_scope();
                for stmt in body {
                    self.resolve_stmt(stmt);
                }
                self.resolve_expr(result);
                let vars = self.pop_scope();
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
            Expr::EnumLiteral(_) => todo!(),
            Expr::StructLiteral(_) => todo!(),
            Expr::GetVar(_) => unreachable!("added by this pass {expr:?}"),
        }
    }

    fn find_var(&self, name: &Ident<'p>) -> Option<Var<'p>> {
        for scope in self.scopes.iter().rev() {
            if let Some(found) = scope.iter().position(|v| v.0 == *name) {
                return Some(scope[found]);
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

    fn push_scope(&mut self) {
        self.scopes.push(Default::default());
    }

    fn pop_scope(&mut self) -> Vec<Var<'p>> {
        self.scopes.pop().unwrap()
    }
}
