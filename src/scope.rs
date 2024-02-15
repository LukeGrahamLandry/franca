use std::{mem, ops::DerefMut};

use crate::{
    ast::{
        Expr, FatExpr, FatStmt, Func, LazyFnType, LazyType, Stmt, TypeId, Var, VarInfo, VarType,
    },
    logging::{log, logln, PoolLog},
    pool::{Ident, StringPool},
};

pub struct ResolveScope<'p> {
    next_var: usize,
    scopes: Vec<Vec<Var<'p>>>,
    track_captures_before_scope: Vec<usize>,
    captures: Vec<Var<'p>>,
    info: Vec<VarInfo>,
    local_constants: Vec<Vec<FatStmt<'p>>>,
    pool: &'p StringPool<'p>,
}

impl<'p> ResolveScope<'p> {
    // TODO: we know when we're at the top level where order doesn't matter, so should prescan for decls?
    //       then functions could be delt with here too.
    // TODO: will need to keep some state about this for macros that want to add vars?
    // TODO: instead of doing this up front, should do this tree walk at the same time as the interp is doing it?
    pub fn of(stmts: &mut Func<'p>, pool: &'p StringPool<'p>) -> Vec<VarInfo> {
        let mut resolver = ResolveScope {
            next_var: Default::default(),
            scopes: Default::default(),
            track_captures_before_scope: Default::default(),
            captures: Default::default(),
            info: Default::default(),
            local_constants: Default::default(),
            pool,
        };

        resolver.push_scope(true);
        resolver.resolve_func(stmts);
        let (_globals, outer_captures) = resolver.pop_scope();

        assert!(resolver.scopes.is_empty(), "ICE: unmatched scopes");
        assert!(outer_captures.unwrap().is_empty(), "unreachable?");
        resolver.info
    }

    fn resolve_func(&mut self, func: &mut Func<'p>) {
        self.local_constants.push(Default::default());
        self.push_scope(true);
        for name in func.arg_names.iter().flatten() {
            let _ = self.decl_var(name);
            self.info.push(VarInfo {
                ty: TypeId::any(),
                kind: VarType::Var,
                loc: func.loc,
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

        for v in capures {
            if self.info[v.1].kind == VarType::Const {
                func.capture_vars_const.push(v);
            } else {
                func.capture_vars.push(v);
            }
        }

        func.local_constants = self.local_constants.pop().unwrap();

        logln!();
        logln!("Scope for Func {:?}", func.synth_name(self.pool));
        logln!(
            "- Runtime captures: {:?}",
            func.capture_vars
                .iter()
                .map(|v| v.log(self.pool))
                .collect::<Vec<String>>()
        );
        logln!(
            "- Const captures: {:?}",
            func.capture_vars_const
                .iter()
                .map(|v| v.log(self.pool))
                .collect::<Vec<String>>()
        );

        logln!("- Const locals:");
        for d in &func.local_constants {
            logln!("    - {:?}", d.log(self.pool).replace("\n", " "));
        }
    }

    fn resolve_stmt(&mut self, stmt: &mut FatStmt<'p>) {
        let loc = stmt.loc;
        for a in &mut stmt.annotations {
            if let Some(args) = &mut a.args {
                self.resolve_expr(args)
            }
        }

        let aaa = stmt.annotations.clone();
        match stmt.deref_mut() {
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
                    loc,
                });
                let decl = Stmt::DeclVar {
                    name: new,
                    ty: mem::replace(ty, Some(FatExpr::null(loc))),
                    value: mem::replace(value, Some(FatExpr::null(loc))),
                    dropping: old,
                    kind: *kind,
                };
                if *kind == VarType::Const {
                    self.local_constants
                        .last_mut()
                        .unwrap()
                        .push(decl.fat_with(mem::take(&mut stmt.annotations), stmt.loc));
                    stmt.stmt = Stmt::Noop;
                } else {
                    stmt.stmt = decl;
                }
            }
            Stmt::Set { place, value } => {
                self.resolve_expr(place);
                self.resolve_expr(value);
            }
            Stmt::Noop => {}
            Stmt::Eval(e) => self.resolve_expr(e),
            Stmt::DeclFunc(func) => {
                if let Some(name) = func.name {
                    let v = self.decl_var(&name);
                    self.info.push(VarInfo {
                        ty: TypeId::any(),
                        kind: VarType::Const,
                        loc: func.loc,
                    });
                }
                func.annotations = aaa;
                self.resolve_func(func);
                self.local_constants
                    .last_mut()
                    .unwrap()
                    .push(mem::replace(stmt, Stmt::Noop.fat_empty(loc)));
            }
            Stmt::DeclVar { .. } => {
                unreachable!("added by this pass {stmt:?}")
            }
        }
    }

    fn resolve_expr(&mut self, expr: &mut FatExpr<'p>) {
        match expr.deref_mut() {
            Expr::GenericArgs(f, arg) | Expr::Call(f, arg) => {
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
                *locals = Some(vars);
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
                    *expr.deref_mut() = Expr::GetVar(var);
                }
                // else it might be a global, like a function with overloading, or undeclared. We'll find out later.
            }
            Expr::EnumLiteral(_) => todo!(),
            Expr::GetVar(_) => unreachable!("added by this pass {expr:?}"),
            Expr::FieldAccess(e, _) => self.resolve_expr(e),
            Expr::StructLiteralP(p) => {
                for e in p.types.iter_mut().flatten() {
                    self.resolve_expr(e);
                }
            }
            Expr::String(_) => {}
        }
    }

    fn find_var(&mut self, name: &Ident<'p>) -> Option<Var<'p>> {
        let boundery = *self.track_captures_before_scope.last().unwrap();
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if let Some(found) = scope.iter().position(|v| v.0 == *name) {
                let v = scope[found];
                if i < boundery && !self.captures.contains(&v) {
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
