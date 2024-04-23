use std::{mem, ops::DerefMut};

use codemap::Span;

use crate::{
    assert,
    ast::{Binding, Expr, FatExpr, FatStmt, Func, LazyType, Name, ScopeId, Stmt, Var, VarType},
    compiler::{Compile, Res},
    err,
    logging::LogTag::Scope,
    outln,
    pool::Ident,
};

pub struct ResolveScope<'z, 'a, 'p> {
    scopes: Vec<Vec<Var<'p>>>,
    track_captures_before_scope: Vec<usize>,
    captures: Vec<Var<'p>>,
    local_constants: Vec<Vec<FatStmt<'p>>>,
    compiler: &'z mut Compile<'a, 'p>,
    last_loc: Span,
    scope_stack: Vec<ScopeId>,
}

impl<'z, 'a, 'p> ResolveScope<'z, 'a, 'p> {
    // TODO: we know when we're at the top level where order doesn't matter, so should prescan for decls?
    //       then functions could be delt with here too.
    // TODO: will need to keep some state about this for macros that want to add vars?
    // TODO: instead of doing this up front, should do this tree walk at the same time as the interp is doing it?
    pub fn of(stmts: &mut Func<'p>, compiler: &'z mut Compile<'a, 'p>) -> Res<'p, ()> {
        let mut resolver = ResolveScope {
            scopes: Default::default(),
            track_captures_before_scope: Default::default(),
            captures: Default::default(),
            local_constants: Default::default(),
            compiler,
            last_loc: stmts.loc,
            scope_stack: vec![ScopeId::from_index(0)],
        };
        if let Err(mut e) = resolver.run(stmts) {
            e.loc = e.loc.or(Some(resolver.last_loc));
            return Err(e);
        }
        assert!(resolver.scopes.is_empty(), "ICE: unmatched scopes");
        Ok(())
    }

    fn run(&mut self, stmts: &mut Func<'p>) -> Res<'p, ()> {
        self.push_scope(true);
        self.push_scope(true);
        self.resolve_func(stmts)?;
        let (_globals, _captured_imports) = self.pop_scope();
        let (_imports, outer_captures) = self.pop_scope();
        let outer_captures = outer_captures.expect("well formed blocks (ICE)");
        assert!(outer_captures.is_empty(), "unreachable? {:?}", outer_captures);
        Ok(())
    }

    fn resolve_func(&mut self, func: &mut Func<'p>) -> Res<'p, ()> {
        self.local_constants.push(Default::default());
        self.push_scope(true);
        for b in &mut func.arg.bindings {
            self.resolve_binding(b, true, func.loc)?;
        }
        self.walk_ty(&mut func.ret);
        self.push_scope(false);
        if let Some(body) = &mut func.body {
            self.resolve_expr(body)?;
        }
        let (outer_locals, _) = self.pop_scope();
        assert!(outer_locals.is_empty(), "function needs block");

        let (_args, captures) = self.pop_scope();
        let capures = captures.unwrap();
        // Now check which things we captured from *our* parent.
        for c in &capures {
            self.find_var(&c.0); // This adds it back to self.captures if needed
        }

        for v in capures {
            if v.3 == VarType::Const {
                func.capture_vars_const.push(v);
            } else {
                func.capture_vars.push(v);
            }
        }
        // if !func.allow_rt_capture {
        //     assert!(func.capture_vars.is_empty(), "{}", func.log(self.compiler.pool));
        // }

        debug_assert!(func.local_constants.is_empty());
        func.local_constants = self.local_constants.pop().unwrap();

        outln!(Scope, "{}", func.log_captures(self.compiler.pool));
        Ok(())
    }

    fn resolve_stmt_if_constant(&mut self, stmt: &mut FatStmt<'p>) -> Res<'p, ()> {
        let loc = stmt.loc;
        match stmt.deref_mut() {
            Stmt::DeclNamed { name, ty, value, kind } => {
                if *kind == VarType::Const {
                    let new = self.decl_var(name, *kind, loc)?;
                    let decl = Stmt::DeclVar {
                        name: new,
                        ty: mem::replace(ty, LazyType::Infer),
                        value: mem::replace(value, Some(FatExpr::null(loc))),
                        kind: *kind,
                    };
                    stmt.stmt = decl;
                    // Don't move to local_constants yet because want value to be able to reference later constants
                }
            }
            Stmt::DeclFunc(func) => {
                assert!(func.referencable_name);
                // Functions don't shadow, they just add to an overload group.
                // TODO: what happens if you're shadowing a normal variable? just err maybe?
                // TOOD: @pub vs @private
                if let Some(v) = self.find_var(&func.name) {
                    func.var_name = Some(v);
                } else {
                    let v = self.decl_var(&func.name, VarType::Const, func.loc)?;
                    func.var_name = Some(v);
                }

                // Don't move to local_constants yet because want value to be able to reference later constants
            }
            _ => {}
        }

        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &mut FatStmt<'p>) -> Res<'p, ()> {
        let loc = stmt.loc;
        self.last_loc = loc;
        for a in &mut stmt.annotations {
            if let Some(args) = &mut a.args {
                self.resolve_expr(args)?
            }
        }

        let aaa = stmt.annotations.clone();
        match stmt.deref_mut() {
            Stmt::DoneDeclFunc(_) => unreachable!("compiled twice?"),
            Stmt::DeclNamed { name, ty, value, kind } => {
                assert!(*kind != VarType::Const);
                self.walk_ty(ty);
                if let Some(value) = value {
                    self.resolve_expr(value)?;
                }

                let new = self.decl_var(name, *kind, loc)?;
                let decl = Stmt::DeclVar {
                    name: new,
                    ty: mem::replace(ty, LazyType::Infer),
                    value: mem::replace(value, Some(FatExpr::null(loc))),
                    kind: *kind,
                };
                stmt.stmt = decl;
            }
            Stmt::DeclFunc(func) => {
                func.annotations = aaa;
                self.resolve_func(func)?;
                self.local_constants
                    .last_mut()
                    .unwrap()
                    .push(mem::replace(stmt, Stmt::Noop.fat_empty(loc)));
            }
            Stmt::DeclVar { kind, ty, value, .. } => {
                assert!(*kind == VarType::Const);
                self.walk_ty(ty);
                if let Some(value) = value {
                    self.resolve_expr(value)?;
                }
                self.local_constants
                    .last_mut()
                    .unwrap()
                    .push(mem::replace(stmt, Stmt::Noop.fat_empty(loc)));
            }
            Stmt::Set { place, value } => {
                self.resolve_expr(place)?;
                self.resolve_expr(value)?;
            }
            Stmt::Noop => {}
            Stmt::Eval(e) => self.resolve_expr(e)?,

            Stmt::DeclVarPattern { binding, value } => {
                if let Some(value) = value {
                    self.resolve_expr(value)?;
                }
                for b in &mut binding.bindings {
                    self.resolve_binding(b, true, loc)?;
                }
                // TODO: add 'let (x, y) = whatever()' to the frontend.
                todo!("this isnt actually tested because only the backend makes these. its probably fine tho other than needing special constant handling.");
            }
        }
        Ok(())
    }

    // TODO: this could use walk_whatever()
    fn resolve_expr(&mut self, expr: &mut FatExpr<'p>) -> Res<'p, ()> {
        let loc = expr.loc;
        self.last_loc = loc;
        match expr.deref_mut() {
            Expr::Poison => err!("ICE: POISON",),
            Expr::WipFunc(_) => unreachable!(),
            Expr::Call(fst, snd) | Expr::Index { ptr: fst, index: snd } => {
                self.resolve_expr(fst)?;
                self.resolve_expr(snd)?;
            }
            Expr::PrefixMacro { handler, arg, target } => {
                self.resolve_expr(handler)?;
                self.resolve_expr(arg)?;
                self.resolve_expr(target)?;
            }
            Expr::Block {
                body,
                result,
                locals,
                resolved,
            } => {
                assert!(locals.is_none());
                self.push_scope(false);
                for stmt in body.iter_mut() {
                    self.resolve_stmt_if_constant(stmt)?;
                }
                for stmt in body.iter_mut() {
                    self.resolve_stmt(stmt)?;
                }
                self.resolve_expr(result)?;
                let (vars, _) = self.pop_scope();
                *locals = Some(vars);
                *resolved = true;
            }
            Expr::Tuple(values) => {
                for value in values {
                    self.resolve_expr(value)?;
                }
            }
            Expr::SuffixMacro(_, e) => self.resolve_expr(e)?,
            Expr::Closure(func) => self.resolve_func(func)?,
            Expr::Raw { .. } | Expr::Value { .. } => {}
            Expr::GetNamed(name) => {
                if let Some(var) = self.find_var(name) {
                    *expr.deref_mut() = Expr::GetVar(var);
                }
                // else it might be a global, like a function with overloading, or undeclared. We'll find out later.
            }
            Expr::GetVar(_) => unreachable!("added by this pass {expr:?}"),
            Expr::FieldAccess(e, _) => self.resolve_expr(e)?,
            Expr::StructLiteralP(p) => {
                self.push_scope(false);
                for b in &mut p.bindings {
                    self.resolve_binding(b, true, loc)?;
                }
                let _ = self.pop_scope();
            }
            Expr::String(_) => {}
        }
        Ok(())
    }

    fn find_var(&mut self, name: &Ident<'p>) -> Option<Var<'p>> {
        let boundery = *self.track_captures_before_scope.last().unwrap();
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            // Reverse so you get the shadowing first.
            if let Some(found) = scope.iter().rev().position(|v| v.0 == *name) {
                let v = scope[scope.len() - found - 1];
                if i < boundery && !self.captures.contains(&v) {
                    // We got it from our parent function.
                    self.captures.push(v);
                }
                return Some(v);
            }
        }
        None
    }

    fn decl_var(&mut self, name: &Ident<'p>, kind: VarType, loc: Span) -> Res<'p, Var<'p>> {
        let scope = if kind == VarType::Const {
            let s = *self.scope_stack.last().unwrap();
            if self.compiler[s]
                .constants
                .insert(*name, (FatExpr::synthetic(Expr::Poison, loc), LazyType::Infer))
                .is_some()
            {
                // TODO: probably want the reverse too where you can't shadow a const with a let, etc.
                // TODO: show other declaration site.
                err!(
                    "Constants (will be) order independent, so cannot shadow in the same scope: {}",
                    self.compiler.pool.get(*name)
                )
            }
            s
        } else {
            ScopeId::from_index(0)
        };
        let var = Var(*name, self.compiler.program.next_var, scope, kind); // TODO:SCOPE
        self.compiler.program.next_var += 1;
        let current = self.scopes.last_mut().unwrap();
        current.push(var);
        Ok(var)
    }

    fn get_scope(&self) -> ScopeId {
        *self.scope_stack.last().unwrap()
    }

    fn push_scope(&mut self, track_captures: bool) {
        let scope = self.get_scope();
        self.scope_stack.push(self.compiler.new_scope(scope));
        let function_scope = if track_captures {
            self.scopes.len()
        } else {
            *self.track_captures_before_scope.last().unwrap()
        };
        self.track_captures_before_scope.push(function_scope);
        self.scopes.push(Default::default());
    }

    #[must_use]
    fn pop_scope(&mut self) -> (Vec<Var<'p>>, Option<Vec<Var<'p>>>) {
        self.scope_stack.pop().unwrap();
        let boundery = self.track_captures_before_scope.pop().unwrap();
        let vars = self.scopes.pop().unwrap();
        let captures = if boundery == self.scopes.len() {
            Some(mem::take(&mut self.captures))
        } else {
            None
        };
        (vars, captures)
    }

    fn resolve_binding(&mut self, binding: &mut Binding<'p>, declaring: bool, loc: Span) -> Res<'p, ()> {
        match binding.name {
            Name::Ident(name) => {
                self.walk_ty(&mut binding.ty);
                if declaring {
                    let var = self.decl_var(&name, binding.kind, loc)?;
                    *binding = Binding {
                        name: Name::Var(var),
                        ty: mem::replace(&mut binding.ty, LazyType::Infer),
                        default: mem::take(&mut binding.default),
                        kind: binding.kind,
                    };
                }
            }
            Name::Var(_) => unreachable!(),
            Name::None => self.walk_ty(&mut binding.ty),
        }
        if let Some(expr) = &mut binding.default {
            self.resolve_expr(expr)?;
        }
        Ok(())
    }
    fn walk_ty(&mut self, ty: &mut LazyType<'p>) {
        match ty {
            LazyType::EvilUnit => panic!(),
            LazyType::Infer => {}
            LazyType::PendingEval(e) => self.resolve_expr(e).unwrap(), // TODO: have WalkAst return errs.
            LazyType::Finished(_) => {}
            LazyType::Different(parts) => parts.iter_mut().for_each(|t| self.walk_ty(t)),
        }
    }
}
