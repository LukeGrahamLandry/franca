use std::{mem, ops::DerefMut};

use codemap::Span;

use crate::{
    assert,
    ast::{Binding, Expr, FatExpr, FatStmt, Func, LazyType, Name, ScopeId, Stmt, Var, VarType},
    compiler::{Compile, Res},
    err,
    logging::{LogTag::Scope, PoolLog},
    outln,
    pool::Ident,
};

pub struct ResolveScope<'z, 'a, 'p> {
    captures: Vec<Var<'p>>,
    local_constants: Vec<Vec<FatStmt<'p>>>,
    compiler: &'z mut Compile<'a, 'p>,
    last_loc: Span,
    scope: ScopeId,
}

impl<'z, 'a, 'p> ResolveScope<'z, 'a, 'p> {
    fn new(compiler: &'z mut Compile<'a, 'p>, scope: ScopeId, last_loc: Span) -> Self {
        ResolveScope {
            captures: Default::default(),
            local_constants: Default::default(),
            compiler,
            last_loc,
            scope,
        }
    }

    // TODO: will need to keep some state about this for macros that want to add vars?
    // TODO: instead of doing this up front, should do this tree walk at the same time as the interp is doing it?
    pub fn toplevel(stmts: &mut Func<'p>, compiler: &'z mut Compile<'a, 'p>, scope: ScopeId) -> Res<'p, ()> {
        let mut resolver = Self::new(compiler, scope, stmts.loc);
        if let Err(mut e) = resolver.run(stmts) {
            e.loc = e.loc.or(Some(resolver.last_loc));
            return Err(e);
        }
        debug_assert_eq!(resolver.scope, scope, "ICE: unmatched scopes");
        Ok(())
    }

    fn run(&mut self, stmts: &mut Func<'p>) -> Res<'p, ()> {
        self.push_scope(Some(stmts.name));
        self.resolve_func(stmts)?;
        let (globals, outer_captures) = self.pop_scope();
        let outer_captures = outer_captures.expect("well formed blocks (ICE)");
        debug_assert!(globals.is_empty() && outer_captures.is_empty(), "unreachable?");
        Ok(())
    }

    pub fn inner(func: &mut Func<'p>, compiler: &'z mut Compile<'a, 'p>, scope: ScopeId) -> Res<'p, Vec<Var<'p>>> {
        let mut r = ResolveScope::new(compiler, scope, func.loc);
        r.resolve_func(func)?;
        debug_assert!(r.local_constants.is_empty());
        Ok(r.captures)
    }

    fn resolve_func(&mut self, func: &mut Func<'p>) -> Res<'p, ()> {
        self.local_constants.push(Default::default());
        func.parent_scope = Some(self.scope);
        self.push_scope(Some(func.name));
        func.args_scope = Some(self.scope);
        for b in &mut func.arg.bindings {
            self.resolve_binding(b, true, func.loc)?;
        }
        self.walk_ty(&mut func.ret);
        self.push_scope(None);
        func.body_scope = Some(self.scope);
        func.resolved_body = true;
        if let Some(body) = &mut func.body {
            self.resolve_expr(body)?;
        }
        let (outer_locals, cap) = self.pop_scope();
        debug_assert!(cap.is_none());
        debug_assert!(outer_locals.is_empty(), "function needs block");

        let (_args, captures) = self.pop_scope();
        let capures = captures.unwrap();
        // Now check which things we captured from *our* parent.
        for c in capures {
            self.find_var(&c.0); // This adds it back to self.captures if needed
            if c.3 == VarType::Const {
                func.capture_vars_const.push(c);
            } else {
                func.capture_vars.push(c);
            }
        }

        if !func.allow_rt_capture {
            // TODO: show the captured var use site and declaration site.
            self.last_loc = func.loc;
            let n = self.compiler.pool.get(func.name);
            assert!(
                func.capture_vars.is_empty(),
                "Closure '{}' cannot be public. captures: {:?}",
                n,
                func.capture_vars.iter().map(|v| v.log(self.compiler.pool)).collect::<Vec<_>>()
            );
        }

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
                // TOOD: @pub vs @private
                if let Some(v) = self.find_var(&func.name) {
                    assert!(v.3 == VarType::Const);
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
                debug_assert!(*kind != VarType::Const);
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

                // Note: not Self::_ because of lifetimes. 'z needs to be different.
                self.captures.extend(ResolveScope::inner(func, self.compiler, self.scope)?);

                self.local_constants
                    .last_mut()
                    .unwrap()
                    .push(mem::replace(stmt, Stmt::Noop.fat_empty(loc)));
            }
            Stmt::DeclVar { kind, ty, value, .. } => {
                debug_assert!(*kind == VarType::Const);
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
            Expr::Block { body, result, resolved } => {
                self.push_scope(None);
                for stmt in body.iter_mut() {
                    self.resolve_stmt_if_constant(stmt)?;
                }
                for stmt in body.iter_mut() {
                    self.resolve_stmt(stmt)?;
                }
                self.resolve_expr(result)?;
                let (_, cap) = self.pop_scope();
                debug_assert!(cap.is_none());
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
                // else it might be an ffi type or undeclared. We'll find out later.
                // TODO: declare all ffi types with @builtin instead of catching undeclared.
                // TODO: eventually want do breadth first so you always inject comptime added idents first and can say its an error in the else of the check above.
            }
            Expr::GetVar(_) => unreachable!("added by this pass {expr:?}"),
            Expr::FieldAccess(e, _) => self.resolve_expr(e)?,
            Expr::StructLiteralP(p) => {
                self.push_scope(None);
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
        let find = |comp: &Compile<'_, 'p>, s: ScopeId| {
            let scope = &comp[s];
            for block in scope.wip_local_scopes.iter().rev() {
                if let Some(found) = block.iter().rev().position(|v| v.0 == *name) {
                    // Reverse so you get the shadowing first.
                    let v = block[block.len() - found - 1];
                    return Some(v);
                }
            }
            None
        };

        let mut s = self.get_scope();
        // Check the current functions scopes.
        if let Some(v) = find(self.compiler, s) {
            return Some(v);
        }
        s = self.compiler[s].parent;

        loop {
            let scope = &self.compiler[s];
            if scope.parent == s {
                return None;
            }
            let found = find(self.compiler, s);
            if let Some(v) = found {
                // TODO: the depth thing is a bit confusing. it was a bit less jaring before when it was just local on the resolver.
                //       brifly needed -1 because scope 0 is now a marker and always empty i guess, but now thats done in push_scope instead.
                //       its about which scopes count as function captures vs just normal blocks. should try to clean that up. -- Apr 23
                if !self.captures.contains(&v) {
                    // We got it from our parent function.
                    self.captures.push(v);
                }
                return Some(v);
            }
            s = scope.parent;
        }
    }

    fn decl_var(&mut self, name: &Ident<'p>, kind: VarType, loc: Span) -> Res<'p, Var<'p>> {
        let s = self.scope;
        // Note: you can't shadow a let with a const either but that already works because consts are done first.
        // TODO: when this was a hashmap ident->(_,_) of justs constants this was faster, but its a tiny difference in release mode so its probably fine for now.
        //       this makes it easier to think about having functions be the unit of resolving instead of blocks but still allowing shadowing consts in inner blocks.
        let mut wip = self.compiler[s].wip_local_scopes.last().unwrap().iter();
        if wip.any(|v| v.0 == *name && v.3 == VarType::Const) {
            err!("Cannot shadow constant in the same scope: {}", self.compiler.pool.get(*name))
        }

        let var = Var(*name, self.compiler.program.next_var, s, kind);
        if kind == VarType::Const {
            let empty = (FatExpr::synthetic(Expr::Poison, loc), LazyType::Infer);
            self.compiler[s].constants.insert(var, empty); // sad. two lookups per constant. but doing it different on each branch looks verbose.
        }
        self.compiler[s].vars.push(var); // includes constants!
        self.compiler[s].wip_local_scopes.last_mut().unwrap().push(var);
        self.compiler.program.next_var += 1;
        Ok(var)
    }

    fn get_scope(&self) -> ScopeId {
        self.scope
    }

    fn push_scope(&mut self, name: Option<Ident<'p>>) {
        let mut scope = self.get_scope();
        if let Some(name) = name {
            scope = self.compiler.new_scope(scope, name);
            self.scope = scope;
        }
        self.compiler[scope].wip_local_scopes.push(vec![]);
    }

    #[must_use]
    fn pop_scope(&mut self) -> (Vec<Var<'p>>, Option<Vec<Var<'p>>>) {
        let s = self.get_scope();
        let vars = self.compiler[s].wip_local_scopes.pop().unwrap();

        let captures = if self.compiler[s].wip_local_scopes.is_empty() {
            self.scope = self.compiler[s].parent;
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
