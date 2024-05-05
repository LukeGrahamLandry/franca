use std::{mem, ops::DerefMut};

use codemap::Span;

use crate::{
    assert,
    ast::{Binding, Expr, FatExpr, FatStmt, Flag, Func, LazyType, Name, ScopeId, Stmt, Var, VarType},
    compiler::{add_unique, BlockScope, CErr, Compile, Res},
    err, ice,
    logging::{LogTag::Scope, PoolLog},
    outln,
    pool::Ident,
    STATS,
};

pub struct ResolveScope<'z, 'a, 'p> {
    captures: Vec<Var<'p>>,
    local_constants: Vec<Vec<(Var<'p>, LazyType<'p>, FatExpr<'p>)>>,
    compiler: &'z mut Compile<'a, 'p>,
    last_loc: Span,
    scope: ScopeId,
    block: usize,
}

impl<'z, 'a, 'p> ResolveScope<'z, 'a, 'p> {
    fn new(compiler: &'z mut Compile<'a, 'p>, scope: ScopeId, last_loc: Span) -> Self {
        ResolveScope {
            captures: Default::default(),
            local_constants: Default::default(),
            compiler,
            last_loc,
            scope,
            block: 0, // TODO: new block for each specialization so they can't read eachother's arguments
        }
    }

    pub fn run(func: &mut Func<'p>, compiler: &'z mut Compile<'a, 'p>, scope: ScopeId) -> Res<'p, Vec<Var<'p>>> {
        let mut r = ResolveScope::new(compiler, scope, func.loc);
        if let Err(mut e) = r.resolve_func(func) {
            e.loc = e.loc.or(Some(r.last_loc));
            return Err(e);
        }
        debug_assert!(r.local_constants.is_empty());
        debug_assert_eq!(r.scope, scope, "ICE: unmatched scopes");
        r.compiler.pool.use_constants(|c| c.adjust_writable());
        Ok(r.captures)
    }

    fn resolve_func(&mut self, func: &mut Func<'p>) -> Res<'p, ()> {
        if func.resolved_body && func.resolved_sign {
            return Ok(());
        }

        let (s, b) = (self.scope, self.block);

        self.push_scope(Some(func.name));
        func.scope = Some(self.scope);

        if func.allow_rt_capture {
            self.resolve_func_args(func)?;
            self.resolve_func_body(func)?;
            debug_assert_eq!(s, self.scope);
            debug_assert_eq!(b, self.block);
        } else {
            // ResolveScope::resolve_all(func, self.compiler)?;
            self.pop_scope();
            debug_assert_ne!(self.scope, func.scope.unwrap());
        }

        Ok(())
    }

    #[track_caller]
    pub fn resolve_sign(func: &mut Func<'p>, compiler: &'z mut Compile<'a, 'p>) -> Res<'p, ()> {
        let scope = func.scope.unwrap();
        let mut r = ResolveScope::new(compiler, scope, func.loc);
        r.resolve_func_args(func)?;
        debug_assert_eq!(r.scope, scope);
        Ok(())
    }

    pub fn resolve_body(func: &mut Func<'p>, compiler: &'z mut Compile<'a, 'p>) -> Res<'p, ()> {
        let scope = func.scope.unwrap();
        let mut r = ResolveScope::new(compiler, scope, func.loc);
        r.resolve_func_body(func)?;
        Ok(())
    }

    fn resolve_func_args(&mut self, func: &mut Func<'p>) -> Res<'p, ()> {
        if func.resolved_sign {
            return Ok(());
        }
        self.scope = func.scope.unwrap();
        self.block = 0;
        debug_assert_eq!(self.scope, func.scope.unwrap());
        let generic = func.has_tag(Flag::Generic); // ret is allowed to depend on previous args.

        self.local_constants.push(Default::default());
        func.resolved_sign = true;
        for b in &mut func.arg.bindings {
            self.resolve_binding(b)?;
        }
        if !generic {
            self.walk_ty(&mut func.ret);
        }
        self.pop_block();
        let arg_block_const_decls = self.local_constants.pop().unwrap();
        assert!(arg_block_const_decls.is_empty()); // TODO: allow block exprs with consts in arg types but for now i dont use it and this is easier to think about. -- Apr 24
        debug_assert_eq!(self.scope, func.scope.unwrap());
        debug_assert_eq!(self.block, 0);
        Ok(())
    }

    fn resolve_func_body(&mut self, func: &mut Func<'p>) -> Res<'p, ()> {
        if func.resolved_body {
            return Ok(());
        }
        unsafe { STATS.fn_body_resolve += 1 };
        self.scope = func.scope.unwrap();
        let generic = func.has_tag(Flag::Generic); // args and ret are allowed to depend on previous args.

        self.push_scope(None);
        debug_assert!(func.args_block.is_none());
        func.args_block = Some(self.block);

        self.block = func.args_block.unwrap();
        self.local_constants.push(Default::default());

        for b in &mut func.arg.bindings {
            self.declare_binding(b, func.loc)?;
        }
        if generic {
            self.walk_ty(&mut func.ret);
        }
        self.push_scope(None);
        func.resolved_body = true;
        if let Some(body) = &mut func.body {
            self.resolve_expr(body)?;
        }
        self.pop_block();
        debug_assert_eq!(self.scope, func.scope.unwrap());
        debug_assert_eq!(self.block, func.args_block.unwrap());
        self.pop_block();
        self.pop_scope();
        let capures = mem::take(&mut self.captures);
        // Now check which things we captured from *our* parent.
        for c in capures {
            self.find_var(&c.0); // This adds it back to self.captures if needed
            if c.3 != VarType::Const {
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
        } else {
            assert!(!func.any_const_args() && !func.has_tag(Flag::Comptime), "TODO: closures with const args");
        }

        debug_assert!(func.local_constants.is_empty());

        for (name, ty, mut val) in self.local_constants.pop().unwrap() {
            let consts = &mut self.compiler[name.2].constants;
            add_unique(&mut func.local_constants, name);
            if let Some((prev_val, prev_ty)) = consts.get_mut(&name) {
                if let Expr::AddToOverloadSet(new) = &mut val.expr {
                    if let Expr::AddToOverloadSet(prev) = &mut prev_val.expr {
                        prev.extend(mem::take(new));
                        continue;
                    } else if let Expr::Value { value } = &mut prev_val.expr {
                        // let v = &prev_val.clone();
                        // let os: Option<OverloadSetId> = self.compiler.as_value_expr(v);
                        // let os = unwrap!(os, "");
                        debug_assert!(!prev_val.ty.is_unknown());
                        let os = value.as_overload_set()?;
                        self.compiler.program[os].just_resolved.extend(mem::take(new));
                        continue;
                    }
                }

                debug_assert!(matches!(prev_ty, LazyType::Infer));
                *prev_ty = ty;
                *prev_val = val;
            } else {
                ice!("missing constant pre-decl {}", name.log(self.compiler.program.pool));
            }
        }
        outln!(Scope, "{}", func.log_captures(self.compiler.pool));
        Ok(())
    }

    fn scan_const_decls(&mut self, stmt: &mut FatStmt<'p>) -> Res<'p, ()> {
        let loc = stmt.loc;
        match stmt.deref_mut() {
            Stmt::DeclNamed { name, ty, value, kind } => {
                if *kind == VarType::Const {
                    let new = self.decl_var(name, *kind, loc)?;
                    let decl = Stmt::DeclVar {
                        name: new,
                        ty: mem::replace(ty, LazyType::Infer),
                        value: mem::replace(value, FatExpr::null(loc)),
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
            Stmt::ExpandParsedStmts(_) => todo!(),
            Stmt::DoneDeclFunc(_, _) => unreachable!("compiled twice?"),
            Stmt::DeclNamed { name, ty, value, kind } => {
                debug_assert!(*kind != VarType::Const);
                self.walk_ty(ty);
                self.resolve_expr(value)?;

                let new = self.decl_var(name, *kind, loc)?;
                let decl = Stmt::DeclVar {
                    name: new,
                    ty: mem::replace(ty, LazyType::Infer),
                    value: mem::replace(value, FatExpr::null(loc)),
                    kind: *kind,
                };
                stmt.stmt = decl;
            }
            Stmt::DeclFunc(func) => {
                func.annotations = aaa;

                // Note: not Self::_ because of lifetimes. 'z needs to be different.
                // let cap = ResolveScope::run(func, self.compiler, self.scope)?;
                // debug_assert!(cap.is_empty());
                self.resolve_func(func)?;

                let name = func.var_name.unwrap();
                // TODO: now this is a bit weird. later has to distinguish between const _ = expr and fn _ = stmt by whether it can capture.
                let expr = Expr::AddToOverloadSet(vec![mem::take(func)]);
                self.local_constants
                    .last_mut()
                    .unwrap()
                    .push((name, LazyType::Infer, FatExpr::synthetic(expr, loc)));
                stmt.stmt = Stmt::Noop;
            }
            Stmt::DeclVar { kind, ty, value, name, .. } => {
                debug_assert!(*kind == VarType::Const);
                self.walk_ty(ty);
                self.resolve_expr(value)?;

                let ty = mem::take(ty);
                let value = mem::take(value);
                // TODO: take annotations too somehow.
                let consts = self.local_constants.last_mut().unwrap();
                consts.push((*name, ty, value));
                stmt.stmt = Stmt::Noop;
            }
            Stmt::Set { place, value } => {
                self.resolve_expr(place)?;
                self.resolve_expr(value)?;
            }
            Stmt::Noop => {}
            Stmt::Eval(e) => self.resolve_expr(e)?,

            Stmt::DeclVarPattern { binding, value } => {
                self.resolve_expr(value)?;
                for b in &mut binding.bindings {
                    self.resolve_binding(b)?;
                    self.declare_binding(b, loc)?;
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
            Expr::GetParsed(index) => {
                *expr = match self.compiler.parsing.wait_for_expr(*index) {
                    Ok(expr) => expr,
                    Err(e) => err!(CErr::Diagnostic(e.diagnostic)),
                };
                debug_assert!(!matches!(expr.expr, Expr::GetParsed(_)));
                self.resolve_expr(expr)?;
            }
            Expr::AddToOverloadSet(_) | Expr::WipFunc(_) => unreachable!(),
            Expr::Poison => ice!("POISON",),
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

                let mut new_body = vec![];
                let mut dirty = true;
                while dirty {
                    dirty = false;
                    for stmt in mem::take(body) {
                        if let Stmt::ExpandParsedStmts(name) = stmt.stmt {
                            dirty = true;
                            match self.compiler.parsing.wait_for_stmts(name) {
                                Ok(stmts) => {
                                    for s in stmts {
                                        new_body.push(s);
                                    }
                                }
                                Err(e) => err!(CErr::Diagnostic(e.diagnostic)),
                            }
                        } else {
                            new_body.push(stmt);
                        }
                    }
                    *body = mem::take(&mut new_body);
                }

                for stmt in body.iter_mut() {
                    self.scan_const_decls(stmt)?;
                }
                for stmt in body.iter_mut() {
                    self.resolve_stmt(stmt)?;
                }
                self.resolve_expr(result)?;
                *resolved = Some((self.scope, self.block));
                self.pop_block();
            }
            Expr::Tuple(values) => {
                for value in values {
                    self.resolve_expr(value)?;
                }
            }
            Expr::SuffixMacro(_, e) => self.resolve_expr(e)?,
            Expr::Closure(func) => self.resolve_func(func)?,
            Expr::Value { .. } => {}
            Expr::GetNamed(name) => {
                if let Some(var) = self.find_var(name) {
                    *expr.deref_mut() = Expr::GetVar(var);
                } else {
                    // println!(
                    //     "{} undeclared in s:{} b:{}. nextvar: {}",
                    //     self.compiler.pool.get(*name),
                    //     self.scope.as_index(),
                    //     self.block,
                    //     self.compiler.program.next_var
                    // );
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
                    self.resolve_binding(b)?;
                    self.declare_binding(b, loc)?;
                }
                self.pop_block();
            }
            Expr::String(_) => {}
        }
        Ok(())
    }

    fn find_var(&mut self, name: &Ident<'p>) -> Option<Var<'p>> {
        let find = |comp: &Compile<'_, 'p>, s: ScopeId, mut block: usize| {
            let scope = &comp[s];
            if scope.vars.is_empty() {
                debug_assert_eq!(block, 0);
                return None;
            }
            let mut vars = &scope.vars[block];
            loop {
                // 13M
                if let Some(found) = vars.vars.iter().rev().position(|v| v.0 == *name) {
                    // Reverse so you get the shadowing first.
                    let v = vars.vars[vars.vars.len() - found - 1];
                    if v.3 == VarType::Const {
                        debug_assert!(scope.constants.contains_key(&v));
                    }
                    return Some(v);
                }

                if vars.parent == block {
                    debug_assert_eq!(block, 0);
                    return None;
                }
                debug_assert_ne!(block, 0);
                block = vars.parent;
                vars = &scope.vars[block];
            }
        };

        let mut s = self.scope;
        let mut block = self.block;
        // Check the current functions scopes.
        if let Some(v) = find(self.compiler, s, block) {
            return Some(v);
        }
        block = self.compiler[s].block_in_parent;
        s = self.compiler[s].parent;

        loop {
            // println!("- look {} in s{} b{}", self.compiler.pool.get(*name), s.as_index(), block);
            let scope = &self.compiler[s];
            let found = find(self.compiler, s, block);
            if let Some(v) = found {
                // TODO: the depth thing is a bit confusing. it was a bit less jaring before when it was just local on the resolver.
                //       brifly needed -1 because scope 0 is now a marker and always empty i guess, but now thats done in push_scope instead.
                //       its about which scopes count as function captures vs just normal blocks. should try to clean that up. -- Apr 23
                if !self.captures.contains(&v) && v.3 != VarType::Const {
                    // We got it from our parent function.
                    self.captures.push(v);
                }
                return Some(v);
            }
            if scope.parent == s {
                return None;
            }
            s = scope.parent;
            block = scope.block_in_parent;
        }
    }

    fn decl_var(&mut self, name: &Ident<'p>, kind: VarType, loc: Span) -> Res<'p, Var<'p>> {
        self.last_loc = loc;
        let s = self.scope;
        // Note: you can't shadow a let with a const either but that already works because consts are done first.
        // TODO: when this was a hashmap ident->(_,_) of justs constants this was faster, but its a tiny difference in release mode so its probably fine for now.
        //       this makes it easier to think about having functions be the unit of resolving instead of blocks but still allowing shadowing consts in inner blocks.
        let wip = &self.compiler[s].vars[self.block].vars;
        // 1.6M
        let shadow_const = |v: &Var<'_>| v.0 == *name && v.3 == VarType::Const;
        if wip.iter().any(shadow_const) {
            err!(
                "Cannot shadow constant in the same scope: {}",
                wip.iter().find(|v| shadow_const(v)).unwrap().log(self.compiler.pool)
            )
        }

        let var = Var(*name, self.compiler.program.next_var, s, kind, self.block as u32);
        if kind == VarType::Const {
            let empty = (FatExpr::synthetic(Expr::Poison, loc), LazyType::Infer);
            self.compiler[s].constants.insert(var, empty); // sad. two lookups per constant. but doing it different on each branch looks verbose.
        }
        // println!("= decl {} in s{} b{}", self.compiler.pool.get(*name), s.as_index(), self.block);
        self.compiler[s].vars[self.block].vars.push(var); // includes constants!
        self.compiler.program.next_var += 1;
        // println!("{} declared in {}", var.log(self.compiler.pool), var.2.as_index());
        Ok(var)
    }

    fn push_scope(&mut self, name: Option<Ident<'p>>) {
        if let Some(name) = name {
            self.scope = self.compiler.new_scope(self.scope, name, self.block);
            debug_assert!(self.compiler[self.scope].vars.is_empty());
            self.compiler[self.scope].vars.push(BlockScope { vars: vec![], parent: 0 });
        } else {
            debug_assert!(!self.compiler[self.scope].vars.is_empty());
            self.compiler[self.scope].vars.push(BlockScope {
                vars: vec![],
                parent: self.block,
            });
        }
        self.block = self.compiler[self.scope].vars.len() - 1;
    }

    fn pop_scope(&mut self) {
        let s = self.scope;
        self.scope = self.compiler[s].parent;
        self.block = self.compiler[s].block_in_parent;
    }

    fn pop_block(&mut self) {
        let s = self.scope;
        let locals = &self.compiler[s].vars[self.block];
        self.block = locals.parent;
    }

    fn resolve_binding(&mut self, binding: &mut Binding<'p>) -> Res<'p, ()> {
        self.walk_ty(&mut binding.ty);
        if let Some(expr) = &mut binding.default {
            self.resolve_expr(expr)?;
        }
        Ok(())
    }

    fn declare_binding(&mut self, binding: &mut Binding<'p>, loc: Span) -> Res<'p, ()> {
        match binding.name {
            Name::Ident(name) => {
                let var = self.decl_var(&name, binding.kind, loc)?;
                *binding = Binding {
                    name: Name::Var(var),
                    ty: mem::replace(&mut binding.ty, LazyType::Infer),
                    default: mem::take(&mut binding.default),
                    kind: binding.kind,
                };
            }
            Name::Var(_) => unreachable!(),
            Name::None => {}
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
