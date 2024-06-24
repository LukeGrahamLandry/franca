use std::{mem, ops::DerefMut};

use crate::self_hosted::{get_include_std, Span};

use crate::{
    assert,
    ast::{Binding, Expr, FatExpr, FatStmt, Flag, FnFlag, Func, FuncImpl, LazyType, Name, ScopeId, Stmt, Var, VarType},
    compiler::{BlockScope, Compile, Res},
    err,
    export_ffi::BigOption,
    ice,
    logging::PoolLog,
    self_hosted::Ident,
    STATS,
};

use crate::export_ffi::BigResult::*;
pub struct ResolveScope<'z, 'a, 'p> {
    captures: Vec<Var<'p>>,
    compiler: &'z mut Compile<'a, 'p>,
    last_loc: Span,
    scope: ScopeId,
    block: usize,
}

impl<'z, 'a, 'p> ResolveScope<'z, 'a, 'p> {
    fn new(compiler: &'z mut Compile<'a, 'p>, scope: ScopeId, last_loc: Span) -> Self {
        ResolveScope {
            captures: Default::default(),
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
        debug_assert_eq!(r.scope, scope, "ICE: unmatched scopes");
        Ok(r.captures)
    }

    fn resolve_func(&mut self, func: &mut Func<'p>) -> Res<'p, ()> {
        if func.get_flag(FnFlag::ResolvedBody) && func.get_flag(FnFlag::ResolvedSign) {
            return Ok(());
        }

        // TODO: need to remember to update from this if we get one from the user (like they added it in a macro)?
        if func.has_tag(Flag::Generic) {
            func.set_flag(FnFlag::Generic, true);
            func.annotations.retain(|a| a.name != Flag::Generic.ident());
        }
        if func.has_tag(Flag::Unsafe_Noop_Cast) {
            func.set_flag(FnFlag::UnsafeNoopCast, true);
            func.annotations.retain(|a| a.name != Flag::Unsafe_Noop_Cast.ident());
        }

        let (s, b) = (self.scope, self.block);

        self.push_scope(Some(func.name));
        func.scope = BigOption::Some(self.scope);

        if func.get_flag(FnFlag::AllowRtCapture) {
            self.resolve_func_args(func)?;
            self.resolve_func_body(func)?;
            debug_assert_eq!(s, self.scope);
            debug_assert_eq!(b, self.block);
        } else {
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
        if func.get_flag(FnFlag::ResolvedSign) {
            return Ok(());
        }
        self.scope = func.scope.unwrap();
        self.block = 0;
        debug_assert_eq!(self.scope, func.scope.unwrap());
        let generic = func.get_flag(FnFlag::Generic); // ret is allowed to depend on previous args.

        func.set_flag(FnFlag::ResolvedSign, true);

        if generic {
            for b in &mut func.arg.bindings {
                self.declare_binding(b, func.loc)?;
            }
            for b in &mut func.arg.bindings {
                self.resolve_binding(b)?;
            }
            self.walk_ty(&mut func.ret);
        } else {
            for b in &mut func.arg.bindings {
                self.resolve_binding(b)?;
            }
            self.walk_ty(&mut func.ret);
        }
        self.pop_block();
        // TODO: allow block exprs with consts in arg types but for now i dont use it and this is easier to think about. -- Apr 24
        debug_assert_eq!(self.scope, func.scope.unwrap());
        debug_assert_eq!(self.block, 0);
        Ok(())
    }

    fn resolve_func_body(&mut self, func: &mut Func<'p>) -> Res<'p, ()> {
        if func.get_flag(FnFlag::ResolvedBody) {
            return Ok(());
        }
        unsafe { STATS.fn_body_resolve += 1 };
        self.scope = func.scope.unwrap();
        let generic = func.get_flag(FnFlag::Generic); // args and ret are allowed to depend on previous args.

        self.push_scope(None);

        if generic {
        } else {
            for b in &mut func.arg.bindings {
                self.declare_binding(b, func.loc)?;
            }
        }
        self.push_scope(None);
        func.set_flag(FnFlag::ResolvedBody, true);
        if matches!(func.body, FuncImpl::Normal(_)) {
            if func.get_flag(FnFlag::AllowRtCapture) {
                func.return_var = BigOption::Some(self.decl_var(&Flag::Local_Return.ident(), VarType::Const, func.loc)?);
            } else {
                func.return_var = BigOption::Some(self.decl_var(&Flag::Return.ident(), VarType::Const, func.loc)?);
            }
            // func.return_var = BigOption::Some(self.decl_var(&Flag::__Return.ident(), VarType::Const, func.loc)?);
        }
        if let FuncImpl::Normal(body) = &mut func.body {
            self.resolve_expr(body)?;
        }
        self.pop_block();
        debug_assert_eq!(self.scope, func.scope.unwrap());
        self.pop_block();
        self.pop_scope();
        let capures = mem::take(&mut self.captures);
        // Now check which things we captured from *our* parent.
        for c in capures {
            self.find_var(&c.name); // This adds it back to self.captures if needed
            debug_assert!(c.kind != VarType::Const);
            func.capture_vars.push(c);
        }

        if !func.get_flag(FnFlag::AllowRtCapture) {
            // TODO: show the captured var use site and declaration site.
            self.last_loc = func.loc;
            let n = self.compiler.program.pool.get(func.name);
            assert!(
                func.capture_vars.is_empty(),
                "Must use '=>' for closure '{}' with captures: {:?}",
                n,
                func.capture_vars.iter().map(|v| v.log(self.compiler.program.pool)).collect::<Vec<_>>()
            );
        }

        Ok(())
    }

    fn scan_const_decls(&mut self, stmt: &mut FatStmt<'p>) -> Res<'p, ()> {
        let loc = stmt.loc;
        match stmt.deref_mut() {
            Stmt::DeclNamed { name, ty, value, kind } => {
                // kinda hack. just makes it easier to read logs when you do 'name :: fn() ...'
                if let Expr::Closure(func) = &mut value.expr {
                    if func.name == Flag::Anon.ident() {
                        func.name = *name;
                    }
                }

                if *kind == VarType::Const {
                    let new = self.decl_var(name, *kind, loc)?;
                    let decl = Stmt::DeclVar {
                        name: new,
                        ty: mem::replace(ty, LazyType::Infer),
                        value: mem::replace(value, FatExpr::null(loc)),
                    };
                    stmt.stmt = decl;
                    // Don't move to local_constants yet because want value to be able to reference later constants
                }
            }
            Stmt::DeclFunc(func) => {
                // Functions don't shadow, they just add to an overload group.
                // TOOD: @pub vs @private
                if let Some(v) = self.find_var(&func.name) {
                    assert!(v.kind == VarType::Const);
                    func.var_name = BigOption::Some(v);
                } else {
                    let v = self.decl_var(&func.name, VarType::Const, func.loc)?;
                    func.var_name = BigOption::Some(v);
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
            if let BigOption::Some(args) = &mut a.args {
                self.resolve_expr(args)?
            }
        }

        let aaa = stmt.annotations.clone();
        match stmt.deref_mut() {
            Stmt::ExpandParsedStmts(_) => todo!(),
            Stmt::DeclNamed { name, ty, value, kind } => {
                debug_assert!(*kind != VarType::Const);
                self.walk_ty(ty);
                self.resolve_expr(value)?;

                let new = self.decl_var(name, *kind, loc)?;
                let decl = Stmt::DeclVar {
                    name: new,
                    ty: mem::replace(ty, LazyType::Infer),
                    value: mem::replace(value, FatExpr::null(loc)),
                };
                stmt.stmt = decl;
            }
            Stmt::DeclFunc(func) => {
                func.annotations.extend(aaa);
                self.resolve_func(func)?;
            }
            Stmt::DeclVar { name, ty, value } => {
                debug_assert!(name.kind == VarType::Const);
                self.walk_ty(ty);
                self.resolve_expr(value)?;
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
                *expr = self.compiler.program.pool.wait_for_expr(*index)?;
                debug_assert!(!matches!(expr.expr, Expr::GetParsed(_)));
                self.resolve_expr(expr)?;
            }
            Expr::AddToOverloadSet(_) | Expr::WipFunc(_) => unreachable!(),
            Expr::Poison => ice!("POISON",),
            Expr::Call(fst, snd) => {
                self.resolve_expr(fst)?;
                self.resolve_expr(snd)?;
            }
            Expr::PtrOffset { ptr, .. } => {
                self.resolve_expr(ptr)?;
            }
            Expr::PrefixMacro { handler, arg, target } => {
                self.resolve_expr(handler)?;
                self.resolve_expr(arg)?;
                self.resolve_expr(target)?;
            }
            Expr::Block { body, result, .. } => {
                self.push_scope(None);

                for stmt in body.iter_mut() {
                    if stmt.annotations.len() == 1 {
                        // println!("{:?} {:?}", stmt.annotations[0].name, Flag::Include_Std.ident());
                        if stmt.annotations[0].name == Flag::Include_Std.ident() {
                            self.slow_body(body)?;
                            break;
                        }
                    }
                    // if matches!(stmt.stmt, Stmt::ExpandParsedStmts(_)) {
                    //     self.slow_body(body)?;
                    //     break;
                    // }
                    self.scan_const_decls(stmt)?;
                }
                for stmt in body.iter_mut() {
                    self.resolve_stmt(stmt)?;
                }
                self.resolve_expr(result)?;
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
                    //     self.compiler.program.pool.get(*name),
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
            Expr::Cast(v) => {
                self.resolve_expr(v)?;
                todo!("probably never get here cause its added by compiler");
            }
        }
        Ok(())
    }

    // most blocks aren't using #include_std.
    // doing it this way means you scan_const_decls twice anything in a block before a #include_std, but that doesn't matter.
    fn slow_body(&mut self, body: &mut Vec<FatStmt<'p>>) -> Res<'p, ()> {
        let mut new_body = Vec::with_capacity(body.len());
        let mut dirty = true;
        while dirty {
            dirty = false;
            for stmt in mem::take(body) {
                if stmt.annotations.len() == 1 && stmt.annotations[0].name == Flag::Include_Std.ident() {
                    let Expr::String(name) = stmt.annotations[0].args.as_ref().unwrap().expr else {
                        err!("expected string for #include_std",)
                    };
                    if self.compiler.already_loaded.insert(name) {
                        // TODO: let the other side get if it needs to.
                        let name = self.compiler.program.pool.get(name);

                        let file = unsafe { get_include_std(self.compiler as *mut Compile, name) };
                        let BigOption::Some(file) = file else {
                            err!("unknown path for #include_std {name}",);
                        };
                        dirty = true;
                        for s in self.compiler.program.pool.wait_for_stmts(file)? {
                            new_body.push(s);
                        }
                    }
                    continue;
                }

                new_body.push(stmt);
            }
            *body = mem::take(&mut new_body);
        }
        for stmt in body.iter_mut() {
            self.scan_const_decls(stmt)?;
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
                if let Some(found) = vars.vars.iter().rev().position(|v| v.name == *name) {
                    // Reverse so you get the shadowing first.
                    let v = vars.vars[vars.vars.len() - found - 1];
                    if v.kind == VarType::Const {
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
            // println!("- look {} in s{} b{}", self.compiler.program.pool.get(*name), s.as_index(), block);
            let scope = &self.compiler[s];
            let found = find(self.compiler, s, block);
            if let Some(v) = found {
                // TODO: the depth thing is a bit confusing. it was a bit less jaring before when it was just local on the resolver.
                //       brifly needed -1 because scope 0 is now a marker and always empty i guess, but now thats done in push_scope instead.
                //       its about which scopes count as function captures vs just normal blocks. should try to clean that up. -- Apr 23
                if !self.captures.contains(&v) && v.kind != VarType::Const {
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
        let shadow_const = |v: &Var<'_>| v.name == *name && v.kind == VarType::Const;
        if wip.iter().any(shadow_const) {
            err!(
                "Cannot shadow constant in the same scope: {}",
                wip.iter().find(|v| shadow_const(v)).unwrap().log(self.compiler.program.pool)
            )
        }

        let var = Var {
            name: *name,
            id: self.compiler.program.next_var,
            scope: s,
            block: self.block as u16,
            kind,
        };
        if kind == VarType::Const {
            let empty = (FatExpr::synthetic(Expr::Poison, loc), LazyType::Infer);
            self.compiler[s].constants.insert(var, empty); // sad. two lookups per constant. but doing it different on each branch looks verbose.
        }
        // println!("{}", var.log(self.compiler.program.pool));
        // println!("= decl {} in s{} b{}", self.compiler.program.pool.get(*name), s.as_index(), self.block);
        self.compiler[s].vars[self.block].vars.push(var); // includes constants!
        self.compiler.program.next_var += 1;
        // println!("{} declared in {}", var.log(self.compiler.program.pool), var.2.as_index());
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
        if let BigOption::Some(expr) = &mut binding.default {
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
