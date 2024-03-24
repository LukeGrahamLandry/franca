use std::{mem, ops::DerefMut};

use codemap::Span;

use crate::{
    ast::{Annotation, Binding, Expr, FatExpr, FatStmt, Flag, Func, LazyType, ModuleBody, Name, Stmt, Var, VarInfo, VarType},
    compiler::{Compile, Res},
    err,
    logging::LogTag::Scope,
    outln,
    pool::{Ident, StringPool},
    unwrap,
};

pub struct ResolveScope<'z, 'a, 'p> {
    next_var: usize,
    scopes: Vec<Vec<Var<'p>>>,
    track_captures_before_scope: Vec<usize>,
    captures: Vec<Var<'p>>,
    local_constants: Vec<Vec<FatStmt<'p>>>,
    pool: &'p StringPool<'p>,
    exports: Vec<Var<'p>>,
    compiler: &'z mut Compile<'a, 'p>,
    last_loc: Span,
}

impl<'z, 'a, 'p> ResolveScope<'z, 'a, 'p> {
    // TODO: we know when we're at the top level where order doesn't matter, so should prescan for decls?
    //       then functions could be delt with here too.
    // TODO: will need to keep some state about this for macros that want to add vars?
    // TODO: instead of doing this up front, should do this tree walk at the same time as the interp is doing it?
    pub fn of(stmts: &mut Func<'p>, compiler: &'z mut Compile<'a, 'p>, directives: Vec<(Ident<'p>, FatExpr<'p>)>) -> Res<'p, ()> {
        let mut resolver = ResolveScope {
            next_var: compiler.program.vars.len(),
            scopes: Default::default(),
            track_captures_before_scope: Default::default(),
            captures: Default::default(),
            local_constants: Default::default(),
            pool: compiler.pool,
            exports: vec![],
            compiler,
            last_loc: stmts.loc,
        };
        if let Err(mut e) = resolver.run(stmts, directives) {
            e.loc = e.loc.or(Some(resolver.last_loc));
            return Err(e);
        }
        assert!(resolver.scopes.is_empty(), "ICE: unmatched scopes");
        Ok(())
    }

    fn run(&mut self, stmts: &mut Func<'p>, directives: Vec<(Ident<'p>, FatExpr<'p>)>) -> Res<'p, ()> {
        self.push_scope(true);
        let current = unwrap!(stmts.module, "unknown module");
        // Handle imports
        for (i, arg) in directives {
            self.last_loc = arg.loc;
            let i = Flag::try_from(i)?;
            match i {
                Flag::Open => {
                    let name = arg.parse_dot_chain()?;
                    let module = self.compiler.resolve_module(current, &name)?;
                    self.compiler.get_module(module)?;
                    // TODO: something less fragile for making them not collide
                    self.next_var = self.compiler.program.vars.len();
                    // TODO: do it lazily
                    for var in self.compiler.program[module].exports.values() {
                        let import = Annotation {
                            name: Flag::Import.ident(),
                            args: Some(FatExpr::synthetic(Expr::int(module.0 as i64), self.last_loc)),
                        };
                        let stmt = FatStmt {
                            stmt: Stmt::DeclVar {
                                name: *var,
                                ty: LazyType::Infer,
                                value: None,
                                kind: VarType::Const,
                                dropping: None,
                            },
                            annotations: vec![import],
                            loc: self.last_loc,
                        };
                        stmts.local_constants.push(stmt);
                        self.scopes.last_mut().unwrap().push(*var);
                    }
                }
                Flag::Module => {
                    let mut body = arg.as_func()?;
                    let module = self.compiler.add_module(body.name, Some(current))?;
                    body.module = Some(module);
                    self.compiler.program[module].toplevel = ModuleBody::Parsed(body);
                }
                _ => err!("Unknown directive",),
            }
        }

        self.push_scope(true);
        self.resolve_func(stmts);
        let (_globals, _captured_imports) = self.pop_scope();
        let (_imports, outer_captures) = self.pop_scope();
        assert!(outer_captures.unwrap().is_empty(), "unreachable?");
        assert!(self.compiler.program[current].exports.is_empty());
        for var in &self.exports {
            self.compiler.program[current].exports.insert(var.0, *var);
        }
        Ok(())
    }

    fn resolve_func(&mut self, func: &mut Func<'p>) {
        self.local_constants.push(Default::default());
        self.push_scope(true);
        for b in &mut func.arg.bindings {
            self.resolve_binding(b, true, func.loc);
        }
        self.resolve_type(&mut func.ret);
        self.push_scope(false);
        if let Some(body) = &mut func.body {
            self.resolve_expr(body)
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
            if self.compiler.program.vars[v.1].kind == VarType::Const {
                func.capture_vars_const.push(v);
            } else {
                func.capture_vars.push(v);
            }
        }

        // Extend because #open pokes some constants in earlier.
        func.local_constants.extend(self.local_constants.pop().unwrap());

        outln!(Scope, "{}", func.log_captures(self.pool));
    }

    fn resolve_stmt(&mut self, stmt: &mut FatStmt<'p>) {
        debug_assert_eq!(self.next_var, self.compiler.program.vars.len());
        let loc = stmt.loc;
        self.last_loc = loc;
        let mut public = false;
        for a in &mut stmt.annotations {
            if let Some(args) = &mut a.args {
                self.resolve_expr(args)
            }
            if a.name == Flag::Pub.ident() {
                public = true;
            }
        }

        let aaa = stmt.annotations.clone();
        match stmt.deref_mut() {
            Stmt::DoneDeclFunc(_) => unreachable!("compiled twice?"),
            Stmt::DeclNamed { name, ty, value, kind } => {
                self.resolve_type(ty);
                if let Some(value) = value {
                    self.resolve_expr(value);
                }
                let (old, new) = self.decl_var(name);
                self.compiler.program.vars.push(VarInfo { kind: *kind, loc });
                let decl = Stmt::DeclVar {
                    name: new,
                    ty: mem::replace(ty, LazyType::Infer),
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
                    if public {
                        self.exports.push(new);
                    }
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
                if func.referencable_name {
                    // Functions don't shadow, they just add to an overload group.
                    // TODO: what happens if you're shadowing a normal variable? just err maybe?
                    // TOOD: @pub vs @private
                    let name = if let Some(v) = self.find_var(&func.name) {
                        func.var_name = Some(v);
                        v
                    } else {
                        let (_, v) = self.decl_var(&func.name);
                        func.var_name = Some(v);
                        self.compiler.program.vars.push(VarInfo {
                            kind: VarType::Const,
                            loc: func.loc,
                        });
                        v
                    };
                    if public {
                        self.exports.push(name);
                    }
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
            Stmt::DeclVarPattern { binding, value } => {
                // TODO: this isnt actually tested because only the backend makes these
                if let Some(value) = value {
                    self.resolve_expr(value);
                }
                for b in &mut binding.bindings {
                    self.resolve_binding(b, true, loc);
                }
            }
        }
    }

    // TODO: this could use walk_whatever()
    fn resolve_expr(&mut self, expr: &mut FatExpr<'p>) {
        let loc = expr.loc;
        self.last_loc = loc;
        match expr.deref_mut() {
            Expr::WipFunc(_) => unreachable!(),
            Expr::Call(fst, snd) | Expr::Index { ptr: fst, index: snd } => {
                self.resolve_expr(fst);
                self.resolve_expr(snd);
            }
            Expr::PrefixMacro { name, arg, target } => {
                if let Some(var) = self.find_var(&name.0) {
                    *name = var;
                }
                self.resolve_expr(arg);
                self.resolve_expr(target);
            }
            Expr::Block { body, result, locals } => {
                assert!(locals.is_none());
                self.push_scope(false);
                for stmt in body {
                    self.resolve_stmt(stmt);
                }
                self.resolve_expr(result);
                let (vars, _) = self.pop_scope();
                *locals = Some(vars);
            }
            Expr::Tuple(values) => {
                for value in values {
                    self.resolve_expr(value);
                }
            }
            Expr::SuffixMacro(_, e) => self.resolve_expr(e),
            Expr::Closure(func) => self.resolve_func(func),
            Expr::Value { .. } => {}
            Expr::GetNamed(name) => {
                if let Some(var) = self.find_var(name) {
                    *expr.deref_mut() = Expr::GetVar(var);
                }
                // else it might be a global, like a function with overloading, or undeclared. We'll find out later.
            }
            Expr::GetVar(_) => unreachable!("added by this pass {expr:?}"),
            Expr::FieldAccess(e, _) => self.resolve_expr(e),
            Expr::StructLiteralP(p) => {
                self.push_scope(false);
                for b in &mut p.bindings {
                    self.resolve_binding(b, true, loc)
                }
                let _ = self.pop_scope();
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

    fn resolve_type(&mut self, ty: &mut LazyType<'p>) {
        match ty {
            LazyType::EvilUnit => panic!(),
            LazyType::Infer => {}
            LazyType::PendingEval(e) => self.resolve_expr(e),
            LazyType::Finished(_) => {}
            LazyType::Different(parts) => parts.iter_mut().for_each(|t| self.resolve_type(t)),
        }
    }

    fn resolve_binding(&mut self, binding: &mut Binding<'p>, declaring: bool, loc: Span) {
        match binding.name {
            Name::Ident(name) => {
                self.resolve_type(&mut binding.ty);
                if declaring {
                    let (_old, var) = self.decl_var(&name);
                    self.compiler.program.vars.push(VarInfo { kind: VarType::Var, loc });
                    *binding = Binding {
                        name: Name::Var(var),
                        ty: mem::replace(&mut binding.ty, LazyType::Infer),
                        default: mem::take(&mut binding.default),
                        kind: binding.kind,
                    };
                }
            }
            Name::Var(_) => unreachable!(),
            Name::None => self.resolve_type(&mut binding.ty),
        }
        if let Some(expr) = &mut binding.default {
            self.resolve_expr(expr);
        }
    }
}