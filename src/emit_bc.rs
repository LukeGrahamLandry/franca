//! Converts simple ASTs into my bytecode-ish format.

#![allow(clippy::wrong_self_convention)]

use codemap::Span;
use std::marker::PhantomData;
use std::ops::Deref;
use std::panic::Location;
use std::usize;

use crate::ast::{Expr, FatExpr, FuncId, Program, Stmt, TypeId, TypeInfo};
use crate::ast::{FatStmt, Flag, Pattern, Var, VarType};
use crate::compiler::{CErr, Compile, FnWip, Res};
use crate::extend_options;
use crate::logging::PoolLog;
use crate::reflect::BitSet;
use crate::{bc::*, Map};

use crate::{assert, assert_eq, err, ice, unwrap};

#[derive(Debug, Clone)]
pub struct DebugInfo<'p> {
    pub internal_loc: Option<&'static Location<'static>>,
    pub src_loc: Span,
    pub p: PhantomData<&'p str>,
}

pub struct EmitBc<'z, 'p: 'z> {
    program: &'z Program<'p>,
    sizes: &'z mut SizeCache,
    last_loc: Option<Span>,
    locals: Vec<Vec<u16>>,
    var_lookup: Map<Var<'p>, u16>,
    stack_height: u16,
}

pub fn emit_bc<'p>(compile: &mut Compile<'_, 'p>, f: FuncId) -> Res<'p, ()> {
    EmitBc::compile(compile.program, &mut compile.ready, f)
}

impl<'z, 'p: 'z> EmitBc<'z, 'p> {
    pub fn compile(program: &'z Program<'p>, interp: &'z mut BcReady<'p>, f: FuncId) -> Res<'p, ()> {
        extend_options(&mut interp.ready, f.as_index());
        if interp[f].is_some() {
            return Ok(());
        }
        let mut emit = EmitBc::new(program, &mut interp.sizes);
        let body = emit.compile_inner(f)?;
        interp[f] = Some(body);
        Ok(())
    }

    fn new(program: &'z Program<'p>, sizes: &'z mut SizeCache) -> Self {
        Self {
            last_loc: None,
            program,
            sizes,
            locals: vec![],
            var_lookup: Default::default(),
            stack_height: 0,
        }
    }

    #[track_caller]
    fn empty_fn(&mut self, func: &FnWip<'p>) -> FnBody<'p> {
        let mut jump_targets = BitSet::empty();
        jump_targets.set(0); // entry is the first instruction
        FnBody {
            jump_targets,
            vars: Default::default(),
            when: func.when,
            func: func.func,
            why: func.why.clone(),
            last_loc: func.last_loc,
            insts: vec![],
            slot_types: vec![],
            if_debug_count: 0,
            _p: PhantomData,
        }
    }

    fn compile_inner(&mut self, f: FuncId) -> Res<'p, FnBody<'p>> {
        self.locals.clear();
        self.locals.push(vec![]);
        let func = &self.program[f];
        let wip = unwrap!(func.wip.as_ref(), "Not done comptime for {f:?}"); // TODO
        debug_assert!(!func.evil_uninit);
        let mut result = self.empty_fn(wip);
        match self.emit_body(&mut result, f) {
            Ok(_) => Ok(result),
            Err(mut e) => {
                e.loc = self.last_loc;
                Err(e)
            }
        }
    }

    fn bind_args(&mut self, result: &mut FnBody<'p>, pattern: &Pattern<'p>) -> Res<'p, ()> {
        let arguments = pattern.flatten();
        // reversed because they're on the stack like [0, 1, 2]
        for (name, ty, kind) in arguments.into_iter().rev() {
            // TODO:? probably fine, i jsut set to const in closure capture but then shouldn't be adding to vars below.
            // TODO: the frontend needs to remove the 'const' parts from the ast in DeclVarPattern
            assert!(kind != VarType::Const, "{:?}", name.map(|v| v.log(self.program.pool)));

            let id = result.add_var(ty);
            result.push(Bc::AddrVar { id });
            let slots = self.slot_count(ty);
            result.push(Bc::Store { slots });
            self.stack_height -= slots;

            self.locals.last_mut().unwrap().push(id);
            if let Some(name) = name {
                let prev = self.var_lookup.insert(name, id);
                assert!(prev.is_none(), "overwrite arg? {}", name.log(self.program.pool));
            }
        }
        Ok(())
    }

    fn emit_body(&mut self, result: &mut FnBody<'p>, f: FuncId) -> Res<'p, ()> {
        let func = &self.program[f];
        let has_body = func.body.is_some();

        if !has_body {
            // These are handled at the callsite, they don't need to emit a bytecode body.
            if func.comptime_addr.is_some() || func.llvm_ir.is_some() || func.jitted_code.is_some() {
                // You should never actually try to run this code, the caller should have just done the call,
                // so there isn't an extra indirection and I don't have to deal with two bodies for comptime vs runtime,
                // just too ways of emitting the call.
                result.push(Bc::NoCompile);
                return Ok(());
            }
            err!("called function without body: {f:?} {}", self.program.pool.get(func.name));
        }

        let slots = self.slot_count(func.finished_arg.unwrap());
        // TODO: HACK. this opt shouldn't be nessisary, i just generate so much garbage :(
        if let Expr::Value { value } = &func.body.as_ref().unwrap().expr {
            result.push(Bc::Pop { slots });
            for value in value.clone().vec().into_iter() {
                result.push(Bc::PushConstant { value });
            }
            result.push(Bc::Ret);
            return Ok(());
        }
        self.stack_height += slots;
        self.bind_args(result, &func.arg)?;
        let body = func.body.as_ref().unwrap();
        self.compile_expr(result, body)?;

        for _id in self.locals.pop().unwrap() {
            // result.push(Bc::LastUse { id }); // TODO: why bother, we're returning anyway -- May 1
        }
        assert!(self.locals.is_empty());

        let ret_slots = self.slot_count(func.finished_ret.unwrap());
        debug_assert_eq!(self.stack_height, ret_slots, "{}", body.log(self.program.pool));

        result.push(Bc::Ret); // TODO: this could just be implicit  -- May 1

        Ok(())
    }

    fn emit_runtime_call(&mut self, result: &mut FnBody<'p>, f: FuncId, arg_expr: &FatExpr<'p>) -> Res<'p, ()> {
        self.compile_expr(result, arg_expr)?;
        let func = &self.program[f];
        assert!(func.capture_vars.is_empty());
        assert!(!func.has_tag(Flag::Inline));
        result.push(Bc::CallDirect { f });
        let f_ty = self.program[f].unwrap_ty();
        self.stack_height -= self.slot_count(f_ty.arg);
        self.stack_height += self.slot_count(f_ty.ret);
        Ok(())
    }

    fn compile_stmt(&mut self, result: &mut FnBody<'p>, stmt: &FatStmt<'p>) -> Res<'p, ()> {
        self.last_loc = Some(stmt.loc);
        match stmt.deref() {
            Stmt::Eval(expr) => {
                debug_assert!(!expr.ty.is_unknown());
                self.compile_expr(result, expr)?;
                let slots = self.slot_count(expr.ty);
                result.push(Bc::Pop { slots });
                self.stack_height -= slots;
            }
            Stmt::DeclVar { name, ty, value, kind } => {
                assert_ne!(VarType::Const, *kind);
                let ty = ty.unwrap();

                self.compile_expr(result, value)?; // TODO: ()!uninit
                let id = result.add_var(ty);
                result.push(Bc::AddrVar { id });
                let slots = self.slot_count(ty);
                result.push(Bc::Store { slots });
                self.stack_height -= slots;

                let prev = self.var_lookup.insert(*name, id);
                self.locals.last_mut().unwrap().push(id);
                assert!(prev.is_none(), "shadow is still new var");
            }
            Stmt::Set { place, value } => self.set_deref(result, place, value)?,
            Stmt::DeclVarPattern { binding, value } => {
                self.compile_expr(result, value)?;
                self.bind_args(result, binding)?;
            }
            Stmt::Noop => {}
            // Can't hit DoneDeclFunc because we don't re-eval constants.
            Stmt::ExpandParsedStmts(_) | Stmt::DoneDeclFunc(_, _) | Stmt::DeclNamed { .. } | Stmt::DeclFunc(_) => unreachable!(),
        }
        Ok(())
    }

    // TODO: make the indices always work out so you could just do it with a normal stack machine.
    //       and just use the slow linear types for debugging.
    fn compile_expr(&mut self, result: &mut FnBody<'p>, expr: &FatExpr<'p>) -> Res<'p, ()> {
        assert!(!expr.ty.is_unknown(), "Not typechecked: {}", expr.log(self.program.pool));
        result.last_loc = expr.loc;
        self.last_loc = Some(expr.loc);

        match expr.deref() {
            Expr::String(_) | Expr::GetParsed(_) | Expr::AddToOverloadSet(_) | Expr::GetNamed(_) | Expr::WipFunc(_) | Expr::Closure(_) => {
                unreachable!()
            }
            Expr::Poison => ice!("POISON",),
            Expr::Call(f, arg) => {
                assert!(!f.ty.is_unknown(), "Not typechecked: {}", f.log(self.program.pool));
                assert!(!arg.ty.is_unknown(), "Not typechecked: {}", arg.log(self.program.pool));
                if let Some(f_id) = f.as_fn() {
                    let func = &self.program[f_id];
                    assert!(!func.has_tag(Flag::Comptime));
                    return self.emit_runtime_call(result, f_id, arg);
                }
                if let Expr::Value {
                    value: Values::One(Value::SplitFunc { ct, rt }),
                    ..
                } = f.expr
                {
                    self.compile_expr(result, arg)?;
                    let f_ty = self.program[ct].unwrap_ty();
                    result.push(Bc::CallSplit { ct, rt });
                    self.stack_height -= self.slot_count(f_ty.arg);
                    self.stack_height += self.slot_count(f_ty.ret);
                    return Ok(());
                }
                if let TypeInfo::FnPtr(f_ty) = self.program[f.ty] {
                    self.compile_expr(result, f)?;
                    self.compile_expr(result, arg)?;
                    result.push(Bc::CallFnPtr {
                        ty: f_ty,
                        comp_ctx: false, // TODO
                    });

                    self.stack_height -= self.slot_count(f_ty.arg) + 1;
                    self.stack_height += self.slot_count(f_ty.ret);
                    return Ok(());
                }
                unreachable!("{}", f.log(self.program.pool))
            }
            Expr::Block { body, result: value, .. } => {
                self.locals.push(vec![]);
                for stmt in body {
                    let s = self.stack_height;
                    self.compile_stmt(result, stmt)?;
                    debug_assert_eq!(s, self.stack_height, "{}", stmt.log(self.program.pool));
                }
                self.compile_expr(result, value)?;

                // TODO: check if you try to let an address to a variable escape from its scope.
                let slots = self.locals.pop().unwrap();
                for id in slots {
                    result.push(Bc::LastUse { id });
                }
            }
            Expr::Tuple(values) => {
                debug_assert!(values.len() > 1, "no trivial tuples: {:?}", values);
                for value in values {
                    self.compile_expr(result, value)?;
                }
            }
            Expr::GetVar(var) => {
                if let Some(id) = self.var_lookup.get(var).cloned() {
                    result.push(Bc::AddrVar { id });
                    let slots = self.slot_count(result.vars[id as usize]);
                    result.push(Bc::Load { slots });
                    self.stack_height += slots;
                } else {
                    ice!("(emit_bc) Missing resolved variable {:?}", var.log(self.program.pool),)
                }
            }
            Expr::Value { value } => {
                // TODO: sometimes you probably want to reference by pointer?
                for value in value.clone().vec().into_iter() {
                    result.push(Bc::PushConstant { value });
                    self.stack_height += 1;
                }
            }
            Expr::SuffixMacro(macro_name, arg) => {
                let name = if let Ok(f) = Flag::try_from(*macro_name) {
                    f
                } else {
                    // TODO: this will change when you're able to define !bang macros in the language.
                    err!(CErr::UndeclaredIdent(*macro_name))
                };
                match name {
                    // TODO: make `let` deeply immutable so only const addr
                    // Note: arg is not evalutated
                    Flag::If => self.emit_call_if(result, arg)?,
                    Flag::While => self.emit_call_while(result, arg)?,
                    Flag::Addr => self.addr_macro(result, arg)?,
                    Flag::Quote => unreachable!(),
                    Flag::Slice => {
                        let container_ty = arg.ty;
                        self.compile_expr(result, arg)?;

                        // Note: number of elements, not size of the whole array value.
                        let ty = self.program.tuple_types(container_ty);
                        let count = if let Some(types) = ty { types.len() } else { 1 };

                        let id = result.add_var(container_ty);

                        let slots = self.slot_count(container_ty);
                        result.push(Bc::AddrVar { id });
                        result.push(Bc::Store { slots });
                        self.stack_height -= slots;
                        result.push(Bc::AddrVar { id });
                        result.push(Bc::PushConstant { value: count as i64 });
                        self.stack_height += 2;
                        self.locals.last_mut().unwrap().push(id);
                    }
                    Flag::C_Call => err!("!c_call has been removed. calling convention is part of the type now.",),
                    Flag::Deref => {
                        self.compile_expr(result, arg)?;
                        let slots = self.slot_count(expr.ty);
                        result.push(Bc::Load { slots });
                        self.stack_height += slots - 1;
                    }
                    Flag::Tag => {
                        debug_assert_eq!(self.program[expr.ty], TypeInfo::Ptr(TypeId::i64()));
                        self.compile_expr(result, arg)?;
                    }
                    Flag::Fn_Ptr => {
                        // TODO: say arg is of type fn and dont let as_fn accept addr ptr. -- Apr 30
                        let f = unwrap!(arg.as_fn(), "expected fn for ptr");
                        result.push(Bc::GetNativeFnPtr(f));
                        self.stack_height += 1;
                    }
                    Flag::Unreachable => {
                        // Don't care about setting output to anything.
                        // TODO: but it does need to leave the v-stack with the expected amount of stuff or asm gest confused. -- May 2
                        result.push(Bc::Unreachable);
                        self.stack_height += 1; // oops
                    }
                    Flag::Uninitialized => {
                        assert!(!expr.ty.is_never(), "call exit() to produce a value of type 'Never'");
                        // Wierd special case I have mixed feelings about. should at least set to sentinal value in debug mode.
                        // Now I have to not mess up the stack, tell the backend somehow.
                        let slots = self.slot_count(expr.ty);
                        for _ in 0..slots {
                            result.push(Bc::PushConstant { value: 0 });
                        }
                        self.stack_height += slots;
                        return Ok(());
                    }
                    name => err!("{name:?} is known flag but not builtin macro",),
                }
            }
            Expr::FieldAccess(_, _) => unreachable!(),
            Expr::Index { ptr, index } => {
                self.compile_expr(result, ptr)?;
                let index = unwrap!(index.as_int(), "tuple index must be const") as usize;
                self.index_expr(result, ptr.ty, index)?
            }
            Expr::StructLiteralP(pattern) => self.construct_struct(result, pattern, expr.ty)?,
            Expr::PrefixMacro { .. } => {
                unreachable!("unhandled macro {}", expr.log(self.program.pool));
            }
        };
        Ok(())
    }

    fn addr_macro(&mut self, result: &mut FnBody<'p>, arg: &FatExpr<'p>) -> Res<'p, ()> {
        self.last_loc = Some(arg.loc);
        match arg.deref() {
            Expr::GetVar(var) => {
                if let Some(id) = self.var_lookup.get(var).cloned() {
                    if var.3 != VarType::Var {
                        err!(
                            "Can only take address of vars not {:?} {}. TODO: allow read field.",
                            var.3,
                            var.log(self.program.pool)
                        )
                    }
                    result.push(Bc::AddrVar { id });
                    self.stack_height += 1;
                } else {
                    ice!("Missing var {} (in !addr)", var.log(self.program.pool))
                }
            }
            // TODO: this is a bit weird but it makes place expressions work.
            Expr::Index { .. } => self.compile_expr(result, arg)?,
            _ => err!("took address of r-value",),
        }
        Ok(())
    }

    // TODO: make this not a special case.
    /// This swaps out the closures for function accesses.
    fn emit_call_if(&mut self, result: &mut FnBody<'p>, arg: &FatExpr<'p>) -> Res<'p, ()> {
        let (if_true, if_false) = if let Expr::Tuple(parts) = &arg.expr {
            self.compile_expr(result, &parts[0])?; // cond
            self.stack_height -= 1;
            let if_true = &parts[1];
            let if_false = &parts[2];
            (if_true, if_false)
        } else {
            ice!("if args must be tuple not {:?}", arg);
        };
        let slots = self.slot_count(if_true.ty);

        let index = result.if_debug_count;
        result.if_debug_count += 1;

        let id = result.add_var(if_true.ty);

        let branch_ip = result.push(Bc::NoCompile); // patch
        let true_ip = result.insts.len() as u16;
        self.compile_expr(result, if_true)?;
        self.stack_height -= slots;
        result.push(Bc::AddrVar { id });
        result.push(Bc::Store { slots });
        result.push(Bc::EndIf { index, slots });
        let jump_over_false = result.push(Bc::NoCompile);
        let false_ip = result.insts.len() as u16;
        self.compile_expr(result, if_false)?;

        // TODO: 'Never' should work anywhere, not just false branch of ifs. it just so happens thats the only one my tests need currently -- May 3.
        // TODO: HACK: need to know how many slots to pretend we have
        if if_false.ty.is_never() && !if_true.ty.is_never() {
            // currently we treat Never as taking 1 slot (for unreachable for example), so pop that off, and then pretend we pushed everything they want.
            // dont bother storing, we never get there, but the backend can load the temp var set by the other branch.
            result.push(Bc::Pop { slots: 1 });
            self.stack_height += slots - 1;
        } else {
            result.push(Bc::AddrVar { id });
            result.push(Bc::Store { slots });
        }

        result.push(Bc::EndIf { index, slots });
        result.insts[branch_ip] = Bc::JumpIf { false_ip, true_ip };
        result.insts[jump_over_false] = Bc::Goto {
            ip: result.insts.len() as u16,
        };
        result.jump_targets.set(result.insts.len());
        result.jump_targets.set(true_ip as usize);
        result.jump_targets.set(false_ip as usize);
        result.push(Bc::AddrVar { id });
        result.push(Bc::Load { slots });
        result.push(Bc::LastUse { id });

        Ok(())
    }

    fn emit_call_while(&mut self, result: &mut FnBody<'p>, arg: &FatExpr<'p>) -> Res<'p, ()> {
        let (cond_fn, body_fn) = if let Expr::Tuple(parts) = arg.deref() {
            (&parts[0], &parts[1])
        } else {
            ice!("if args must be tuple not {:?}", arg);
        };

        debug_assert_eq!(body_fn.ty, TypeId::unit());

        let index = result.if_debug_count;
        result.if_debug_count += 1;

        result.push(Bc::StartLoop);
        result.push(Bc::EndIf { index, slots: 0 });
        let cond_ip = result.insts.len() as u16;
        debug_assert_eq!(cond_fn.ty, TypeId::bool());
        self.compile_expr(result, cond_fn)?;
        self.stack_height -= 1;
        let branch_ip = result.push(Bc::NoCompile); // patch
        let body_ip = result.insts.len() as u16;
        self.compile_expr(result, body_fn)?;
        let slots = self.slot_count(body_fn.ty);
        result.push(Bc::Pop { slots });
        result.push(Bc::EndIf { index, slots: 0 });
        self.stack_height -= slots;

        result.push(Bc::Goto { ip: cond_ip });
        let end_ip = result.insts.len() as u16;

        // TODO: change to conditional so dont have to store the true_ip, but then I'd need to reconstruct it for llvm so meh
        result.insts[branch_ip] = Bc::JumpIf {
            true_ip: body_ip,
            false_ip: end_ip,
        };
        result.jump_targets.set(cond_ip as usize);
        result.jump_targets.set(branch_ip);
        result.jump_targets.set(body_ip as usize);
        result.jump_targets.set(end_ip as usize);

        self.stack_height += 1;
        result.push(Bc::PushConstant { value: 0 }); // TODO: caller expects a unit on the stack

        Ok(())
    }

    fn set_deref(&mut self, result: &mut FnBody<'p>, place: &FatExpr<'p>, value: &FatExpr<'p>) -> Res<'p, ()> {
        match place.deref() {
            Expr::GetVar(var) => {
                let id = self.var_lookup.get(var);
                let id = *unwrap!(id, "SetVar: var must be declared: {}", var.log(self.program.pool));
                self.compile_expr(result, value)?;
                result.push(Bc::AddrVar { id });
                let slots = self.slot_count(value.ty);
                result.push(Bc::Store { slots });
                self.stack_height -= slots;
                Ok(())
            }
            Expr::SuffixMacro(macro_name, arg) => {
                // TODO: this is bad UB kinda!!! You'd expect the expr producing the pointer to be evaled first but
                //       i want the ptr on top of the stack so i do it in the other order!
                //       only matters if its an lvalue that has side effects. should deal with this!!! -- May 1
                if let Ok(Flag::Deref) = Flag::try_from(*macro_name) {
                    self.compile_expr(result, value)?;
                    self.compile_expr(result, arg)?;
                    let slots = self.slot_count(value.ty);
                    result.push(Bc::Store { slots });
                    self.stack_height -= slots + 1; // 1 for ptr
                    return Ok(());
                }
                todo!()
            }
            &Expr::GetNamed(n) => err!(CErr::UndeclaredIdent(n)),
            _ => ice!("TODO: other `place=e;`"),
        }
    }

    fn index_expr(&mut self, result: &mut FnBody<'p>, container_ptr_ty: TypeId, index: usize) -> Res<'p, ()> {
        let depth = self.program.ptr_depth(container_ptr_ty);
        assert_eq!(depth, 1);
        let container_ty = unwrap!(
            self.program.unptr_ty(container_ptr_ty),
            "unreachable unptr_ty {:?}",
            self.program.log_type(container_ptr_ty)
        );

        let raw_container_ty = self.program.raw_type(container_ty);
        match &self.program[raw_container_ty] {
            TypeInfo::Struct { fields, .. } => {
                let mut offset = 0;
                for f in fields.iter().take(index) {
                    offset += self.slot_count(f.ty);
                }
                let f = fields[index];
                let offset = if let Some(bytes) = f.ffi_byte_offset {
                    assert_eq!(bytes % 8, 0);
                    (bytes / 8) as u16
                } else {
                    offset
                };
                result.push(Bc::IncPtr { offset });
            }
            TypeInfo::Enum { .. } => {
                result.push(Bc::TagCheck { expected: index as u16 });
                result.push(Bc::IncPtr { offset: 1 });
            }
            TypeInfo::Tuple(types) => {
                let mut offset = 0;
                for f_ty in types.iter().take(index) {
                    offset += self.slot_count(*f_ty);
                }
                result.push(Bc::IncPtr { offset });
            }
            _ => err!(
                "only structs/enums support field access but found {} = {}",
                self.program.log_type(container_ty),
                self.program.log_type(raw_container_ty)
            ),
        }
        Ok(())
    }
    pub fn slot_count(&mut self, ty: TypeId) -> u16 {
        self.sizes.slot_count(self.program, ty) as u16
    }

    fn construct_struct(&mut self, result: &mut FnBody<'p>, pattern: &Pattern<'p>, requested: TypeId) -> Res<'p, ()> {
        let names: Vec<_> = pattern.flatten_names();
        // TODO: why must this suck so bad
        let values: Option<_> = pattern.flatten_exprs_ref();
        let values: Vec<_> = values.unwrap();
        assert_eq!(names.len(), values.len());
        let raw_container_ty = self.program.raw_type(requested);

        match &self.program[raw_container_ty] {
            TypeInfo::Struct { fields, .. } => {
                assert_eq!(
                    fields.len(),
                    values.len(),
                    "Cannot assign {values:?} to type {} = {fields:?}",
                    self.program.log_type(requested)
                );
                let all = names.into_iter().zip(values).zip(fields);
                let mut offset = 0;
                for ((name, value), field) in all {
                    assert_eq!(name, field.name);
                    let size = self.slot_count(field.ty);
                    self.compile_expr(result, value)?;
                    offset += size;
                }
                assert_eq!(offset, self.slot_count(raw_container_ty), "Didn't fill whole struct");
            }
            TypeInfo::Enum { cases } => {
                let size = self.slot_count(raw_container_ty);
                assert_eq!(
                    1,
                    values.len(),
                    "{} is an enum, value should have one active varient not {values:?}",
                    self.program.log_type(requested)
                );
                let i = cases.iter().position(|f| f.0 == names[0]).unwrap();
                let payload_size = self.slot_count(cases[i].1);
                if payload_size >= size {
                    ice!("Enum value won't fit.")
                }
                // TODO: make this constexpr in compiler
                result.push(Bc::PushConstant { value: i as i64 });
                self.stack_height += 1;
                self.compile_expr(result, values[0])?;

                // If this is a smaller varient, pad out the slot.
                for _ in (payload_size + 1)..size {
                    result.push(Bc::PushConstant { value: 123 });
                    self.stack_height += 1;
                }
                // TODO: support explicit uninit so backend doesn't emit code for the padding above.
                //       current system also means weird type stuff where you write ints into units in bc_to_asm.
                //       if we staticlly know the tag value, could only copy the size of the active varient (which would be the general case of leaving it uninit when creating one).
                //       but also eventually we probably want to define and preserve padding so tags could be stored there.
                //       even without that, maybe its a weird undefined behaviour to just drop some of your bytes,
                //       should have compiler settings about being allowed to cast *T -> *[size_of(T)]u8 because that would observe the padding.
                //       I like the idea of granular toggling ub vs optimisations and having those flags as a place to hang comments,
                //       but that adds a lot of extra work testing all the combinations which might not be worth it.
                //       -- Apr 17
                //
            }
            _ => err!("struct literal but expected {:?}", requested),
        }
        Ok(())
    }
}

// TODO: !!! doesnt work if you use .slot_count and .byte_count. dont allow that.
impl SizeCache {
    // TODO: Unsized types. Any should be a TypeId and then some memory with AnyPtr being the fat ptr version.
    //       With raw Any version, you couldn't always change types without reallocating the space and couldn't pass it by value.
    //       AnyScalar=(TypeId, one value), AnyPtr=(TypeId, one value=stack/heap ptr), AnyUnsized=(TypeId, some number of stack slots...)
    pub fn slot_count(&mut self, program: &Program, ty: TypeId) -> usize {
        extend_options(&mut self.known, ty.as_index());
        if let Some(size) = self.known[ty.as_index()] {
            return size;
        }
        let ty = program.raw_type(ty);
        extend_options(&mut self.known, ty.as_index());
        let size = match &program[ty] {
            TypeInfo::Unknown => 9999,
            TypeInfo::Tuple(args) => args.iter().map(|t| self.slot_count(program, *t)).sum(),
            TypeInfo::Struct { fields, .. } => fields.iter().map(|f| self.slot_count(program, f.ty)).sum(),
            TypeInfo::Enum { cases, .. } => 1 + cases.iter().map(|(_, ty)| self.slot_count(program, *ty)).max().expect("no empty enum"),
            TypeInfo::Scope => 2,
            TypeInfo::Int(_)
            | TypeInfo::Any
            | TypeInfo::Never
            | TypeInfo::F64
            | TypeInfo::Bool
            | TypeInfo::Fn(_)
            | TypeInfo::Ptr(_)
            | TypeInfo::VoidPtr
            | TypeInfo::FnPtr(_)
            | TypeInfo::Type
            | TypeInfo::OverloadSet
            | TypeInfo::Unit => 1,
            TypeInfo::Unique(_, _) | TypeInfo::Named(_, _) => unreachable!(),
        };
        self.known[ty.as_index()] = Some(size);
        size
    }
}

impl<'p> FnBody<'p> {
    #[track_caller]
    fn push(&mut self, inst: Bc) -> usize {
        let ip = self.insts.len();
        self.insts.push(inst);
        ip
    }
}
