//! Converts simple ASTs into my bytecode-ish format.

#![allow(clippy::wrong_self_convention)]

use codemap::Span;
use std::marker::PhantomData;
use std::ops::Deref;
use std::panic::Location;
use std::usize;

use crate::ast::{Expr, FatExpr, FuncId, Program, Stmt, TypeId, TypeInfo};
use crate::ast::{FatStmt, Flag, Pattern, Var, VarType};
use crate::bc::*;
use crate::compiler::{CErr, Compile, FnWip, Res};
use crate::extend_options;
use crate::logging::PoolLog;
use crate::reflect::BitSet;

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
    locals: Vec<Vec<StackRange>>,
    locals_drop: Vec<Vec<StackRange>>,
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
            locals_drop: vec![],
        }
    }

    #[track_caller]
    fn empty_fn(&mut self, func: &FnWip<'p>) -> FnBody<'p> {
        let mut jump_targets = BitSet::empty();
        jump_targets.set(0); // entry is the first instruction
        FnBody {
            jump_targets,
            arg_range: StackRange {
                first: StackOffset(0),
                count: 0,
            },
            stack_slots: 0,
            vars: Default::default(),
            when: func.when,
            func: func.func,
            why: func.why.clone(),
            last_loc: func.last_loc,
            insts: vec![],
            debug: vec![],
            slot_types: vec![],
            to_drop: vec![],
            slot_is_var: BitSet::empty(),
        }
    }

    fn compile_inner(&mut self, f: FuncId) -> Res<'p, FnBody<'p>> {
        self.locals.clear();
        self.locals.push(vec![]);
        self.locals_drop.clear();
        self.locals_drop.push(vec![]); // TODO: use this for args_to_drop
        let func = &self.program[f];
        let wip = unwrap!(func.wip.as_ref(), "Not done comptime for {f:?}"); // TODO
        debug_assert!(!func.evil_uninit);
        let mut result = self.empty_fn(wip);
        let func = &self.program[f];
        // println!("{} {:?}", self.program.pool.get(func.name), func.body);
        let arg_range = result.reserve_slots(self, func.unwrap_ty().arg)?;
        result.arg_range = arg_range;
        let ret_val = result.reserve_slots(self, func.unwrap_ty().ret)?;
        match self.emit_body(&mut result, arg_range, f, ret_val) {
            Ok(_) => {
                result.push(Bc::Ret(ret_val));
                Ok(result)
            }
            Err(mut e) => {
                e.loc = self.last_loc;
                Err(e)
            }
        }
    }

    fn bind_args(
        &mut self,
        result: &mut FnBody<'p>,
        full_arg_range: StackRange,
        pattern: &Pattern<'p>,
    ) -> Res<'p, Vec<(Option<Var<'p>>, StackRange, TypeId)>> {
        for i in full_arg_range {
            result.slot_is_var.set(i);
        }
        let mut args_to_drop = vec![];
        let arguments = pattern.flatten();
        let mut slot_count = 0;
        for (name, ty, kind) in arguments {
            // TODO:? probably fine, i jsut set to const in closure capture but then shouldn't be adding to vars below.
            // TODO: the frontend needs to remove the 'const' parts from the ast in DeclVarPattern
            assert!(kind != VarType::Const, "{:?}", name.map(|v| v.log(self.program.pool)));
            let size = self.slot_count(ty);
            let range = StackRange {
                first: full_arg_range.offset(slot_count),
                count: size,
            };
            if let Some(name) = name {
                let prev = result.vars.insert(name, (range, ty));
                assert!(prev.is_none(), "overwrite arg? {}", name.log(self.program.pool));
            }
            args_to_drop.push((name, range, ty));
            slot_count += size;
        }
        // assert_eq!(full_arg_range.count, slot_count);
        Ok(args_to_drop)
    }

    fn emit_body(&mut self, result: &mut FnBody<'p>, full_arg_range: StackRange, f: FuncId, ret_val: StackRange) -> Res<'p, ()> {
        let func = &self.program[f];
        let has_body = func.body.is_some();

        let mut args_to_drop = self.bind_args(result, full_arg_range, &func.arg)?;
        self.locals.last_mut().unwrap().push(full_arg_range);

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

        let body = func.body.as_ref().unwrap();
        // TODO: this would be where you want to do result ptr param stuff.
        // We're done with our arguments, get rid of them. Same for other vars.
        // TODO: once non-copy types are supported, this needs to get smarter because we might have moved out of our argument.
        self.compile_expr(result, body, ret_val)?;
        args_to_drop.extend(result.to_drop.drain(0..).map(|(s, ty)| (None, s, ty)));
        for (var, range, _ty) in args_to_drop {
            if let Some(var) = var {
                let (slot, _) = unwrap!(result.vars.remove(&var), "lost arg");
                assert_eq!(range, slot, "moved arg");
            }
        }
        // TODO: copy-paste
        for slot in self.locals.pop().unwrap() {
            result.push(Bc::LastUse(slot));
            for i in slot {
                assert!(result.slot_is_var.get(i));
            }
        }
        assert!(self.locals.is_empty());
        let slots = self.locals_drop.pop().unwrap();
        for slot in slots {
            result.push(Bc::LastUse(slot));
            for i in slot {
                assert!(result.slot_is_var.get(i));
            }
        }
        assert!(self.locals_drop.is_empty());

        Ok(())
    }

    fn emit_runtime_call(&mut self, result: &mut FnBody<'p>, f: FuncId, arg_expr: &FatExpr<'p>, ret: StackRange) -> Res<'p, ()> {
        let arg = result.reserve_slots(self, arg_expr.ty)?;
        self.compile_expr(result, arg_expr, arg)?;
        let func = &self.program[f];
        let f_ty = func.unwrap_ty();
        let func = &self.program[f];
        assert!(func.capture_vars.is_empty());
        assert!(!func.has_tag(Flag::Inline));

        result.push(Bc::CallDirect { f, ret, arg });

        if !func.finished_ret.unwrap().is_never() {
            assert_eq!(self.return_stack_slots(f), ret.count);
            assert_eq!(self.slot_count(f_ty.ret), ret.count);
        }
        Ok(())
    }

    fn compile_stmt(&mut self, result: &mut FnBody<'p>, stmt: &FatStmt<'p>) -> Res<'p, ()> {
        self.last_loc = Some(stmt.loc);
        match stmt.deref() {
            Stmt::Eval(expr) => {
                let ret = result.reserve_slots(self, expr.ty)?;
                self.compile_expr(result, expr, ret)?;
            }
            Stmt::DeclVar { name, ty, value, kind } => {
                assert_ne!(VarType::Const, *kind);
                let ty = ty.unwrap();
                let ret = result.reserve_slots(self, ty)?;

                if let Some(expr) = value {
                    self.compile_expr(result, expr, ret)?;
                }
                for i in ret {
                    result.slot_is_var.set(i);
                }
                let prev = result.vars.insert(*name, (ret, ty));
                self.locals.last_mut().unwrap().push(ret);
                assert!(prev.is_none(), "shadow is still new var");
            }
            Stmt::Set { place, value } => self.set_deref(result, place, value)?,
            Stmt::DeclVarPattern { binding, value } => {
                if let Some(value) = value.as_ref() {
                    let full_arg_range = result.reserve_slots(self, value.ty)?;
                    self.compile_expr(result, value, full_arg_range)?;
                    for i in full_arg_range {
                        result.slot_is_var.set(i);
                    }
                    self.locals.last_mut().unwrap().push(full_arg_range);
                    let args_to_drop = self.bind_args(result, full_arg_range, binding)?;
                    for (name, _, _) in args_to_drop {
                        if name.is_none() {}
                    }
                } else {
                    assert!(binding.bindings.is_empty());
                }
            }
            Stmt::Noop => {}
            // Can't hit DoneDeclFunc because we don't re-eval constants.
            Stmt::ExpandParsedStmts(_) | Stmt::DoneDeclFunc(_, _) | Stmt::DeclNamed { .. } | Stmt::DeclFunc(_) => unreachable!(),
        }
        Ok(())
    }

    fn return_stack_slots(&mut self, f: FuncId) -> usize {
        // You must self.infer_types(f); before calling this
        let func = &self.program[f];
        let ty = func.unwrap_ty();
        self.slot_count(ty.ret)
    }

    // TODO: make the indices always work out so you could just do it with a normal stack machine.
    //       and just use the slow linear types for debugging.
    fn compile_expr(&mut self, result: &mut FnBody<'p>, expr: &FatExpr<'p>, output: StackRange) -> Res<'p, ()> {
        assert!(!expr.ty.is_unknown(), "Not typechecked: {}", expr.log(self.program.pool));
        result.last_loc = expr.loc;
        self.last_loc = Some(expr.loc);

        match expr.deref() {
            Expr::GetParsed(_) | Expr::AddToOverloadSet(_) => unreachable!(),
            Expr::Poison => ice!("POISON",),
            Expr::GetNamed(_) | Expr::WipFunc(_) | Expr::Closure(_) => unreachable!(),
            Expr::Call(f, arg) => {
                assert!(!f.ty.is_unknown(), "Not typechecked: {}", f.log(self.program.pool));
                assert!(!arg.ty.is_unknown(), "Not typechecked: {}", arg.log(self.program.pool));
                if let Some(f_id) = f.as_fn() {
                    let func = &self.program[f_id];
                    assert!(!func.has_tag(Flag::Comptime));
                    return self.emit_runtime_call(result, f_id, arg, output);
                }
                if let Expr::Value {
                    value: Values::One(Value::SplitFunc { ct, rt }),
                    ..
                } = f.expr
                {
                    let arg_slot = result.reserve_slots(self, arg.ty)?;
                    self.compile_expr(result, arg, arg_slot)?;

                    result.push(Bc::CallSplit {
                        ct,
                        rt,
                        arg: arg_slot,
                        ret: output,
                    });
                    return Ok(());
                }
                if let TypeInfo::FnPtr(f_ty) = self.program[f.ty] {
                    let f_slot = result.reserve_slots(self, f.ty)?;
                    self.compile_expr(result, f, f_slot)?;
                    let arg_slot = result.reserve_slots(self, arg.ty)?;
                    self.compile_expr(result, arg, arg_slot)?;

                    result.push(Bc::CallFnPtr {
                        f: f_slot.single(),
                        arg: arg_slot,
                        ret: output,
                        ty: f_ty,
                        comp_ctx: false, // TODO
                    });
                    return Ok(());
                }
                unreachable!("{}", f.log(self.program.pool))
            }
            Expr::Block { body, result: value, .. } => {
                self.locals.push(vec![]);
                self.locals_drop.push(vec![]);
                for stmt in body {
                    self.compile_stmt(result, stmt)?;
                }
                self.compile_expr(result, value, output)?;

                // TODO: check if you try to let an address to a variable escape from its scope.
                // TODO: redundant with ^ but i dont trust
                let slots = self.locals.pop().unwrap();
                for slot in slots {
                    result.push(Bc::LastUse(slot));
                    for i in slot {
                        assert!(result.slot_is_var.get(i));
                    }
                }
                let slots = self.locals_drop.pop().unwrap();
                for slot in slots {
                    result.push(Bc::LastUse(slot));
                    for i in slot {
                        assert!(result.slot_is_var.get(i));
                    }
                }
            }
            Expr::Tuple(values) => {
                debug_assert!(values.len() > 1, "no trivial tuples: {:?}", values);
                let mut offset = 0;
                for value in values {
                    let slot = output.range(offset, self.slot_count(value.ty));
                    self.compile_expr(result, value, slot)?;
                    offset += self.slot_count(value.ty);
                }
            }
            Expr::GetVar(var) => {
                if let Some((from, ty)) = result.vars.get(var).cloned() {
                    debug_assert_eq!(expr.ty, ty);
                    debug_assert_eq!(from.count, output.count, "{}", self.program.log_type(ty));
                    if from.count == 1 {
                        result.push(Bc::Clone {
                            from: from.first,
                            to: output.first,
                        });
                    } else {
                        result.push(Bc::CloneRange { from, to: output });
                    }
                } else {
                    ice!("(emit_bc) Missing resolved variable {:?}", var.log(self.program.pool),)
                }
            }
            Expr::Value { value, .. } => {
                // TODO: sometimes you probably want to reference by pointer?
                for (i, value) in value.clone().vec().into_iter().enumerate() {
                    result.push(Bc::LoadConstant {
                        slot: output.offset(i),
                        value,
                    });
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
                    Flag::If => self.emit_call_if(result, arg, output)?,
                    Flag::While => self.emit_call_while(result, arg, output)?,
                    Flag::Addr => self.addr_macro(result, arg, expr.ty, output)?,
                    Flag::Quote => unreachable!(),
                    Flag::Slice => {
                        let container_ty = arg.ty;
                        let container = result.reserve_slots(self, container_ty)?;
                        self.compile_expr(result, arg, container)?;
                        let ty = self.program.tuple_types(container_ty);
                        let (_, count) = if let Some(types) = ty {
                            let expect = *unwrap!(types.iter().find(|t| !t.is_any()), "all any");
                            (expect, types.len())
                        } else {
                            (container_ty, 1)
                        };
                        for i in container {
                            result.slot_is_var.set(i);
                        }
                        assert_eq!(output.count, 2, "Expected slice ouput");
                        result.push(Bc::AbsoluteStackAddr {
                            of: container,
                            to: output.offset(0),
                        });
                        result.push(Bc::LoadConstant {
                            slot: output.offset(1),
                            value: Value::I64(count as i64),
                        });
                        self.locals_drop.last_mut().unwrap().push(container);
                    }
                    Flag::C_Call => err!("!c_call has been removed. calling convention is part of the type now.",),
                    Flag::Deref => {
                        // TODO: @switch gets you a unique type here.
                        // debug_assert_eq!(self.program[arg.ty], TypeInfo::Ptr(expr.ty));
                        let ptr = result.reserve_slots(self, arg.ty)?;
                        self.compile_expr(result, arg, ptr)?;
                        result.push(Bc::Load {
                            from: ptr.single(),
                            to: output,
                        });
                    }
                    Flag::Tag => {
                        // TODO: auto deref and typecheking
                        debug_assert_eq!(self.program[expr.ty], TypeInfo::Ptr(TypeId::i64()));
                        let addr = result.reserve_slots(self, arg.ty)?;
                        self.compile_expr(result, arg, addr)?;
                        result.push(Bc::SlicePtr {
                            base: addr.single(),
                            offset: 0,
                            count: 1,
                            ret: output.single(),
                        });
                    }
                    Flag::Fn_Ptr => unreachable!(),
                    Flag::Unreachable => {
                        result.push(Bc::Unreachable);
                        // Don't care about setting output to anything.
                    }
                    Flag::Uninitialized => {
                        assert!(!expr.ty.is_never(), "call exit() to produce a value of type 'Never'");
                        // Wierd special case I have mixed feelings about. should at least set to sentinal value in debug mode.
                        return Ok(());
                    }
                    name => err!("{name:?} is known flag but not builtin macro",),
                }
            }
            Expr::FieldAccess(_, _) => unreachable!(),
            Expr::Index { ptr, index } => {
                let container_ptr = result.reserve_slots(self, ptr.ty)?;
                self.compile_expr(result, ptr, container_ptr)?;
                let index = unwrap!(index.as_int(), "tuple index must be const") as usize;
                self.index_expr(result, ptr.ty, container_ptr, index, output)?
            }
            Expr::StructLiteralP(pattern) => self.construct_struct(result, pattern, expr.ty, output)?,
            Expr::PrefixMacro { .. } => {
                unreachable!("unhandled macro {}", expr.log(self.program.pool));
            }
            Expr::String(_) => unreachable!("{}", expr.log(self.program.pool)),
        };
        Ok(())
    }

    fn addr_macro(&mut self, result: &mut FnBody<'p>, arg: &FatExpr<'p>, ptr_ty: TypeId, addr_slot: StackRange) -> Res<'p, ()> {
        self.last_loc = Some(arg.loc);
        match arg.deref() {
            Expr::GetVar(var) => {
                if let Some((stack_slot, value_ty)) = result.vars.get(var).cloned() {
                    if var.3 != VarType::Var {
                        err!(
                            "Can only take address of vars not {:?} {}. TODO: allow read field.",
                            var.3,
                            var.log(self.program.pool)
                        )
                    }
                    for i in stack_slot {
                        assert!(result.slot_is_var.get(i), "addr non-var {}", var.log(self.program.pool));
                    }

                    debug_assert_eq!(self.program[ptr_ty], TypeInfo::Ptr(value_ty));
                    result.push(Bc::AbsoluteStackAddr {
                        of: stack_slot,
                        to: addr_slot.single(),
                    });
                } else {
                    ice!("Missing var {} (in !addr)", var.log(self.program.pool))
                }
            }
            Expr::SuffixMacro(macro_name, _) => {
                let name = self.program.pool.get(*macro_name);
                ice!("Took address of macro {name} not supported")
            }
            Expr::FieldAccess(_, _) => unreachable!(),
            // TODO: this is a bit weird but it makes place expressions work.
            Expr::Index { .. } => self.compile_expr(result, arg, addr_slot)?,
            &Expr::GetNamed(i) => err!(CErr::UndeclaredIdent(i)),
            _ => err!("took address of r-value",),
        }
        Ok(())
    }

    // TODO: make this not a special case.
    /// This swaps out the closures for function accesses.
    fn emit_call_if(&mut self, result: &mut FnBody<'p>, arg: &FatExpr<'p>, ret: StackRange) -> Res<'p, ()> {
        let cond_slot = result.reserve_slots(self, TypeId::bool())?;
        let (if_true, if_false) = if let Expr::Tuple(parts) = &arg.expr {
            self.compile_expr(result, &parts[0], cond_slot)?;
            let if_true = &parts[1];
            let if_false = &parts[2];
            (if_true, if_false)
        } else {
            ice!("if args must be tuple not {:?}", arg);
        };

        // TODO: Really its not a var cause its only set on one branch but I don't want to deal with llvm phi right now.
        for i in ret {
            result.slot_is_var.set(i);
        }

        let branch_ip = result.push(Bc::DebugMarker(Flag::Patch.ident(), Flag::Builtin_If.ident()));
        let true_ip = result.insts.len();
        self.compile_expr(result, if_true, ret)?;
        let jump_over_false = result.push(Bc::DebugMarker(Flag::Patch.ident(), Flag::Builtin_If.ident()));
        let false_ip = result.insts.len();
        self.compile_expr(result, if_false, ret)?;

        result.insts[branch_ip] = Bc::JumpIf {
            // TODO: change to conditional so dont have to store the true_ip
            cond: cond_slot.single(),
            true_ip,
            false_ip,
        };
        result.insts[jump_over_false] = Bc::Goto { ip: result.insts.len() };
        result.jump_targets.set(result.insts.len());
        result.jump_targets.set(true_ip);
        result.jump_targets.set(false_ip);

        Ok(())
    }

    fn emit_call_while(&mut self, result: &mut FnBody<'p>, arg: &FatExpr<'p>, output: StackRange) -> Res<'p, ()> {
        let (cond_fn, body_fn) = if let Expr::Tuple(parts) = arg.deref() {
            (&parts[0], &parts[1])
        } else {
            ice!("if args must be tuple not {:?}", arg);
        };

        // This fixes interp poison debug err when loop only runs once.
        result.slot_is_var.set(output.single().0); // TODO: just don't even do any of this shit with Unit. this is for ssa
        result.push(Bc::LoadConstant {
            slot: output.single(),
            value: Value::Unit,
        });

        let cond_ret = result.reserve_slots(self, TypeId::bool())?;
        let cond_ip = result.insts.len();
        self.compile_expr(result, cond_fn, cond_ret)?;
        let branch_ip = result.push(Bc::DebugMarker(Flag::Patch.ident(), Flag::Builtin_While.ident()));

        let body_ip = result.insts.len();
        self.compile_expr(result, body_fn, output)?;
        result.push(Bc::Goto { ip: cond_ip });
        let end_ip = result.insts.len();
        result.insts[branch_ip] = Bc::JumpIf {
            // TODO: change to conditional so dont have to store the true_ip
            cond: cond_ret.single(),
            true_ip: body_ip,
            false_ip: end_ip,
        };
        result.jump_targets.set(cond_ip);
        result.jump_targets.set(branch_ip);
        result.jump_targets.set(body_ip);
        result.jump_targets.set(end_ip);

        Ok(())
    }

    fn set_deref(&mut self, result: &mut FnBody<'p>, place: &FatExpr<'p>, value: &FatExpr<'p>) -> Res<'p, ()> {
        match place.deref() {
            Expr::GetVar(var) => {
                let slot = result.vars.get(var);
                let (slot, _) = *unwrap!(slot, "SetVar: var must be declared: {}", var.log(self.program.pool));
                self.compile_expr(result, value, slot)?;
                Ok(())
            }
            Expr::SuffixMacro(macro_name, arg) => {
                // TODO: type checking
                // TODO: general place expressions.
                if let Ok(Flag::Deref) = Flag::try_from(*macro_name) {
                    // TODO: this is why you want the output to be an enum Place { StackRange | Deref(StackSlot, len) }
                    let ptr = result.reserve_slots(self, arg.ty)?;
                    let value_slot = result.reserve_slots(self, value.ty)?;
                    self.compile_expr(result, arg, ptr)?;
                    self.compile_expr(result, value, value_slot)?;
                    result.push(Bc::Store {
                        to: ptr.single(),
                        from: value_slot,
                    });

                    return Ok(());
                }
                todo!()
            }
            &Expr::GetNamed(n) => err!(CErr::UndeclaredIdent(n)),
            _ => ice!("TODO: other `place=e;`"),
        }
    }

    fn index_expr(
        &mut self,
        result: &mut FnBody<'p>,
        container_ptr_ty: TypeId,
        container_ptr: StackRange,
        index: usize,
        ret: StackRange,
    ) -> Res<'p, ()> {
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
                    bytes / 8
                } else {
                    offset
                };
                result.push(Bc::SlicePtr {
                    base: container_ptr.single(),
                    offset,
                    count: self.slot_count(f.ty),
                    ret: ret.single(),
                });
            }
            TypeInfo::Enum { cases, .. } => {
                let f_ty = cases[index].1;
                let count = self.slot_count(f_ty);
                result.push(Bc::TagCheck {
                    enum_ptr: container_ptr.single(),
                    value: index as i64,
                });
                result.push(Bc::SlicePtr {
                    base: container_ptr.single(),
                    offset: 1,
                    count,
                    ret: ret.single(),
                });
            }
            TypeInfo::Tuple(types) => {
                let mut offset = 0;
                for f_ty in types.iter().take(index) {
                    offset += self.slot_count(*f_ty);
                }
                let f_ty = types[index];
                result.push(Bc::SlicePtr {
                    base: container_ptr.single(),
                    offset,
                    count: self.slot_count(f_ty),
                    ret: ret.single(),
                });
            }
            _ => err!(
                "only structs/enums support field access but found {} = {}",
                self.program.log_type(container_ty),
                self.program.log_type(raw_container_ty)
            ),
        }
        Ok(())
    }
    pub fn slot_count(&mut self, ty: TypeId) -> usize {
        self.sizes.slot_count(self.program, ty)
    }

    fn construct_struct(&mut self, result: &mut FnBody<'p>, pattern: &Pattern<'p>, requested: TypeId, output: StackRange) -> Res<'p, ()> {
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
                    self.compile_expr(result, value, output.range(offset, size))?;
                    offset += size;
                }
                assert_eq!(offset, output.count, "Didn't fill whole struct");
            }
            TypeInfo::Enum { cases } => {
                let size = self.slot_count(raw_container_ty);
                assert_eq!(size, output.count);
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
                let payload_slot = output.range(1, payload_size);
                self.compile_expr(result, values[0], payload_slot)?;
                // TODO: make this constexpr in compiler
                result.push(Bc::LoadConstant {
                    slot: output.first,
                    value: Value::I64(i as i64),
                });
                // If this is a smaller varient, pad out the slot instead of poisons. cant be unit because then asm doesnt copy it around
                for i in (payload_size + 1)..output.count {
                    result.push(Bc::LoadConstant {
                        slot: output.offset(i),
                        value: Value::I64(123),
                    });
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
    fn reserve_slots_raw(&mut self, program: &mut EmitBc<'_, 'p>, count: usize, ty: TypeId) -> Res<'p, StackRange> {
        let first = StackOffset(self.stack_slots);

        if ty.is_any() {
            debug_assert_eq!(count, 1, "no any tuple");
            self.slot_types.push(TypeId::any());
            self.stack_slots += count;
        } else if count == 1 {
            self.slot_types.push(ty);
            self.stack_slots += count;
        } else {
            let types = unwrap!(
                program.program.tuple_types(ty),
                "expected multiple slots {:?}",
                program.program.log_type(ty)
            );
            let mut found = 0;
            for ty in types {
                found += self.reserve_slots_inner(program, *ty)?.count;
            }
            debug_assert_eq!(found, count, "bad tuple size");
            // Note: don't bump self.stack_slots here.
        }
        Ok(StackRange { first, count })
    }

    #[track_caller]
    fn reserve_slots(&mut self, program: &mut EmitBc<'_, 'p>, ty: TypeId) -> Res<'p, StackRange> {
        self.mark_contiguous(program, ty);
        self.reserve_slots_inner(program, ty)
    }

    fn mark_contiguous(&mut self, program: &mut EmitBc<'_, 'p>, ty: TypeId) {
        let ty = program.program.raw_type(ty);
        let count = program.slot_count(ty);
        if count == 1 {
            return;
        }
        let first = StackOffset(self.stack_slots);
        self.push(Bc::MarkContiguous(StackRange { first, count }, ty));
    }

    #[track_caller]
    fn reserve_slots_inner(&mut self, program: &mut EmitBc<'_, 'p>, ty: TypeId) -> Res<'p, StackRange> {
        let ty = program.program.raw_type(ty);
        let count = program.slot_count(ty);
        match &program.program[ty] {
            TypeInfo::Enum { .. } => {
                let first = StackOffset(self.stack_slots);
                self.slot_types.push(TypeId::i64());
                for _ in 1..count {
                    self.slot_types.push(TypeId::i64()); // TODO: more elegant thing for padding.
                }
                self.stack_slots += count;
                Ok(StackRange { first, count })
            }
            _ => self.reserve_slots_raw(program, count, ty),
        }
    }

    #[track_caller]
    fn _load_constant(&mut self, program: &mut EmitBc<'_, 'p>, value: Values, ty: TypeId) -> Res<'p, (StackRange, TypeId)> {
        match value {
            Values::One(value) => {
                let to = self.reserve_slots(program, ty)?;
                self.push(Bc::LoadConstant { slot: to.single(), value });
                Ok((to, ty))
            }
            Values::Many(values) => {
                self.mark_contiguous(program, ty);
                let start = self.stack_slots;
                let count = values.len();
                let res = (
                    StackRange {
                        first: StackOffset(start),
                        count,
                    },
                    ty,
                );
                let ty = program.program.raw_type(ty); // TODO: enums
                if let Some(types) = program.program.flat_types(ty) {
                    if types.len() == values.len() {
                        let types = types.to_vec();
                        for (value, ty) in values.into_iter().zip(types.into_iter()) {
                            let to = self.reserve_slots_inner(program, ty)?;
                            self.push(Bc::LoadConstant { slot: to.single(), value });
                        }
                        return Ok(res);
                    }
                }

                for value in values {
                    let to = self.reserve_slots_inner(program, TypeId::any())?; // TODO: this breaks llvm, now just for enums maybe
                    self.push(Bc::LoadConstant { slot: to.single(), value });
                }
                Ok(res)
            }
        }
    }

    #[track_caller]
    fn push(&mut self, inst: Bc<'p>) -> usize {
        let ip = self.insts.len();
        self.insts.push(inst);

        #[cfg(feature = "some_log")]
        {
            self.debug.push(DebugInfo {
                internal_loc: if cfg!(feature = "trace_errors") {
                    Some(std::panic::Location::caller())
                } else {
                    None
                },
                src_loc: self.last_loc,
                p: Default::default(),
            });
            debug_assert_eq!(self.insts.len(), self.debug.len(), "lost debug info");
        }
        ip
    }
}
