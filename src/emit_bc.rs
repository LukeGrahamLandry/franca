//! Converts simple ASTs into my bytecode-ish format.
//! All comptime execution for a function is finished before it reaches this phase.
//! - Flatten nested expressions to stack operations.
//! - Convert control flow to explicit basic blocks.
//! - Bind non-local return labels.
//! - Reduce variable useage to register sized loads/stores.
//! - Convert large arguments/returns to references and remap signatures to use only register sized types.

#![allow(clippy::wrong_self_convention)]

use codemap::Span;
use interp_derive::InterpSend;
use std::ops::Deref;
use std::ptr::{null, slice_from_raw_parts};

use crate::ast::{CallConv, Expr, FatExpr, FnFlag, FnType, FuncId, FuncImpl, LabelId, Program, Stmt, TypeId, TypeInfo};
use crate::ast::{FatStmt, Flag, Pattern, Var, VarType};
use crate::bc_to_asm::Jitted;
use crate::compiler::{CErr, Compile, ExecStyle, Res};
use crate::export_ffi::BigResult::*;
use crate::logging::PoolLog;
use crate::reflect::BitSet;
use crate::{assert, assert_eq, err, ice, unwrap};
use crate::{bc::*, extend_options, Map, STATS};

struct EmitBc<'z, 'p: 'z> {
    program: &'z Program<'p>,
    asm: &'z Jitted,
    last_loc: Option<Span>,
    locals: Vec<Vec<u16>>,
    var_lookup: Map<Var<'p>, u16>,
    is_flat_call: bool,
}

pub fn emit_bc<'p>(compile: &Compile<'_, 'p>, f: FuncId, when: ExecStyle) -> Res<'p, FnBody<'p>> {
    let mut emit = EmitBc::new(compile.program, &compile.aarch64);

    let body = emit.compile_inner(f, when)?;
    unsafe { STATS.bytecodes += body.blocks.iter().map(|b| b.insts.len()).sum::<usize>() };
    Ok(body)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, InterpSend)]
pub enum ResultLoc {
    PushStack,
    ResAddr,
    Discard,
}
use ResultLoc::*;

pub fn empty_fn_body<'p>(program: &Program<'p>, func: FuncId, when: ExecStyle) -> Res<'p, FnBody<'p>> {
    let f = &program[func];
    Ok(FnBody {
        is_ssa_var: BitSet::empty(),
        var_names: vec![],
        vars: Default::default(),
        when,
        func,
        blocks: vec![],
        name: f.name,
        current_block: BbId(0),
        inlined_return_addr: Default::default(),
        want_log: f.has_tag(Flag::Log_Bc),
        clock: 0,
        signeture: prim_sig(program, f.finished_ty().unwrap(), f.cc.unwrap())?,
    })
}

impl<'z, 'p: 'z> EmitBc<'z, 'p> {
    fn new(program: &'z Program<'p>, asm: &'z Jitted) -> Self {
        Self {
            asm,
            last_loc: None,
            program,
            locals: vec![],
            var_lookup: Default::default(),
            is_flat_call: false,
        }
    }

    fn compile_inner(&mut self, f: FuncId, when: ExecStyle) -> Res<'p, FnBody<'p>> {
        if self.program[f].has_tag(Flag::Log_Ast) {
            println!("{}", self.program[f].log(self.program.pool));
        }

        self.locals.clear();
        self.locals.push(vec![]);
        let mut result = empty_fn_body(self.program, f, when)?;
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
        // TODO: cringe copy-paste
        if self.is_flat_call {
            // (ret_ptr, compiler, arg_ptr, arg_len, ret_len)
            result.pop(2);

            let mut offset = 0;
            for (name, ty, kind) in arguments.into_iter() {
                result.push(Bc::PeekDup(0));
                assert!(kind != VarType::Const, "{:?}", name.map(|v| v.log(self.program.pool)));

                let info = self.program.get_info(ty);
                offset = align_to(offset, info.align_bytes as usize);
                result.inc_ptr_bytes(offset as u16);
                let id = result.save_ssa_var();
                offset += info.stride_bytes as usize;

                self.locals.last_mut().unwrap().push(id);
                if let Some(name) = name {
                    let prev = self.var_lookup.insert(name, id);
                    assert!(prev.is_none(), "overwrite arg? {}", name.log(self.program.pool));
                }
            }
            result.pop(2);
            // Now just ret_ptr is on top of the stack, which matches normal cc.
        } else {
            let mut pushed = if result.signeture.first_arg_is_indirect_return { 1 } else { 0 };
            // reversed because they're on the stack like [0, 1, 2]
            for (name, ty, kind) in arguments.into_iter().rev() {
                // TODO:? probably fine, i jsut set to const in closure capture but then shouldn't be adding to vars below.
                // TODO: the frontend needs to remove the 'const' parts from the ast in DeclVarPattern
                assert!(kind != VarType::Const, "{:?}", name.map(|v| v.log(self.program.pool)));

                let info = self.program.get_info(ty);

                let id = if info.size_slots == 0 {
                    continue;
                } else if info.pass_by_ref {
                    pushed += 1;
                    // TODO: callee make copy if it wants to modify
                    result.save_ssa_var()
                } else {
                    let slots = self.program.slot_count(ty);
                    let id = result.add_var(ty);
                    match slots {
                        0 => {}
                        1 => {
                            let ty = self.program.prim(ty).unwrap();
                            result.addr_var(id);
                            result.push(Bc::StorePost { ty });
                            pushed += 1;
                        }
                        2 => {
                            let types = self.program.flat_tuple_types(ty);
                            let offset_2 = align_to(
                                self.program.get_info(types[0]).stride_bytes as usize,
                                self.program.get_info(types[1]).align_bytes as usize,
                            );
                            result.addr_var(id);
                            result.inc_ptr_bytes(offset_2 as u16);
                            result.push(Bc::StorePost {
                                ty: self.program.prim(types[1]).unwrap(),
                            });
                            result.addr_var(id);
                            result.push(Bc::StorePost {
                                ty: self.program.prim(types[0]).unwrap(),
                            });
                            pushed += 2;
                        }
                        _ => unreachable!(),
                    }
                    id
                };

                self.locals.last_mut().unwrap().push(id);
                if let Some(name) = name {
                    let prev = self.var_lookup.insert(name, id);
                    assert!(prev.is_none(), "overwrite arg? {}", name.log(self.program.pool));
                }
            }

            assert_eq!(pushed, result.signeture.arg_slots);
        }
        Ok(())
    }

    fn compile_for_arg(&mut self, result: &mut FnBody<'p>, arg: &FatExpr<'p>, arity: usize) -> Res<'p, bool> {
        let info = self.program.get_info(arg.ty);
        // If the whole thing is passed in registers, cool, we're done.
        if !info.pass_by_ref {
            // assert_eq!(arity, info.size_slots as usize); // TODO??
            self.compile_expr(result, arg, PushStack, false)?; // TODO: tail if its for a ret of the main function?
            return Ok(false);
        }

        if arity == 1 {
            let id = result.add_var(arg.ty);
            result.addr_var(id);
            self.compile_expr(result, arg, ResAddr, false)?;
            result.addr_var(id);
            return Ok(true);
        }

        if let Some(types) = self.program.tuple_types(arg.ty) {
            debug_assert!(types.len() == self.program.arity(arg) as usize);

            if types.iter().all(|t| !self.program.get_info(*t).pass_by_ref) {
                self.compile_expr(result, arg, PushStack, false)?;
                return Ok(false);
            }

            let mut _pushed = 0;
            if let Expr::Tuple(parts) = &arg.expr {
                debug_assert!(types.len() == parts.len());

                for (&ty, val) in types.iter().zip(parts.iter()) {
                    let info = self.program.get_info(ty);
                    if !info.pass_by_ref {
                        _pushed += info.size_slots;
                        self.compile_expr(result, val, PushStack, false)?;
                        continue;
                    }

                    if let Expr::SuffixMacro(name, macro_arg) = &val.expr {
                        if *name == Flag::Deref.ident() {
                            _pushed += 1;
                            // TODO: this is probably unsound.
                            //       we're assuming that we can defer the load to be done by the callee but that might not be true.
                            self.compile_expr(result, macro_arg, PushStack, false)?;
                            continue;
                        }
                    }

                    if let Expr::Value { value } = &val.expr {
                        // TODO: factor out aot handling from main value handling so can use here too. -- Jun 3
                        if result.when == ExecStyle::Jit {
                            // TODO: this gets super bad if the callee isn't properly copying it because they'll be nmodifying something we think is constant
                            _pushed += 1;
                            result.push(Bc::PushConstant {
                                value: value.bytes().as_ptr() as i64,
                            });
                            continue;
                        }
                    }

                    _pushed += 1;
                    let id = result.add_var(ty);
                    result.addr_var(id);
                    self.compile_expr(result, val, ResAddr, false)?;
                    result.addr_var(id);
                }
                // TODO: this isn't always true because of slices (or any 2 slot struct).
                // assert_eq!(
                //     pushed as usize,
                //     arity,
                //     "arity mismatch. this might be a compiler bug. {:?}",
                //     types.iter().map(|t| self.program.log_type(*t)).collect::<Vec<_>>()
                // );
                return Ok(true);
            }
        }

        todo!("{} {arity}", self.program.log_type(arg.ty))
    }

    fn store_pre(&mut self, result: &mut FnBody<'p>, ty: TypeId) {
        let slots = self.program.slot_count(ty);
        match slots {
            0 => {}
            1 => {
                let ty = self.program.prim(ty).unwrap();
                result.push(Bc::StorePre { ty });
            }
            2 => {
                let types = self.program.flat_tuple_types(ty);
                let offset_2 = align_to(
                    self.program.get_info(types[0]).stride_bytes as usize,
                    self.program.get_info(types[1]).align_bytes as usize,
                );
                result.push(Bc::PeekDup(2)); // grab the pointer
                result.inc_ptr_bytes(offset_2 as u16);
                result.push(Bc::StorePost {
                    ty: self.program.prim(types[1]).unwrap(),
                });
                result.push(Bc::StorePre {
                    ty: self.program.prim(types[0]).unwrap(),
                });
            }
            _ => unreachable!(),
        }
    }

    fn load(&mut self, result: &mut FnBody<'p>, ty: TypeId) {
        let slots = self.program.slot_count(ty);
        match slots {
            0 => {}
            1 => {
                let ty = self.program.prim(ty).unwrap();
                result.push(Bc::Load { ty });
            }
            2 => {
                let types = self.program.flat_tuple_types(ty);
                let offset_2 = align_to(
                    self.program.get_info(types[0]).stride_bytes as usize,
                    self.program.get_info(types[1]).align_bytes as usize,
                );
                result.push(Bc::PeekDup(0));
                result.push(Bc::Load {
                    ty: self.program.prim(types[0]).unwrap(),
                });
                result.push(Bc::PeekDup(1));
                result.inc_ptr_bytes(offset_2 as u16);
                result.push(Bc::Load {
                    ty: self.program.prim(types[1]).unwrap(),
                });
                result.push(Bc::Snipe(2));
            }
            _ => unreachable!(),
        }
    }

    fn emit_body(&mut self, result: &mut FnBody<'p>, f: FuncId) -> Res<'p, ()> {
        let func = &self.program[f];
        let is_flat_call = matches!(func.cc.unwrap(), CallConv::Flat | CallConv::FlatCt);
        self.is_flat_call = is_flat_call;

        let FuncImpl::Normal(body) = &func.body else {
            // You should never actually try to run this code, the caller should have just done the call,
            // so there isn't an extra indirection and I don't have to deal with two bodies for comptime vs runtime,
            // just too ways of emitting the call.
            // result.push(Bc::NoCompile);
            return Ok(());
        };

        let entry_block = result.push_block(result.signeture.arg_slots, result.signeture.arg_float_mask);
        self.bind_args(result, &func.arg)?;
        // We represent the indirect return argument as the left-most thing on the stack,
        // so after popping all the args, its at the top and we can emit the thing normally.
        let result_location = if result.signeture.first_arg_is_indirect_return {
            ResAddr
        } else {
            PushStack
        };
        let return_block = result.push_block(result.signeture.ret_slots, result.signeture.ret_float_mask);

        result.current_block = entry_block;

        // TODO: flat_call tail
        self.compile_expr(result, body, result_location, !is_flat_call)?;

        if result.blocks[return_block.0 as usize].incoming_jumps > 0 {
            result.push(Bc::Goto {
                ip: return_block,
                slots: result.signeture.ret_slots,
            });
            result.blocks[return_block.0 as usize].incoming_jumps += 1;
            result.current_block = return_block;
        } else {
            result.push_to(return_block, Bc::NoCompile);
        }

        self.locals.pop().unwrap();
        assert!(self.locals.is_empty());

        // Note: this is different from the body expr type because of early returns.
        let ret = self.program[f].finished_ret.unwrap();
        if !ret.is_never() {
            let slots = self.program.slot_count(ret);
            let op = if self.is_flat_call {
                Bc::Ret0
            } else {
                match slots {
                    1 => Bc::Ret1(self.program.prim(ret).unwrap()),
                    2 => Bc::Ret2(self.program.prim_pair(ret)?),
                    _ => Bc::Ret0, // void or indirect return
                }
            };

            result.push(op);
        }
        if result.want_log {
            println!("{}", result.log(self.program));
        }

        Ok(())
    }

    fn emit_runtime_call(
        &mut self,
        result: &mut FnBody<'p>,
        f_ty: FnType,
        cc: CallConv,
        arg_expr: &FatExpr<'p>,
        result_location: ResultLoc,
        can_tail: bool,
        do_call: impl FnOnce(&mut FnBody, PrimSig, bool),
    ) -> Res<'p, ()> {
        // TODO: what if it hasnt been compiled to bc yet so hasn't had the tag added yet but will later?  -- May 7
        //       should add test of inferred flatcall mutual recursion.
        let force_flat = matches!(cc, CallConv::Flat | CallConv::FlatCt);
        let sig = prim_sig(self.program, f_ty, cc)?;
        if force_flat {
            // (ret_ptr, compiler, arg_ptr, arg_len, ret_len)

            let id = result.add_var(arg_expr.ty); // Note: this is kinda good because it gets scary if both sides try to avoid the copy and they alias.
            result.addr_var(id);
            self.compile_expr(result, arg_expr, ResAddr, false)?;

            match result_location {
                PushStack => {
                    let ret_id = result.add_var(f_ty.ret);
                    result.addr_var(ret_id);
                    result.push(Bc::GetCompCtx);
                    result.addr_var(id);
                    self.push_flat_lengths(result, f_ty);
                    do_call(result, sig, false);
                    result.addr_var(ret_id);
                    self.load(result, f_ty.ret);
                    result.push(Bc::LastUse { id: ret_id });
                }
                ResAddr => {
                    // res ptr was already on stack
                    result.push(Bc::GetCompCtx);
                    result.addr_var(id);
                    self.push_flat_lengths(result, f_ty);
                    do_call(result, sig, false);
                }
                Discard => {
                    let ret_id = result.add_var(f_ty.ret);
                    result.addr_var(ret_id);
                    result.push(Bc::GetCompCtx);
                    result.addr_var(id);
                    self.push_flat_lengths(result, f_ty);
                    do_call(result, sig, false);
                    result.push(Bc::LastUse { id: ret_id });
                }
            }
            result.push(Bc::LastUse { id });
        } else {
            let result_var = if sig.first_arg_is_indirect_return && result_location != ResAddr {
                let id = result.add_var(f_ty.ret);
                result.addr_var(id);
                Some(id)
            } else {
                None
            };

            if cc == CallConv::CCallRegCt {
                result.push(Bc::GetCompCtx);
            }

            let any_by_ref = self.compile_for_arg(result, arg_expr, f_ty.arity as usize)?;
            // if any args are pointers, they might be to the stack and then you probably can't tail call.
            // TODO: can do better than this without getting too fancy, function pointers are fine, and anything in constant data is fine (we know if the arg is a Values).
            // TODO: !tail to force it when you know its fine.
            let tail = can_tail && !self.program.get_info(f_ty.arg).contains_pointers && !any_by_ref;
            do_call(result, sig, tail);
            let slots = self.slot_count(f_ty.ret);
            if slots > 0 {
                match result_location {
                    PushStack => {
                        if sig.first_arg_is_indirect_return {
                            if let Some(id) = result_var {
                                result.addr_var(id);
                            }
                            self.load(result, f_ty.ret);
                        }
                    }
                    ResAddr => {
                        if !sig.first_arg_is_indirect_return {
                            self.store_pre(result, f_ty.ret);
                        }
                    }
                    Discard => {
                        if !sig.first_arg_is_indirect_return {
                            result.pop(slots)
                        }
                    }
                }
            } else if result_location == ResAddr {
                // pop dest!
                result.pop(1)
            }
        }
        if f_ty.ret.is_never() {
            result.push(Bc::Unreachable);
        }
        Ok(())
    }

    fn compile_stmt(&mut self, result: &mut FnBody<'p>, stmt: &FatStmt<'p>) -> Res<'p, ()> {
        self.last_loc = Some(stmt.loc);
        match stmt.deref() {
            Stmt::Eval(expr) => {
                debug_assert!(!expr.ty.is_unknown());
                self.compile_expr(result, expr, Discard, false)?;
            }
            Stmt::DeclVar { name, ty, value } => {
                assert_ne!(VarType::Const, name.kind);
                let ty = ty.unwrap();

                let id = result.add_var(ty);
                if result.want_log {
                    extend_options(&mut result.var_names, id as usize);
                    result.var_names[id as usize] = Some(*name);
                }
                result.addr_var(id);
                self.compile_expr(result, value, ResAddr, false)?;
                let prev = self.var_lookup.insert(*name, id);
                self.locals.last_mut().unwrap().push(id);
                assert!(prev.is_none(), "shadow is still new var");
            }
            Stmt::Set { place, value } => self.set_deref(result, place, value)?,
            Stmt::DeclVarPattern { binding, value } => {
                // TODO: test for evaluation order
                if let Expr::Tuple(parts) = &value.expr {
                    debug_assert_eq!(parts.len(), binding.bindings.len());
                    for ((name, ty, kind), value) in binding.flatten().into_iter().zip(parts.iter()) {
                        assert!(kind != VarType::Const, "{:?}", name.map(|v| v.log(self.program.pool)));
                        self.do_binding(result, name, ty, value)?;
                    }
                } else {
                    debug_assert_eq!(1, binding.bindings.len());
                    let (name, ty, _) = binding.flatten().into_iter().next().unwrap(); // TODO: sad alloc
                    self.do_binding(result, name, ty, value)?;
                }
            }
            Stmt::Noop => {}
            // Can't hit DoneDeclFunc because we don't re-eval constants.
            Stmt::ExpandParsedStmts(_) | Stmt::DeclNamed { .. } | Stmt::DeclFunc(_) => unreachable!(),
        }
        Ok(())
    }

    fn do_binding(&mut self, result: &mut FnBody<'p>, name: Option<Var<'p>>, ty: TypeId, value: &FatExpr<'p>) -> Res<'p, ()> {
        let id = result.add_var(ty);
        if result.want_log {
            extend_options(&mut result.var_names, id as usize);
            result.var_names[id as usize] = name;
        }
        result.addr_var(id);
        self.compile_expr(result, value, ResAddr, false)?;
        self.locals.last_mut().unwrap().push(id);
        if let Some(name) = name {
            let prev = self.var_lookup.insert(name, id);
            assert!(prev.is_none(), "overwrite arg? {}", name.log(self.program.pool));
        }
        Ok(())
    }

    // if result_location==true, the top of the stack on entry to this function has the pointer where the result should be stored.
    // otherwise, just push it to the stack.
    fn compile_expr(&mut self, result: &mut FnBody<'p>, expr: &FatExpr<'p>, result_location: ResultLoc, can_tail: bool) -> Res<'p, ()> {
        assert!(
            !expr.ty.is_unknown(),
            "Not typechecked: {}. done:{}",
            expr.log(self.program.pool),
            expr.done
        );

        debug_assert!(
            self.slot_count(expr.ty) <= 8 || result_location != PushStack,
            "{} {}",
            expr.log(self.program.pool),
            self.program.log_type(expr.ty)
        );

        // let info = self.program.get_info(expr.ty);
        // debug_assert!(
        //     result_location != ResAddr || info.stride_bytes % 8 == 0 || info.has_special_pointer_fns,
        //     "{} should have special fns",
        //     self.program.log_type(expr.ty)
        // );
        self.last_loc = Some(expr.loc);

        match expr.deref() {
            Expr::String(_)
            | Expr::GetParsed(_)
            | Expr::AddToOverloadSet(_)
            | Expr::GetNamed(_)
            | Expr::WipFunc(_)
            | Expr::FieldAccess(_, _)
            | Expr::Closure(_) => {
                unreachable!("didn't desugar: {}", expr.log(self.program.pool))
            }
            Expr::Poison => ice!("POISON",),
            Expr::Cast(v) => self.compile_expr(result, v, result_location, can_tail)?,
            Expr::Call(f, arg) => {
                assert!(!f.ty.is_unknown(), "Not typechecked: {}", f.log(self.program.pool));
                assert!(!arg.ty.is_unknown(), "Not typechecked: {}", arg.log(self.program.pool));
                if let TypeInfo::Fn(_) = self.program[f.ty] {
                    let mut f_id = unwrap!(f.as_const(), "tried to call non-const fn").unwrap_func_id();
                    let func = &self.program[f_id];
                    assert!(!func.get_flag(FnFlag::Generic));
                    assert!(func.capture_vars.is_empty());
                    assert!(
                        func.cc != Some(CallConv::Inline),
                        "tried to call inlined {}",
                        self.program.pool.get(func.name)
                    );

                    // TODO: ideally the redirect should just be stored in the overloadset so you don't have to have the big Func thing every time.
                    let f_ty = self.program[f_id].finished_ty().unwrap(); // kinda HACK to fix unaligned store?
                    while let FuncImpl::Redirect(target) = self.program[f_id].body {
                        f_id = target;
                    }
                    let cc = self.program[f_id].cc.unwrap();

                    let mut can_tail = can_tail;
                    if func.has_tag(Flag::No_Tail) {
                        can_tail = false;
                    }
                    return self.emit_runtime_call(result, f_ty, cc, arg, result_location, can_tail, |r, sig, tail| {
                        r.push(Bc::CallDirect { f: f_id, sig, tail })
                    });
                }
                if let TypeInfo::FnPtr { ty: f_ty, cc } = self.program[f.ty] {
                    self.compile_expr(result, f, PushStack, false)?;
                    let will_use_indirect_ret = cc == CallConv::Flat || cc == CallConv::CCallRegCt || self.program.slot_count(f_ty.ret) > 2;
                    if result_location == ResAddr && will_use_indirect_ret {
                        // grab the result pointer to the top of the stack so the layout matches a normal call.
                        // however, if the function wants to push stack but we want it to a resaddr, we don't do this here because emit_runtime_call handles it which is kinda HACK.
                        result.push(Bc::PeekDup(1));
                    }
                    self.emit_runtime_call(result, f_ty, cc, arg, result_location, can_tail, |r, sig, _| {
                        r.push(Bc::CallFnPtr { sig })
                    })?;
                    if result_location == ResAddr && will_use_indirect_ret {
                        result.push(Bc::Snipe(0)); // original ret ptr
                    }
                    return Ok(());
                }

                if let TypeInfo::Label(ret_ty) = self.program[f.ty] {
                    let return_from: LabelId = from_values(self.program, unwrap!(f.as_const(), ""))?;
                    // result_location is the result of the ret() expression, which is Never and we don't care.
                    let (ip, res_loc) = *unwrap!(
                        result.inlined_return_addr.get(&return_from),
                        "missing return label. forgot '=>' on function?"
                    );
                    let slots = match res_loc {
                        PushStack => self.slot_count(ret_ty),
                        ResAddr => {
                            ice!("TODO: need be be able to find the ResAddr cause it might not be on top of the stack. (early return from flat_call: big ret/arg value )")
                        }
                        Discard => 0,
                    };
                    // TODO: sometimes can_tail, if you're returning the main function
                    self.compile_expr(result, arg, res_loc, false)?;
                    result.push(Bc::Goto { ip, slots });
                    result.blocks[ip.0 as usize].incoming_jumps += 1;
                    return Ok(());
                }

                unreachable!("{}", f.log(self.program.pool))
            }
            Expr::Block {
                body,
                result: value,
                ret_label,
                ..
            } => {
                self.locals.push(vec![]);
                let out = self.program.get_info(expr.ty);
                debug_assert!(out.size_slots < 8 || result_location != PushStack);

                if let Some(ret_var) = ret_label {
                    let entry_block = result.current_block;
                    let return_block = if result_location == PushStack {
                        result.push_block(out.size_slots, out.float_mask)
                    } else {
                        result.push_block(0, 0)
                    };
                    let prev = result.inlined_return_addr.insert(*ret_var, (return_block, result_location));
                    assert!(prev.is_none());
                    result.current_block = entry_block;

                    for stmt in body {
                        self.compile_stmt(result, stmt)?;
                    }
                    self.compile_expr(result, value, result_location, can_tail)?;

                    if result.blocks[return_block.0 as usize].incoming_jumps > 0 {
                        let slots = result.blocks[return_block.0 as usize].arg_slots;
                        result.push(Bc::Goto { ip: return_block, slots });
                        result.blocks[return_block.0 as usize].incoming_jumps += 1;
                        result.current_block = return_block;
                    } else {
                        result.push_to(return_block, Bc::NoCompile);
                    }
                    result.inlined_return_addr.remove(ret_var);
                } else {
                    // TODO: sometimes the last one can tail, if value is Unit and we're the body of the main function.
                    for stmt in body {
                        self.compile_stmt(result, stmt)?;
                    }

                    self.compile_expr(result, value, result_location, can_tail)?;
                }

                // TODO: check if you try to let an address to a variable escape from its scope.
                let slots = self.locals.pop().unwrap();
                for id in slots {
                    result.push(Bc::LastUse { id });
                }
            }
            Expr::Tuple(values) => {
                debug_assert!(values.len() > 1, "no trivial tuples: {:?}", values);
                let TypeInfo::Struct { fields, layout_done, .. } = &self.program[self.program.raw_type(expr.ty)] else {
                    err!("Expr::Tuple should have struct type",)
                };
                assert!(*layout_done);
                for (value, f) in values.iter().zip(fields.iter()) {
                    if result_location == ResAddr {
                        result.push(Bc::PeekDup(0));
                        result.inc_ptr_bytes(f.byte_offset as u16);
                    }
                    self.compile_expr(result, value, result_location, false)?;
                }

                if result_location == ResAddr {
                    result.pop(1)
                }
            }
            Expr::GetVar(var) => {
                err!(
                    "var loads should be converted to addrs so smaller loads happen properly. {}",
                    var.log(self.program.pool)
                )
            }
            Expr::Value { value } => {
                if result_location == Discard {
                    return Ok(());
                }

                let want_emit_by_memcpy = value.bytes().len() > 16;
                if result.when == ExecStyle::Aot && (self.program.get_info(expr.ty).contains_pointers || want_emit_by_memcpy) {
                    // TODO: this is dumb because a slice becomes a pointer to a slice.
                    let id = emit_relocatable_constant(expr.ty, value, self.program, &self.asm.dispatch)?;
                    result.push(Bc::PushGlobalAddr { id });
                    let info = self.program.get_info(expr.ty);
                    match result_location {
                        PushStack => self.load(result, expr.ty), // TODO: non %8
                        ResAddr => result.push(Bc::CopyBytesToFrom { bytes: info.stride_bytes }),
                        Discard => unreachable!(),
                    }
                    return Ok(());
                }

                // TODO
                // debug_assert_eq!(
                //     self.program.get_info(expr.ty).stride_bytes as usize,
                //     value.0.len(),
                //     "{:?} is {}",
                //     value.0,
                //     self.program.log_type(expr.ty)
                // );
                match result_location {
                    PushStack => {
                        let mut parts = vec![];
                        deconstruct_values(
                            self.program,
                            expr.ty,
                            &mut ReadBytes { bytes: value.bytes(), i: 0 },
                            &mut parts,
                            &mut None,
                        )?;
                        for value in parts {
                            result.push(Bc::PushConstant { value });
                        }
                    }
                    ResAddr => {
                        if want_emit_by_memcpy {
                            // TODO: i really need to not do this for the constant 'true'!
                            // TODO: HACK
                            //       for a constant ast node, you need to load an enum but my deconstruct_values can't handle it.
                            //       this solution is extra bad becuase it relies on the value vec not being free-ed
                            // TODO: need to leak when small by value?
                            let ptr = value.bytes().as_ptr();
                            result.push(Bc::PushConstant { value: ptr as i64 });

                            result.push(Bc::CopyBytesToFrom {
                                bytes: self.program.get_info(expr.ty).stride_bytes, // Note: not the same as value.len!!!!!
                            });
                        } else {
                            let mut parts = vec![];
                            let mut offsets = vec![];
                            deconstruct_values(
                                self.program,
                                expr.ty,
                                &mut ReadBytes { bytes: value.bytes(), i: 0 },
                                &mut parts,
                                &mut Some(&mut offsets),
                            )?;
                            debug_assert_eq!(parts.len(), offsets.len());
                            for (value, (ty, offset)) in parts.into_iter().zip(offsets.into_iter()) {
                                result.push(Bc::PeekDup(0));
                                result.inc_ptr_bytes(offset);
                                result.push(Bc::PushConstant { value });
                                result.push(Bc::StorePre { ty });
                            }
                            result.pop(1) // res ptr
                        }
                    }
                    Discard => {}
                }
            }
            Expr::SuffixMacro(macro_name, arg) => {
                let Ok(name) = Flag::try_from(*macro_name) else {
                    // TODO: this will change when you're able to define !bang macros in the language.
                    err!(CErr::UndeclaredIdent(*macro_name))
                };
                match name {
                    Flag::If => self.emit_call_if(result, arg, result_location, can_tail)?,
                    Flag::Loop => {
                        debug_assert_eq!(result_location, Discard);
                        self.emit_call_loop(result, arg)?;
                    }
                    Flag::Addr => self.addr_macro(result, arg, result_location)?,
                    Flag::Quote => unreachable!(),
                    Flag::Slice => {
                        let container_ty = arg.ty;
                        // Note: number of elements, not size of the whole array value.
                        let ty = self.program.tuple_types(container_ty);
                        let count = if let Some(types) = ty { types.len() } else { 1 };

                        let id = result.add_var(container_ty);
                        result.addr_var(id);
                        self.compile_expr(result, arg, ResAddr, false)?;

                        result.addr_var(id);
                        result.push(Bc::PushConstant { value: count as i64 });
                        match result_location {
                            PushStack => {}
                            ResAddr => {
                                result.push(Bc::PeekDup(2));
                                result.inc_ptr_bytes(8); // Note: backwards!
                                result.push(Bc::StorePost { ty: Prim::I64 });
                                result.push(Bc::PeekDup(1));
                                result.push(Bc::StorePost { ty: Prim::I64 });
                                result.pop(1)
                            }
                            Discard => result.pop(2),
                        }
                        self.locals.last_mut().unwrap().push(id);
                    }
                    Flag::Deref => {
                        self.compile_expr(result, arg, PushStack, false)?; // get the pointer
                        let slots = self.slot_count(expr.ty);
                        // we care about the type of the pointer, not the value because there might be a cast. (// TODO: that shouldn't be true anymore because of ::Cast)
                        let value_type = self.program.unptr_ty(arg.ty).unwrap();
                        if slots == 0 {
                            match result_location {
                                ResAddr => {
                                    // pop dest too!
                                    result.pop(2)
                                }
                                PushStack | Discard => result.push(Bc::Snipe(0)),
                            };
                        } else {
                            match result_location {
                                PushStack => self.load(result, value_type),
                                ResAddr => {
                                    let info = self.program.get_info(expr.ty);

                                    if info.size_slots == 1 {
                                        let ty = self.program.prim(value_type).unwrap();
                                        result.push(Bc::Load { ty });
                                        result.push(Bc::StorePre { ty });
                                    } else if info.size_slots == 2 && info.stride_bytes == 16 && info.align_bytes == 8 {
                                        result.push(Bc::PeekDup(1));
                                        result.inc_ptr_bytes(8);
                                        result.push(Bc::PeekDup(1));
                                        result.inc_ptr_bytes(8);
                                        result.push(Bc::Load { ty: Prim::I64 });
                                        result.push(Bc::StorePre { ty: Prim::I64 });
                                        result.push(Bc::Load { ty: Prim::I64 });
                                        result.push(Bc::StorePre { ty: Prim::I64 });
                                    } else {
                                        let bytes = info.stride_bytes;
                                        result.push(Bc::CopyBytesToFrom { bytes });
                                    }
                                }
                                Discard => result.push(Bc::Snipe(0)),
                            };
                        }
                    }
                    Flag::Tag => {
                        debug_assert_eq!(self.program[expr.ty], TypeInfo::Ptr(TypeId::i64()));
                        self.compile_expr(result, arg, result_location, can_tail)?;
                    }
                    Flag::Fn_Ptr => {
                        debug_assert!(matches!(self.program[arg.ty], TypeInfo::Fn(_)));
                        let mut f = unwrap!(arg.as_const(), "expected fn for ptr").unwrap_func_id();
                        // For jit, the redirects go in the dispatch table so it doesn't matter,
                        // but for emitting c, you need to get the real name.
                        while let FuncImpl::Redirect(target) = self.program[f].body {
                            f = target;
                        }
                        result.push(Bc::GetNativeFnPtr(f));
                        match result_location {
                            PushStack => {}
                            ResAddr => result.push(Bc::StorePre { ty: Prim::I64 }),
                            Discard => result.push(Bc::Snipe(0)),
                        };
                    }
                    Flag::Unreachable => {
                        // Don't care about setting output to anything.
                        // TODO: but it does need to leave the v-stack with the expected amount of stuff or asm gest confused. -- May 2
                        result.push(Bc::Unreachable);
                    }
                    Flag::Uninitialized => {
                        assert!(!expr.ty.is_never(), "call exit() to produce a value of type 'Never'");
                        // Wierd special case I have mixed feelings about. should at least set to sentinal value in debug mode.
                        // Now I have to not mess up the stack, tell the backend somehow.
                        match result_location {
                            PushStack => {
                                let slots = self.slot_count(expr.ty);
                                for _ in 0..slots {
                                    result.push(Bc::PushConstant { value: 0 });
                                }
                            }
                            ResAddr => result.push(Bc::Snipe(0)), // just pop the res ptr
                            Discard => {}
                        };
                        return Ok(());
                    }
                    Flag::Return => ice!("!return is const only",),
                    name => err!("{name:?} is known flag but not builtin macro",),
                }
            }
            Expr::PtrOffset { ptr, bytes, .. } => {
                // TODO: compiler has to emit tagchecks for enums now!!
                self.compile_expr(result, ptr, PushStack, false)?;

                result.inc_ptr_bytes(*bytes as u16);
                match result_location {
                    PushStack => {}
                    ResAddr => result.push(Bc::StorePre { ty: Prim::I64 }),
                    Discard => result.push(Bc::Snipe(0)),
                }
            }
            Expr::StructLiteralP(pattern) => self.construct_struct(result, pattern, expr.ty, result_location)?,
            Expr::PrefixMacro { .. } => {
                unreachable!("unhandled macro {}", expr.log(self.program.pool));
            }
        };
        Ok(())
    }

    // :PlaceExpr
    fn addr_macro(&mut self, result: &mut FnBody<'p>, arg: &FatExpr<'p>, result_location: ResultLoc) -> Res<'p, ()> {
        self.last_loc = Some(arg.loc);
        let Expr::GetVar(var) = arg.deref() else {
            // field accesses should have been desugared.
            err!("took address of r-value",)
        };
        // TODO: this shouldn't allow let either but i changed how variable refs work for :SmallTypes
        assert_ne!(
            var.kind,
            VarType::Const,
            "Can only take address of var (not let/const) {}",
            var.log(self.program.pool)
        );
        let id = *unwrap!(self.var_lookup.get(var), "Missing var {} (in !addr)", var.log(self.program.pool));
        result.addr_var(id);

        match result_location {
            PushStack => {}
            ResAddr => result.push(Bc::StorePre { ty: Prim::I64 }),
            Discard => result.push(Bc::Snipe(0)),
        }

        Ok(())
    }

    // we never make the temp variable. if the arg is big, caller needs to setup a result location.
    fn emit_call_if(&mut self, result: &mut FnBody<'p>, arg: &FatExpr<'p>, result_location: ResultLoc, can_tail: bool) -> Res<'p, ()> {
        let Expr::Tuple(parts) = &arg.expr else {
            ice!("if args must be tuple not {:?}", arg);
        };
        self.compile_expr(result, &parts[0], PushStack, false)?; // cond
        let (if_true, if_false) = (&parts[1], &parts[2]);

        let out = self.program.get_info(if_true.ty);
        debug_assert!(out.size_slots < 8 || result_location != PushStack); // now its the callers problem to deal with this case

        let branch_block = result.current_block;
        let true_ip = result.push_block(0, 0);
        self.compile_expr(result, if_true, result_location, can_tail)?;
        let end_true_block = result.current_block;
        let false_ip = result.push_block(0, 0);
        self.compile_expr(result, if_false, result_location, can_tail)?;
        let end_false_block = result.current_block;

        let block_slots = if result_location == PushStack { out.size_slots } else { 0 };
        let block_floats = if result_location == PushStack { out.float_mask } else { 0 };
        let ip = result.push_block(block_slots, block_floats);
        result.push_to(branch_block, Bc::JumpIf { true_ip, false_ip, slots: 0 });
        result.push_to(end_true_block, Bc::Goto { ip, slots: block_slots });
        // TODO: hack
        if !if_false.ty.is_never() {
            result.push_to(end_false_block, Bc::Goto { ip, slots: block_slots });
            result.blocks[ip.0 as usize].incoming_jumps += 2;
        } else {
            result.push_to(end_false_block, Bc::Unreachable);
            result.blocks[ip.0 as usize].incoming_jumps += 1;
        }
        result.blocks[true_ip.0 as usize].incoming_jumps += 1;
        result.blocks[false_ip.0 as usize].incoming_jumps += 1;
        result.bump_clock(ip);

        Ok(())
    }

    fn emit_call_loop(&mut self, result: &mut FnBody<'p>, arg: &FatExpr<'p>) -> Res<'p, ()> {
        debug_assert_eq!(arg.ty, TypeId::unit);

        let prev_block = result.current_block;
        let start_body_block = result.push_block(0, 0);
        result.current_block = start_body_block;
        result.push_to(
            prev_block,
            Bc::Goto {
                ip: start_body_block,
                slots: 0,
            },
        );

        self.compile_expr(result, arg, Discard, false)?;
        let end_body_block = result.current_block;

        result.push_to(
            end_body_block,
            Bc::Goto {
                ip: start_body_block,
                slots: 0,
            },
        );
        result.blocks[start_body_block.0 as usize].incoming_jumps += 2;
        Ok(())
    }

    // :PlaceExpr
    fn set_deref(&mut self, result: &mut FnBody<'p>, place: &FatExpr<'p>, value: &FatExpr<'p>) -> Res<'p, ()> {
        // we care about the type of the pointer, not the value because there might be a cast.
        match place.deref() {
            Expr::GetVar(_) => unreachable!("var set should be converted to place expr"),
            Expr::SuffixMacro(macro_name, arg) => {
                // TODO: write a test for pooiinter eval oreder. left hsould come first. -- MAy 7
                if let Ok(Flag::Deref) = Flag::try_from(*macro_name) {
                    self.compile_expr(result, arg, PushStack, false)?;
                    self.compile_expr(result, value, ResAddr, false)?;
                    return Ok(());
                }
                todo!()
            }
            &Expr::GetNamed(n) => err!(CErr::UndeclaredIdent(n)),
            _ => ice!("TODO: other `place=e;` :("),
        }
    }

    fn slot_count(&self, ty: TypeId) -> u16 {
        self.program.slot_count(ty)
    }

    fn construct_struct(&mut self, result: &mut FnBody<'p>, pattern: &Pattern<'p>, requested: TypeId, result_location: ResultLoc) -> Res<'p, ()> {
        let names: Vec<_> = pattern.flatten_names();
        // TODO: why must this suck so bad
        let values: Option<_> = pattern.flatten_defaults_ref();
        let values: Vec<_> = values.unwrap();
        assert_eq!(names.len(), values.len());
        let raw_container_ty = self.program.raw_type(requested);
        let slots = self.slot_count(raw_container_ty);
        debug_assert!(slots < 8 || result_location != PushStack);

        match &self.program[raw_container_ty] {
            TypeInfo::Struct { fields, .. } => {
                assert_eq!(
                    fields.len(),
                    values.len(),
                    "Cannot assign {values:?} to type {} = {fields:?}",
                    self.program.log_type(requested)
                );
                let all = names.into_iter().zip(values).zip(fields);
                for ((name, value), field) in all {
                    assert_eq!(name, field.name);
                    if result_location == ResAddr {
                        result.push(Bc::PeekDup(0));
                        result.push(Bc::IncPtrBytes {
                            bytes: field.byte_offset as u16,
                        });
                    }
                    self.compile_expr(result, value, result_location, false)?;
                }

                if result_location == ResAddr {
                    result.pop(1) // res ptr
                }
            }
            // TODO: make this constexpr in compiler
            TypeInfo::Tagged { cases } => {
                debug_assert!(result_location != Discard, "todo");

                let size = self.slot_count(raw_container_ty);
                assert_eq!(
                    1,
                    values.len(),
                    "{} is @tagged, value should have one active varient not {values:?}",
                    self.program.log_type(requested)
                );
                let i = cases.iter().position(|f| f.0 == names[0]).unwrap();
                let payload_size = self.slot_count(cases[i].1);
                if payload_size >= size {
                    ice!("Enum value won't fit.")
                }
                match result_location {
                    PushStack => {
                        result.push(Bc::PushConstant { value: i as i64 });
                        self.compile_expr(result, values[0], result_location, false)?;
                        // If this is a smaller varient, pad out the slot.
                        for _ in (payload_size + 1)..size {
                            result.push(Bc::PushConstant { value: 0 });
                        }
                    }
                    ResAddr => {
                        result.push(Bc::PeekDup(0));
                        result.push(Bc::PushConstant { value: i as i64 });
                        result.push(Bc::StorePre { ty: Prim::I64 });
                        result.inc_ptr_bytes(8); // TODO: differetn sizes of tag
                        self.compile_expr(result, values[0], result_location, false)?;
                    }
                    Discard => {
                        self.compile_expr(result, values[0], result_location, false)?;
                    }
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

    fn push_flat_lengths(&self, result: &mut FnBody, f_ty: crate::ast::FnType) {
        let (arg, ret) = self.program.get_infos(f_ty);
        result.push(Bc::PushConstant {
            value: arg.stride_bytes as i64,
        });
        result.push(Bc::PushConstant {
            value: ret.stride_bytes as i64,
        });
    }
}

// TODO: i should have constant pointers as a concept in my language.
//       right now i could check if its in the constant data arena for a hint that should catch a few of them.
fn emit_relocatable_constant<'p>(ty: TypeId, value: &Values, program: &Program<'p>, dispatch: &[*const u8]) -> Res<'p, BakedVarId> {
    let raw = program.raw_type(ty);
    let jit_ptr = value.bytes().as_ptr();

    // Eventually we'll recurse to something with no pointers. ie Str -> [u8; n]
    if !program.get_info(raw).contains_pointers {
        return Ok(program.baked.make(BakedVar::Bytes(value.bytes().to_vec()), jit_ptr));
    }

    match &program[raw] {
        TypeInfo::FnPtr { ty, .. } => {
            // TODO: this is sad and slow.
            let want: i64 = from_values(program, value.clone())?;
            for (i, check) in dispatch.iter().enumerate() {
                if *check as i64 == want {
                    let f = FuncId::from_index(i);
                    assert_eq!(*ty, program[f].finished_ty().unwrap());
                    return Ok(program.baked.make(BakedVar::FnPtr(f), jit_ptr));
                }
            }
            err!("function not found",)
        }
        &TypeInfo::Ptr(inner) => {
            let inner_info = program.get_info(inner);
            let bytes = inner_info.stride_bytes as usize;

            // TODO: CStr needs special handling...

            // load the pointer and recurse.
            // TODO: deduplicate!
            let ptr: i64 = from_values(program, value.clone())?;
            let data = unsafe { &*slice_from_raw_parts(ptr as *const u8, bytes) };
            let inner_value = Values::many(data.to_vec());
            let value = emit_relocatable_constant(inner, &inner_value, program, dispatch)?;
            Ok(program.baked.make(BakedVar::AddrOf(value), jit_ptr))
        }
        TypeInfo::Struct { fields, .. } => {
            if fields.len() == 2 && fields[0].name == Flag::Ptr.ident() && fields[1].name == Flag::Len.ident() {
                // TODO: actually construct the slice type from unptr_ty(ptr) and check that its the same.
                // TODO: really you want to let types overload a function to do this,
                //       because List doesn't really want to keep its uninitialized memory.
                let (ptr, len): (i64, i64) = from_values(program, value.clone())?;

                let len = len as usize;
                if len == 0 {
                    // an empty slice can have a null data ptr i guess.
                    let ptr = program.baked.make(BakedVar::Num(0), null());
                    let len = program.baked.make(BakedVar::Num(0), null());
                    return Ok(program.baked.make(BakedVar::VoidPtrArray(vec![ptr, len]), jit_ptr));
                }

                let inner = program.unptr_ty(fields[0].ty).unwrap();
                let inner_info = program.get_info(inner);
                assert_eq!(ptr % inner_info.align_bytes as i64, 0, "miss-aligned constant pointer. ");
                let bytes = len * inner_info.stride_bytes as usize;
                let data = unsafe { &*slice_from_raw_parts(ptr as *const u8, bytes) }.to_vec();

                let value = if inner_info.contains_pointers {
                    let mut ptrs = vec![];

                    let stride = inner_info.stride_bytes as usize;
                    for i in 0..len {
                        let v = data[i * stride..(i + 1) * stride].to_vec();
                        let id = emit_relocatable_constant(inner, &Values::many(v), program, dispatch)?;
                        match program.baked.get(id).0 {
                            BakedVar::Zeros { .. } | BakedVar::Bytes(_) => todo!(),
                            BakedVar::Num(_) => ptrs.push(id),
                            BakedVar::FnPtr(_) => ptrs.push(id),
                            BakedVar::AddrOf(_) => ptrs.push(id),
                            BakedVar::VoidPtrArray(parts) => ptrs.extend(parts),
                        }
                    }

                    program.baked.make(BakedVar::VoidPtrArray(ptrs), null())
                } else {
                    program.baked.make(BakedVar::Bytes(data.to_vec()), null())
                };

                let addr = program.baked.make(BakedVar::AddrOf(value), null());
                let len = program.baked.make(BakedVar::Num(len as i64), null());
                Ok(program.baked.make(BakedVar::VoidPtrArray(vec![addr, len]), jit_ptr))
            } else if ty == program.save_cstr_t.unwrap() {
                assert_eq!(value.bytes().len(), 8);
                let ptr: i64 = from_values(program, value.clone())?;
                let mut ptr = ptr as *const u8;
                let mut bytes = vec![];
                unsafe {
                    while *ptr != 0 {
                        bytes.push(*ptr);
                        ptr = ptr.offset(1);
                    }
                }
                bytes.push(0);

                let value = program.baked.make(BakedVar::Bytes(bytes), null());
                let addr = program.baked.make(BakedVar::AddrOf(value), jit_ptr);
                Ok(addr)
            } else {
                if fields.iter().all(|f| program.get_info(f.ty).stride_bytes == 8) {
                    let mut ptrs = vec![];
                    for (i, f) in fields.iter().enumerate() {
                        let v = value.bytes()[i * 8..(i + 1) * 8].to_vec();
                        ptrs.push(emit_relocatable_constant(f.ty, &Values::many(v), program, dispatch)?)
                    }
                    return Ok(program.baked.make(BakedVar::VoidPtrArray(ptrs), jit_ptr));
                }
                todo!("{}", program.log_type(raw))
            }
        }
        TypeInfo::Tagged { .. } => err!("TODO: pointers in constant tagged union",),
        TypeInfo::Enum { raw, .. } => emit_relocatable_constant(*raw, value, program, dispatch),
        TypeInfo::VoidPtr => {
            if value.bytes().iter().all(|b| *b == 0) {
                // You're allowed to have a constant null pointer (like for global allocator interface instances).
                Ok(program.baked.make(BakedVar::Num(0), null()))
            } else {
                err!("You can't have a void pointer as a constant. The compiler can't tell how many bytes to put in the final executable.",)
            }
        }
        _ => err!("ICE: bad constant",),
    }
}

impl<'p> FnBody<'p> {
    fn push_block(&mut self, arg_slots: u16, arg_float_mask: u32) -> BbId {
        self.blocks.push(BasicBlock {
            insts: vec![],
            arg_slots,
            arg_float_mask,
            incoming_jumps: 0,
            clock: self.clock,
            height: arg_slots,
        });
        let b = BbId(self.blocks.len() as u16 - 1);
        self.current_block = b;
        b
    }

    fn bump_clock(&mut self, b: BbId) {
        self.clock += 1;
        self.blocks[b.0 as usize].clock = self.clock;
    }

    #[track_caller]
    fn push(&mut self, inst: Bc) {
        self.push_to(self.current_block, inst);
    }

    fn addr_var(&mut self, id: u16) {
        if self.is_ssa_var.get(id as usize) {
            self.push(Bc::LoadSsa { id })
        } else {
            self.push(Bc::AddrVar { id })
        }
    }

    fn save_ssa_var(&mut self) -> u16 {
        let id = self.add_var(TypeId::i64());
        self.is_ssa_var.set(id as usize);
        self.push(Bc::SaveSsa { id, ty: Prim::I64 });
        id
    }

    fn inc_ptr_bytes(&mut self, bytes: u16) {
        if bytes != 0 {
            self.push(Bc::IncPtrBytes { bytes })
        }
    }

    #[track_caller]
    fn push_to(&mut self, b: BbId, inst: Bc) {
        self.blocks[b.0 as usize].insts.push(inst);
    }

    pub(crate) fn pop(&mut self, slots: u16) {
        for _ in 0..slots {
            self.push(Bc::Snipe(0));
        }
    }
}

pub fn prim_sig<'p>(program: &Program<'p>, f_ty: FnType, cc: CallConv) -> Res<'p, PrimSig> {
    if matches!(cc, CallConv::Flat | CallConv::FlatCt) {
        return Ok(PrimSig {
            arg_slots: 5,
            arg_float_mask: 0,
            ret_slots: 0,
            ret_float_mask: 0,
            first_arg_is_indirect_return: true, // really it does have indirect ret but we don't want to pass the first arg in x8
            use_special_register_for_indirect_return: false,
            no_return: f_ty.ret.is_never(),
        });
    }

    let has_indirect_ret = program.get_info(f_ty.ret).size_slots > 2;

    let (arg, ret) = program.get_infos(f_ty);
    let mut sig = PrimSig {
        arg_slots: arg.size_slots,
        arg_float_mask: arg.float_mask,
        ret_slots: ret.size_slots,
        ret_float_mask: ret.float_mask,
        first_arg_is_indirect_return: has_indirect_ret,
        use_special_register_for_indirect_return: has_indirect_ret,
        no_return: f_ty.ret.is_never(),
    };

    if has_indirect_ret {
        sig.arg_slots += 1;
        sig.ret_slots = 0;
        sig.ret_float_mask = 0;
    }

    if matches!(cc, CallConv::CCallRegCt) {
        sig.arg_slots += 1;
    }

    // sad copy paste from compile_for_arg
    if arg.pass_by_ref {
        if f_ty.arity == 1 {
            sig.arg_slots -= program.get_info(f_ty.arg).size_slots;
            sig.arg_slots += 1;
        } else if let Some(types) = program.tuple_types(f_ty.arg) {
            if !types.iter().all(|t| !program.get_info(*t).pass_by_ref) {
                for ty in types {
                    let info = program.get_info(ty);
                    if info.pass_by_ref {
                        sig.arg_slots -= info.size_slots;
                        sig.arg_slots += 1;
                        debug_assert!(
                            sig.arg_float_mask == 0,
                            "TODO: shift float mask correctly to account for passing by reference"
                        );
                    }
                }
            }
        } else {
            // if arity == 1 ??
            sig.arg_slots = 1;
        }
    }

    Ok(sig)
}
