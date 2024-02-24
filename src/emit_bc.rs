//! Converts simple ASTs into my bytecode-ish format.

#![allow(clippy::wrong_self_convention)]
use codemap::Span;
use std::marker::PhantomData;
use std::ops::Deref;
use std::panic::Location;

use crate::ast::{FatStmt, VarType};
use crate::bc::*;
use crate::compiler::{CErr, FnWip, Res};
use crate::interp::Interp;
use crate::logging::PoolLog;
use crate::{
    ast::{Expr, FatExpr, FuncId, Program, Stmt, TypeId, TypeInfo},
    pool::{Ident, StringPool},
};

use crate::logging::{assert, assert_eq, err, ice, unwrap};

#[derive(Debug, Clone)]
pub struct DebugInfo<'p> {
    pub internal_loc: &'static Location<'static>,
    pub src_loc: Span,
    pub p: PhantomData<&'p str>,
}

pub struct EmitBc<'b, 'a, 'p> {
    pub pool: &'a StringPool<'p>,
    pub interp: &'b mut Interp<'a, 'p>,
    last_loc: Option<Span>,
}

impl<'b, 'a, 'p> EmitBc<'b, 'a, 'p> {
    pub fn new(pool: &'a StringPool<'p>, interp: &'b mut Interp<'a, 'p>) -> Self {
        Self {
            pool,
            last_loc: None,
            interp,
        }
    }

    #[track_caller]
    fn empty_fn(&mut self, func: &FnWip<'p>) -> FnBody<'p> {
        FnBody {
            stack_slots: 0,
            vars: Default::default(),
            when: func.when,
            func: func.func,
            why: func.why.clone(),
            last_loc: func.last_loc,
            constants: func.constants.clone(),
            insts: vec![],
            debug: vec![],
            slot_types: vec![],
            to_drop: vec![],
        }
    }

    pub fn compile(&mut self, f: FuncId) -> Res<'p, ()> {
        if self.interp.ready[f.0].is_some() {
            return Ok(());
        }
        let func = &self.interp.program.funcs[f.0];
        let wip = unwrap!(func.wip.clone(), "Not done comptime for {f:?}"); // TODO
        debug_assert!(!func.evil_uninit);
        let mut result = self.empty_fn(&wip);
        let func = &self.interp.program.funcs[f.0];
        let arg_range = result.reserve_slots(self.interp.program, func.unwrap_ty().arg)?;
        let return_value = self.emit_body(&mut result, arg_range, f);
        match return_value {
            Ok(return_value) => {
                let return_value = result.load(self.interp.program, return_value)?.0;
                result.push(Bc::Ret(return_value));
                self.interp.ready[f.0] = Some(result);
                Ok(())
            }
            Err(mut e) => {
                e.loc = self.last_loc;
                Err(e)
            }
        }
    }

    fn emit_body(
        &mut self,
        result: &mut FnBody<'p>,
        full_arg_range: StackRange,
        f: FuncId,
    ) -> Res<'p, Structured> {
        let func = &self.interp.program.funcs[f.0];
        let has_body = func.body.is_some();

        let mut args_to_drop = vec![];
        let arguments = func.arg.flatten();
        let mut slot_count = 0;
        for (name, ty) in arguments {
            let size = self.interp.program.slot_count(ty);
            let range = StackRange {
                first: full_arg_range.offset(slot_count),
                count: size,
            };
            if let Some(name) = name {
                let prev = result.vars.insert(name, (range, ty));
                assert!(prev.is_none(), "overwrite arg?");
            }
            args_to_drop.push((name, range));
            slot_count += size;
        }
        assert_eq!(full_arg_range.count, slot_count);

        if !has_body {
            // Functions without a body are always builtins.
            // It's convient to give them a FuncId so you can put them in a variable,
            // but just force inline call.
            let ret_ty = func.ret.unwrap();
            let ret = result.reserve_slots(self.interp.program, func.ret.unwrap())?;
            // TODO: this check is what prevents making types comptime only work because you need to pass a type to builtin alloc,
            //       but specializing kills the name. But before that will work anyway i need tonot blindly pass on the shim args to the builtin
            //       since the shim might be specialized so some args are in constants instead of at the base of the stack.
            assert!(func.referencable_name, "fn no body needs name");
            result.push(Bc::CallBuiltin {
                name: func.name,
                ret,
                arg: full_arg_range,
            });
            for (var, range) in args_to_drop {
                if let Some(var) = var {
                    let (slot, _) = unwrap!(result.vars.remove(&var), "lost arg");
                    assert_eq!(range, slot, "moved arg");
                }
                // Don't drop, they were moved to the call.
            }

            return Ok(Structured::Emitted(ret_ty, ret));
        }

        let body = func.body.clone().unwrap(); // TODO: can i just have immutable program?
        let ret_val = self.compile_expr(result, &body)?;
        let func = &self.interp.program.funcs[f.0];
        // We're done with our arguments, get rid of them. Same for other vars.
        // TODO: once non-copy types are supported, this needs to get smarter because we might have moved out of our argument.
        result.push(Bc::DebugMarker(
            self.pool.intern("drop_args"),
            func.get_name(self.pool),
        ));
        args_to_drop.extend(result.to_drop.drain(0..).map(|(s, _)| (None, s)));
        for (var, range) in args_to_drop {
            if let Some(var) = var {
                let (slot, _) = unwrap!(result.vars.remove(&var), "lost arg");
                assert_eq!(range, slot, "moved arg");
            }
            result.push(Bc::Drop(range));
        }

        Ok(ret_val)
    }

    fn emit_runtime_call(
        &mut self,
        result: &mut FnBody<'p>,
        f: FuncId,
        arg_expr: &FatExpr<'p>,
    ) -> Res<'p, Structured> {
        let arg = self.compile_expr(result, arg_expr)?;
        let func = &self.interp.program.funcs[f.0];
        let f_ty = func.unwrap_ty();
        assert!(
            !self.interp.program.is_comptime_only_type(f_ty.arg),
            "{}",
            arg_expr.log(self.pool)
        );
        let func = &self.interp.program.funcs[f.0];
        let will_capture = func.capture_vars.is_empty();
        let will_inline = func.has_tag(self.pool, "inline");
        let (arg, _) = result.load(self.interp.program, arg)?;
        if will_inline {
            self.emit_inline_call(result, arg, f)
        } else if will_capture {
            self.emit_capturing_call(result, arg, f)
        } else {
            let ret = result.reserve_slots(self.interp.program, f_ty.ret)?;
            result.push(Bc::CallDirect { f, ret, arg });
            assert_eq!(self.return_stack_slots(f), ret.count);
            assert_eq!(self.interp.program.slot_count(f_ty.ret), ret.count);
            Ok(Structured::Emitted(f_ty.ret, ret))
        }
    }

    fn emit_inline_call(
        &mut self,
        result: &mut FnBody<'p>,
        arg: StackRange,
        f: FuncId,
    ) -> Res<'p, Structured> {
        let name = self.interp.program.funcs[f.0].get_name(self.pool);
        result.push(Bc::DebugMarker(self.pool.intern("start:inline_call"), name));
        // This move ensures they end up at the base of the renumbered stack.
        let func = &self.interp.program.funcs[f.0];
        let msg = "capturing calls are already inlined. what are you doing here?";
        assert_eq!(func.capture_vars.len(), 0, "{}", msg);
        let f_ty = func.unwrap_ty();

        let stack_offset = if arg.first.0 == (result.stack_slots - arg.count) {
            // It's already at the top of the stack so don't need to move
            result.stack_slots - arg.count
        } else {
            let stack_offset = result.stack_slots;
            let arg_slots = result.reserve_slots(self.interp.program, f_ty.arg)?; // These are included in the new function's stack
            debug_assert_eq!(arg_slots.count, arg.count);
            result.push(Bc::MoveRange {
                from: arg,
                to: arg_slots,
            });
            stack_offset
        };

        let ip_offset = result.insts.len();
        let func = unwrap!(
            self.interp.ready[f.0].as_ref(),
            "inline fn must be compiled: {}",
            func.wip.as_ref().unwrap().why
        );
        // TODO: check for recusion somewhere.
        // TODO: put constants somewhere so dont have to clone them each time a function is inlined.
        result.stack_slots += func.stack_slots;
        result.slot_types.extend(func.slot_types.iter());
        let mut ret = None;
        for (i, mut inst) in func.insts.iter().cloned().enumerate() {
            assert!(ret.is_none()); // TODO
            inst.renumber(stack_offset, ip_offset);
            if let Bc::Ret(return_value) = inst {
                ret = Some(return_value);
            } else {
                result.insts.push(inst);
                if cfg!(feature = "some_log") {
                    result.debug.push(result.debug[i].clone());
                }
            }
        }
        assert!(
            ret.is_some(),
            "inline function had no ret instruction. \n{}",
            func.log(self.pool)
        );
        result.push(Bc::DebugMarker(self.pool.intern("end:inline_call"), name));
        Ok(Structured::Emitted(f_ty.ret, ret.unwrap()))
    }

    fn compile_stmt(&mut self, result: &mut FnBody<'p>, stmt: &FatStmt<'p>) -> Res<'p, ()> {
        debug_assert!(result.constants.is_valid);
        self.last_loc = Some(stmt.loc);
        match stmt.deref() {
            Stmt::DoneDeclFunc(_) => unreachable!("compiled twice?"),
            Stmt::Eval(expr) => {
                let ret = self.compile_expr(result, expr)?;
                result.drop(self.interp.program, ret)?;
            }
            Stmt::DeclVar {
                name,
                ty,
                value,
                dropping,
                kind,
            } => {
                let ty = ty.unwrap();

                match kind {
                    VarType::Const => {}
                    VarType::Let | VarType::Var => {
                        let value =
                            invert(value.as_ref().map(|expr| self.compile_expr(result, expr)))?;
                        let value = match value {
                            None => (result.reserve_slots(self.interp.program, ty)?, ty),
                            Some(value) => {
                                result.load(self.interp.program, value.unchecked_cast(ty))?
                            }
                        };

                        let prev = result.vars.insert(*name, value);
                        assert!(prev.is_none(), "shadow is still new var");

                        // TODO: what if shadow is const? that would be more consistant if did it like rust.
                        if let Some(dropping) = dropping {
                            // Maybe should be like rust and dont call drop on the shadowed thing until the end of scope.
                            // It would be consistant and it mean you can reference its data if you do a chain of transforming something with the same name.
                            // But need to change my debugging check that everything was dropped.
                            // Actually if i did that just put them in the block's list instead of carefully taking them out which i did because i thought i wanted to egarly drop.
                            let (slot, _) = unwrap!(result.vars.remove(dropping), "missing shadow");
                            result.push(Bc::Drop(slot));
                        }
                    }
                }
            }
            Stmt::Set { place, value } => self.set_deref(result, place, value)?,
            Stmt::DeclNamed { .. } => unreachable!(),
            Stmt::Noop => {}
            Stmt::DeclFunc(_) => unreachable!(),
        }
        Ok(())
    }

    fn return_stack_slots(&mut self, f: FuncId) -> usize {
        // You must self.infer_types(f); before calling this
        let func = &self.interp.program.funcs[f.0];
        let ty = func.unwrap_ty();
        self.interp.program.slot_count(ty.ret)
    }

    // TODO: make the indices always work out so you could just do it with a normal stack machine.
    //       and just use the slow linear types for debugging.
    fn compile_expr(&mut self, result: &mut FnBody<'p>, expr: &FatExpr<'p>) -> Res<'p, Structured> {
        assert!(
            !expr.ty.is_unknown(),
            "Not typechecked: {}",
            expr.log(self.pool)
        );
        result.last_loc = expr.loc;
        self.last_loc = Some(expr.loc);

        Ok(match expr.deref() {
            Expr::Closure(_) => unreachable!(),
            Expr::Call(f, arg) => {
                assert!(!f.ty.is_unknown(), "Not typechecked: {}", f.log(self.pool));
                assert!(
                    !arg.ty.is_unknown(),
                    "Not typechecked: {}",
                    arg.log(self.pool)
                );
                if let Some(f_id) = f.as_fn() {
                    let func = &self.interp.program.funcs[f_id.0];
                    assert!(!func.has_tag(self.pool, "comptime"));
                    return self.emit_runtime_call(result, f_id, arg);
                }
                unreachable!("{}", f.log(self.pool))
            }
            Expr::Block {
                body,
                result: value,
                locals,
            } => {
                for stmt in body {
                    self.compile_stmt(result, stmt)?;
                }
                let ret = self.compile_expr(result, value)?;

                for local in locals.as_ref().expect("resolve failed") {
                    if let Some((slot, _ty)) = result.vars.remove(local) {
                        result.push(Bc::Drop(slot));
                    } else if !result.constants.get(*local).is_some() {
                        ice!("Missing local {local:?}")
                    }
                }

                ret
            }
            Expr::ArrayLiteral(_) => todo!(),
            Expr::Tuple(values) => {
                debug_assert!(values.len() > 1, "no trivial tuples");
                let values: Res<'p, Vec<_>> = values
                    .iter()
                    .map(|v| self.compile_expr(result, v))
                    .collect();
                let values = values?;
                result.produce_tuple(self.interp.program, values, expr.ty)?
            }
            Expr::RefType(_) => todo!(),
            Expr::GetVar(var) => {
                if let Some((from, ty)) = result.vars.get(var).cloned() {
                    debug_assert_eq!(expr.ty, ty);
                    let to = result.reserve_slots(self.interp.program, ty)?;
                    debug_assert_eq!(from.count, to.count, "{}", self.interp.program.log_type(ty));
                    if from.count == 1 {
                        result.push(Bc::Clone {
                            from: from.first,
                            to: to.first,
                        });
                    } else {
                        result.push(Bc::CloneRange { from, to });
                    }
                    Structured::Emitted(ty, to)
                } else if let Some((value, ty)) = result.constants.get(*var) {
                    debug_assert_eq!(expr.ty, ty);
                    Structured::Const(ty, value)
                } else {
                    ice!("Missing resolved variable {:?}", var.log(self.pool),)
                }
            }
            Expr::GetNamed(_) => unreachable!(),
            Expr::EnumLiteral(_) => todo!(),
            Expr::Value { ty, value } => Structured::Const(*ty, value.clone()),
            Expr::SuffixMacro(macro_name, arg) => {
                let name = self.pool.get(*macro_name);
                match name {
                    // TODO: make `let` deeply immutable so only const addr
                    // Note: arg is not evalutated
                    "if" => self.emit_call_if(result, arg, expr.ty)?,
                    "while" => self.emit_call_while(result, arg)?,
                    "addr" => self.addr_macro(result, arg)?,
                    "quote" => unreachable!(),
                    "slice" => {
                        let container = self.compile_expr(result, arg)?;
                        let container_ty = container.ty();
                        let ty = self.interp.program.tuple_types(container.ty());
                        let (expect, count) = if let Some(types) = ty {
                            let expect = *unwrap!(types.iter().find(|t| !t.is_any()), "all any");
                            (expect, types.len())
                        } else {
                            (container.ty(), 1)
                        };
                        let ptr_ty = self.interp.program.slice_type(expect);
                        let ptr = result.reserve_slots(self.interp.program, ptr_ty)?;
                        let slot = result.load(self.interp.program, container)?.0;
                        result.push(Bc::AbsoluteStackAddr {
                            of: slot,
                            to: ptr.offset(0),
                        });
                        result.push(Bc::LoadConstant {
                            slot: ptr.offset(1),
                            value: Value::I64(count as i64),
                        });
                        result.to_drop.push((slot, container_ty));
                        (ptr, ptr_ty).into()
                    }
                    "c_call" => {
                        if let Expr::Call(f, arg) = arg.deref().deref().deref() {
                            if let Expr::Value { value, .. } = f.deref().deref() {
                                if let Value::CFnPtr { ty, .. } = value.clone().single()? {
                                    let arg = self.compile_expr(result, arg)?;
                                    let arg = result.load(self.interp.program, arg)?.0;
                                    let ret = result.reserve_slots(self.interp.program, ty.ret)?;
                                    let (f, _) =
                                        result.load_constant(self.interp.program, value.clone())?;
                                    result.push(Bc::CallC {
                                        f: f.single(),
                                        arg,
                                        ret,
                                    });
                                    (ret, ty.ret).into()
                                } else {
                                    unreachable!()
                                }
                            } else {
                                err!("c_call expected Expr:Call(Expr::GetVar, ...args)",)
                            }
                        } else {
                            err!("c_call expected Expr:Call",)
                        }
                    }
                    "deref" => {
                        let ptr = self.compile_expr(result, arg)?;
                        let ty = unwrap!(self.interp.program.unptr_ty(ptr.ty()), "");
                        let to = result.reserve_slots(self.interp.program, ty)?;
                        let from = result.load(self.interp.program, ptr)?.0;
                        result.push(Bc::Load {
                            from: from.single(),
                            to,
                        });
                        (to, ty).into()
                    }
                    "reflect_print" => {
                        let arg = self.compile_expr(result, arg)?;
                        let arg = result.load(self.interp.program, arg)?.0;
                        let ret = result.reserve_slots(self.interp.program, TypeId::unit())?;
                        result.push(Bc::CallBuiltin {
                            name: *macro_name,
                            ret,
                            arg,
                        });
                        (ret, TypeId::unit()).into()
                    }
                    "type"
                    | "assert_compile_error"
                    | "comptime_print"
                    | "symbol"
                    | "struct"
                    | "enum" => unreachable!(),
                    "tag" => {
                        // TODO: auto deref and typecheking
                        let addr = self.addr_macro(result, arg)?;
                        let ty = self
                            .interp
                            .program
                            .intern_type(TypeInfo::Ptr(TypeId::i64()));
                        let addr = result.load(self.interp.program, addr)?.0;
                        let ret = result.reserve_slots(self.interp.program, ty)?;
                        result.push(Bc::SlicePtr {
                            base: addr.single(),
                            offset: 0,
                            count: 1,
                            ret: ret.single(),
                        });
                        (ret, ty).into()
                    }
                    _ => err!(CErr::UndeclaredIdent(*macro_name)),
                }
            }
            Expr::FieldAccess(e, name) => {
                let container_ptr = self.addr_macro(result, e)?;
                self.field_access_expr(result, container_ptr, *name)?
            }
            Expr::StructLiteralP(pattern) => {
                let requested = expr.ty;
                let names: Vec<_> = pattern.flatten_names();
                // TODO: why must this suck so bad
                let values: Option<_> = pattern.flatten_exprs_ref();
                let values: Vec<_> = values.unwrap();
                assert_eq!(names.len(), values.len());
                let raw_container_ty = self.interp.program.raw_type(requested);

                match self.interp.program.types[raw_container_ty.0].clone() {
                    TypeInfo::Struct {
                        fields, as_tuple, ..
                    } => {
                        assert_eq!(
                            fields.len(),
                            values.len(),
                            "Cannot assign {values:?} to type {} = {fields:?}",
                            self.interp.program.log_type(requested)
                        );
                        let all = names.into_iter().zip(values).zip(fields);
                        let mut values = vec![];
                        for ((name, value), field) in all {
                            assert_eq!(name, field.name);
                            let value = self.compile_expr(result, value)?;
                            values.push(value.unchecked_cast(field.ty));
                        }

                        let ret = result.produce_tuple(self.interp.program, values, as_tuple)?;
                        ret.unchecked_cast(requested)
                    }
                    TypeInfo::Enum {
                        cases,
                        size_including_tag: size,
                    } => {
                        assert_eq!(
                            1,
                            values.len(),
                            "{} is an enum, value should have one active varient not {values:?}",
                            self.interp.program.log_type(requested)
                        );
                        let i = cases.iter().position(|f| f.0 == names[0]).unwrap();
                        let value = self.compile_expr(result, values[0])?;
                        // TODO: make this constexpr
                        let value = result.load(self.interp.program, value)?.0;
                        if value.count >= size {
                            ice!("Enum value won't fit.")
                        }
                        let mut ret =
                            result.reserve_slots(self.interp.program, raw_container_ty)?;
                        result.push(Bc::LoadConstant {
                            slot: ret.first,
                            value: Value::I64(i as i64),
                        });
                        result.push(Bc::MoveRange {
                            from: value,
                            to: StackRange {
                                first: ret.offset(1),
                                count: value.count,
                            },
                        });

                        // If this is a smaller varient, pad out the slot with units instead of poisons.
                        ret.count = size;
                        for i in (value.count + 1)..ret.count {
                            result.push(Bc::LoadConstant {
                                slot: ret.offset(i),
                                value: Value::Unit,
                            });
                        }

                        (ret, requested).into()
                    }
                    _ => err!("struct literal but expected {:?}", requested),
                }
            }
            Expr::String(_) | Expr::PrefixMacro { .. } => unreachable!("{}", expr.log(self.pool)),
        })
    }

    fn addr_macro(&mut self, result: &mut FnBody<'p>, arg: &FatExpr<'p>) -> Res<'p, Structured> {
        match arg.deref() {
            Expr::GetVar(var) => {
                if let Some((stack_slot, value_ty)) = result.vars.get(var).cloned() {
                    let kind = self.interp.program.vars[var.1].kind;
                    if kind != VarType::Var {
                        err!(
                            "Can only take address of vars not {kind:?} {}. TODO: allow read field.",
                            var.log(self.pool)
                        )
                    }

                    let ptr_ty = self.interp.program.ptr_type(value_ty);
                    let addr_slot = result.reserve_slots(self.interp.program, ptr_ty)?;
                    result.push(Bc::AbsoluteStackAddr {
                        of: stack_slot,
                        to: addr_slot.single(),
                    });
                    Ok((addr_slot, ptr_ty).into())
                } else if result.constants.get(*var).is_some() {
                    err!("Took address of constant {}", var.log(self.pool))
                } else {
                    ice!("Missing var {} (in !addr)", var.log(self.pool))
                }
            }
            Expr::SuffixMacro(macro_name, _) => {
                let name = self.pool.get(*macro_name);
                ice!("Took address of macro {name} not supported")
            }
            // TODO: this is a bit weird but it makes place expressions work.
            Expr::FieldAccess(_, _) => self.compile_expr(result, arg),
            &Expr::GetNamed(i) => err!(CErr::UndeclaredIdent(i)),
            _ => err!(CErr::AddrRvalue(arg.clone())),
        }
    }

    fn emit_capturing_call(
        &mut self,
        result: &mut FnBody<'p>,
        arg: StackRange,
        f: FuncId,
    ) -> Res<'p, Structured> {
        let name = self.interp.program.funcs[f.0].get_name(self.pool);
        result.push(Bc::DebugMarker(
            self.pool.intern("start:capturing_call"),
            name,
        ));
        let return_range = self.emit_body(result, arg, f)?;
        result.push(Bc::DebugMarker(
            self.pool.intern("end:capturing_call"),
            name,
        ));
        Ok(return_range)
    }

    // TODO: make this not a special case.
    /// This swaps out the closures for function accesses.
    fn emit_call_if(
        &mut self,
        result: &mut FnBody<'p>,
        arg: &FatExpr<'p>,
        out_ty: TypeId,
    ) -> Res<'p, Structured> {
        let (cond, if_true, if_false) = if let Expr::Tuple(parts) = &arg.expr {
            let cond = self.compile_expr(result, &parts[0])?;
            let if_true = parts[1].as_fn().unwrap();
            let if_false = parts[2].as_fn().unwrap();
            (cond, if_true, if_false)
        } else {
            ice!("if args must be tuple not {:?}", arg);
        };
        // TODO: if its constant you don't need the branch...
        let cond = result.load(self.interp.program, cond)?.0;

        let ret = result.reserve_slots(self.interp.program, out_ty)?;

        let unit = result
            .load_constant(self.interp.program, Values::One(Value::Unit))?
            .0; // Note: before you start doing ip stuff!
        let name = self.pool.intern("builtin:if");
        let branch_ip = result.push(Bc::DebugMarker(self.pool.intern("patch"), name));
        let true_ip = result.insts.len();
        let true_ret = self.emit_capturing_call(result, unit, if_true)?;
        let true_ret = result.load(self.interp.program, true_ret)?.0;
        result.push(Bc::MoveRange {
            from: true_ret,
            to: ret,
        });
        let jump_over_false = result.push(Bc::DebugMarker(self.pool.intern("patch"), name));
        let false_ip = result.insts.len();
        let false_ret = self.emit_capturing_call(result, unit, if_false)?;
        let false_ret = result.load(self.interp.program, false_ret)?.0;
        result.push(Bc::MoveRange {
            from: false_ret,
            to: ret,
        });

        result.insts[branch_ip] = Bc::JumpIf {
            // TODO: change to conditional so dont have to store the true_ip
            cond: cond.first,
            true_ip,
            false_ip,
        };
        result.insts[jump_over_false] = Bc::Goto {
            ip: result.insts.len(),
        };

        Ok((ret, arg.ty).into())
    }

    fn emit_call_while(
        &mut self,
        result: &mut FnBody<'p>,
        arg: &FatExpr<'p>,
    ) -> Res<'p, Structured> {
        let (cond_fn, body_fn) = if let Expr::Tuple(parts) = arg.deref() {
            let cond = parts[0].as_fn().unwrap();
            let body = parts[1].as_fn().unwrap();
            (cond, body)
        } else {
            ice!("if args must be tuple not {:?}", arg);
        };

        let name = self.pool.intern("builtin:while");
        let cond_ip = result.insts.len();
        let unit_val = Values::One(Value::Unit);
        let unit = result
            .load_constant(self.interp.program, unit_val.clone())?
            .0;
        let cond_ret = self.emit_capturing_call(result, unit, cond_fn)?;
        let branch_ip = result.push(Bc::DebugMarker(self.pool.intern("patch"), name));

        let body_ip = result.insts.len();
        let unit = result
            .load_constant(self.interp.program, unit_val.clone())?
            .0;
        let body_ret = self.emit_capturing_call(result, unit, body_fn)?;
        result.drop(self.interp.program, body_ret)?;
        result.push(Bc::Goto { ip: cond_ip });
        let end_ip = result.insts.len();

        // TODO: if cond is constant, we dont need the loop check
        let cond_ret = result.load(self.interp.program, cond_ret)?.0;
        result.insts[branch_ip] = Bc::JumpIf {
            // TODO: change to conditional so dont have to store the true_ip
            cond: cond_ret.single(),
            true_ip: body_ip,
            false_ip: end_ip,
        };

        Ok(self.interp.program.load_value(Value::Unit))
    }

    fn set_deref(
        &mut self,
        result: &mut FnBody<'p>,
        place: &FatExpr<'p>,
        value: &FatExpr<'p>,
    ) -> Res<'p, ()> {
        match place.deref().deref() {
            Expr::GetVar(var) => {
                let slot = result.vars.get(var);
                let (slot, _) =
                    *unwrap!(slot, "SetVar: var must be declared: {}", var.log(self.pool));

                let value = self.compile_expr(result, value)?;
                result.push(Bc::Drop(slot));
                let value = result.load(self.interp.program, value)?.0;
                assert_eq!(value.count, slot.count);
                for i in 0..value.count {
                    result.push(Bc::Move {
                        from: value.offset(i),
                        to: slot.offset(i),
                    });
                }
                Ok(())
            }
            Expr::SuffixMacro(macro_name, arg) => {
                // TODO: type checking
                // TODO: general place expressions.
                let macro_name = self.pool.get(*macro_name);
                if macro_name == "deref" {
                    let ptr = self.compile_expr(result, arg)?;
                    let value = self.compile_expr(result, value)?;
                    let ptr = result.load(self.interp.program, ptr)?.0;
                    let value = result.load(self.interp.program, value)?.0;
                    result.push(Bc::Store {
                        to: ptr.single(),
                        from: value,
                    });

                    return Ok(());
                }
                todo!()
            }
            &Expr::GetNamed(n) => err!(CErr::UndeclaredIdent(n)),
            _ => ice!("TODO: other `place=e;`"),
        }
    }

    fn field_access_expr(
        &mut self,
        result: &mut FnBody<'p>,
        container_ptr: Structured,
        name: Ident<'p>,
    ) -> Res<'p, Structured> {
        let mut container_ptr_ty = self.interp.program.raw_type(container_ptr.ty());
        let mut container_ptr = result.load(self.interp.program, container_ptr)?.0;
        // Auto deref for nested place expressions.
        // TODO: i actually just want same depth in chains, not always deref all the way, you might want to do stuff with a &&T or whatever.
        let depth = self.interp.program.ptr_depth(container_ptr_ty);
        if depth > 1 {
            for _ in 0..(depth - 1) {
                container_ptr_ty = unwrap!(self.interp.program.unptr_ty(container_ptr_ty), "");
                container_ptr_ty = self.interp.program.raw_type(container_ptr_ty);
                let ret = result.reserve_slots(self.interp.program, container_ptr_ty)?;
                result.push(Bc::Load {
                    from: container_ptr.single(),
                    to: ret,
                });
                container_ptr = ret;
            }
        }
        let container_ty = unwrap!(
            self.interp.program.unptr_ty(container_ptr_ty),
            "unreachable unptr_ty {:?}",
            self.interp.program.log_type(container_ptr_ty)
        );

        let raw_container_ty = self.interp.program.raw_type(container_ty);
        match &self.interp.program.types[raw_container_ty.0] {
            TypeInfo::Struct { fields, .. } => {
                for f in fields {
                    if f.name == name {
                        let f = *f;
                        let ty = self.interp.program.ptr_type(f.ty);
                        let ret = result.reserve_slots(self.interp.program, ty)?;

                        result.push(Bc::SlicePtr {
                            base: container_ptr.single(),
                            offset: f.first,
                            count: f.count,
                            ret: ret.single(),
                        });
                        return Ok((ret, ty).into());
                    }
                }
                err!(
                    "unknown name {} on {:?}",
                    self.pool.get(name),
                    self.interp.program.log_type(container_ty)
                );
            }
            TypeInfo::Enum { cases, .. } => {
                for (i, (f_name, f_ty)) in cases.iter().enumerate() {
                    if *f_name == name {
                        let f_ty = *f_ty;
                        let ty = self.interp.program.ptr_type(f_ty);
                        let ret = result.reserve_slots(self.interp.program, ty)?;
                        let count = self.interp.program.slot_count(f_ty);
                        result.push(Bc::TagCheck {
                            enum_ptr: container_ptr.single(),
                            value: i as i64,
                        });
                        result.push(Bc::SlicePtr {
                            base: container_ptr.single(),
                            offset: 1,
                            count,
                            ret: ret.single(),
                        });
                        return Ok((ret, ty).into());
                    }
                }
                err!(
                    "unknown name {} on {:?}",
                    self.pool.get(name),
                    self.interp.program.log_type(container_ty)
                );
            }
            _ => err!(
                "only structs/enums support field access but found {} = {}",
                self.interp.program.log_type(container_ty),
                self.interp.program.log_type(raw_container_ty)
            ),
        }
    }
}

impl<'p> FnBody<'p> {
    #[track_caller]
    fn reserve_slots_raw(
        &mut self,
        program: &Program<'p>,
        count: usize,
        ty: TypeId,
    ) -> Res<'p, StackRange> {
        let first = StackOffset(self.stack_slots);

        if ty.is_any() {
            // debug_assert_eq!(count, 1, "no any tuple");
            for _ in 0..count {
                self.slot_types.push(TypeId::any());
            }
            self.stack_slots += count;
        } else if count == 1 {
            self.slot_types.push(ty);
            self.stack_slots += count;
        } else {
            let types = unwrap!(
                program.tuple_types(ty),
                "expected multiple slots {:?}",
                program.log_type(ty)
            );
            let mut found = 0;
            for ty in types {
                found += self.reserve_slots(program, *ty)?.count;
            }
            debug_assert_eq!(found, count, "bad tuple size");
            // Note: don't bump self.stack_slots here.
        }
        Ok(StackRange { first, count })
    }

    #[track_caller]
    fn reserve_slots(&mut self, program: &Program<'p>, ty: TypeId) -> Res<'p, StackRange> {
        let ty = program.raw_type(ty);
        match &program.types[ty.0] {
            TypeInfo::Enum {
                size_including_tag: size,
                ..
            } => {
                let size = *size;
                let first = StackOffset(self.stack_slots);
                self.slot_types.push(TypeId::i64());
                self.stack_slots += size;
                Ok(StackRange { first, count: size })
            }
            _ => {
                let count = program.slot_count(ty);
                self.reserve_slots_raw(program, count, ty)
            }
        }
    }

    fn load_constant(
        &mut self,
        program: &mut Program<'p>,
        value: Values,
    ) -> Res<'p, (StackRange, TypeId)> {
        let ty = program.type_of_raw(&value);
        match value {
            Values::One(value) => {
                let to = self.reserve_slots(program, ty)?;
                self.push(Bc::LoadConstant {
                    slot: to.single(),
                    value,
                });
                Ok((to, ty))
            }
            Values::Many(values) => {
                let start = self.stack_slots;
                let mut count = 0;
                for value in values {
                    let (slot, _) = self.load_constant(program, value.into())?;
                    count += slot.count
                }
                Ok((
                    StackRange {
                        first: StackOffset(start),
                        count,
                    },
                    ty,
                ))
            }
        }
    }

    fn load(
        &mut self,
        program: &mut Program<'p>,
        value: Structured,
    ) -> Res<'p, (StackRange, TypeId)> {
        let expected = value.ty();
        let (slot, _ty) = match value {
            Structured::Emitted(ty, slot) => (slot, ty),
            Structured::Const(_, value) => self.load_constant(program, value)?,
            Structured::TupleDifferent(_, values) => {
                let slots: Res<'p, Vec<_>> = values
                    .into_iter()
                    .map(|value| self.load(program, value))
                    .collect();
                let slots = slots?;
                let slots = self.create_tuple_slots(program, expected, slots)?;
                (slots, expected)
            }
            Structured::RuntimeOnly(_) => unreachable!(),
        };
        // TODO: another typecheck?
        Ok((slot, expected))
    }

    fn drop(&mut self, _program: &mut Program<'p>, ret: Structured) -> Res<'p, ()> {
        match ret {
            Structured::Emitted(_, slots) => {
                self.push(Bc::Drop(slots));
            }
            Structured::Const(_, _) => {} // noop
            Structured::TupleDifferent(_, s) => {
                for s in s {
                    self.drop(_program, s)?;
                }
            }
            Structured::RuntimeOnly(_) => unreachable!(),
        }
        Ok(())
    }

    #[track_caller]
    fn push(&mut self, inst: Bc<'p>) -> usize {
        let ip = self.insts.len();
        self.insts.push(inst);

        #[cfg(feature = "some_log")]
        {
            self.debug.push(DebugInfo {
                internal_loc: Location::caller(),
                src_loc: self.last_loc,
                p: Default::default(),
            });
            debug_assert_eq!(self.insts.len(), self.debug.len(), "lost debug info");
        }
        ip
    }

    fn produce_tuple(
        &mut self,
        program: &mut Program<'p>,
        owned_values: Vec<Structured>,
        tuple_ty: TypeId,
    ) -> Res<'p, Structured> {
        let mut all_stack = true;
        let mut all_const = true;
        for v in &owned_values {
            match v {
                Structured::Emitted(_, _) => all_const = false,
                Structured::Const(_, _) => all_stack = false,
                Structured::TupleDifferent(_, _) => {
                    all_const = false;
                    all_stack = false;
                }
                Structured::RuntimeOnly(_) => unreachable!(),
            }
        }

        assert!(!(all_const && all_stack), "todo empty");
        if all_const {
            let values: Vec<_> = owned_values.into_iter().map(|v| v.get().unwrap()).collect();
            return Ok(Structured::Const(tuple_ty, values.into()));
        }

        if !all_stack {
            return Ok(Structured::TupleDifferent(tuple_ty, owned_values));
        }

        let owned_values: Vec<_> = owned_values
            .into_iter()
            .map(|v| self.load(program, v).unwrap())
            .collect();
        let ret = self.create_tuple_slots(program, tuple_ty, owned_values)?;
        Ok(Structured::Emitted(tuple_ty, ret))
    }

    fn create_tuple_slots(
        &mut self,
        program: &mut Program<'p>,
        tuple_ty: TypeId,
        owned_values: Vec<(StackRange, TypeId)>,
    ) -> Res<'p, StackRange> {
        // They might already be consecutive

        debug_assert!(!owned_values.is_empty());
        let mut next = (owned_values[0].0).first;
        let mut ok = true;
        for (r, _) in &owned_values {
            if r.first != next {
                ok = false;
                break;
            }
            next.0 += r.count;
        }
        let ret = if ok {
            StackRange {
                first: (owned_values[0].0).first,
                count: program.slot_count(tuple_ty),
            }
        } else {
            let ret = self.reserve_slots(program, tuple_ty)?;
            // TODO: they might already be consecutive. kinda want to have something to profile before fixing.
            let base = ret.first.0;
            let mut count = 0;
            for (v, _) in owned_values {
                for i in 0..v.count {
                    self.push(Bc::Move {
                        from: StackOffset(v.first.0 + i),
                        to: StackOffset(base + count),
                    });
                    count += 1;
                }
            }
            assert_eq!(count, ret.count);
            ret
        };
        Ok(ret)
    }
}

/// https://users.rust-lang.org/t/convenience-method-for-flipping-option-result-to-result-option/13695
fn invert<T, E>(x: Option<Result<T, E>>) -> Result<Option<T>, E> {
    x.map_or(Ok(None), |v| v.map(Some))
}

impl<'p> Bc<'p> {
    // Used for inlining
    fn renumber(&mut self, stack_offset: usize, ip_offset: usize) {
        match self {
            Bc::CallDynamic { f, ret, arg } | Bc::CallC { f, ret, arg } => {
                f.0 += stack_offset;
                ret.first.0 += stack_offset;
                arg.first.0 += stack_offset;
            }
            Bc::CallDirect { f: _, ret, arg } => {
                ret.first.0 += stack_offset;
                arg.first.0 += stack_offset;
            }
            Bc::CallBuiltin { ret, arg, .. } => {
                ret.first.0 += stack_offset;
                arg.first.0 += stack_offset;
            }
            Bc::LoadConstant { slot, .. } => slot.0 += stack_offset,
            Bc::JumpIf {
                cond,
                true_ip,
                false_ip,
            } => {
                cond.0 += stack_offset;
                *true_ip += ip_offset;
                *false_ip += ip_offset;
            }
            Bc::Goto { ip } => *ip += ip_offset,
            Bc::Drop(arg) | Bc::Ret(arg) => {
                arg.first.0 += stack_offset;
            }

            Bc::Move { from, to } | Bc::Clone { from, to } => {
                from.0 += stack_offset;
                to.0 += stack_offset;
            }
            Bc::CloneRange { from, to } | Bc::MoveRange { from, to } => {
                from.first.0 += stack_offset;
                to.first.0 += stack_offset;
            }
            Bc::Load { from, to } => {
                from.0 += stack_offset;
                to.first.0 += stack_offset;
            }
            Bc::AbsoluteStackAddr { of, to } => {
                of.first.0 += stack_offset;
                to.0 += stack_offset;
            }
            Bc::Store { from, to } => {
                from.first.0 += stack_offset;
                to.0 += stack_offset;
            }
            Bc::SlicePtr {
                base, offset, ret, ..
            } => {
                base.0 += stack_offset;
                *offset += stack_offset;
                ret.0 += stack_offset;
            }
            Bc::TagCheck { enum_ptr, .. } => {
                enum_ptr.0 += stack_offset;
            }
            Bc::DebugLine(_) | Bc::DebugMarker(_, _) => {}
        }
    }
}
