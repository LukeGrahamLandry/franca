//! Converts simple ASTs into my bytecode-ish format.

#![allow(clippy::wrong_self_convention)]
use codemap::Span;
use std::marker::PhantomData;
use std::ops::Deref;
use std::panic::Location;

use crate::ast::{FatStmt, Pattern, Var, VarType};
use crate::bc::*;
use crate::compiler::{CErr, FnWip, Res};
use crate::interp::Interp;
use crate::logging::PoolLog;
use crate::{
    ast::{Expr, FatExpr, FuncId, Program, Stmt, TypeId, TypeInfo},
    pool::Ident,
};

use crate::logging::{assert, assert_eq, err, ice, unwrap};

#[derive(Debug, Clone)]
pub struct DebugInfo<'p> {
    pub internal_loc: &'static Location<'static>,
    pub src_loc: Span,
    pub p: PhantomData<&'p str>,
}

#[derive(Default)]
pub struct SizeCache {
    pub known: Vec<Option<usize>>,
}

pub struct EmitBc<'z, 'p: 'z> {
    program: &'z Program<'p>,
    sizes: &'z mut SizeCache,
    last_loc: Option<Span>,
}

impl<'z, 'p: 'z> EmitBc<'z, 'p> {
    pub fn compile(
        program: &'z Program<'p>,
        interp: &'z mut Interp<'_, 'p>,
        f: FuncId,
    ) -> Res<'p, ()> {
        while interp.ready.len() <= f.0 {
            interp.ready.push(None);
        }
        if interp.ready[f.0].is_some() {
            return Ok(());
        }
        let mut emit = EmitBc::new(program, &mut interp.sizes);
        let body = emit.compile_inner(f)?;
        interp.ready[f.0] = Some(body);
        Ok(())
    }

    fn new(program: &'z Program<'p>, sizes: &'z mut SizeCache) -> Self {
        Self {
            last_loc: None,
            program,
            sizes,
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
            constants: func.constants.clone(), // TODO: dont clone. can have a lighter thing live on to the interpreter.
            insts: vec![],
            debug: vec![],
            slot_types: vec![],
            to_drop: vec![],
        }
    }

    fn compile_inner(&mut self, f: FuncId) -> Res<'p, FnBody<'p>> {
        let func = &self.program.funcs[f.0];
        let wip = unwrap!(func.wip.as_ref(), "Not done comptime for {f:?}"); // TODO
        debug_assert!(!func.evil_uninit);
        let mut result = self.empty_fn(wip);
        let func = &self.program.funcs[f.0];
        let arg_range = result.reserve_slots(self, func.unwrap_ty().arg)?;
        let return_value = self.emit_body(&mut result, arg_range, f);
        match return_value {
            Ok(return_value) => {
                let return_value = result.load(self, return_value)?.0;
                result.push(Bc::Ret(return_value));
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
        let mut args_to_drop = vec![];
        let arguments = pattern.flatten();
        let mut slot_count = 0;
        for (name, ty) in arguments {
            let size = self.slot_count(ty);
            let range = StackRange {
                first: full_arg_range.offset(slot_count),
                count: size,
            };
            if let Some(name) = name {
                let prev = result.vars.insert(name, (range, ty));
                assert!(prev.is_none(), "overwrite arg?");
            }
            args_to_drop.push((name, range, ty));
            slot_count += size;
        }
        assert_eq!(full_arg_range.count, slot_count);
        Ok(args_to_drop)
    }

    fn emit_body(
        &mut self,
        result: &mut FnBody<'p>,
        full_arg_range: StackRange,
        f: FuncId,
    ) -> Res<'p, Structured> {
        let func = &self.program.funcs[f.0];
        let has_body = func.body.is_some();

        let mut args_to_drop = self.bind_args(result, full_arg_range, &func.arg)?;

        if !has_body {
            // Functions without a body are always builtins.
            // It's convient to give them a FuncId so you can put them in a variable,
            // but just force inline call.
            let ret_ty = func.ret.unwrap();
            let ret = result.reserve_slots(self, func.ret.unwrap())?;
            // TODO: this check is what prevents making types comptime only work because you need to pass a type to builtin alloc,
            //       but specializing kills the name. But before that will work anyway i need tonot blindly pass on the shim args to the builtin
            //       since the shim might be specialized so some args are in constants instead of at the base of the stack.
            assert!(func.referencable_name, "fn no body needs name");
            result.push(Bc::CallBuiltin {
                name: func.name,
                ret,
                arg: full_arg_range,
            });
            for (var, range, _ty) in args_to_drop {
                if let Some(var) = var {
                    let (slot, _) = unwrap!(result.vars.remove(&var), "lost arg");
                    assert_eq!(range, slot, "moved arg");
                }
                // Don't drop, they were moved to the call.
            }

            return Ok(Structured::Emitted(ret_ty, ret));
        }

        let body = func.body.as_ref().unwrap();
        let ret_val = self.compile_expr(result, body)?;
        let func = &self.program.funcs[f.0];
        // We're done with our arguments, get rid of them. Same for other vars.
        // TODO: once non-copy types are supported, this needs to get smarter because we might have moved out of our argument.
        result.push(Bc::DebugMarker(
            self.program.pool.intern("drop_args"),
            func.get_name(self.program.pool),
        ));
        args_to_drop.extend(result.to_drop.drain(0..).map(|(s, ty)| (None, s, ty)));
        for (var, range, _ty) in args_to_drop {
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
        let func = &self.program.funcs[f.0];
        let f_ty = func.unwrap_ty();
        assert!(
            !self.program.is_comptime_only_type(f_ty.arg),
            "{}",
            arg_expr.log(self.program.pool)
        );
        let func = &self.program.funcs[f.0];
        assert!(func.capture_vars.is_empty());
        assert!(!func.has_tag(self.program.pool, "inline"));
        let (arg, arg_ty) = result.load(self, arg)?;
        let ret = result.reserve_slots(self, f_ty.ret)?;
        result.push(Bc::CallDirect { f, ret, arg });
        assert_eq!(self.return_stack_slots(f), ret.count);
        assert_eq!(self.slot_count(f_ty.ret), ret.count);
        assert_eq!(
            self.slot_count(arg_ty),
            self.slot_count(f_ty.arg),
            "{:?} vs {:?}",
            self.program.log_type(arg_ty),
            self.program.log_type(f_ty.arg),
        );
        Ok(Structured::Emitted(f_ty.ret, ret))
    }

    fn compile_stmt(&mut self, result: &mut FnBody<'p>, stmt: &FatStmt<'p>) -> Res<'p, ()> {
        debug_assert!(result.constants.is_valid);
        self.last_loc = Some(stmt.loc);
        match stmt.deref() {
            Stmt::Eval(expr) => {
                let ret = self.compile_expr(result, expr)?;
                result.drop(self, ret)?;
            }
            Stmt::DeclVar {
                name,
                ty,
                value,
                dropping,
                kind,
            } => {
                assert_ne!(VarType::Const, *kind);
                let ty = ty.unwrap();

                let value = invert(value.as_ref().map(|expr| self.compile_expr(result, expr)))?;
                let value = match value {
                    None => (result.reserve_slots(self, ty)?, ty),
                    Some(value) => result.load(self, value.unchecked_cast(ty))?,
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
            Stmt::Set { place, value } => self.set_deref(result, place, value)?,
            Stmt::DeclVarPattern { binding, value } => {
                let full_arg_range = self.compile_expr(result, value.as_ref().unwrap())?;
                let (full_arg_range, _) = result.load(self, full_arg_range)?;
                let args_to_drop = self.bind_args(result, full_arg_range, binding)?;
                for (name, slot, _) in args_to_drop {
                    if name.is_none() {
                        result.push(Bc::Drop(slot));
                        // result.to_drop.push((slot, ty));
                    }
                }
            }
            Stmt::Noop => {}
            // Can't hit DoneDeclFunc because we don't re-eval constants.
            Stmt::DoneDeclFunc(_) | Stmt::DeclNamed { .. } | Stmt::DeclFunc(_) => unreachable!(),
        }
        Ok(())
    }

    fn return_stack_slots(&mut self, f: FuncId) -> usize {
        // You must self.infer_types(f); before calling this
        let func = &self.program.funcs[f.0];
        let ty = func.unwrap_ty();
        self.slot_count(ty.ret)
    }

    // TODO: make the indices always work out so you could just do it with a normal stack machine.
    //       and just use the slow linear types for debugging.
    fn compile_expr(&mut self, result: &mut FnBody<'p>, expr: &FatExpr<'p>) -> Res<'p, Structured> {
        assert!(
            !expr.ty.is_unknown(),
            "Not typechecked: {}",
            expr.log(self.program.pool)
        );
        result.last_loc = expr.loc;
        self.last_loc = Some(expr.loc);

        Ok(match expr.deref() {
            Expr::Closure(_) => unreachable!(),
            Expr::Call(f, arg) => {
                assert!(
                    !f.ty.is_unknown(),
                    "Not typechecked: {}",
                    f.log(self.program.pool)
                );
                assert!(
                    !arg.ty.is_unknown(),
                    "Not typechecked: {}",
                    arg.log(self.program.pool)
                );
                if let Some(f_id) = f.as_fn() {
                    let func = &self.program.funcs[f_id.0];
                    assert!(!func.has_tag(self.program.pool, "comptime"));
                    return self.emit_runtime_call(result, f_id, arg);
                }
                if let TypeInfo::FnPtr(f_ty) = self.program.types[f.ty.0] {
                    let f = self.compile_expr(result, f)?;
                    let f = result.load(self, f)?;
                    let arg = self.compile_expr(result, arg)?;
                    let arg = result.load(self, arg)?.0;
                    let ret = result.reserve_slots(self, f_ty.ret)?;
                    result.push(Bc::CallC {
                        f: f.0.single(),
                        arg,
                        ret,
                        ty: f_ty,
                    });
                    return Ok((ret, f_ty.ret).into());
                }
                unreachable!("{}", f.log(self.program.pool))
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
                    } else if result.constants.get(*local).is_none() {
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
                result.produce_tuple(self, values, expr.ty)?
            }
            Expr::RefType(_) => todo!(),
            Expr::GetVar(var) => {
                if let Some((from, ty)) = result.vars.get(var).cloned() {
                    debug_assert_eq!(expr.ty, ty);
                    let to = result.reserve_slots(self, ty)?;
                    debug_assert_eq!(from.count, to.count, "{}", self.program.log_type(ty));
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
                    ice!("Missing resolved variable {:?}", var.log(self.program.pool),)
                }
            }
            Expr::GetNamed(_) => unreachable!(),
            Expr::EnumLiteral(_) => todo!(),
            Expr::Value { ty, value } => Structured::Const(*ty, value.clone()),
            Expr::SuffixMacro(macro_name, arg) => {
                let name = self.program.pool.get(*macro_name);
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
                        let ty = self.program.tuple_types(container.ty());
                        let (_, count) = if let Some(types) = ty {
                            let expect = *unwrap!(types.iter().find(|t| !t.is_any()), "all any");
                            (expect, types.len())
                        } else {
                            (container.ty(), 1)
                        };
                        let ptr_ty = expr.ty;
                        let ptr = result.reserve_slots(self, ptr_ty)?;
                        let slot = result.load(self, container)?.0;
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
                        if let Expr::Call(f, arg) = arg.deref().deref() {
                            if let Expr::Value { value, ty: val_ty } = f.deref().deref() {
                                if let Value::CFnPtr { ty: f_ty, .. } = value.clone().single()? {
                                    let arg = self.compile_expr(result, arg)?;
                                    let arg = result.load(self, arg)?.0;
                                    let ret = result.reserve_slots(self, f_ty.ret)?;
                                    let (f, ty) =
                                        result.load_constant(self, value.clone(), *val_ty)?;
                                    result.push(Bc::CallC {
                                        f: f.single(),
                                        arg,
                                        ret,
                                        ty: f_ty,
                                    });
                                    (ret, ty).into()
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
                        let ty = unwrap!(self.program.unptr_ty(ptr.ty()), "");
                        let to = result.reserve_slots(self, ty)?;
                        let from = result.load(self, ptr)?.0;
                        result.push(Bc::Load {
                            from: from.single(),
                            to,
                        });
                        (to, ty).into()
                    }
                    "reflect_print" => {
                        let arg = self.compile_expr(result, arg)?;
                        let arg = result.load(self, arg)?.0;
                        let ret = result.reserve_slots(self, TypeId::unit())?;
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
                        let ty = self.program.find_interned(TypeInfo::Ptr(TypeId::i64()));
                        let addr = result.load(self, addr)?.0;
                        let ret = result.reserve_slots(self, ty)?;
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
                let raw_container_ty = self.program.raw_type(requested);

                match &self.program.types[raw_container_ty.0] {
                    TypeInfo::Struct {
                        fields, as_tuple, ..
                    } => {
                        assert_eq!(
                            fields.len(),
                            values.len(),
                            "Cannot assign {values:?} to type {} = {fields:?}",
                            self.program.log_type(requested)
                        );
                        let all = names.into_iter().zip(values).zip(fields);
                        let mut values = vec![];
                        for ((name, value), field) in all {
                            assert_eq!(name, field.name);
                            let value = self.compile_expr(result, value)?;
                            values.push(value.unchecked_cast(field.ty));
                        }

                        let ret = result.produce_tuple(self, values, *as_tuple)?;
                        ret.unchecked_cast(requested)
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
                        let value = self.compile_expr(result, values[0])?;
                        // TODO: make this constexpr
                        let value = result.load(self, value)?.0;
                        if value.count >= size {
                            ice!("Enum value won't fit.")
                        }
                        let mut ret = result.reserve_slots(self, raw_container_ty)?;
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
            Expr::String(_) | Expr::PrefixMacro { .. } => {
                unreachable!("{}", expr.log(self.program.pool))
            }
        })
    }

    fn addr_macro(&mut self, result: &mut FnBody<'p>, arg: &FatExpr<'p>) -> Res<'p, Structured> {
        match arg.deref() {
            Expr::GetVar(var) => {
                if let Some((stack_slot, value_ty)) = result.vars.get(var).cloned() {
                    let kind = self.program.vars[var.1].kind;
                    if kind != VarType::Var {
                        err!(
                            "Can only take address of vars not {kind:?} {}. TODO: allow read field.",
                            var.log(self.program.pool)
                        )
                    }

                    let ptr_ty = self.program.find_interned(TypeInfo::Ptr(value_ty));
                    let addr_slot = result.reserve_slots(self, ptr_ty)?;
                    result.push(Bc::AbsoluteStackAddr {
                        of: stack_slot,
                        to: addr_slot.single(),
                    });
                    Ok((addr_slot, ptr_ty).into())
                } else if let Some(value) = result.constants.get(*var) {
                    // HACK: this is wrong but it makes constant structs work better.
                    if let TypeInfo::Ptr(_) = self.program.types[value.1 .0] {
                        return Ok(value.into());
                    }
                    err!("Took address of constant {}", var.log(self.program.pool))
                } else {
                    ice!("Missing var {} (in !addr)", var.log(self.program.pool))
                }
            }
            Expr::SuffixMacro(macro_name, _) => {
                let name = self.program.pool.get(*macro_name);
                ice!("Took address of macro {name} not supported")
            }
            // TODO: this is a bit weird but it makes place expressions work.
            Expr::FieldAccess(_, _) => self.compile_expr(result, arg),
            &Expr::GetNamed(i) => err!(CErr::UndeclaredIdent(i)),
            _ => err!(CErr::AddrRvalue(arg.clone())),
        }
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
            let if_true = &parts[1];
            let if_false = &parts[2];
            (cond, if_true, if_false)
        } else {
            ice!("if args must be tuple not {:?}", arg);
        };
        // TODO: if its constant you don't need the branch...
        let cond = result.load(self, cond)?.0;

        let ret = result.reserve_slots(self, out_ty)?;

        let name = self.program.pool.intern("builtin:if");
        let branch_ip = result.push(Bc::DebugMarker(self.program.pool.intern("patch"), name));
        let true_ip = result.insts.len();
        let true_ret = self.compile_expr(result, if_true)?;
        let true_ret = result.load(self, true_ret)?.0;
        result.push(Bc::MoveRange {
            from: true_ret,
            to: ret,
        });
        let jump_over_false = result.push(Bc::DebugMarker(self.program.pool.intern("patch"), name));
        let false_ip = result.insts.len();
        let false_ret = self.compile_expr(result, if_false)?;
        let false_ret = result.load(self, false_ret)?.0;
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
            (&parts[0], &parts[1])
        } else {
            ice!("if args must be tuple not {:?}", arg);
        };

        let name = self.program.pool.intern("builtin:while");
        let cond_ip = result.insts.len();
        let cond_ret = self.compile_expr(result, cond_fn)?;
        let branch_ip = result.push(Bc::DebugMarker(self.program.pool.intern("patch"), name));

        let body_ip = result.insts.len();
        let body_ret = self.compile_expr(result, body_fn)?;
        result.drop(self, body_ret)?;
        result.push(Bc::Goto { ip: cond_ip });
        let end_ip = result.insts.len();

        // TODO: if cond is constant, we dont need the loop check
        let cond_ret = result.load(self, cond_ret)?.0;
        result.insts[branch_ip] = Bc::JumpIf {
            // TODO: change to conditional so dont have to store the true_ip
            cond: cond_ret.single(),
            true_ip: body_ip,
            false_ip: end_ip,
        };

        Ok(Structured::Const(TypeId::unit(), Value::Unit.into()))
    }

    fn set_deref(
        &mut self,
        result: &mut FnBody<'p>,
        place: &FatExpr<'p>,
        value: &FatExpr<'p>,
    ) -> Res<'p, ()> {
        match place.deref() {
            Expr::GetVar(var) => {
                let slot = result.vars.get(var);
                let (slot, _) = *unwrap!(
                    slot,
                    "SetVar: var must be declared: {}",
                    var.log(self.program.pool)
                );

                let value = self.compile_expr(result, value)?;
                result.push(Bc::Drop(slot));
                let value = result.load(self, value)?.0;
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
                let macro_name = self.program.pool.get(*macro_name);
                if macro_name == "deref" {
                    let ptr = self.compile_expr(result, arg)?;
                    let value = self.compile_expr(result, value)?;
                    let ptr = result.load(self, ptr)?.0;
                    let value = result.load(self, value)?.0;
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
        let mut container_ptr_ty = self.program.raw_type(container_ptr.ty());
        let mut container_ptr = result.load(self, container_ptr)?.0;
        // Auto deref for nested place expressions.
        // TODO: i actually just want same depth in chains, not always deref all the way, you might want to do stuff with a &&T or whatever.
        let depth = self.program.ptr_depth(container_ptr_ty);
        if depth > 1 {
            for _ in 0..(depth - 1) {
                container_ptr_ty = unwrap!(self.program.unptr_ty(container_ptr_ty), "");
                container_ptr_ty = self.program.raw_type(container_ptr_ty);
                let ret = result.reserve_slots(self, container_ptr_ty)?;
                result.push(Bc::Load {
                    from: container_ptr.single(),
                    to: ret,
                });
                container_ptr = ret;
            }
        }
        let container_ty = unwrap!(
            self.program.unptr_ty(container_ptr_ty),
            "unreachable unptr_ty {:?}",
            self.program.log_type(container_ptr_ty)
        );

        let raw_container_ty = self.program.raw_type(container_ty);
        match &self.program.types[raw_container_ty.0] {
            TypeInfo::Struct { fields, .. } => {
                let mut offset = 0;
                for f in fields {
                    if f.name == name {
                        let f = *f;
                        let ty = self.program.find_interned(TypeInfo::Ptr(f.ty));
                        let ret = result.reserve_slots(self, ty)?;

                        result.push(Bc::SlicePtr {
                            base: container_ptr.single(),
                            offset,
                            count: self.slot_count(f.ty),
                            ret: ret.single(),
                        });
                        return Ok((ret, ty).into());
                    }
                    offset += self.slot_count(f.ty);
                }
                err!(
                    "unknown name {} on {:?}",
                    self.program.pool.get(name),
                    self.program.log_type(container_ty)
                );
            }
            TypeInfo::Enum { cases, .. } => {
                for (i, (f_name, f_ty)) in cases.iter().enumerate() {
                    if *f_name == name {
                        let f_ty = *f_ty;
                        let ty = self.program.find_interned(TypeInfo::Ptr(f_ty));
                        let ret = result.reserve_slots(self, ty)?;
                        let count = self.slot_count(f_ty);
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
                    self.program.pool.get(name),
                    self.program.log_type(container_ty)
                );
            }
            _ => err!(
                "only structs/enums support field access but found {} = {}",
                self.program.log_type(container_ty),
                self.program.log_type(raw_container_ty)
            ),
        }
    }
    pub fn slot_count(&mut self, ty: TypeId) -> usize {
        self.sizes.slot_count(self.program, ty)
    }
}

impl SizeCache {
    // TODO: Unsized types. Any should be a TypeId and then some memory with AnyPtr being the fat ptr version.
    //       With raw Any version, you couldn't always change types without reallocating the space and couldn't pass it by value.
    //       AnyScalar=(TypeId, one value), AnyPtr=(TypeId, one value=stack/heap ptr), AnyUnsized=(TypeId, some number of stack slots...)
    pub fn slot_count(&mut self, program: &Program, ty: TypeId) -> usize {
        while self.known.len() <= ty.0 {
            self.known.push(None);
        }
        if let Some(size) = self.known[ty.0] {
            return size;
        }
        let ty = program.raw_type(ty);
        let size = match &program.types[ty.0] {
            TypeInfo::Unknown => 9999,
            TypeInfo::Tuple(args) => args.iter().map(|t| self.slot_count(program, *t)).sum(),
            TypeInfo::Struct { fields, .. } => {
                fields.iter().map(|f| self.slot_count(program, f.ty)).sum()
            }
            TypeInfo::Enum { cases, .. } => {
                1 + cases
                    .iter()
                    .map(|(_, ty)| self.slot_count(program, *ty))
                    .max()
                    .expect("no empty enum")
            }
            TypeInfo::Int(_)
            | TypeInfo::Any
            | TypeInfo::Never
            | TypeInfo::F64
            | TypeInfo::Bool
            | TypeInfo::Fn(_)
            | TypeInfo::Ptr(_)
            | TypeInfo::VoidPtr
            | TypeInfo::FnPtr(_)
            | TypeInfo::Slice(_)
            | TypeInfo::Type
            | TypeInfo::Unit => 1,
            TypeInfo::Unique(_, _) | TypeInfo::Named(_, _) => unreachable!(),
        };
        self.known[ty.0] = Some(size);
        size
    }
}

impl<'p> FnBody<'p> {
    #[track_caller]
    fn reserve_slots_raw(
        &mut self,
        program: &mut EmitBc<'_, 'p>,
        count: usize,
        ty: TypeId,
    ) -> Res<'p, StackRange> {
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
                found += self.reserve_slots(program, *ty)?.count;
            }
            debug_assert_eq!(found, count, "bad tuple size");
            // Note: don't bump self.stack_slots here.
        }
        Ok(StackRange { first, count })
    }

    #[track_caller]
    fn reserve_slots(&mut self, program: &mut EmitBc<'_, 'p>, ty: TypeId) -> Res<'p, StackRange> {
        let ty = program.program.raw_type(ty);
        let count = program.slot_count(ty);
        match &program.program.types[ty.0] {
            TypeInfo::Enum { .. } => {
                let first = StackOffset(self.stack_slots);
                self.slot_types.push(TypeId::i64());
                self.stack_slots += count;
                Ok(StackRange { first, count })
            }
            _ => self.reserve_slots_raw(program, count, ty),
        }
    }

    fn load_constant(
        &mut self,
        program: &mut EmitBc<'_, 'p>,
        value: Values,
        ty: TypeId,
    ) -> Res<'p, (StackRange, TypeId)> {
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
                let count = values.len();
                for value in values {
                    let to = self.reserve_slots(program, TypeId::any())?;
                    self.push(Bc::LoadConstant {
                        slot: to.single(),
                        value,
                    });
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
        program: &mut EmitBc<'_, 'p>,
        value: Structured,
    ) -> Res<'p, (StackRange, TypeId)> {
        let expected = value.ty();
        let (slot, _ty) = match value {
            Structured::Emitted(ty, slot) => (slot, ty),
            Structured::Const(ty, value) => self.load_constant(program, value, ty)?,
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

    fn drop(&mut self, _program: &EmitBc<'_, 'p>, ret: Structured) -> Res<'p, ()> {
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
        program: &mut EmitBc<'_, 'p>,
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
        program: &mut EmitBc<'_, 'p>,
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
