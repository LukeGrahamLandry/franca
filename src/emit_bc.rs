//! Converts simple ASTs into my bytecode-ish format.

#![allow(clippy::wrong_self_convention)]

use codemap::Span;
use std::ops::Deref;
use std::ptr::slice_from_raw_parts;
use std::usize;

use crate::ast::{CallConv, Expr, FatExpr, FuncId, FuncImpl, LabelId, Program, Stmt, TypeId, TypeInfo};
use crate::ast::{FatStmt, Flag, Pattern, Var, VarType};
use crate::compiler::{CErr, Compile, ExecTime, Res};
use crate::logging::PoolLog;
use crate::reflect::BitSet;
use crate::{bc::*, extend_options, Map};

use crate::{assert, assert_eq, err, ice, unwrap};

struct EmitBc<'z, 'p: 'z> {
    program: &'z Program<'p>,
    last_loc: Option<Span>,
    locals: Vec<Vec<u16>>,
    var_lookup: Map<Var<'p>, u16>,
    is_flat_call: bool,
}

pub fn emit_bc<'p>(compile: &Compile<'_, 'p>, f: FuncId, when: ExecTime) -> Res<'p, FnBody<'p>> {
    let mut emit = EmitBc::new(compile.program);
    let body = emit.compile_inner(f, when)?;
    Ok(body)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResultLoc {
    PushStack,
    ResAddr,
    Discard,
}
use ResultLoc::*;

pub fn empty_fn_body<'p>(program: &Program<'p>, func: FuncId, when: ExecTime) -> FnBody<'p> {
    let mut jump_targets = BitSet::empty();
    jump_targets.set(0); // entry is the first instruction
    FnBody {
        var_names: vec![],
        vars: Default::default(),
        when,
        func,
        blocks: vec![],
        name: program[func].name,
        aarch64_stack_bytes: None,
        current_block: BbId(0),
        inlined_return_addr: Default::default(),
        want_log: program[func].has_tag(Flag::Log_Bc),
        clock: 0,
    }
}

impl<'z, 'p: 'z> EmitBc<'z, 'p> {
    fn new(program: &'z Program<'p>) -> Self {
        Self {
            last_loc: None,
            program,
            locals: vec![],
            var_lookup: Default::default(),
            is_flat_call: false,
        }
    }

    fn compile_inner(&mut self, f: FuncId, when: ExecTime) -> Res<'p, FnBody<'p>> {
        if self.program[f].has_tag(Flag::Log_Ast) {
            println!("{}", self.program[f].log(self.program.pool));
        }

        self.locals.clear();
        self.locals.push(vec![]);
        let mut result = empty_fn_body(self.program, f, when);
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
            let mut offset = 0;
            for (name, ty, kind) in arguments.into_iter() {
                assert!(kind != VarType::Const, "{:?}", name.map(|v| v.log(self.program.pool)));

                let id = result.add_var(ty);
                let slots = self.slot_count(ty);

                result.push(Bc::NameFlatCallArg { id, offset });
                offset += slots;

                self.locals.last_mut().unwrap().push(id);
                if let Some(name) = name {
                    let prev = self.var_lookup.insert(name, id);
                    assert!(prev.is_none(), "overwrite arg? {}", name.log(self.program.pool));
                }
            }
        } else {
            // reversed because they're on the stack like [0, 1, 2]
            for (name, ty, kind) in arguments.into_iter().rev() {
                // TODO:? probably fine, i jsut set to const in closure capture but then shouldn't be adding to vars below.
                // TODO: the frontend needs to remove the 'const' parts from the ast in DeclVarPattern
                assert!(kind != VarType::Const, "{:?}", name.map(|v| v.log(self.program.pool)));

                let id = result.add_var(ty);

                let slots = self.slot_count(ty);

                if slots != 0 {
                    result.push(Bc::AddrVar { id });
                    result.push(Bc::StorePost { slots });
                }

                self.locals.last_mut().unwrap().push(id);
                if let Some(name) = name {
                    let prev = self.var_lookup.insert(name, id);
                    assert!(prev.is_none(), "overwrite arg? {}", name.log(self.program.pool));
                }
            }
        }
        Ok(())
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

        let f_ty = func.unwrap_ty();
        let (arg, ret) = self.program.get_infos(f_ty);
        let slots = if is_flat_call { 0 } else { arg.size_slots };
        let floats = if is_flat_call { 0 } else { arg.float_mask };
        let entry_block = result.push_block(slots, floats);

        // TODO: HACK. this opt shouldn't be nessisary, i just generate so much garbage :(
        if let Expr::Value { .. } = &body.expr {
        } else {
            self.bind_args(result, &func.arg)?;
        }

        debug_assert!(is_flat_call || ret.size_slots <= 1); // change ret handling if fix real c_call?
        let result_location = if is_flat_call { ResAddr } else { PushStack };
        let return_block = result.push_block(ret.size_slots, ret.float_mask);

        result.current_block = entry_block;
        if result_location == ResAddr {
            result.push(Bc::AddrFnResult);
        }
        // TODO: flat_call tail
        self.compile_expr(result, body, result_location, !is_flat_call)?;

        if result.blocks[return_block.0 as usize].incoming_jumps > 0 {
            result.push(Bc::Goto {
                ip: return_block,
                slots: ret.size_slots,
            });
            result.blocks[return_block.0 as usize].incoming_jumps += 1;
            result.current_block = return_block;
        } else {
            result.push_to(return_block, Bc::NoCompile);
        }

        self.locals.pop().unwrap();
        assert!(self.locals.is_empty());

        if !body.ty.is_never() {
            result.push(Bc::Ret); // TODO: this could just be implicit  -- May 1
        }
        if result.want_log {
            println!("{}", result.log(self.program));
        }

        Ok(())
    }

    fn emit_runtime_call(
        &mut self,
        result: &mut FnBody<'p>,
        mut f: FuncId,
        arg_expr: &FatExpr<'p>,
        result_location: ResultLoc,
        mut can_tail: bool,
    ) -> Res<'p, ()> {
        // TODO: ideally the redirect should just be stored in the overloadset so you don't have to have the big Func thing every time.
        if let FuncImpl::Redirect(target) = self.program[f].body {
            f = target;
        }

        // TODO: what if it hasnt been compiled to bc yet so hasn't had the tag added yet but will later?  -- May 7
        //       should add test of inferred flatcall mutual recursion.
        let force_flat = matches!(self.program[f].cc.unwrap(), CallConv::Flat | CallConv::FlatCt);
        if self.program[f].cc.unwrap() == CallConv::Arg8Ret1Ct {
            result.push(Bc::GetCompCtx);
        }
        let flat_arg_loc = self.compile_for_arg(result, arg_expr, force_flat)?;
        let func = &self.program[f];
        assert!(func.capture_vars.is_empty());
        assert!(
            func.cc != Some(CallConv::Inline),
            "tried to call inlined {}",
            self.program.pool.get(self.program[f].name)
        );
        let f_ty = self.program[f].unwrap_ty();
        if let Some(id) = flat_arg_loc {
            match result_location {
                PushStack => {
                    let ret_id = result.add_var(f_ty.ret);
                    result.push(Bc::AddrVar { id: ret_id });
                    result.push(Bc::AddrVar { id });
                    result.push(Bc::CallDirectFlat { f });
                    let slots = self.slot_count(f_ty.ret);
                    result.push(Bc::AddrVar { id: ret_id });
                    result.push(Bc::Load { slots });
                    result.push(Bc::LastUse { id: ret_id });
                }
                ResAddr => {
                    // res ptr was already on stack
                    result.push(Bc::AddrVar { id });
                    result.push(Bc::CallDirectFlat { f });
                }
                Discard => {
                    let ret_id = result.add_var(f_ty.ret);
                    result.push(Bc::AddrVar { id: ret_id });
                    result.push(Bc::AddrVar { id });
                    result.push(Bc::CallDirectFlat { f });
                    result.push(Bc::LastUse { id: ret_id });
                }
            }
            result.push(Bc::LastUse { id });
        } else {
            if func.has_tag(Flag::No_Tail) {
                can_tail = false;
            }
            // TODO: check if any args are pointers cause they might be to the stack and then you probably can't tail call.
            result.push(Bc::CallDirect { f, tail: can_tail });
            let slots = self.slot_count(f_ty.ret);
            if slots > 0 {
                match result_location {
                    PushStack => {}
                    ResAddr => result.push(Bc::StorePre { slots }),
                    Discard => result.push(Bc::Pop { slots }),
                }
            } else if result_location == ResAddr {
                // pop dest!
                result.push(Bc::Pop { slots: 1 });
            }
        }
        if func.finished_ret.unwrap().is_never() {
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
                result.push(Bc::AddrVar { id });
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
        result.push(Bc::AddrVar { id });
        self.compile_expr(result, value, ResAddr, false)?;
        self.locals.last_mut().unwrap().push(id);
        if let Some(name) = name {
            let prev = self.var_lookup.insert(name, id);
            assert!(prev.is_none(), "overwrite arg? {}", name.log(self.program.pool));
        }
        Ok(())
    }

    fn compile_for_arg(&mut self, result: &mut FnBody<'p>, arg: &FatExpr<'p>, force_flat: bool) -> Res<'p, Option<u16>> {
        // TODO: hack
        if force_flat {
            let id = result.add_var(arg.ty); // Note: this is kinda good because it gets scary if both sides try to avoid the copy and they alias.
            result.push(Bc::AddrVar { id });
            self.compile_expr(result, arg, ResAddr, false)?;
            Ok(Some(id))
        } else {
            self.compile_expr(result, arg, PushStack, false)?; // TODO: if its for a ret of the main function
            Ok(None)
        }
    }

    // if result_location==true, the top of the stack on entry to this function has the pointer where the result should be stored.
    // otherwise, just push it to the stack.
    fn compile_expr(&mut self, result: &mut FnBody<'p>, expr: &FatExpr<'p>, result_location: ResultLoc, can_tail: bool) -> Res<'p, ()> {
        assert!(!expr.ty.is_unknown(), "Not typechecked: {}", expr.log(self.program.pool));

        debug_assert!(
            self.slot_count(expr.ty) <= 8 || result_location != PushStack,
            "{}",
            expr.log(self.program.pool)
        );
        self.last_loc = Some(expr.loc);

        match expr.deref() {
            Expr::TupleAccess { .. }
            | Expr::String(_)
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
                    let f_id = unwrap!(f.as_const(), "tried to call non-const fn").unwrap_func_id();
                    let func = &self.program[f_id];
                    assert!(!func.has_tag(Flag::Comptime));
                    return self.emit_runtime_call(result, f_id, arg, result_location, can_tail);
                }
                if let TypeInfo::FnPtr(f_ty) = self.program[f.ty] {
                    self.compile_expr(result, f, PushStack, false)?;
                    self.compile_for_arg(result, arg, false)?; // TODO: im assuming no flat call
                    result.push(Bc::CallFnPtr {
                        ty: f_ty,
                        comp_ctx: false, // TODO
                    });
                    let slots = self.slot_count(f_ty.ret);
                    if slots > 0 {
                        match result_location {
                            PushStack => {}
                            ResAddr => result.push(Bc::StorePre { slots }),
                            Discard => result.push(Bc::Pop { slots }),
                        }
                    } else if result_location == ResAddr {
                        todo!("untested. assign to variable a call that returns unit through a function ptr");
                        // pop the dest!
                        // result.push(Bc::Pop { slots: 1 })
                    }
                    return Ok(());
                }

                if let TypeInfo::Label(ret_ty) = self.program[f.ty] {
                    let Expr::Value {
                        value: Values::One(return_from),
                        ..
                    } = f.expr
                    else {
                        todo!("{}", f.log(self.program.pool))
                    };
                    let return_from = LabelId::from_raw(return_from);
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
                    result.inlined_return_addr.insert(*ret_var, (return_block, result_location));
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
                let mut offset = 0;
                for value in values {
                    let slots = self.slot_count(value.ty);
                    if result_location == ResAddr {
                        result.push(Bc::Dup);
                        result.push(Bc::IncPtr { offset });
                        offset += slots;
                    }
                    self.compile_expr(result, value, result_location, false)?;
                }

                if result_location == ResAddr {
                    result.push(Bc::Pop { slots: 1 });
                }
            }
            Expr::GetVar(var) => {
                err!(
                    "var loads should be converted to addrs so smaller loads happen properly. {}",
                    var.log(self.program.pool)
                )
            }
            Expr::Value { value } => {
                // TODO
                // if result.when != ExecTime::Comptime && self.program.get_info(expr.ty).contains_pointers {
                //     if result_location != Discard {
                //         self.emit_relocatable_constant(expr.ty, value, result, result_location)?;
                //     }
                //     return Ok(());
                // }

                match result_location {
                    PushStack => {
                        for value in value.clone().vec().into_iter() {
                            result.push(Bc::PushConstant { value });
                        }
                    }
                    ResAddr => {
                        for value in value.clone().vec().into_iter() {
                            result.push(Bc::Dup);
                            result.push(Bc::PushConstant { value });
                            result.push(Bc::StorePre { slots: 1 });
                            result.push(Bc::IncPtr { offset: 1 });
                        }
                        result.push(Bc::Pop { slots: 1 }); // res ptr
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
                    Flag::While => {
                        debug_assert_eq!(result_location, Discard);
                        self.emit_call_while(result, arg)?;
                    }
                    Flag::Addr => self.addr_macro(result, arg, result_location)?,
                    Flag::Quote => unreachable!(),
                    Flag::Slice => {
                        let container_ty = arg.ty;
                        // Note: number of elements, not size of the whole array value.
                        let ty = self.program.tuple_types(container_ty);
                        let count = if let Some(types) = ty { types.len() } else { 1 };

                        let id = result.add_var(container_ty);
                        result.push(Bc::AddrVar { id });
                        self.compile_expr(result, arg, ResAddr, false)?;

                        result.push(Bc::AddrVar { id });
                        result.push(Bc::PushConstant { value: count as i64 });
                        match result_location {
                            PushStack => {}
                            ResAddr => result.push(Bc::StorePre { slots: 2 }),
                            Discard => result.push(Bc::Pop { slots: 2 }),
                        }
                        self.locals.last_mut().unwrap().push(id);
                    }
                    Flag::Deref => {
                        self.compile_expr(result, arg, PushStack, false)?; // get the pointer
                        let slots = self.slot_count(expr.ty);
                        // we care about the type of the pointer, not the value because there might be a cast.
                        assert!(
                            !self.program.get_info(self.program.unptr_ty(arg.ty).unwrap()).has_special_pointer_fns,
                            "{}",
                            expr.log(self.program.pool)
                        );
                        if slots == 0 {
                            match result_location {
                                ResAddr => result.push(Bc::Pop { slots: 2 }), // pop dest too!
                                PushStack | Discard => result.push(Bc::Pop { slots: 1 }),
                            };
                        } else {
                            match result_location {
                                PushStack => result.push(Bc::Load { slots }),
                                ResAddr => result.push(Bc::CopyToFrom { slots }),
                                Discard => result.push(Bc::Pop { slots: 1 }),
                            };
                        }
                    }
                    Flag::Tag => {
                        debug_assert_eq!(self.program[expr.ty], TypeInfo::Ptr(TypeId::i64()));
                        self.compile_expr(result, arg, result_location, can_tail)?;
                    }
                    Flag::Fn_Ptr => {
                        debug_assert!(matches!(self.program[arg.ty], TypeInfo::Fn(_)));
                        let f = unwrap!(arg.as_const(), "expected fn for ptr").unwrap_func_id();
                        result.push(Bc::GetNativeFnPtr(f));
                        match result_location {
                            PushStack => {}
                            ResAddr => result.push(Bc::StorePre { slots: 1 }),
                            Discard => result.push(Bc::Pop { slots: 1 }),
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
                            ResAddr => result.push(Bc::Pop { slots: 1 }), // just pop the res ptr
                            Discard => {}
                        };
                        return Ok(());
                    }
                    Flag::Return => ice!("!return is const only",),
                    name => err!("{name:?} is known flag but not builtin macro",),
                }
            }
            Expr::PtrOffset { ptr, index } => {
                self.compile_expr(result, ptr, PushStack, false)?;
                self.index_expr(result, ptr.ty, *index)?;
                match result_location {
                    PushStack => {}
                    ResAddr => result.push(Bc::StorePre { slots: 1 }),
                    Discard => result.push(Bc::Pop { slots: 1 }),
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
        result.push(Bc::AddrVar { id });

        match result_location {
            PushStack => {}
            ResAddr => result.push(Bc::StorePre { slots: 1 }),
            Discard => result.push(Bc::Pop { slots: 1 }),
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

    fn emit_call_while(&mut self, result: &mut FnBody<'p>, arg: &FatExpr<'p>) -> Res<'p, ()> {
        let Expr::Tuple(parts) = &arg.expr else {
            ice!("while args must be tuple not {:?}", arg);
        };
        let (cond_fn, body_fn) = (&parts[0], &parts[1]);

        debug_assert_eq!(body_fn.ty, TypeId::unit);

        let prev_block = result.current_block;
        let start_cond_block = result.push_block(0, 0);
        result.current_block = start_cond_block;
        result.push_to(
            prev_block,
            Bc::Goto {
                ip: start_cond_block,
                slots: 0,
            },
        );

        self.compile_expr(result, cond_fn, PushStack, false)?;
        let end_cond_block = result.current_block;

        let start_body_block = result.push_block(0, 0);
        self.compile_expr(result, body_fn, Discard, false)?;
        let end_body_block = result.current_block;

        let exit_block = result.push_block(0, 0);
        result.push_to(
            end_body_block,
            Bc::Goto {
                ip: start_cond_block,
                slots: 0,
            },
        );
        result.push_to(
            end_cond_block,
            Bc::JumpIf {
                true_ip: start_body_block,
                false_ip: exit_block,
                slots: 0,
            },
        );
        result.blocks[start_cond_block.0 as usize].incoming_jumps += 2;
        result.blocks[start_body_block.0 as usize].incoming_jumps += 1;
        result.blocks[exit_block.0 as usize].incoming_jumps += 1;

        result.bump_clock(exit_block);
        Ok(())
    }

    // :PlaceExpr
    fn set_deref(&mut self, result: &mut FnBody<'p>, place: &FatExpr<'p>, value: &FatExpr<'p>) -> Res<'p, ()> {
        // we care about the type of the pointer, not the value because there might be a cast.
        debug_assert!(!self.program.get_info(place.ty).has_special_pointer_fns,);
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

    // This is just doing a GEP
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
                let f = &fields[index];
                let offset = if let Some(bytes) = f.ffi_byte_offset {
                    assert_eq!(bytes % 8, 0);
                    (bytes / 8) as u16
                } else {
                    offset
                };
                if offset > 0 {
                    result.push(Bc::IncPtr { offset });
                }
            }
            TypeInfo::Tagged { .. } => {
                result.push(Bc::TagCheck { expected: index as u16 });
                result.push(Bc::IncPtr { offset: 1 });
            }
            TypeInfo::Tuple(types) => {
                let mut offset = 0;
                for f_ty in types.iter().take(index) {
                    offset += self.slot_count(*f_ty);
                }
                if offset > 0 {
                    result.push(Bc::IncPtr { offset });
                }
            }
            _ => err!(
                "only structs/enums support field access but found {} = {}",
                self.program.log_type(container_ty),
                self.program.log_type(raw_container_ty)
            ),
        }
        Ok(())
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
                let mut offset = 0;
                for ((name, value), field) in all {
                    assert_eq!(name, field.name);
                    let size = self.slot_count(field.ty);
                    if result_location == ResAddr {
                        result.push(Bc::Dup);
                        result.push(Bc::IncPtr { offset });
                    }
                    self.compile_expr(result, value, result_location, false)?;
                    offset += size;
                }

                if result_location == ResAddr {
                    result.push(Bc::Pop { slots: 1 }); // res ptr
                }
                assert_eq!(offset, slots, "Didn't fill whole struct");
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
                        result.push(Bc::Dup);
                        result.push(Bc::PushConstant { value: i as i64 });
                        result.push(Bc::StorePre { slots: 1 });
                        result.push(Bc::IncPtr { offset: 1 });
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

    fn _emit_relocatable_constant(&mut self, ty: TypeId, value: &Values, result: &mut FnBody<'p>, result_location: ResultLoc) -> Res<'p, ()> {
        let raw = self.program.raw_type(ty);

        match &self.program[raw] {
            TypeInfo::FnPtr(_) => err!("TODO: fnptr in constant",),
            TypeInfo::Tuple(_) => err!("TODO: partial pointers constant tuple",),
            TypeInfo::Ptr(inner) => {
                // TODO: CStr is special...
                if self.program.get_info(*inner).contains_pointers {
                    err!("TODO: nested constant pointers",)
                } else {
                    todo!()
                }
            }
            TypeInfo::Struct { fields, as_tuple } => {
                if fields.len() == 2 && fields[0].name == Flag::Ptr.ident() && fields[1].name == Flag::Len.ident() {
                    // TODO: actually construct the slice type from unptr_ty(ptr) and check that its the same.
                    // TODO: really you want to let types overload a function to do this,
                    //       because List doesn't really want to keep its uninitialized memory.
                    let parts = value.clone().vec(); // sad
                    let ptr = parts[0];
                    let len = parts[1];
                    let inner = self.program.unptr_ty(fields[0].ty).unwrap();
                    let inner_info = self.program.get_info(inner);
                    if inner_info.contains_pointers {
                        err!("TODO: nested constant pointers",)
                    }

                    if len == 0 {
                        // an empty slice can have a null data ptr i guess.
                        result.push(Bc::PushConstant { value: 0 });
                    } else {
                        assert_eq!(ptr % inner_info.align_bytes as i64, 0, "miss-aligned constant pointer. ");
                        let bytes = len as usize * inner_info.stride_bytes as usize;
                        let data = unsafe { &*slice_from_raw_parts(ptr as *const u8, bytes) };
                        result.push(Bc::PushRelocatablePointer { bytes: data });
                    }

                    result.push(Bc::PushConstant { value: len });
                } else {
                    self._emit_relocatable_constant(*as_tuple, value, result, result_location)?;
                }
            }
            TypeInfo::Tagged { .. } => err!("TODO: pointers in constant tagged union",),
            TypeInfo::Enum { raw, .. } => self._emit_relocatable_constant(*raw, value, result, result_location)?,
            TypeInfo::VoidPtr => {
                err!("You can't have a void pointer as a constant. The compiler can't tell how many bytes to put in the final executable.",)
            }
            _ => err!("ICE: bad constant",),
        }

        if result_location == ResAddr {
            let slots = self.slot_count(ty);
            result.push(Bc::StorePre { slots });
        }
        Ok(())
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

    #[track_caller]
    fn push_to(&mut self, b: BbId, inst: Bc) {
        self.blocks[b.0 as usize].insts.push(inst);
    }
}
