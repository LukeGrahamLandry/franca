//! Converts simple ASTs into my bytecode-ish format.

#![allow(clippy::wrong_self_convention)]

use codemap::Span;
use std::marker::PhantomData;
use std::ops::Deref;
use std::usize;

use crate::ast::{Expr, FatExpr, FuncId, Program, Stmt, TypeId, TypeInfo};
use crate::ast::{FatStmt, Flag, Pattern, Var, VarType};
use crate::compiler::{CErr, Compile, FnWip, Res};
use crate::extend_options;
use crate::logging::PoolLog;
use crate::reflect::BitSet;
use crate::{bc::*, Map};

use crate::{assert, assert_eq, err, ice, unwrap};

pub struct EmitBc<'z, 'p: 'z> {
    program: &'z Program<'p>,
    sizes: &'z mut SizeCache,
    last_loc: Option<Span>,
    locals: Vec<Vec<u16>>,
    var_lookup: Map<Var<'p>, u16>,
}

pub fn emit_bc<'p>(compile: &mut Compile<'_, 'p>, f: FuncId) -> Res<'p, FnBody<'p>> {
    let mut emit = EmitBc::new(compile.program, &mut compile.sizes);
    let body = emit.compile_inner(f)?;
    Ok(body)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResultLoc {
    PushStack,
    ResAddr,
    Discard,
}
use ResultLoc::*;

impl<'z, 'p: 'z> EmitBc<'z, 'p> {
    fn new(program: &'z Program<'p>, sizes: &'z mut SizeCache) -> Self {
        Self {
            last_loc: None,
            program,
            sizes,
            locals: vec![],
            var_lookup: Default::default(),
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
            blocks: vec![],
            slot_types: vec![],
            if_debug_count: 0,
            _p: PhantomData,
            aarch64_stack_bytes: None,
            current_block: BbId(0),
            inlined_return_addr: Default::default(),
            clock: 0,
        }
    }

    fn compile_inner(&mut self, f: FuncId) -> Res<'p, FnBody<'p>> {
        if self.program[f].has_tag(Flag::Log_Ast) {
            println!("{}", self.program[f].log(self.program.pool));
        }

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
            result.push(Bc::StorePost { slots });

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

        let arg = func.finished_arg.unwrap();
        let slots = self.slot_count(arg);
        let floats = self.program.float_mask_one(arg);
        let entry_block = result.push_block(slots, floats);

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

        let ret_slots = self.slot_count(func.finished_ret.unwrap());
        let ret_floats = self.program.float_mask_one(func.finished_ret.unwrap());

        // TODO: HACK. this opt shouldn't be nessisary, i just generate so much garbage :(
        if let Expr::Value { .. } = &func.body.as_ref().unwrap().expr {
        } else {
            self.bind_args(result, &func.arg)?;
        }
        let body = func.body.as_ref().unwrap();

        let is_flat_call = func.has_tag(Flag::Flat_Call);
        debug_assert!(is_flat_call || ret_slots <= 1); // change ret handling if fix real c_call?
        let result_location = if is_flat_call { ResAddr } else { PushStack };
        let return_block = result.push_block(ret_slots, ret_floats);
        result.inlined_return_addr.insert(f, (return_block, result_location));
        result.current_block = entry_block;
        if result_location == ResAddr {
            result.push(Bc::AddrFnResult);
        }
        self.compile_expr(result, body, result_location)?;

        if result.blocks[return_block.0 as usize].incoming_jumps > 0 {
            result.push(Bc::Goto {
                ip: return_block,
                slots: ret_slots,
            });
            result.blocks[return_block.0 as usize].incoming_jumps += 1;
            result.current_block = return_block;
        } else {
            result.push_to(return_block, Bc::NoCompile);
        }
        result.inlined_return_addr.remove(&f);

        for _id in self.locals.pop().unwrap() {
            // result.push(Bc::LastUse { id }); // TODO: why bother, we're returning anyway -- May 1
        }
        assert!(self.locals.is_empty());

        if !body.ty.is_never() {
            result.sub_height(ret_slots);
            result.push(Bc::Ret); // TODO: this could just be implicit  -- May 1
        }
        if func.has_tag(Flag::Log_Bc) {
            println!();
            println!("=== Bytecode for {f:?}: {} ===", self.program.pool.get(func.name));
            for (b, insts) in result.blocks.iter().enumerate() {
                if insts.insts.len() == 1 && insts.insts[0] == Bc::NoCompile && insts.incoming_jumps == 0 {
                    continue;
                }
                println!(
                    "[b{b}({})]: ({} incoming. end height={})",
                    insts.arg_slots, insts.incoming_jumps, insts.height
                );
                for (i, op) in insts.insts.iter().enumerate() {
                    println!("    {i}. {op:?}");
                }
            }
            println!("===")
        }

        Ok(())
    }

    fn emit_runtime_call(&mut self, result: &mut FnBody<'p>, f: FuncId, arg_expr: &FatExpr<'p>, result_location: ResultLoc) -> Res<'p, ()> {
        // TODO: what if it hasnt been compiled to bc yet so hasn't had the tag added yet but will later?  -- May 7
        //       should add test of inferred flatcall mutual recursion.
        let force_flat = self.program[f].has_tag(Flag::Flat_Call);
        let flat_arg_loc = self.compile_for_arg(result, arg_expr, force_flat)?;
        let func = &self.program[f];
        assert!(func.capture_vars.is_empty());
        assert!(!func.has_tag(Flag::Inline));
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
            result.push(Bc::CallDirect { f });
            result.sub_height(self.slot_count(f_ty.arg));
            let slots = self.slot_count(f_ty.ret);
            result.add_height(slots);
            match result_location {
                PushStack => {}
                ResAddr => result.push(Bc::StorePre { slots }),
                Discard => result.push(Bc::Pop { slots }),
            }
        }
        Ok(())
    }

    fn compile_stmt(&mut self, result: &mut FnBody<'p>, stmt: &FatStmt<'p>) -> Res<'p, ()> {
        self.last_loc = Some(stmt.loc);
        match stmt.deref() {
            Stmt::Eval(expr) => {
                debug_assert!(!expr.ty.is_unknown());
                self.compile_expr(result, expr, Discard)?;
            }
            Stmt::DeclVar { name, ty, value, kind } => {
                assert_ne!(VarType::Const, *kind);
                let ty = ty.unwrap();

                let id = result.add_var(ty);
                result.push(Bc::AddrVar { id });
                self.compile_expr(result, value, ResAddr)?;
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
            Stmt::ExpandParsedStmts(_) | Stmt::DoneDeclFunc(_, _) | Stmt::DeclNamed { .. } | Stmt::DeclFunc(_) => unreachable!(),
        }
        Ok(())
    }

    fn do_binding(&mut self, result: &mut FnBody<'p>, name: Option<Var<'p>>, ty: TypeId, value: &FatExpr<'p>) -> Res<'p, ()> {
        let id = result.add_var(ty);
        result.push(Bc::AddrVar { id });
        self.compile_expr(result, value, ResAddr)?;
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
            // TODO: fowrad this ot flat call somehow
            let id = result.add_var(arg.ty);
            result.push(Bc::AddrVar { id });
            self.compile_expr(result, arg, ResAddr)?;
            Ok(Some(id))
        } else {
            self.compile_expr(result, arg, PushStack)?;
            Ok(None)
        }
    }

    // if result_location==true, the top of the stack on entry to this function has the pointer where the result should be stored.
    // otherwise, just push it to the stack.
    fn compile_expr(&mut self, result: &mut FnBody<'p>, expr: &FatExpr<'p>, result_location: ResultLoc) -> Res<'p, ()> {
        assert!(!expr.ty.is_unknown(), "Not typechecked: {}", expr.log(self.program.pool));

        debug_assert!(
            self.slot_count(expr.ty) < 8 || result_location != PushStack,
            "{}",
            expr.log(self.program.pool)
        );
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
                    return self.emit_runtime_call(result, f_id, arg, result_location);
                }
                if let Expr::Value {
                    value: Values::One(Value::SplitFunc { ct, rt }),
                    ..
                } = f.expr
                {
                    todo!()
                }
                if let TypeInfo::FnPtr(f_ty) = self.program[f.ty] {
                    self.compile_expr(result, f, PushStack)?;
                    self.compile_for_arg(result, arg, false)?; // TODO: im assuming no flat call
                    result.push(Bc::CallFnPtr {
                        ty: f_ty,
                        comp_ctx: false, // TODO
                    });

                    result.sub_height(self.slot_count(f_ty.arg) + 1); // for ptr
                    let slots = self.slot_count(f_ty.ret);
                    result.add_height(slots);

                    match result_location {
                        PushStack => {}
                        ResAddr => result.push(Bc::StorePre { slots }),
                        Discard => result.push(Bc::Pop { slots }),
                    }
                    return Ok(());
                }

                if let TypeInfo::Label(ret_ty) = self.program[f.ty] {
                    if let Expr::Value {
                        value: Values::One(Value::Label { return_from }),
                        ..
                    } = f.expr
                    {
                        // result_location is the result of the ret() expression, which is Never and we don't care.
                        let (ip, res_loc) = *result.inlined_return_addr.get(&return_from).unwrap();
                        debug_assert_eq!(res_loc, PushStack); // TODO: need be be able to find the ResAddr cause it might not be on top of the stack
                        let slots = self.slot_count(ret_ty);
                        self.compile_expr(result, arg, res_loc)?;
                        result.push(Bc::Goto { ip, slots });
                        result.blocks[ip.0 as usize].incoming_jumps += 1;
                        return Ok(());
                    } else {
                        todo!()
                    }
                }

                unreachable!("{}", f.log(self.program.pool))
            }
            Expr::Block {
                body,
                result: value,
                inlined,
                ..
            } => {
                self.locals.push(vec![]);
                let slots = self.slot_count(expr.ty);
                debug_assert!(slots < 8 || result_location != PushStack);

                if let Some(inlined) = inlined {
                    let floats = self.program.float_mask_one(expr.ty);
                    let entry_block = result.current_block;
                    let return_block = if result_location == PushStack {
                        result.push_block(slots, floats)
                    } else {
                        result.push_block(0, 0)
                    };
                    result.inlined_return_addr.insert(*inlined, (return_block, result_location));
                    result.current_block = entry_block;

                    for stmt in body {
                        self.compile_stmt(result, stmt)?;
                    }
                    self.compile_expr(result, value, result_location)?;

                    if result.blocks[return_block.0 as usize].incoming_jumps > 0 {
                        let slots = result.blocks[return_block.0 as usize].arg_slots;
                        result.push(Bc::Goto { ip: return_block, slots });
                        result.blocks[return_block.0 as usize].incoming_jumps += 1;
                        result.current_block = return_block;
                    } else {
                        result.push_to(return_block, Bc::NoCompile);
                    }
                    result.inlined_return_addr.remove(inlined);
                } else {
                    for stmt in body {
                        self.compile_stmt(result, stmt)?;
                    }

                    self.compile_expr(result, value, result_location)?;
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
                    self.compile_expr(result, value, result_location)?;
                }

                if result_location == ResAddr {
                    result.push(Bc::Pop { slots: 1 });
                }
            }
            Expr::GetVar(var) => {
                if result_location == Discard {
                    return Ok(());
                }
                if let Some(id) = self.var_lookup.get(var).cloned() {
                    let slots = self.slot_count(result.vars[id as usize]);
                    result.push(Bc::AddrVar { id });
                    if result_location == ResAddr {
                        result.push(Bc::CopyToFrom { slots });
                    } else {
                        result.push(Bc::Load { slots });
                    }
                } else {
                    ice!("(emit_bc) Missing resolved variable {:?}", var.log(self.program.pool),)
                }
            }
            Expr::Value { value } => match result_location {
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
            },
            Expr::SuffixMacro(macro_name, arg) => {
                let name = if let Ok(f) = Flag::try_from(*macro_name) {
                    f
                } else {
                    // TODO: this will change when you're able to define !bang macros in the language.
                    err!(CErr::UndeclaredIdent(*macro_name))
                };
                match name {
                    Flag::If => self.emit_call_if(result, arg, result_location)?,
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
                        let slots = self.slot_count(container_ty);
                        result.push(Bc::AddrVar { id });

                        self.compile_expr(result, arg, ResAddr)?;

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
                        self.compile_expr(result, arg, PushStack)?; // get the pointer
                        let slots = self.slot_count(expr.ty);
                        match result_location {
                            PushStack => result.push(Bc::Load { slots }),
                            ResAddr => result.push(Bc::CopyToFrom { slots }),
                            Discard => result.push(Bc::Pop { slots: 1 }),
                        };
                    }
                    Flag::Tag => {
                        debug_assert_eq!(self.program[expr.ty], TypeInfo::Ptr(TypeId::i64()));
                        self.compile_expr(result, arg, result_location)?;
                    }
                    Flag::Fn_Ptr => {
                        // TODO: say arg is of type fn and dont let as_fn accept addr ptr. -- Apr 30
                        let f = unwrap!(arg.as_fn(), "expected fn for ptr");
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
            Expr::FieldAccess(_, _) => unreachable!(),
            Expr::Index { ptr, index } => {
                self.compile_expr(result, ptr, PushStack)?;
                let index = unwrap!(index.as_int(), "tuple index must be const") as usize;
                self.index_expr(result, ptr.ty, index)?;
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

    fn addr_macro(&mut self, result: &mut FnBody<'p>, arg: &FatExpr<'p>, result_location: ResultLoc) -> Res<'p, ()> {
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
                } else {
                    ice!("Missing var {} (in !addr)", var.log(self.program.pool))
                }
            }
            // TODO: this is a bit weird but it makes place expressions work.
            Expr::Index { .. } => self.compile_expr(result, arg, PushStack)?,
            _ => err!("took address of r-value",),
        }
        match result_location {
            PushStack => {}
            ResAddr => result.push(Bc::StorePre { slots: 1 }),
            Discard => result.push(Bc::Pop { slots: 1 }),
        }

        Ok(())
    }

    // we never make the temp variable. if the arg is big, caller needs to setup a result location.
    fn emit_call_if(&mut self, result: &mut FnBody<'p>, arg: &FatExpr<'p>, result_location: ResultLoc) -> Res<'p, ()> {
        let (if_true, if_false) = if let Expr::Tuple(parts) = &arg.expr {
            self.compile_expr(result, &parts[0], PushStack)?; // cond
            let if_true = &parts[1];
            let if_false = &parts[2];
            (if_true, if_false)
        } else {
            ice!("if args must be tuple not {:?}", arg);
        };
        let slots = self.slot_count(if_true.ty);
        debug_assert!(slots < 8 || result_location != PushStack); // now its the callers problem to deal with this case

        let branch_block = result.current_block;
        let true_ip = result.push_block(0, 0);
        result.current_block = true_ip;
        self.compile_expr(result, if_true, result_location)?;
        let end_true_block = result.current_block;
        let false_ip = result.push_block(0, 0);
        result.current_block = false_ip;
        self.compile_expr(result, if_false, result_location)?;
        let end_false_block = result.current_block;

        let floats = self.program.float_mask_one(if_true.ty);

        let block_slots = if result_location == PushStack { slots } else { 0 };
        let block_floats = if result_location == PushStack { floats } else { 0 };
        let ip = result.push_block(block_slots, block_floats);
        result.current_block = ip;
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
        let (cond_fn, body_fn) = if let Expr::Tuple(parts) = arg.deref() {
            (&parts[0], &parts[1])
        } else {
            ice!("if args must be tuple not {:?}", arg);
        };

        debug_assert_eq!(body_fn.ty, TypeId::unit());

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

        self.compile_expr(result, cond_fn, PushStack)?;
        let end_cond_block = result.current_block;

        let slots = self.slot_count(body_fn.ty);
        let start_body_block = result.push_block(0, 0);
        self.compile_expr(result, body_fn, Discard)?;
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

    fn set_deref(&mut self, result: &mut FnBody<'p>, place: &FatExpr<'p>, value: &FatExpr<'p>) -> Res<'p, ()> {
        match place.deref() {
            Expr::GetVar(var) => {
                let id = self.var_lookup.get(var);
                let id = *unwrap!(id, "SetVar: var must be declared: {}", var.log(self.program.pool));
                result.push(Bc::AddrVar { id });
                self.compile_expr(result, value, ResAddr)?;
                Ok(())
            }
            Expr::SuffixMacro(macro_name, arg) => {
                // TODO: write a test for pooiinter eval oreder. left hsould come first. -- MAy 7
                if let Ok(Flag::Deref) = Flag::try_from(*macro_name) {
                    self.compile_expr(result, arg, PushStack)?;
                    self.compile_expr(result, value, ResAddr)?;
                    return Ok(());
                }
                todo!()
            }
            &Expr::GetNamed(n) => err!(CErr::UndeclaredIdent(n)),
            _ => ice!("TODO: other `place=e;`"),
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

    pub fn slot_count(&mut self, ty: TypeId) -> u16 {
        self.sizes.slot_count(self.program, ty) as u16
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
                    self.compile_expr(result, value, result_location)?;
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
                        self.compile_expr(result, values[0], result_location)?;
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
                        self.compile_expr(result, values[0], result_location)?;
                    }
                    Discard => {
                        self.compile_expr(result, values[0], result_location)?;
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
            TypeInfo::Tagged { cases, .. } => 1 + cases.iter().map(|(_, ty)| self.slot_count(program, *ty)).max().expect("no empty enum"),
            TypeInfo::Scope => 2,
            TypeInfo::Never => 0,
            TypeInfo::Int(_)
            | TypeInfo::Any
            | TypeInfo::Label(_)
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
    fn add_height(&mut self, delta: u16) {
        // self.blocks[self.current_block.0 as usize].height += delta;
    }

    #[track_caller]
    fn sub_height(&mut self, delta: u16) {
        // self.blocks[self.current_block.0 as usize].height -= delta;
    }

    #[track_caller]
    fn push_to(&mut self, b: BbId, inst: Bc) {
        self.blocks[b.0 as usize].insts.push(inst);
        // match inst {
        //     Bc::Noop => {}
        //     // these are handled manually
        //     Bc::CallDirect { .. } | Bc::CallSplit { .. } | Bc::CallFnPtr { .. } | Bc::Ret => {}
        //     Bc::Load { slots } => self.blocks[b.0 as usize].height += slots - 1,
        //     Bc::Store { slots } => self.blocks[b.0 as usize].height -= slots + 1,
        //     Bc::Goto { slots, .. } => {
        //         self.blocks[b.0 as usize].height -= slots;
        //         // debug_assert_eq!(self.blocks[b.0 as usize].height, 0);
        //     }
        //     Bc::JumpIf { slots, .. } => {
        //         self.blocks[b.0 as usize].height -= slots + 1;
        //         // debug_assert_eq!(self.blocks[b.0 as usize].height, 0);
        //     }
        //     Bc::Pop { slots } => self.blocks[b.0 as usize].height -= slots,
        //     Bc::IncPtr { .. } | Bc::TagCheck { .. } | Bc::Unreachable | Bc::NoCompile | Bc::LastUse { .. } => {}
        //     Bc::AddrFnResult | Bc::PushConstant { .. } | Bc::GetNativeFnPtr(_) | Bc::AddrVar { .. } | Bc::Pick { .. } => {
        //         self.blocks[b.0 as usize].height += 1
        //     }
        // }
    }
}
