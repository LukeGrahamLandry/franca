#![allow(clippy::wrong_self_convention)]
#![allow(unused)]

use codemap::Span;
use std::collections::HashMap;
use std::ops::Deref;

use crate::ast::{FatStmt, Pattern, Var, VarType};
use crate::bc::*;
use crate::compiler::{CErr, Res};
use crate::emit_bc::SizeCache;
use crate::experiments::arena::Arena;
use crate::experiments::ir::{Block, Call, Callable, Cc, Inst, Ip, IrFunc, Item, Ret, Val};
use crate::experiments::reflect::BitSet;
use crate::interp::Interp;
use crate::logging::PoolLog;
use crate::{
    ast::{Expr, FatExpr, FuncId, Program, Stmt, TypeId, TypeInfo},
    pool::Ident,
};

use crate::logging::{assert, assert_eq, err, ice, unwrap};

pub struct EmitIr<'z, 'p: 'z, 'a> {
    program: &'z Program<'p>,
    sizes: &'z mut SizeCache,
    last_loc: Option<Span>,
    ir: IrFunc<'a>,
    vars: HashMap<Var<'p>, Val>,
    func: FuncId,
}

impl<'z, 'p: 'z, 'a> EmitIr<'z, 'p, 'a> {
    pub fn compile(
        program: &'z Program<'p>,
        interp: &'z mut Interp<'_, 'p>,
        f: FuncId,
        arena: &'a Arena<'a>,
    ) -> Res<'p, IrFunc<'a>> {
        let mut emit = EmitIr::new(program, &mut interp.sizes, arena, f);
        if let Err(mut e) = emit.compile_inner(f) {
            e.loc = emit.last_loc;
            return Err(e);
        }
        Ok(emit.ir)
    }

    fn new(
        program: &'z Program<'p>,
        sizes: &'z mut SizeCache,
        arena: &'a Arena<'a>,
        f: FuncId,
    ) -> Self {
        Self {
            last_loc: None,
            program,
            sizes,
            ir: IrFunc {
                arena,
                blocks: Vec::new_in(arena),
                alloc_a: Vec::new_in(arena),
                values: Vec::new_in(arena),
                reachable: BitSet::empty(),
            },
            vars: Default::default(),
            func: f,
        }
    }

    fn new_block(&mut self, arg_ty: TypeId) -> Ip {
        let args = self.new_val(arg_ty, Item::Scalar);
        self.ir.blocks.push(Block {
            args: (arg_ty, args),
            body: Vec::new_in(self.ir.arena),
            end: Ret::Empty,
        });
        Ip(self.ir.blocks.len() - 1)
    }

    fn new_val(&mut self, ty: TypeId, item: Item<'a>) -> Val {
        self.ir.values.push((ty, item));
        Val(self.ir.values.len() - 1)
    }

    fn push(&mut self, block: Ip, inst: Inst) {
        self.ir.blocks[block.0].body.push(inst)
    }

    fn arg_of(&mut self, block: Ip) -> Val {
        self.ir.blocks[block.0].args.1
    }

    #[track_caller]
    fn end(&mut self, block: Ip, end: Ret<'a>) {
        debug_assert!(
            matches!(self.ir.blocks[block.0].end, Ret::Empty),
            "Tried to close twice {:?} -> {end:?}",
            self.ir.blocks[block.0].end
        );
        self.ir.blocks[block.0].end = end;
    }

    fn compile_inner(&mut self, f: FuncId) -> Res<'p, ()> {
        let func = &self.program.funcs[f.0];
        debug_assert!(!func.evil_uninit);
        let func = &self.program.funcs[f.0];
        if let Some(body) = func.body.as_ref() {
            let entry = self.new_block(func.finished_arg.unwrap());
            self.bind_args(entry, &func.arg)?;
            let out = self.new_block(func.finished_ret.unwrap());
            let start = self.compile_expr(out, body)?;
            self.end(entry, Ret::Goto(start));
            let ret = self.arg_of(out);
            self.end(out, Ret::Return(ret));
        }
        Ok(())
    }

    // Do pattern matching to attach names to individual parts of a block's argument value.
    fn bind_args(&mut self, block: Ip, pattern: &Pattern<'p>) -> Res<'p, ()> {
        let arguments = pattern.flatten();
        let container = self.arg_of(block);
        for (index, (name, ty)) in arguments.into_iter().enumerate() {
            let val = self.new_val(ty, Item::Gep { container, index });
            if let Some(name) = name {
                let prev = self.vars.insert(name, val);
                assert!(prev.is_none(), "overwrite arg?");
            }
        }
        Ok(())
    }

    fn emit_runtime_call(
        &mut self,
        output: Ip,
        f: &FatExpr<'p>,
        arg_expr: &FatExpr<'p>,
    ) -> Res<'p, Ip> {
        if let Some(f) = f.as_fn() {
            let func = &self.program.funcs[f.0];
            let f_ty = func.unwrap_ty();
            let do_call = self.new_block(f_ty.arg);
            let do_arg = self.compile_expr(do_call, arg_expr)?;
            let arg = self.arg_of(do_call);
            self.end(
                do_call,
                Ret::Call(Call {
                    convention: Cc::PushFrame,
                    f: Callable::Fn(f),
                    arg,
                    then: Callable::Block(output),
                }),
            );
            Ok(do_arg)
        } else if let TypeInfo::FnPtr(f_ty) = self.program.types[f.ty.0] {
            let do_call = self.new_block(f_ty.arg);
            let do_arg = self.compile_expr(do_call, arg_expr)?;
            let do_func = self.compile_expr(do_arg, f)?; // TODO: wrong. 'output' arg ty needs to be the fn ptr
            let f = Callable::Ptr(self.arg_of(do_func));
            let arg = self.arg_of(do_call);
            self.end(
                do_call,
                Ret::Call(Call {
                    convention: Cc::PushFrame,
                    f,
                    arg,
                    then: Callable::Block(output),
                }),
            );
            Ok(do_func)
        } else {
            unreachable!()
        }
    }

    fn compile_stmt(&mut self, stmt: &FatStmt<'p>) -> Res<'p, (Ip, Ip)> {
        self.last_loc = Some(stmt.loc);
        Ok(match stmt.deref() {
            Stmt::Eval(expr) => {
                let out = self.new_block(expr.ty);
                let start = self.compile_expr(out, expr)?;
                (start, out)
            }
            Stmt::DeclVar {
                name,
                ty,
                value,
                kind,
                ..
            } => {
                assert_ne!(VarType::Const, *kind);
                let ty = ty.unwrap();
                let dest =
                    self.new_val(self.program.find_interned(TypeInfo::Ptr(ty)), Item::Scalar);
                let (start, end) = match value {
                    None => {
                        let start = self.new_block(TypeId::unit());
                        (start, start)
                    }
                    Some(value) => {
                        let set = self.new_block(ty);
                        let start = self.compile_expr(set, value)?;
                        let src = self.arg_of(set);
                        self.push(
                            set,
                            Inst::Store {
                                src,
                                dest_ptr: dest,
                            },
                        );
                        (start, set)
                    }
                };
                self.push(start, Inst::AllocA { ty, dest });
                self.vars.insert(*name, dest);
                (start, end)
            }
            Stmt::Set { place, value } => self.set_deref(place, value)?,
            Stmt::DeclVarPattern { binding, value } => {
                let ty: Vec<TypeId> = binding.bindings.iter().map(|b| b.ty.unwrap()).collect();
                let ty = if ty.is_empty() {
                    TypeId::unit()
                } else if ty.len() == 1 {
                    ty[0]
                } else {
                    self.program.find_interned(TypeInfo::Tuple(ty))
                };
                let set = self.new_block(ty);
                match value {
                    None => todo!(),
                    Some(value) => {
                        let start = self.compile_expr(set, value)?;
                        self.bind_args(set, binding)?;
                        (start, set)
                    }
                }
            }
            Stmt::Noop => {
                let start = self.new_block(TypeId::unit());
                (start, start)
            }
            // Can't hit DoneDeclFunc because we don't re-eval constants.
            Stmt::DoneDeclFunc(_) | Stmt::DeclNamed { .. } | Stmt::DeclFunc(_) => unreachable!(),
        })
    }

    /// Produce a value and pass it as an argument to the block <output>
    fn compile_expr(&mut self, output: Ip, expr: &FatExpr<'p>) -> Res<'p, Ip> {
        self.last_loc = Some(expr.loc);

        Ok(match expr.deref() {
            Expr::WipFunc(_) | Expr::Closure(_) | Expr::GetNamed(_) => unreachable!(),
            Expr::Call(f, arg) => self.emit_runtime_call(output, f, arg)?,
            Expr::Block {
                body,
                result: value,
                ..
            } => {
                let start = self.new_block(TypeId::unit());
                let mut prev = start;
                for stmt in body {
                    let (start, end) = self.compile_stmt(stmt)?;
                    self.end(prev, Ret::Goto(start));
                    prev = end;
                }
                let res = self.compile_expr(output, value)?;
                self.end(prev, Ret::Goto(res));
                start
            }
            Expr::Tuple(values) => {
                debug_assert!(values.len() > 1, "no trivial tuples");
                let mut parts = Vec::with_capacity_in(values.len(), self.ir.arena);
                let mut start: Option<Ip> = None;
                let mut last: Option<Ip> = None;
                for expr in values.iter().rev() {
                    let consume = self.new_block(expr.ty);
                    parts.push(self.arg_of(consume));
                    let create = self.compile_expr(consume, expr)?;
                    if let Some(start) = start {
                        self.end(consume, Ret::Goto(start));
                    } else {
                        last = Some(consume);
                    }
                    start = Some(create);
                }
                let arg = self.new_val(expr.ty, Item::Tuple(parts));
                self.end(last.unwrap(), Ret::GotoWith(output, arg));
                start.unwrap()
            }
            Expr::GetVar(var) => {
                let start = self.new_block(TypeId::unit());
                let val = if let Some(val) = self.vars.get(var).cloned() {
                    val
                } else if let Some((value, ty)) =
                    self.program.funcs[self.func.0].closed_constants.get(*var)
                {
                    debug_assert_eq!(expr.ty, ty);
                    self.new_val(ty, Item::Value(value.single()?))
                } else {
                    ice!("Missing resolved variable {:?}", var.log(self.program.pool),)
                };
                self.end(start, Ret::GotoWith(output, val));
                start
            }
            Expr::Value { ty, value } => {
                let start = self.new_block(TypeId::unit());
                let arg = self.new_val(*ty, Item::Value(value.clone().single()?));
                self.end(start, Ret::GotoWith(output, arg));
                start
            }
            Expr::SuffixMacro(macro_name, arg) => {
                let name = self.program.pool.get(*macro_name);
                match name {
                    // TODO: make `let` deeply immutable so only const addr
                    "if" => self.emit_call_if(output, arg)?,
                    "while" => self.emit_call_while(output, arg)?,
                    "addr" => self.addr_macro(output, arg)?,
                    "quote" => unreachable!(),
                    "slice" => {
                        // let container = self.compile_expr(result, arg)?;
                        // let container_ty = container.ty();
                        // let ty = self.program.tuple_types(container.ty());
                        // let (_, count) = if let Some(types) = ty {
                        //     let expect = *unwrap!(types.iter().find(|t| !t.is_any()), "all any");
                        //     (expect, types.len())
                        // } else {
                        //     (container.ty(), 1)
                        // };
                        // let ptr_ty = expr.ty;
                        // let ptr = result.reserve_slots(self, ptr_ty)?;
                        // let slot = result.load(self, container)?.0;
                        // for i in slot {
                        //     result.slot_is_var.set(i);
                        // }
                        // result.push(Bc::AbsoluteStackAddr {
                        //     of: slot,
                        //     to: ptr.offset(0),
                        // });
                        // result.push(Bc::LoadConstant {
                        //     slot: ptr.offset(1),
                        //     value: Value::I64(count as i64),
                        // });
                        // result.to_drop.push((slot, container_ty));
                        // (ptr, ptr_ty).into()
                        todo!()
                    }
                    "c_call" => err!(
                        "!c_call has been removed. calling convention is part of the type now.",
                    ),
                    "deref" => {
                        let use_ptr = self.new_block(arg.ty);
                        let start = self.compile_expr(use_ptr, arg)?;
                        let ty = unwrap!(self.program.unptr_ty(arg.ty), "");
                        let src = self.arg_of(use_ptr);
                        let dest = self.new_val(ty, Item::Scalar);
                        self.push(use_ptr, Inst::Load { src, dest });
                        self.end(use_ptr, Ret::GotoWith(output, dest));
                        start
                    }
                    "reflect_print" => todo!(),
                    "type"
                    | "assert_compile_error"
                    | "comptime_print"
                    | "symbol"
                    | "struct"
                    | "enum" => unreachable!(),
                    "tag" => {
                        // TODO: auto deref and typecheking
                        // let addr = self.addr_macro(result, arg)?;
                        // let ty = self.program.find_interned(TypeInfo::Ptr(TypeId::i64()));
                        // let addr = result.load(self, addr)?.0;
                        // let ret = result.reserve_slots(self, ty)?;
                        // result.push(Bc::SlicePtr {
                        //     base: addr.single(),
                        //     offset: 0,
                        //     count: 1,
                        //     ret: ret.single(),
                        // });
                        // (ret, ty).into()
                        todo!()
                    }
                    _ => err!(CErr::UndeclaredIdent(*macro_name)),
                }
            }
            Expr::FieldAccess(e, name) => {
                // let container_ptr = self.addr_macro(result, e)?;
                // self.field_access_expr(result, container_ptr, *name)?
                todo!()
            }
            Expr::StructLiteralP(pattern) => {
                // let requested = expr.ty;
                // let names: Vec<_> = pattern.flatten_names();
                // // TODO: why must this suck so bad
                // let values: Option<_> = pattern.flatten_exprs_ref();
                // let values: Vec<_> = values.unwrap();
                // assert_eq!(names.len(), values.len());
                // let raw_container_ty = self.program.raw_type(requested);
                //
                // match &self.program.types[raw_container_ty.0] {
                //     TypeInfo::Struct {
                //         fields, as_tuple, ..
                //     } => {
                //         assert_eq!(
                //             fields.len(),
                //             values.len(),
                //             "Cannot assign {values:?} to type {} = {fields:?}",
                //             self.program.log_type(requested)
                //         );
                //         let all = names.into_iter().zip(values).zip(fields);
                //         let mut values = vec![];
                //         for ((name, value), field) in all {
                //             assert_eq!(name, field.name);
                //             let value = self.compile_expr(result, value)?;
                //             values.push(value.unchecked_cast(field.ty));
                //         }
                //
                //         let ret = result.produce_tuple(self, values, *as_tuple)?;
                //         ret.unchecked_cast(requested)
                //     }
                //     TypeInfo::Enum { cases } => {
                //         let size = self.slot_count(raw_container_ty);
                //         assert_eq!(
                //             1,
                //             values.len(),
                //             "{} is an enum, value should have one active varient not {values:?}",
                //             self.program.log_type(requested)
                //         );
                //         let i = cases.iter().position(|f| f.0 == names[0]).unwrap();
                //         let value = self.compile_expr(result, values[0])?;
                //         // TODO: make this constexpr
                //         let value = result.load(self, value)?.0;
                //         if value.count >= size {
                //             ice!("Enum value won't fit.")
                //         }
                //         let mut ret = result.reserve_slots(self, raw_container_ty)?;
                //         result.push(Bc::LoadConstant {
                //             slot: ret.first,
                //             value: Value::I64(i as i64),
                //         });
                //         result.push(Bc::MoveRange {
                //             from: value,
                //             to: StackRange {
                //                 first: ret.offset(1),
                //                 count: value.count,
                //             },
                //         });
                //
                //         // If this is a smaller varient, pad out the slot with units instead of poisons.
                //         ret.count = size;
                //         for i in (value.count + 1)..ret.count {
                //             result.push(Bc::LoadConstant {
                //                 slot: ret.offset(i),
                //                 value: Value::Unit,
                //             });
                //         }
                //
                //         (ret, requested).into()
                //     }
                //     _ => err!("struct literal but expected {:?}", requested),
                // }
                todo!()
            }
            Expr::String(_) | Expr::PrefixMacro { .. } => {
                unreachable!("{}", expr.log(self.program.pool))
            }
        })
    }

    fn addr_macro(&mut self, output: Ip, arg: &FatExpr<'p>) -> Res<'p, Ip> {
        let start = self.new_block(TypeId::unit());
        match arg.deref() {
            Expr::GetVar(var) => {
                if let Some(val) = self.vars.get(var).cloned() {
                    let kind = self.program.vars[var.1].kind;
                    if kind != VarType::Var {
                        err!(
                            "Can only take address of vars not {kind:?} {}",
                            var.log(self.program.pool)
                        )
                    }
                    self.end(start, Ret::GotoWith(output, val));
                    Ok(start)
                } else if let Some((value, ty)) =
                    self.program.funcs[self.func.0].closed_constants.get(*var)
                {
                    // HACK: this is wrong but it makes constant structs work better.
                    if let TypeInfo::Ptr(_) = self.program.types[ty.0] {
                        let value = self.new_val(ty, Item::Value(value.single()?));
                        self.end(start, Ret::GotoWith(output, value));
                        return Ok(start);
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
            Expr::FieldAccess(_, _) => self.compile_expr(output, arg),
            &Expr::GetNamed(i) => err!(CErr::UndeclaredIdent(i)),
            _ => err!(CErr::AddrRvalue(arg.clone())),
        }
    }

    // TODO: make this not a special case.
    /// This swaps out the closures for function accesses.
    fn emit_call_if(&mut self, output: Ip, arg: &FatExpr<'p>) -> Res<'p, Ip> {
        let do_branch = self.new_block(TypeId::bool());
        let (do_cond, if_true, if_false) = if let Expr::Tuple(parts) = &arg.expr {
            let cond = self.compile_expr(do_branch, &parts[0])?;
            let if_true = &parts[1];
            let if_false = &parts[2];
            (cond, if_true, if_false)
        } else {
            ice!("if args must be tuple not {:?}", arg);
        };

        let if_true = self.compile_expr(output, if_true)?;
        let if_false = self.compile_expr(output, if_false)?;
        let cond = self.arg_of(do_branch);
        self.end(
            do_branch,
            Ret::If {
                cond,
                if_true,
                if_false,
            },
        );
        Ok(do_cond)
    }

    fn emit_call_while(&mut self, output: Ip, arg: &FatExpr<'p>) -> Res<'p, Ip> {
        let (cond_fn, body_fn) = if let Expr::Tuple(parts) = arg.deref() {
            (&parts[0], &parts[1])
        } else {
            ice!("if args must be tuple not {:?}", arg);
        };

        let do_branch = self.new_block(TypeId::bool());
        let start = self.compile_expr(do_branch, cond_fn)?;
        let do_body = self.compile_expr(start, body_fn)?;
        let cond = self.arg_of(do_branch);
        self.end(
            do_branch,
            Ret::If {
                cond,
                if_true: do_body,
                if_false: output,
            },
        );
        Ok(start)
    }

    fn set_deref(&mut self, place: &FatExpr<'p>, value: &FatExpr<'p>) -> Res<'p, (Ip, Ip)> {
        match place.deref() {
            Expr::GetVar(var) => {
                let set = self.new_block(value.ty);
                let start = self.compile_expr(set, value)?;
                let dest_ptr = *unwrap!(self.vars.get(var), "undeclared");
                let src = self.arg_of(set);
                self.push(set, Inst::Store { src, dest_ptr });
                Ok((start, set))
            }
            Expr::SuffixMacro(macro_name, arg) => {
                // TODO: type checking
                // TODO: general place expressions.
                let macro_name = self.program.pool.get(*macro_name);
                if macro_name == "deref" {
                    let set = self.new_block(value.ty);
                    let use_ptr = self.new_block(arg.ty);
                    let start = self.compile_expr(use_ptr, arg)?;
                    let start_set = self.compile_expr(set, value)?;
                    self.end(use_ptr, Ret::Goto(start_set));
                    let src = self.arg_of(set);
                    let dest_ptr = self.arg_of(use_ptr);
                    self.push(set, Inst::Store { src, dest_ptr });
                    return Ok((start, set));
                }
                todo!()
            }
            &Expr::GetNamed(n) => err!(CErr::UndeclaredIdent(n)),
            _ => ice!("TODO: other `place=e;`"),
        }
    }

    fn field_access_expr(
        &mut self,
        result: &FnBody<'p>,
        container_ptr: Structured,
        name: Ident<'p>,
    ) -> Res<'p, Structured> {
        // let mut container_ptr_ty = self.program.raw_type(container_ptr.ty());
        // let mut container_ptr = result.load(self, container_ptr)?.0;
        // // Auto deref for nested place expressions.
        // // TODO: i actually just want same depth in chains, not always deref all the way, you might want to do stuff with a &&T or whatever.
        // let depth = self.program.ptr_depth(container_ptr_ty);
        // if depth > 1 {
        //     for _ in 0..(depth - 1) {
        //         container_ptr_ty = unwrap!(self.program.unptr_ty(container_ptr_ty), "");
        //         container_ptr_ty = self.program.raw_type(container_ptr_ty);
        //         let ret = result.reserve_slots(self, container_ptr_ty)?;
        //         result.push(Bc::Load {
        //             from: container_ptr.single(),
        //             to: ret,
        //         });
        //         container_ptr = ret;
        //     }
        // }
        // let container_ty = unwrap!(
        //     self.program.unptr_ty(container_ptr_ty),
        //     "unreachable unptr_ty {:?}",
        //     self.program.log_type(container_ptr_ty)
        // );
        //
        // let raw_container_ty = self.program.raw_type(container_ty);
        // match &self.program.types[raw_container_ty.0] {
        //     TypeInfo::Struct { fields, .. } => {
        //         let mut offset = 0;
        //         for f in fields {
        //             if f.name == name {
        //                 let f = *f;
        //                 let ty = self.program.find_interned(TypeInfo::Ptr(f.ty));
        //                 let ret = result.reserve_slots(self, ty)?;
        //                 let offset = if let Some(bytes) = f.ffi_byte_offset {
        //                     assert_eq!(bytes % 8, 0);
        //                     bytes / 8
        //                 } else {
        //                     offset
        //                 };
        //                 result.push(Bc::SlicePtr {
        //                     base: container_ptr.single(),
        //                     offset,
        //                     count: self.slot_count(f.ty),
        //                     ret: ret.single(),
        //                 });
        //                 return Ok((ret, ty).into());
        //             }
        //             offset += self.slot_count(f.ty);
        //         }
        //         err!(
        //             "unknown name {} on {:?}",
        //             self.program.pool.get(name),
        //             self.program.log_type(container_ty)
        //         );
        //     }
        //     TypeInfo::Enum { cases, .. } => {
        //         for (i, (f_name, f_ty)) in cases.iter().enumerate() {
        //             if *f_name == name {
        //                 let f_ty = *f_ty;
        //                 let ty = self.program.find_interned(TypeInfo::Ptr(f_ty));
        //                 let ret = result.reserve_slots(self, ty)?;
        //                 let count = self.slot_count(f_ty);
        //                 result.push(Bc::TagCheck {
        //                     enum_ptr: container_ptr.single(),
        //                     value: i as i64,
        //                 });
        //                 result.push(Bc::SlicePtr {
        //                     base: container_ptr.single(),
        //                     offset: 1,
        //                     count,
        //                     ret: ret.single(),
        //                 });
        //                 return Ok((ret, ty).into());
        //             }
        //         }
        //         err!(
        //             "unknown name {} on {:?}",
        //             self.program.pool.get(name),
        //             self.program.log_type(container_ty)
        //         );
        //     }
        //     _ => err!(
        //         "only structs/enums support field access but found {} = {}",
        //         self.program.log_type(container_ty),
        //         self.program.log_type(raw_container_ty)
        //     ),
        // }
        todo!()
    }

    pub fn slot_count(&mut self, ty: TypeId) -> usize {
        self.sizes.slot_count(self.program, ty)
    }
}
