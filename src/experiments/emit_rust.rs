#![allow(unused)]

use codemap::Span;
use std::collections::HashMap;
use std::mem;
use std::ops::Deref;

use crate::ast::{Expr, FatExpr, FuncId, Program, Stmt, TypeId, TypeInfo};
use crate::ast::{FatStmt, Var};
use crate::bc::*;
use crate::compiler::{Executor, Res};
use crate::emit_bc::SizeCache;
use crate::experiments::aarch64::Three::*;
use crate::experiments::aarch64::Two::*;
use crate::experiments::aarch64::{Assembler, Inst, Mem, Reg, SP, X0};
use crate::logging::PoolLog;
use crate::logging::{assert_eq, err, ice, unwrap};

#[derive(Default)]
pub struct RsDone {
    sizes: SizeCache,
    ready: Vec<Option<String>>,
}

pub struct EmitRs<'z, 'p: 'z> {
    program: &'z Program<'p>,
    sizes: &'z mut SizeCache,
    last_loc: Option<Span>,
    out: String,
}

impl<'z, 'p: 'z> EmitRs<'z, 'p> {
    pub fn compile(program: &'z Program<'p>, exec: &'z mut RsDone, f: FuncId) -> Res<'p, ()> {
        while exec.ready.len() <= f.0 {
            exec.ready.push(None);
        }
        if exec.ready[f.0].is_some() {
            return Ok(());
        }
        let emit = EmitRs::new(program, &mut exec.sizes);
        let body = emit.compile_inner(f)?;
        exec.ready[f.0] = Some(body);
        Ok(())
    }

    fn new(program: &'z Program<'p>, sizes: &'z mut SizeCache) -> Self {
        Self {
            last_loc: None,
            program,
            sizes,
            out: String::new(),
        }
    }

    fn compile_inner(mut self, f: FuncId) -> Res<'p, String> {
        let func = &self.program.funcs[f.0];
        let wip = unwrap!(func.wip.as_ref(), "Not done comptime for {f:?}"); // TODO
        debug_assert!(!func.evil_uninit);
        let func = &self.program.funcs[f.0];
        let mut ret = self.compile_expr(func.body.as_ref().unwrap());
        if let Err(e) = &mut ret {
            e.loc = self.last_loc;
        }
        ret
    }

    fn compile_stmt(&mut self, stmt: &FatStmt<'p>) -> Res<'p, ()> {
        match stmt.deref() {
            Stmt::Noop => {}
            Stmt::Eval(expr) => {
                self.compile_expr(expr)?;
            }
            Stmt::DeclVar { name, value, .. } => {
                todo!()
            }
            Stmt::DeclVarPattern { .. } => todo!(),
            Stmt::Set { place, value } => todo!(),
            Stmt::DeclNamed { .. } | Stmt::DeclFunc(_) | Stmt::DoneDeclFunc(_) => unreachable!(),
        }
        Ok(())
    }

    fn compile_expr(&mut self, expr: &FatExpr<'p>) -> Res<'p, String> {
        let dest = match expr.deref() {
            Expr::Value { value, .. } => {
                todo!()
            }
            Expr::Call(_, _) => todo!(),
            Expr::Block {
                body, result: end, ..
            } => {
                for stmt in body {
                    self.compile_stmt(stmt)?;
                }
                self.compile_expr(end)?
            }
            Expr::Tuple(_) => todo!(),
            Expr::SuffixMacro(_, _) => todo!(),
            Expr::FieldAccess(_, _) => todo!(),
            Expr::StructLiteralP(_) => todo!(),
            Expr::GetVar(var) => {
                todo!()
            }
            Expr::PrefixMacro { .. }
            | Expr::ArrayLiteral(_)
            | Expr::RefType(_)
            | Expr::EnumLiteral(_)
            | Expr::Closure(_)
            | Expr::GetNamed(_)
            | Expr::String(_) => unreachable!(),
        };
        Ok(dest)
    }
    pub fn slot_count(&mut self, ty: TypeId) -> usize {
        self.sizes.byte_count(self.program, ty)
    }
}
