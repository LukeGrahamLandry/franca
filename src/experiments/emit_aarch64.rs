#![allow(unused)]

use codemap::Span;
use std::collections::HashMap;
use std::mem;
use std::ops::Deref;

use crate::experiments::aarch64::{Assembler, Inst, Mem, Reg, SP, X0};
use crate::ast::{FatStmt, Var};
use crate::bc::*;
use crate::compiler::{Executor, Res};
use crate::emit_bc::SizeCache;
use crate::logging::PoolLog;
use crate::
    ast::{Expr, FatExpr, FuncId, Program, Stmt, TypeId, TypeInfo}
 ;
use crate::experiments::aarch64::Two::*;
use crate::experiments::aarch64::Three::*;
use crate::logging::{assert_eq, err, ice, unwrap};

#[derive(Default)]
pub struct AsmExecutor {
    sizes: SizeCache,
    ready: Vec<Option<FnDone>>,
}

struct FnDone {
    asm: Assembler,
}

#[derive(Debug, Clone, Copy)]
struct SpOffset(u64);

#[derive(Debug, Clone, Copy, PartialEq)]
enum Place {
    Mem(Mem),
    Reg(Reg),
}

pub struct EmitAsm<'z, 'p: 'z> {
    program: &'z Program<'p>,
    sizes: &'z mut SizeCache,
    last_loc: Option<Span>,
    places: HashMap<Var<'p>, Place>,
    open_reg: Vec<Reg>,
    asm: Assembler,
}

impl<'z, 'p: 'z> EmitAsm<'z, 'p> {
    pub fn compile(program: &'z Program<'p>, exec: &'z mut AsmExecutor, f: FuncId) -> Res<'p, ()> {
        while exec.ready.len() <= f.0 {
            exec.ready.push(None);
        }
        if exec.ready[f.0].is_some() {
            return Ok(());
        }
        let emit = EmitAsm::new(program, &mut exec.sizes);
        let body = emit.compile_inner(f)?;
        exec.ready[f.0] = Some(body);
        Ok(())
    }

    fn new(program: &'z Program<'p>, sizes: &'z mut SizeCache) -> Self {
        Self {
            last_loc: None,
            program,
            sizes,
            places: Default::default(),
            open_reg: vec![Reg {
                r: 1
            },Reg {
                r: 2
            }], // TODO
            asm: Assembler::default(),
        }
    }

    fn compile_inner(mut self, f: FuncId) -> Res<'p, FnDone> {
        let func = &self.program.funcs[f.0];
        let wip = unwrap!(func.wip.as_ref(), "Not done comptime for {f:?}"); // TODO
        debug_assert!(!func.evil_uninit);
        let func = &self.program.funcs[f.0];
        for (var, ty) in &wip.vars {}

        let ret = self.compile_expr(func.body.as_ref().unwrap(), Some(Place::Reg(X0)));
        match ret {
            Ok(ret) => {
                assert_eq!(ret, Place::Reg(X0));
                self.asm.push(Inst::Ret);
                Ok(FnDone {
                    asm: mem::take(&mut self.asm),
                })
            }
            Err(mut e) => {
                e.loc = self.last_loc;
                Err(e)
            }
        }
    }
    
    fn compile_stmt(
        &mut self,
        stmt: &FatStmt<'p>,
    ) -> Res<'p, ()> {
        match stmt.deref() {
            Stmt::Noop => {},
            Stmt::Eval(expr) => {
                self.compile_expr(expr, None)?;
            },
            Stmt::DeclVar { name, value, .. } => {
                if let Some(value) = value {
                    self.set_var(*name, value)?;
                }
            },
            Stmt::DeclVarPattern { .. } => todo!(),
            Stmt::Set { place, value } => {
                match place.deref() {
                    &Expr::GetVar(var) => self.set_var(var, value)?,
                    _ => ice!("TODO: Stmt::Set")
                }
            },
            Stmt::DeclNamed { .. } |
            Stmt::DeclFunc(_) |
            Stmt::DoneDeclFunc(_) => unreachable!()
        }
        Ok(())
    }
    
    fn set_var(&mut self, var: Var<'p>, value: &FatExpr<'p>) -> Res<'p, ()> {
        let dest = *unwrap!(self.places.get(&var), "no place {}", var.log(self.program.pool));
        let at = self.compile_expr(value, Some(dest))?;
        assert_eq!(dest, at);
        Ok(())
    }

    fn compile_expr(
        &mut self,
        expr: &FatExpr<'p>,
        dest: Option<Place>,
    ) -> Res<'p, Place> {
        let dest = match expr.deref() {
            Expr::WipFunc(_) => unreachable!(),
            Expr::Value { value, .. } => {
                let out = match dest {
                    Some(r) => r,
                    // TODO: return it
                    None => Place::Reg(unwrap!(self.open_reg.pop(), "TODO: out of registers")),
                };
                match value {
                Values::One(v) => match v {
                    Value::I64(i) => {
                        self.const_int(u64::from_ne_bytes(i.to_ne_bytes()), out)
                    }
                    Value::Bool(b) => {
                        self.const_int(if *b {1} else {0}, out)
                    },
                    // These only make sense during comptime execution but they're also really just numbers.
                    Value::OverloadSet(i) |  Value::GetFn(FuncId(i)) | Value::Type(TypeId(i)) | Value::Symbol(i) => {
                        self.const_int(*i as u64, out)
                    }
                    Value::Heap {
                        ..
                    } => todo!(),
                    
                    Value::F64(_) => todo!(),
                    Value::Unit => todo!(),
                    Value::Poison => todo!(),
                    
                    Value::InterpAbsStackAddr(_) => unreachable!(),
                },
                Values::Many(_) => todo!(),
            }
            out
            },
            Expr::Call(_, _) => todo!(),
            Expr::Block {
                body,
                result: end,
                ..
            } => {
                for stmt in body {
                    self.compile_stmt(stmt)?;
                }
                self.compile_expr(end, dest)?
            },
            Expr::Tuple(_) => todo!(),
            Expr::SuffixMacro(_, _) => todo!(),
            Expr::FieldAccess(_, _) => todo!(),
            Expr::StructLiteralP(_) => todo!(),
            Expr::GetVar(var) => {
                assert_eq!(self.slot_count(expr.ty), 8);
                let slot = *unwrap!(self.places.get(var), "no place {}", var.log(self.program.pool));
                match dest {
                    Some(dest) => {
                        self.set_place(dest, slot);
                        dest
                    },
                    None => slot,
                }
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
    
    fn compile_as_place(&mut self, expr: &FatExpr<'p>) -> Res<'p, Place>{ 
        match expr.deref() {
            Expr::GetVar(var) => Ok(*unwrap!(self.places.get(var), "")),
            // TODO: field access for compute offset
            // TODO: deref suffix macro
            _ => ice!("TODO: place expr {expr:?}")
        }
    }
    
    // Convert a statically known field offset to a runtime pointer of the field's type. 
    fn realize_offset(&mut self, value: Mem, dest: Reg) -> Res<'p, ()> {
        self.asm.push(Inst::WithImm  { op: ADD, dest, src: value.addr, imm: value.offset });
        Ok(())
    }
    
    fn set_place(&mut self, dest: Place, src: Place) {
        todo!()
    }
    
    fn load_usize(&mut self, dest: Reg, src: Place) {
        match src {
            Place::Mem(src) => {
                self.asm.push(Inst::Ldr { src, dest });
            },
            Place::Reg(src) => {
                if src != dest {
                    self.asm.push(Inst::Two { op: MOV, lhs: dest, rhs: src });
                }
            },
        }
    }
    
    fn store_usize(&mut self, dest: Place, src: Reg) {
        match dest {
            Place::Mem(dest) => {
                self.asm.push(Inst::Str { src, dest });
            },
            Place::Reg(dest) => {
                if src != dest {
                    self.asm.push(Inst::Two { op: MOV, lhs: dest, rhs: src });
                }
            },
        }
    }

    fn const_int(&mut self, imm: u64, dest: Place) {
        match dest {
            Place::Mem(dest) => {
                let reg = self.open_reg.pop().unwrap(); // TODO: -> Res<'p, ()>
                self.const_int(imm, Place::Reg(reg));
                self.store_usize(Place::Mem(dest), reg);
                self.open_reg.push(reg);
            },
            Place::Reg(dest) => {
                self.asm.push(Inst::MOVZ { dest, imm });  // TODO: other sizes
            },
        }
    }

    pub fn slot_count(&mut self, ty: TypeId) -> usize {
        self.sizes.byte_count(self.program, ty)
    }
}

impl SizeCache {
    pub fn byte_count(&mut self, program: &Program, ty: TypeId) -> usize {
        while self.known.len() <= ty.0 {
            self.known.push(None);
        }
        if let Some(size) = self.known[ty.0] {
            return size;
        }
        let ty = program.raw_type(ty);
        let size = match &program.types[ty.0] {
            TypeInfo::Unknown => 9999,
            TypeInfo::Tuple(args) => args.iter().map(|t| self.byte_count(program, *t)).sum(),
            TypeInfo::Struct { fields, .. } => {
                fields.iter().map(|f| self.byte_count(program, f.ty)).sum()
            }
            TypeInfo::Enum { cases, .. } => {
                1 + cases
                    .iter()
                    .map(|(_, ty)| self.byte_count(program, *ty))
                    .max()
                    .expect("no empty enum")
            }
            TypeInfo::Unit => 0,
            TypeInfo::Bool => 1,
            TypeInfo::Never | TypeInfo::Slice(_) => unreachable!(),
            TypeInfo::Any
            | TypeInfo::F64
            | TypeInfo::Int(_) // TODO: actually different int types
            | TypeInfo::Fn(_)
            | TypeInfo::Ptr(_)
            | TypeInfo::VoidPtr
            | TypeInfo::FnPtr(_)
            | TypeInfo::Type => 8,
            TypeInfo::Unique(_, _) | TypeInfo::Named(_, _) => unreachable!(),
        };
        self.known[ty.0] = Some(size);
        size
    }
}

impl<'p> PoolLog<'p> for AsmExecutor {
    fn log(&self, _: &crate::pool::StringPool<'p>) -> String {
        String::from("AsmExecutor")
    }
}

impl<'p> Executor<'p> for AsmExecutor {
    type SavedState = ();

    fn compile_func(&mut self, program: &Program<'p>, f: FuncId) -> Res<'p, ()> {
        EmitAsm::compile(program, self, f)
    }
    
    #[cfg(not(target_arch = "aarch64"))]
    fn run_func(&mut self, program: &mut Program<'p>, f: FuncId, arg: Values) -> Res<'p, Values> {
        err!("Jitted aarch64 is not supported on this platform.",)
    }

    #[cfg(target_arch = "aarch64")]
    fn run_func(&mut self, _: &mut Program<'p>, f: FuncId, arg: Values) -> Res<'p, Values> {
        let arg = arg.single()?.to_int()?; // TODO
        let func = unwrap!(self.ready[f.0].as_mut(), "not jitted");
        let code = func.asm.map_exec();
        let code: extern "C" fn(i64) -> i64 = unsafe { mem::transmute(code) };
        let res = code(arg);
        Ok(Value::I64(res).into())
    }

    fn run_continuation(&mut self, _: &mut Program<'p>, _: Values) -> Res<'p, Values> {
        todo!()
    }

    fn size_of(&mut self, program: &Program<'p>, ty: TypeId) -> usize {
        self.sizes.byte_count(program, ty)
    }

    fn is_ready(&self, f: FuncId) -> bool {
        self.ready.len() > f.0 && self.ready[f.0].is_some()
    }

    fn dump_repr(&self, _: &Program<'p>, f: FuncId) -> String {
        if let Some(f) = &self.ready[f.0] {
            f.asm.log()
        } else {
            String::from("NOT JITTED")
        }
    }

    fn tag_error(&self, _: &mut crate::compiler::CompileError<'p>) {
       // TODO
    }

    fn assertion_count(&self) -> usize {
        // TODO
        0
    }

    fn mark_state(&self) -> Self::SavedState {
        // TODO
    }

    fn restore_state(&mut self, _: Self::SavedState) {
        // TODO
    }

    fn run_with_arg<T: crate::experiments::reflect::Reflect>(&mut self, program: &mut Program<'p>, f: FuncId, arg: &mut T) -> Res<'p, ()> {
        todo!()
    }
    
    fn get_bc(&self, f: FuncId) -> Option<FnBody<'p>> {
        None
    }
}


#[allow(unused)]
#[cfg(target_arch = "aarch64")]
mod tests {
    use std::mem::transmute;
    use codemap::CodeMap;
    use crate::{ast::{garbage_loc, FatStmt, Program}, compiler::{Compile, ExecTime, Res}, logging::{err, ice, unwrap}, make_toplevel, parse::Parser, pool::StringPool, scope::ResolveScope};
    use super::{AsmExecutor, Value};

    fn jit_main<Arg, Ret>(src: &str, f: impl FnOnce(extern "C" fn(Arg) -> Ret)) -> Res<'_, ()> {
        let pool = Box::leak(Box::<StringPool>::default());
        let mut codemap = CodeMap::new();
        let file = codemap.add_file("main_file".into(), src.to_string());
        let stmts = Parser::parse(file.clone(), pool).unwrap();
        let mut global = make_toplevel(pool, garbage_loc(), stmts);
        let vars = ResolveScope::of(&mut global, pool);
        let mut program = Program::new(vars, pool);
        let mut comp = Compile::new(pool, &mut program, AsmExecutor::default());
        comp.add_declarations(global)?;
        let name = pool.intern("main");
        let main = unwrap!(comp.lookup_unique_func(name), "");
        comp.compile(main, ExecTime::Runtime)?;
        let asm = unwrap!(comp.executor.ready[main.0].as_mut(), "");
        asm.asm.encode_all();
        let code = asm.asm.map_exec();
        let code: extern "C" fn(Arg) -> Ret = unsafe{ transmute(code) };
        f(code);
        Ok(())
    }

    #[test]
    fn trivial() {
        jit_main("const i64; fn main() i64 = { 12 }", |f| {
            let ret: i64 = f(());
            assert_eq!(ret, 12);
        }).unwrap();
    }
}
