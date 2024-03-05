#![allow(unused)]

use codemap::{CodeMap, Span};
use std::collections::HashMap;
use std::mem;
use std::ops::Deref;

use crate::ast::{Expr, FatExpr, FuncId, LazyType, Name, Program, Stmt, TypeId, TypeInfo};
use crate::ast::{FatStmt, Var};
use crate::compiler::{Compile, ExecTime, Executor, Res};
use crate::emit_bc::SizeCache;
use crate::experiments::aarch64::Three::*;
use crate::experiments::aarch64::Two::*;
use crate::experiments::aarch64::{Assembler, Inst, Mem, Reg, SP, X0};
use crate::interp::Interp;
use crate::logging::{assert_eq, err, ice, unwrap, LogTag};
use crate::logging::{outln, PoolLog};
use crate::parse::Parser;
use crate::pool::StringPool;
use crate::scope::ResolveScope;
use crate::{bc::*, emit_diagnostic, make_toplevel, LIB};
use crate::experiments::bc_to_asm::{BcToAsm, Jitted};

pub fn bootstrap() -> (String, String) {
    let pool = Box::leak(Box::<StringPool>::default());
    let mut codemap = CodeMap::new();
    let mut stmts = Vec::<FatStmt>::new();
    let mut libs: Vec<_> = LIB
        .iter()
        .map(|(name, code)| codemap.add_file(name.to_string(), code.to_string()))
        .collect();
    let user_span = libs.last().unwrap().span;
    for file in &libs {
        stmts.extend(Parser::parse(file.clone(), pool).unwrap());
    }

    let mut global = make_toplevel(pool, user_span, stmts);
    let vars = ResolveScope::of(&mut global, pool);
    let mut program = Program::new(vars, pool);
    let mut comp = Compile::new(pool, &mut program, Interp::new(pool));
    comp.add_declarations(global).unwrap();

    let (mut rs, mut comp) = EmitRs::emit_rs(comp).unwrap();

    let bs = comp.save_bootstrap.clone();
    for f in &bs {
        comp.compile(*f, ExecTime::Runtime).unwrap();
    }
    let mut asm = BcToAsm {
        interp: &comp.executor,
        program: &mut program,
        asm: Jitted::new(1<<26)  // Its just virtual memory right? I really don't want to ever run out of space and need to change the address.
    };
    asm.asm.reserve(asm.program.funcs.len());
    for f in &bs {
        asm.compile(*f).unwrap();
    }

    let symbol_bs = pool.intern("bs");
    let mut fr = String::new();
    for f in &bs {
        let bytes = unsafe { &*asm.asm.get_fn(*f).unwrap() };

        let annotations: String = asm.program.funcs[f.0]
            .annotations
            .iter()
            .filter(|a| a.name != symbol_bs)
            .map(|a| {
                assert!(a.args.is_none(), "TODO: args");
                format!("@{}", pool.get(a.name))
            })
            .collect();

        let sig = pool.get(asm.program.sig_str(*f).unwrap());
        let bytes: String = bytes
            .iter()
            .copied()
            .array_chunks::<4>()
            .map(|b| format!("{:#05x}, ", u32::from_le_bytes(b)))
            .collect();
        fr += &format!("\n{annotations} {sig} = (\n    {bytes}\n)!asm;\n")
    }

    (rs, fr)
}

pub struct EmitRs<'z, 'p: 'z, Exec: Executor<'p>> {
    comp: Compile<'z, 'p, Exec>,
    last_loc: Option<Span>,
    ready: Vec<Option<String>>,
    global_constants: HashMap<Var<'p>, (TypeId, String)>,
    func: Option<FuncId>,
}

const HEADER: &str =r##"
#![allow(non_snake_case)]
#![allow(unused)]
#![allow(non_upper_case_globals)]
#![allow(clippy::no_effect)]
#![allow(clippy::explicit_auto_deref)]
#![allow(clippy::deref_addrof)]

struct ShiftTy {
    LSL: i64,
    LSR: i64,
    ASR: i64
}

const Shift: &ShiftTy = &ShiftTy {
    LSL: 0b00,
    LSR: 0b01,
    ASR: 0b10,
};
"##;

impl<'z, 'p: 'z, Exec: Executor<'p>> EmitRs<'z, 'p, Exec> {
    pub fn emit_rs(e: Compile<'z, 'p, Exec>) -> Res<'p, (String, Compile<'z, 'p, Exec>)> {
        let mut emit = EmitRs::new(e);

        for f in 0..emit.comp.program.funcs.len() {
            if emit.comp.program.funcs[f].has_tag(emit.comp.program.pool, "rs") {
                emit.compile(FuncId(f))?;
            }
        }

        let constants: String = emit
            .global_constants
            .clone()
            .into_iter()
            .map(|(name, (ty, value))| {
                let name = emit.comp.pool.get(name.0).to_string();
                let ty = emit.emit_type(ty).unwrap();
                format!("const {name}: {ty} = {value};")
            })
            .collect();
        let functions: String = emit.ready.into_iter().flatten().collect();

        Ok((format!("{HEADER}\n\n{constants}\n\n{functions}\n"), emit.comp))
    }

    pub fn compile(&mut self, f: FuncId) -> Res<'p, ()> {
        while self.ready.len() <= f.0 {
            self.ready.push(None);
        }
        if self.ready[f.0].is_some() {
            return Ok(());
        }
        self.comp.compile(f, ExecTime::Runtime)?;
        let callees = self.comp.program.funcs[f.0]
            .wip
            .as_ref()
            .unwrap()
            .callees
            .clone();
        for c in callees {
            self.compile(c)?;
        }

        self.func = Some(f);
        let body = self.compile_inner(f)?;
        let func = &self.comp.program.funcs[f.0];
        let name = self.make_fn_name(f);
        let ty = func.unwrap_ty();
        let args = func.arg.flatten();
        let want_export = func.has_tag(self.comp.program.pool, "rs");
        let args: String = func
            .arg
            .flatten()
            .iter()
            .map(|(name, ty)| {
                let name = name.map(|name| self.comp.pool.get(name.0)).unwrap_or("_");
                let ty = self.emit_type(*ty).unwrap();
                format!("{}: {}, ", name, ty)
            })
            .collect();
        let ret = self.emit_type(ty.ret)?;
        self.ready[f.0] = Some(format!(
            "#[rustfmt::skip]\n{}fn {name}({args}) -> {ret} {{ \n{body}\n}}\n\n",
            if want_export { "pub " } else { "" }
        ));
        Ok(())
    }

    // If its just something we call not something we want to export, can mangle the name so overloads are allowed.
    fn make_fn_name(&self, f: FuncId) -> String {
        let func = &self.comp.program.funcs[f.0];
        let name = self.comp.program.pool.get(func.name).to_string();
        let ty = func.unwrap_ty();
        let want_export = func.has_tag(self.comp.program.pool, "rs");
        // TODO: doing this is bad because it means the names change every time because my typeids aren't deterministic
        //       so since im committing this it makes the diff noisy which annoys me
        // if want_export {
            name
        // } else {
        //     format!("{name}_{:?}{:?}", ty.arg, ty.ret)
        // }
    }

    fn new(comp: Compile<'z, 'p, Exec>) -> Self {
        Self {
            last_loc: None,
            comp,
            ready: vec![],
            global_constants: HashMap::new(),
            func: None,
        }
    }

    fn compile_inner(&mut self, f: FuncId) -> Res<'p, String> {
        let func = &self.comp.program.funcs[f.0];
        let wip = unwrap!(func.wip.as_ref(), "Not done comptime for {f:?}"); // TODO
        debug_assert!(!func.evil_uninit);
        let func = self.comp.program.funcs[f.0].clone(); // TODO: no clone
        if let Some(body) = func.body {
            let mut ret = self.compile_expr(&body);
            if let Err(e) = &mut ret {
                e.loc = self.last_loc;
            }
            ret
        } else {
            Ok(match self.comp.pool.get(func.name) {
                "shift_left" => String::from("value << shift_amount"),
                "bit_or" => String::from("a | b"),
                "bit_and" => String::from("a & b"),
                "bit_not" => String::from("!a"),
                "sub" => String::from("a - b"),
                "mul" => String::from("a * b"),
                "le" => String::from("a <= b"),
                "add" => String::from("a + b"),
                s => todo!("builtin {s}"),
            })
        }
    }

    fn emit_type(&mut self, ty: TypeId) -> Res<'p, String> {
        let info = self.comp.program.raw_type(ty);
        Ok(match &self.comp.program.types[info.0] {
            TypeInfo::Unknown => todo!(),
            TypeInfo::Any => todo!(),
            TypeInfo::Never => todo!(),
            TypeInfo::F64 => todo!(),
            TypeInfo::Int(int) => String::from("i64"),
            TypeInfo::Bool => String::from("bool"),
            TypeInfo::Fn(_) => todo!(),
            TypeInfo::FnPtr(_) => todo!(),
            TypeInfo::Tuple(_) => todo!(),
            TypeInfo::Ptr(_) => todo!(),
            TypeInfo::Slice(_) => todo!(),
            TypeInfo::Struct {
                fields,
                as_tuple,
                ffi_byte_align,
                ffi_byte_stride,
            } => todo!(),
            TypeInfo::Enum { cases } => todo!(),
            TypeInfo::Unique(_, _) => todo!(),
            TypeInfo::Named(_, _) => todo!(),
            TypeInfo::Type => todo!(),
            TypeInfo::Unit => String::from("()"),
            TypeInfo::VoidPtr => todo!(),
        })
    }

    fn compile_stmt(&mut self, stmt: &FatStmt<'p>) -> Res<'p, String> {
        Ok(match stmt.deref() {
            Stmt::Noop => String::new(),
            Stmt::Eval(expr) => {
                format!("{};\n", self.compile_expr(expr)?)
            }
            Stmt::DeclVar { name, value, .. } => {
                let name = self.comp.pool.get(name.0);
                let value = self.compile_expr(value.as_ref().unwrap())?;
                format!("let mut {} = {};", name, value)
            }
            Stmt::DeclVarPattern { binding, value } => {
                // Args of inlined function.
                if binding.bindings.len() == 1
                    && binding.bindings[0].name == Name::None
                    && binding.bindings[0].ty == LazyType::Finished(TypeId::unit())
                {
                    // Probably an if branch. But it could be a call with side-effects so should emit it anyway.
                    let value = value
                        .as_ref()
                        .map(|value| self.compile_expr(value))
                        .unwrap_or(Ok(String::new()));
                    format!("{};", value?)
                } else {
                    todo!()
                }
            }
            Stmt::Set { place, value } => todo!(),
            Stmt::DeclNamed { .. } | Stmt::DeclFunc(_) | Stmt::DoneDeclFunc(_) => unreachable!(),
        })
    }

    fn emit_value(&mut self, value: &Value) -> Res<'p, String> {
        Ok(match value {
            Value::I64(n) => format!("{n}"),
            Value::F64(_) => todo!(),
            Value::Bool(_) => todo!(),
            Value::Type(_) => todo!(),
            Value::GetFn(_) => todo!(),
            Value::Unit => String::from("()"),
            Value::Poison => todo!(),
            Value::InterpAbsStackAddr(_) => todo!(),
            Value::Heap {
                value,
                physical_first,
                physical_count,
            } => todo!(),
            Value::Symbol(_) => todo!(),
            Value::OverloadSet(_) => todo!(),
        })
    }

    fn compile_expr(&mut self, expr: &FatExpr<'p>) -> Res<'p, String> {
        Ok(match expr.deref() {
            Expr::WipFunc(_) => unreachable!(),
            Expr::Value { value, .. } => self.emit_values(value)?,
            Expr::Call(f, arg) => {
                if let Some(f_id) = f.as_fn() {
                    let func = &self.comp.program.funcs[f_id.0];
                    assert!(!func.has_tag(self.comp.program.pool, "comptime"));
                    let name = self.make_fn_name(f_id);
                    let args = self.compile_expr(arg)?;
                    format!("{name}({args})")
                } else {
                    todo!()
                }
            }
            Expr::Block {
                body, result: end, ..
            } => {
                let parts: Res<'p, String> = body.iter().map(|e| self.compile_stmt(e)).collect();
                format!("{{ {}\n {} }}", parts?, self.compile_expr(end)?)
            }
            Expr::Tuple(parts) => {
                let parts: Res<'p, Vec<String>> =
                    parts.iter().map(|e| self.compile_expr(e)).collect();
                parts?.join(", ")
            }
            Expr::SuffixMacro(name, arg) => match self.comp.pool.get(*name) {
                "deref" => format!("(*({}))", self.compile_expr(arg)?),
                "if" => {
                    if let Expr::Tuple(parts) = arg.deref().deref() {
                        let cond = self.compile_expr(&parts[0])?;
                        let if_true = self.compile_expr(&parts[1])?;
                        let if_false = self.compile_expr(&parts[2])?;
                        format!("(if {cond} {{ {if_true} }} else {{ {if_false} }})")
                    } else {
                        err!("malformed !if",)
                    }
                }
                s => todo!("!{s}"),
            },
            Expr::FieldAccess(container, name) => {
                format!(
                    "&(*{}).{}",
                    self.compile_expr(container)?,
                    self.comp.pool.get(*name)
                )
            }
            Expr::StructLiteralP(_) => todo!(),
            Expr::GetVar(var) => {
                // if let Some((value, ty)) = self.comp.program.funcs[self.func.unwrap().0]
                //     .closed_constants
                //     .get(*var)
                // {
                //     if !self.global_constants.contains_key(var) {
                //         let value = self.emit_values(&value)?;
                //         self.global_constants.insert(*var, (ty, value));
                //     }
                // }
                self.comp.pool.get(var.0).to_string()
            }
            Expr::PrefixMacro { .. }
            | Expr::ArrayLiteral(_)
            | Expr::RefType(_)
            | Expr::EnumLiteral(_)
            | Expr::Closure(_)
            | Expr::GetNamed(_)
            | Expr::String(_) => unreachable!(),
        })
    }

    fn emit_values(&mut self, value: &Values) -> Res<'p, String> {
        match value {
            Values::One(v) => self.emit_value(v),
            Values::Many(v) => {
                if v.len() == 1 {
                    self.emit_value(&v[0])
                } else {
                    todo!("{v:?}")
                }
            }
        }
    }
}
