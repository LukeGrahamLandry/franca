use codemap::Span;
use std::collections::HashMap;
use std::ops::Deref;

use crate::ast::{Expr, FatExpr, Flag, FuncId, LazyType, Name, Program, Stmt, TargetArch, TypeId, TypeInfo, VarType};
use crate::ast::{FatStmt, Var};
use crate::bc_to_asm::emit_aarch64;
use crate::compiler::{Compile, ExecTime, Res};
use crate::logging::PoolLog;
use crate::pool::StringPool;
use crate::{bc::*, ice, load_program};
use crate::{err, unwrap};

pub fn bootstrap() -> (String, String) {
    let pool = Box::leak(Box::<StringPool>::default());
    let mut program = Program::new(pool, TargetArch::Aarch64, TargetArch::Aarch64);
    let mut comp = Compile::new(pool, &mut program);
    load_program(&mut comp, "").unwrap();

    let (rs, mut comp) = EmitRs::emit_rs(comp).unwrap();

    let bs = comp.save_bootstrap.clone();
    for f in &bs {
        comp.compile(*f, ExecTime::Runtime).unwrap();
    }

    for f in &bs {
        emit_aarch64(&mut comp, *f, ExecTime::Both).unwrap();
    }

    let mut fr = String::from("//! This file was @generated from lib/codegen/aarch64/basic.fr\n");
    for f in &bs {
        let bytes = unsafe { &*comp.aarch64.get_fn(*f).unwrap() };

        let annotations: String = comp.program[*f]
            .annotations
            .iter()
            .filter(|a| a.name != Flag::Bs.ident())
            .map(|a| {
                assert!(a.args.is_none(), "TODO: args");
                format!("#{} ", pool.get(a.name))
            })
            .collect();

        let sig = pool.get(comp.program.sig_str(*f).unwrap());
        let bytes: String = bytes
            .iter()
            .copied()
            .array_chunks::<4>()
            .map(|b| format!("{:#05x}, ", u32::from_le_bytes(b)))
            .collect();
        fr += &format!("\n{annotations}\n{sig} = (\n    {bytes}\n)!asm;\n")
        // TODO dont hardcode arch
    }

    (rs, fr)
}

pub struct EmitRs<'z, 'p: 'static> {
    comp: Compile<'z, 'p>,
    last_loc: Option<Span>,
    ready: Vec<Option<String>>,
    global_constants: HashMap<Var<'p>, (TypeId, String)>,
    func: Option<FuncId>,
}

const HEADER: &str = r##"
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

impl<'z, 'p: 'z> EmitRs<'z, 'p> {
    pub fn emit_rs(e: Compile<'z, 'p>) -> Res<'p, (String, Compile<'z, 'p>)> {
        let mut emit = EmitRs::new(e);

        for f in 0..emit.comp.program.funcs.len() {
            if emit.comp.program.funcs[f].has_tag(Flag::Rs) {
                emit.compile(FuncId::from_index(f))?;
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
        while self.ready.len() <= f.as_index() {
            self.ready.push(None);
        }
        if self.ready[f.as_index()].is_some() {
            return Ok(());
        }
        self.comp.compile(f, ExecTime::Runtime)?;
        let callees = self.comp.program[f].wip.as_ref().unwrap().callees.clone();
        for c in callees {
            if c.1 != ExecTime::Comptime {
                self.compile(c.0)?;
            }
        }

        self.func = Some(f);
        let body = self.compile_inner(f)?;
        let func = &self.comp.program[f];
        let name = self.make_fn_name(f);
        let ty = func.unwrap_ty();
        let want_export = func.has_tag(Flag::Rs);
        let args: String = func
            .arg
            .flatten()
            .iter()
            .map(|(name, ty, kind)| {
                assert_ne!(*kind, VarType::Const);
                let name = name.map(|name| self.comp.pool.get(name.0)).unwrap_or("_");
                let ty = self.emit_type(*ty).unwrap();
                format!("{}: {}, ", name, ty)
            })
            .collect();
        let ret = self.emit_type(ty.ret)?;
        self.ready[f.as_index()] = Some(format!(
            "#[rustfmt::skip]\n{}fn {name}({args}) -> {ret} {{ \n{body}\n}}\n\n",
            if want_export { "pub " } else { "" }
        ));
        Ok(())
    }

    // If its just something we call not something we want to export, can mangle the name so overloads are allowed.
    fn make_fn_name(&self, f: FuncId) -> String {
        let func = &self.comp.program[f];
        let name = self.comp.program.pool.get(func.name).to_string();
        let _ty = func.unwrap_ty();
        let _want_export = func.has_tag(Flag::Rs);
        // TODO: doing this is bad because it means the names change every time because my typeids aren't deterministic
        //       so since im committing this it makes the diff noisy which annoys me
        // if want_export {
        name
        // } else {
        //     format!("{name}_{:?}{:?}", ty.arg, ty.ret)
        // }
    }

    fn new(comp: Compile<'z, 'p>) -> Self {
        Self {
            last_loc: None,
            comp,
            ready: vec![],
            global_constants: HashMap::new(),
            func: None,
        }
    }

    fn compile_inner(&mut self, f: FuncId) -> Res<'p, String> {
        let func = &self.comp.program[f];
        let _ = unwrap!(func.wip.as_ref(), "Not done comptime for {f:?}"); // TODO
        debug_assert!(!func.evil_uninit);
        let func = self.comp.program[f].clone(); // TODO: no clone
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
        Ok(match &self.comp.program[info] {
            TypeInfo::Int(_) => String::from("i64"),
            TypeInfo::Bool => String::from("bool"),
            TypeInfo::Unit => String::from("()"),
            _ => todo!(),
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
                if binding.bindings.len() == 1 && binding.bindings[0].name == Name::None {
                    if let LazyType::Finished(ty) = binding.bindings[0].ty {
                        if !ty.is_unit() {
                            todo!()
                        }
                        // Probably an if branch. But it could be a call with side-effects so should emit it anyway.
                        let value = value.as_ref().map(|value| self.compile_expr(value)).unwrap_or(Ok(String::new()));
                        format!("{};", value?)
                    } else {
                        todo!()
                    }
                } else {
                    todo!(
                        "{binding:?} {:?} {}",
                        value.as_ref().map(|v| v.log(self.comp.pool)),
                        self.comp.pool.get(binding.bindings[0].name().unwrap())
                    )
                }
            }
            Stmt::Set { .. } => todo!(),
            Stmt::ExpandParsedStmts(_) | Stmt::DeclNamed { .. } | Stmt::DeclFunc(_) | Stmt::DoneDeclFunc(_, _) => unreachable!(),
        })
    }

    fn emit_value(&mut self, value: &Value) -> Res<'p, String> {
        Ok(match value {
            Value::I64(n) => format!("{n}"),
            Value::Unit => String::from("()"),
            _ => todo!(),
        })
    }

    fn compile_expr(&mut self, expr: &FatExpr<'p>) -> Res<'p, String> {
        Ok(match expr.deref() {
            Expr::GetParsed(_) | Expr::AddToOverloadSet(_) => unreachable!(),
            Expr::Poison => ice!("POISON",),
            Expr::Index { .. } => todo!(),
            Expr::WipFunc(_) => unreachable!(),
            Expr::Raw { .. } => todo!(),
            Expr::Value { value, .. } => self.emit_values(value)?,
            Expr::Call(f, arg) => {
                if let Some(f_id) = f.as_fn() {
                    let func = &self.comp.program[f_id];
                    assert!(!func.has_tag(Flag::Comptime));
                    let name = self.make_fn_name(f_id);
                    let args = self.compile_expr(arg)?;
                    format!("{name}({args})")
                } else {
                    todo!()
                }
            }
            Expr::Block { body, result: end, .. } => {
                let parts: Res<'p, String> = body.iter().map(|e| self.compile_stmt(e)).collect();
                format!("{{ {}\n {} }}", parts?, self.compile_expr(end)?)
            }
            Expr::Tuple(parts) => {
                let parts: Res<'p, Vec<String>> = parts.iter().map(|e| self.compile_expr(e)).collect();
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
                format!("&(*{}).{}", self.compile_expr(container)?, self.comp.pool.get(*name))
            }
            Expr::StructLiteralP(_) => todo!(),
            Expr::GetVar(var) => self.comp.pool.get(var.0).to_string(),
            Expr::PrefixMacro { .. } | Expr::Closure(_) | Expr::GetNamed(_) | Expr::String(_) => {
                unreachable!()
            }
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
