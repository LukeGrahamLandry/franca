// nobody cares its just logging. TODO: should do it anyway i guess.
#![allow(unused_must_use)]
use core::fmt;
use std::fmt::{Debug, Formatter, Write};

#[track_caller]
#[inline(never)]
pub fn break_here(e: &CErr) {
    let depth = unsafe { EXPECT_ERR_DEPTH.load(std::sync::atomic::Ordering::SeqCst) };
    if depth == 0 {
        // TODO: make this never happen so dont have to worry about short circuiting -- Apr 25
        println!("err: {e:?} at {}", std::panic::Location::caller());
    }
}

#[track_caller]
pub fn make_err(reason: CErr) -> Box<CompileError> {
    Box::new(CompileError {
        internal_loc: Some(std::panic::Location::caller()),
        loc: None,
        reason,
        trace: String::new(),
    })
}

#[macro_export]
macro_rules! err {
    ($payload:expr) => {{
        let e = $payload;
        $crate::logging::break_here(&e);

        return Err($crate::logging::make_err(e))
    }};
    ($($arg:tt)*) => {{
        let msg = format!($($arg)*);
        err!($crate::compiler::CErr::Fatal(msg))
    }};
}

#[macro_export]
macro_rules! assert {
    ($cond:expr, $($arg:tt)+) => {{
        if !($cond) {
            err!("Assertion Failed: {}\n{}", stringify!($cond), format!($($arg)*))
        }
    }};
    ($cond:expr $(,)?) => {{
        assert!($cond, "")
    }};
}

// looks weird cause i stole it from the rust one. apparently its faster or whatever
#[macro_export]
macro_rules! assert_eq {
    ($left:expr, $right:expr, $($arg:tt)+) => {
        match (&$left, &$right) {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    let msg = format!($($arg)*);
                    err!(
                        "Expected {} == {}\nBut {:?} != {:?}\n{}",
                        stringify!($left_val),
                        stringify!($right_val),
                        left_val,
                        right_val,
                        msg
                    )
                }
            }
        }
    };
    ($left:expr, $right:expr) => {
        assert_eq!($left, $right, "");
    };
}

// I want to be as easy to use my error system as just paniking.
// Use this one for things that aren't supported yet or should have been caught in a previous stage of compilation.
#[macro_export]
macro_rules! ice {
    ($($arg:tt)*) => {{
        let msg = format!($($arg)*);
        $crate::err!($crate::compiler::CErr::Fatal(msg))
    }};
}
pub use ice;

// TODO: compile errors should include the line number of the most recent ast node.

// TODO: have an enum for distringuishing ice from invalid input and use that everywhere instead of having two of each macro.
// TODO: I really like stringify! for error messages. make sure ast macros in my language have that.
// Convert a missing option into a compile error with a message.
#[macro_export]
macro_rules! unwrap {
    ($maybe:expr, $($arg:tt)*) => {{
        if let Some(value) = $maybe {
            value
        } else {
            $crate::ice!("Missing value {}.\n{}", stringify!($maybe), format!($($arg)*))
        }
    }};
}

pub use unwrap;

use crate::ast::{FatStmt, FnFlag, FuncImpl, Pattern};
use crate::pool::Ident;
use crate::{
    ast::{Expr, FatExpr, Func, FuncId, LazyType, Program, Stmt, TypeId, TypeInfo, Var},
    compiler::{CErr, CompileError, DebugState},
    pool::StringPool,
};
use crate::{bc::*, STATS};

/// It felt like a good idea to be able to compare identifiers super fast but now its forever a pain to look at them while debugging.
pub trait PoolLog<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String;
}

use crate::ast::safe_rec;
use crate::compiler::EXPECT_ERR_DEPTH;

impl<'p> Program<'p> {
    pub fn log_type(&self, t: TypeId) -> String {
        if let Some(&Some(name)) = self.inferred_type_names.get(t.as_index()) {
            let name = self.pool.get(name);
            // HACK: need to be smarter about when to infer an assignment as a type name.
            if name != "Self" {
                return name.to_string();
            }
        }

        safe_rec!(self, t, format!("{t:?}"), {
            if t.is_valid() && t.as_index() < self.types.len() {
                match &self[t] {
                    TypeInfo::Placeholder => "UnfinishedPlaceHolder".to_owned(),
                    TypeInfo::Never => "Never".to_owned(),
                    TypeInfo::F64 => "f64".to_owned(),
                    TypeInfo::Bool => "bool".to_owned(),
                    // TODO: be careful of recursion
                    TypeInfo::Ptr(e) => format!("*{}", self.log_type(*e)),
                    TypeInfo::Struct { fields, .. } => {
                        // TODO: factor out iter().join(str), how does that not already exist
                        let v: Vec<_> = fields
                            .iter()
                            .map(|f| format!("{}: {}", self.pool.get(f.name), self.log_type(f.ty)))
                            .collect();
                        format!("@struct({})", v.join(", "))
                    }
                    TypeInfo::Tagged { cases, .. } => {
                        // TODO: factor out iter().join(str), how does that not already exist
                        let v: Vec<_> = cases
                            .iter()
                            .map(|(name, ty)| format!("{}: {}", self.pool.get(*name), self.log_type(*ty)))
                            .collect();
                        format!("@tagged({})", v.join(", "))
                    }
                    TypeInfo::Enum { raw: inner, .. } => self.log_type(*inner),
                    TypeInfo::Named(_, n) => self.pool.get(*n).to_string(),
                    TypeInfo::Fn(f) => format!("(fn({}) {} #arity({}))", self.log_type(f.arg), self.log_type(f.ret), f.arity),
                    TypeInfo::FnPtr { ty: f, cc } => {
                        format!("&(fn({}) {} #{cc:?} #arity({}))", self.log_type(f.arg), self.log_type(f.ret), f.arity)
                    }
                    TypeInfo::Label(e) => format!("Label({})", self.log_type(*e)),

                    TypeInfo::Unit => "Unit".to_owned(),
                    TypeInfo::VoidPtr => "rawptr".to_owned(),
                    TypeInfo::Int(int) => {
                        format!("{}{}", if int.signed { "i" } else { "u" }, int.bit_count)
                    }
                    TypeInfo::Array { inner, len } => format!("Array({}, {len})", self.log_type(*inner)),
                }
            } else {
                "INVALID".to_string()
            }

            // format!("{t:?}={s}")
        })
    }

    pub fn find_ffi_type(&self, name: Ident<'p>) -> Option<TypeId> {
        let mut found = None;
        for ty in self.ffi_types.values() {
            if let &TypeInfo::Named(ty, check) = &self[*ty] {
                if name == check {
                    if found.is_some() {
                        println!("err: duplicate ffi name {}", self.pool.get(name));
                        return None;
                    }
                    found = Some(ty);
                } else if let TypeInfo::Tagged { cases, .. } = &self[ty] {
                    for (check, ty) in cases {
                        if *check == name {
                            if found.is_some() {
                                println!("err: duplicate ffi name {}", self.pool.get(name));
                                return None;
                            }
                            found = Some(*ty);
                        }
                    }
                }
            } else if let TypeInfo::Tagged { cases, .. } = &self[*ty] {
                for (check, ty) in cases {
                    if *check == name {
                        if found.is_some() {
                            println!("err: duplicate ffi name {}", self.pool.get(name));
                            return None;
                        }
                        found = Some(*ty);
                    }
                }
            }
        }
        found
    }

    pub fn dump_ffi_types(&mut self) -> String {
        let mut out = String::new();

        for info in &self.types {
            if let &TypeInfo::Named(ty, name) = info {
                if let TypeInfo::Tagged { cases, .. } = &self[ty] {
                    writeln!(out, "const {} = Unique$ @tagged(", self.pool.get(name),).unwrap();
                    for (name, mut ty) in cases {
                        if let &TypeInfo::Named(inner, _) = &self[ty] {
                            // Not unique, name is probably just name of the case.
                            ty = inner;
                        }
                        writeln!(out, "    {}: {},", self.pool.get(*name), self.log_type(ty)).unwrap();
                    }
                    out += ");\n"
                } else {
                    writeln!(out, "const {} = Unique({});", self.pool.get(name), self.log_type(ty)).unwrap();
                }
            }
        }
        out
    }
}

impl<'p> PoolLog<'p> for Stmt<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        self.logd(pool, 0)
    }
}

impl<'p> PoolLog<'p> for LazyType<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        match self {
            LazyType::EvilUnit => "EVIL_UNINIT !!!".into(),
            LazyType::Infer => "Infer".into(),
            LazyType::PendingEval(e) => e.log(pool),
            LazyType::Finished(ty) => format!("{:?}", ty),
            LazyType::Different(parts) => parts.iter().map(|p| p.log(pool)).collect(),
        }
    }
}
impl<'p> PoolLog<'p> for Pattern<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        // TODO: copy-paste. expr::StructLiteralP should
        let body: String = self
            .bindings
            .iter()
            .map(|b| format!("{}: ({}), ", b.name().map_or("_", |n| pool.get(n)), b.lazy().log(pool)))
            .collect();
        format!("({body})")
    }
}

impl<'p> PoolLog<'p> for Expr<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        self.logd(pool, 0)
    }
}

impl<'p> Expr<'p> {
    fn logd(&self, pool: &StringPool<'p>, depth: usize) -> String {
        unsafe { STATS.expr_fmt += 1 };
        match self {
            Expr::Call(func, arg) => {
                format!("{}({})", func.logd(pool, depth), arg.logd(pool, depth))
            }
            &Expr::GetNamed(i) => pool.get(i).to_string(),
            &Expr::String(i) => format!("\"{}\"", pool.get(i)),
            Expr::Block { body, result, .. } => {
                let es: Vec<_> = body
                    .iter()
                    .filter(|s| !matches!(s.stmt, Stmt::Noop) || !s.annotations.is_empty())
                    .filter(|s| {
                        // HACK to skip the useless args added by !if and !while.
                        if let Stmt::DeclVarPattern { binding, value } = &s.stmt {
                            if binding.bindings.len() == 1 && value.is_raw_unit() {
                                return false;
                            }
                        }
                        true
                    })
                    .map(|e| {
                        let s = e.logd(pool, depth + 1);
                        let a: String = e
                            .annotations
                            .iter()
                            .map(|a| format!("#{}({})", pool.get(a.name), a.args.as_ref().map(|f| f.log(pool)).unwrap_or_default()))
                            .collect();
                        format!("{a} {s}")
                    })
                    .collect();
                let es = es.join(";\n");
                format!(
                    "{{\n{}\n{}{}\n{}}}",
                    es,
                    "    ".repeat(depth + 1),
                    result.logd(pool, depth + 1),
                    "    ".repeat(depth)
                )
            }
            Expr::Tuple(args) => {
                let args: Vec<_> = args.iter().map(|e| e.logd(pool, depth)).collect();
                let args: String = args.join(", ");
                format!("[{}]", args)
            }
            Expr::Value { value } => format!("{:?}", value.bytes()),
            Expr::GetVar(v) => v.log(pool),
            Expr::Closure(f) => format!("closure(fn {:?})", pool.get(f.name)),
            Expr::SuffixMacro(i, e) => format!("{}!{}", e.logd(pool, depth), pool.get(*i)),
            Expr::StructLiteralP(pattern) => {
                let body: String = pattern
                    .bindings
                    .iter()
                    .map(|b| {
                        format!(
                            "{}: ({}) = {}, ",
                            b.name().map_or("_", |n| pool.get(n)),
                            b.lazy().log(pool),
                            b.default.as_ref().map(|e| e.log(pool)).unwrap_or_else(|| String::from("---"))
                        )
                    })
                    .collect();

                format!(".{{ {body} }}")
            }
            Expr::FieldAccess(container, name) => {
                format!("{}.{}", container.logd(pool, depth), pool.get(*name))
            }
            Expr::PrefixMacro { handler, arg, target } => {
                format!("[@{}({}) {}]", handler.logd(pool, depth), arg.logd(pool, depth), target.logd(pool, depth))
            }

            Expr::PtrOffset { ptr, bytes, name } => format!("{}.{}~{bytes}", ptr.logd(pool, depth), pool.get(*name)),
            Expr::Cast(inner) => format!("@@cast({})", inner.logd(pool, depth)),
            _ => format!("{:?}", self),
        }
    }
}

impl<'p> Stmt<'p> {
    pub(crate) fn logd(&self, pool: &StringPool<'p>, depth: usize) -> String {
        let s = match self {
            Stmt::DeclNamed { name, ty, value, kind } => format!("{kind:?} {}: {} = {};", pool.get(*name), ty.log(pool), value.logd(pool, depth)),
            Stmt::DeclVar { name, ty, value, .. } => format!("{:?} {}: {} = {};", name.kind, name.log(pool), ty.log(pool), value.logd(pool, depth)),
            Stmt::Eval(e) => e.logd(pool, depth),
            Stmt::DeclFunc(func) => format!("declare(fn {})", pool.get(func.name)),
            Stmt::Noop => "".to_owned(),
            Stmt::Set { place, value } => {
                format!("{} = {};", place.logd(pool, depth), value.logd(pool, depth))
            }
            Stmt::DeclVarPattern { binding, value } => {
                let body: String = binding
                    .bindings
                    .iter()
                    .map(|b| format!("{}: ({}), ", b.var().map_or("_".to_string(), |n| n.log(pool)), b.lazy().log(pool)))
                    .collect();

                format!("({body}) = {};", value.logd(pool, depth))
            }
            _ => format!("{:?}", self),
        };

        format!("{}{s}", "    ".repeat(depth))
    }
}

impl<'p> PoolLog<'p> for FatExpr<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        self.expr.log(pool)
    }
}

impl<'p> PoolLog<'p> for Var<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        use crate::ast::VarType;
        let kind = match self.kind {
            VarType::Let => "L",
            VarType::Var => "V",
            VarType::Const => "C",
        };
        format!("{}%{}{}s{}b{}", pool.get(self.name), self.id, kind, self.scope.as_index(), self.block)
    }
}

impl<'p> PoolLog<'p> for Func<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        if !self.get_flag(FnFlag::NotEvilUninit) {
            return "[UNINIT (wip/dropped)]".to_string();
        }
        format!(
            "[fn {} {:?} {} = \n \nBODY: \n{}\nEND\nARG: {}\n A:{:?}]\n{}\n",
            pool.get(self.name),
            self.name,
            self.ret.log(pool),
            self.body.log(pool),
            self.arg.log(pool), // TODO: better formatting.
            self.annotations.iter().map(|i| pool.get(i.name)),
            if self.capture_vars.is_empty() {
                String::from("Raw function, no captures.")
            } else {
                format!(
                    "Closure capturing: {:?}.",
                    self.capture_vars.iter().map(|v| v.log(pool)).collect::<Vec<_>>()
                )
            },
        )
    }
}

impl<'p> FnBody<'p> {
    pub fn log(&self, program: &Program<'p>) -> String {
        let mut f = String::new();

        writeln!(f, "=== Bytecode for {} ===", program.pool.get(self.name));
        for (b, insts) in self.blocks.iter().enumerate() {
            if insts.insts.len() == 1 && insts.insts[0] == Bc::NoCompile && insts.incoming_jumps == 0 {
                continue;
            }
            writeln!(
                f,
                "[b{b}({})]: ({} incoming. end height={})",
                insts.arg_slots, insts.incoming_jumps, insts.height
            );
            for (i, op) in insts.insts.iter().enumerate() {
                write!(f, "    {i}. {op:?}");
                match *op {
                    Bc::CallDirect { f: id, .. } => writeln!(
                        f,
                        "; {}", //({}) -> {} ",
                        program.pool.get(program[id].name),
                        // program.log_type(program[id].finished_arg.unwrap()),
                        // program.log_type(program[id].finished_ret.unwrap())
                    ),
                    Bc::AddrVar { id } => {
                        if let Some(&Some(name)) = self.var_names.get(id as usize) {
                            write!(f, "; {}", program.pool.get(name.name));
                        }
                        writeln!(f, "; {}", program.log_type(self.vars[id as usize]))
                    }
                    _ => writeln!(f),
                };
            }
        }
        writeln!(f, "===");
        f
    }
}

impl<'p> Debug for CompileError<'p> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "COMPILATION ERROR:")?;
        writeln!(f, "{:?}", self.reason)?;
        writeln!(f, "Internal: {}", self.internal_loc.unwrap())?;
        write!(f, "{}", self.trace)
    }
}

impl<'p> DebugState<'p> {
    pub fn log(&self, pool: &StringPool<'p>, _: &Program<'p>) -> String {
        match self {
            DebugState::Msg(s) => s.clone(),
            DebugState::Compile(f, i) => format!("| Compile    | {:?} {}", *f, pool.get(*i)),
            DebugState::EnsureCompiled(f, i, when) => format!("| Ensure Comp | {:?} {} for {:?}", *f, pool.get(*i), when),
            DebugState::EmitBody(f, i) => format!("|  Emit Body  | {:?} {}", *f, pool.get(*i)),
            DebugState::EvalConstants(f, i) => format!("| Eval Consts | {:?} {}", *f, pool.get(*i)),
            DebugState::EmitCapturingCall(f, i) => format!("| Captur Call | {:?} {}", *f, pool.get(*i)),
            DebugState::ResolveFnRef(v) => format!("| Find Fn | {}", v.log(pool)),
            DebugState::RunInstLoop(f, i) => format!("| Execute  | {:?} {}", *f, pool.get(*i)),
            DebugState::ComptimeCall(f, i) => format!("| Comptime Call | {:?} {}", *f, pool.get(*i)),
            DebugState::ResolveFnType(f, i) => format!("| Resolve Type| {:?} {}", *f, pool.get(*i)),
            DebugState::ResolveConstant(v) => format!("| Find Const | {}", v.log(pool)),
            _ => format!("{self:?}"),
        }
    }
}

impl<'p> CErr<'p> {
    pub fn log(&self, program: &Program<'p>, pool: &StringPool<'p>) -> String {
        match self {
            &CErr::UndeclaredIdent(i) => format!("Undeclared Ident: {i:?} = {}", pool.get(i)),
            &CErr::TypeCheck(found, expected, msg) => format!(
                "{msg}. Type check expected {expected:?} = {} but found {found:?} = {}\n{}",
                program.log_type(expected),
                program.log_type(found),
                program.conversion_help(found, expected, 1)
            ),
            CErr::Fatal(s) => s.clone(),
            _ => format!("{:?}", self),
        }
    }
}

impl<'p> Program<'p> {
    fn conversion_help(&self, found: TypeId, expected: TypeId, depth: usize) -> String {
        let indent = "  ".repeat(depth);
        if found == expected {
            return format!("{indent} ICE: these are the same type");
        }
        let found = self.raw_type(found);
        let expected = self.raw_type(expected);
        if found == expected {
            return format!(
                "{indent} `@as({}) <value>` can be used to convert between a unique type and its backing representation.",
                self.log_type(expected)
            );
        }

        match (&self[found], &self[expected]) {
            (_, TypeInfo::Never) => {
                format!(
                    "{indent} An expression with type Never should... never return to its caller. use `unreachable()` to assert that a codepath is never executed.",
                )
            }
            // :Coercion // TODO: only one direction makes sense
            (TypeInfo::Never, _) => {
                format!(
                    "ICE: Never should typecheck as any value. for now use `unreachable_hack({})` to fool the type system into thinking it has a valid value of some type, but really it just crashes if the codepath is ever reached.",
                    self.log_type(expected)
                )
            }
            (TypeInfo::Int(f), TypeInfo::Int(e)) => {
                if !sane_int_size(f.bit_count) || !sane_int_size(e.bit_count) {
                    return "TODO: error message".to_string();
                }
                let fname = format!("{}{}", if f.signed { "i" } else { "u" }, f.bit_count);
                let ename = format!("{}{}", if e.signed { "i" } else { "u" }, e.bit_count);
                // TODO: && !f.signed && !e.signed discuss preserving the sign bit.
                if e.bit_count > f.bit_count {
                    let warn = if f.signed {
                        "since zext does not extend the sign bits, negative inputs result in high positive numbers.".to_string()
                    } else {
                        format!("{ename} can represent all values of {fname} so this conversion is safe.")
                    };
                    return format!(
                        "{indent}trying to convert {fname} to larger integer {ename}. \n{indent}use `zext(<value>)` to fill the extra space with zeros. \n{indent}{warn}"
                    );
                }
                if e.bit_count < f.bit_count {
                    let warn = if f.signed && !e.signed {
                        "converting signed to unsigned will leave low order sign bits, so negative numbers become high positive numbers"
                    } else {
                        ""
                    };
                    return format!(
                        "{indent}trying to convert {fname} to smaller integer {ename}. \n{indent}use `trunc(<value>)` to chop off the most signifigant bits. \n{indent}{warn}"
                    );
                }

                if e.bit_count == f.bit_count {
                    let warn = if e.signed && !f.signed {
                        "Large unsigned values become negative signed values."
                    } else if !e.signed && f.signed {
                        "Negative signed values become large unsiged values."
                    } else {
                        "Internal Compiler Error"
                    };
                    return format!("{indent}Trying to convert {fname} to {ename} (same size but different sign).\n{indent}Use `bitcast(<value>)` to preserve the bits.\n{indent}Values within range of the target type (small and positive) will be preserved. \n{indent}{warn}");
                }

                String::from("TODO: help message")
            }
            // TODO: not this!!!
            (TypeInfo::Struct { fields: f, .. }, TypeInfo::Struct { fields: e, .. }) => {
                if f.len() != e.len() {
                    return format!(
                        "{indent} Structs have a different number of fields. expected {} but found {}",
                        e.len(),
                        f.len()
                    );
                }
                // TODO: check if they are structuraly the same but don't typecheck because structs are nominal.
                // if f.len() == e.len() {
                //     let ok = f
                //         .iter()
                //         .zip(e.iter())
                //         .all(|(f, e)| self.coerce_type_check_arg(f.ty, e.ty, msg).is_ok() && f.byte_offset == e.byte_offset && f.name == e.name);
                //     if ok {
                //         return Ok(());
                //     }
                // }
                String::from("TODO: help message")
            }
            (&TypeInfo::Ptr(f), &TypeInfo::Ptr(e)) => {
                // TODO: recurse
                format!("{indent} you can (UNSAFELY) cast between pointers with ptr_cast_unchecked(From = {}, To = {}, ptr = <value>). this cast is a noop at runtime.",
                    self.log_type(f),
                    self.log_type(e),
                )
            }
            (&TypeInfo::Ptr(f), TypeInfo::VoidPtr) => {
                format!(
                    "{indent} you can coerce to a rawptr with `raw_from_ptr({}, <value>)`. this cast is a noop at runtime.",
                    self.log_type(f)
                )
            }
            (TypeInfo::VoidPtr, &TypeInfo::Ptr(e)) => {
                format!(
                    "{indent} you can (UNSAFELY) coerce from a rawptr with `ptr_from_raw({}, <value>)`. this cast is a noop at runtime.",
                    self.log_type(e)
                )
            }

            (&TypeInfo::Fn(_), &TypeInfo::Fn(_)) => format!("{indent} incompatible function types. TODO: correct varience tracking."),
            (&TypeInfo::FnPtr { .. }, &TypeInfo::FnPtr { .. }) => {
                format!("{indent} incompatible function types. TODO: correct varience tracking.")
            }
            (_, &TypeInfo::Fn(_)) => {
                if found == TypeId::overload_set {
                    format!("{indent} use @resolve to convert from a function to an overload set. ")
                } else {
                    format!("{indent} bad conversion")
                }
            }
            _ => format!("{indent} bad conversion"),
        }
    }
}

impl<'p> FatStmt<'p> {
    pub fn log_annotations(&self, pool: &StringPool<'p>) -> String {
        self.annotations.iter().map(|a| pool.get(a.name)).collect()
    }
}

impl Debug for FuncId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Fn{}", self.as_index())
    }
}

impl<'p> PoolLog<'p> for FuncImpl<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        match self {
            FuncImpl::Normal(e) => e.log(pool),
            FuncImpl::DynamicImport(_) => todo!(),
            FuncImpl::ComptimeAddr(n) => format!("ComptimeAddr({n})"),
            FuncImpl::JittedAarch64(c) => format!("JittedAarch64({c:?})"),
            &FuncImpl::LlvmIr(ir) => pool.get(ir).to_string(),
            FuncImpl::EmitCranelift(n) => format!("EmitCranelift({n})"),
            FuncImpl::PendingRedirect { .. } => todo!(),
            FuncImpl::Redirect(n) => format!("Redirect({n:?})"),
            FuncImpl::Merged(parts) => parts.iter().map(|p| p.log(pool)).collect(),
            FuncImpl::Empty => String::from("Unknown"),
            &FuncImpl::CSource(ir) => pool.get(ir).to_string(),
        }
    }
}

fn sane_int_size(bits: i64) -> bool {
    matches!(bits, 8 | 16 | 32 | 64 | 128)
}

impl Debug for PrimSig {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let render = |f: &mut Formatter, slots: u16, float_mask: u32| -> fmt::Result {
            write!(f, "(")?;
            for i in 0..slots as usize {
                write!(f, "{},", if is_float(i, slots, float_mask) { "f64" } else { "i64" })?
            }
            write!(f, ")")
        };

        render(f, self.arg_slots, self.arg_float_mask)?;
        write!(f, " -> ")?;
        render(f, self.ret_slots, self.ret_float_mask)?;
        if self.first_arg_is_indirect_return {
            write!(f, "#indirect")?;
        }
        if self.no_return {
            write!(f, "#never")?;
        }
        Ok(())
    }
}
