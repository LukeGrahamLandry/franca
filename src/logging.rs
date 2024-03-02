// nobody cares its just logging. TODO: should do it anyway i guess.
#![allow(unused_must_use)]
use core::fmt;
use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::{Debug, Write};
use std::{fs, mem};

macro_rules! bin_int {
    ($self:expr, $op:tt, $arg:expr, $res:expr) => {{
        let (a, b) = $arg.load_int_pair()?;
        $res(a $op b).into()
    }};
}

macro_rules! err {
    ($payload:expr) => {{
        return Err($crate::compiler::CompileError {
            internal_loc: std::panic::Location::caller(),
            loc: None,
            reason: $payload,
            trace: String::new(),
            value_stack: vec![],
            call_stack: String::new(),
        })
    }};
    ($($arg:tt)*) => {{
        let msg = format!($($arg)*);
        err!($crate::compiler::CErr::Msg(msg))
    }};
}

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
macro_rules! ice {
    ($($arg:tt)*) => {{
        let msg = format!($($arg)*);
        err!(crate::compiler::CErr::IceFmt(msg))
    }};
}
use codemap::Span;
pub(crate) use ice;

// TODO: compile errors should include the line number of the most recent ast node.

// TODO: have an enum for distringuishing ice from invalid input and use that everywhere instead of having two of each macro.
// TODO: I really like stringify! for error messages. make sure ast macros in my language have that.
// Convert a missing option into a compile error with a message.
macro_rules! unwrap {
    ($maybe:expr, $($arg:tt)*) => {{
        if let Some(value) = $maybe {
            value
        } else {
            ice!("Missing value {}.\n{}", stringify!($maybe), format!($($arg)*))
        }
    }};
}

pub(crate) use unwrap;

// Note: if you add more, make sure nobody's using 255 to mean 'all'.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogTag {
    Parsing = 0,
    Scope = 1,
    InitialAst = 2,
    Bytecode = 3,
    FinalAst = 4,
    ShowPrint = 5,
    ShowErr = 6,
    Macros = 7,
    Generics = 8,
    Jitted = 9,
    RtRust = 10,
    _Last = 11,
}

pub struct LogSettings {
    pub track: u64,
    pub logs: Vec<Vec<String>>,
}

thread_local! {
    pub static LOG: RefCell<LogSettings> = RefCell::new(LogSettings {
        track: 0,
        logs: vec![],
    });
}

pub fn init_logs(want: &[LogTag]) {
    let mut flag = 0;
    for tag in want {
        flag |= 1 << (*tag as usize);
    }
    init_logs_flag(flag);
}

pub fn init_logs_flag(want: u64) {
    LOG.with(|settings| {
        let mut settings = settings.borrow_mut();
        settings.track = want;
        settings.logs = vec![vec![]; LogTag::_Last as usize + 1];
    });
}

pub fn get_logs(kind: LogTag) -> String {
    LOG.with(|settings| settings.borrow().logs[kind as usize].join("\n"))
}

pub fn save_logs(folder: &str) {
    for i in 0..(LogTag::_Last as usize) {
        let tag: LogTag = unsafe { mem::transmute(i as u8) };
        let s = get_logs(tag);

        #[cfg(target_arch = "wasm32")]
        {
            unsafe { crate::web::show_log(i, s.as_ptr(), s.len()) };
        }
        #[cfg(not(target_arch = "wasm32"))]
        {
            fs::write(format!("{folder}/{tag:?}.log"), s).unwrap();
        }
    }
}

macro_rules! outln {
    ($tag:expr, $($arg:tt)*) => {{
        if cfg!(feature = "println_now") && $tag == crate::logging::LogTag::ShowPrint {
            println!($($arg)*);

        } else if cfg!(feature = "some_log") {
            let tag: crate::logging::LogTag = $tag;
            crate::logging::LOG.with(|settings| {
                if (settings.borrow().track & (1 << tag as usize)) != 0 {
                    settings.borrow_mut().logs[tag as usize].push(format!($($arg)*));
                }
            })
        } else if $tag == crate::logging::LogTag::ShowPrint || $tag == crate::logging::LogTag::ShowErr {
            let tag: crate::logging::LogTag = $tag;
            crate::logging::LOG.with(|settings| {
                settings.borrow_mut().logs[tag as usize].push(format!($($arg)*));
            })
        }
    }};
}

pub(crate) use outln;

macro_rules! log {
    ($($arg:tt)*) => {{
        if cfg!(feature = "spam_log") {
            print!($($arg)*);
        }
    }};
}
pub(crate) use log;

macro_rules! logln {
    // Using cfg!(...) instead of #[cfg(...)] to avoid unused var warnings.
    ($($arg:tt)*) => {{
        if cfg!(feature = "spam_log") {
            println!($($arg)*);
        }
    }};
}
pub(crate) use logln;

pub(crate) use assert;
pub(crate) use assert_eq;
pub(crate) use bin_int;
pub(crate) use err;

use crate::ast::FatStmt;
use crate::bc::*;
use crate::emit_bc::DebugInfo;
use crate::pool::Ident;
use crate::{
    ast::{Expr, FatExpr, Func, FuncId, LazyType, Program, Stmt, TypeId, TypeInfo, Var},
    compiler::{CErr, CompileError, DebugState},
    interp::Interp,
    pool::StringPool,
};

/// It felt like a good idea to be able to compare identifiers super fast but now its forever a pain to look at them while debugging.
pub trait PoolLog<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String;
}

use crate::ast::safe_rec;
impl<'p> Program<'p> {
    pub fn log_type(&self, t: TypeId) -> String {
        safe_rec!(self, t, format!("{t:?}"), {
            match &self.types[t.0] {
                TypeInfo::Unknown => "Unknown".to_owned(),
                TypeInfo::Any => "Any".to_owned(),
                TypeInfo::Never => "Never".to_owned(),
                TypeInfo::F64 => "f64".to_owned(),
                TypeInfo::Bool => "bool".to_owned(),
                // TODO: be careful of recursion
                TypeInfo::Ptr(e) => format!("(&{})", self.log_type(*e)),
                TypeInfo::Slice(e) => format!("([]{})", self.log_type(*e)),
                TypeInfo::Struct { fields, .. } => {
                    // TODO: factor out iter().join(str), how does that not already exist
                    let v: Vec<_> = fields
                        .iter()
                        .map(|f| format!("{}: {}", self.pool.get(f.name), self.log_type(f.ty)))
                        .collect();
                    format!("{{ {}}}!struct", v.join(", "))
                }
                TypeInfo::Enum { cases, .. } => {
                    // TODO: factor out iter().join(str), how does that not already exist
                    let v: Vec<_> = cases
                        .iter()
                        .map(|(name, ty)| {
                            format!("{}: {}", self.pool.get(*name), self.log_type(*ty))
                        })
                        .collect();
                    format!("{{ {}}}!enum", v.join(", "))
                }
                TypeInfo::Unique(inner, _) => self.log_type(*inner),
                TypeInfo::Named(_, n) => self.pool.get(*n).to_string(),
                TypeInfo::Fn(f) => format!("fn({}) {}", self.log_type(f.arg), self.log_type(f.ret)),
                TypeInfo::FnPtr(f) => {
                    format!("&(fn({}) {})", self.log_type(f.arg), self.log_type(f.ret))
                }
                TypeInfo::Tuple(v) => {
                    let v: Vec<_> = v.iter().map(|v| self.log_type(*v)).collect();
                    format!("({})", v.join(", "))
                }
                TypeInfo::Type => "Type".to_owned(),
                TypeInfo::Unit => "Unit".to_owned(),
                TypeInfo::VoidPtr => "(&Void)".to_owned(),
                TypeInfo::Int(int) => {
                    format!("{}{}", if int.signed { "i" } else { "u" }, int.bit_count)
                }
            }
            // format!("{t:?}={s}")
        })
    }

    // Note: be careful this can't get into a recursive loop trying to pretty print stuff.
    pub fn log_cached_types(&self) -> String {
        let mut s = String::new();
        writeln!(s, "=== {} CACHED TYPES ===", self.types.len());
        for (i, ty) in self.types.iter().enumerate() {
            writeln!(
                s,
                "- {:?} = {} = {:?};",
                TypeId(i),
                self.log_type(TypeId(i)),
                ty,
            );
        }
        writeln!(s, "====================");
        s
    }

    pub fn log_finished_ast(&self, start: FuncId) -> String {
        let mut done = HashSet::new();
        let mut pending = vec![start];
        let mut out = String::new();
        let mut const_reads = HashSet::new();

        while let Some(next) = pending.pop() {
            if !done.insert(next) {
                continue;
            }

            let func = &self.funcs[next.0];
            if let Some(body) = &func.body {
                collect_func_references(body, &mut pending, &mut const_reads);
                out += &format!(
                    "{next:?}: [fn {:?}=Name={:?} Arg={} -> Ret={}] = \nBODY: \n{}\nEND\n{}\nCONSTS:\n",
                    if func.referencable_name {
                        func.synth_name(self.pool)
                    } else {
                        "@anon@"
                    },
                    func.get_name(self.pool),
                    self.log_type(func.unwrap_ty().arg),
                    self.log_type(func.unwrap_ty().ret),
                    func.body
                        .as_ref()
                        .map(|e| e.log(self.pool))
                        .unwrap_or_else(|| "@NO_BODY@".to_owned()),
                    if func.capture_vars.is_empty() {
                        String::from("Raw function, no captures.")
                    } else {
                        format!(
                            "Closure capturing: {:?}.",
                            func.capture_vars
                                .iter()
                                .map(|v| v.log(self.pool))
                                .collect::<Vec<_>>()
                        )
                    },
                );
                for c in const_reads.drain() {
                    if let Some((val, ty)) = func.closed_constants.get(c) {
                        out += &format!(
                            "const {:?}: {} = {:?};\n",
                            c.log(self.pool),
                            self.log_type(ty),
                            val
                        );
                        val.vec()
                            .iter()
                            .for_each(|v| collect_func_references_value(v, &mut pending));
                    }
                }
                out += "=======================================\n\n\n\n";
            }
        }

        out
    }

    pub fn find_ffi_type(&self, name: Ident<'p>) -> Option<TypeId> {
        let mut found = None;
        for ty in self.ffi_types.values() {
            if let TypeInfo::Unique(ty, _) = &self.types[ty.0] {
                if let &TypeInfo::Named(ty, check) = &self.types[ty.0] {
                    if name == check {
                        if found.is_some() {
                            outln!(
                                LogTag::ShowErr,
                                "duplicate ffi name {}",
                                self.pool.get(name)
                            );
                            return None;
                        }
                        found = Some(ty);
                    } else if let TypeInfo::Enum { cases, .. } = &self.types[ty.0] {
                        for (_, mut ty) in cases {
                            if let &TypeInfo::Named(inner, case_name) = &self.types[ty.0] {
                                // TODO: ffi cases that are akreayd named types
                                if case_name == name {
                                    if found.is_some() {
                                        outln!(
                                            LogTag::ShowErr,
                                            "duplicate ffi name {}",
                                            self.pool.get(name)
                                        );
                                        return None;
                                    }
                                    found = Some(inner);
                                }
                            }
                        }
                    }
                }
            }
        }
        found
    }

    pub fn dump_ffi_types(&mut self) -> String {
        let mut out = String::new();

        for info in &self.types {
            if let TypeInfo::Unique(ty, _) = info {
                if let &TypeInfo::Named(ty, name) = &self.types[ty.0] {
                    if let TypeInfo::Enum { cases, .. } = &self.types[ty.0] {
                        writeln!(out, "const {} = Unique({{", self.pool.get(name),).unwrap();
                        for (name, mut ty) in cases {
                            if let &TypeInfo::Named(inner, _) = &self.types[ty.0] {
                                // Not unique, name is probably just name of the case.
                                ty = inner;
                            }
                            writeln!(out, "    {}: {},", self.pool.get(*name), self.log_type(ty))
                                .unwrap();
                        }
                        out += "}!enum);\n"
                    } else {
                        writeln!(
                            out,
                            "const {} = Unique({});",
                            self.pool.get(name),
                            self.log_type(ty)
                        )
                        .unwrap();
                    }
                }
            }
        }
        out
    }
}

fn collect_func_references_stmt<'p>(
    stmt: &Stmt<'p>,
    refs: &mut Vec<FuncId>,
    const_reads: &mut HashSet<Var<'p>>,
) {
    match stmt {
        Stmt::DoneDeclFunc(_) | Stmt::Noop => {}
        Stmt::Eval(e) => collect_func_references(e, refs, const_reads),
        Stmt::DeclVar { value, .. } => {
            if let Some(v) = value {
                collect_func_references(v, refs, const_reads);
            }
        }
        Stmt::Set { place, value } => {
            collect_func_references(place, refs, const_reads);
            collect_func_references(value, refs, const_reads);
        }
        Stmt::DeclNamed { .. } | Stmt::DeclFunc(_) => {
            unreachable!("finished ast contained {stmt:?}")
        }
        Stmt::DeclVarPattern { value, .. } => {
            if let Some(v) = value {
                collect_func_references(v, refs, const_reads);
            }
            // the bindings should just be types by now
        }
    }
}

fn collect_func_references_value(v: &Value, refs: &mut Vec<FuncId>) {
    let mut vals = vec![v];
    while let Some(v) = vals.pop() {
        match v {
            Value::GetFn(f) => refs.push(*f),
            Value::Heap { value, .. } => {
                let value = unsafe { &**value };
                vals.extend(&value.values);
            }
            Value::OverloadSet(_) => {
                // TODO // unreachable!("finished ast contained {v:?}")
            }
            _ => {}
        }
    }
}

fn collect_func_references<'p>(
    expr: &Expr<'p>,
    refs: &mut Vec<FuncId>,
    const_reads: &mut HashSet<Var<'p>>,
) {
    match expr {
        Expr::Value { value, .. } => value
            .clone()
            .vec()
            .iter()
            .for_each(|v| collect_func_references_value(v, refs)),
        Expr::Call(f, arg) => {
            collect_func_references(f, refs, const_reads);
            collect_func_references(arg, refs, const_reads);
        }
        Expr::Block { body, result, .. } => {
            for s in body {
                collect_func_references_stmt(s, refs, const_reads);
            }
            collect_func_references(result, refs, const_reads);
        }
        Expr::Tuple(exprs) => {
            for e in exprs {
                collect_func_references(e, refs, const_reads);
            }
        }

        Expr::FieldAccess(e, _) | Expr::SuffixMacro(_, e) => {
            collect_func_references(e, refs, const_reads);
        }
        Expr::GetVar(v) => {
            const_reads.insert(*v);
        }
        Expr::StructLiteralP(_) => {}
        Expr::ArrayLiteral(_)
        | Expr::GetNamed(_)
        | Expr::RefType(_)
        | Expr::EnumLiteral(_)
        | Expr::PrefixMacro { .. }
        | Expr::String(_)
        | Expr::Closure(_) => {} // TODO // unreachable!("finished ast contained {expr:?}"),
    }
}

impl<'p> PoolLog<'p> for Program<'p> {
    fn log(&self, _: &StringPool<'p>) -> String {
        let mut s = String::new();
        s += &self.log_cached_types();
        s
    }
}

impl<'a, 'p> PoolLog<'p> for Interp<'a, 'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        let mut s = String::new();
        // s += &self.program.log_cached_types();
        writeln!(s, "=== {} FUNCTIONS ===", self.ready.len());
        for (i, f) in self.ready.iter().enumerate() {
            write!(s, "FuncId({i}): ");
            // s += &self.program.funcs[i].log(pool);
            // s += "\n";
            if let Some(bc) = f {
                writeln!(s, "{}", bc.log(pool));
            }
            writeln!(s, "===");
        }
        s
    }
}

impl<'p> Program<'p> {
    pub fn log_consts(&self, c: &Constants<'p>) -> String {
        let mut s = String::new();
        for (name, (v, ty)) in &c.local {
            writeln!(
                s,
                "   - {} = {:?} is {}",
                name.log(self.pool),
                v,
                self.log_type(*ty)
            );
        }

        s
    }
}

impl<'p> PoolLog<'p> for Stmt<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        match self {
            Stmt::DeclNamed {
                name,
                ty,
                value,
                kind,
            } => format!(
                "{kind:?} {}: {} = {};",
                pool.get(*name),
                ty.log(pool),
                value
                    .as_ref()
                    .map(|v| v.log(pool))
                    .unwrap_or_else(|| String::from("uninit()"))
            ),
            Stmt::DeclVar {
                name, ty, value, ..
            } => format!(
                "let {}: {} = {};",
                name.log(pool),
                ty.log(pool),
                value
                    .as_ref()
                    .map(|v| v.log(pool))
                    .unwrap_or_else(|| String::from("uninit()"))
            ),
            Stmt::Eval(e) => e.log(pool),
            Stmt::DeclFunc(func) => format!("declare(fn {})", func.synth_name(pool)),
            Stmt::Noop => "".to_owned(),
            Stmt::Set { place, value } => {
                format!("{} = {};", place.log(pool), value.log(pool))
            }
            _ => format!("{:?}", self),
        }
    }
}

impl<'p> PoolLog<'p> for LazyType<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        match self {
            LazyType::EvilUnit => panic!(),
            LazyType::Infer => "Infer".into(),
            LazyType::PendingEval(e) => e.log(pool),
            LazyType::Finished(ty) => format!("{:?}", ty),
            LazyType::Different(parts) => parts.iter().map(|p| p.log(pool)).collect(),
        }
    }
}

impl<'p> PoolLog<'p> for Expr<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        match self {
            Expr::Call(func, arg) => {
                format!("{}({})", func.log(pool), arg.log(pool))
            }
            &Expr::GetNamed(i) => pool.get(i).to_string(),
            Expr::Block { body, result, .. } => {
                let es: Vec<_> = body
                    .iter()
                    .map(|e| e.log(pool))
                    .filter(|s| !s.is_empty())
                    .enumerate()
                    .map(|(i, s)| format!("{i}. {s}"))
                    .collect();
                let es = es.join(";\n");
                format!("{{ {}; {} }}", es, result.log(pool))
            }
            Expr::ArrayLiteral(args) => {
                let args: Vec<_> = args.iter().map(|e| e.log(pool)).collect();
                let args: String = args.join(", ");
                format!("array({})", args)
            }
            Expr::Tuple(args) => {
                let args: Vec<_> = args.iter().map(|e| e.log(pool)).collect();
                let args: String = args.join(", ");
                format!("T[{}]", args)
            }
            Expr::RefType(e) => format!("&({})", e.log(pool)),
            Expr::Value {
                value: Values::One(Value::Unit),
                ..
            } => "unit".to_string(),
            Expr::Value {
                value: Values::One(Value::GetFn(f)),
                ..
            } => format!("Fn{}", f.0),
            Expr::Value { value, .. } => format!("{:?}", value),
            Expr::GetVar(v) => v.log(pool),
            Expr::Closure(f) => format!("closure(fn {:?})", f.synth_name(pool)),
            Expr::SuffixMacro(i, e) => format!("{}!{}", e.log(pool), pool.get(*i)),
            Expr::StructLiteralP(pattern) => {
                let body: String = pattern
                    .bindings
                    .iter()
                    .map(|b| {
                        format!(
                            "{}: ({}), ",
                            b.name().map_or("_", |n| pool.get(n)),
                            b.lazy().log(pool)
                        )
                    })
                    .collect();

                format!(".{{ {body} }}")
            }
            Expr::FieldAccess(container, name) => {
                format!("{}.{}", container.log(pool), pool.get(*name))
            }
            Expr::PrefixMacro { name, arg, target } => format!(
                "[@{}({}) {}]",
                name.log(pool),
                arg.log(pool),
                target.log(pool)
            ),
            _ => format!("{:?}", self),
        }
    }
}

impl<'p> PoolLog<'p> for FatExpr<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        self.expr.log(pool)
    }
}

impl<'p> PoolLog<'p> for Var<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        let i = self.1;
        format!("{}%{}", pool.get(self.0), i)
    }
}

impl<'p> PoolLog<'p> for Func<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        if self.evil_uninit {
            return "[UNINIT (wip/dropped)]".to_string();
        }
        format!(
            "[fn {} {:?} {} = \nCONSTANTS: \n{} \nBODY: \n{}\nEND\nARG: {:?}\n A:{:?}]\n{}\n{}\n",
            self.synth_name(pool),
            self.get_name(pool),
            self.ret.log(pool),
            self.local_constants
                .iter()
                .map(|e| e.log(pool))
                .collect::<Vec<_>>()
                .join("\n"),
            self.body
                .as_ref()
                .map(|e| e.log(pool))
                .unwrap_or_else(|| "@NO_BODY@".to_owned()),
            self.arg, // TODO: better formatting.
            self.annotations.iter().map(|i| pool.get(i.name)),
            if self.capture_vars.is_empty() {
                String::from("Raw function, no captures.")
            } else {
                format!(
                    "Closure capturing: {:?}.",
                    self.capture_vars
                        .iter()
                        .map(|v| v.log(pool))
                        .collect::<Vec<_>>()
                )
            },
            if self.capture_vars_const.is_empty() {
                String::from("No const captures.")
            } else {
                format!(
                    "Const capturing: {:?}.",
                    self.capture_vars_const
                        .iter()
                        .map(|v| v.log(pool))
                        .collect::<Vec<_>>()
                )
            }
        )
    }
}

impl<'p> PoolLog<'p> for DebugInfo<'p> {
    fn log(&self, _: &StringPool<'p>) -> String {
        format!("// {} ", self.internal_loc)
    }
}

impl<'p> PoolLog<'p> for FnBody<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        let mut f = String::new();
        writeln!(f, "=== Bytecode for {:?} at {:?} ===", self.func, self.when,);
        writeln!(f, "TYPES: ");
        for (i, ty) in self.slot_types.iter().enumerate() {
            write!(f, "${i}:{ty:?}, ");
        }
        writeln!(f);
        let width = 75;
        for (i, bc) in self.insts.iter().enumerate() {
            let bc = format!("{i}. {}", bc.log(pool));
            writeln!(
                f,
                "{:width$} {}",
                bc,
                self.debug
                    .get(i)
                    .map(|d| d.log(pool))
                    .unwrap_or_else(|| String::from("// ???"))
            );
        }
        writeln!(f, "{}", self.why);
        f
    }
}

impl<'p> PoolLog<'p> for Bc<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        let mut f = String::new();
        match self {
            Bc::CallDynamic {
                f: func_slot,
                ret,
                arg,
            }
            | Bc::CallC {
                f: func_slot,
                ret,
                arg,
                ..
            } => write!(f, "{ret:?} = call({func_slot:?}, {arg:?});"),
            Bc::CallDirect { f: func, ret, arg } => {
                write!(f, "{ret:?} = call(f({:?}), {arg:?});", func.0)
            }
            Bc::CallBuiltin { name, ret, arg } => {
                write!(f, "{ret:?} = builtin(S{}, {arg:?});", name.0)
            }
            Bc::LoadConstant { slot, value } => write!(f, "{:?} = {:?};", slot, value),
            Bc::JumpIf {
                cond,
                true_ip,
                false_ip,
            } => write!(
                f,
                "if ({:?}) goto {} else goto {};",
                cond, true_ip, false_ip
            ),
            Bc::Goto { ip } => write!(f, "goto {ip};",),
            Bc::Ret(i) => write!(f, "return {i:?};"),
            Bc::Clone { from, to } => write!(f, "{:?} = @clone({:?});", to, from),
            Bc::CloneRange { from, to } => write!(f, "{:?} = @clone({:?});", to, from),
            Bc::Move { from, to } => write!(f, "{:?} = move({:?});", to, from),
            Bc::MoveRange { from, to } => write!(f, "{:?} = move({:?});", to, from),
            Bc::Load { from, to } => write!(f, "{:?} = {:?}!deref;", to, from),
            Bc::Store { from, to } => write!(f, "{:?}!deref = {:?};", to, from),
            Bc::Drop(i) => write!(f, "drop({:?});", i),
            Bc::SlicePtr {
                base,
                offset,
                count,
                ret,
            } => write!(
                f,
                "{:?} = slice({:?}, first={}, count={});",
                ret, base, offset, count
            ),
            Bc::AbsoluteStackAddr { of, to } => write!(f, "{:?} = @addr({:?});", to, of),
            Bc::DebugMarker(s, i) => write!(f, "debug({:?}, {:?} = {:?});", s, i, pool.get(*i)),
            Bc::DebugLine(loc) => write!(f, "debug({:?});", loc),
            Bc::TagCheck { enum_ptr, value } => write!(f, "@assert(Tag({enum_ptr:?}) == {value});"),
        };
        f
    }
}

impl Debug for StackOffset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

impl Debug for StackRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let args: Vec<_> = (self.first.0..(self.first.0 + self.count))
            .map(|i| format!("${}", i))
            .collect();
        let args = args.join(", ");
        write!(f, "({args})")
    }
}

impl Debug for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ty{}", self.0)
    }
}

impl Stmt<'_> {
    pub fn get_loc(&self) -> Option<Span> {
        match self {
            Stmt::Noop => None,
            Stmt::Eval(e) => Some(e.loc),
            Stmt::DeclFunc(f) => f.body.as_ref().map(|e| e.loc),
            Stmt::DeclVar { value, .. } => value.as_ref().map(|e| e.loc),
            Stmt::Set { place, .. } => Some(place.loc),
            _ => None,
        }
    }
}

impl<'a, 'p> Interp<'a, 'p> {
    pub fn log_stack(&self) {
        if cfg!(not(feature = "spam_log")) {
            return;
        }
        log!("STACK ");
        let frame = self.call_stack.last().unwrap();
        for i in 0..frame.stack_base.0 {
            if self.value_stack[i] != Value::Poison {
                log!("({i}|{:?}), ", self.value_stack[i]);
            }
        }
        for i in frame.stack_base.0..self.value_stack.len() {
            if self.value_stack[i] != Value::Poison {
                let slot = i - frame.stack_base.0;
                log!("[{i}|${slot}|{:?}], ", self.value_stack[i]);
            }
        }
        logln!("END");
    }

    pub fn log_callstack(&self) -> String {
        let mut s = String::new();
        write!(s, "CALLS ");
        for frame in &self.call_stack {
            write!(s, "[{}], ", self.pool.get(frame.debug_name));
        }
        write!(s, "END");
        s
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::F64(v) => write!(f, "{v}"),
            Value::I64(v) => write!(f, "{v}"),
            Value::Bool(v) => write!(f, "{v}"),
            _ => write!(f, "{self:?}"),
        }
    }
}

impl<'p> Debug for CompileError<'p> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "COMPILATION ERROR:")?;
        writeln!(f, "{:?}", self.reason)?;
        writeln!(f, "Internal: {}", self.internal_loc)?;
        write!(f, "{}", self.trace)
    }
}

impl<'p> DebugState<'p> {
    pub fn log(&self, pool: &StringPool<'p>, program: &Program<'p>) -> String {
        let show_f = |func: FuncId| {
            format!(
                "f{}:{:?}:{}",
                func.0,
                program.funcs[func.0].get_name(pool),
                program.funcs[func.0].synth_name(pool)
            )
        };
        match self {
            DebugState::Msg(s) => s.clone(),
            DebugState::Compile(f) => {
                format!("| Comptime    | {}", show_f(*f))
            }
            DebugState::EnsureCompiled(f, when) => {
                format!("| Ensure Comp | {} for {:?}", show_f(*f), when)
            }
            DebugState::EmitBody(f) => {
                format!("|  Emit Body  | {}", show_f(*f))
            }
            DebugState::EvalConstants(f) => {
                format!("| Eval Consts | {:?}", show_f(*f))
            }
            DebugState::EmitCapturingCall(f) => {
                format!("| Captur Call | {:?}", show_f(*f))
            }
            DebugState::ResolveFnRef(v) => {
                format!("| Find Fn | {}", v.log(pool))
            }
            DebugState::RunInstLoop(f) => format!("| Loop Insts  | {}", show_f(*f)),
            DebugState::ComputeCached(e) => format!("| Cache Eval  | {}", e.log(pool)),
            DebugState::ResolveFnType(f) => {
                format!("| Resolve Type| {}", show_f(*f),)
            }
        }
    }
}

impl<'p> CErr<'p> {
    pub fn log(&self, program: &Program<'p>, pool: &StringPool<'p>) -> String {
        match self {
            CErr::UndeclaredIdent(i) => format!("Undeclared Ident: {i:?} = {}", pool.get(*i)),
            CErr::TypeCheck(found, expected, msg) => format!(
                "{msg}. Type check expected {expected:?} = {} but found {found:?} = {}",
                program.log_type(*expected),
                program.log_type(*found)
            ),
            CErr::IceFmt(s) | CErr::Msg(s) => s.clone(),
            CErr::VarNotFound(var) => format!(
                "Resolved var not found {:?}. (forgot to make something const? ice?)",
                var.log(pool)
            ),
            _ => format!("{:?}", self),
        }
    }
}

impl<'p> FatStmt<'p> {
    pub fn log_annotations(&self, pool: &StringPool<'p>) -> String {
        self.annotations.iter().map(|a| pool.get(a.name)).collect()
    }
}

impl<'p> Func<'p> {
    pub fn log_captures(&self, pool: &StringPool<'p>) -> String {
        let mut s = String::new();
        writeln!(s, "Scope for fn {:?}", self.synth_name(pool));
        if !self.capture_vars.is_empty() {
            writeln!(
                s,
                "- Runtime captures: {:?}",
                self.capture_vars
                    .iter()
                    .map(|v| v.log(pool))
                    .collect::<Vec<String>>()
            );
        }
        if !self.capture_vars_const.is_empty() {
            writeln!(
                s,
                "- Const captures: {:?}",
                self.capture_vars_const
                    .iter()
                    .map(|v| v.log(pool))
                    .collect::<Vec<String>>()
            );
        }

        if !self.local_constants.is_empty() {
            writeln!(s, "- Const locals:");
            for d in &self.local_constants {
                writeln!(s, "    - {}", d.log(pool).replace('\n', " "));
            }
        }
        writeln!(s, "====");
        s
    }
}

pub fn log_tag_info() {
    use LogTag::*;
    let info = |tag: LogTag, msg: &str| outln!(tag, "{msg}\n###################################");

    info(Parsing, "The parser uncovers the structure of your program without any understanding of its semantics.");
    info(FinalAst, "This is how the compiler sees your program after comptime execution but before code generation. \nOverloads have been resolved, types are explicit, macros have been applied.");
    info(
        Macros,
        "Macros are functions, written in this language, that transform the AST.",
    );
    info(Scope, "As part of resolving variable shadowing, we find out if any functions are actually closures that capture part of thier environment.");
    info(
        Bytecode,
        "Your program broken down into simple instructions. This is the format understood by the comptime interpreter. It's also the only backend that exists so far.",
    );
}
