// nobody cares its just logging. TODO: should do it anyway i guess.
#![allow(unused_must_use)]
use core::fmt;
use std::cell::RefCell;
use std::fmt::{Debug, Formatter, Write};
use std::{fs, mem};

#[inline(never)]
pub fn break_here(_e: &CErr) {
    let depth = unsafe { EXPECT_ERR_DEPTH.load(std::sync::atomic::Ordering::SeqCst) };
    if depth == 0 {
        // TODO: make this never happen so dont have to worry about short circuiting -- Apr 25
        // println!("err: {e:?}");
    }
}

#[macro_export]
macro_rules! err {
    ($payload:expr) => {{
        let e = $payload;
        $crate::logging::break_here(&e);

        return Err($crate::compiler::CompileError {
            internal_loc: if cfg!(feature="trace_errors") {Some(std::panic::Location::caller())} else {None},
            loc: None,
            reason: e,
            trace: String::new(),
        })
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
use codemap::Span;
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
    Perf = 11,
    Consts = 12,
    _Last = 13,
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
        if !s.is_empty() {
            fs::write(format!("{folder}/{tag:?}.log"), s).unwrap();
        }
    }
}

#[macro_export]
macro_rules! outln {
    ($tag:expr, $($arg:tt)*) => {{
        if $tag == $crate::logging::LogTag::ShowPrint {
            println!($($arg)*);
        } else if cfg!(feature = "some_log") {
            let tag: $crate::logging::LogTag = $tag;
            $crate::logging::LOG.with(|settings| {
                if (settings.borrow().track & (1 << tag as usize)) != 0 {
                    settings.borrow_mut().logs[tag as usize].push(format!($($arg)*));
                }
            })
        } else if $tag == $crate::logging::LogTag::ShowPrint || $tag == $crate::logging::LogTag::ShowErr {
            let tag: $crate::logging::LogTag = $tag;
            $crate::logging::LOG.with(|settings| {
                settings.borrow_mut().logs[tag as usize].push(format!($($arg)*));
            })
        }
    }};
}

#[macro_export]
macro_rules! log {
    ($($arg:tt)*) => {{
        if cfg!(feature = "spam_log") {
            print!($($arg)*);
        }
    }};
}

#[macro_export]
macro_rules! logln {
    // Using cfg!(...) instead of #[cfg(...)] to avoid unused var warnings.
    ($($arg:tt)*) => {{
        if cfg!(feature = "spam_log") {
            println!($($arg)*);
        }
    }};
}

use crate::ast::{FatStmt, Pattern};
use crate::bc::*;
use crate::emit_bc::DebugInfo;
use crate::pool::Ident;
use crate::{
    ast::{Expr, FatExpr, Func, FuncId, LazyType, Program, Stmt, TypeId, TypeInfo, Var},
    compiler::{CErr, CompileError, DebugState},
    pool::StringPool,
};

/// It felt like a good idea to be able to compare identifiers super fast but now its forever a pain to look at them while debugging.
pub trait PoolLog<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String;
}

use crate::ast::safe_rec;
use crate::compiler::EXPECT_ERR_DEPTH;

impl<'p> Program<'p> {
    pub fn log_type(&self, t: TypeId) -> String {
        safe_rec!(self, t, format!("{t:?}"), {
            match &self[t] {
                TypeInfo::Unknown => "Unknown".to_owned(),
                TypeInfo::Any => "Any".to_owned(),
                TypeInfo::Never => "Never".to_owned(),
                TypeInfo::F64 => "f64".to_owned(),
                TypeInfo::Bool => "bool".to_owned(),
                TypeInfo::OverloadSet => "OverloadSet".to_owned(),
                // TODO: be careful of recursion
                TypeInfo::Ptr(e) => format!("*{}", self.log_type(*e)),
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
                        .map(|(name, ty)| format!("{}: {}", self.pool.get(*name), self.log_type(*ty)))
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
                TypeInfo::VoidPtr => "VoidPtr".to_owned(),
                TypeInfo::Int(int) => {
                    format!("{}{}", if int.signed { "i" } else { "u" }, int.bit_count)
                }
                TypeInfo::Scope => "ScopeId".to_owned(),
            }
            // format!("{t:?}={s}")
        })
    }

    // Note: be careful this can't get into a recursive loop trying to pretty print stuff.
    pub fn log_cached_types(&self) -> String {
        let mut s = String::new();
        writeln!(s, "=== {} CACHED TYPES ===", self.types.len());
        for (i, ty) in self.types.iter().enumerate() {
            writeln!(s, "- {:?} = {} = {:?};", TypeId::from_index(i), self.log_type(TypeId::from_index(i)), ty,);
        }
        writeln!(s, "====================");
        s
    }

    pub fn find_ffi_type(&self, name: Ident<'p>) -> Option<TypeId> {
        let mut found = None;
        for ty in self.ffi_types.values() {
            if let TypeInfo::Unique(ty, _) = &self[*ty] {
                if let &TypeInfo::Named(ty, check) = &self[*ty] {
                    if name == check {
                        if found.is_some() {
                            outln!(LogTag::ShowErr, "duplicate ffi name {}", self.pool.get(name));
                            return None;
                        }
                        found = Some(ty);
                    } else if let TypeInfo::Enum { cases, .. } = &self[ty] {
                        for (_, ty) in cases {
                            if let &TypeInfo::Named(inner, case_name) = &self[*ty] {
                                // TODO: ffi cases that are akreayd named types
                                if case_name == name {
                                    if found.is_some() {
                                        outln!(LogTag::ShowErr, "duplicate ffi name {}", self.pool.get(name));
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
                if let &TypeInfo::Named(ty, name) = &self[*ty] {
                    if let TypeInfo::Enum { cases, .. } = &self[ty] {
                        writeln!(out, "const {} = Unique({{", self.pool.get(name),).unwrap();
                        for (name, mut ty) in cases {
                            if let &TypeInfo::Named(inner, _) = &self[ty] {
                                // Not unique, name is probably just name of the case.
                                ty = inner;
                            }
                            writeln!(out, "    {}: {},", self.pool.get(*name), self.log_type(ty)).unwrap();
                        }
                        out += "}!enum);\n"
                    } else {
                        writeln!(out, "const {} = Unique({});", self.pool.get(name), self.log_type(ty)).unwrap();
                    }
                }
            }
        }
        out
    }
}

impl<'p> PoolLog<'p> for Program<'p> {
    fn log(&self, _: &StringPool<'p>) -> String {
        let mut s = String::new();
        s += &self.log_cached_types();
        s
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
        match self {
            Expr::Call(func, arg) => {
                format!("{}({})", func.logd(pool, depth), arg.logd(pool, depth))
            }
            &Expr::GetNamed(i) => pool.get(i).to_string(),
            Expr::Block { body, result, .. } => {
                let es: Vec<_> = body
                    .iter()
                    .filter(|s| !matches!(s.stmt, Stmt::Noop))
                    .filter(|s| {
                        // HACK to skip the useless args added by !if and !while.
                        if let Stmt::DeclVarPattern { binding, value } = &s.stmt {
                            if binding.bindings.len() == 1 && value.is_some() && value.as_ref().unwrap().is_raw_unit() {
                                return false;
                            }
                        }
                        true
                    })
                    .map(|e| e.logd(pool, depth + 1))
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
            Expr::Value {
                value: Values::One(Value::Unit),
                ..
            } => "unit".to_string(),
            Expr::Value {
                value: Values::One(Value::GetFn(f)),
                ..
            } => format!("Fn{}", f.as_index()),
            Expr::Value { value, .. } => match value {
                Values::One(Value::I64(i)) => format!("{i}"),
                Values::One(Value::Type(i)) => format!("{i:?}"),
                v => format!("{v:?}"),
            },
            Expr::GetVar(v) => v.log(pool),
            Expr::Closure(f) => format!("closure(fn {:?})", f.synth_name(pool)),
            Expr::SuffixMacro(i, e) => format!("{}!{}", e.logd(pool, depth), pool.get(*i)),
            Expr::StructLiteralP(pattern) => {
                let body: String = pattern
                    .bindings
                    .iter()
                    .map(|b| format!("{}: ({}), ", b.name().map_or("_", |n| pool.get(n)), b.lazy().log(pool)))
                    .collect();

                format!(".{{ {body} }}")
            }
            Expr::FieldAccess(container, name) => {
                format!("{}.{}", container.logd(pool, depth), pool.get(*name))
            }
            Expr::PrefixMacro { handler, arg, target } => {
                format!("[@{}({}) {}]", handler.logd(pool, depth), arg.logd(pool, depth), target.logd(pool, depth))
            }
            Expr::Index { ptr, index } => format!("{}[{}]", ptr.logd(pool, depth), index.logd(pool, depth)),
            _ => format!("{:?}", self),
        }
    }
}

impl<'p> Stmt<'p> {
    pub(crate) fn logd(&self, pool: &StringPool<'p>, depth: usize) -> String {
        let s = match self {
            Stmt::DeclNamed { name, ty, value, kind } => format!(
                "{kind:?} {}: {} = {};",
                pool.get(*name),
                ty.log(pool),
                value.as_ref().map(|v| v.logd(pool, depth)).unwrap_or_else(|| String::from("uninit()"))
            ),
            Stmt::DeclVar { name, ty, value, kind, .. } => format!(
                "{:?} {}: {} = {};",
                kind,
                name.log(pool),
                ty.log(pool),
                value.as_ref().map(|v| v.logd(pool, depth)).unwrap_or_else(|| String::from("uninit()"))
            ),
            Stmt::Eval(e) => e.logd(pool, depth),
            Stmt::DeclFunc(func) => format!("declare(fn {})", func.synth_name(pool)),
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

                format!("({body}) = {};", value.as_ref().map_or("_".to_string(), |v| v.logd(pool, depth)))
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
        let i = self.1;
        use crate::ast::VarType;
        let kind = match self.3 {
            VarType::Let => "L",
            VarType::Var => "V",
            VarType::Const => "C",
        };
        format!("{}%{}{}s{}b{}", pool.get(self.0), i, kind, self.2.as_index(), self.4)
    }
}

impl<'p> PoolLog<'p> for Func<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        if self.evil_uninit {
            return "[UNINIT (wip/dropped)]".to_string();
        }
        format!(
            "[fn {} {:?} {} = \n \nBODY: \n{}\nEND\nARG: {}\n A:{:?}]\n{}llvm={}, aarch64={}\nCONSTS:\n{:?}",
            self.synth_name(pool),
            self.get_name(pool),
            self.ret.log(pool),
            self.body.as_ref().map(|e| e.log(pool)).unwrap_or_else(|| "@NO_BODY@".to_owned()),
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
            self.llvm_ir.is_some(),
            self.jitted_code.is_some(),
            self.local_constants.iter().map(|v| v.log(pool)).collect::<Vec<_>>(),
        )
    }
}

impl<'p> PoolLog<'p> for DebugInfo<'p> {
    fn log(&self, _: &StringPool<'p>) -> String {
        if cfg!(feature = "trace_errors") {
            format!("// {} ", self.internal_loc.unwrap())
        } else {
            String::new()
        }
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
                self.debug.get(i).map(|d| d.log(pool)).unwrap_or_else(|| String::from("// ???"))
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
            Bc::CallFnPtr { f: func_slot, ret, arg, .. } => {
                write!(f, "{ret:?} = call_c({func_slot:?}, {arg:?});")
            }
            Bc::CallDirect { f: func, ret, arg } => {
                write!(f, "{ret:?} = call(f({:?}), {arg:?});", func.as_index())
            }
            Bc::CallSplit { ct, rt, ret, arg } => {
                write!(f, "{ret:?} = call(f({ct:?} | {rt:?}), {arg:?});")
            }
            Bc::LoadConstant { slot, value } => write!(f, "{:?} = {:?};", slot, value),
            Bc::JumpIf { cond, true_ip, false_ip } => write!(f, "if ({:?}) goto {} else goto {};", cond, true_ip, false_ip),
            Bc::Goto { ip } => write!(f, "goto {ip};",),
            Bc::Ret(i) => write!(f, "return {i:?};"),
            Bc::Clone { from, to } => write!(f, "{:?} = @clone({:?});", to, from),
            Bc::CloneRange { from, to } => write!(f, "{:?} = @clone({:?});", to, from),
            Bc::Load { from, to } => write!(f, "{:?} = {:?}!deref;", to, from),
            Bc::Store { from, to } => write!(f, "{:?}!deref = {:?};", to, from),
            Bc::SlicePtr { base, offset, count, ret } => write!(f, "{:?} = slice({:?}, first={}, count={});", ret, base, offset, count),
            Bc::AbsoluteStackAddr { of, to } => write!(f, "{:?} = @addr({:?});", to, of),
            Bc::DebugMarker(s, i) => write!(f, "debug({:?}, {:?} = {:?});", s, i, pool.get(*i)),
            Bc::DebugLine(loc) => write!(f, "debug({:?});", loc),
            Bc::TagCheck { enum_ptr, value } => write!(f, "@assert(Tag({enum_ptr:?}) == {value});"),
            _ => write!(f, "{self:?}"),
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
        let args: Vec<_> = (self.first.0..(self.first.0 + self.count)).map(|i| format!("${}", i)).collect();
        let args = args.join(", ");
        write!(f, "({args})")
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
        if cfg!(feature = "trace_errors") {
            writeln!(f, "Internal: {}", self.internal_loc.unwrap())?;
        }
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
            DebugState::RunInstLoop(f, i) => format!("| Loop Insts  | {:?} {}", *f, pool.get(*i)),
            DebugState::ComptimeCall(f, i) => format!("| Comptime Call | {:?} {}", *f, pool.get(*i)),
            DebugState::ResolveFnType(f, i) => format!("| Resolve Type| {:?} {}", *f, pool.get(*i)),
            _ => format!("{self:?}"),
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
            CErr::Fatal(s) => s.clone(),
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
        writeln!(s, "fn {:?}", self.synth_name(pool));
        if !self.capture_vars.is_empty() {
            writeln!(
                s,
                "- Runtime captures: {:?}",
                self.capture_vars.iter().map(|v| v.log(pool)).collect::<Vec<String>>()
            );
        }
        writeln!(s, "====");
        s
    }
}

pub fn log_tag_info() {
    use LogTag::*;
    let info = |tag: LogTag, msg: &str| outln!(tag, "{msg}\n###################################");

    info(
        Parsing,
        "The parser uncovers the structure of your program without any understanding of its semantics.",
    );
    info(FinalAst, "This is how the compiler sees your program after comptime execution but before code generation. \nOverloads have been resolved, types are explicit, macros have been applied.");
    info(Macros, "Macros are functions, written in this language, that transform the AST.");
    info(
        Scope,
        "As part of resolving variable shadowing, we find out if any functions are actually closures that capture part of thier environment.",
    );
    info(
        Bytecode,
        "Your program broken down into simple instructions. This is the format understood by the comptime interpreter. It's also the only backend that exists so far.",
    );
}

impl Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            &Value::F64(v) => write!(f, "{}", f64::from_bits(v)),
            &Value::I64(v) => write!(f, "{}", v),
            &Value::Bool(v) => write!(f, "{}", v),
            &Value::Type(v) => write!(f, "{:?}", v),
            &Value::GetFn(v) => write!(f, "{:?}", v),
            &Value::Unit => write!(f, "unit"),
            Value::Heap(ptr) => write!(f, "{:p}", ptr),
            &Value::Symbol(v) => write!(f, "sym{v}"),
            &Value::OverloadSet(v) => write!(f, "os{v}"),
            &Value::GetNativeFnPtr(v) => write!(f, "{v:?}&"),
            &Value::SplitFunc { ct, rt } => write!(f, "cr={ct:?}|rt={rt:?}"),
        }
    }
}

impl Debug for Values {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Values::One(v) => write!(f, "{v:?}"),
            Values::Many(v) => write!(f, "{v:?}"),
        }
    }
}

impl Debug for FuncId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Fn{}", self.as_index())
    }
}
