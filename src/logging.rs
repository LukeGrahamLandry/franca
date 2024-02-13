use core::fmt;
use std::{
    fmt::{Debug, Write},
    panic::Location,
};

macro_rules! bin_int {
    ($self:expr, $op:tt, $arg:expr, $res:expr) => {{
        let (a, b) = $self.load_int_pair($arg)?;
        $res(a $op b)
    }};
}

macro_rules! err {
    ($self:expr, $payload:expr) => {{
        return Err($self.error($payload));
    }};
    ($self:expr, $($arg:tt)*) => {{
        let msg = format!($($arg)*);
        return err!($self, CErr::Msg(msg))
    }};
}

macro_rules! assert {
    ($self:expr, $cond:expr, $($arg:tt)+) => {{
        if !($cond) {
            let msg = format!("Assertion Failed: {}\n{}", stringify!($cond), format!($($arg)*));
            return err!($self, CErr::Msg(msg))
        }
    }};
    ($self:expr, $cond:expr $(,)?) => {{
        assert!($self, $cond, "")
    }};
}

// looks weird cause i stole it from the rust one. apparently its faster or whatever
macro_rules! assert_eq {
    ($self:expr, $left:expr, $right:expr, $($arg:tt)+) => {
        match (&$left, &$right) {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    let msg = format!($($arg)*);
                    err!(
                        $self,
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
    ($self:expr, $left:expr, $right:expr) => {
        assert_eq!($self, $left, $right, "");
    };
}

// TODO: use this for interp bounds checks and accept an enum so you can turn them off individually. Distinguish between runtime/comptime.
macro_rules! check {
    ($self:expr, $cond:expr, $($arg:tt)+) => {{
        if !($cond) {
            let msg = format!("Builtin Safety Check Failed: {}\n{}", stringify!($cond), format!($($arg)*));
            return err!($self, CErr::Msg(msg))
        }
    }};
    ($self:expr, $cond:expr $(,)?) => {{
        check!($self, $cond, "")
    }};
}
pub(crate) use check;

// I want to be as easy to use my error system as just paniking.
// Use this one for things that aren't supported yet or should have been caught in a previous stage of compilation.
macro_rules! ice {
    ($self:expr, $($arg:tt)*) => {{
        let msg = format!($($arg)*);
        return err!($self, CErr::IceFmt(msg))
    }};
}
use codemap::Span;
pub(crate) use ice;

// TODO: compile errors should include the line number of the most recent ast node.

// TODO: have an enum for distringuishing ice from invalid input and use that everywhere instead of having two of each macro.
// TODO: I really like stringify! for error messages. make sure ast macros in my language have that.
// Convert a missing option into a compile error with a message.
macro_rules! unwrap {
    ($self:expr, $maybe:expr, $($arg:tt)*) => {{
        if let Some(value) = $maybe {
            value
        } else {
            let msg = format!("Missing value {}.\n{}", stringify!($maybe), format!($($arg)*));
            return err!($self, CErr::IceFmt(msg))
        }
    }};
}

pub(crate) use unwrap;

pub enum LogTag {
    Parsing,
    InstLoop,
    Jitting,
}

macro_rules! log {
    // Using cfg!(...) instead of #[cfg(...)] to avoid unused var warnings.
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

use crate::{
    ast::{
        Expr, FatExpr, Func, FuncId, LazyFnType, LazyType, Program, Stmt, TypeId, TypeInfo, Var,
    },
    interp::{
        Bc, CErr, CompileError, DebugInfo, DebugState, FnBody, Interp, StackOffset, StackRange,
        Value,
    },
    pool::StringPool,
};

/// It felt like a good idea to be able to compare identifiers super fast but now its forever a pain to look at them while debugging.
pub trait PoolLog<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String;
}

impl<'p> Program<'p> {
    pub fn log_type(&self, t: TypeId) -> String {
        match &self.types[t.0] {
            TypeInfo::Any => "Any".to_owned(),
            TypeInfo::Never => "Never".to_owned(),
            TypeInfo::F64 => "f64".to_owned(),
            TypeInfo::I64 => "i64".to_owned(),
            TypeInfo::Bool => "bool".to_owned(),
            TypeInfo::Ptr(e) => format!("Ptr({})", self.log_type(*e)),
            TypeInfo::Struct {
                fields,
                size,
                as_tuple,
            } => {
                // TODO: factor out iter().join(str), how does that not already exist
                let v: Vec<_> = fields.iter().map(|(f)| format!("{f:?}")).collect();
                format!("Struct({})", v.join(", "))
            }
            TypeInfo::Unique(n, inner) => format!("{:?} is {}", n, self.log_type(*inner)),
            TypeInfo::Fn(f) => format!("fn({}) {}", self.log_type(f.arg), self.log_type(f.ret)),
            TypeInfo::Tuple(v) => {
                let v: Vec<_> = v.iter().map(|v| self.log_type(*v)).collect();
                format!("Tuple({})", v.join(", "))
            }
            TypeInfo::Type => "Type".to_owned(),
            TypeInfo::Unit => "Unit".to_owned(),
        }
    }

    // Note: be careful this can't get into a recursive loop trying to pretty print stuff.
    pub fn log_cached_types(&self) -> String {
        let mut s = String::new();
        writeln!(s, "=== {} CACHED TYPES ===", self.types.len());
        for (i, ty) in self.types.iter().enumerate() {
            writeln!(
                s,
                "- {:?} = {} = {:?}",
                TypeId(i),
                self.log_type(TypeId(i)),
                ty
            );
        }
        writeln!(s, "====================");
        s
    }
}

impl<'p> PoolLog<'p> for Program<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        let mut s = String::new();
        s += &self.log_cached_types();
        s
    }
}

impl<'a, 'p> PoolLog<'p> for Interp<'a, 'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        let mut s = String::new();
        s += &self.program.log_cached_types();
        writeln!(s, "=== {} FUNCTIONS ===", self.ready.len());
        for (i, f) in self.ready.iter().enumerate() {
            s += &self.program.funcs[i].log(pool);
            s += "\n";
            if let Some(bc) = f {
                writeln!(s, "{}", bc.log(pool));
            } else {
                writeln!(s, "NOT JITTED");
            }
            writeln!(s, "===");
        }
        s
    }
}

impl<'p> PoolLog<'p> for LazyFnType<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        match self {
            LazyFnType::Finished(arg, ret) => format!("(fn({:?}) {:?})", arg, ret),
            LazyFnType::Pending { arg, ret } => {
                format!("(fn({}) {})", arg.log(pool), ret.log(pool))
            }
        }
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
                ty.as_ref()
                    .map(|v| v.log(pool))
                    .unwrap_or_else(|| String::from("_")),
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
                ty.as_ref()
                    .map(|v| v.log(pool))
                    .unwrap_or_else(|| String::from("_")),
                value
                    .as_ref()
                    .map(|v| v.log(pool))
                    .unwrap_or_else(|| String::from("uninit()"))
            ),
            Stmt::Eval(e) => e.log(pool),
            Stmt::SetNamed(i, e) => format!("{} = {}", pool.get(*i), e.log(pool)),
            Stmt::DeclFunc(func) => format!("declare(fn {})", func.synth_name(pool)),
            Stmt::Noop => "".to_owned(),
            _ => format!("{:?}", self),
        }
    }
}

impl<'p> PoolLog<'p> for LazyType<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        match self {
            LazyType::Infer => "Infer".into(),
            LazyType::PendingEval(e) => e.log(pool),
            LazyType::Finished(ty) => format!("{:?}", ty),
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
            Expr::Block {
                body,
                result,
                locals,
            } => {
                let es: Vec<_> = body
                    .iter()
                    .map(|e| e.log(pool))
                    .filter(|s| !s.is_empty())
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
                format!("tuple({})", args)
            }
            Expr::RefType(e) => format!("&({})", e.log(pool)),
            Expr::Value(Value::Unit) => "unit".to_string(),
            Expr::Value(v) => format!("{:?}", v),
            Expr::GetVar(v) => v.log(pool),
            Expr::Closure(f) => format!("closure(fn {})", f.synth_name(pool)),
            Expr::SuffixMacro(i, e) => format!("{}!{}", e.log(pool), pool.get(*i)),
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
        format!(
            "[fn {} {:?} {} = \nCONSTANTS: \n{} \nBODY: \n{}\nEND\n A:{:?}]\n{}\n",
            self.synth_name(pool),
            self.get_name(pool),
            self.ty.log(pool),
            self.local_constants
                .iter()
                .map(|e| e.log(pool))
                .collect::<Vec<_>>()
                .join("\n"),
            self.body
                .as_ref()
                .map(|e| e.log(pool))
                .unwrap_or_else(|| "@forward_decl()".to_owned()),
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
            }
        )
    }
}

impl<'p> PoolLog<'p> for DebugInfo<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        format!("// {} ", self.internal_loc)
    }
}

impl<'p> PoolLog<'p> for FnBody<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        let mut f = String::new();
        writeln!(f, "=== Bytecode for {:?} at {:?} ===", self.func, self.when);
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
            } => write!(f, "{ret:?} = call({func_slot:?}, {arg:?});"),
            Bc::CallDirect { f: func, ret, arg } => {
                write!(f, "{ret:?} = call(f({:?}), {arg:?});", func.0)
            }
            Bc::CallDirectMaybeCached { f: func, ret, arg } => {
                write!(f, "{ret:?} = cached_call(f({:?}), {arg:?});", func.0)
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
            Bc::MoveCreateTuple { values, target } => {
                write!(f, "{target:?} = move{values:?};")
            }
            Bc::CloneCreateTuple { values, target } => {
                write!(f, "{target:?} = @clone{values:?};")
            }
            Bc::Ret(i) => write!(f, "return {i:?};"),
            Bc::Clone { from, to } => write!(f, "{:?} = @clone({:?});", to, from),
            Bc::CloneRange { from, to } => write!(f, "{:?} = @clone({:?});", to, from),
            Bc::Move { from, to } => write!(f, "{:?} = move({:?});", to, from),
            Bc::ExpandTuple { from, to } => write!(f, "{:?} = move({:?});", to, from),
            Bc::MoveRange { from, to } => write!(f, "{:?} = move({:?});", to, from),
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
            Stmt::SetVar(_, e) => Some(e.loc),
            Stmt::DeclVar {
                name,
                ty,
                value,
                dropping,
                kind,
            } => value.as_ref().or(ty.as_ref()).map(|e| e.loc),
            Stmt::DeclNamed {
                name,
                ty,
                value,
                kind,
            } => todo!(),
            Stmt::SetNamed(_, _) => todo!(),
        }
    }
}

impl<'a, 'p> Interp<'a, 'p> {
    #[track_caller]
    pub fn log_trace(&self) -> String {
        let mut out = String::new();
        #[cfg(not(feature = "some_log"))]
        {
            return out;
        }
        writeln!(out, "=== TRACE ===");
        writeln!(out, "{}", Location::caller());

        for (i, s) in self.debug_trace.iter().enumerate() {
            writeln!(out, "{i} {};", s.log(self.pool, self.program));
        }
        writeln!(out, "=============");
        out
    }

    pub fn log_stack(&self) {
        #[cfg(not(feature = "spam_log"))]
        {
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
            Value::Enum {
                container_type,
                tag,
                value,
            } => todo!(),
            Value::Tuple {
                container_type,
                values,
            } => {
                write!(f, "(");
                for v in values {
                    write!(f, "{v}, ")?;
                }
                write!(f, ")")
            }
            _ => write!(f, "{self:?}"),
        }
    }
}

impl<'p> Debug for CompileError<'p> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "COMPILATION ERROR:")?;
        writeln!(f, "{:?}", self.reason)?;
        write!(f, "{}", self.trace)
    }
}

impl<'p> DebugState<'p> {
    fn log(&self, pool: &StringPool<'p>, program: &Program<'p>) -> String {
        let show_f = |func: FuncId| {
            format!(
                "f{}:{:?}:{}",
                func.0,
                program.funcs[func.0].get_name(pool),
                program.funcs[func.0].synth_name(pool)
            )
        };
        match self {
            DebugState::OuterCall(f, arg) => {
                format!("| Prep Interp | {} on val:{arg:?}", show_f(*f))
            }
            DebugState::JitToBc(f, when) => {
                format!("| Jit Bytecode| {} for {:?}", show_f(*f), when)
            }
            DebugState::EvalConstants(f) => {
                format!("| Eval Consts | {:?}", show_f(*f))
            }
            DebugState::RunInstLoop(f) => format!("| Loop Insts  | {}", show_f(*f)),
            DebugState::ComputeCached(e) => format!("| Cache Eval  | {}", e.log(pool)),
            DebugState::ResolveFnType(f, arg, ret) => {
                format!(
                    "| Resolve Type| {} is fn({}) {}",
                    show_f(*f),
                    arg.log(pool),
                    ret.log(pool)
                )
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
            CErr::Msg(s) => s.clone(),
            _ => format!("{:?}", self),
        }
    }
}
