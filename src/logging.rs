use core::fmt;
use std::fmt::{Debug, Write};

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

// I want to be as easy to use my error system as just paniking.
// Use this one for things that aren't supported yet or should have been caught in a previous stage of compilation.
macro_rules! ice {
    ($self:expr, $($arg:tt)*) => {{
        let msg = format!($($arg)*);
        return err!($self, CErr::IceFmt(msg))
    }};
}

pub enum LogTag {
    Parsing,
    InstLoop,
    Jitting,
}

macro_rules! log {
    // Using cfg!(...) instead of #[cfg(...)] to avoid unused var warnings.
    ($($arg:tt)*) => {{
        if cfg!(feature = "logging") {
            print!($($arg)*);
        }
    }};
}
macro_rules! logln {
    // Using cfg!(...) instead of #[cfg(...)] to avoid unused var warnings.
    ($($arg:tt)*) => {{
        if cfg!(feature = "logging") {
            println!($($arg)*);
        }
    }};
}

pub(crate) use assert;
pub(crate) use assert_eq;
pub(crate) use bin_int;
pub(crate) use check;
pub(crate) use err;
pub(crate) use ice;
pub(crate) use log;
pub(crate) use logln;

use crate::{
    ast::{Expr, FatExpr, Func, LazyFnType, LazyType, Program, Stmt, TypeId, TypeInfo, Var},
    interp::{Bc, FnBody, Interp, StackOffset, StackRange, Value},
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
            TypeInfo::Array(e) => format!("Array({})", self.log_type(*e)),
            TypeInfo::Struct(e) => format!("Struct({}, {})", t.0, self.log_type(*e)),
            TypeInfo::Fn(f) => format!(
                "fn({}) {}",
                self.log_type(f.param),
                self.log_type(f.returns)
            ),
            TypeInfo::Tuple(v) => {
                let v: Vec<_> = v.iter().map(|v| self.log_type(*v)).collect();
                format!("Tuple({})", v.join(", "))
            }
            TypeInfo::Enum(_) => "Enum(TODO)".to_owned(),
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
                writeln!(s, "{:?}", bc);
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
            Stmt::DeclNamed { name, ty, value } => format!(
                "let {}: {} = {};",
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
            Stmt::Eval(e) => format!("L{}# {}", e.loc.row + 1, e.log(pool)),
            Stmt::SetNamed(i, e) => format!("{} = {}", pool.get(*i), e.log(pool)),
            Stmt::DeclFunc(func) => format!(
                "L{}# declare(fn {})",
                func.body
                    .as_ref()
                    .map(|b| b.loc.row + 1)
                    .unwrap_or(1234567890),
                func.synth_name(pool)
            ),
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
            Expr::Closure(f) => format!("declare(fn {})", f.synth_name(pool)),
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
        format!("{}%{}", pool.get(self.0), self.1)
    }
}

impl<'p> PoolLog<'p> for Func<'p> {
    fn log(&self, pool: &StringPool<'p>) -> String {
        format!(
            "[fn {} {:?} {} = {}; A:{:?}]",
            self.synth_name(pool),
            self.get_name(pool),
            self.ty.log(pool),
            self.body
                .as_ref()
                .map(|e| e.log(pool))
                .unwrap_or_else(|| "@forward_decl()".to_owned()),
            self.annotations.iter().map(|i| pool.get(i.name))
        )
    }
}

impl Debug for FnBody<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "=== Bytecode for {:?} at {:?} ===", self.func, self.when)?;
        writeln!(f, "TYPES: {:?}", &self.slot_types);
        for (i, bc) in self.insts.iter().enumerate() {
            writeln!(f, "{i}. {bc:?}");
        }
        writeln!(f, "===============")?;
        Ok(())
    }
}

impl Debug for Bc<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Bc::CallDynamic {
                f: func_slot,
                ret,
                arg,
            } => write!(f, "{ret:?} = call({func_slot:?}, {arg:?});")?,
            Bc::CallDirect { f: func, ret, arg } => {
                write!(f, "{ret:?} = call(f({:?}), {arg:?});", func.0)?
            }
            Bc::CallDirectMaybeCached { f: func, ret, arg } => {
                write!(f, "{ret:?} = cached_call(f({:?}), {arg:?});", func.0)?
            }
            Bc::CallBuiltin { name, ret, arg } => {
                write!(f, "{ret:?} = builtin(i({}), {arg:?});", name.0)?
            }
            Bc::LoadConstant { slot, value } => write!(f, "{:?} = {:?};", slot, value)?,
            Bc::JumpIf {
                cond,
                true_ip,
                false_ip,
            } => write!(
                f,
                "if ({:?}) goto {} else goto {};",
                cond, true_ip, false_ip
            )?,
            Bc::Goto { ip } => write!(f, "goto {ip};",)?,
            Bc::MoveCreateTuple { values, target } => {
                write!(f, "{target:?} = move{values:?};")?;
            }
            Bc::CloneCreateTuple { values, target } => {
                write!(f, "{target:?} = @clone{values:?};")?;
            }
            Bc::Ret(i) => write!(f, "return {i:?};")?,
            Bc::Clone { from, to } => write!(f, "{:?} = @clone({:?});", to, from)?,
            Bc::CloneRange { from, to } => write!(f, "{:?} = @clone({:?});", to, from)?,
            Bc::Move { from, to } => write!(f, "{:?} = move({:?});", to, from)?,
            Bc::ExpandTuple { from, to } => write!(f, "{:?} = move({:?});", to, from)?,
            Bc::MoveRange { from, to } => write!(f, "{:?} = move({:?});", to, from)?,
            Bc::Drop(i) => write!(f, "drop({:?});", i)?,
            Bc::AbsoluteStackAddr { of, to } => write!(f, "{:?} = @addr({:?});", to, of)?,
            Bc::DebugMarker(s, i) => write!(f, "debug({:?}, {:?});", s, i)?,
        }
        Ok(())
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
        if self.is_any() {
            write!(f, "TyAny")
        } else {
            write!(f, "Ty{}", self.0)
        }
    }
}
