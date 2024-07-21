// nobody cares its just logging. TODO: should do it anyway i guess.
#![allow(unused_must_use)]
use core::fmt;
use std::fmt::{Debug, Formatter};

#[track_caller]
#[inline(never)]
pub(crate) fn break_here(e: &CErr) {
    let depth = unsafe { EXPECT_ERR_DEPTH.load(std::sync::atomic::Ordering::SeqCst) };
    if depth == 0 {
        // TODO: make this never happen so dont have to worry about short circuiting -- Apr 25
        println!("err: {e:?} at {}", std::panic::Location::caller());
    }
}

#[track_caller]
pub(crate) fn make_err(reason: CErr) -> Box<CompileError> {
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

        return $crate::export_ffi::BigResult::Err($crate::logging::make_err(e))
    }};
    ($($arg:tt)*) => {{
        let msg = format!($($arg)*);
        eprintln!("{msg}");
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
#[macro_export]
macro_rules! unwrap2 {
    ($maybe:expr, $($arg:tt)*) => {{
        if let BigOption::Some(value) = $maybe {
            value
        } else {
            $crate::ice!("Missing value {}.\n{}", stringify!($maybe), format!($($arg)*))
        }
    }};
}

pub use unwrap;

use crate::ast::{FatStmt, FuncImpl, Pattern};
use crate::self_hosted::log_pattern;
use crate::self_hosted::{log_expr, log_func, log_lazy_type, log_stmt, SelfHosted};
use crate::{
    ast::{FatExpr, Func, FuncId, LazyType, Program, TypeId, TypeInfo, Var},
    compiler::{CErr, CompileError, DebugState},
};

/// It felt like a good idea to be able to compare identifiers super fast but now its forever a pain to look at them while debugging.
pub trait PoolLog<'p> {
    fn log(&self, pool: &SelfHosted<'p>) -> String;
}

use crate::ast::safe_rec;
use crate::compiler::EXPECT_ERR_DEPTH;

impl<'p> Program<'p> {
    pub(crate) fn log_type(&self, t: TypeId) -> String {
        if let Some(&Some(name)) = self.inferred_type_names.get(t.as_index()) {
            let name = self.pool.get(name);
            // HACK: need to be smarter about when to infer an assignment as a type name.
            if name != "Self" && name.len() != 1 {
                return name.to_string();
            }
        }

        safe_rec!(self, t, format!("{t:?}"), {
            if t.is_valid() {
                // && t.as_index() < self.types.len()
                match &self[t] {
                    TypeInfo::Placeholder => "UnfinishedPlaceHolder".to_owned(),
                    TypeInfo::Never => "Never".to_owned(),
                    TypeInfo::F64 => "f64".to_owned(),
                    TypeInfo::F32 => "f32".to_owned(),
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
                    TypeInfo::Enum { raw: inner, fields, .. } => {
                        format!(
                            "{}:{:?}",
                            self.log_type(*inner),
                            fields.iter().map(|(n, _)| self.pool.get(*n)).collect::<Vec<_>>()
                        )
                    }
                    TypeInfo::Named(_, n) => self.pool.get(*n).to_string(),
                    TypeInfo::Fn(f) => format!("(fn({}) {} #arity({}))", self.log_type(f.arg), self.log_type(f.ret), f.arity),
                    TypeInfo::FnPtr { ty: f, cc } => {
                        format!("&(fn({}) {} #{cc:?} #arity({}))", self.log_type(f.arg), self.log_type(f.ret), f.arity)
                    }
                    TypeInfo::Label(e) => format!("Label({})", self.log_type(*e)),

                    TypeInfo::Unit => "void".to_owned(),
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
}

impl<'p> PoolLog<'p> for LazyType<'p> {
    fn log(&self, pool: &SelfHosted<'p>) -> String {
        unsafe { (*log_lazy_type(pool.pool, self)).to_string() }
    }
}
impl<'p> PoolLog<'p> for Pattern<'p> {
    fn log(&self, pool: &SelfHosted<'p>) -> String {
        unsafe { (*log_pattern(pool.pool, self)).to_string() }
    }
}

impl<'p> PoolLog<'p> for FatExpr<'p> {
    fn log(&self, pool: &SelfHosted<'p>) -> String {
        unsafe { (*log_expr(pool.pool, self)).to_string() }
    }
}

impl<'p> PoolLog<'p> for FatStmt<'p> {
    fn log(&self, pool: &SelfHosted<'p>) -> String {
        unsafe { (*log_stmt(pool.pool, self)).to_string() }
    }
}

impl<'p> PoolLog<'p> for Var<'p> {
    fn log(&self, pool: &SelfHosted<'p>) -> String {
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
    fn log(&self, pool: &SelfHosted<'p>) -> String {
        unsafe { (*log_func(pool.pool, self)).to_string() }
    }
}

impl<'p> Debug for CompileError<'p> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "COMPILATION ERROR:")?;
        write!(f, "{}", self.trace)?;
        writeln!(f, "{:?}", self.reason)?;
        writeln!(f, "Internal: {}", self.internal_loc.unwrap())?;
        Ok(())
    }
}

impl<'p> DebugState<'p> {
    pub(crate) fn log(&self, pool: &SelfHosted<'p>, _: &Program<'p>) -> String {
        match self {
            DebugState::Msg(s) => s.clone(),
            DebugState::Compile(f, i) => format!("| Compile    | {:?} {}", *f, pool.get(*i)),
            DebugState::EnsureCompiled(f, i, when) => format!("| Ensure Comp | {:?} {} for {:?}", *f, pool.get(*i), when),
            DebugState::EmitBody(f, i) => format!("|  Emit Body  | {:?} {}", *f, pool.get(*i)),
            DebugState::EvalConstants(f, i) => format!("| Eval Consts | {:?} {}", *f, pool.get(*i)),
            DebugState::EmitCapturingCall(f, i) => format!("| Captur Call | {:?} {}", *f, pool.get(*i)),
            DebugState::RunInstLoop(f, i) => format!("| Execute  | {:?} {}", *f, pool.get(*i)),
            DebugState::ComptimeCall(f, i) => format!("| Comptime Call | {:?} {}", *f, pool.get(*i)),
            DebugState::ResolveFnType(f, i) => format!("| Resolve Type| {:?} {}", *f, pool.get(*i)),
            DebugState::ResolveConstant(v) => format!("| Find Const | {}", v.log(pool)),
            _ => format!("{self:?}"),
        }
    }
}

impl<'p> CErr<'p> {
    pub(crate) fn log(&self, program: &Program<'p>, pool: &SelfHosted<'p>) -> String {
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
                // TODO: suggest [] or & if its just mismatching levels of indirection.
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
                    // :coerce_for_const_arg
                    format!("{indent} you can implicitly convert from a function to an overload set only if its constant. \n(currently you might have to introduce an extra :: name binding, this will be fixed eventually.) ")
                } else {
                    format!("{indent} bad conversion")
                }
            }
            _ => format!("{indent} bad conversion"),
        }
    }
}

impl Debug for FuncId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Fn{}", self.as_index())
    }
}

impl<'p> PoolLog<'p> for FuncImpl<'p> {
    fn log(&self, pool: &SelfHosted<'p>) -> String {
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
            &FuncImpl::CompilerBuiltin(n) => format!("CompilerBuiltin({})", pool.get(n)),
        }
    }
}

fn sane_int_size(bits: i64) -> bool {
    matches!(bits, 8 | 16 | 32 | 64 | 128)
}
