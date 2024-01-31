use crate::{
    interp::Value,
    pool::{Ident, StringPool},
};
use std::{
    collections::HashMap,
    fmt::{format, Debug},
    hash::Hash,
    marker::PhantomData,
    rc::Rc,
    sync::RwLock,
};

#[derive(Copy, Clone, PartialEq, Hash, Eq)]
pub struct TypeId(usize);

#[derive(Clone, PartialEq, Hash, Eq)]
pub struct FnType {
    // Functions with multiple arguments are treated as a tuple.
    pub param: TypeId,
    pub returns: TypeId,
}

pub struct FuncFlags {
    // Calls must complete and produce a value at compile time.
    pub comptime: bool,
    // I don't support escaping captures. Calls must be inlined.
    pub has_captures: bool,
    // Used for imported c functions.
    pub no_overloads: bool,
    // This is a special one the compiler might have a better implementation for.
    pub intrinsic: bool,
}

#[derive(Clone, PartialEq, Hash, Eq)]
pub enum TypeInfo {
    Never,
    F64,
    I64,
    Bool,
    Fn(FnType),
    Tuple(Vec<TypeId>),
    Ptr(TypeId),
    Array(TypeId),
    Struct(TypeId),
    Enum(Vec<TypeId>),
}

#[derive(Copy, Clone, PartialEq, Hash, Eq)]
pub struct Var(usize, usize);

#[derive(Clone, PartialEq)]
pub enum Expr<'p> {
    Num(f64),
    Call(Box<Self>, Box<Self>),
    Block(Vec<Stmt<'p>>, Box<Self>),
    IfElse(Box<Self>, Box<Self>, Box<Self>),
    Array(Vec<Self>),
    Tuple(Vec<Self>),
    RefType(Box<Self>),

    // Backend only
    GetVar(Var),

    // Frontend only
    GetNamed(Ident<'p>),
}

#[derive(Clone, PartialEq)]
pub enum Stmt<'p> {
    Eval(Expr<'p>),

    // Backend Only
    SetVar(Var, Expr<'p>),
    Scope(Vec<Var>, Box<Self>),

    // Frontend only
    DeclVar(Ident<'p>),
    SetNamed(Ident<'p>, Expr<'p>),
    DeclFunc {
        name: Ident<'p>,
        return_type: Option<Expr<'p>>,
        body: Option<Expr<'p>>,
    },

    /// for <free> with <cond> { <definitions> }
    Generic {
        free: Ident<'p>,
        cond: Expr<'p>,
        definitions: Box<Self>,
    },
}

#[derive(Clone, PartialEq)]
pub struct Func<'p> {
    pub name: Ident<'p>,
    pub ty: TypeId,
    pub body: Expr<'p>,
}

#[derive(Copy, Clone, PartialEq)]
pub struct FuncId(pub usize);

#[derive(Clone, Default)]
pub struct Program<'p> {
    pub types: Vec<TypeInfo>,
    // At the call site, you know the name but not the type.
    // So you need to look at everybody that might be declaring the function you're trying to call.
    pub declarations: HashMap<Ident<'p>, Vec<Stmt<'p>>>,
    // If you already know the arg/ret type at a callsite, you can just grab the function directly.
    pub func_lookup: HashMap<(Ident<'p>, TypeId), FuncId>,
    pub funcs: Vec<Func<'p>>,
    /// Comptime function calls that return a type are memoized so identity works out.
    pub generics_memo: HashMap<(FuncId, Value), TypeId>,
}

impl<'p> Stmt<'p> {
    pub fn log(&self, pool: &StringPool) -> String {
        match self {
            &Stmt::DeclVar(i) => format!("let {};", pool.get(i)),
            Stmt::Eval(e) => e.log(pool),
            Stmt::SetNamed(i, e) => format!("{} = {}", pool.get(*i), e.log(pool)),
            Stmt::Generic {
                free,
                cond,
                definitions,
            } => todo!(),
            Stmt::DeclFunc {
                name,
                return_type,
                body,
            } => format!(
                "fn{} {:?} = {:?}",
                pool.get(*name),
                return_type.as_ref().map(|e| e.log(pool)),
                body.as_ref().map(|e| e.log(pool))
            ),
            _ => todo!(),
        }
    }
}

impl<'p> Expr<'p> {
    pub fn log(&self, pool: &StringPool) -> String {
        match self {
            Expr::Num(n) => n.to_string(),
            Expr::Call(func, arg) => {
                format!("{}({})", func.log(pool), arg.log(pool))
            }
            &Expr::GetNamed(i) => pool.get(i).to_string(),
            Expr::Block(es, val) => {
                let es: Vec<_> = es.iter().map(|e| e.log(pool)).collect();
                let es = es.join("; ");
                format!("{{ {}; {} }}", es, val.log(pool))
            }
            Expr::IfElse(cond, yes, no) => format!(
                "if {} {{ {} }} else {{ {} }}",
                cond.log(pool),
                yes.log(pool),
                no.log(pool)
            ),
            Expr::Array(args) => {
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
            _ => todo!(),
        }
    }
}

impl<'p> Program<'p> {
    pub fn log_type(&self, t: TypeId) -> String {
        match &self.types[t.0] {
            TypeInfo::Never => "Never".to_owned(),
            TypeInfo::F64 => "f64".to_owned(),
            TypeInfo::I64 => "i64".to_owned(),
            TypeInfo::Bool => "bool".to_owned(),
            TypeInfo::Ptr(e) => format!("Ptr({})", self.log_type(t)),
            TypeInfo::Array(e) => format!("Array({})", self.log_type(t)),
            TypeInfo::Struct(e) => format!("Struct({}, {})", t.0, self.log_type(t)),
            TypeInfo::Fn(f) => format!(
                "fn({}) {}",
                self.log_type(f.param),
                self.log_type(f.returns)
            ),
            TypeInfo::Tuple(_) => "Tuple(TODO)".to_owned(),
            TypeInfo::Enum(_) => "Enum(TODO)".to_owned(),
        }
    }
}
