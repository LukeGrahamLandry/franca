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

#[derive(Copy, Clone, PartialEq, Hash, Eq, Debug)]
pub struct TypeId(pub usize);

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
    Type,
    Unit, // TODO: same as empty tuple but easier to type
}

#[derive(Copy, Clone, PartialEq, Hash, Eq, Debug)]
pub struct Var(usize, usize);

#[derive(Clone, PartialEq, Debug)]
pub enum Expr<'p> {
    Num(f64),
    Call(Box<Self>, Box<Self>),
    Block(Vec<Stmt<'p>>, Box<Self>),
    IfElse(Box<Self>, Box<Self>, Box<Self>),
    Array(Vec<Self>),
    Tuple(Vec<Self>),
    RefType(Box<Self>),
    EnumLiteral(Vec<(Ident<'p>, Self)>),
    StructLiteral(Vec<(Ident<'p>, Self)>),
    // Backend only
    GetVar(Var),

    // Frontend only
    GetNamed(Ident<'p>),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Stmt<'p> {
    Eval(Expr<'p>),

    // Backend Only
    SetVar(Var, Expr<'p>),
    Scope(Vec<Var>, Box<Self>),

    // Frontend only
    DeclVar(Ident<'p>),
    SetNamed(Ident<'p>, Expr<'p>),
    DeclFunc(Func<'p>),

    /// for <free> with <cond> { <definitions> }
    Generic {
        free: Ident<'p>,
        cond: Expr<'p>,
        definitions: Box<Self>,
    },
}

#[derive(Clone, PartialEq, Debug)]
pub struct Func<'p> {
    pub name: Ident<'p>,
    pub ty: LazyFnType<'p>,     // We might not have typechecked yet.
    pub body: Option<Expr<'p>>, // It might be a forward declaration / ffi.
    pub arg_names: Vec<Option<Ident<'p>>>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum LazyFnType<'p> {
    Finished(TypeId, TypeId),
    Pending {
        arg: LazyType<'p>,
        ret: LazyType<'p>,
    },
}

#[derive(Clone, PartialEq, Debug)]
pub enum LazyType<'p> {
    Infer,
    PendingEval(Expr<'p>),
    Finished(TypeId),
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub struct FuncId(pub usize);

#[derive(Clone, Default)]
pub struct Program<'p> {
    pub types: Vec<TypeInfo>,
    // At the call site, you know the name but not the type.
    // So you need to look at everybody that might be declaring the function you're trying to call.
    pub declarations: HashMap<Ident<'p>, Vec<FuncId>>,
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
            Stmt::DeclFunc(func) => format!(
                "fn {} {:?} = {:?}",
                pool.get(func.name),
                func.ty.log(pool),
                func.body.as_ref().map(|e| e.log(pool))
            ),
            _ => todo!(),
        }
    }
}

impl<'p> LazyFnType<'p> {
    pub fn log(&self, pool: &StringPool) -> String {
        match self {
            LazyFnType::Finished(arg, ret) => format!("(fn({:?}) {:?})", arg, ret),
            LazyFnType::Pending { arg, ret } => {
                format!("(fn({}) {})", arg.log(pool), ret.log(pool))
            }
        }
    }

    pub fn of(arg_type: Option<Expr<'p>>, return_type: Option<Expr<'p>>) -> LazyFnType<'p> {
        let arg = match &return_type {
            Some(arg) => LazyType::PendingEval(arg.clone()),
            None => LazyType::Infer,
        };

        let ret = match &return_type {
            Some(ret) => LazyType::PendingEval(ret.clone()),
            None => LazyType::Infer,
        };
        LazyFnType::Pending { arg, ret }
    }

    pub fn unwrap(&self) -> (TypeId, TypeId) {
        match self {
            LazyFnType::Finished(arg, ret) => (*arg, *ret),
            LazyFnType::Pending { arg, ret } => panic!("Not ready {:?} {:?}", arg, ret),
        }
    }
}

// TODO: print actual type info
impl<'p> LazyType<'p> {
    pub fn log(&self, pool: &StringPool) -> String {
        match self {
            LazyType::Infer => "Infer".into(),
            LazyType::PendingEval(e) => e.log(pool),
            LazyType::Finished(ty) => format!("{:?}", ty),
        }
    }

    pub fn unwrap(&self) -> TypeId {
        match self {
            LazyType::Finished(ty) => *ty,
            _ => panic!("Type not ready: {:?}", self),
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
            TypeInfo::Type => "Type".to_owned(),
            TypeInfo::Unit => "Unit".to_owned(),
        }
    }

    pub fn slot_count(&self, ty: TypeId) -> usize {
        match &self.types[ty.0] {
            TypeInfo::Tuple(args) => args.iter().map(|t| self.slot_count(*t)).sum(),
            TypeInfo::Struct(_) => todo!(),
            _ => 1,
        }
    }

    pub fn intern_type(&mut self, ty: TypeInfo) -> TypeId {
        let id = self.types.len();
        self.types.push(ty);
        TypeId(id)
    }

    pub fn add_func<'a>(&'a mut self, func: Func<'p>) -> FuncId {
        let id = FuncId(self.funcs.len());
        let name = func.name;
        self.funcs.push(func);
        // TODO: add to func_lookup

        // TODODODODO: wrong! need comptiem intern. need resolve. just testing.
        // assert!(self.declarations.get(&name).is_none(), "TODO");
        self.declarations.insert(name, vec![id]);
        id
    }
}
