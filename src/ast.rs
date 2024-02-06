use tree_sitter::Point;

use crate::{
    interp::Value,
    pool::{Ident, StringPool},
};
use std::{
    collections::HashMap,
    fmt::{format, Debug},
    hash::Hash,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    rc::Rc,
    sync::RwLock,
};

#[derive(Copy, Clone, PartialEq, Hash, Eq)]
pub struct TypeId(usize);

#[derive(Clone, PartialEq, Hash, Eq, Debug)]
pub struct FnType {
    // Functions with multiple arguments are treated as a tuple.
    pub param: TypeId,
    pub returns: TypeId,
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

impl TypeId {
    pub fn is_any(&self) -> bool {
        self.0 == 0
    }

    /// Placeholder to use while working on typechecking.
    pub fn any() -> TypeId {
        TypeId(0)
    }
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

#[derive(Clone, PartialEq, Hash, Eq, Debug)]
pub enum TypeInfo {
    Any,
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

#[derive(Clone, PartialEq, Hash, Eq, Debug)]
pub struct Annotation<'p> {
    pub name: Ident<'p>,
    pub args: Option<Expr<'p>>,
}

#[derive(Copy, Clone, PartialEq, Hash, Eq, Debug)]
pub struct Var<'p>(pub Ident<'p>, pub usize);

#[derive(Clone, PartialEq, Debug, Hash, Eq)]
pub enum Expr<'p> {
    Value(Value),
    Call(Box<FatExpr<'p>>, Box<FatExpr<'p>>),
    Block(Vec<Stmt<'p>>, Box<FatExpr<'p>>),
    IfElse(Box<Self>, Box<FatExpr<'p>>, Box<FatExpr<'p>>),
    Array(Vec<FatExpr<'p>>),
    Tuple(Vec<FatExpr<'p>>),
    RefType(Box<FatExpr<'p>>),
    EnumLiteral(Vec<(Ident<'p>, FatExpr<'p>)>),
    StructLiteral(Vec<(Ident<'p>, FatExpr<'p>)>),
    Closure(Box<Func<'p>>),
    SuffixMacro(Ident<'p>, Box<FatExpr<'p>>),
    // Backend only
    GetVar(Var<'p>),

    // Frontend only
    GetNamed(Ident<'p>),
}

#[derive(Clone, Debug, Eq)]
pub struct FatExpr<'p> {
    pub expr: Expr<'p>,
    pub loc: Point,
    pub id: usize,
}

impl PartialEq for FatExpr<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Hash for FatExpr<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.id)
    }
}

#[derive(Clone, PartialEq, Debug, Hash, Eq)]
pub enum Stmt<'p> {
    Noop,
    Eval(FatExpr<'p>),

    // Backend Only
    SetVar(Var<'p>, FatExpr<'p>),
    Scope(Vec<Var<'p>>, Box<Self>),

    // Frontend only
    DeclVar(Ident<'p>, Box<FatExpr<'p>>),
    SetNamed(Ident<'p>, FatExpr<'p>),
    DeclFunc(Func<'p>),

    /// for <free> with <cond> { <definitions> }
    Generic {
        free: Ident<'p>,
        cond: FatExpr<'p>,
        definitions: Box<Self>,
    },
}

#[derive(Clone, PartialEq, Debug, Hash, Eq)]
pub struct Func<'p> {
    pub annotations: Vec<Annotation<'p>>,
    pub name: Option<Ident<'p>>,   // it might be an annonomus closure
    pub ty: LazyFnType<'p>,        // We might not have typechecked yet.
    pub body: Option<FatExpr<'p>>, // It might be a forward declaration / ffi.
    pub arg_names: Vec<Option<Ident<'p>>>,
}

impl<'p> Func<'p> {
    pub fn synth_name(&self, pool: &StringPool<'p>) -> &'p str {
        pool.get(self.get_name(pool))
    }

    pub fn get_name(&self, pool: &StringPool<'p>) -> Ident<'p> {
        self.name.unwrap_or_else(|| pool.intern("@anon@"))
    }
}

#[derive(Clone, PartialEq, Debug, Hash, Eq)]
pub enum LazyFnType<'p> {
    Finished(TypeId, TypeId),
    Pending {
        arg: LazyType<'p>,
        ret: LazyType<'p>,
    },
}

#[derive(Clone, PartialEq, Debug, Hash, Eq)]
pub enum LazyType<'p> {
    Infer,
    PendingEval(FatExpr<'p>),
    Finished(TypeId),
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub struct FuncId(pub usize);

#[derive(Clone)]
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
    pub fn log(&self, pool: &StringPool<'p>) -> String {
        match self {
            Stmt::DeclVar(i, v) => format!("let {} = {};", pool.get(*i), v.log(pool)),
            Stmt::Eval(e) => e.log(pool),
            Stmt::SetNamed(i, e) => format!("{} = {}", pool.get(*i), e.log(pool)),
            Stmt::Generic {
                free,
                cond,
                definitions,
            } => todo!(),
            Stmt::DeclFunc(func) => format!(
                "fn {} {:?} = {:?}",
                func.synth_name(pool),
                func.ty.log(pool),
                func.body.as_ref().map(|e| e.log(pool))
            ),
            Stmt::Noop => "".to_owned(),
            _ => todo!(),
        }
    }
}

impl<'p> LazyFnType<'p> {
    pub fn log(&self, pool: &StringPool<'p>) -> String {
        match self {
            LazyFnType::Finished(arg, ret) => format!("(fn({:?}) {:?})", arg, ret),
            LazyFnType::Pending { arg, ret } => {
                format!("(fn({}) {})", arg.log(pool), ret.log(pool))
            }
        }
    }

    pub fn of(arg_type: Option<FatExpr<'p>>, return_type: Option<FatExpr<'p>>) -> LazyFnType<'p> {
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
    pub fn log(&self, pool: &StringPool<'p>) -> String {
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
    pub fn log(&self, pool: &StringPool<'p>) -> String {
        match self {
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
            Expr::Value(v) => format!("{:?}", v),
            _ => format!("{:?}", self),
        }
    }
}

impl<'p> Default for Program<'p> {
    fn default() -> Self {
        Self {
            // Any needs to be first becuase I use TypeId(0) as a place holder.
            // The rest are just common ones that i want to find faster if i end up iterating the array.
            types: vec![
                TypeInfo::Any,
                TypeInfo::Unit,
                TypeInfo::I64,
                TypeInfo::Bool,
                TypeInfo::Type,
                TypeInfo::F64,
            ],
            declarations: Default::default(),
            func_lookup: Default::default(),
            funcs: Default::default(),
            generics_memo: Default::default(),
        }
    }
}

impl<'p> Program<'p> {
    pub fn log_type(&self, t: TypeId) -> String {
        match &self.types[t.0] {
            TypeInfo::Any => "Any".to_owned(),
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
            TypeInfo::Tuple(v) => {
                let v: Vec<_> = v.iter().map(|v| self.log_type(*v)).collect();
                format!("Tuple({})", v.join(", "))
            }
            TypeInfo::Enum(_) => "Enum(TODO)".to_owned(),
            TypeInfo::Type => "Type".to_owned(),
            TypeInfo::Unit => "Unit".to_owned(),
        }
    }

    pub fn log_cached_types(&self) {
        logln!("=== CACHED TYPES ===");
        for (i, ty) in self.types.iter().enumerate() {
            logln!("- id({i}) = {} = {:?}", self.log_type(TypeId(i)), ty);
        }
        logln!("====================");
    }

    pub fn slot_count(&self, ty: TypeId) -> usize {
        match &self.types[ty.0] {
            TypeInfo::Tuple(args) => args.iter().map(|t| self.slot_count(*t)).sum(),
            TypeInfo::Struct(_) => todo!(),
            _ => 1,
        }
    }

    // TODO: this is O(n), at the very least make sure the common types are at the beginning.
    pub fn intern_type(&mut self, ty: TypeInfo) -> TypeId {
        let id = self
            .types
            .iter()
            .position(|check| check == &ty)
            .unwrap_or_else(|| {
                let id = self.types.len();
                self.types.push(ty);
                id
            });
        TypeId(id)
    }

    pub fn add_func<'a>(&'a mut self, func: Func<'p>) -> FuncId {
        let id = FuncId(self.funcs.len());
        let name = func.name;
        self.funcs.push(func);
        // TODO: add to func_lookup

        // TODODODODO: wrong! need comptiem intern. need resolve. just testing.
        // assert!(self.declarations.get(&name).is_none(), "TODO");
        if let Some(name) = name {
            self.declarations.insert(name, vec![id]);
        }
        id
    }

    pub fn returns_type(&self, f: FuncId) -> bool {
        let func = &self.funcs[f.0];
        let (_, ret) = func.ty.unwrap();
        let ty = &self.types[ret.0];
        ty == &TypeInfo::Type
    }

    pub fn type_of(&mut self, v: &Value) -> TypeId {
        match v {
            Value::F64(_) => todo!(),
            Value::I64(_) => self.intern_type(TypeInfo::I64),
            Value::Bool(_) => self.intern_type(TypeInfo::Bool),
            Value::Enum {
                container_type,
                tag,
                value,
            } => *container_type,
            Value::Tuple {
                container_type,
                values,
            }
            | Value::Array {
                container_type,
                values,
            } => *container_type,
            Value::Ptr {
                container_type,
                value,
            } => *container_type,
            Value::Fn(_, _) => todo!(),
            Value::Type(_) => self.intern_type(TypeInfo::Type),
            Value::GetFn(_) => todo!(),
            Value::Unit => todo!(),
            Value::Poison => panic!("Tried to typecheck Value::Poison"),
            Value::Slice(_) => todo!(),
            Value::Map(_, _) => todo!(),
            Value::Symbol(_) => todo!(),
            Value::InterpAbsStackAddr(_) => TypeId::any(),
        }
    }

    pub fn is_type(&self, ty: TypeId, expect: TypeInfo) -> bool {
        self.types[ty.0] == expect
    }
}

impl<'p> Deref for FatExpr<'p> {
    type Target = Expr<'p>;

    fn deref(&self) -> &Self::Target {
        &self.expr
    }
}

impl<'p> DerefMut for FatExpr<'p> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.expr
    }
}
