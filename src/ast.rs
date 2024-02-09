use tree_sitter::Point;

use crate::{
    interp::Value,
    logging::PoolLog,
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
#[macro_use]
use crate::logging::logln;

#[derive(Copy, Clone, PartialEq, Hash, Eq)]
pub struct TypeId(pub usize);

#[derive(Clone, PartialEq, Hash, Eq, Debug)]
pub struct FnType {
    // Functions with multiple arguments are treated as a tuple.
    pub param: TypeId,
    pub returns: TypeId,
}

impl TypeId {
    pub fn is_any(&self) -> bool {
        self.0 == 0
    }

    /// Placeholder to use while working on typechecking.
    pub fn any() -> TypeId {
        TypeId(0)
    }

    // Be careful that this is in the pool correctly!
    pub fn unit() -> TypeId {
        TypeId(1)
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

#[derive(Copy, Clone, PartialEq, Hash, Eq, Debug)]
pub enum VarType {
    Let,
    Var,
    Const,
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
    pub args: Option<FatExpr<'p>>,
}

#[derive(Copy, Clone, PartialEq, Hash, Eq, Debug)]
pub struct Var<'p>(pub Ident<'p>, pub usize);

#[derive(Clone, PartialEq, Debug, Eq)]
pub enum Expr<'p> {
    Value(Value),
    Call(Box<FatExpr<'p>>, Box<FatExpr<'p>>),
    Block {
        body: Vec<Stmt<'p>>,
        result: Box<FatExpr<'p>>,
        locals: Option<Vec<Var<'p>>>, // useful information for calling drop
    },
    ArrayLiteral(Vec<FatExpr<'p>>),
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

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Known {
    // Known at comptime but could be computed at runtime too.
    Foldable,
    RuntimeOnly,
    Maybe,
    // Types, function literals (can convert to FnPtr for runtime if no captures)
    ComptimeOnly,
}

// Some common data needed by all expression types.
// This is annoying and is why I want `using(SomeStructType, SomeEnumType)` in my language.
#[derive(Clone, Debug, Eq)]
pub struct FatExpr<'p> {
    pub expr: Expr<'p>,
    pub loc: Point,
    pub id: usize,
    pub ty: Option<TypeId>,
    pub known: Known,
}

impl<'p> FatExpr<'p> {
    pub fn synthetic(expr: Expr<'p>) -> Self {
        FatExpr {
            expr,
            loc: Point {
                row: 0,
                column: 123456789,
            },
            id: 123456789,
            ty: None,
            known: Known::ComptimeOnly,
        }
    }
    // used for moving out of ast
    pub fn null() -> Self {
        FatExpr::synthetic(Expr::Value(Value::Poison))
    }
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

#[derive(Clone, PartialEq, Debug, Eq)]
pub enum Stmt<'p> {
    Noop,
    Eval(FatExpr<'p>),
    DeclFunc(Func<'p>),

    // Backend Only
    SetVar(Var<'p>, FatExpr<'p>),
    DeclVar {
        name: Var<'p>,
        ty: Option<FatExpr<'p>>,
        value: Option<FatExpr<'p>>,
        dropping: Option<Var<'p>>, // if this is a redeclaration, immediatly call the drop handler on the old one
        kind: VarType,
    },

    // Frontend only
    DeclNamed {
        name: Ident<'p>,
        ty: Option<FatExpr<'p>>,
        value: Option<FatExpr<'p>>,
        kind: VarType,
    },
    SetNamed(Ident<'p>, FatExpr<'p>),
}

#[derive(Clone, PartialEq, Debug, Eq)]
pub struct Func<'p> {
    pub annotations: Vec<Annotation<'p>>,
    pub name: Option<Ident<'p>>,   // it might be an annonomus closure
    pub ty: LazyFnType<'p>,        // We might not have typechecked yet.
    pub body: Option<FatExpr<'p>>, // It might be a forward declaration / ffi.
    pub arg_names: Vec<Option<Ident<'p>>>,
    pub arg_vars: Option<Vec<Var<'p>>>,
    pub capture_vars: Vec<Var<'p>>,
    pub local_constants: Vec<Var<'p>>,
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

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub struct VarInfo {
    pub ty: TypeId,
    pub kind: VarType,
}

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
    pub generics_memo: HashMap<(FuncId, Value), Value>,
    pub vars: Vec<VarInfo>,
}

impl<'p> Stmt<'p> {
    // used for moving out of ast
    pub fn null() -> Stmt<'p> {
        Stmt::Eval(FatExpr::null())
    }
}

impl<'p> LazyFnType<'p> {
    pub fn of(arg_type: Option<FatExpr<'p>>, return_type: Option<FatExpr<'p>>) -> LazyFnType<'p> {
        let arg = match &arg_type {
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
    pub fn unwrap(&self) -> TypeId {
        match self {
            LazyType::Finished(ty) => *ty,
            _ => panic!("Type not ready: {:?}", self),
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
            vars: vec![],
        }
    }
}

impl<'p> Program<'p> {
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
            Value::GetFn(f) => {
                // TODO: its unfortunate that this means you cant ask the type of a value unless you already know
                let (param, returns) = self.funcs[f.0].ty.unwrap();
                self.intern_type(TypeInfo::Fn(FnType { param, returns }))
            }
            Value::Unit => self.intern_type(TypeInfo::Unit),
            Value::Poison => panic!("Tried to typecheck Value::Poison"),
            Value::Slice(_) => todo!(),
            Value::Map(_, _) => todo!(),
            Value::Symbol(_) => todo!(),
            Value::InterpAbsStackAddr(_) => TypeId::any(),
            Value::Heap {
                value,
                first,
                count,
            } => TypeId::any(),
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
