//! High level representation of a Franca program. Macros operate on these types.
use crate::{
    bc::{ConstId, SharedConstants, Value},
    compiler::insert_multi,
    pool::{Ident, StringPool},
};
use codemap::Span;
use std::{
    collections::HashMap,
    hash::Hash,
    ops::{Deref, DerefMut},
};

#[derive(Copy, Clone, PartialEq, Hash, Eq)]
pub struct TypeId(pub usize);

#[derive(Copy, Clone, PartialEq, Hash, Eq, Debug)]
pub struct FnType {
    // Functions with multiple arguments are treated as a tuple.
    pub arg: TypeId,
    pub ret: TypeId,
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

    // Be careful that this is in the pool correctly!
    pub fn ty() -> TypeId {
        TypeId(2)
    }

    // Be careful that this is in the pool correctly!
    pub fn i64() -> TypeId {
        TypeId(3)
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
pub enum TypeInfo<'p> {
    Any,
    Never,
    F64,
    I64,
    Bool,
    Fn(FnType),
    Tuple(Vec<TypeId>),
    Ptr(TypeId),   // One element
    Slice(TypeId), // A pointer and a length
    Struct {
        // You probably always have few enough that this is faster than a hash map.
        fields: Vec<Field<'p>>,
        size: usize,
        as_tuple: TypeId,
    },
    Enum {
        cases: Vec<(Ident<'p>, TypeId)>,
        size: usize,
    },
    // Let you ask for type checking on things that have same repr but don't make the backend deal with it.
    Unique(Ident<'p>, TypeId),
    Type,
    Unit, // TODO: same as empty tuple but easier to type
}

#[derive(Copy, Clone, Hash, Debug, PartialEq, Eq)]
pub struct Field<'p> {
    pub name: Ident<'p>,
    pub ty: TypeId,
    pub first: usize,
    pub count: usize,
}

#[derive(Clone, PartialEq, Hash, Debug)]
pub struct Annotation<'p> {
    pub name: Ident<'p>,
    pub args: Option<FatExpr<'p>>,
}

#[derive(Copy, Clone, PartialEq, Hash, Eq, Debug)]
pub struct Var<'p>(pub Ident<'p>, pub usize);

#[derive(Clone, Debug)]
pub enum Expr<'p> {
    Value(Value),
    Call(Box<FatExpr<'p>>, Box<FatExpr<'p>>),
    Block {
        body: Vec<FatStmt<'p>>,
        result: Box<FatExpr<'p>>,
        locals: Option<Vec<Var<'p>>>, // useful information for calling drop
    },
    ArrayLiteral(Vec<FatExpr<'p>>),
    Tuple(Vec<FatExpr<'p>>),
    RefType(Box<FatExpr<'p>>),
    EnumLiteral(Vec<(Ident<'p>, FatExpr<'p>)>),
    Closure(Box<Func<'p>>),
    SuffixMacro(Ident<'p>, Box<FatExpr<'p>>),
    FieldAccess(Box<FatExpr<'p>>, Ident<'p>),
    StructLiteralP(Pattern<'p>),
    GenericArgs(Box<FatExpr<'p>>, Box<FatExpr<'p>>),

    // Backend only
    GetVar(Var<'p>),

    // Frontend only
    GetNamed(Ident<'p>),
    String(Ident<'p>),
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
#[derive(Clone, Debug)]
pub struct FatExpr<'p> {
    pub expr: Expr<'p>,
    pub loc: Span,
    pub id: usize,
    pub ty: Option<TypeId>,
    pub known: Known,
}

// argument of a function and left of variable declaration.
#[derive(Clone, Debug)]
pub struct Pattern<'p> {
    pub names: Vec<Option<Ident<'p>>>,
    pub types: Vec<Option<FatExpr<'p>>>,
    pub loc: Span,
}

impl<'p> Pattern<'p> {
    pub fn empty(loc: Span) -> Self {
        Self {
            names: vec![],
            types: vec![],
            loc,
        }
    }

    // Useful for function args.
    pub fn then(&mut self, other: Self) {
        self.names.extend(other.names);
        self.types.extend(other.types);
    }

    pub fn make_ty(self) -> LazyType<'p> {
        match self.types.len() {
            // Note: this is the *type* `Unit`, NOT the *value* `unit`
            0 => LazyType::Finished(TypeId::unit()),
            1 => {
                let ty = self.types.into_iter().next().unwrap();
                match ty {
                    Some(e) => LazyType::PendingEval(e),
                    None => LazyType::Infer,
                }
            }
            _ => {
                let e = FatExpr {
                    expr: Expr::Tuple(self.types.into_iter().map(|ty| ty.unwrap()).collect()),
                    loc: self.loc,
                    id: 3456789,
                    ty: None,
                    known: Known::Foldable,
                };
                LazyType::PendingEval(e)
            }
        }
    }
}

// Some(
//     self.expr(
//         Expr::Tuple(
//             arg_types
//                 .into_iter()
//                 .map(|ty| ty.unwrap_or_else(|| any_type_expr.clone()))
//                 .collect(),
//         ),
//         node.start_position(),
//         Known::Foldable,
//     ),
// ),

impl<'p> FatExpr<'p> {
    pub fn synthetic(expr: Expr<'p>, loc: Span) -> Self {
        FatExpr {
            expr,
            loc,
            id: 123456789,
            ty: None,
            known: Known::ComptimeOnly,
        }
    }
    // used for moving out of ast
    pub fn null(loc: Span) -> Self {
        FatExpr::synthetic(Expr::Value(Value::Poison), loc)
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

#[derive(Clone, Debug)]
pub enum Stmt<'p> {
    Noop,
    Eval(FatExpr<'p>),
    DeclFunc(Func<'p>),

    // Backend Only
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
    Set {
        place: FatExpr<'p>,
        value: FatExpr<'p>,
    },
}

#[derive(Clone, Debug)]
pub struct FatStmt<'p> {
    pub stmt: Stmt<'p>,
    pub annotations: Vec<Annotation<'p>>,
    pub loc: Span,
}

#[derive(Clone, Debug)]
pub struct Func<'p> {
    pub annotations: Vec<Annotation<'p>>,
    pub name: Option<Ident<'p>>,   // it might be an annonomus closure
    pub ty: LazyFnType<'p>,        // We might not have typechecked yet.
    pub body: Option<FatExpr<'p>>, // It might be a forward declaration / ffi.
    pub arg_names: Vec<Option<Ident<'p>>>,
    pub arg_loc: Vec<Option<Span>>,
    pub arg_vars: Option<Vec<Var<'p>>>,
    pub capture_vars: Vec<Var<'p>>,
    pub local_constants: Vec<FatStmt<'p>>,
    pub loc: Span,
}

impl<'p> Func<'p> {
    pub fn synth_name(&self, pool: &StringPool<'p>) -> &'p str {
        pool.get(self.get_name(pool))
    }

    pub fn get_name(&self, pool: &StringPool<'p>) -> Ident<'p> {
        self.name.unwrap_or_else(|| pool.intern("@anon@"))
    }

    /// Find annotation ignoring arguments
    pub fn has_tag(&self, pool: &StringPool<'p>, name: &str) -> bool {
        let name = pool.intern(name);
        self.annotations.iter().any(|a| a.name == name)
    }
}

#[derive(Clone, PartialEq, Debug, Hash)]
pub enum LazyFnType<'p> {
    Finished(TypeId, TypeId),
    Pending {
        arg: LazyType<'p>,
        ret: LazyType<'p>,
    },
}

#[derive(Clone, PartialEq, Debug, Hash)]
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
    pub loc: Span,
}

#[derive(Clone)]
pub struct Program<'p> {
    pub pool: &'p StringPool<'p>,
    pub types: Vec<TypeInfo<'p>>,
    // At the call site, you know the name but not the type.
    // So you need to look at everybody that might be declaring the function you're trying to call.
    pub declarations: HashMap<Ident<'p>, Vec<FuncId>>,
    pub funcs: Vec<Func<'p>>,
    /// Comptime function calls that return a type are memoized so identity works out.
    pub generics_memo: HashMap<(FuncId, Value), Value>,
    // If you're looking for a function/type name that doesn't exist, these are places you can try instantiating them.
    pub impls: HashMap<Ident<'p>, Vec<FuncId>>,
    pub vars: Vec<VarInfo>,
    pub constants: Vec<SharedConstants<'p>>,
}

impl<'p> Stmt<'p> {
    // used for moving out of ast
    pub fn null(loc: Span) -> Stmt<'p> {
        Stmt::Eval(FatExpr::null(loc))
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

impl<'p> Program<'p> {
    pub fn new(vars: Vec<VarInfo>, pool: &'p StringPool<'p>) -> Self {
        Self {
            // Any needs to be first becuase I use TypeId(0) as a place holder.
            // The rest are just common ones that i want to find faster if i end up iterating the array.
            types: vec![
                TypeInfo::Any,
                TypeInfo::Unit,
                TypeInfo::Type,
                TypeInfo::I64,
                TypeInfo::Bool,
                TypeInfo::F64,
            ],
            declarations: Default::default(),
            funcs: Default::default(),
            generics_memo: Default::default(),
            impls: Default::default(),
            vars,
            pool,
            constants: vec![],
        }
    }

    pub fn get_enum(&self, enum_ty: TypeId) -> Option<&[(Ident<'p>, TypeId)]> {
        if let TypeInfo::Enum { cases, .. } = &self.types[enum_ty.0] {
            Some(cases)
        } else {
            None
        }
    }

    pub fn empty_consts(&mut self) -> ConstId {
        let i = self.constants.len();
        self.constants.push(SharedConstants {
            id: ConstId(i),
            parents: Default::default(),
            local: Default::default(),
            overloads: Default::default(),
            references: 1,
        });
        ConstId(i)
    }

    pub fn consts_child(&mut self, c: ConstId) -> ConstId {
        self.constants[c.0].references += 1;
        let new = self.empty_consts();
        self.constants[new.0].parents.push(c);
        new
    }
}

impl<'p> Program<'p> {
    // TODO: Unsized types. Any should be a TypeId and then some memory with AnyPtr being the fat ptr version.
    //       With raw Any version, you couldn't always change types without reallocating the space and couldn't pass it by value.
    //       AnyScalar=(TypeId, one value), AnyPtr=(TypeId, one value=stack/heap ptr), AnyUnsized=(TypeId, some number of stack slots...)
    pub fn slot_count(&self, ty: TypeId) -> usize {
        match &self.types[ty.0] {
            TypeInfo::Tuple(args) => args.iter().map(|t| self.slot_count(*t)).sum(),
            &TypeInfo::Struct { size, .. } => size,
            &TypeInfo::Enum { size, .. } => size,
            _ => 1,
        }
    }

    // TODO: this is O(n), at the very least make sure the common types are at the beginning.
    pub fn intern_type(&mut self, ty: TypeInfo<'p>) -> TypeId {
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
        if let Some(name) = name {
            insert_multi(&mut self.declarations, name, id);
        }
        id
    }

    pub fn returns_type(&self, f: FuncId) -> bool {
        let func = &self.funcs[f.0];
        let (_, ret) = func.ty.unwrap();
        let ty = &self.types[ret.0];
        ty == &TypeInfo::Type
    }

    // TODO: The world might be a better place if the root types were in TypeId,
    //       So you didn't need to do the interning dance for i64/f64/unit/any
    //       The important thing is that they're copy and quick to compare no matter how much nesting.
    //       Tho number types might become something with flags so maybe just Type, Unit, Any should be blessed.
    pub fn type_of(&mut self, v: &Value) -> TypeId {
        match v {
            Value::F64(_) => todo!(),
            Value::I64(_) => self.intern_type(TypeInfo::I64),
            Value::Bool(_) => self.intern_type(TypeInfo::Bool),
            Value::Enum { container_type, .. } => *container_type,
            Value::Tuple { values, .. } => {
                // TODO: actually use container_type
                let types = values.iter().map(|v| self.type_of(v)).collect();
                self.intern_type(TypeInfo::Tuple(types))
            }
            Value::Type(_) => self.intern_type(TypeInfo::Type),
            // TODO: its unfortunate that this means you cant ask the type of a value unless you already know
            Value::GetFn(f) => self.func_type(*f),
            Value::Unit => self.intern_type(TypeInfo::Unit),
            Value::Poison => panic!("Tried to typecheck Value::Poison"),
            Value::Symbol(_) => TypeId::i64(),
            Value::InterpAbsStackAddr(_) => TypeId::any(),
            Value::Heap { .. } => TypeId::any(),
        }
    }

    pub fn is_type(&self, ty: TypeId, expect: TypeInfo) -> bool {
        self.types[ty.0] == expect
    }

    pub fn fn_ty(&mut self, id: TypeId) -> Option<FnType> {
        if let TypeInfo::Fn(ty) = self.types[id.0] {
            Some(ty)
        } else {
            None
        }
    }

    pub fn func_type(&mut self, id: FuncId) -> TypeId {
        let (arg, ret) = self.funcs[id.0].ty.unwrap();
        self.intern_type(TypeInfo::Fn(FnType { arg, ret }))
    }

    pub fn ptr_type(&mut self, value_ty: TypeId) -> TypeId {
        self.intern_type(TypeInfo::Ptr(value_ty))
    }

    pub fn slice_type(&mut self, value_ty: TypeId) -> TypeId {
        self.intern_type(TypeInfo::Slice(value_ty))
    }

    pub fn unptr_ty(&self, ptr_ty: TypeId) -> Option<TypeId> {
        let ptr_ty = &self.types[ptr_ty.0];
        if let TypeInfo::Slice(ty) | TypeInfo::Ptr(ty) = ptr_ty {
            Some(*ty)
        } else {
            None
        }
    }

    pub fn tuple_types(&self, ty: TypeId) -> Option<&[TypeId]> {
        match &self.types[ty.0] {
            TypeInfo::Tuple(types) => Some(types),
            &TypeInfo::Struct { as_tuple, .. } => self.tuple_types(as_tuple),
            TypeInfo::Unique(_, _) => todo!(),
            _ => None,
        }
    }

    pub fn ptr_depth(&self, mut ptr_ty: TypeId) -> usize {
        let mut d = 0;
        while let &TypeInfo::Ptr(inner) = &self.types[ptr_ty.0] {
            d += 1;
            ptr_ty = inner;
        }
        d
    }

    pub fn struct_type(&mut self, _todo_name: &str, fields_in: &[(&str, TypeId)]) -> TypeId {
        let mut types = vec![];
        let mut fields = vec![];
        let mut size = 0;
        for (name, ty) in fields_in {
            let count = self.slot_count(*ty);
            fields.push(Field {
                name: self.pool.intern(name),
                ty: *ty,
                first: size,
                count,
            });
            types.push(*ty);
            size += count;
        }
        let as_tuple = self.intern_type(TypeInfo::Tuple(types));
        let ty = TypeInfo::Struct {
            fields,
            size,
            as_tuple,
        };
        self.intern_type(ty)
    }

    pub fn to_enum(&mut self, ty: TypeInfo<'p>) -> TypeId {
        if let TypeInfo::Struct { fields, .. } = ty {
            let size = fields.iter().map(|f| self.slot_count(f.ty)).max().unwrap();
            let ty = TypeInfo::Enum {
                cases: fields.into_iter().map(|f| (f.name, f.ty)).collect(),
                size: size + 1, // for tag.
            };
            self.intern_type(ty)
        } else {
            panic!()
        }
    }

    pub fn named_tuple(&mut self, _todo_name: &str, types: Vec<TypeId>) -> TypeId {
        self.intern_type(TypeInfo::Tuple(types))
    }

    pub fn const_get(&self, scope: ConstId, var: &Var<'p>) -> Option<(Value, TypeId)> {
        let c = &self.constants[scope.0];
        c.local.get(var).cloned().or_else(|| {
            for p in &c.parents {
                if let Some(v) = self.const_get(*p, var) {
                    return Some(v);
                }
            }
            None
        })
    }

    pub fn const_get_overload(
        &self,
        scope: ConstId,
        key: &(Ident<'p>, Value),
    ) -> Option<(Value, TypeId)> {
        let c = &self.constants[scope.0];
        c.overloads.get(key).cloned().or_else(|| {
            for p in &c.parents {
                if let Some(v) = self.const_get_overload(*p, key) {
                    return Some(v);
                }
            }
            None
        })
    }

    pub fn const_get_named(&self, scope: ConstId, name: Ident<'_>) -> Option<(Value, TypeId)> {
        let c = &self.constants[scope.0];
        if let Some((_, t)) = c.local.iter().find(|(k, _)| k.0 == name) {
            return Some(t.clone());
        }
        for p in &c.parents {
            if let Some(v) = self.const_get_named(*p, name) {
                return Some(v);
            }
        }
        None
    }

    pub fn const_insert(
        &mut self,
        scope: ConstId,
        k: Var<'p>,
        v: (Value, TypeId),
    ) -> Option<(Value, TypeId)> {
        let c = &mut self.constants[scope.0];
        // debug_assert!(c.references == 1);
        c.local.insert(k, v)
    }

    pub fn const_insert_overload(
        &mut self,
        scope: ConstId,
        k: (Ident<'p>, Value),
        v: (Value, TypeId),
    ) -> Option<(Value, TypeId)> {
        let c = &mut self.constants[scope.0];
        // debug_assert!(c.references == 1);
        c.overloads.insert(k, v)
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

impl<'p> Deref for FatStmt<'p> {
    type Target = Stmt<'p>;

    fn deref(&self) -> &Self::Target {
        &self.stmt
    }
}

impl<'p> DerefMut for FatStmt<'p> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.stmt
    }
}

impl<'p> Stmt<'p> {
    pub fn fat_empty(self, loc: Span) -> FatStmt<'p> {
        FatStmt {
            stmt: self,
            annotations: vec![],
            loc,
        }
    }

    pub fn fat_with(self, annotations: Vec<Annotation<'p>>, loc: Span) -> FatStmt<'p> {
        FatStmt {
            stmt: self,
            annotations,
            loc,
        }
    }
}

impl<'p> FatStmt<'p> {
    pub fn null(loc: Span) -> Self {
        FatStmt {
            stmt: Stmt::null(loc),
            annotations: vec![],
            loc,
        }
    }

    pub(crate) fn _has_tag(&self, pool: &StringPool<'_>, name: &str) -> bool {
        let name = pool.intern(name);
        self.annotations.iter().any(|a| a.name == name)
    }

    pub(crate) fn get_tag_arg(&self, pool: &StringPool<'p>, name: &str) -> Option<&FatExpr<'p>> {
        let name = pool.intern(name);
        self.annotations
            .iter()
            .find(|a| a.name == name)
            .and_then(|a| a.args.as_ref())
    }
}

impl<'p> Expr<'p> {
    pub fn as_ident(&self) -> Option<Ident<'p>> {
        match self {
            Expr::GetVar(v) => Some(v.0),
            &Expr::GetNamed(i) => Some(i),
            _ => None,
        }
    }
}
