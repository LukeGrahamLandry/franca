//! High level representation of a Franca program. Macros operate on these types.
use crate::{
    bc::{Constants, Structured, Value},
    compiler::insert_multi,
    ffi::InterpSend,
    pool::{Ident, StringPool},
};
use codemap::Span;
use interp_derive::InterpSend;
use std::{
    collections::HashMap,
    hash::Hash,
    mem,
    ops::{Deref, DerefMut},
};

#[derive(Copy, Clone, PartialEq, Hash, Eq, InterpSend)]
pub struct TypeId(pub usize);

#[derive(Copy, Clone, PartialEq, Hash, Eq, Debug, InterpSend)]
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

    // Be careful that this is in the pool correctly!
    pub fn bool() -> TypeId {
        TypeId(4)
    }
}

#[derive(Copy, Clone, PartialEq, Hash, Eq, Debug, InterpSend)]
pub enum VarType {
    Let,
    Var,
    Const,
}

#[derive(Clone, PartialEq, Hash, Eq, Debug, InterpSend)]
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
    Unique(TypeId, usize),
    Named(TypeId, Ident<'p>),
    Type,
    Unit, // TODO: same as empty tuple but easier to type
}

#[derive(Copy, Clone, Hash, Debug, PartialEq, Eq, InterpSend)]
pub struct Field<'p> {
    pub name: Ident<'p>,
    pub ty: TypeId,
    pub first: usize,
    pub count: usize,
}

#[derive(Clone, PartialEq, Hash, Debug, InterpSend)]
pub struct Annotation<'p> {
    pub name: Ident<'p>,
    pub args: Option<FatExpr<'p>>,
}

#[derive(Copy, Clone, PartialEq, Hash, Eq, Debug, InterpSend)]
pub struct Var<'p>(pub Ident<'p>, pub usize);

#[derive(Clone, Debug, InterpSend)]
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

#[derive(Copy, Clone, Debug, Eq, PartialEq, InterpSend)]
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
#[derive(Clone, Debug, InterpSend)]
pub struct FatExpr<'p> {
    pub expr: Expr<'p>,
    pub loc: Span,
    pub id: usize,
    pub ty: Option<TypeId>,
    pub known: Known,
}

#[derive(Clone, Debug, InterpSend)]
pub struct Pattern<'p> {
    pub bindings: Vec<Binding<'p>>,
    pub loc: Span,
}

// arguments of a function and left of variable declaration.
#[derive(Clone, Debug, InterpSend)]
pub enum Binding<'p> {
    Named(Ident<'p>, LazyType<'p>),
    Var(Var<'p>, LazyType<'p>),
    Discard(LazyType<'p>),
}

impl<'p> Binding<'p> {
    pub fn type_for_name(&self, name: Var) -> Option<&LazyType> {
        match self {
            Binding::Named(_, _) => unreachable!(),
            Binding::Var(v, ty) => {
                if *v == name {
                    return Some(ty);
                }
                None
            }
            Binding::Discard(_) => None,
        }
    }

    pub fn unwrap(&self) -> TypeId {
        match self {
            Binding::Named(_, ty) => ty.unwrap(),
            Binding::Var(_, ty) => ty.unwrap(),
            Binding::Discard(ty) => ty.unwrap(),
        }
    }
}

impl<'p> Pattern<'p> {
    pub fn empty(loc: Span) -> Self {
        Self {
            loc,
            bindings: vec![],
        }
    }

    // Useful for function args.
    pub fn then(&mut self, other: Self) {
        self.bindings.extend(other.bindings);
    }

    pub fn flatten(&self) -> Vec<(Option<Var<'p>>, TypeId)> {
        self.bindings
            .iter()
            .map(|b| {
                let name = match b {
                    Binding::Named(_, _) => None,
                    Binding::Var(v, _) => Some(*v),
                    Binding::Discard(_) => None,
                };
                (name, b.unwrap())
            })
            .collect()
    }

    pub fn flatten_names(&self) -> Vec<Ident<'p>> {
        self.bindings
            .iter()
            .map(|b| match b {
                Binding::Named(i, _) => *i,
                Binding::Var(v, _) => v.0,
                Binding::Discard(e) => todo!("struct no name? {e:?}"),
            })
            .collect()
    }

    pub fn flatten_exprs(&self) -> Option<Vec<FatExpr<'p>>> {
        self.bindings
            .iter()
            .map(|b| match b {
                Binding::Named(_, e) | Binding::Var(_, e) | Binding::Discard(e) => e.clone(),
            })
            .map(|t| match t {
                LazyType::EvilUnit => panic!(),
                LazyType::Infer => None,
                LazyType::PendingEval(e) => Some(e),
                LazyType::Finished(_) => None,
                LazyType::Different(_) => unreachable!(),
            })
            .collect()
    }

    pub fn remove_named(&mut self, arg_name: Var<'p>) {
        let start = self.bindings.len();
        self.bindings.retain(|b| match b {
            Binding::Var(name, _) => *name == arg_name,
            _ => true,
        });
        debug_assert_ne!(start, self.bindings.len());
    }
}

#[derive(Debug, Default)]
pub struct Matched<'e, 'p> {
    _discard: Vec<(&'e mut FatExpr<'p>, TypeId)>,
    _bound: Vec<(&'e mut FatExpr<'p>, TypeId, Var<'p>)>,
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

#[derive(Clone, Debug, InterpSend)]
pub enum Stmt<'p> {
    Noop,
    Eval(FatExpr<'p>),
    DeclFunc(Func<'p>),

    // Backend Only
    DeclVar {
        name: Var<'p>,
        ty: LazyType<'p>,
        value: Option<FatExpr<'p>>,
        dropping: Option<Var<'p>>, // if this is a redeclaration, immediatly call the drop handler on the old one
        kind: VarType,
    },

    // Frontend only
    DeclNamed {
        name: Ident<'p>,
        ty: LazyType<'p>,
        value: Option<FatExpr<'p>>,
        kind: VarType,
    },
    Set {
        place: FatExpr<'p>,
        value: FatExpr<'p>,
    },
    DoneDeclFunc(FuncId),
}

#[derive(Clone, Debug, InterpSend)]
pub struct FatStmt<'p> {
    pub stmt: Stmt<'p>,
    pub annotations: Vec<Annotation<'p>>,
    pub loc: Span,
}

#[derive(Clone, Debug, InterpSend)]
pub struct Func<'p> {
    pub annotations: Vec<Annotation<'p>>,
    pub name: Ident<'p>,           // it might be an annonomus closure
    pub var_name: Option<Var<'p>>, // TODO: having both ^ is redundant
    pub body: Option<FatExpr<'p>>, // It might be a forward declaration / ffi.
    pub arg_loc: Vec<Option<Span>>,
    pub arg: Pattern<'p>,
    pub ret: LazyType<'p>,
    pub capture_vars: Vec<Var<'p>>,
    pub local_constants: Vec<FatStmt<'p>>,
    pub loc: Span,
    pub capture_vars_const: Vec<Var<'p>>,
    pub closed_constants: Constants<'p>,
    pub finished_type: Option<FnType>,
    pub referencable_name: bool, // Diferentiate closures, etc which can't be refered to by name in the program text but I assign a name for debugging.
    pub evil_uninit: bool,
}

impl<'p> Func<'p> {
    pub fn new(
        name: Ident<'p>,
        arg: Pattern<'p>,
        ret: LazyType<'p>,
        body: Option<FatExpr<'p>>,
        loc: Span,
        has_name: bool,
    ) -> Self {
        Func {
            annotations: vec![],
            name,
            arg,
            ret,
            body,
            arg_loc: vec![],
            capture_vars: vec![],
            local_constants: vec![],
            loc,
            closed_constants: Constants::empty(),
            capture_vars_const: vec![],
            var_name: None,
            finished_type: None,
            referencable_name: has_name,
            evil_uninit: false,
        }
    }

    // TODO: remove
    pub fn synth_name(&self, pool: &StringPool<'p>) -> &'p str {
        pool.get(self.get_name(pool))
    }

    // TODO: remove
    pub fn get_name(&self, _: &StringPool<'p>) -> Ident<'p> {
        self.name
    }

    /// Find annotation ignoring arguments
    pub fn has_tag(&self, pool: &StringPool<'p>, name: &str) -> bool {
        let name = pool.intern(name);
        self.annotations.iter().any(|a| a.name == name)
    }

    pub fn unwrap_ty(&self) -> FnType {
        self.finished_type.unwrap()
    }

    pub fn known_args(arg: TypeId, ret: TypeId, loc: Span) -> (Pattern<'p>, LazyType<'p>) {
        let arg = Pattern {
            bindings: vec![Binding::Discard(LazyType::Finished(arg))],
            loc,
        };
        let ret = LazyType::Finished(ret);
        (arg, ret)
    }
}

#[derive(Clone, PartialEq, Debug, Hash, Default, InterpSend)]
pub enum LazyType<'p> {
    #[default]
    EvilUnit,
    Infer,
    PendingEval(FatExpr<'p>),
    Finished(TypeId),
    Different(Vec<Self>),
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, InterpSend)]
pub struct FuncId(pub usize);

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, InterpSend)]
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
    pub overload_sets: Vec<OverloadSet<'p>>,
    pub ffi_types: HashMap<u128, TypeId>,
}

#[derive(Clone)]
pub struct OverloadSet<'p>(pub Vec<OverloadOption<'p>>);

#[derive(Clone)]
pub struct OverloadOption<'p> {
    pub name: Ident<'p>,
    pub ty: FnType,
    pub func: FuncId,
}

impl<'p> Stmt<'p> {
    // used for moving out of ast
    pub fn null(loc: Span) -> Stmt<'p> {
        Stmt::Eval(FatExpr::null(loc))
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
            overload_sets: Default::default(),
            ffi_types: Default::default(),
        }
    }

    /// This allows ffi types to be unique.
    pub fn get_ffi_type<T: InterpSend<'p>>(&mut self, id: u128) -> TypeId {
        self.ffi_types.get(&id).copied().unwrap_or_else(|| {
            let ty = T::get_type(self);
            self.ffi_types.insert(id, ty);
            ty
        })
    }

    pub fn raw_type(&self, mut ty: TypeId) -> TypeId {
        while let &TypeInfo::Unique(inner, _) | &TypeInfo::Named(inner, _) = &self.types[ty.0] {
            ty = inner
        }
        ty
    }

    pub fn get_enum(&self, enum_ty: TypeId) -> Option<&[(Ident<'p>, TypeId)]> {
        let enum_ty = self.raw_type(enum_ty);
        if let TypeInfo::Enum { cases, .. } = &self.types[enum_ty.0] {
            Some(cases)
        } else {
            None
        }
    }

    pub fn tuple_of(&mut self, types: Vec<TypeId>) -> TypeId {
        match types.len() {
            0 => TypeId::unit(),
            1 => types[0],
            _ => self.intern_type(TypeInfo::Tuple(types)),
        }
    }

    pub fn unique_ty(&mut self, ty: TypeId) -> TypeId {
        self.intern_type(TypeInfo::Unique(ty, self.types.len()))
    }

    pub fn load_fn(&mut self, id: FuncId) -> Structured {
        Structured::Const(self.func_type(id), Value::GetFn(id))
    }

    pub fn load_value(&mut self, v: Value) -> Structured {
        Structured::Const(self.type_of(&v), v)
    }
}

impl<'p> Program<'p> {
    // TODO: Unsized types. Any should be a TypeId and then some memory with AnyPtr being the fat ptr version.
    //       With raw Any version, you couldn't always change types without reallocating the space and couldn't pass it by value.
    //       AnyScalar=(TypeId, one value), AnyPtr=(TypeId, one value=stack/heap ptr), AnyUnsized=(TypeId, some number of stack slots...)
    pub fn slot_count(&self, ty: TypeId) -> usize {
        let ty = self.raw_type(ty);
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

    // BRO DO NOT FUCKING CALL THIS ONE UNLESS YOU'RE SURE YOU REMEMBER TO CLOSE CONSTANTS
    #[track_caller]
    pub fn add_func<'a>(&'a mut self, func: Func<'p>) -> FuncId {
        let id = FuncId(self.funcs.len());
        let name = func.name;
        let named = func.referencable_name;
        self.funcs.push(func);
        if named {
            insert_multi(&mut self.declarations, name, id);
        }
        id
    }

    pub fn returns_type(&self, f: FuncId) -> bool {
        let func = &self.funcs[f.0];
        let ret = func.ret.unwrap();
        let ty = &self.types[ret.0];
        ty == &TypeInfo::Type
    }

    // TODO: The world might be a better place if the root types were in TypeId,
    //       So you didn't need to do the interning dance for i64/f64/unit/any
    //       The important thing is that they're copy and quick to compare no matter how much nesting.
    //       Tho number types might become something with flags so maybe just Type, Unit, Any should be blessed.
    pub fn type_of(&mut self, v: &Value) -> TypeId {
        match v {
            Value::F64(_) => self.intern_type(TypeInfo::F64),
            Value::I64(_) => TypeId::i64(),
            Value::Bool(_) => TypeId::bool(),
            Value::Enum { container_type, .. } => *container_type,
            Value::Tuple { values, .. } => {
                // TODO: actually use container_type
                let types = values.iter().map(|v| self.type_of(v)).collect();
                self.tuple_of(types)
            }
            Value::Type(_) => self.intern_type(TypeInfo::Type),
            // TODO: its unfortunate that this means you cant ask the type of a value unless you already know
            Value::GetFn(f) => self.func_type(*f),
            Value::Unit => TypeId::unit(),
            Value::Poison => panic!("Tried to typecheck Value::Poison"),
            Value::Symbol(_) => Ident::get_type(self),
            Value::InterpAbsStackAddr(_) => TypeId::any(),
            Value::Heap { .. } => TypeId::any(),
            Value::OverloadSet(_) => TypeId::any(),
            Value::CFnPtr { ty, .. } => self.intern_type(TypeInfo::Fn(*ty)),
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
        let ty = self.funcs[id.0].unwrap_ty();
        self.intern_type(TypeInfo::Fn(ty))
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
            &TypeInfo::Unique(ty, _) => self.tuple_types(ty),
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

    pub fn struct_type(&mut self, name: &str, fields_in: &[(&str, TypeId)]) -> TypeId {
        let name = self.pool.intern(name);
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
        let as_tuple = self.tuple_of(types);
        let ty = TypeInfo::Struct {
            fields,
            size,
            as_tuple,
        };
        let ty = self.intern_type(ty);
        self.intern_type(TypeInfo::Named(ty, name))
    }

    pub fn enum_type(&mut self, _name: &str, varients: &[TypeId]) -> TypeId {
        let as_tuple = self.tuple_of(varients.to_vec());
        self.to_enum(self.types[as_tuple.0].clone())
    }

    pub fn synth_name(&mut self, ty: TypeId) -> Ident<'p> {
        match self.types[ty.0] {
            TypeInfo::Named(_, name) => name,
            _ => self.pool.intern(&format!("__anon_ty{}", ty.0)),
        }
    }

    pub fn to_enum(&mut self, ty: TypeInfo<'p>) -> TypeId {
        if let TypeInfo::Struct { fields, .. } = ty {
            let size = fields.iter().map(|f| self.slot_count(f.ty)).max().unwrap();
            let ty = TypeInfo::Enum {
                cases: fields.into_iter().map(|f| (f.name, f.ty)).collect(),
                size: size + 1, // for tag.
            };
            self.intern_type(ty)
        } else if let TypeInfo::Tuple(fields) = ty {
            let size = fields.iter().map(|ty| self.slot_count(*ty)).max().unwrap();
            let ty = TypeInfo::Enum {
                cases: fields
                    .into_iter()
                    .map(|ty| (self.synth_name(ty), ty))
                    .collect(),
                size: size + 1, // for tag.
            };
            self.intern_type(ty)
        } else {
            panic!()
        }
    }

    pub fn named_tuple(&mut self, _todo_name: &str, types: Vec<TypeId>) -> TypeId {
        self.tuple_of(types)
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

    pub(crate) fn _get_tag_arg(&self, pool: &StringPool<'p>, name: &str) -> Option<&FatExpr<'p>> {
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

impl<'p> Default for Func<'p> {
    fn default() -> Self {
        Self {
            annotations: vec![],
            name: Ident::null(),
            var_name: None,
            body: None,
            arg_loc: vec![],
            arg: Pattern {
                bindings: vec![],
                loc: garbage_loc(),
            },
            ret: LazyType::Infer,
            capture_vars: vec![],
            local_constants: vec![],
            loc: garbage_loc(),
            capture_vars_const: vec![],
            closed_constants: Default::default(),
            finished_type: None,
            referencable_name: false,
            evil_uninit: true,
        }
    }
}

fn garbage_loc() -> Span {
    // Surely any (u32, u32) is valid
    unsafe { mem::zeroed() }
}
