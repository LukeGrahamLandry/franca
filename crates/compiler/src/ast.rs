//! High level representation of a Franca program. Macros operate on these types.
use crate::{
    bc::{Bc, Constants, Structured, Value, Values},
    compiler::{CErr, CompileError, FnWip, Res},
    err,
    experiments::reflect::{Reflect, RsType},
    ffi::{init_interp_send, InterpSend},
    impl_index, impl_index_imm,
    pool::{Ident, StringPool},
};
use codemap::{CodeMap, Span};
use interp_derive::{InterpSend, Reflect};
use std::{
    cell::RefCell,
    collections::HashMap,
    hash::Hash,
    mem::{self, transmute},
    ops::{Deref, DerefMut},
};

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Hash, Eq, InterpSend, Default)]
pub struct TypeId(pub usize);

#[repr(C)]
#[derive(Copy, Clone, PartialEq, Hash, Eq, Debug, InterpSend)]
pub struct FnType {
    // Functions with multiple arguments are treated as a tuple.
    pub arg: TypeId,
    pub ret: TypeId,
}

#[derive(Copy, Clone, PartialEq, Hash, Eq, Debug, InterpSend)]
pub enum VarType {
    Let,
    Var,
    Const,
}

#[repr(C)]
#[derive(Clone, PartialEq, Hash, Eq, Debug, InterpSend, Default)]
pub enum TypeInfo<'p> {
    #[default]
    Unknown,
    Any,
    Never,
    F64,
    Int(IntType),
    Bool,
    Fn(FnType),
    FnPtr(FnType),
    Tuple(Vec<TypeId>),
    Ptr(TypeId), // One element
    Struct {
        // You probably always have few enough that this is faster than a hash map.
        fields: Vec<Field<'p>>,
        as_tuple: TypeId,
        ffi_byte_align: Option<usize>,
        ffi_byte_stride: Option<usize>,
    },
    Enum {
        cases: Vec<(Ident<'p>, TypeId)>,
    },
    // Let you ask for type checking on things that have same repr but don't make the backend deal with it.
    Unique(TypeId, usize),
    Named(TypeId, Ident<'p>),
    Type,
    Unit, // TODO: same as empty tuple but easier to type
    VoidPtr,
}

#[derive(Copy, Clone, Hash, Debug, PartialEq, Eq, InterpSend)]
pub struct Field<'p> {
    pub name: Ident<'p>,
    pub ty: TypeId,
    pub ffi_byte_offset: Option<usize>,
}

#[derive(Clone, PartialEq, Hash, Debug, InterpSend)]
pub struct Annotation<'p> {
    pub name: Ident<'p>,
    pub args: Option<FatExpr<'p>>,
}

#[derive(Copy, Clone, PartialEq, Hash, Eq, Debug, InterpSend)]
pub struct Var<'p>(pub Ident<'p>, pub usize);

// TODO: should really get an arena going because boxes make me sad.
#[derive(Clone, Debug, InterpSend)]
pub enum Expr<'p> {
    Value {
        ty: TypeId,
        value: Values,
    },
    WipFunc(FuncId),
    Call(Box<FatExpr<'p>>, Box<FatExpr<'p>>),
    Block {
        body: Vec<FatStmt<'p>>,
        result: Box<FatExpr<'p>>,
        locals: Option<Vec<Var<'p>>>, // useful information for calling drop
    },
    Tuple(Vec<FatExpr<'p>>),
    Closure(Box<Func<'p>>),
    SuffixMacro(Ident<'p>, Box<FatExpr<'p>>),
    FieldAccess(Box<FatExpr<'p>>, Ident<'p>),
    StructLiteralP(Pattern<'p>),
    PrefixMacro {
        name: Var<'p>,
        arg: Box<FatExpr<'p>>,
        target: Box<FatExpr<'p>>,
    },
    GetVar(Var<'p>),
    GetNamed(Ident<'p>),
    String(Ident<'p>),
    Index {
        ptr: Box<FatExpr<'p>>,
        index: Box<FatExpr<'p>>,
    },
}

pub trait WalkAst<'p> {
    fn walk_expr(&mut self, _: &mut FatExpr<'p>) {}
    fn post_walk_expr(&mut self, _: &mut FatExpr<'p>) {}
    fn walk_stmt(&mut self, _: &mut FatStmt<'p>) {}
    fn walk_func(&mut self, _: &mut Func<'p>) {}
    fn walk_pattern(&mut self, _: &mut Pattern<'p>) {}
    fn walk_ty(&mut self, _: &mut LazyType<'p>) {}

    fn expr(&mut self, expr: &mut FatExpr<'p>) {
        self.walk_expr(expr);
        match &mut expr.expr {
            Expr::Call(fst, snd) | Expr::Index { ptr: fst, index: snd } => {
                self.expr(fst);
                self.expr(snd);
            }
            Expr::Block { body, result, .. } => {
                for stmt in body {
                    self.walk_stmt(stmt);
                    match &mut stmt.stmt {
                        Stmt::Noop | Stmt::DeclNamed { .. } | Stmt::DoneDeclFunc(_) => {}
                        Stmt::Eval(arg) => self.expr(arg),
                        Stmt::DeclFunc(func) => self.walk_func(func), // TODO: more maybe?
                        Stmt::DeclVar { ty, value, .. } => {
                            if let Some(value) = value {
                                self.expr(value);
                            }
                            self.ty(ty);
                        }
                        Stmt::DeclVarPattern { binding, value } => {
                            self.pattern(binding);
                            if let Some(arg) = value {
                                self.expr(arg);
                            }
                        }
                        Stmt::Set { place, value } => {
                            self.expr(place);
                            self.expr(value);
                        }
                    }
                }
                self.expr(result);
            }
            Expr::Tuple(parts) => {
                for p in parts {
                    self.expr(p);
                }
            }
            Expr::SuffixMacro(_, arg) => self.expr(arg),
            Expr::FieldAccess(arg, _) => self.expr(arg),
            Expr::StructLiteralP(binding) => {
                self.pattern(binding);
            }
            // TODO: idk if i want to be going into macros. maybe inlining should always happen after them.
            Expr::PrefixMacro { arg, target, .. } => {
                self.expr(arg);
                self.expr(target);
            }
            Expr::GetVar(_) => {}
            Expr::Closure(func) => {
                // TODO: why am i treating this differently from the other thing
                self.pattern(&mut func.arg);
                self.ty(&mut func.ret);
                if let Some(body) = &mut func.body {
                    self.expr(body);
                }
            }
            Expr::WipFunc(_) => todo!("walkwip"),
            Expr::Value { .. } | Expr::GetNamed(_) | Expr::String(_) => {}
        }
        self.post_walk_expr(expr);
    }

    fn pattern(&mut self, binding: &mut Pattern<'p>) {
        self.walk_pattern(binding);
        for b in &mut binding.bindings {
            self.ty(&mut b.ty);
            if let Some(arg) = &mut b.default {
                self.expr(arg);
            }
        }
    }

    fn ty(&mut self, ty: &mut LazyType<'p>) {
        self.walk_ty(ty);
        match ty {
            LazyType::EvilUnit | LazyType::Infer | LazyType::Finished(_) => {}
            LazyType::PendingEval(arg) => {
                self.expr(arg);
            }
            LazyType::Different(_) => {} // TODO
        }
    }
}

// Used for inlining closures.
struct RenumberVars<'a, 'p> {
    vars: &'a mut Vec<VarInfo>,
    mapping: &'a mut HashMap<Var<'p>, Var<'p>>,
}

impl<'a, 'p> WalkAst<'p> for RenumberVars<'a, 'p> {
    fn walk_expr(&mut self, expr: &mut FatExpr<'p>) {
        match &mut expr.expr {
            Expr::GetVar(v) => {
                if let Some(new) = self.mapping.get(v) {
                    *v = *new;
                }
            }
            Expr::PrefixMacro { name, .. } => {
                if let Some(new) = self.mapping.get(name) {
                    *name = *new; // probably never happens?
                }
            }
            _ => {}
        }
    }

    fn post_walk_expr(&mut self, expr: &mut FatExpr<'p>) {
        if let Expr::Block { locals: Some(locals), .. } = &mut expr.expr {
            for var in locals {
                if let Some(new) = self.mapping.get(var) {
                    *var = *new
                } // TODO: else seems like a problem
            }
        }
    }

    fn walk_stmt(&mut self, stmt: &mut FatStmt<'p>) {
        if let Stmt::DeclVar { name, dropping, .. } = &mut stmt.stmt {
            self.decl(name);
            if let Some(dropping) = dropping {
                if let Some(new_d) = self.mapping.get(dropping) {
                    *dropping = *new_d;
                }
            }
        }
    }

    fn walk_pattern(&mut self, binding: &mut Pattern<'p>) {
        for b in &mut binding.bindings {
            if let Name::Var(v) = &mut b.name {
                self.decl(v);
            }
        }
    }
}

impl<'a, 'p> RenumberVars<'a, 'p> {
    fn decl(&mut self, name: &mut Var<'p>) {
        let new = Var(name.0, self.vars.len());
        self.vars.push(self.vars[name.1]);
        let stomp = self.mapping.insert(*name, new);
        debug_assert!(stomp.is_none());
        *name = new;
    }
}

impl<'p> FatExpr<'p> {
    pub fn renumber_vars(&mut self, vars: &mut Vec<VarInfo>) {
        let mut mapping = HashMap::new();
        let mut ctx = RenumberVars { vars, mapping: &mut mapping };
        ctx.expr(self);
    }
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
    pub ty: TypeId,
    pub known: Known,
}

impl<'p> FatExpr<'p> {
    pub fn as_int(&self) -> Option<i64> {
        if let Expr::Value {
            value: Values::One(Value::I64(v)),
            ..
        } = self.expr
        {
            return Some(v);
        }
        None
    }
}

impl Default for FatExpr<'_> {
    fn default() -> Self {
        FatExpr::null(garbage_loc())
    }
}

#[derive(Clone, Debug, InterpSend)]
pub struct Pattern<'p> {
    pub bindings: Vec<Binding<'p>>,
    pub loc: Span,
}

impl<'p> Default for Pattern<'p> {
    fn default() -> Self {
        Self {
            bindings: vec![],
            loc: garbage_loc(),
        }
    }
}

#[derive(Copy, Clone, Debug, InterpSend, PartialEq, Eq)]
pub enum Name<'p> {
    Ident(Ident<'p>),
    Var(Var<'p>),
    None,
}

// arguments of a function and left of variable declaration.
#[derive(Clone, Debug, InterpSend)]
pub struct Binding<'p> {
    pub name: Name<'p>,
    pub ty: LazyType<'p>,
    pub default: Option<FatExpr<'p>>,
    pub kind: VarType,
}

impl<'p> Binding<'p> {
    pub fn type_for_name(&self, name: Var) -> Option<&LazyType> {
        if self.name == Name::Var(name) {
            return Some(&self.ty);
        }
        None
    }

    pub fn unwrap(&self) -> TypeId {
        self.ty.unwrap()
    }

    pub fn name(&self) -> Option<Ident<'p>> {
        match self.name {
            Name::Ident(n) => Some(n),
            Name::Var(n) => Some(n.0),
            Name::None => None,
        }
    }
    pub fn lazy(&self) -> &LazyType<'p> {
        &self.ty
    }
}

impl<'p> Pattern<'p> {
    pub fn empty(loc: Span) -> Self {
        Self { loc, bindings: vec![] }
    }

    // Useful for function args.
    pub fn then(&mut self, other: Self) {
        self.bindings.extend(other.bindings);
    }

    pub fn flatten(&self) -> Vec<(Option<Var<'p>>, TypeId, VarType)> {
        self.bindings
            .iter()
            .map(|b| {
                let name = match b.name {
                    Name::Var(v) => Some(v),
                    _ => None,
                };
                (name, b.unwrap(), b.kind)
            })
            .collect()
    }

    pub fn collect_vars(&self) -> Vec<Var<'p>> {
        self.bindings
            .iter()
            .flat_map(|b| match b.name {
                Name::Var(v) => Some(v),
                _ => None,
            })
            .collect()
    }

    pub fn flatten_names(&self) -> Vec<Ident<'p>> {
        self.bindings.iter().map(|b| b.name().unwrap()).collect()
    }

    // TODO: remove?
    pub fn flatten_exprs(&self) -> Option<Vec<FatExpr<'p>>> {
        self.bindings
            .iter()
            .map(|b| b.ty.clone())
            .map(|t| match t {
                LazyType::EvilUnit => panic!(),
                LazyType::Infer => None,
                LazyType::PendingEval(e) => Some(e),
                LazyType::Finished(_) => None,
                LazyType::Different(_) => unreachable!(),
            })
            .collect()
    }

    pub fn flatten_exprs_mut(&mut self) -> Option<Vec<&mut FatExpr<'p>>> {
        self.bindings
            .iter_mut()
            .map(|b| &mut b.ty)
            .map(|t| match t {
                LazyType::EvilUnit => panic!(),
                LazyType::Infer => None,
                LazyType::PendingEval(e) => Some(e),
                LazyType::Finished(_) => None,
                LazyType::Different(_) => unreachable!(),
            })
            .collect()
    }

    pub fn flatten_exprs_ref(&self) -> Option<Vec<&FatExpr<'p>>> {
        self.bindings
            .iter()
            .map(|b| &b.ty)
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
        self.bindings.retain(|b| match b.name {
            Name::Var(name) => name != arg_name,
            _ => true,
        });
        debug_assert_ne!(start, self.bindings.len());
    }

    pub fn ty(&self, program: &mut Program<'p>) -> TypeId {
        let types: Vec<_> = self
            .bindings
            .iter()
            .map(|b| &b.ty)
            .map(|t| match t {
                LazyType::Finished(ty) => *ty,
                LazyType::Infer | LazyType::PendingEval(_) | LazyType::EvilUnit | LazyType::Different(_) => unreachable!(),
            })
            .collect();
        program.tuple_of(types)
    }
}

// TODO: use this as a canary when I start doing asm stuff.
pub const MY_SECRET_VALUE: u64 = 0xDEADBEEFCAFEBABE;

impl<'p> FatExpr<'p> {
    pub fn synthetic(expr: Expr<'p>, loc: Span) -> Self {
        FatExpr {
            expr,
            loc,
            id: 123456789,
            ty: TypeId::unknown(),
            known: Known::ComptimeOnly,
        }
    }
    // used for moving out of ast
    pub fn null(loc: Span) -> Self {
        FatExpr::synthetic(
            Expr::Value {
                ty: TypeId::unknown(),
                value: Value::Poison.into(),
            },
            loc,
        )
    }

    pub fn parse_dot_chain(&self) -> Res<'p, Vec<Ident<'p>>> {
        match &self.expr {
            Expr::FieldAccess(parent, child) => {
                let mut parent = parent.parse_dot_chain()?;
                parent.push(*child);
                Ok(parent)
            }
            Expr::GetVar(v) => Ok(vec![v.0]),
            &Expr::GetNamed(i) => Ok(vec![i]),
            _ => err!("expected a.b.c.d.e",),
        }
    }

    pub fn as_func(self) -> Res<'p, Func<'p>> {
        match self.expr {
            Expr::Closure(f) => Ok(*f),
            _ => err!("expected function expression",),
        }
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
        // TODO: if this is a redeclaration, immediatly call the drop handler on the old one?
        //       thats not what rust does. but lexical destructors seem like they'd block tail recursion a lot.
        //       but also i like the thing where you refine your
        dropping: Option<Var<'p>>,
        kind: VarType,
    },
    // I have to write the logic for this anyway to deal with function args and I need it for inlining because I don't want the backend to have to deal with it.
    // The main thing this solves is letting you defer figuring out how to unwrap an expression.
    // TODO: but really you don't want the backend to think about pattern matching...
    //       unless it wants to because it would be cool to emit it as someone else's match statement when transpiling...
    //       but that gets into a dangerous land of slightly different behaviour everywhere so maybe its a bad idea. Feb-24
    // TODO: all variables should use this.
    DeclVarPattern {
        binding: Pattern<'p>,
        value: Option<FatExpr<'p>>,
        // dropping: Option<Var<'p>>,
        // kind: VarType,  // TODO: put this in pattern so function args really are the same as variables. I hate how many minor variations of shit I have.
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
    pub finished_arg: Option<TypeId>,
    pub finished_ret: Option<TypeId>,
    pub referencable_name: bool, // Diferentiate closures, etc which can't be refered to by name in the program text but I assign a name for debugging.
    pub wip: Option<FnWip<'p>>,
    pub evil_uninit: bool,

    /// Implies body.is_none(). For native targets this is the symbol to put in the indirect table for this forward declaration.
    /// This can be used for calling at runtime but not at comptime because we can't just ask the linker for the address.
    pub dynamic_import_symbol: Option<Ident<'p>>,
    /// An address to call this function. Body may be None or this could be jitted.
    /// It might correspond to dynamic_import_symbol (for libc things that you can call at runtime or comptime).
    pub comptime_addr: Option<u64>, // TODO: NonZero for niche
    /// Inline assembly will be saved here.
    // TODO: Maybe body should always be none? or maybe you want to allow composing !asm by calling the !asm again to inline with different offsets.
    pub jitted_code: Option<Vec<u32>>,
    pub any_reg_template: Option<FuncId>,
    pub llvm_ir: Option<Vec<Ident<'p>>>,
    pub module: Option<ModuleId>,
}

// TODO: use this instead of having a billion fields.
#[derive(Clone, Debug, InterpSend)]
enum FuncImpl<'p> {
    Normal(FatExpr<'p>),
    /// An external symbol to be resolved by the dynamic loader at runtime.
    /// Libc functions (prefixed with '_') are a safe bet.
    DynamicImport(Ident<'p>),
    /// An address to call this function in the compiler's process.
    ComptimeAddr(usize),
    /// Some opcodes to be emitted directly as the function body.
    /// They had better be position independent and follow the expected calling convention.
    JittedAarch64(Vec<u32>),
    /// A function the compiler can call with which registers it wants to use and get back some machine code.
    /// This allows core operations to be written as functions but leave some freedom to the register allocator.
    AnyRegTemplate(FuncId),
    /// Lines of llvm ir text to be concatenated as the body of a function.
    /// The compiler creates the signeture, prefix arg ssa references with '%', you cant declare globals.
    LlvmIr(Vec<Ident<'p>>),
    /// Purely a forward declaration. Perhaps an interp builtin.  
    None,
}

impl<'p> Func<'p> {
    pub fn new(name: Ident<'p>, arg: Pattern<'p>, ret: LazyType<'p>, body: Option<FatExpr<'p>>, loc: Span, has_name: bool) -> Self {
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
            finished_arg: None,
            finished_ret: None,
            referencable_name: has_name,
            evil_uninit: false,
            wip: None,
            dynamic_import_symbol: None,
            comptime_addr: None,
            jitted_code: None,
            any_reg_template: None,
            llvm_ir: None,
            module: None,
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
    pub fn has_tag(&self, flag: Flag) -> bool {
        self.annotations.iter().any(|a| a.name == flag.ident())
    }

    pub fn add_tag(&mut self, name: Flag) {
        if !self.has_tag(name) {
            self.annotations.push(Annotation {
                name: name.ident(),
                args: None,
            });
        }
    }

    #[track_caller]
    pub fn unwrap_ty(&self) -> FnType {
        FnType {
            arg: self.finished_arg.expect("fn type"),
            ret: self.finished_ret.expect("fn type"),
        }
    }

    pub fn known_args(arg: TypeId, ret: TypeId, loc: Span) -> (Pattern<'p>, LazyType<'p>) {
        let arg = Pattern {
            bindings: vec![Binding {
                ty: LazyType::Finished(arg),
                name: Name::None,
                default: None,
                kind: VarType::Let,
            }],
            loc,
        };
        let ret = LazyType::Finished(ret);
        (arg, ret)
    }

    pub fn any_const_args(&self) -> bool {
        // TODO: include comptime only types
        self.arg.bindings.iter().any(|b| b.kind == VarType::Const)
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

#[derive(Copy, Clone, Eq, PartialEq, Hash, InterpSend)]
pub struct FuncId(pub usize);

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, InterpSend)]
pub struct VarInfo {
    pub kind: VarType,
    pub loc: Span,
}

pub struct Program<'p> {
    pub pool: &'p StringPool<'p>,
    pub types: Vec<TypeInfo<'p>>,
    // twice as much memory but it's so much faster. TODO: can i just store hashes?
    type_lookup: HashMap<TypeInfo<'p>, TypeId>,
    pub funcs: Vec<Func<'p>>,
    /// Comptime function calls that return a type are memoized so identity works out.
    pub generics_memo: HashMap<(FuncId, Values), (Values, TypeId)>,
    // If you're looking for a function/type name that doesn't exist, these are places you can try instantiating them.
    pub impls: HashMap<Ident<'p>, Vec<FuncId>>,
    pub vars: Vec<VarInfo>,
    pub overload_sets: Vec<OverloadSet<'p>>, // TODO: use this instead of lookup_unique_func
    pub ffi_types: HashMap<u128, TypeId>,
    pub log_type_rec: RefCell<Vec<TypeId>>,
    comptime_only: BitSet, // Index is TypeId
    pub assertion_count: usize,
    pub runtime_arch: TargetArch,
    pub comptime_arch: TargetArch,
    pub inline_llvm_ir: Vec<FuncId>,
    pub modules: Vec<Module<'p>>,
    pub codemap: CodeMap,
}

impl_index_imm!(Program<'p>, TypeId, TypeInfo<'p>, types);
impl_index!(Program<'p>, FuncId, Func<'p>, funcs);
impl_index!(Program<'p>, ModuleId, Module<'p>, modules);

#[derive(Clone)]
pub struct Module<'p> {
    pub name: Ident<'p>,
    pub id: ModuleId,
    pub parent: Option<ModuleId>,
    pub toplevel: ModuleBody<'p>,
    pub exports: HashMap<Ident<'p>, Var<'p>>,
    pub children: HashMap<Ident<'p>, ModuleId>,
    pub i_depend_on: Vec<ModuleId>,
    pub depend_on_me: Vec<ModuleId>,
}

#[derive(Clone, Debug)]
pub enum ModuleBody<'p> {
    Ready(FuncId),
    Compiling(FuncId),
    Resolving,
    Parsed(Func<'p>),
    Src(String),
}

#[derive(Copy, Clone, Debug, InterpSend)]
pub struct ModuleId(pub usize);

#[derive(Clone, Debug)]
pub struct OverloadSet<'p>(pub Vec<OverloadOption>, pub Ident<'p>, pub Vec<FuncId>);

#[derive(Clone, Debug)]
pub struct OverloadOption {
    pub arg: TypeId,
    pub ret: Option<TypeId>, // For @comptime, we might not know without the args
    pub func: FuncId,
}

#[derive(Debug, Reflect)]
pub struct SuperSimple {
    pub a: i64,
    pub b: i64,
}

impl<'p> Stmt<'p> {
    // used for moving out of ast
    pub fn null(loc: Span) -> Stmt<'p> {
        Stmt::Eval(FatExpr::null(loc))
    }
}

// TODO: print actual type info
impl<'p> LazyType<'p> {
    #[track_caller]
    pub fn unwrap(&self) -> TypeId {
        self.ty().unwrap()
    }

    pub fn ty(&self) -> Option<TypeId> {
        match self {
            LazyType::Finished(ty) => Some(*ty),
            _ => None,
        }
    }

    pub fn expr(self) -> Option<FatExpr<'p>> {
        match self {
            LazyType::PendingEval(e) => Some(e),
            _ => None,
        }
    }

    pub fn expr_ref(&self) -> Option<&FatExpr<'p>> {
        match self {
            LazyType::PendingEval(e) => Some(e),
            _ => None,
        }
    }
}

macro_rules! safe_rec {
    ($self:expr, $ty:expr, $default:expr, $body:expr) => {{
        let ty = $ty;
        if $self.log_type_rec.borrow().contains(&ty) {
            $default
        } else {
            $self.log_type_rec.borrow_mut().push(ty);
            let res = $body;
            let i = $self.log_type_rec.borrow().iter().position(|check| *check == ty).unwrap();
            $self.log_type_rec.borrow_mut().remove(i);
            res
        }
    }};
}

use crate::experiments::reflect::BitSet;
pub(crate) use safe_rec;

#[repr(C)]
#[derive(Debug, Clone, Copy, Default, InterpSend, PartialEq, Eq, Hash)]
pub struct IntType {
    pub bit_count: i64,
    pub signed: bool,
}

impl<'p> Program<'p> {
    pub fn new(pool: &'p StringPool<'p>, comptime_arch: TargetArch, runtime_arch: TargetArch) -> Self {
        let mut program = Self {
            // Any needs to be first becuase I use TypeId(0) as a place holder.
            // The rest are just common ones that i want to find faster if i end up iterating the array.
            types: vec![
                TypeInfo::Unknown,
                TypeInfo::Any,
                TypeInfo::Unit,
                TypeInfo::Type,
                TypeInfo::Int(IntType { bit_count: 64, signed: true }),
                TypeInfo::Bool,
                TypeInfo::VoidPtr,
                TypeInfo::Never, // This needs to be here before calling get_ffi_type so if you try to intern one for some reason you get a real one.
                TypeInfo::F64,
            ],
            funcs: Default::default(),
            generics_memo: Default::default(),
            impls: Default::default(),
            vars: vec![],
            pool,
            overload_sets: Default::default(),
            ffi_types: Default::default(),
            log_type_rec: RefCell::new(vec![]),
            comptime_only: BitSet::empty(),
            assertion_count: 0,
            runtime_arch,
            comptime_arch,
            inline_llvm_ir: vec![],
            modules: vec![],
            type_lookup: HashMap::new(),
            codemap: CodeMap::new(),
        };

        for (i, ty) in program.types.iter().enumerate() {
            program.type_lookup.insert(ty.clone(), TypeId(i));
        }

        init_interp_send!(&mut program, FatStmt, TypeInfo);
        init_interp_send!(&mut program, Bc, IntType); // TODO: aaaa
        program.get_rs_type(SuperSimple::get_ty());

        program
    }

    /// This allows ffi types to be unique.
    pub fn get_ffi_type<T: InterpSend<'p>>(&mut self, id: u128) -> TypeId {
        self.ffi_types.get(&id).copied().unwrap_or_else(|| {
            let n = TypeId::unknown();
            // for recusive data structures, you need to create a place holder for where you're going to put it when you're ready.
            let placeholder = self.types.len();
            let ty_final = TypeId(placeholder);
            // This is unfortuante. My clever backpatching thing doesn't work because structs and enums save thier size on creation.
            // The problem manifested as wierd bugs in array stride for a few types.
            self.types.push(TypeInfo::simple_struct(vec![], n));
            self.ffi_types.insert(id, ty_final);
            let ty = T::create_type(self); // Note: Not get_type!
            self.types[placeholder] = TypeInfo::Unique(ty, (id & usize::MAX as u128) as usize);
            ty_final
        })
    }

    pub fn get_rs_type(&mut self, type_info: &'static RsType<'static>) -> TypeId {
        use crate::experiments::reflect::*;
        let id = type_info as *const RsType as usize as u128;
        self.ffi_types.get(&id).copied().unwrap_or_else(|| {
            let n = TypeId::unknown();
            // for recusive data structures, you need to create a place holder for where you're going to put it when you're ready.
            let placeholder = self.types.len();
            let ty_final = TypeId(placeholder);
            // This is unfortuante. My clever backpatching thing doesn't work because structs and enums save thier size on creation.
            // The problem manifested as wierd bugs in array stride for a few types.
            self.types.push(TypeInfo::Struct {
                fields: vec![],
                as_tuple: n,
                ffi_byte_align: Some(type_info.align),
                ffi_byte_stride: Some(type_info.stride),
            });
            self.ffi_types.insert(id, ty_final);

            let ty = match type_info.data {
                RsData::Struct(data) => {
                    let mut fields = vec![];
                    let mut types = vec![];
                    for f in data {
                        let ty = self.get_rs_type((f.ty)());
                        types.push(ty);
                        fields.push(Field {
                            ty,
                            name: self.pool.intern(f.name),
                            ffi_byte_offset: Some(f.offset),
                        })
                    }
                    let as_tuple = self.tuple_of(types);
                    self.intern_type(TypeInfo::Struct {
                        fields,
                        as_tuple,
                        ffi_byte_align: Some(type_info.align),
                        ffi_byte_stride: Some(type_info.stride),
                    })
                }
                RsData::Enum { .. } => todo!(),
                RsData::Ptr { inner, .. } => {
                    let inner = self.get_rs_type(inner);
                    self.ptr_type(inner)
                }
                RsData::Opaque => self.intern_type(TypeInfo::Int(IntType {
                    bit_count: (type_info.stride * 8) as i64,
                    signed: false,
                })),
            };

            let named = self.intern_type(TypeInfo::Named(ty, self.pool.intern(type_info.name)));
            self.types[placeholder] = TypeInfo::Unique(named, (id & usize::max_value() as u128) as usize);
            ty_final
        })
    }

    pub fn sig_str(&self, f: FuncId) -> Res<'p, Ident<'p>> {
        let func = &self.funcs[f.0];

        let args: String = func
            .arg
            .flatten()
            .iter()
            .map(|(name, ty, _)| format!("{}: {}, ", name.map(|n| self.pool.get(n.0)).unwrap_or("_"), self.log_type(*ty)))
            .collect();
        let ret = self.log_type(func.ret.unwrap());
        let out = format!("fn {}({args}) {ret}", self.pool.get(func.name));
        Ok(self.pool.intern(&out))
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

    pub extern "C" fn unique_ty(&mut self, ty: TypeId) -> TypeId {
        self.intern_type(TypeInfo::Unique(ty, self.types.len()))
    }

    pub fn load_value(&mut self, v: Value) -> Structured {
        let ty = self.type_of(&v);
        let mut v: Values = v.into();
        v.make_heap_constant();
        Structured::Const(ty, v)
    }

    pub fn named_type(&mut self, ty: TypeId, name: &str) -> TypeId {
        let ty = TypeInfo::Named(ty, self.pool.intern(name));
        self.intern_type(ty)
    }

    // aaaaa
    #[track_caller]
    pub fn find_interned(&self, ty: TypeInfo) -> TypeId {
        let id = self.types.iter().position(|check| *check == ty).expect("find_interned");
        TypeId(id)
    }

    pub fn emit_inline_llvm_ir(&mut self) -> String {
        let mut out = String::new();

        for f in self.inline_llvm_ir.clone() {
            let arg = self.funcs[f.0].arg.clone();
            let ret = self.funcs[f.0].finished_ret.unwrap();
            let body = self.funcs[f.0]
                .llvm_ir
                .as_ref()
                .unwrap()
                .iter()
                .map(|n| self.pool.get(*n).to_string())
                .collect::<Vec<_>>()
                .join("\n");
            let args = arg
                .flatten()
                .into_iter()
                .map(|(name, ty, _)| format!("{} %{}", self.for_llvm_ir(ty), self.pool.get(name.unwrap().0)))
                .collect::<Vec<_>>()
                .join(", ");
            out += &format!("define {} @FN{}({args}) alwaysinline {{ {body} }}\n\n", self.for_llvm_ir(ret), f.0);
        }

        out
    }

    pub fn for_llvm_ir(&self, ty: TypeId) -> &str {
        match &self.types[ty.0] {
            TypeInfo::Unknown | TypeInfo::Any | TypeInfo::Never | TypeInfo::F64 => todo!(),
            // TODO: special case Unit but need different type for enum padding. for returns unit should be LLVMVoidTypeInContext(self.context)
            TypeInfo::Unit | TypeInfo::Type | TypeInfo::Int(_) => "i64",
            TypeInfo::Bool => "i1",
            TypeInfo::VoidPtr | TypeInfo::Ptr(_) => "ptr",
            TypeInfo::Fn(_) | TypeInfo::FnPtr(_) | TypeInfo::Struct { .. } | TypeInfo::Tuple(_) | TypeInfo::Enum { .. } => todo!(),
            &TypeInfo::Unique(ty, _) | &TypeInfo::Named(ty, _) => self.for_llvm_ir(ty),
        }
    }

    pub fn find_unique_func(&self, name: Ident<'p>) -> Option<FuncId> {
        for overloads in &self.overload_sets {
            if overloads.1 == name {
                if overloads.0.is_empty() && overloads.2.len() == 1 {
                    return Some(overloads.2[0]);
                }
                if overloads.2.is_empty() && overloads.0.len() == 1 {
                    return Some(overloads.0[0].func);
                }
            }
        }

        None
    }

    pub fn flat_types(&self, ty: TypeId) -> Option<&[TypeId]> {
        match &self.types[ty.0] {
            TypeInfo::Tuple(t) => Some(t),
            &TypeInfo::Struct { as_tuple, .. } => self.flat_types(as_tuple),
            _ => None,
        }
    }
}

impl<'p> Program<'p> {
    // TODO: this is O(n), at the very least make sure the common types are at the beginning.
    pub fn intern_type(&mut self, ty: TypeInfo<'p>) -> TypeId {
        self.type_lookup.get(&ty).copied().unwrap_or_else(|| {
            let id = self.types.len();
            self.types.push(ty.clone());
            if self.calc_is_comptime_only_type(TypeId(id)) {
                self.comptime_only.set(id);
            }
            self.type_lookup.insert(ty, TypeId(id));
            TypeId(id)
        })
        // let id = self.types.iter().position(|check| check == &ty).unwrap_or_else(|| {
        //
        // });
        // TypeId(id)
    }

    // BRO DO NOT FUCKING CALL THIS ONE UNLESS YOU'RE SURE YOU REMEMBER TO CLOSE CONSTANTS
    #[track_caller]
    pub fn add_func<'a>(&'a mut self, func: Func<'p>) -> FuncId {
        let id = FuncId(self.funcs.len());
        self.funcs.push(func);
        id
    }

    pub fn returns_type(&self, f: FuncId) -> bool {
        let func = &self.funcs[f.0];
        let ret = func.ret.unwrap();
        let ty = &self.types[ret.0];
        ty == &TypeInfo::Type
    }

    pub fn type_of(&mut self, v: &Value) -> TypeId {
        match v {
            Value::F64(_) => self.intern_type(TypeInfo::F64),
            Value::I64(_) => TypeId::i64(),
            Value::Bool(_) => TypeId::bool(),
            Value::Type(_) => TypeId::ty(),
            // TODO: its unfortunate that this means you cant ask the type of a value unless you already know
            Value::GetNativeFnPtr(f) | Value::GetFn(f) => self.func_type(*f),
            Value::Unit => TypeId::unit(),
            Value::Poison => panic!("Tried to typecheck Value::Poison"),
            Value::Symbol(_) => Ident::get_type(self),
            Value::InterpAbsStackAddr(_) | Value::Heap { .. } => TypeId::void_ptr(),
            Value::OverloadSet(_) => todo!(),
        }
    }
    pub fn type_of_raw(&mut self, v: &Values) -> TypeId {
        match v {
            Values::One(v) => self.type_of(v),
            Values::Many(values) => {
                let types = values.iter().map(|v| self.type_of(v)).collect();
                self.tuple_of(types)
            }
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

    #[track_caller]
    pub fn func_type(&mut self, id: FuncId) -> TypeId {
        let ty = self.funcs[id.0].unwrap_ty();
        self.intern_type(TypeInfo::Fn(ty))
    }

    pub extern "C" fn ptr_type(&mut self, value_ty: TypeId) -> TypeId {
        self.intern_type(TypeInfo::Ptr(value_ty))
    }

    pub fn slice_type(&mut self, value_ty: TypeId) -> TypeId {
        let ptr = self.ptr_type(value_ty);
        self.intern_type(TypeInfo::Tuple(vec![ptr, TypeId::i64()]))
    }

    pub fn unptr_ty(&self, ptr_ty: TypeId) -> Option<TypeId> {
        let ptr_ty = &self.types[ptr_ty.0];
        if let TypeInfo::Ptr(ty) = ptr_ty {
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

    // TODO: skip through named and unique as well.
    pub fn ptr_depth(&self, mut ptr_ty: TypeId) -> usize {
        ptr_ty = self.raw_type(ptr_ty);
        let mut d = 0;
        while let &TypeInfo::Ptr(inner) = &self.types[ptr_ty.0] {
            d += 1;
            ptr_ty = self.raw_type(inner);
        }
        d
    }

    pub fn struct_type(&mut self, name: &str, fields_in: &[(&str, TypeId)]) -> TypeId {
        let name = self.pool.intern(name);
        let mut types = vec![];
        let mut fields = vec![];
        for (name, ty) in fields_in {
            fields.push(Field {
                name: self.pool.intern(name),
                ty: *ty,
                ffi_byte_offset: None,
            });
            types.push(*ty);
        }
        let as_tuple = self.tuple_of(types);
        let ty = TypeInfo::simple_struct(fields, as_tuple);
        let ty = self.intern_type(ty);
        self.intern_type(TypeInfo::Named(ty, name))
    }

    pub fn enum_type(&mut self, name: &str, varients: &[TypeId]) -> TypeId {
        let as_tuple = self.tuple_of(varients.to_vec());
        let ty = self.to_enum(self.types[as_tuple.0].clone());
        let name = self.pool.intern(name);
        self.intern_type(TypeInfo::Named(ty, name))
    }

    pub fn synth_name(&mut self, ty: TypeId) -> Ident<'p> {
        match self.types[ty.0] {
            TypeInfo::Named(_, name) => name,
            TypeInfo::Unique(ty, _) => self.synth_name(ty),
            _ => self.pool.intern(&format!("__anon_ty{}", ty.0)),
        }
    }

    pub fn to_enum(&mut self, ty: TypeInfo<'p>) -> TypeId {
        if let TypeInfo::Struct { fields, .. } = ty {
            let ty = TypeInfo::Enum {
                cases: fields.into_iter().map(|f| (f.name, f.ty)).collect(),
            };
            self.intern_type(ty)
        } else if let TypeInfo::Tuple(fields) = ty {
            let ty = TypeInfo::Enum {
                cases: fields.into_iter().map(|ty| (self.synth_name(ty), ty)).collect(),
            };
            self.intern_type(ty)
        } else {
            panic!()
        }
    }

    pub fn named_tuple(&mut self, name: &str, types: Vec<TypeId>) -> TypeId {
        let ty = self.tuple_of(types);
        let name = self.pool.intern(name);
        self.intern_type(TypeInfo::Named(ty, name))
    }

    pub fn is_comptime_only(&self, value: &Structured) -> bool {
        match value {
            Structured::RuntimeOnly(_) | Structured::Emitted(_, _) => false, // sure hope not or we're already fucked.
            Structured::TupleDifferent(ty, _) | Structured::Const(ty, _) => self.is_comptime_only_type(*ty),
        }
    }

    // All TypeId must have gone through intern_type, so it will always be here.
    pub fn is_comptime_only_type(&self, ty: TypeId) -> bool {
        self.comptime_only.get(ty.0)
    }

    fn calc_is_comptime_only_type(&self, ty: TypeId) -> bool {
        match &self.types[ty.0] {
            TypeInfo::Unit
            | TypeInfo::Any
            | TypeInfo::Unknown
            | TypeInfo::Never
            | TypeInfo::F64
            | TypeInfo::VoidPtr
            | TypeInfo::FnPtr(_)
            | TypeInfo::Int(_)
            | TypeInfo::Bool => false,
            // TODO: supply "runtime" versions of these for macros to work with
            TypeInfo::Fn(_) => true,
            // TODO: !!! this is wrong. when true it tries to do it to the builtin shims which is probably fine but need to fix something with the missing name vs body.
            TypeInfo::Type => false,
            TypeInfo::Tuple(types) => types.iter().any(|ty| self.is_comptime_only_type(*ty)),
            TypeInfo::Unique(ty, _) | TypeInfo::Named(ty, _) | TypeInfo::Ptr(ty) => self.is_comptime_only_type(*ty),
            TypeInfo::Struct { fields, .. } => fields.iter().any(|f| self.is_comptime_only_type(f.ty)),
            TypeInfo::Enum { cases, .. } => cases.iter().any(|(_, ty)| self.is_comptime_only_type(*ty)),
        }
    }

    #[track_caller]
    pub fn to_type(&mut self, value: Values) -> Res<'p, TypeId> {
        match value {
            Values::One(Value::Unit) => Ok(TypeId::unit()),
            Values::One(Value::Type(id)) => Ok(id),
            Values::Many(values) => {
                let values: Res<'_, Vec<_>> = values.into_iter().map(|v| self.to_type(v.into())).collect();
                Ok(self.tuple_of(values?))
            }
            _ => {
                err!(CErr::TypeError("Type", value))
            }
        }
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

    pub fn get_tag_arg(&self, name: Flag) -> Option<&FatExpr<'p>> {
        self.annotations.iter().find(|a| a.name == name.ident()).and_then(|a| a.args.as_ref())
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

    pub fn as_fn(&self) -> Option<FuncId> {
        match self {
            &Expr::Value {
                value: Values::One(Value::GetFn(f)),
                ..
            }
            | &Expr::WipFunc(f) => Some(f),
            _ => None,
        }
    }

    pub fn unit() -> Expr<'p> {
        Expr::Value {
            ty: TypeId::unit(),
            value: Value::Unit.into(),
        }
    }

    pub fn ty(ty: TypeId) -> Expr<'p> {
        Expr::Value {
            ty: TypeId::ty(),
            value: Value::Type(ty).into(),
        }
    }

    pub fn int(v: i64) -> Expr<'p> {
        Expr::Value {
            ty: TypeId::i64(),
            value: Value::I64(v).into(),
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
            finished_arg: None,
            finished_ret: None,
            referencable_name: false,
            evil_uninit: true,
            wip: None,
            dynamic_import_symbol: None,
            comptime_addr: None,
            jitted_code: None,
            any_reg_template: None,
            llvm_ir: None,
            module: None,
        }
    }
}

pub fn garbage_loc() -> Span {
    // Surely any (u32, u32) is valid
    unsafe { mem::zeroed() }
}

// TODO: replace with new walk

impl<'p, M: FnMut(&mut Expr<'p>)> WalkAst<'p> for M {
    fn walk_expr(&mut self, expr: &mut FatExpr<'p>) {
        self(&mut expr.expr)
    }
}

impl<'p> TypeInfo<'p> {
    pub fn simple_struct(fields: Vec<Field<'p>>, as_tuple: TypeId) -> Self {
        TypeInfo::Struct {
            fields,
            as_tuple,
            ffi_byte_align: None,
            ffi_byte_stride: None,
        }
    }
}

impl TypeId {
    pub fn is_unit(&self) -> bool {
        *self == Self::unit()
    }

    pub fn is_unknown(&self) -> bool {
        self.0 == 0
    }

    pub fn is_any(&self) -> bool {
        self.0 == 1
    }

    pub fn is_never(&self) -> bool {
        *self == Self::never()
    }

    // Be careful that this is in the pool correctly!
    pub fn unknown() -> TypeId {
        TypeId(0)
    }

    // Be careful that this is in the pool correctly!
    /// Placeholder to use while working on typechecking.
    pub fn any() -> TypeId {
        TypeId(1)
    }

    // Be careful that this is in the pool correctly!
    pub fn unit() -> TypeId {
        TypeId(2)
    }

    // Be careful that this is in the pool correctly!
    pub fn ty() -> TypeId {
        TypeId(3)
    }

    // Be careful that this is in the pool correctly!
    pub fn i64() -> TypeId {
        TypeId(4)
    }

    // Be careful that this is in the pool correctly!
    pub fn bool() -> TypeId {
        TypeId(5)
    }

    // Be careful that this is in the pool correctly!
    pub fn void_ptr() -> TypeId {
        TypeId(6)
    }

    // Be careful that this is in the pool correctly!
    pub fn never() -> TypeId {
        TypeId(7)
    }
}

/// It's important that these are consecutive in flags for safety of TryFrom
#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Hash, Debug, PartialEq, Eq, InterpSend)]
#[repr(u8)]
pub enum TargetArch {
    Interp = Flag::Interp as u8,
    Aarch64 = Flag::Aarch64 as u8,
    Llvm = Flag::Llvm as u8,
}

/// I don't require the values be stable, it just needs to be fixed within one run of the compiler so I can avoid a billion hash lookups.
/// They're converted to lowercase which means you can't express an uppercase one.
#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Hash, Debug, PartialEq, Eq, InterpSend)]
#[repr(u8)]
pub enum Flag {
    _Reserved_Null_,
    Interp,
    Aarch64,
    Llvm,
    _Reserved_End_Arch_, // It's important which are above and below this point.
    Comptime,
    Forward,
    Generic,
    No_Interp,
    As,
    Inline,
    NoInline,
    Struct,
    Enum,
    Asm,
    C_Call,
    Annotation,
    Placeholder,
    Comptime_Addr,
    Init,
    Construct,
    Slice,
    Unquote_Macro_Apply_Placeholders,
    From_Bit_Literal,
    Quote,
    Unquote,
    Deref,
    Patch,
    Drop_Args,
    Backpass,
    Ct,
    Bs,
    Rs,
    Any_Reg,
    Impl,
    Ptr,
    Literal_Ast,
    Main,
    Builtin_If,
    Builtin_While,
    If,
    While,
    Addr,
    Tag,
    Reflect_Print,
    Fn_Ptr,
    Overload_Set_Ast,
    Import,
    TopLevel,
    Module,
    Include_Std,
    Alloc,
    Pub,
    Open,
    This,
    Super,
    _Reserved_Count_,
}

impl<'p> TryFrom<Ident<'p>> for Flag {
    type Error = CompileError<'p>;

    fn try_from(value: Ident<'p>) -> Result<Self, Self::Error> {
        // # Safety
        // https://rust-lang.github.io/unsafe-code-guidelines/layout/enums.html
        // "As in C, discriminant values that are not specified are defined as either 0 (for the first variant) or as one more than the prior variant."
        if value.0 > Flag::_Reserved_Null_ as usize && value.0 < Flag::_Reserved_Count_ as usize {
            Ok(unsafe { transmute(value.0 as u8) })
        } else {
            err!("Unknown Ident",)
        }
    }
}

impl<'p> TryFrom<Ident<'p>> for TargetArch {
    type Error = ();

    fn try_from(value: Ident<'p>) -> Result<Self, Self::Error> {
        // # Safety
        // https://rust-lang.github.io/unsafe-code-guidelines/layout/enums.html
        // "As in C, discriminant values that are not specified are defined as either 0 (for the first variant) or as one more than the prior variant."
        // I defined thier values to be the values in Flag (where I made sure they're consecutive)
        if value.0 > Flag::_Reserved_Null_ as usize && value.0 < Flag::_Reserved_End_Arch_ as usize {
            Ok(unsafe { transmute(value.0 as u8) })
        } else {
            Err(())
        }
    }
}