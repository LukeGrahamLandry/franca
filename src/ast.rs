//! High level representation of a Franca program. Macros operate on these types.
use crate::{
    bc::Values,
    compiler::{CErr, Compile, CompileError, Res},
    err, extend_options,
    ffi::InterpSend,
    impl_index, impl_index_imm,
    pool::{Ident, StringPool},
    reflect::{BitSet, RsType},
    unwrap, Map, STATS,
};
use codemap::Span;
use interp_derive::InterpSend;
use std::{
    cell::RefCell,
    hash::Hash,
    mem::{self, transmute},
    ops::{Deref, DerefMut},
};

impl std::fmt::Debug for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_valid() {
            write!(f, "Ty{}", self.as_index())
        } else {
            write!(f, "Ty_BAD_{}", self.0)
        }
    }
}

#[derive(Copy, Clone, PartialEq, Hash, Eq, Debug, InterpSend)]
pub struct FnType {
    // Functions with multiple arguments are treated as a tuple.
    pub arg: TypeId,
    pub ret: TypeId,
}

#[repr(i64)]
#[derive(Copy, Clone, PartialEq, Hash, Eq, Debug, InterpSend)]
pub enum VarType {
    Let,
    Var,
    Const,
}

#[repr(C, i64)]
#[derive(Clone, PartialEq, Hash, Eq, Debug, InterpSend, Default)]
pub enum TypeInfo<'p> {
    #[default]
    Unknown,
    Never,
    F64,
    Int(IntTypeInfo),
    Bool,
    Fn(FnType),
    FnPtr(FnType),
    Ptr(TypeId), // One element
    Array {
        inner: TypeId,
        len: usize,
    },
    Struct {
        // You probably always have few enough that this is faster than a hash map. // TODO: check that
        fields: Vec<Field<'p>>,
        layout_done: bool,
    },
    // What rust calls an enum
    Tagged {
        cases: Vec<(Ident<'p>, TypeId)>,
    },
    // TODO: on assignment, check that it's a valid value. (at the very least do it for constants)
    Enum {
        raw: TypeId,
        fields: Vec<(Ident<'p>, Values)>,
    },
    // Let you ask for type checking on things that have same repr but don't make the backend deal with it.
    Unique(TypeId, usize),
    Named(TypeId, Ident<'p>),
    Type,
    Unit, // TODO: same as empty tuple but easier to type
    VoidPtr,
    OverloadSet,
    Scope,
    Label(TypeId),
}

#[derive(Clone, Copy, PartialEq, Hash, Eq, Debug, InterpSend, Default)]
pub struct TypeMeta {
    pub size_slots: u16,
    pub stride_bytes: u16,
    pub align_bytes: u16,
    pub float_mask: u32,
    pub has_special_pointer_fns: bool,
    pub contains_pointers: bool,
}

impl TypeMeta {
    #[track_caller]
    fn new(size_slots: u16, align_bytes: u16, float_mask: u32, has_special_pointer_fns: bool, contains_pointers: bool, stride_bytes: u16) -> Self {
        debug_assert_eq!(stride_bytes % align_bytes, 0);
        Self {
            size_slots,
            align_bytes,
            stride_bytes,
            float_mask,
            has_special_pointer_fns,
            contains_pointers,
        }
    }
}

#[derive(Clone, Hash, Debug, PartialEq, Eq, InterpSend)]
pub struct Field<'p> {
    pub name: Ident<'p>,
    pub ty: TypeId,
    pub default: Option<Values>,
    pub byte_offset: usize,
}

#[derive(Clone, Debug, InterpSend)]
pub struct Annotation<'p> {
    pub name: Ident<'p>,
    pub args: Option<FatExpr<'p>>,
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug, InterpSend)]
pub struct Var<'p> {
    pub name: Ident<'p>,
    pub id: u32,
    pub scope: ScopeId,
    pub block: u16,
    pub kind: VarType,
}

// impl<'p> PartialEq for Var<'p> {
//     fn eq(&self, other: &Self) -> bool {
//         self.id == other.id
//     }
// }
// TODO: this should work??? implies the above only works by luck so im afraid until i understand
// impl<'p> Hash for Var<'p> {
//     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
//         state.write_u32(self.id);
//     }
// }
// TODO: should really get an arena going because boxes make me sad.
#[repr(C, i64)]
#[derive(Clone, Debug, InterpSend)]
pub enum Expr<'p> {
    Poison,
    Value {
        value: Values,
    },
    WipFunc(FuncId),
    Call(Box<FatExpr<'p>>, Box<FatExpr<'p>>),
    Block {
        body: Vec<FatStmt<'p>>,
        result: Box<FatExpr<'p>>,
        ret_label: Option<LabelId>,
        hoisted_constants: bool,
    },
    Tuple(Vec<FatExpr<'p>>),
    Closure(Box<Func<'p>>),
    AddToOverloadSet(Vec<Func<'p>>),
    SuffixMacro(Ident<'p>, Box<FatExpr<'p>>),
    FieldAccess(Box<FatExpr<'p>>, Ident<'p>),
    StructLiteralP(Pattern<'p>),
    PrefixMacro {
        handler: Box<FatExpr<'p>>,
        arg: Box<FatExpr<'p>>,
        target: Box<FatExpr<'p>>,
    },
    GetVar(Var<'p>),
    GetNamed(Ident<'p>),
    String(Ident<'p>),
    // This is what TupleAccess and FieldAccess both desugar to. Even for tagged unions!
    PtrOffset {
        ptr: Box<FatExpr<'p>>,
        bytes: usize,
    },
    GetParsed(usize),
    Cast(Box<FatExpr<'p>>),
}

impl<'p> FatExpr<'p> {
    pub fn is_raw_unit(&self) -> bool {
        matches!(self.expr, Expr::Value { .. }) && self.ty == TypeId::unit // TODO: Unique
    }
}
pub trait WalkAst<'p> {
    // Return false to not go deeper down this branch.
    fn pre_walk_expr(&mut self, _: &mut FatExpr<'p>) -> bool {
        true
    }
    fn post_walk_expr(&mut self, _: &mut FatExpr<'p>) {}
    fn pre_walk_stmt(&mut self, _: &mut FatStmt<'p>) {}
    fn post_walk_stmt(&mut self, _: &mut FatStmt<'p>) {}
    fn pre_walk_func(&mut self, _: &mut Func<'p>) {}
    fn walk_pattern(&mut self, _: &mut Pattern<'p>) {}
    fn walk_ty(&mut self, _: &mut LazyType<'p>) {}

    fn expr(&mut self, expr: &mut FatExpr<'p>) {
        if !self.pre_walk_expr(expr) {
            return;
        }
        match &mut expr.expr {
            Expr::GetParsed(_) | Expr::AddToOverloadSet(_) => unreachable!(),
            Expr::Poison => unreachable!("ICE: POISON"),
            Expr::Call(fst, snd) => {
                self.expr(fst);
                self.expr(snd);
            }
            Expr::Cast(v) => {
                self.expr(v);
            }
            Expr::PtrOffset { ptr, .. } => {
                self.expr(ptr);
            }
            Expr::Block { body, result, .. } => {
                for stmt in body {
                    self.stmt(stmt);
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
            Expr::PrefixMacro { handler, arg, target } => {
                self.expr(handler);
                self.expr(arg);
                self.expr(target);
            }
            Expr::GetVar(_) => {}
            Expr::Closure(func) => {
                self.func(func);
            }
            Expr::WipFunc(_) => todo!("walkwip"),
            Expr::Value { .. } | Expr::GetNamed(_) | Expr::String(_) => {}
        }
        self.post_walk_expr(expr);
    }

    fn func(&mut self, func: &mut Func<'p>) {
        self.pre_walk_func(func);
        self.pattern(&mut func.arg);
        self.ty(&mut func.ret);
        if let FuncImpl::Normal(body) = &mut func.body {
            self.expr(body);
        }
    }

    fn stmt(&mut self, stmt: &mut FatStmt<'p>) {
        self.pre_walk_stmt(stmt);
        for a in &mut stmt.annotations {
            if let Some(args) = &mut a.args {
                self.expr(args)
            }
        }
        match &mut stmt.stmt {
            Stmt::ExpandParsedStmts(_) => todo!(),
            Stmt::DeclNamed { ty, value, .. } => {
                self.expr(value);
                self.ty(ty);
            }
            Stmt::Noop => {}
            Stmt::Eval(arg) => self.expr(arg),
            Stmt::DeclFunc(func) => self.func(func),
            Stmt::DeclVar { ty, value, .. } => {
                self.expr(value);
                self.ty(ty);
            }
            Stmt::DeclVarPattern { binding, value } => {
                self.pattern(binding);
                self.expr(value);
            }
            Stmt::Set { place, value } => {
                self.expr(place);
                self.expr(value);
            }
        }
        self.post_walk_stmt(stmt);
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
pub(crate) struct RenumberVars<'a, 'p, 'aa> {
    pub vars: u32,
    pub mapping: &'a mut Map<Var<'p>, Var<'p>>,
    pub(crate) _compile: &'a mut Compile<'aa, 'p>,
}

impl<'a, 'p, 'aa> WalkAst<'p> for RenumberVars<'a, 'p, 'aa> {
    fn pre_walk_func(&mut self, func: &mut Func<'p>) {
        if let Some(name) = &mut func.var_name {
            if let Some(new) = self.mapping.get(name) {
                *name = *new;
            } else {
                self.decl(name);
            }
        }
        for name in &mut func.capture_vars {
            if let Some(new) = self.mapping.get(name) {
                *name = *new;
            }
        }
        if let Some(name) = &mut func.return_var {
            if let Some(new) = self.mapping.get(name) {
                *name = *new;
            }
        }
    }

    fn pre_walk_expr(&mut self, expr: &mut FatExpr<'p>) -> bool {
        if let Expr::GetVar(v) = &mut expr.expr {
            if let Some(new) = self.mapping.get(v) {
                *v = *new;
            }
        }
        true
    }

    fn pre_walk_stmt(&mut self, stmt: &mut FatStmt<'p>) {
        if let Stmt::DeclVar { name, .. } = &mut stmt.stmt {
            self.decl(name);
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

impl<'a, 'p, 'aa> RenumberVars<'a, 'p, 'aa> {
    fn decl(&mut self, name: &mut Var<'p>) {
        let new = Var { id: self.vars, ..*name };
        self.vars += 1;
        let stomp = self.mapping.insert(*name, new);
        debug_assert!(stomp.is_none());
        *name = new;
    }
}

impl<'p> FatExpr<'p> {
    pub fn renumber_vars(&mut self, vars: u32, mapping: &mut Map<Var<'p>, Var<'p>>, compile: &mut Compile<'_, 'p>) -> u32 {
        let mut ctx = RenumberVars {
            vars,
            mapping,
            _compile: compile,
        };
        ctx.expr(self);
        ctx.vars
    }

    pub fn as_const(&self) -> Option<Values> {
        if let Expr::Value { value } = &self.expr {
            Some(value.clone())
        } else {
            None
        }
    }

    pub fn expect_const(&self) -> Res<'p, Values> {
        Ok(unwrap!(self.as_const(), "expected const values"))
    }
}

// Some common data needed by all expression types.
// This is annoying and is why I want `using(SomeStructType, SomeEnumType)` in my language.
#[derive(Clone, Debug, InterpSend)]
pub struct FatExpr<'p> {
    pub expr: Expr<'p>,
    pub loc: Span,
    pub ty: TypeId,
    pub done: bool,
}

impl<'p> FatExpr<'p> {
    pub fn set(&mut self, value: Values, ty: TypeId) {
        debug_assert!(!ty.is_unknown());
        self.expr = Expr::Value { value };
        self.ty = ty;
        self.done = true;
    }

    pub fn value(value: Values, ty: TypeId, loc: Span) -> Self {
        FatExpr::synthetic_ty(Expr::Value { value }, loc, ty)
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

impl<'p> Name<'p> {
    pub fn ident(&self) -> Option<Ident<'p>> {
        match self {
            Name::Ident(n) => Some(*n),
            Name::Var(n) => Some(n.name),
            Name::None => None,
        }
    }
}

impl<'p> Binding<'p> {
    pub fn unwrap(&self) -> TypeId {
        self.ty.unwrap()
    }

    pub fn name(&self) -> Option<Ident<'p>> {
        match self.name {
            Name::Ident(n) => Some(n),
            Name::Var(n) => Some(n.name),
            Name::None => None,
        }
    }
    pub fn var(&self) -> Option<Var<'p>> {
        match self.name {
            Name::Ident(_) => None,
            Name::Var(n) => Some(n),
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

    pub fn flatten_names(&self) -> Vec<Ident<'p>> {
        self.bindings.iter().map(|b| b.name().unwrap()).collect()
    }

    pub fn flatten_defaults_mut(&mut self) -> Option<Vec<&mut FatExpr<'p>>> {
        self.bindings.iter_mut().map(|b| b.default.as_mut()).collect()
    }

    pub fn flatten_defaults_ref(&self) -> Option<Vec<&FatExpr<'p>>> {
        self.bindings.iter().map(|b| b.default.as_ref()).collect()
    }

    pub fn remove_named(&mut self, arg_name: Var<'p>) {
        let start = self.bindings.len();
        self.bindings.retain(|b| match b.name {
            Name::Var(name) => name != arg_name,
            _ => true,
        });
        debug_assert_ne!(start, self.bindings.len());
        if self.bindings.is_empty() {
            self.bindings.push(Binding {
                name: Name::None,
                ty: LazyType::Finished(TypeId::unit),
                default: None,
                kind: VarType::Let,
            })
        }
    }

    // TODO: probably shouldn't do this because ideally someone would take it out later anyway.
    //       but for now it fixes a Poison debug check on inferp and kinda makes it more consistant
    //       so all functions have an argument (but I could just know empty means Unit).
    pub fn if_empty_add_unit(&mut self) {
        if self.bindings.is_empty() {
            self.bindings.push(Binding {
                name: Name::None,
                ty: LazyType::Finished(TypeId::unit),
                default: None,
                kind: VarType::Let,
            });
        }
    }
}

impl<'p> FatExpr<'p> {
    pub fn synthetic(expr: Expr<'p>, loc: Span) -> Self {
        unsafe { STATS.ast_expr_nodes_all += 1 };

        FatExpr {
            expr,
            loc,
            ty: TypeId::unknown,
            done: false,
        }
    }
    pub fn synthetic_ty(expr: Expr<'p>, loc: Span, ty: TypeId) -> Self {
        debug_assert!(!ty.is_unknown());
        let mut e = Self::synthetic(expr, loc);
        e.ty = ty;
        e.done = false;
        e
    }

    // used for moving out of ast
    pub fn null(loc: Span) -> Self {
        FatExpr::synthetic(Expr::Poison, loc)
    }
}

#[derive(Clone, Debug, InterpSend)]
pub enum Stmt<'p> {
    Noop,
    Eval(FatExpr<'p>),
    DeclFunc(Box<Func<'p>>),

    // Backend Only
    DeclVar {
        name: Var<'p>,
        ty: LazyType<'p>,
        value: FatExpr<'p>,
    },
    // I have to write the logic for this anyway to deal with function args and I need it for inlining because I don't want the backend to have to deal with it.
    // The main thing this solves is letting you defer figuring out how to unwrap an expression.
    // TODO: but really you don't want the backend to think about pattern matching...
    //       unless it wants to because it would be cool to emit it as someone else's match statement when transpiling...
    //       but that gets into a dangerous land of slightly different behaviour everywhere so maybe its a bad idea. Feb-24
    // TODO: all variables should use this.
    DeclVarPattern {
        binding: Pattern<'p>,
        value: FatExpr<'p>,
        // kind: VarType,  // TODO: put this in pattern so function args really are the same as variables. I hate how many minor variations of shit I have.
    },

    // Frontend only
    DeclNamed {
        name: Ident<'p>,
        ty: LazyType<'p>,
        value: FatExpr<'p>,
        kind: VarType,
    },
    Set {
        place: FatExpr<'p>,
        value: FatExpr<'p>,
    },
    ExpandParsedStmts(usize),
}

#[derive(Clone, Debug, InterpSend)]
pub struct FatStmt<'p> {
    pub stmt: Stmt<'p>,
    pub annotations: Vec<Annotation<'p>>,
    pub loc: Span,
}

// NOTE: you can't store the FuncId in here because I clone it!
#[repr(C)]
#[derive(Clone, Debug, InterpSend)]
pub struct Func<'p> {
    pub annotations: Vec<Annotation<'p>>,
    pub name: Ident<'p>,           // it might be an annonomus closure
    pub var_name: Option<Var<'p>>, // TODO: having both ^ is redundant
    pub arg: Pattern<'p>,
    pub ret: LazyType<'p>,
    pub capture_vars: Vec<Var<'p>>,
    pub loc: Span,
    pub finished_arg: Option<TypeId>,
    pub finished_ret: Option<TypeId>,

    // This is the scope containing the args/body constants for this function and all its specializations. It's parent contained the function declaration.
    pub scope: Option<ScopeId>,
    pub cc: Option<CallConv>,
    pub return_var: Option<Var<'p>>,
    pub callees: Vec<FuncId>,
    pub mutual_callees: Vec<FuncId>,
    pub body: FuncImpl<'p>,
    pub flags: u32,
}

#[repr(i64)]
pub enum FuncFlags {
    NotEvilUninit,
    ResolvedBody,
    ResolvedSign,
    AllowRtCapture,
    EnsuredCompiled,
    AsmDone,
    TryConstantFold,
    CalleesAsmDone,
}

impl<'p> Func<'p> {
    pub fn get_flag(&self, f: FuncFlags) -> bool {
        self.flags & (1 << f as u32) != 0
    }
    pub fn set_flag(&mut self, f: FuncFlags, v: bool) {
        if v {
            self.flags |= 1 << f as u32;
        } else {
            self.flags &= !(1 << f as u32);
        }
    }
}

#[repr(i64)]
#[derive(Copy, Clone, Debug, InterpSend, Eq, PartialEq)]
pub enum CallConv {
    Arg8Ret1, // This is what #c_call means currently but its not the real c abi cause it can't do structs.
    Arg8Ret1Ct,
    // #flat_call
    Flat,   // first arg is a zero
    FlatCt, // first arg is compiler context pointer
    // #one_ret_pic.
    OneRetPic,
    /// The front end duplicates the function body ast at each callsite.
    Inline,
}

// TODO: use this instead of having a billion fields.
#[repr(C, i64)]
#[derive(Clone, Debug, InterpSend)]
pub enum FuncImpl<'p> {
    Normal(FatExpr<'p>),
    /// An external symbol to be resolved by the dynamic loader at runtime.
    /// Libc functions (prefixed with '_') are a safe bet.
    DynamicImport(Ident<'p>),
    /// An address to call this function in the compiler's process.
    ComptimeAddr(usize),
    /// Some opcodes to be emitted directly as the function body.
    /// They had better be position independent and follow the expected calling convention.
    JittedAarch64(Vec<u32>),
    /// Lines of llvm ir text to be concatenated as the body of a function.
    /// The compiler creates the signeture, prefix arg ssa references with '%', you cant declare globals.
    LlvmIr(Ident<'p>),
    EmitCranelift(usize), // CfEmit
    PendingRedirect {
        arg: TypeId,
        ret: TypeId,
        os: OverloadSetId,
    },
    Redirect(FuncId),
    Merged(Vec<FuncImpl<'p>>),
    Empty,
}

impl<'p> Func<'p> {
    pub fn new(name: Ident<'p>, arg: Pattern<'p>, ret: LazyType<'p>, body: Option<FatExpr<'p>>, loc: Span, allow_rt_capture: bool) -> Self {
        let mut f = Func {
            name,
            arg,
            ret,
            body: body.map(FuncImpl::Normal).unwrap_or(FuncImpl::Empty),
            loc,
            ..Default::default()
        };
        f.set_flag(FuncFlags::NotEvilUninit, true);
        f.set_flag(FuncFlags::AllowRtCapture, allow_rt_capture);
        f
    }

    /// Find annotation ignoring arguments
    pub fn has_tag(&self, flag: Flag) -> bool {
        self.annotations.iter().any(|a| a.name == flag.ident())
    }

    pub fn get_tag_mut(&mut self, flag: Flag) -> Option<&mut Annotation<'p>> {
        self.annotations.iter_mut().find(|a| a.name == flag.ident())
    }
    pub fn get_tag(&self, flag: Flag) -> Option<&Annotation<'p>> {
        self.annotations.iter().find(|a| a.name == flag.ident())
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
    pub fn set_cc(&mut self, cc: CallConv) -> Res<'static, ()> {
        if cc == CallConv::Inline {
            assert!(!self.has_tag(Flag::NoInline), "#inline and #noinline");
        }
        if self.cc == Some(CallConv::OneRetPic) && cc == CallConv::Arg8Ret1 {
            // TODO: HACK. now with merging. the aarch64 version says OneRetPic and llvm version says Arg8Ret1 but thats fine cause really they're different functions.
            //       so treating them as the same one is kinda but also the old SplitFunc system was more dumb.
            return Ok(());
        }
        if self.cc == Some(CallConv::Arg8Ret1) && cc == CallConv::OneRetPic {
            // TODO: HACK.
            self.cc = Some(CallConv::OneRetPic);
            return Ok(());
        }
        match self.cc {
            Some(old) => assert!(old == cc, "tried to change cc from {old:?} to {cc:?}"),
            None => self.cc = Some(cc),
        }
        Ok(())
    }

    #[track_caller]
    pub(crate) fn unwrap_ty(&self) -> FnType {
        self.finished_ty().expect("fn type")
    }

    pub(crate) fn finished_ty(&self) -> Option<FnType> {
        if let Some(arg) = self.finished_arg {
            if let Some(ret) = self.finished_ret {
                return Some(FnType { arg, ret });
            }
        }
        None
    }

    pub(crate) fn known_args(arg: TypeId, ret: TypeId, loc: Span) -> (Pattern<'p>, LazyType<'p>) {
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

    pub(crate) fn any_const_args(&self) -> bool {
        self.arg.bindings.iter().any(|b| b.kind == VarType::Const)
    }

    #[track_caller]
    pub(crate) fn assert_body_not_resolved(&self) -> Res<'p, ()> {
        assert!(self.get_flag(FuncFlags::NotEvilUninit));
        assert!(!self.get_flag(FuncFlags::ResolvedBody));
        Ok(())
    }
}
#[derive(Clone, Debug, Default, InterpSend)]
pub enum LazyType<'p> {
    #[default]
    EvilUnit,
    Infer,
    PendingEval(FatExpr<'p>),
    Finished(TypeId),
    Different(Vec<Self>),
}

pub struct Program<'p> {
    pub pool: &'p StringPool<'p>,
    pub types: Vec<TypeInfo<'p>>,
    // twice as much memory but it's so much faster. TODO: can i just store hashes?
    pub type_lookup: Map<TypeInfo<'p>, TypeId>,
    pub funcs: Vec<Func<'p>>,
    /// Comptime function calls that return a type are memoized so identity works out.
    /// Note: if i switch to Values being raw bytes, make sure to define any padding so this works.
    pub generics_memo: Map<(FuncId, Values), (Values, TypeId)>,
    pub next_var: u32,
    pub overload_sets: Vec<OverloadSet<'p>>, // TODO: use this instead of lookup_unique_func
    pub ffi_types: Map<u128, TypeId>,
    pub log_type_rec: RefCell<Vec<TypeId>>,
    pub assertion_count: usize,
    pub runtime_arch: TargetArch,
    pub comptime_arch: TargetArch,
    pub inline_llvm_ir: Vec<FuncId>,
    pub ffi_definitions: String,
    // After binding const args to a function, you get a new function with fewer arguments.
    pub const_bound_memo: Map<(FuncId, Vec<u8>), FuncId>,
    pub types_extra: RefCell<Vec<Option<TypeMeta>>>,
    finished_layout_deep: BitSet,
}

impl_index_imm!(Program<'p>, TypeId, TypeInfo<'p>, types);
impl_index!(Program<'p>, FuncId, Func<'p>, funcs);
impl_index!(Program<'p>, OverloadSetId, OverloadSet<'p>, overload_sets);

#[derive(Clone, Debug)]
pub struct OverloadSet<'p> {
    pub ready: Vec<OverloadOption>,
    pub name: Ident<'p>,
    pub pending: Vec<FuncId>,
    pub public: bool,
    pub just_resolved: Vec<Func<'p>>,
}

#[derive(Clone, Debug)]
pub struct OverloadOption {
    pub arg: TypeId,
    pub ret: Option<TypeId>, // For @comptime, we might not know without the args
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

pub(crate) use safe_rec;

#[derive(Debug, Clone, Copy, Default, InterpSend, PartialEq, Eq, Hash)]
pub struct IntTypeInfo {
    pub bit_count: i64,
    pub signed: bool,
}

impl<'p> Program<'p> {
    pub fn new(pool: &'p StringPool<'p>, comptime_arch: TargetArch, runtime_arch: TargetArch) -> Self {
        let mut program = Self {
            finished_layout_deep: BitSet::empty(),
            // these are hardcoded numbers in TypeId constructors
            types: vec![
                TypeInfo::Unknown,
                TypeInfo::Unit,
                TypeInfo::Type,
                TypeInfo::Int(IntTypeInfo { bit_count: 64, signed: true }),
                TypeInfo::Bool,
                TypeInfo::VoidPtr,
                // TODO: this is flawed now that its a hashmap -- Apr 28 also if you remove it remember to fix later indices!
                TypeInfo::Never, // This needs to be here before calling get_ffi_type so if you try to intern one for some reason you get a real one.
                TypeInfo::F64,
                TypeInfo::OverloadSet,
                TypeInfo::Scope,
            ],
            funcs: Default::default(),
            generics_memo: Default::default(),
            next_var: 0,
            pool,
            overload_sets: Default::default(),
            ffi_types: Default::default(),
            log_type_rec: RefCell::new(vec![]),
            assertion_count: 0,
            runtime_arch,
            comptime_arch,
            inline_llvm_ir: vec![],
            type_lookup: Default::default(),
            ffi_definitions: String::new(),
            const_bound_memo: Default::default(),
            types_extra: Default::default(),
        };

        for (i, ty) in program.types.iter().enumerate() {
            program.type_lookup.insert(ty.clone(), TypeId::from_index(i));
        }

        program
    }

    pub(crate) fn add_ffi_definition<'a, T: InterpSend<'a>>(&mut self) {
        let def = T::definition();
        if !def.is_empty() {
            self.ffi_definitions.push_str(&T::name());
            self.ffi_definitions.push_str(" :: ");
            self.ffi_definitions.push_str(&def);
            self.ffi_definitions.push_str(";\n");
        }
    }

    /// This allows ffi types to be unique.
    pub(crate) fn get_ffi_type<'a, T: InterpSend<'a>>(&mut self, id: u128) -> TypeId {
        self.ffi_types.get(&id).copied().unwrap_or_else(|| {
            self.add_ffi_definition::<T>();
            // for recusive data structures, you need to create a place holder for where you're going to put it when you're ready.
            let placeholder = self.types.len();
            let ty_final = TypeId::from_index(placeholder);
            // This is unfortuante. My clever backpatching thing doesn't work because structs and enums save thier size on creation.
            // The problem manifested as wierd bugs in array stride for a few types.
            self.types.push(TypeInfo::Unknown);
            self.ffi_types.insert(id, ty_final);
            let ty = T::create_type(self); // Note: Not get_type!
            self.types[placeholder] = TypeInfo::Unique(ty, (id & usize::MAX as u128) as usize);
            self.types_extra.borrow_mut().truncate(placeholder);
            ty_final
        })
    }

    pub(crate) fn get_rs_type(&mut self, type_info: &'static RsType<'static>) -> TypeId {
        use crate::reflect::*;
        // TODO: think more about this but need same int type
        if let RsData::Opaque = type_info.data {
            return self.intern_type(TypeInfo::Int(IntTypeInfo {
                bit_count: (type_info.stride * 8) as i64,
                signed: true, // TODO
            }));
        }
        let id = type_info as *const RsType as usize as u128;
        self.ffi_types.get(&id).copied().unwrap_or_else(|| {
            let n = TypeId::unknown;
            // for recusive data structures, you need to create a place holder for where you're going to put it when you're ready.
            let placeholder = self.types.len();
            let ty_final = TypeId::from_index(placeholder);
            // This is unfortuante. My clever backpatching thing doesn't work because structs and enums save thier size on creation.
            // The problem manifested as wierd bugs in array stride for a few types.
            self.types.push(TypeInfo::Struct {
                fields: vec![],
                layout_done: false,
            });
            self.ffi_types.insert(id, ty_final);
            println!("from RsType {}", type_info.name);
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
                            byte_offset: f.offset,
                            default: None,
                        })
                    }
                    self.intern_type(TypeInfo::Struct { fields, layout_done: true })
                }
                RsData::Enum { .. } => todo!(),
                RsData::Ptr { inner, .. } => {
                    let inner = self.get_rs_type(inner);
                    self.ptr_type(inner)
                }
                RsData::Opaque => unreachable!(),
            };

            let named = self.intern_type(TypeInfo::Named(ty, self.pool.intern(type_info.name)));
            self.types[placeholder] = TypeInfo::Unique(named, (id & usize::max_value() as u128) as usize);
            ty_final
        })
    }

    #[track_caller]
    pub(crate) fn raw_type(&self, mut ty: TypeId) -> TypeId {
        debug_assert!(ty.is_valid(), "invalid type: {}", ty.0);

        while let &TypeInfo::Unique(inner, _) | &TypeInfo::Named(inner, _) | &TypeInfo::Enum { raw: inner, .. } = &self[ty] {
            ty = inner
        }
        ty
    }

    pub(crate) fn get_enum(&self, enum_ty: TypeId) -> Option<&[(Ident<'p>, TypeId)]> {
        let enum_ty = self.raw_type(enum_ty);
        if let TypeInfo::Tagged { cases, .. } = &self[enum_ty] {
            Some(cases)
        } else {
            None
        }
    }

    pub(crate) fn tuple_of(&mut self, types: Vec<TypeId>) -> TypeId {
        match types.len() {
            0 => TypeId::unit,
            1 => types[0],
            _ => {
                // TODO: dont allocate the string a billion times
                println!("tuple struct of {}", types.iter().map(|t| self.log_type(*t)).collect::<String>());
                let info = self
                    .make_struct(
                        types
                            .iter()
                            .enumerate()
                            .map(|(i, ty)| Ok((*ty, self.pool.intern(&format!("_{i}")), None))),
                    )
                    .unwrap();
                self.intern_type(info)
            }
        }
    }

    pub(crate) fn make_struct(&self, parts: impl Iterator<Item = Res<'p, (TypeId, Ident<'p>, Option<Values>)>>) -> Res<'p, TypeInfo<'p>> {
        let mut fields = vec![];
        for p in parts {
            let (ty, name, default) = p?;
            fields.push(Field {
                name,
                ty,
                default,
                byte_offset: 99999999999,
            });
        }

        Ok(TypeInfo::Struct { fields, layout_done: false })
    }

    pub(crate) fn finish_layout_deep(&mut self, ty: TypeId) -> Res<'p, ()> {
        if self.finished_layout_deep.get(ty.as_index()) {
            return Ok(());
        }
        self.finished_layout_deep.insert(ty.as_index(), true); // do this at the beginning to stop recursion.
        self.finish_layout(ty)?;
        let ty = self.raw_type(ty);
        match &self[ty] {
            &TypeInfo::Fn(f) | &TypeInfo::FnPtr(f) => {
                self.finish_layout_deep(f.arg)?;
                self.finish_layout_deep(f.ret)?;
            }
            TypeInfo::Ptr(inner) | TypeInfo::Label(inner) | TypeInfo::Array { inner, .. } => self.finish_layout_deep(*inner)?,
            TypeInfo::Struct { fields, .. } => {
                // TODO: no clone
                for f in fields.clone() {
                    self.finish_layout_deep(f.ty)?
                }
            }
            TypeInfo::Tagged { cases } => {
                // TODO: no clone
                for f in cases.clone() {
                    self.finish_layout_deep(f.1)?
                }
            }
            _ => {}
        }
        Ok(())
    }
    pub(crate) fn finish_layout(&mut self, ty: TypeId) -> Res<'p, ()> {
        let ty = self.raw_type(ty);
        if let TypeInfo::Array { inner, .. } = self[ty] {
            return self.finish_layout(inner);
        }

        if let TypeInfo::Tagged { cases } = self[ty].clone() {
            // sad!
            for c in cases {
                self.finish_layout(c.1)?;
            }
            return Ok(());
        }

        let TypeInfo::Struct { fields, layout_done } = &self[ty] else {
            return Ok(());
        };
        if *layout_done {
            return Ok(());
        }

        let mut fields = fields.clone();
        for f in &fields {
            self.finish_layout(f.ty)?;
        }

        let mut bytes = 0;
        for p in &mut fields {
            let info = self.get_info(p.ty);
            let inv_pad = bytes % info.align_bytes as usize;
            if inv_pad != 0 {
                bytes += info.align_bytes as usize - inv_pad;
            }
            p.byte_offset = bytes;

            // TODO: this must be wrong? surely you dont want to have the array padding out to alignment if its just a nested struct.
            //       tho its what c does. so i guess i need different reprs. and then size_of vs stride_of become different things.
            bytes += info.stride_bytes as usize;
        }

        let info = TypeInfo::Struct { fields, layout_done: true };
        self.types[ty.as_index()] = info.clone();
        self.type_lookup.insert(info, ty);

        Ok(())
    }

    pub extern "C" fn unique_ty(&mut self, ty: TypeId) -> TypeId {
        self.intern_type(TypeInfo::Unique(ty, self.types.len()))
    }

    pub fn named_type(&mut self, ty: TypeId, name: &str) -> TypeId {
        let ty = TypeInfo::Named(ty, self.pool.intern(name));
        self.intern_type(ty)
    }

    pub fn emit_inline_llvm_ir(&mut self) -> String {
        let mut out = String::new();

        for f in self.inline_llvm_ir.clone() {
            let arg = self.funcs[f.as_index()].arg.clone();
            let ret = self.funcs[f.as_index()].finished_ret.unwrap();
            let FuncImpl::LlvmIr(n) = self.funcs[f.as_index()].body else {
                unreachable!()
            };
            let body = self.pool.get(n).to_string();
            let args = arg
                .flatten()
                .into_iter()
                .map(|(name, ty, _)| format!("{} %{}", self.for_llvm_ir(ty), self.pool.get(name.unwrap().name)))
                .collect::<Vec<_>>()
                .join(", ");
            out += &format!(
                "define {} @FN{}({args}) alwaysinline {{ {body} }}\n\n",
                self.for_llvm_ir(ret),
                f.as_index()
            );
        }

        out
    }

    pub fn for_llvm_ir(&self, ty: TypeId) -> &str {
        match &self[ty] {
            TypeInfo::Array { .. } => todo!(),
            TypeInfo::F64 => "double",
            TypeInfo::Unknown | TypeInfo::Never | TypeInfo::Label(_) => todo!(),
            // TODO: special case Unit but need different type for enum padding. for returns unit should be LLVMVoidTypeInContext(self.context)
            TypeInfo::Unit => todo!(),
            TypeInfo::OverloadSet | TypeInfo::Type | TypeInfo::Int(_) => "i64",
            TypeInfo::Bool => "i1",
            TypeInfo::VoidPtr | TypeInfo::Ptr(_) => "ptr",
            TypeInfo::Scope | TypeInfo::Fn(_) | TypeInfo::FnPtr(_) | TypeInfo::Struct { .. } | TypeInfo::Tagged { .. } => {
                todo!()
            }
            &TypeInfo::Enum { raw: ty, .. } | &TypeInfo::Unique(ty, _) | &TypeInfo::Named(ty, _) => self.for_llvm_ir(ty),
        }
    }

    pub fn find_unique_func(&self, name: Ident<'p>) -> Option<FuncId> {
        for overloads in &self.overload_sets {
            if overloads.name == name {
                if overloads.ready.is_empty() && overloads.pending.len() == 1 {
                    return Some(overloads.pending[0]);
                }
                if overloads.pending.is_empty() && overloads.ready.len() == 1 {
                    return Some(overloads.ready[0].func);
                }
            }
        }

        None
    }
}

impl<'p> Program<'p> {
    pub fn intern_type(&mut self, ty: TypeInfo<'p>) -> TypeId {
        self.type_lookup.get(&ty).copied().unwrap_or_else(|| {
            let id = self.types.len();
            self.types.push(ty.clone());
            let id = TypeId::from_index(id);
            self.type_lookup.insert(ty, id);
            id
        })
    }

    #[track_caller]
    pub fn add_func<'a>(&'a mut self, func: Func<'p>) -> FuncId {
        debug_assert!(func.get_flag(FuncFlags::NotEvilUninit));
        let id = FuncId::from_index(self.funcs.len());
        self.funcs.push(func);
        id
    }

    pub fn fn_ty(&mut self, id: TypeId) -> Option<FnType> {
        if let TypeInfo::Fn(ty) = self[id] {
            Some(ty)
        } else {
            None
        }
    }

    #[track_caller]
    pub fn func_type(&mut self, id: FuncId) -> TypeId {
        let ty = self[id].unwrap_ty();
        self.intern_type(TypeInfo::Fn(ty))
    }

    #[track_caller]
    pub fn fn_type(&mut self, id: FuncId) -> Option<TypeId> {
        self[id].finished_ty().map(|ty| self.intern_type(TypeInfo::Fn(ty)))
    }

    pub extern "C" fn ptr_type(&mut self, value_ty: TypeId) -> TypeId {
        self.intern_type(TypeInfo::Ptr(value_ty))
    }

    pub fn unptr_ty(&self, ptr_ty: TypeId) -> Option<TypeId> {
        let ptr_ty = self.raw_type(ptr_ty);
        let ptr_ty = &self[ptr_ty];
        if let TypeInfo::Ptr(ty) = ptr_ty {
            Some(*ty)
        } else {
            None
        }
    }

    // TODO: get rid of this. its dumb now that it needs to reallocate the vec everytime -- May 25
    pub fn tuple_types(&self, ty: TypeId) -> Option<Vec<TypeId>> {
        match &self[ty] {
            TypeInfo::Struct { fields, .. } => Some(fields.iter().map(|f| f.ty).collect()),
            &TypeInfo::Unique(ty, _) => self.tuple_types(ty),
            _ => None,
        }
    }

    // TODO: skip through named and unique as well.
    pub fn ptr_depth(&self, mut ptr_ty: TypeId) -> usize {
        ptr_ty = self.raw_type(ptr_ty);
        let mut d = 0;
        while let &TypeInfo::Ptr(inner) = &self[ptr_ty] {
            d += 1;
            ptr_ty = self.raw_type(inner);
        }
        d
    }

    pub fn struct_type(&mut self, name: &str, fields_in: &[(&str, TypeId)]) -> TypeId {
        println!("start struct {name}");
        let pool = self.pool;
        let info = self
            .make_struct(fields_in.iter().map(|(name, ty)| Ok((*ty, pool.intern(name), None))))
            .unwrap();
        println!("end struct {name}");
        let name = pool.intern(name);
        let ty = self.intern_type(info);
        self.intern_type(TypeInfo::Named(ty, name))
    }

    pub fn enum_type(&mut self, name: &str, varients: &[TypeId]) -> TypeId {
        let ty = self.intern_type(TypeInfo::Tagged {
            cases: varients
                .iter()
                .enumerate()
                .map(|(i, ty)| {
                    let name = if let TypeInfo::Named(_, name) = self[*ty] {
                        name
                    } else {
                        self.pool.intern(&format!("_{i}"))
                    };
                    (name, *ty)
                })
                .collect(),
        });
        let name = self.pool.intern(name);
        self.intern_type(TypeInfo::Named(ty, name))
    }

    pub fn synth_name(&mut self, ty: TypeId) -> Ident<'p> {
        match self[ty] {
            TypeInfo::Named(_, name) => name,
            TypeInfo::Unique(ty, _) => self.synth_name(ty),
            _ => self.pool.intern(&format!("__anon_ty{}", ty.0)),
        }
    }

    pub fn to_enum(&mut self, ty: TypeInfo<'p>) -> TypeId {
        if let TypeInfo::Struct { fields, .. } = ty {
            let ty = TypeInfo::Tagged {
                cases: fields.into_iter().map(|f| (f.name, f.ty)).collect(),
            };
            self.intern_type(ty)
        } else {
            let ty = self.intern_type(ty);
            panic!("{}", self.log_type(ty));
        }
    }

    pub(crate) fn named_tuple(&mut self, name: &str, types: Vec<TypeId>) -> TypeId {
        println!("named {name}");
        let ty = self.tuple_of(types);
        let name = self.pool.intern(name);
        self.intern_type(TypeInfo::Named(ty, name))
    }

    pub fn log_struct_layout(&self, fields: &[Field]) -> String {
        fields
            .iter()
            .map(|f| {
                format!(
                    "{}: {}; // align={} size={} offset={}\n",
                    self.pool.get(f.name),
                    self.log_type(f.ty),
                    self.get_info(f.ty).align_bytes,
                    self.get_info(f.ty).stride_bytes,
                    f.byte_offset
                )
            })
            .collect::<String>()
    }

    pub fn get_or_create_info(&mut self, ty: TypeId) -> Res<'p, TypeMeta> {
        let ty = self.raw_type(ty);
        self.finish_layout(ty)?;
        Ok(self.get_info(ty))
    }

    // TODO: Unsized types. Any should be a TypeId and then some memory with AnyPtr being the fat ptr version.
    //       With raw Any version, you couldn't always change types without reallocating the space and couldn't pass it by value.
    //       AnyScalar=(TypeId, one value), AnyPtr=(TypeId, one value=stack/heap ptr), AnyUnsized=(TypeId, some number of stack slots...)
    pub fn get_info(&self, ty: TypeId) -> TypeMeta {
        extend_options(self.types_extra.borrow_mut().deref_mut(), ty.as_index());
        if let Some(info) = self.types_extra.borrow_mut().deref_mut()[ty.as_index()] {
            return info;
        }
        let ty = self.raw_type(ty);
        extend_options(self.types_extra.borrow_mut().deref_mut(), ty.as_index());
        let info = match &self[ty] {
            TypeInfo::Unknown => todo!(), //TypeMeta::new(0, 1, 0, false, false, 0),
            TypeInfo::Struct { fields, layout_done } => {
                debug_assert!(*layout_done);
                let mut size = 0;
                debug_assert_eq!(fields[0].byte_offset, 0);
                let align = self.get_info(fields[0].ty).align_bytes;
                let mut mask = 0;
                let mut pointers = false;
                let mut bytes = 0;

                for arg in fields {
                    let info = self.get_info(arg.ty);
                    debug_assert!(arg.byte_offset as u16 >= bytes, "{}", self.log_struct_layout(fields));
                    let end = arg.byte_offset as u16 + info.stride_bytes;
                    bytes = bytes.max(end);
                    size += info.size_slots;
                    if size > 16 {
                        // you only actually care about floats for passing in registers so if its bigger than 16, wont matter anyway?
                        // until i do struct c abi properly and tuples mean args in a different way.
                        mask = 0;
                    } else {
                        mask <<= info.size_slots;
                        mask |= info.float_mask;
                    }
                    pointers |= info.contains_pointers;
                }

                if bytes % align != 0 {
                    bytes += align - (bytes % align);
                }

                // TODO: two u8s should have special load like a u16 (eventually).
                TypeMeta::new(size, align, mask, false, pointers, bytes)
            }
            TypeInfo::Tagged { cases, .. } => {
                let size = 1 + cases.iter().map(|(_, ty)| self.get_info(*ty).size_slots).max().expect("no empty enum");
                let mut bytes = 8 + cases.iter().map(|(_, ty)| self.get_info(*ty).stride_bytes).max().expect("no empty enum");
                let pointers = cases.iter().any(|(_, ty)| self.get_info(*ty).contains_pointers);

                let align = 8;
                if bytes % align != 0 {
                    bytes += align - (bytes % align);
                }
                // TODO: currently tag is always i64 so align 8 but should use byte since almost always enough. but you just have to pad it out anyway.
                //       even without that, if i add 16 byte align, need to check the fields too.
                TypeMeta::new(size, 8, 0, false, pointers, bytes)
            }
            TypeInfo::Never => TypeMeta::new(0, 1, 0, false, false, 0),
            TypeInfo::Int(int) => {
                // TODO: u16
                // :SmallTypes
                match int.bit_count {
                    8 => TypeMeta::new(1, 1, 0, true, false, 1),
                    32 => TypeMeta::new(1, 4, 0, true, false, 4),
                    _ => TypeMeta::new(1, 8, 0, false, false, 8),
                }
            }
            TypeInfo::F64 => TypeMeta::new(1, 8, 1, false, false, 8),
            TypeInfo::Unit => TypeMeta::new(0, 1, 0, true, false, 0),
            TypeInfo::Bool => TypeMeta::new(1, 1, 0, true, false, 1), // :SmallTypes
            TypeInfo::Ptr(_) | TypeInfo::VoidPtr | TypeInfo::FnPtr(_) => TypeMeta::new(1, 8, 0, false, true, 8),
            // this has to be 8 bytes because it can't have a special load function
            TypeInfo::Type => TypeMeta::new(1, 8, 0, false, false, 8),
            TypeInfo::Scope | TypeInfo::Label(_) | TypeInfo::Fn(_) | TypeInfo::OverloadSet => TypeMeta::new(1, 4, 0, true, false, 4),
            TypeInfo::Enum { .. } | TypeInfo::Unique(_, _) | TypeInfo::Named(_, _) => unreachable!(),
            &TypeInfo::Array { inner, len } => {
                let info = self.get_info(inner);
                TypeMeta::new(
                    info.size_slots * len as u16,
                    info.align_bytes,
                    0,
                    false,
                    info.contains_pointers,
                    info.stride_bytes * len as u16,
                )
            }
        };
        self.types_extra.borrow_mut().deref_mut()[ty.as_index()] = Some(info);
        info
    }

    pub(crate) fn get_infos(&self, ty: FnType) -> (TypeMeta, TypeMeta) {
        (self.get_info(ty.arg), self.get_info(ty.ret))
    }

    pub(crate) fn slot_count(&self, ty: TypeId) -> u16 {
        self.get_info(ty).size_slots
    }

    pub fn size_bytes(&self, ty: TypeId) -> usize {
        self.get_info(ty).stride_bytes as usize
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

    pub fn get_tag_arg(&self, name: Flag) -> Option<&FatExpr<'p>> {
        self.annotations.iter().find(|a| a.name == name.ident()).and_then(|a| a.args.as_ref())
    }
}

impl<'p> Expr<'p> {
    pub fn as_ident(&self) -> Option<Ident<'p>> {
        match self {
            Expr::GetVar(v) => Some(v.name),
            &Expr::GetNamed(i) => Some(i),
            _ => None,
        }
    }

    // TODO: this needs to be removed becuase it doesn't respect shadowing or arbitrary expressions that evaluate to the expected builtin thing.
    //       maybe all the ones the compiler knows about should be a different syntax/node? but the eventual goal is consistancy.     -- Apr 21
    pub fn as_prefix_macro(&self, flag: Flag) -> Option<(&FatExpr, &FatExpr)> {
        if let Expr::PrefixMacro { handler, arg, target } = self {
            if let Some(name) = handler.as_ident() {
                if name == flag.ident() {
                    return Some((arg, target));
                }
            }
        }
        None
    }
    pub fn as_suffix_macro(&self, flag: Flag) -> Option<&FatExpr<'p>> {
        if let Expr::SuffixMacro(name, arg) = self {
            if *name == flag.ident() {
                return Some(arg);
            }
        }
        None
    }
    pub fn as_suffix_macro_mut(&mut self, flag: Flag) -> Option<&mut FatExpr<'p>> {
        if let Expr::SuffixMacro(name, arg) = self {
            if *name == flag.ident() {
                return Some(arg);
            }
        }
        None
    }
}

impl<'p> Default for Func<'p> {
    fn default() -> Self {
        Self {
            mutual_callees: vec![],
            flags: 0,
            callees: vec![],
            return_var: None,
            cc: None,
            annotations: vec![],
            name: Ident::null(),
            var_name: None,
            body: FuncImpl::Empty,
            arg: Pattern {
                bindings: vec![],
                loc: garbage_loc(),
            },
            ret: LazyType::Infer,
            capture_vars: vec![],
            loc: garbage_loc(),
            finished_arg: None,
            finished_ret: None,
            scope: None,
        }
    }
}

pub fn garbage_loc() -> Span {
    // Surely any (u32, u32) is valid
    unsafe { mem::zeroed() }
}

// TODO: replace with new walk

impl<'p, M: FnMut(&mut Expr<'p>)> WalkAst<'p> for M {
    fn pre_walk_expr(&mut self, expr: &mut FatExpr<'p>) -> bool {
        self(&mut expr.expr);
        true
    }
}

#[allow(non_upper_case_globals)]
impl TypeId {
    pub fn is_unit(&self) -> bool {
        *self == Self::unit
    }

    pub fn is_unknown(&self) -> bool {
        *self == Self::unknown
    }
    pub fn is_never(&self) -> bool {
        *self == Self::never
    }

    // Be careful that these is in the pool correctly!
    pub const unknown: Self = Self::from_index(0);
    pub const unit: Self = Self::from_index(1);
    pub const ty: Self = Self::from_index(2);

    // Be careful that this is in the pool correctly!
    pub const fn i64() -> TypeId {
        TypeId::from_index(3)
    }

    // Be careful that this is in the pool correctly!
    pub fn bool() -> TypeId {
        TypeId::from_index(4)
    }

    pub const never: Self = Self::from_index(6);

    pub fn f64() -> TypeId {
        TypeId::from_index(7)
    }

    pub const overload_set: Self = Self::from_index(8);
    pub const scope: Self = Self::from_index(9);
}

/// It's important that these are consecutive in flags for safety of TryFrom
#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Hash, Debug, PartialEq, Eq, InterpSend)]
#[repr(u8)]
pub enum TargetArch {
    Aarch64 = Flag::Aarch64 as u8,
    Llvm = Flag::Llvm as u8,
    Cranelift = Flag::Cranelift as u8,
}

/// I don't require the values be stable, it just needs to be fixed within one run of the compiler so I can avoid a billion hash lookups.
/// They're converted to lowercase which means you can't express an uppercase one.
#[allow(non_camel_case_types)]
#[derive(Copy, Clone, Hash, Debug, PartialEq, Eq, InterpSend)]
#[repr(u8)]
pub enum Flag {
    _Reserved_Null_,
    Aarch64,
    Llvm,
    Cranelift,
    _Reserved_End_Arch_, // It's important which are above and below this point.
    Comptime,
    Generic,
    As,
    Inline,
    NoInline,
    Asm,
    C_Call,
    Macro,
    Placeholder,
    Comptime_Addr,
    Init,
    Slice,
    Unquote_Macro_Apply_Placeholders,
    From_Bit_Literal,
    Quote,
    Unquote,
    Deref,
    Patch,
    Ct,
    Any_Reg,
    Impl,
    Main,
    Builtin_If,
    Builtin_While,
    If,
    While,
    Addr,
    Tag,
    Reflect_Print,
    Fn_Ptr,
    TopLevel,
    Include_Std,
    Pub,
    Unreachable,
    Rt,
    Test,
    Operator_Star_Prefix,
    Operator_Question_Prefix,
    Operator_Up_Arrow_Prefix,
    Operator_Ampersand_Prefix,
    Operator_Plus_Equal,
    Operator_Minus_Equal,
    Operator_Star_Equal,
    Operator_Slash_Equal,
    Operator_Index,
    Flat_Call,
    Builtin,
    No_Memo,
    Uninitialized,
    Const_Eval,
    Contextual_Field,
    Outputs,
    When,
    One_Ret_Pic,
    Anon,
    Log_Bc,
    Log_Asm,
    Log_Ir,
    Log_Ast,
    Skip_Cranelift,
    Log_Asm_Bc,
    Return,
    Cranelift_Emit,
    Force_Cranelift,
    Force_Aarch64,
    Tail,
    Redirect,
    __Shift_Or_Slice,
    No_Tail, // TOOD: HACK. stack ptr/slice arg is UB so have to manually use this! not acceptable!
    __Return,
    __Get_Assertions_Passed,
    __String_Escapes,
    Test_Broken,
    Load,
    Store,
    Fold,
    Ptr,
    Len,
    __Tag_Check,
    _Reserved_Count_,
}

macro_rules! flag_subset {
    ($ty:ty, $before:expr, $after:expr) => {
        impl<'p> TryFrom<Ident<'p>> for $ty {
            type Error = Box<CompileError<'p>>;

            #[track_caller]
            fn try_from(value: Ident<'p>) -> Result<Self, Self::Error> {
                // # Safety
                // https://rust-lang.github.io/unsafe-code-guidelines/layout/enums.html
                // "As in C, discriminant values that are not specified are defined as either 0 (for the first variant) or as one more than the prior variant."
                // I defined thier values to be the values in Flag (where I made sure they're consecutive)
                if value.0 > $before as u32 && value.0 < $after as u32 {
                    Ok(unsafe { transmute(value.0 as u8) })
                } else {
                    // TODO: just return an option?
                    // TODO: make sure getting Caller::locatiom isn't slow
                    err!(CErr::UndeclaredIdent(value))
                }
            }
        }
    };
}

flag_subset!(TargetArch, Flag::_Reserved_Null_, Flag::_Reserved_End_Arch_);
flag_subset!(Flag, Flag::_Reserved_Null_, Flag::_Reserved_Count_);

/// When the compiler is compiled in debug mode, we set specific bits in different index types as a runtime tag to make sure we're right about what type an integer is interpreted as.
/// Also has the nice property that zero is never valid (in debug mode) so you can catch some uninitilized reads if you zero init all your memory.
/// This tag is never used for correctness in compiling the program, we track the types of each expression statically and don't need them at runtime.
/// The tag is not added by release builds of the compiler. In that case, the index types are jsut raw indexes into thier containers.
macro_rules! tagged_index {
    ($name:ty, $magic_offset:expr) => {
        impl $name {
            pub const MASK: u32 = if cfg!(debug_assertions) { (1 << $magic_offset) } else { 0 };

            #[track_caller]
            pub fn as_index(self) -> usize {
                debug_assert!(self.is_valid(), "{}", self.0);
                (self.0 & (!Self::MASK)) as usize
            }

            #[track_caller]
            pub fn as_raw(self) -> u32 {
                debug_assert!(self.is_valid(), "{}", self.0);
                self.0 as u32
            }

            #[track_caller]
            pub fn from_raw(value: u32) -> Self {
                let s = Self(value);
                debug_assert!(s.is_valid(), "{value}");
                s
            }

            #[track_caller]
            pub const fn from_index(value: usize) -> Self {
                debug_assert!((value as u32) < Self::MASK);
                Self(value as u32 | Self::MASK)
            }

            pub fn is_valid(self) -> bool {
                cfg!(not(debug_assertions)) || (self.0 & Self::MASK) != 0
            }
        }
    };
}

// Make sure these tag numbers are all different!
tagged_index!(TypeId, 31);
tagged_index!(FuncId, 30);
tagged_index!(ScopeId, 29);
tagged_index!(OverloadSetId, 28);
tagged_index!(LabelId, 27);

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Hash, Eq, Default)]
pub struct TypeId(pub u32);

#[derive(Copy, Clone, Eq, PartialEq, Hash, InterpSend)]
pub struct FuncId(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OverloadSetId(u32);

#[derive(Clone, Copy, PartialEq, Eq, Hash, InterpSend, Debug)]
pub struct LabelId(u32);

macro_rules! func_impl_getter {
    ($s:ident, $varient:ident) => {
        match &$s {
            FuncImpl::$varient(c) => Some(c),
            FuncImpl::Merged(parts) => {
                for p in parts {
                    if let FuncImpl::$varient(c) = p {
                        return Some(c);
                    }
                }
                None
            }
            _ => None,
        }
    };
}
impl<'p> FuncImpl<'p> {
    pub fn jitted_aarch64(&self) -> Option<&Vec<u32>> {
        func_impl_getter!(self, JittedAarch64)
    }

    pub fn cranelift_emit(&self) -> Option<&usize> {
        func_impl_getter!(self, EmitCranelift)
    }

    pub fn comptime_addr(&self) -> Option<&usize> {
        func_impl_getter!(self, ComptimeAddr)
    }
}
