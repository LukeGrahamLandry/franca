## Function Tags

## Expression Macros

- @slice(a, b, c)
  - create a slice of elements on the stack
- @if(cond, true_expr, false_expr)
  - for runtime conditions, prefer calling `if(bool, $@Fn() T, $@Fn() T) T` from the standard libary.
    note that the macro accepts expressions directly and causes only one to be evaluated at runtime.
    while the function forces you to explicitly pass functions.
  - however, if cond is constant, the condition will be folded in the front end and only the active branch will be emitted.
    the in-active branch is allowed to contain invalid code.
    you can use `@if(@run cond, t, f)` to assert that this folding is possible.
- @\_uninitialized
  - prefer calling `uninitialized($T) T` from the standard libary.
  - create uninitilized memory. illegal to read but you can write over it.

fn enum(arg: FatExpr, target: FatExpr) FatExpr

fn as(T: FatExpr, e: FatExpr) FatExpr

fn type(e: FatExpr) FatExpr #outputs(Type) #macro #ct #comptime_addr(%);\n", type_macro);

fn struct(arg: FatExpr) FatExpr #outputs(Type) #macro #ct #comptime_addr(%);\n", struct_macro_wrap);

fn tagged(arg: FatExpr) FatExpr #outputs(Type) #macro #ct #comptime_addr(%);\n", tagged_macro_wrap);

fn loop(e: FatExpr) FatExpr
fn builtin(t: FatExpr) FatExpr

fn Fn(Arg: FatExpr, Ret: FatExpr) FatExpr #outputs(Type) #macro #ct #comptime_addr(%);\n", fn_type_macro_erase);
fn FnPtr(Arg: FatExpr, Ret: FatExpr) FatExpr #outputs(Type) #macro #ct #comptime_addr(%);\n", fn_ptr_type_macro_erase);
fn Fn(Ret: FatExpr) FatExpr #outputs(Type) #macro #ct #comptime_addr(%);\n", fn_type_macro_single);
fn FnPtr(Ret: FatExpr) FatExpr #outputs(Type) #macro #ct #comptime_addr(%);\n", fn_ptr_type_macro_single);

## Comptime Functions

fn lookup_baked(addr: i64) ?BakedVarId
fn cache_baked(addr: i64, id: BakedVarId) ?BakedVarId
fn dyn_bake_relocatable_value(raw_bytes: Slice(u8), ty: Type, force_default_handling: bool) Slice(BakedEntry)
fn operator_star_prefix(T: Type) Type
fn Label(Ret: Type) Type

fn debug_log_int(i: i64) void
fn debug_log_str(i: Str) void

fn str(i: Symbol) Str

fn sym(i: Str) Symbol

fn get_comptime_environment() \*ComptimeEnvironment

fn by_the_way_you_are_compiling_the_compiler_right_now_just_a_helpful_hint() void

fn compile_error(msg: Str, loc: Span) Never #

fn bake_value(v: BakedVar) BakedVarId

fn safety_check_enabled(check: SafetyCheck) bool

fn get_type_info_ref(T: Type) \*TypeInfo

fn Ty(fst: Type, snd: Type) Type
fn Ty(fst: Type, snd: Type, trd: Type) Type
fn Ty(fst: Type, snd: Type, trd: Type, frt: Type) Type
fn rawptr_from_value(value: \*Values) rawptr

fn get_build_options(c: *SelfHosted) *BuildOptions

fn Tag(tagged: Type) Type
fn Fn(Arg: Type, Ret: Type) Type
fn IntType(bits: i64, signed: bool) Type

fn unquote_macro_apply_placeholders(t: Slice(FatExpr)) FatExpr
fn literal_ast(ty: Type, ptr: rawptr) FatExpr
fn const_eval(expr: FatExpr, ty: Type, result: rawptr) void

fn size_of(T: Type) i64
fn get_type_info(T: Type) TypeInfo
fn debug_log_ast(expr: FatExpr) void
fn debug_log_type(type: Type) void
fn intern_type_ref(info: \*TypeInfo) Type
fn compile_ast(e: FatExpr) FatExpr
fn symbol(e: FatExpr) FatExpr
fn tag_value(E: Type, case_name: Symbol) i64
fn c_str(s: Symbol) CStr
fn assert_compile_error(e: FatExpr) FatExpr
fn FnPtr(arg: Type, ret: Type) Type
fn get_meta(s: Type) TypeMeta

## Intrinsics
