use std::{
    mem::{self, transmute},
    ptr::{slice_from_raw_parts, slice_from_raw_parts_mut},
};

use codemap::Span;

use crate::{
    ast::{Flag, OverloadSetId, Program, ScopeId, TypeId, TypeInfo},
    bc::{zero_padding, ReadBytes, WriteBytes},
    export_ffi::BigOption,
    Map,
};

// TODO: figure out how to check that my garbage type keys are unique.
pub trait InterpSend<'p>: Sized + Clone {
    // TODO: could use ptr&len of a static string of the type name. but thats sad.
    //       cant use std::any because of lifetimes. tho my macro can cheat and refer to the name without lifetimes,
    //       so its jsut about the manual base impls for vec,box,option,hashmap.
    fn get_type_key() -> u128; // fuck off bro
    fn get_or_create_type(program: &mut Program) -> TypeId {
        program.get_ffi_type::<Self>(Self::get_type_key())
    }

    fn get_type(program: &Program) -> TypeId {
        // cant unwrap_or_else cause #[track_caller] on a lambda
        let Some(&ty) = program.ffi_types.get(&Self::get_type_key()) else {
            panic!("get_type before calling get_or_create_type for {}", Self::name())
        };
        let info = program.get_info(ty);
        debug_assert_eq!(info.stride_bytes as usize, mem::size_of::<Self>());
        debug_assert_eq!(
            info.align_bytes as usize,
            mem::align_of::<Self>(),
            "bad align for {}",
            program.log_type(ty)
        );
        ty
    }
    /// This should only be called once! Use get_type which caches it.
    fn create_type(interp: &mut Program) -> TypeId;

    fn definition() -> String {
        String::new()
    }

    fn name() -> String {
        String::new()
    }

    fn add_child_ffi_definitions(_: Program<'p>) {}

    #[track_caller]
    fn size_bytes(program: &Program) -> usize {
        program.size_bytes(Self::get_type(program))
    }

    fn align_bytes(program: &Program) -> usize {
        program.get_info(Self::get_type(program)).align_bytes as usize
    }
}

macro_rules! send_num {
    ($ty:ty, $bits:expr, $signed:expr, $bytes:expr, $read_fn:ident, $write_fn:ident, $as_int:ty) => {
        impl<'p> InterpSend<'p> for $ty {
            fn get_type_key() -> u128 {
                unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
            }

            fn create_type(p: &mut Program) -> TypeId {
                p.intern_type(TypeInfo::Int($crate::ast::IntTypeInfo {
                    bit_count: $bits,
                    signed: $signed,
                }))
            }

            fn get_or_create_type(p: &mut Program) -> TypeId {
                p.intern_type(TypeInfo::Int($crate::ast::IntTypeInfo {
                    bit_count: $bits,
                    signed: $signed,
                }))
            }

            // TODO: use correct types
            fn get_type(p: &Program) -> TypeId {
                *p.type_lookup
                    .get(&TypeInfo::Int($crate::ast::IntTypeInfo {
                        bit_count: $bits,
                        signed: $signed,
                    }))
                    .expect("number type should be pre-interned")
            }

            fn name() -> String {
                stringify!($ty).to_string()
            }
        }
    };
}

send_num!(i64, 64, true, 8, next_i64, push_i64, i64);
send_num!(u64, 64, false, 8, next_i64, push_i64, i64);
send_num!(isize, 64, true, 8, next_i64, push_i64, i64);
send_num!(usize, 64, false, 8, next_i64, push_i64, i64);
send_num!(i32, 32, true, 4, next_u32, push_u32, u32);
send_num!(u32, 32, false, 4, next_u32, push_u32, u32);
send_num!(i16, 16, true, 2, next_u16, push_u16, u16);
send_num!(u16, 16, false, 2, next_u16, push_u16, u16);
send_num!(i8, 8, true, 1, next_u8, push_u8, u8);
send_num!(u8, 8, false, 1, next_u8, push_u8, u8);

impl<'p, T: InterpSend<'p>> InterpSend<'p> for *mut T {
    fn get_type_key() -> u128 {
        mix::<T, Box<()>>(1274222)
    }
    fn create_type(p: &mut Program) -> TypeId {
        let t = T::get_or_create_type(p);
        p.intern_type(TypeInfo::Ptr(t))
    }

    // TODO: use correct types
    fn get_type(p: &Program) -> TypeId {
        let t = T::get_type(p);
        *p.type_lookup.get(&TypeInfo::Ptr(t)).unwrap()
    }

    fn get_or_create_type(p: &mut Program) -> TypeId {
        let t = T::get_or_create_type(p);
        p.intern_type(TypeInfo::Ptr(t))
    }

    fn name() -> String {
        format!("*{}", T::name())
    }
}

impl<'p> InterpSend<'p> for bool {
    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }
    fn create_type(_: &mut Program) -> TypeId {
        TypeId::bool()
    }

    fn get_type(_: &Program) -> TypeId {
        TypeId::bool()
    }

    fn get_or_create_type(_: &mut Program) -> TypeId {
        TypeId::bool()
    }

    fn name() -> String {
        "bool".to_string()
    }
}

impl<'p> InterpSend<'p> for () {
    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }
    fn create_type(_: &mut Program) -> TypeId {
        TypeId::unit
    }

    fn get_type(_: &Program) -> TypeId {
        TypeId::unit
    }
    fn get_or_create_type(_: &mut Program) -> TypeId {
        TypeId::unit
    }

    fn name() -> String {
        "Unit".to_string()
    }
}

impl<'p> InterpSend<'p> for char {
    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }
    fn create_type(_program: &mut Program) -> TypeId {
        TypeId::i64()
    }

    fn name() -> String {
        "u32".to_string()
    }
}

impl<'p> InterpSend<'p> for f64 {
    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }
    fn create_type(_program: &mut Program) -> TypeId {
        TypeId::f64()
    }

    fn name() -> String {
        "f64".to_string()
    }
}

macro_rules! ffi_index {
    ($ty:ty, $typeid:expr, $name:expr) => {
        impl<'p> InterpSend<'p> for $ty {
            fn get_type_key() -> u128 {
                unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
            }
            fn create_type(_: &mut Program) -> TypeId {
                $typeid
            }

            fn get_type(_: &Program) -> TypeId {
                $typeid
            }
            fn get_or_create_type(_: &mut Program) -> TypeId {
                $typeid
            }
            fn name() -> String {
                $name.to_string()
            }
        }
    };
}

ffi_index!(OverloadSetId, TypeId::overload_set, "OverloadSet");
ffi_index!(ScopeId, TypeId::scope, "Scope");

// This needs to be 8 bytes because it can't have a special Load function.
impl<'p> InterpSend<'p> for TypeId {
    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }
    fn create_type(_: &mut Program) -> TypeId {
        TypeId::ty
    }

    fn get_type(_: &Program) -> TypeId {
        TypeId::ty
    }
    fn get_or_create_type(_: &mut Program) -> TypeId {
        TypeId::ty
    }
    fn name() -> String {
        "Type".to_string()
    }
}

macro_rules! fixed_array {
    ($ty:ty, $len:expr) => {
        impl<'p> InterpSend<'p> for [$ty; $len] {
            fn get_type_key() -> u128 {
                unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
            }
            fn create_type(p: &mut Program) -> TypeId {
                let info = TypeInfo::Array {
                    inner: <$ty>::create_type(p),
                    len: $len,
                };
                p.intern_type(info)
            }
            fn name() -> String {
                format!("Array({}, {})", <$ty>::name(), $len)
            }
        }
    };
}

fixed_array!(usize, 2);

impl<'p, A: InterpSend<'p>, B: InterpSend<'p>> InterpSend<'p> for (A, B) {
    fn get_type_key() -> u128 {
        mix::<A, B>(6749973390999)
    }
    fn create_type(program: &mut Program) -> TypeId {
        let a = A::get_or_create_type(program);
        let b = B::get_or_create_type(program);
        program.tuple_of(vec![a, b])
    }

    fn name() -> String {
        format!("Ty({}, {})", A::name(), B::name())
    }
}

impl<'p, A: InterpSend<'p>, B: InterpSend<'p>, C: InterpSend<'p>> InterpSend<'p> for (A, B, C) {
    fn get_type_key() -> u128 {
        mix::<(A, B), C>(87986)
    }
    fn create_type(program: &mut Program) -> TypeId {
        let a = A::get_or_create_type(program);
        let b = B::get_or_create_type(program);
        let c = C::get_or_create_type(program);
        program.tuple_of(vec![a, b, c])
    }

    fn name() -> String {
        format!("Ty({}, {}, {})", A::name(), B::name(), C::name()) // TODO: unnest
    }
}

impl<'p, T: InterpSend<'p>> InterpSend<'p> for Vec<T> {
    fn get_type_key() -> u128 {
        mix::<T, i16>(999998827262625)
    }

    fn create_type(program: &mut Program) -> TypeId {
        let ty = T::get_or_create_type(program);
        let ty = program.intern_type(TypeInfo::Ptr(ty));
        program.tuple_of(vec![TypeId::i64(), ty, TypeId::i64()]) // TODO: not this
    }

    fn name() -> String {
        format!("RsVec({})", T::name())
    }
}

impl<'p, T: InterpSend<'p> + Clone> InterpSend<'p> for *mut [T] {
    fn get_type_key() -> u128 {
        mix::<T, u16>(999998827262625)
    }

    fn create_type(program: &mut Program) -> TypeId {
        let ty = T::get_or_create_type(program);
        let ty = program.intern_type(TypeInfo::Ptr(ty));
        program.tuple_of(vec![ty, TypeId::i64()]) // TODO: not this
    }

    fn name() -> String {
        format!("Slice({})", T::name())
    }
}

impl<'p, T: InterpSend<'p>> InterpSend<'p> for Box<T> {
    fn get_type_key() -> u128 {
        mix::<T, i8>(67445234555533)
    }

    fn create_type(program: &mut Program) -> TypeId {
        let ty = T::get_or_create_type(program);
        program.intern_type(TypeInfo::Ptr(ty))
    }

    fn name() -> String {
        format!("*{}", T::name())
    }
}

// TODO: this should be an enum
impl<'p, T: InterpSend<'p>> InterpSend<'p> for BigOption<T> {
    fn get_type_key() -> u128 {
        mix::<T, bool>(8090890890986)
    }

    fn create_type(interp: &mut Program) -> TypeId {
        let t = T::get_or_create_type(interp);
        let some = interp.pool.intern("Some");
        let none = interp.pool.intern("None");
        interp.intern_type(TypeInfo::Tagged {
            cases: vec![(some, t), (none, TypeId::unit)],
        })
    }

    fn name() -> String {
        format!("?{}", T::name())
    }
}

impl<'p> InterpSend<'p> for Span {
    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }

    fn create_type(program: &mut Program) -> TypeId {
        let ty = <(u32, u32)>::get_or_create_type(program);
        program.named_type(ty, "Span")
    }

    fn definition() -> String {
        "(u32, u32)".to_string()
    }

    fn name() -> String {
        "Span".to_string()
    }
}

impl<'p, K: InterpSend<'p> + Eq + std::hash::Hash, V: InterpSend<'p>> InterpSend<'p> for Map<K, V> {
    fn get_type_key() -> u128 {
        mix::<K, V>(1234567890)
    }

    fn create_type(interp: &mut Program) -> TypeId {
        // Vec::<(K, V)>::get_or_create_type(interp)
        let bytes = mem::size_of::<Self>();
        debug_assert_eq!(bytes % mem::size_of::<i64>(), 0);
        let len = (bytes / mem::size_of::<i64>()) as u32;
        let inner = interp.intern_type(TypeInfo::Array { inner: TypeId::i64(), len });
        interp.struct_type("", &[("opaque", inner)])
    }

    fn name() -> String {
        format!("RsMap({}, {})", K::name(), V::name())
    }
}

fn mix<'p, A: InterpSend<'p>, B: InterpSend<'p>>(extra: u128) -> u128 {
    A::get_type_key().wrapping_mul(B::get_type_key()).wrapping_mul(extra)
}

pub mod c {
    use crate::bc::to_values;
    use crate::bc::Values;
    use crate::compiler::Compile;
    use crate::compiler::Res;
    use crate::err;
    use crate::export_ffi::BigResult::*;
    use std::arch::global_asm;
    use std::mem::transmute;

    pub fn call<'p>(program: &mut Compile<'_, 'p>, ptr: usize, f_ty: crate::ast::FnType, mut args: Vec<i64>, comp_ctx: bool) -> Res<'p, Values> {
        let (arg, ret) = program.program.get_infos(f_ty);
        let mut bounce = if arg.float_mask == 0 && ret.float_mask == 0 {
            arg8ret1
        } else if arg.float_mask.count_ones() == arg.size_slots as u32 && ret.float_mask.count_ones() == ret.size_slots as u32 {
            assert!(ret.size_slots <= 1, "TODO: float call with multiple returns at comptime");
            arg8ret1_all_floats
        } else {
            err!("ICE: i dont do mixed int/float registers but backend does",)
        };

        if comp_ctx {
            args.insert(0, program as *mut Compile as i64);
        }
        let indirect_ret = if ret.size_slots > 2 {
            let mem = vec![0u8; ret.stride_bytes as usize];
            args.insert(0, mem.as_ptr() as i64);
            assert_eq!(arg.float_mask, 0);
            bounce = arg8ret_struct;
            Some(mem)
        } else {
            None
        };
        assert!(args.len() <= 8);
        // not doing this is ub but like... meh.
        // if something interesting happens to be after the args and you call with the wrong signature you could read it.
        // its fine to read garbage memory and put it in the registers you know callee will ignore (if types are right).
        // However, now that unit is zero sized, the vec may have no allocation, so you get a segfault trying to load from it.
        const READ_GARBAGE: bool = true;
        if !READ_GARBAGE || args.is_empty() {
            args.reserve_exact(8 - args.len());
            while args.len() < 8 {
                args.push(0);
            }
        }

        if ret.size_slots <= 1 {
            let r: i64 = unsafe { bounce(ptr, args.as_mut_ptr()) };
            match ret.stride_bytes {
                0 => to_values(program.program, ()),
                1 => to_values(program.program, r as u8),
                2 => to_values(program.program, r as u16),
                4 => to_values(program.program, r as u32),
                8 => to_values(program.program, r as u64),
                _ => err!("bad byte size",),
            }
        } else if ret.size_slots == 2 {
            // TODO: floats!
            let r = arg8ret2(ptr, args.as_mut_ptr());
            to_values(program.program, (r.fst, r.snd))
        } else {
            let _: i64 = unsafe { bounce(ptr, args.as_mut_ptr()) };
            let mem = indirect_ret.unwrap();
            Ok(Values::many(mem))
        }
    }

    #[cfg(target_arch = "aarch64")]
    extern "C" {
        // loads 8 words from args into x0-x7 then calls fnptr
        fn arg8ret1(fnptr: usize, first_of_eight_args: *mut i64) -> i64;
        // loads 8 words from args into d0-d7 then calls fnptr. the return value in d0 is bit cast to an int in x0 for you.
        fn arg8ret1_all_floats(fnptr: usize, first_of_eight_args: *mut i64) -> i64;
        fn arg8ret_struct(fnptr: usize, first_of_eight_args: *mut i64) -> i64;
    }

    #[repr(C)]
    struct IntPair {
        fst: i64,
        snd: i64,
    }

    #[cfg(any(target_arch = "aarch64", target_arch = "x86_64"))]
    extern "C" fn arg8ret2(fnptr: usize, first_of_eight_args: *mut i64) -> IntPair {
        let iknowthecallingconventionisthesame: extern "C" fn(usize, *mut i64) -> IntPair = unsafe { transmute(arg8ret1 as *const ()) };
        iknowthecallingconventionisthesame(fnptr, first_of_eight_args)
    }

    // NOTE: you have to put this in the same module ::c! you cant put it outside and then import it .
    #[cfg(target_arch = "aarch64")]
    global_asm!(
        r#"
        _arg8ret1_impl:
        mov x16, x0 ; save callee since we need to use this register for args
        mov x17, x1
        ldr x0, [x17, #0]
        ldr x1, [x17, #8]
        ldr x2, [x17, #16]
        ldr x3, [x17, #24]
        ldr x4, [x17, #32]
        ldr x5, [x17, #40]
        ldr x6, [x17, #48]
        ldr x7, [x17, #56]
        br x16 ; tail call
        
        _arg8ret1:
        mov x8, #0 ; extra debug check: set indirect return ptr to null so they segfault if they try to return a struct. 
        b _arg8ret1_impl
        
        _arg8ret_struct:
        ldr x8, [x1] ; arm cc has return address in x8 but we want to represent it as first argument. 
        add x1, x1, #8
        b _arg8ret1_impl
    "#
    );

    // TODO: generate these dynamically. the backend knows the calling convention anyway.
    //       but for now this is so easy. and all ints or all floats cover the most common cases anyway.
    #[cfg(target_arch = "aarch64")]
    global_asm!(
        r#"
        _arg8ret1_all_floats:
        sub sp, sp, #16
        stp lr, fp, [sp]
        
        mov x16, x0 ; save callee since we need to use this register for args
        mov x17, x1
        ldr d0, [x17, #0]
        ldr d1, [x17, #8]
        ldr d2, [x17, #16]
        ldr d3, [x17, #24]
        ldr d4, [x17, #32]
        ldr d5, [x17, #40]
        ldr d6, [x17, #48]
        ldr d7, [x17, #56]
        mov x8, #0 ; extra debug check: set indirect return ptr to null so they segfault if they try to return a struct. 
        blr x16
        
        ldp lr, fp, [sp]
        add sp, sp, #16
        fmov x0, d0
        ret
    "#
    );

    // TODO: is there a stack redzone?
    // args 1-6: RDI, RSI, RDX, RCX, R8, R9; then stack
    // floats: xmm0 - xmm7
    #[cfg(target_arch = "x86_64")]
    #[naked]
    extern "C" fn arg8ret1(fnptr: usize, first_of_eight_args: *mut i64) -> i64 {
        use std::arch::asm;
        unsafe {
            asm!(
                r#"
            mov rax, rdi
            mov r11, rsi
            mov rdi, [r11]
            mov rsi, [r11 + 8]
            mov rdx, [r11 + 16]
            mov rcx, [r11 + 24]
            mov r8,  [r11 + 32]
            mov r9,  [r11 + 40]
            push [r11 + 48]
            push [r11 + 56]
            push [r11 + 64] // want to always allow 8 args but we might have a struct return pointer as a fake first arg. 
            call rax
            add sp, 24 // i have to fix the stack pointer
            ret
            "#,
                options(noreturn)
            )
        }
    }

    #[cfg(target_arch = "x86_64")]
    extern "C" fn arg8ret_struct(fnptr: usize, first_of_eight_args: *mut i64) -> i64 {
        unsafe { arg8ret1(fnptr, first_of_eight_args) }
    }

    #[cfg(target_arch = "x86_64")]
    #[naked]
    extern "C" fn arg8ret1_all_floats(fnptr: usize, first_of_eight_args: *mut i64) -> i64 {
        use std::arch::asm;
        unsafe {
            asm!(
                r#"
                movq xmm0, [rsi]
                movq xmm1, [rsi + 8]
                movq xmm2, [rsi + 16]
                movq xmm3, [rsi + 24]
                movq xmm4, [rsi + 32]
                movq xmm5, [rsi + 40]
                movq xmm6, [rsi + 48]
                movq xmm7, [rsi + 56]
                call rdi
                movq rax, xmm0
                ret
                "#,
                options(noreturn)
            )
        }
    }
}

pub const fn const_max(sizes: &[usize]) -> usize {
    pub const fn helper(sizes: &[usize], start: usize) -> usize {
        if sizes.len() - 1 == start {
            sizes[start]
        } else {
            let s = helper(sizes, start + 1);
            if sizes[start] > s {
                sizes[start]
            } else {
                s
            }
        }
    }

    helper(sizes, 0)
}
