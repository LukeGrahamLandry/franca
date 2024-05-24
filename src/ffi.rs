use std::{mem, ptr::slice_from_raw_parts};

use codemap::Span;

use crate::{
    ast::{OverloadSetId, Program, ScopeId, TypeId, TypeInfo},
    bc::{ReadBytes, WriteBytes},
    Map,
};

// TODO: figure out how to check that my garbage type keys are unique.
pub trait InterpSend<'p>: Sized {
    const SIZE_BYTES: usize;

    // TODO: could use ptr&len of a static string of the type name. but thats sad.
    //       cant use std::any because of lifetimes. tho my macro can cheat and refer to the name without lifetimes,
    //       so its jsut about the manual base impls for vec,box,option,hashmap.
    fn get_type_key() -> u128; // fuck off bro
    fn get_type(program: &mut Program<'p>) -> TypeId {
        program.get_ffi_type::<Self>(Self::get_type_key())
    }
    /// This should only be called once! Use get_type which caches it.
    fn create_type(interp: &mut Program<'p>) -> TypeId;
    fn deserialize_from_ints(values: &mut ReadBytes) -> Option<Self>;
    fn serialize_to_ints(self, values: &mut WriteBytes);
    fn serialize_to_ints_one(self) -> Vec<u8> {
        let mut values = WriteBytes::default(); // TODO: with_capacity
        self.serialize_to_ints(&mut values);
        // debug_assert_eq!(values.len(), Self::SIZE); // not true because Unit
        values.0
    }

    fn definition() -> String {
        String::new()
    }

    fn name() -> String {
        String::new()
    }

    fn add_child_ffi_definitions(_: Program<'p>) {}
}

pub fn deserialize_from_ints<'p, T: InterpSend<'p> + Sized>(values: &mut ReadBytes) -> Option<T> {
    T::deserialize_from_ints(values)
}

macro_rules! send_num {
    ($ty:ty, $bits:expr, $signed:expr, $bytes:expr, $read_fn:ident, $write_fn:ident, $as_int:ty) => {
        impl<'p> InterpSend<'p> for $ty {
            const SIZE_BYTES: usize = $bytes;

            fn get_type_key() -> u128 {
                unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
            }

            fn create_type(p: &mut Program<'p>) -> TypeId {
                p.intern_type(TypeInfo::Int($crate::ast::IntTypeInfo {
                    bit_count: $bits,
                    signed: $signed,
                }))
            }

            // TODO: use correct types
            fn get_type(p: &mut Program<'p>) -> TypeId {
                p.intern_type(TypeInfo::Int($crate::ast::IntTypeInfo {
                    bit_count: $bits,
                    signed: $signed,
                }))
            }

            fn serialize_to_ints(self, values: &mut WriteBytes) {
                values.$write_fn(self as $as_int)
            }

            fn deserialize_from_ints(values: &mut ReadBytes) -> Option<Self> {
                Some(values.$read_fn()? as $ty)
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
send_num!(i16, 16, true, 8, next_i64, push_i64, i64); // TODO
send_num!(u16, 16, false, 8, next_i64, push_i64, i64); // TODO
send_num!(i8, 8, true, 1, next_u8, push_u8, u8);
send_num!(u8, 8, false, 1, next_u8, push_u8, u8);

impl<'p, T: InterpSend<'p>> InterpSend<'p> for *mut T {
    const SIZE_BYTES: usize = 8;

    fn get_type_key() -> u128 {
        mix::<T, Box<()>>(1274222)
    }
    fn create_type(p: &mut Program<'p>) -> TypeId {
        let t = T::get_type(p);
        p.intern_type(TypeInfo::Ptr(t))
    }

    // TODO: use correct types
    fn get_type(p: &mut Program<'p>) -> TypeId {
        let t = T::get_type(p);
        p.intern_type(TypeInfo::Ptr(t))
    }

    fn serialize_to_ints(self, values: &mut WriteBytes) {
        values.push_i64(self as i64)
    }

    fn deserialize_from_ints(values: &mut ReadBytes) -> Option<Self> {
        Some(values.next_i64()? as *mut T)
    }

    fn name() -> String {
        format!("*{}", T::name())
    }
}

impl<'p> InterpSend<'p> for bool {
    const SIZE_BYTES: usize = 1;

    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }
    fn create_type(_program: &mut Program<'p>) -> TypeId {
        TypeId::bool()
    }

    fn get_type(_program: &mut Program<'p>) -> TypeId {
        TypeId::bool()
    }

    fn serialize_to_ints(self, values: &mut WriteBytes) {
        values.push_u8(if self { 1 } else { 0 })
    }

    fn deserialize_from_ints(values: &mut ReadBytes) -> Option<Self> {
        Some(values.next_u8()? != 0)
    }

    fn name() -> String {
        "bool".to_string()
    }
}

impl<'p> InterpSend<'p> for () {
    const SIZE_BYTES: usize = 0;

    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }
    fn create_type(_: &mut Program<'p>) -> TypeId {
        TypeId::unit
    }

    fn get_type(_: &mut Program<'p>) -> TypeId {
        TypeId::unit
    }

    fn serialize_to_ints(self, _: &mut WriteBytes) {}

    fn deserialize_from_ints(_: &mut ReadBytes) -> Option<Self> {
        Some(())
    }

    fn name() -> String {
        "Unit".to_string()
    }
}

impl<'p> InterpSend<'p> for char {
    const SIZE_BYTES: usize = 4;

    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }
    fn create_type(_program: &mut Program<'p>) -> TypeId {
        TypeId::i64()
    }

    fn serialize_to_ints(self, values: &mut WriteBytes) {
        values.push_u32(self as u32)
    }

    fn deserialize_from_ints(values: &mut ReadBytes) -> Option<Self> {
        char::from_u32(values.next_u32()? as u32)
    }

    fn name() -> String {
        "u32".to_string()
    }
}

impl<'p> InterpSend<'p> for f64 {
    const SIZE_BYTES: usize = 8;

    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }
    fn create_type(_program: &mut Program<'p>) -> TypeId {
        TypeId::f64()
    }

    fn serialize_to_ints(self, values: &mut WriteBytes) {
        values.push_i64(self.to_bits() as i64)
    }

    fn deserialize_from_ints(values: &mut ReadBytes) -> Option<Self> {
        Some(f64::from_bits(values.next_i64()? as u64))
    }

    fn name() -> String {
        "f64".to_string()
    }
}

impl<'p> InterpSend<'p> for TypeId {
    const SIZE_BYTES: usize = 4;

    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }
    fn create_type(_: &mut Program<'p>) -> TypeId {
        TypeId::ty
    }

    fn get_type(_: &mut Program<'p>) -> TypeId {
        TypeId::ty
    }

    fn serialize_to_ints(self, values: &mut WriteBytes) {
        values.push_u32(self.as_raw())
    }

    fn deserialize_from_ints(values: &mut ReadBytes) -> Option<Self> {
        Some(TypeId::from_raw(values.next_u32()?))
    }

    fn name() -> String {
        "Type".to_string()
    }
}

// TODO: this is sad.
impl<'p> InterpSend<'p> for OverloadSetId {
    const SIZE_BYTES: usize = 4;

    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }
    fn create_type(_: &mut Program<'p>) -> TypeId {
        TypeId::overload_set
    }

    fn get_type(_: &mut Program<'p>) -> TypeId {
        TypeId::overload_set
    }

    fn serialize_to_ints(self, values: &mut WriteBytes) {
        values.push_u32(self.as_raw())
    }

    fn deserialize_from_ints(values: &mut ReadBytes) -> Option<Self> {
        Some(OverloadSetId::from_raw(values.next_u32()?))
    }

    fn name() -> String {
        "OverloadSet".to_string()
    }
}

// TODO: this is sad.
impl<'p> InterpSend<'p> for ScopeId {
    const SIZE_BYTES: usize = 4;

    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }
    fn create_type(_: &mut Program<'p>) -> TypeId {
        TypeId::scope
    }

    fn get_type(_: &mut Program<'p>) -> TypeId {
        TypeId::scope
    }

    fn serialize_to_ints(self, values: &mut WriteBytes) {
        values.push_u32(self.as_raw())
    }

    fn deserialize_from_ints(values: &mut ReadBytes) -> Option<Self> {
        Some(ScopeId::from_raw(values.next_u32()?))
    }

    fn name() -> String {
        "ScopeId".to_string()
    }
}

impl<'p, A: InterpSend<'p>, B: InterpSend<'p>> InterpSend<'p> for (A, B) {
    const SIZE_BYTES: usize = A::SIZE_BYTES + B::SIZE_BYTES; // TODO: alignment
    fn get_type_key() -> u128 {
        mix::<A, B>(6749973390999)
    }
    fn create_type(program: &mut Program<'p>) -> TypeId {
        let a = A::get_type(program);
        let b = B::get_type(program);
        program.tuple_of(vec![a, b])
    }

    fn serialize_to_ints(self, values: &mut WriteBytes) {
        self.0.serialize_to_ints(values);
        self.1.serialize_to_ints(values);
    }

    fn deserialize_from_ints(values: &mut ReadBytes) -> Option<Self> {
        let a = A::deserialize_from_ints(values)?;
        let b = B::deserialize_from_ints(values)?;
        Some((a, b))
    }

    fn name() -> String {
        format!("Ty({}, {})", A::name(), B::name())
    }
}

impl<'p, A: InterpSend<'p>, B: InterpSend<'p>, C: InterpSend<'p>> InterpSend<'p> for (A, B, C) {
    const SIZE_BYTES: usize = A::SIZE_BYTES + B::SIZE_BYTES + C::SIZE_BYTES; // TODO: alignment
    fn get_type_key() -> u128 {
        mix::<(A, B), C>(87986)
    }
    fn create_type(program: &mut Program<'p>) -> TypeId {
        let a = A::get_type(program);
        let b = B::get_type(program);
        let c = C::get_type(program);
        program.tuple_of(vec![a, b, c])
    }

    fn serialize_to_ints(self, values: &mut WriteBytes) {
        self.0.serialize_to_ints(values);
        self.1.serialize_to_ints(values);
        self.2.serialize_to_ints(values);
    }

    fn deserialize_from_ints(values: &mut ReadBytes) -> Option<Self> {
        let a = A::deserialize_from_ints(values)?;
        let b = B::deserialize_from_ints(values)?;
        let c = C::deserialize_from_ints(values)?;
        Some((a, b, c))
    }

    fn name() -> String {
        format!("Ty(Ty({}, {}), {})", A::name(), B::name(), C::name()) // TODO: unnest
    }
}

impl<'p, T: InterpSend<'p>> InterpSend<'p> for Vec<T> {
    const SIZE_BYTES: usize = 16;
    fn get_type_key() -> u128 {
        mix::<T, i16>(999998827262625)
    }

    fn create_type(program: &mut Program<'p>) -> TypeId {
        let ty = T::get_type(program);
        let ty = program.intern_type(TypeInfo::Ptr(ty));
        program.intern_type(TypeInfo::Tuple(vec![ty, TypeId::i64()]))
    }

    fn serialize_to_ints(self, values: &mut WriteBytes) {
        let len = self.len();
        let mut parts = WriteBytes::default(); // TODO: with_capacity
        for e in self {
            e.serialize_to_ints(&mut parts);
        }
        debug_assert_eq!(parts.0.len(), T::SIZE_BYTES * len);
        let (ptr, _, _) = parts.0.into_raw_parts();
        values.push_i64(ptr as i64);
        values.push_i64(len as i64);
    }

    fn deserialize_from_ints(values: &mut ReadBytes) -> Option<Self> {
        let ptr = values.next_i64()?;
        debug_assert_eq!(ptr % 8, 0, "{ptr} is unaligned");
        let len = usize::deserialize_from_ints(values)?;

        let entries = T::SIZE_BYTES * len;
        let s = unsafe { &*slice_from_raw_parts(ptr as *const u8, entries) };

        let mut res = Vec::with_capacity(len);
        let mut reader = ReadBytes { bytes: s, i: 0 }; // TODO: alignment
        for _ in 0..len {
            res.push(T::deserialize_from_ints(&mut reader)?);
        }

        Some(res)
    }

    fn name() -> String {
        format!("List({})", T::name())
    }
}

impl<'p, T: InterpSend<'p>> InterpSend<'p> for Box<T> {
    const SIZE_BYTES: usize = 8;
    fn get_type_key() -> u128 {
        mix::<T, i8>(67445234555533)
    }

    fn create_type(program: &mut Program<'p>) -> TypeId {
        let ty = T::get_type(program);
        program.intern_type(TypeInfo::Ptr(ty))
    }

    fn serialize_to_ints(self, values: &mut WriteBytes) {
        let mut parts = WriteBytes::default();
        let inner: T = *self;
        inner.serialize_to_ints(&mut parts);
        debug_assert_eq!(parts.0.len(), T::SIZE_BYTES);
        let (ptr, _, _) = parts.0.into_raw_parts();
        values.push_i64(ptr as i64)
    }

    fn deserialize_from_ints(values: &mut ReadBytes) -> Option<Self> {
        let ptr = values.next_i64()?;
        let s = unsafe { &*slice_from_raw_parts(ptr as *const u8, T::SIZE_BYTES) };
        let v = T::deserialize_from_ints(&mut ReadBytes { bytes: s, i: 0 })?;
        Some(Box::new(v))
    }

    fn name() -> String {
        format!("*{}", T::name())
    }
}

// TODO: this should be an enum
impl<'p, T: InterpSend<'p>> InterpSend<'p> for Option<T> {
    const SIZE_BYTES: usize = 8 + T::SIZE_BYTES; // TODO
    fn get_type_key() -> u128 {
        mix::<T, bool>(8090890890986)
    }

    fn create_type(interp: &mut Program<'p>) -> TypeId {
        let t = T::get_type(interp);
        interp.tuple_of(vec![TypeId::i64(), t])
    }

    fn serialize_to_ints(self, values: &mut WriteBytes) {
        match self {
            Some(v) => {
                values.push_i64(0);
                v.serialize_to_ints(values);
            }
            None => {
                values.push_i64(1);
                for _ in 8..T::SIZE_BYTES {
                    values.push_u8(0);
                }
            }
        }
    }

    fn deserialize_from_ints(values: &mut ReadBytes) -> Option<Self> {
        match values.next_i64()? {
            0 => Some(T::deserialize_from_ints(values)),
            1 => {
                for _ in 0..T::SIZE_BYTES {
                    let _ = values.next_u8()?;
                }
                Some(None)
            }
            _ => None,
        }
    }

    fn name() -> String {
        format!("?{}", T::name())
    }
}

impl<'p> InterpSend<'p> for Span {
    const SIZE_BYTES: usize = <(u32, u32)>::SIZE_BYTES;
    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }

    fn create_type(program: &mut Program<'p>) -> TypeId {
        let ty = <(u32, u32)>::get_type(program);
        program.named_type(ty, "Span")
    }

    // This looks wierd because no breaking changes is when private field.
    fn serialize_to_ints(self, values: &mut WriteBytes) {
        let (a, b): (u32, u32) = unsafe { mem::transmute(self) };
        (a, b).serialize_to_ints(values)
    }

    fn deserialize_from_ints(values: &mut ReadBytes) -> Option<Self> {
        let (a, b) = <(u32, u32)>::deserialize_from_ints(values)?;
        let res: Span = unsafe { mem::transmute((a, b)) };
        Some(res)
    }

    fn definition() -> String {
        "(u32, u32)".to_string()
    }

    fn name() -> String {
        "Span".to_string()
    }
}

impl<'p, K: InterpSend<'p> + Eq + std::hash::Hash, V: InterpSend<'p>> InterpSend<'p> for Map<K, V> {
    const SIZE_BYTES: usize = 16;
    fn get_type_key() -> u128 {
        mix::<K, V>(1234567890)
    }

    fn create_type(interp: &mut Program<'p>) -> TypeId {
        Vec::<(K, V)>::get_type(interp)
    }

    fn serialize_to_ints(self, values: &mut WriteBytes) {
        self.into_iter().collect::<Vec<_>>().serialize_to_ints(values)
    }

    fn deserialize_from_ints(values: &mut ReadBytes) -> Option<Self> {
        Some(Vec::<(K, V)>::deserialize_from_ints(values)?.into_iter().collect::<Self>())
    }

    fn name() -> String {
        format!("HashMap({}, {})", K::name(), V::name())
    }
}

// impl<'p> InterpSend<'p> for String {
//     const SIZE: usize = 2;
//     fn get_type_key() -> u128 {
//         unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
//     }

//     fn create_type(interp: &mut Program<'p>) -> TypeId {
//         Vec::<u8>::get_type(interp)
//     }

//     fn serialize_to_ints(self, values: &mut WriteBytes) {
//         Vec::<u8>::from(self).serialize_to_ints(values)
//     }

//     fn deserialize_from_ints(values: &mut ReadBytes) -> Option<Self> {
//         Self::from_utf8(Vec::<u8>::deserialize_from_ints(values)?).ok()
//     }

//     fn name() -> String {
//         Vec::<u8>::name()
//     }
// }

fn mix<'p, A: InterpSend<'p>, B: InterpSend<'p>>(extra: u128) -> u128 {
    A::get_type_key().wrapping_mul(B::get_type_key()).wrapping_mul(extra)
}

pub mod c {
    use crate::compiler::Compile;
    use crate::compiler::Res;
    use crate::err;
    use std::arch::global_asm;

    pub fn call<'p>(program: &mut Compile<'_, 'p>, ptr: usize, f_ty: crate::ast::FnType, mut args: Vec<i64>, comp_ctx: bool) -> Res<'p, i64> {
        let (arg, ret) = program.program.get_infos(f_ty);
        let bounce = if arg.float_mask == 0 && ret.float_mask == 0 {
            arg8ret1
        } else if arg.float_mask.count_ones() == arg.size_slots as u32 && ret.float_mask.count_ones() == ret.size_slots as u32 {
            arg8ret1_all_floats
        } else {
            err!("ICE: i dont do mixed int/float registers but backend does",)
        };
        assert!(ret.size_slots <= 1, "i dont do struct calling convention yet");
        if comp_ctx {
            args.insert(0, program as *mut Compile as i64);
        }
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
        let ret: i64 = unsafe { bounce(ptr, args.as_mut_ptr()) };
        Ok(ret)
    }

    #[cfg(target_arch = "aarch64")]
    extern "C" {
        // loads 8 words from args into x0-x7 then calls fnptr
        fn arg8ret1(fnptr: usize, first_of_eight_args: *mut i64) -> i64;
        // loads 8 words from args into d0-d7 then calls fnptr. the return value in d0 is bit cast to an int in x0 for you.
        fn arg8ret1_all_floats(fnptr: usize, first_of_eight_args: *mut i64) -> i64;
    }

    // NOTE: you have to put this in the same module ::c! you cant put it outside and then import it .
    #[cfg(target_arch = "aarch64")]
    global_asm!(
        r#"
        _arg8ret1:
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
        mov x8, #0 ; extra debug check: set indirect return ptr to null so they segfault if they try to return a struct. 
        br x16 ; tail call
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
            call rax
            add sp, 16 // i have to fix the stack pointer
            ret
            "#,
                options(noreturn)
            )
        }
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
