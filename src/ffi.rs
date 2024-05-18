use std::{mem, ptr::slice_from_raw_parts};

use codemap::Span;

use crate::{
    ast::{OverloadSetId, Program, TypeId, TypeInfo},
    Map,
};

// TODO: figure out how to check that my garbage type keys are unique.
pub trait InterpSend<'p>: Sized {
    const SIZE: usize;

    // TODO: could use ptr&len of a static string of the type name. but thats sad.
    //       cant use std::any because of lifetimes. tho my macro can cheat and refer to the name without lifetimes,
    //       so its jsut about the manual base impls for vec,box,option,hashmap.
    fn get_type_key() -> u128; // fuck off bro
    fn get_type(program: &mut Program<'p>) -> TypeId {
        program.get_ffi_type::<Self>(Self::get_type_key())
    }
    fn create_type(interp: &mut Program<'p>) -> TypeId;
    /// This should only be called once! Use get_type which caches it.
    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self>;
    fn serialize_to_ints(self, values: &mut Vec<i64>);
    fn serialize_to_ints_one(self) -> Vec<i64> {
        let mut values = Vec::with_capacity(Self::SIZE);
        self.serialize_to_ints(&mut values);
        // debug_assert_eq!(values.len(), Self::SIZE); // not true because Unit
        values
    }

    fn definition() -> String {
        String::new()
    }

    fn name() -> String {
        String::new()
    }

    fn add_child_ffi_definitions(_: Program<'p>) {}
}

pub fn deserialize_from_ints<'p, T: InterpSend<'p> + Sized>(values: &mut impl Iterator<Item = i64>) -> Option<T> {
    T::deserialize_from_ints(values)
}

macro_rules! send_num {
    ($ty:ty, $bits:expr, $signed:expr) => {
        impl<'p> InterpSend<'p> for $ty {
            const SIZE: usize = 1;

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

            fn serialize_to_ints(self, values: &mut Vec<i64>) {
                values.push(self as i64)
            }

            fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
                Some(values.next()? as $ty)
            }

            fn name() -> String {
                stringify!($ty).to_string()
            }
        }
    };
}

send_num!(i64, 64, true);
send_num!(u64, 64, false);
send_num!(isize, 64, true);
send_num!(usize, 64, false);
send_num!(i32, 32, true);
send_num!(u32, 32, false);
send_num!(i16, 16, true);
send_num!(u16, 16, false);
send_num!(i8, 8, true);
send_num!(u8, 8, false);

impl<'p, T: InterpSend<'p>> InterpSend<'p> for *mut T {
    const SIZE: usize = 1;

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

    fn serialize_to_ints(self, values: &mut Vec<i64>) {
        values.push(self as i64)
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
        Some(values.next()? as *mut T)
    }

    fn name() -> String {
        format!("*{}", T::name())
    }
}

impl<'p> InterpSend<'p> for bool {
    const SIZE: usize = 1;

    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }
    fn create_type(_program: &mut Program<'p>) -> TypeId {
        TypeId::bool()
    }

    fn get_type(_program: &mut Program<'p>) -> TypeId {
        TypeId::bool()
    }

    fn serialize_to_ints(self, values: &mut Vec<i64>) {
        values.push(if self { 1 } else { 0 })
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
        Some(values.next()? != 0)
    }

    fn name() -> String {
        "bool".to_string()
    }
}

impl<'p> InterpSend<'p> for () {
    const SIZE: usize = 0;

    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }
    fn create_type(_: &mut Program<'p>) -> TypeId {
        TypeId::unit
    }

    fn get_type(_: &mut Program<'p>) -> TypeId {
        TypeId::unit
    }

    fn serialize_to_ints(self, _: &mut Vec<i64>) {}

    fn deserialize_from_ints(_: &mut impl Iterator<Item = i64>) -> Option<Self> {
        Some(())
    }

    fn name() -> String {
        "Unit".to_string()
    }
}

impl<'p> InterpSend<'p> for char {
    const SIZE: usize = 1;

    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }
    fn create_type(_program: &mut Program<'p>) -> TypeId {
        TypeId::i64()
    }

    fn serialize_to_ints(self, values: &mut Vec<i64>) {
        values.push(self as i64)
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
        char::from_u32(values.next()? as u32)
    }

    fn name() -> String {
        "u32".to_string()
    }
}

impl<'p> InterpSend<'p> for f64 {
    const SIZE: usize = 1;

    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }
    fn create_type(_program: &mut Program<'p>) -> TypeId {
        TypeId::f64()
    }

    fn serialize_to_ints(self, values: &mut Vec<i64>) {
        values.push(self.to_bits() as i64)
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
        Some(f64::from_bits(values.next()? as u64))
    }

    fn name() -> String {
        "f64".to_string()
    }
}

impl<'p> InterpSend<'p> for TypeId {
    const SIZE: usize = 1;

    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }
    fn create_type(_: &mut Program<'p>) -> TypeId {
        TypeId::ty
    }

    fn get_type(_: &mut Program<'p>) -> TypeId {
        TypeId::ty
    }

    fn serialize_to_ints(self, values: &mut Vec<i64>) {
        values.push(self.as_raw())
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
        Some(TypeId::from_raw(values.next()?))
    }

    fn name() -> String {
        "Type".to_string()
    }
}

// TODO: this is sad.
impl<'p> InterpSend<'p> for OverloadSetId {
    const SIZE: usize = 1;

    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }
    fn create_type(_: &mut Program<'p>) -> TypeId {
        TypeId::overload_set
    }

    fn get_type(_: &mut Program<'p>) -> TypeId {
        TypeId::overload_set
    }

    fn serialize_to_ints(self, values: &mut Vec<i64>) {
        values.push(self.as_raw())
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
        Some(OverloadSetId::from_raw(values.next()?))
    }

    fn name() -> String {
        "OverloadSet".to_string()
    }
}

impl<'p, A: InterpSend<'p>, B: InterpSend<'p>> InterpSend<'p> for (A, B) {
    const SIZE: usize = A::SIZE + B::SIZE;
    fn get_type_key() -> u128 {
        mix::<A, B>(6749973390999)
    }
    fn create_type(program: &mut Program<'p>) -> TypeId {
        let a = A::get_type(program);
        let b = B::get_type(program);
        program.tuple_of(vec![a, b])
    }

    fn serialize_to_ints(self, values: &mut Vec<i64>) {
        self.0.serialize_to_ints(values);
        self.1.serialize_to_ints(values);
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
        let a = A::deserialize_from_ints(values)?;
        let b = B::deserialize_from_ints(values)?;
        Some((a, b))
    }

    fn name() -> String {
        format!("Ty({}, {})", A::name(), B::name())
    }
}

impl<'p, T: InterpSend<'p>> InterpSend<'p> for Vec<T> {
    const SIZE: usize = 2;
    fn get_type_key() -> u128 {
        mix::<T, i16>(999998827262625)
    }

    fn create_type(program: &mut Program<'p>) -> TypeId {
        let ty = T::get_type(program);
        let ty = program.intern_type(TypeInfo::Ptr(ty));
        program.intern_type(TypeInfo::Tuple(vec![ty, TypeId::i64()]))
    }

    fn serialize_to_ints(self, values: &mut Vec<i64>) {
        let len = self.len();
        let mut parts = Vec::with_capacity(T::SIZE * len);
        for e in self {
            e.serialize_to_ints(&mut parts);
        }
        debug_assert_eq!(parts.len(), T::SIZE * len);
        let (ptr, _, _) = parts.into_raw_parts();
        values.push(ptr as i64);
        values.push(len as i64);
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
        let ptr = values.next()?;
        debug_assert_eq!(ptr % 8, 0, "{ptr} is unaligned");
        let len = usize::deserialize_from_ints(values)?;

        let entries = T::SIZE * len;
        let s = unsafe { &*slice_from_raw_parts(ptr as *const i64, entries) };
        let mut values = s.iter().copied();

        let mut res = Vec::with_capacity(len);
        for _ in 0..len {
            res.push(T::deserialize_from_ints(&mut values)?);
        }

        Some(res)
    }

    fn name() -> String {
        format!("List({})", T::name())
    }
}

impl<'p, T: InterpSend<'p>> InterpSend<'p> for Box<T> {
    const SIZE: usize = 1;
    fn get_type_key() -> u128 {
        mix::<T, i8>(67445234555533)
    }

    fn create_type(program: &mut Program<'p>) -> TypeId {
        let ty = T::get_type(program);
        program.intern_type(TypeInfo::Ptr(ty))
    }

    fn serialize_to_ints(self, values: &mut Vec<i64>) {
        let mut parts = vec![];
        let inner: T = *self;
        inner.serialize_to_ints(&mut parts);
        debug_assert_eq!(parts.len(), T::SIZE);
        let (ptr, _, _) = parts.into_raw_parts();
        values.push(ptr as i64)
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
        let ptr = values.next()?;
        debug_assert_eq!(ptr % 8, 0, "{ptr} is unaligned");
        let s = unsafe { &*slice_from_raw_parts(ptr as *const i64, T::SIZE) };
        let mut values = s.iter().copied();
        let v = T::deserialize_from_ints(&mut values)?;
        Some(Box::new(v))
    }

    fn name() -> String {
        format!("*{}", T::name())
    }
}

// TODO: this should be an enum
impl<'p, T: InterpSend<'p>> InterpSend<'p> for Option<T> {
    const SIZE: usize = 1 + T::SIZE;
    fn get_type_key() -> u128 {
        mix::<T, bool>(8090890890986)
    }

    fn create_type(interp: &mut Program<'p>) -> TypeId {
        let t = T::get_type(interp);
        interp.tuple_of(vec![TypeId::i64(), t])
    }

    fn serialize_to_ints(self, values: &mut Vec<i64>) {
        match self {
            Some(v) => {
                values.push(0);
                v.serialize_to_ints(values);
            }
            None => {
                values.push(1);
                for _ in 0..T::SIZE {
                    values.push(66666);
                }
            }
        }
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
        match values.next()? {
            0 => Some(T::deserialize_from_ints(values)),
            1 => {
                for _ in 0..T::SIZE {
                    let _ = values.next()?;
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
    const SIZE: usize = 2;
    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }

    fn create_type(program: &mut Program<'p>) -> TypeId {
        let ty = <(u32, u32)>::get_type(program);
        program.named_type(ty, "Span")
    }

    // This looks wierd because no breaking changes is when private field.
    fn serialize_to_ints(self, values: &mut Vec<i64>) {
        let (a, b): (u32, u32) = unsafe { mem::transmute(self) };
        (a, b).serialize_to_ints(values)
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
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
    const SIZE: usize = 2;
    fn get_type_key() -> u128 {
        mix::<K, V>(1234567890)
    }

    fn create_type(interp: &mut Program<'p>) -> TypeId {
        Vec::<(K, V)>::get_type(interp)
    }

    fn serialize_to_ints(self, values: &mut Vec<i64>) {
        self.into_iter().collect::<Vec<_>>().serialize_to_ints(values)
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
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

//     fn serialize_to_ints(self, values: &mut Vec<i64>) {
//         Vec::<u8>::from(self).serialize_to_ints(values)
//     }

//     fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
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
    use crate::bc_to_asm::Jitted;
    use crate::compiler::Compile;
    use crate::compiler::Res;
    use crate::err;
    use std::arch::global_asm;

    pub fn call<'p>(program: &mut Compile<'_, 'p>, ptr: usize, f_ty: crate::ast::FnType, mut args: Vec<i64>, comp_ctx: bool) -> Res<'p, i64> {
        debug_assert_ne!(ptr, Jitted::EMPTY);
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
