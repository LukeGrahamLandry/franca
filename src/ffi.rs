use std::{mem, ptr::slice_from_raw_parts};

use codemap::Span;

use crate::{
    ast::{Program, TypeId, TypeInfo},
    bc::Value,
    Map,
};

// TODO: figure out how to check that my garbage type keys are unique.
pub trait InterpSend<'p>: Sized {
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
        let mut values = vec![];
        self.serialize_to_ints(&mut values);
        debug_assert_eq!(values.len(), Self::size());
        values
    }

    fn size() -> usize;

    fn definition() -> String {
        String::new()
    }

    fn name() -> String {
        String::new()
    }

    fn add_child_ffi_definitions(_: Program<'p>) {}
}

macro_rules! init_interp_send {
    ($program:expr,) => {};
    ($program:expr, $ty:ty) => {
        <$ty>::get_type($program);
    };
    ($program:expr, $ty:ty, $($arg:ty)*) => {
        init_interp_send!($program, $ty);
        init_interp_send!($program, $($arg)*);
    }
}

pub(crate) use init_interp_send;

pub fn deserialize_from_ints<'p, T: InterpSend<'p> + Sized>(values: &mut impl Iterator<Item = i64>) -> Option<T> {
    T::deserialize_from_ints(values)
}

macro_rules! send_num {
    ($ty:tt) => {
        impl<'p> InterpSend<'p> for $ty {
            fn get_type_key() -> u128 {
                unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
            }
            fn create_type(_: &mut Program<'p>) -> TypeId {
                TypeId::i64()
            }

            // TODO: use correct types
            fn get_type(_: &mut Program<'p>) -> TypeId {
                TypeId::i64()
            }

            fn serialize_to_ints(self, values: &mut Vec<i64>) {
                values.push(self as i64)
            }

            fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
                Some(values.next()? as $ty)
            }

            fn size() -> usize {
                1
            }

            fn name() -> String {
                stringify!($ty).to_string()
            }
        }
    };
    ($ty:tt, $($arg:tt)*) => {
        send_num!($ty);
        send_num!($($arg)*);
    }
}

// They're all treated as i64, just don't overflow and it will be fine.
send_num!(i64, i32, i16, i8, u64, u32, u16, u8, usize, isize);

impl<'p> InterpSend<'p> for bool {
    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }
    fn create_type(_program: &mut Program<'p>) -> TypeId {
        TypeId::bool()
    }

    fn serialize_to_ints(self, values: &mut Vec<i64>) {
        values.push(if self { 1 } else { 0 })
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
        Some(values.next()? != 0)
    }

    fn size() -> usize {
        1
    }

    fn name() -> String {
        "bool".to_string()
    }
}

impl<'p> InterpSend<'p> for () {
    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }
    fn create_type(_: &mut Program<'p>) -> TypeId {
        TypeId::unit()
    }

    fn get_type(_: &mut Program<'p>) -> TypeId {
        TypeId::unit()
    }

    fn serialize_to_ints(self, values: &mut Vec<i64>) {
        values.push(77777)
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
        values.next()?;
        Some(())
    }

    fn size() -> usize {
        1
    }

    fn name() -> String {
        "Unit".to_string()
    }
}

impl<'p> InterpSend<'p> for char {
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

    fn size() -> usize {
        1
    }

    fn name() -> String {
        "u32".to_string()
    }
}

impl<'p> InterpSend<'p> for f64 {
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

    fn size() -> usize {
        1
    }

    fn name() -> String {
        "f64".to_string()
    }
}

impl<'p> InterpSend<'p> for TypeId {
    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }
    fn create_type(_: &mut Program<'p>) -> TypeId {
        TypeId::ty()
    }

    fn get_type(_: &mut Program<'p>) -> TypeId {
        TypeId::ty()
    }

    fn serialize_to_ints(self, values: &mut Vec<i64>) {
        values.push(self.as_raw())
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
        Some(TypeId::from_raw(values.next()?))
    }

    fn size() -> usize {
        1
    }

    fn name() -> String {
        "Type".to_string()
    }
}

impl<'p, A: InterpSend<'p>, B: InterpSend<'p>> InterpSend<'p> for (A, B) {
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

    fn size() -> usize {
        A::size() + B::size()
    }

    fn name() -> String {
        format!("Ty({}, {})", A::name(), B::name())
    }
}

impl<'p, T: InterpSend<'p>> InterpSend<'p> for Vec<T> {
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
        let mut parts = vec![];
        for e in self {
            e.serialize_to_ints(&mut parts);
        }
        debug_assert_eq!(parts.len(), T::size() * len);
        let (ptr, _, _) = parts.into_raw_parts();
        values.push(ptr as i64);
        values.push(len as i64);
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
        let ptr = values.next()?;
        debug_assert_eq!(ptr % 8, 0, "{ptr} is unaligned");
        let len = usize::deserialize_from_ints(values)?;

        let entries = T::size() * len;
        let s = unsafe { &*slice_from_raw_parts(ptr as *const i64, entries) };
        let mut values = s.iter().copied();

        let mut res = vec![];
        for _ in 0..len {
            res.push(T::deserialize_from_ints(&mut values)?);
        }

        Some(res)
    }

    fn size() -> usize {
        2
    }

    fn name() -> String {
        format!("List({})", T::name())
    }
}

impl<'p, T: InterpSend<'p>> InterpSend<'p> for Box<T> {
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
        debug_assert_eq!(parts.len(), T::size());
        let (ptr, _, _) = parts.into_raw_parts();
        values.push(ptr as i64)
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
        let ptr = values.next()?;
        debug_assert_eq!(ptr % 8, 0, "{ptr} is unaligned");
        let s = unsafe { &*slice_from_raw_parts(ptr as *const i64, T::size()) };
        let mut values = s.iter().copied();
        let v = T::deserialize_from_ints(&mut values)?;
        Some(Box::new(v))
    }

    fn size() -> usize {
        1
    }

    fn name() -> String {
        format!("*{}", T::name())
    }
}

impl<'p> InterpSend<'p> for Value {
    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }

    fn create_type(_interp: &mut Program<'p>) -> TypeId {
        TypeId::any()
    }

    fn serialize_to_ints(self, values: &mut Vec<i64>) {
        let addr = Box::into_raw(Box::new(self)) as *const Self as usize as i64;
        values.push(addr)
    }

    fn size() -> usize {
        1
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
        let addr = i64::deserialize_from_ints(values)? as usize as *const Self;
        // TODO: leak
        Some(unsafe { *addr })
    }

    fn name() -> String {
        "VoidPtr".to_string()
    }
}

// TODO: this should be an enum
impl<'p, T: InterpSend<'p>> InterpSend<'p> for Option<T> {
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
                for _ in 0..T::size() {
                    values.push(66666);
                }
            }
        }
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
        match values.next()? {
            0 => Some(T::deserialize_from_ints(values)),
            1 => {
                for _ in 0..T::size() {
                    let _ = values.next()?;
                }
                Some(None)
            }
            _ => None,
        }
    }

    fn size() -> usize {
        1 + T::size()
    }

    fn name() -> String {
        format!("?{}", T::name())
    }
}

impl<'p> InterpSend<'p> for Span {
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

    fn size() -> usize {
        <(u32, u32)>::size()
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

    fn create_type(interp: &mut Program<'p>) -> TypeId {
        Vec::<(K, V)>::get_type(interp)
    }

    fn serialize_to_ints(self, values: &mut Vec<i64>) {
        self.into_iter().collect::<Vec<_>>().serialize_to_ints(values)
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
        Some(Vec::<(K, V)>::deserialize_from_ints(values)?.into_iter().collect::<Self>())
    }

    fn size() -> usize {
        Vec::<(K, V)>::size()
    }

    fn name() -> String {
        format!("HashMap({}, {})", K::name(), V::name())
    }
}

impl<'p> InterpSend<'p> for String {
    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }

    fn create_type(interp: &mut Program<'p>) -> TypeId {
        Vec::<u8>::get_type(interp)
    }

    fn serialize_to_ints(self, values: &mut Vec<i64>) {
        Vec::<u8>::from(self).serialize_to_ints(values)
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
        Self::from_utf8(Vec::<u8>::deserialize_from_ints(values)?).ok()
    }

    fn size() -> usize {
        Vec::<u8>::size()
    }

    fn name() -> String {
        Vec::<u8>::name()
    }
}

macro_rules! send_arr {
    ($ty:tt, $len:expr) => {
        impl<'p> InterpSend<'p> for [$ty; $len] {
            fn get_type_key() -> u128 {
                unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
            }
            fn create_type(_: &mut Program<'p>) -> TypeId {
                TypeId::i64()
            }

            fn serialize_to_ints(self, values: &mut Vec<i64>) {
                for v in self.into_iter() {
                    v.serialize_to_ints(values);
                }
            }

            fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
                let mut s: Self = Default::default();
                for i in 0..$len {
                    s[i] = <$ty>::deserialize_from_ints(values)?;
                }
                Some(s)
            }

            fn size() -> usize {
                1
            }

            fn name() -> String {
                stringify!($ty).to_string()
            }
        }
    };
}

send_arr!(i64, 6);

fn mix<'p, A: InterpSend<'p>, B: InterpSend<'p>>(extra: u128) -> u128 {
    A::get_type_key().wrapping_mul(B::get_type_key()).wrapping_mul(extra)
}

#[test]
fn interp_send() {
    use crate::ast::TargetArch;
    use crate::pool::StringPool;
    use interp_derive::InterpSend;
    #[derive(Debug, InterpSend, PartialEq, Copy, Clone)]
    struct HelloWorld {
        a: i64,
        b: i64,
    }
    #[derive(Debug, InterpSend, PartialEq, Copy, Clone)]
    struct HelloTuple(u8, i64);
    #[derive(Debug, InterpSend, PartialEq, Copy, Clone)]
    enum HelloEnum {
        // A,
        B(i64),
        E { _f: i64, _d: bool },
        G,
        F,
    }

    #[derive(Debug, InterpSend, PartialEq, Copy, Clone)]
    struct Nested {
        c: i64,
        d: HelloWorld,
        e: HelloTuple,
        f: HelloEnum,
        g: HelloEnum,
        h: HelloEnum,
        j: Option<i64>,
    }

    let pool = Box::leak(Box::<StringPool>::default());
    let mut p = Program::new(pool, TargetArch::Aarch64, TargetArch::Aarch64);
    let one = HelloWorld { a: 123, b: 345 };
    let two_b = one.serialize_to_ints_one();
    let three_b = HelloWorld::deserialize_from_ints(&mut two_b.into_iter()).unwrap();
    assert_eq!(one, three_b);

    let four = vec![one, one, HelloWorld { a: 678, b: 910 }, one];
    let five_b = four.clone().serialize_to_ints_one();
    let six_b = Vec::<HelloWorld>::deserialize_from_ints(&mut five_b.into_iter()).unwrap();
    assert_eq!(four, six_b);

    let seven = Nested {
        c: 314,
        d: one,
        e: HelloTuple(1, 2),
        f: HelloEnum::E { _f: 15, _d: true },
        g: HelloEnum::B(25),
        h: HelloEnum::G,
        j: Some(123),
    };
    let eight_b = seven.serialize_to_ints_one();
    let nine_b = Nested::deserialize_from_ints(&mut eight_b.into_iter()).unwrap();
    assert_eq!(seven, nine_b);

    assert_eq!(HelloWorld::get_type(&mut p), HelloWorld::get_type(&mut p));
    assert_ne!(HelloWorld::get_type(&mut p), Nested::get_type(&mut p));
}

pub mod c {
    use libffi::middle::{Arg, Type};

    use crate::compiler::Compile;
    use crate::err;
    use crate::{
        ast::{Program, TypeId, TypeInfo},
        compiler::Res,
    };

    type CTy = libffi::middle::Type;

    impl<'p> Program<'p> {
        pub fn as_c_type(&self, ty: TypeId) -> Res<'p, CTy> {
            Ok(match &self[ty] {
                TypeInfo::F64 => CTy::f64(),
                // TODO: actually different int types
                TypeInfo::OverloadSet | TypeInfo::Unit | TypeInfo::Int(_) | TypeInfo::Fn(_) | TypeInfo::Type | TypeInfo::Never => CTy::i64(),
                // Not a whole word!
                TypeInfo::Bool => CTy::c_uchar(),
                TypeInfo::FnPtr(_) | TypeInfo::VoidPtr | TypeInfo::Ptr(_) => CTy::pointer(),
                TypeInfo::Unique(ty, _) | TypeInfo::Named(ty, _) => self.as_c_type(*ty)?,
                TypeInfo::Tagged { .. } | TypeInfo::Tuple(_) | TypeInfo::Struct { .. } => {
                    err!("i use wrong c abi for aggragates {}", self.log_type(ty))
                }
                _ => err!("No c abi for {}", self.log_type(ty)),
            })
        }
    }

    // NOTE: pointers passed to Arg::new must live until into_cif
    pub fn call<'p>(program: &mut Compile<'_, 'p>, ptr: usize, mut f_ty: crate::ast::FnType, args: Vec<i64>, comp_ctx: bool) -> Res<'p, i64> {
        use libffi::middle::{Builder, CodePtr};
        let ptr = CodePtr::from_ptr(ptr as *const std::ffi::c_void);
        let mut b = Builder::new();

        if comp_ctx {
            b = b.arg(Type::pointer());
        }
        f_ty.arg = program.program.raw_type(f_ty.arg); // so it notises things are tuples and should get splatted into multiple args
        let mut args: Vec<_> = if f_ty.arg == TypeId::unit() {
            vec![]
        } else if let TypeInfo::Tuple(fields) = &program.program[f_ty.arg] {
            for ty in fields {
                b = b.arg(program.program.as_c_type(*ty)?);
            }
            args.iter().map(Arg::new).collect()
        } else {
            b = b.arg(program.program.as_c_type(f_ty.arg)?);
            args.iter().map(Arg::new).collect()
        };

        if f_ty.ret != TypeId::unit() {
            b = b.res(program.program.as_c_type(f_ty.ret)?)
        }
        if comp_ctx {
            // IMPORTANT: extra &indirection. We want a pointer to the argument, even if the argument is already a pointer.
            // Note: dont put it in a local first because release mode reuses the stack slot.
            args.insert(0, Arg::new(&program));
        }

        assert!(
            program.ready.sizes.slot_count(program.program, f_ty.ret) <= 1,
            "my c_call doesn't use correct struct calling convention yet"
        );
        let ret: i64 = unsafe { b.into_cif().call(ptr, &args) };
        Ok(ret)
    }
}
