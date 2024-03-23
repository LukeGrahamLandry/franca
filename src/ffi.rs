use std::{collections::HashMap, mem};

use codemap::Span;

use crate::{
    ast::{Program, TypeId, TypeInfo},
    bc::{Value, Values},
    logging::{outln, LogTag::ShowErr},
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
    /// This should only be called once! Use get_type which caches it.
    fn create_type(interp: &mut Program<'p>) -> TypeId;
    fn serialize(self, values: &mut Vec<Value>);
    fn serialize_one(self) -> Values {
        let mut values = vec![];
        self.serialize(&mut values);
        debug_assert_eq!(values.len(), Self::size());
        values.into()
    }
    fn deserialize(values: &mut impl Iterator<Item = Value>) -> Option<Self>;
    fn deserialize_one(value: Values) -> Option<Self> {
        let value: Vec<_> = value.into();
        debug_assert_eq!(value.len(), Self::size(), "{value:?}");
        Self::deserialize(&mut value.into_iter())
    }

    fn size() -> usize;
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

impl Value {
    pub fn deserialize_from<'p, T: InterpSend<'p> + Sized>(values: &mut impl Iterator<Item = Value>) -> Option<T> {
        T::deserialize(values)
    }
}

impl Values {
    pub fn deserialize<'p, T: InterpSend<'p> + Sized>(self) -> Option<T> {
        let values = self.vec(); // TODO: no alloc for one
        debug_assert_eq!(values.len(), T::size());
        T::deserialize(&mut values.into_iter()) // TODO: deeper flatten?
    }
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

            fn serialize(self, values: &mut Vec<Value>) {
                values.push(Value::I64(self as i64))
            }

            fn deserialize(values: &mut impl Iterator<Item = Value>) -> Option<Self> {
                if let Value::I64(i) = values.next()? {
                    Some(i as $ty)
                } else {
                    None
                }
            }

            fn size() -> usize {
                1
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
    fn create_type(program: &mut Program<'p>) -> TypeId {
        program.intern_type(TypeInfo::Bool)
    }

    fn serialize(self, values: &mut Vec<Value>) {
        values.push(Value::Bool(self))
    }

    fn deserialize(values: &mut impl Iterator<Item = Value>) -> Option<Self> {
        if let Value::Bool(i) = values.next()? {
            Some(i)
        } else {
            None
        }
    }

    fn size() -> usize {
        1
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

    fn serialize(self, values: &mut Vec<Value>) {
        self.0.serialize(values);
        self.1.serialize(values);
    }

    fn deserialize(values: &mut impl Iterator<Item = Value>) -> Option<Self> {
        Some((A::deserialize(values)?, B::deserialize(values)?))
    }

    fn size() -> usize {
        A::size() + B::size()
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

    fn serialize(self, values: &mut Vec<Value>) {
        let len = self.len();
        let mut parts = vec![];
        for e in self {
            e.serialize(&mut parts);
        }
        debug_assert_eq!(parts.len(), T::size() * len);
        values.push(Value::new_box(parts, false));
        values.push(Value::I64(len as i64));
    }

    fn deserialize(values: &mut impl Iterator<Item = Value>) -> Option<Self> {
        let ptr = values.next()?;
        let len = usize::deserialize(values)?;
        if let Value::Heap {
            value,
            physical_first: first,
            physical_count: count,
            ..
        } = ptr
        {
            debug_assert_eq!(len * T::size(), count);
            let value = unsafe { &mut *value };
            if value.references <= 0 {
                outln!(ShowErr, "deserialize: references < 1");
                return None;
            }

            let mut values = value.values[first..(first + count)].iter().copied();
            let mut res = vec![];
            for _ in 0..len {
                res.push(T::deserialize(&mut values)?);
            }
            debug_assert!(values.next().is_none());
            Some(res)
        } else {
            None
        }
    }

    fn size() -> usize {
        2
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

    fn serialize(self, values: &mut Vec<Value>) {
        let mut parts = vec![];
        let inner: T = *self;
        inner.serialize(&mut parts);
        debug_assert_eq!(parts.len(), T::size());
        values.push(Value::new_box(parts, false))
    }

    fn deserialize(values_in: &mut impl Iterator<Item = Value>) -> Option<Self> {
        if let Value::Heap {
            value,
            physical_first: first,
            physical_count: count,
            ..
        } = values_in.next()?
        {
            debug_assert_eq!(count, T::size(), "box must contain one element");
            let value = unsafe { &mut *value };
            if value.references <= 0 {
                outln!(ShowErr, "deserialize: references < 1");
                return None;
            }

            let mut values = value.values[first..(first + count)].iter().copied();
            let res = Box::new(T::deserialize(&mut values)?);
            debug_assert!(values.next().is_none());
            Some(res)
        } else {
            None
        }
    }

    fn size() -> usize {
        1
    }
}

impl<'p> InterpSend<'p> for Value {
    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }

    fn create_type(_interp: &mut Program<'p>) -> TypeId {
        TypeId::any()
    }

    fn serialize(self, values: &mut Vec<Value>) {
        values.push(self)
    }

    fn deserialize(values: &mut impl Iterator<Item = Value>) -> Option<Self> {
        values.next()
    }

    fn size() -> usize {
        1
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

    fn serialize(self, values: &mut Vec<Value>) {
        match self {
            Some(v) => {
                values.push(Value::I64(0));
                v.serialize(values);
            }
            None => {
                values.push(Value::I64(1));
                for _ in 0..T::size() {
                    values.push(Value::Unit);
                }
            }
        }
    }

    fn deserialize(values: &mut impl Iterator<Item = Value>) -> Option<Self> {
        match values.next()? {
            Value::I64(0) => Some(T::deserialize(values)),
            Value::I64(1) => {
                for _ in 0..T::size() {
                    let unit = values.next()?;
                    debug_assert_eq!(unit, Value::Unit);
                }
                Some(None)
            }
            _ => None,
        }
    }

    fn size() -> usize {
        1 + T::size()
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
    fn serialize(self, values: &mut Vec<Value>) {
        let (a, b): (u32, u32) = unsafe { mem::transmute(self) };
        (a, b).serialize(values)
    }

    fn deserialize(values: &mut impl Iterator<Item = Value>) -> Option<Self> {
        let (a, b) = <(u32, u32)>::deserialize(values)?;
        let res: Span = unsafe { mem::transmute((a, b)) };
        Some(res)
    }

    fn size() -> usize {
        <(u32, u32)>::size()
    }
}

impl<'p, K: InterpSend<'p> + Eq + std::hash::Hash, V: InterpSend<'p>> InterpSend<'p> for HashMap<K, V> {
    fn get_type_key() -> u128 {
        mix::<K, V>(1234567890)
    }

    fn create_type(interp: &mut Program<'p>) -> TypeId {
        Vec::<(K, V)>::get_type(interp)
    }

    fn serialize(self, values: &mut Vec<Value>) {
        self.into_iter().collect::<Vec<_>>().serialize(values)
    }

    fn deserialize(values: &mut impl Iterator<Item = Value>) -> Option<Self> {
        Some(Vec::<(K, V)>::deserialize(values)?.into_iter().collect::<Self>())
    }

    fn size() -> usize {
        Vec::<(K, V)>::size()
    }
}

impl<'p> InterpSend<'p> for String {
    fn get_type_key() -> u128 {
        unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
    }

    fn create_type(interp: &mut Program<'p>) -> TypeId {
        Vec::<u8>::get_type(interp)
    }

    fn serialize(self, values: &mut Vec<Value>) {
        Vec::<u8>::from(self).serialize(values)
    }

    fn deserialize(values: &mut impl Iterator<Item = Value>) -> Option<Self> {
        Self::from_utf8(Vec::<u8>::deserialize(values)?).ok()
    }

    fn size() -> usize {
        Vec::<u8>::size()
    }
}

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
    let mut p = Program::new(vec![], pool, TargetArch::Interp, TargetArch::Interp);
    let one = HelloWorld { a: 123, b: 345 };
    let two = one.serialize_one();
    let three = HelloWorld::deserialize_one(two).unwrap();
    assert_eq!(one, three);

    let four = vec![one, one, HelloWorld { a: 678, b: 910 }, one];
    let five = four.clone().serialize_one();
    let six = Vec::<HelloWorld>::deserialize_one(five).unwrap();
    assert_eq!(four, six);

    let seven = Nested {
        c: 314,
        d: one,
        e: HelloTuple(1, 2),
        f: HelloEnum::E { _f: 15, _d: true },
        g: HelloEnum::B(25),
        h: HelloEnum::G,
        j: Some(123),
    };
    let eight = seven.serialize_one();
    let nine = Nested::deserialize_one(eight).unwrap();
    assert_eq!(seven, nine);

    assert_eq!(HelloWorld::get_type(&mut p), HelloWorld::get_type(&mut p));
    assert_ne!(HelloWorld::get_type(&mut p), Nested::get_type(&mut p));

    // assert!(HelloWorld::deserialize_one(seven.serialize_one()).is_none());
    // assert!(Nested::deserialize_one(one.serialize_one()).is_none());

    // let ty = HelloWorld::get_type(&mut p);
    // panic!("{}", p.log_type(ty));
}

#[test]
fn interp_send_empty_ast() {
    use crate::ast::{garbage_loc, FatStmt};
    let empty = FatStmt::null(garbage_loc());
    let prev = format!("{empty:?}");
    let value = empty.serialize_one();
    let empty2: FatStmt = value.deserialize().unwrap();
    assert_eq!(prev, format!("{empty2:?}"));
}

#[test]
fn interp_send_libs_ast() {
    use crate::pool::StringPool;
    use crate::{ast::FatStmt, parse::Parser};
    use codemap::CodeMap;

    let pool = Box::leak(Box::<StringPool>::default());
    let mut codemap = CodeMap::new();

    let file = codemap.add_file("bootstrap".to_string(), "#include_std(\"core.fr\");".to_string());

    let stmts = Parser::parse(&mut codemap, file.clone(), pool).unwrap();
    for s in stmts {
        let prev = format!("{s:?}");
        let value = s.serialize_one();
        let after: FatStmt = value.deserialize().unwrap();
        // Note: this relies on you not printing out addresses in there.
        assert_eq!(prev, format!("{after:?}"));
    }
}

#[cfg(feature = "interp_c_ffi")]
pub mod c {
    use libc::c_void;
    use libffi::middle::{Arg, Type};

    use crate::ast::IntType;
    use crate::ffi::InterpSend;
    use crate::pool::Ident;
    use crate::{
        ast::{Program, TypeId, TypeInfo},
        bc::{Value, Values},
        compiler::Res,
        logging::err,
    };

    type CTy = libffi::middle::Type;

    impl<'p> Program<'p> {
        pub fn as_c_type(&self, ty: TypeId) -> Res<'p, CTy> {
            Ok(match &self.types[ty.0] {
                TypeInfo::F64 => CTy::f64(),
                TypeInfo::Type | TypeInfo::Int(_) => CTy::i64(), // TODO: actually different int types
                TypeInfo::Bool => CTy::c_uchar(),                // Not a whole word!
                TypeInfo::Tuple(_) => {
                    todo!()
                }
                TypeInfo::VoidPtr | TypeInfo::Ptr(_) => CTy::pointer(),
                TypeInfo::Enum { .. } => todo!(),
                TypeInfo::Unique(ty, _) | TypeInfo::Named(ty, _) | TypeInfo::Struct { as_tuple: ty, .. } => self.as_c_type(*ty)?,
                TypeInfo::Unit => todo!(),
                // This is the return type of exit().
                TypeInfo::Never => CTy::usize(),
                _ => err!("No c abi for {}", self.log_type(ty)),
            })
        }
    }

    pub fn to_void_ptr(v: &Value) -> Arg {
        match v {
            Value::F64(v) => Arg::new(v),
            Value::I64(v) => Arg::new(v),
            Value::Bool(v) => Arg::new(v),
            Value::Symbol(v) | Value::Type(TypeId(v)) => Arg::new(v),
            _ => todo!("to_void_ptr {v:?}"),
        }
    }

    pub fn call<'p>(program: &Program<'p>, ptr: usize, f_ty: crate::ast::FnType, arg: Values, comp_ctx: bool) -> Res<'p, Values> {
        let args: Vec<Value> = arg.into();
        use libffi::middle::{Builder, CodePtr};
        let ptr = CodePtr::from_ptr(ptr as *const std::ffi::c_void);
        let mut b = Builder::new();

        if comp_ctx {
            b = b.arg(Type::pointer());
        }
        let mut args: Vec<_> = if f_ty.arg == TypeId::unit() {
            vec![]
        } else if let TypeInfo::Tuple(fields) = &program[f_ty.arg] {
            for ty in fields {
                b = b.arg(program.as_c_type(*ty)?);
            }
            args.iter().map(crate::ffi::c::to_void_ptr).collect()
        } else {
            b = b.arg(program.as_c_type(f_ty.arg)?);
            args.iter().map(crate::ffi::c::to_void_ptr).collect()
        };

        if f_ty.ret != TypeId::unit() {
            b = b.res(program.as_c_type(f_ty.ret)?)
        }
        let int32 = program.find_interned(TypeInfo::Int(IntType { bit_count: 32, signed: true }));
        let sym = *program.ffi_types.get(&Ident::get_type_key()).unwrap();

        if comp_ctx {
            // IMPORTANT: extra &indirection. We want a pointer to the argument, even if the argument is already a pointer.
            args.insert(0, Arg::new(&program));
        }

        // TODO: this is getting deranged.
        Ok(if f_ty.ret == TypeId::unit() {
            unsafe { b.into_cif().call::<c_void>(ptr, &args) };
            Value::Unit.into()
        } else if f_ty.ret == TypeId::ty() {
            let result: usize = unsafe { b.into_cif().call(ptr, &args) };
            Value::Type(TypeId(result)).into()
        } else if f_ty.ret == TypeId::i64() || f_ty.ret == int32 {
            // TODO: other return types. probably want to use the low interface so can get a void ptr and do a match on ret type to read it.
            let result: i64 = unsafe { b.into_cif().call(ptr, &args) };
            Value::I64(result).into()
        } else if f_ty.ret == TypeId::bool() {
            // TODO: other return types. probably want to use the low interface so can get a void ptr and do a match on ret type to read it.
            let result: i64 = unsafe { b.into_cif().call(ptr, &args) };
            Value::Bool(result != 0).into()
        } else if f_ty.ret == sym {
            let result: usize = unsafe { b.into_cif().call(ptr, &args) };
            Value::Symbol(result).into()
        } else if f_ty.ret.is_never() {
            let _: () = unsafe { b.into_cif().call(ptr, &args) };
            unreachable!("Called 'fn(_) Never' but it returned.")
        } else {
            todo!("unsupported c ret type {}", program.log_type(f_ty.ret))
        })
    }
}
