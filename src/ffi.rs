use std::collections::HashMap;

use codemap::Span;

use crate::{
    ast::{Program, TypeId, TypeInfo},
    bc::Value,
};

// TODO
pub trait InterpSend<'p>: Sized {
    fn get_type(interp: &mut Program<'p>) -> TypeId;
    fn serialize(self) -> Value;
    fn deserialize(value: Value) -> Option<Self>;
}

impl Value {
    // This is stupid but macros suck and its somehow really hard to add a turbofish so you can use assosiated functions.
    pub fn deserialize<'p, T: InterpSend<'p> + Sized>(self) -> Option<T> {
        T::deserialize(self)
    }
}

macro_rules! send_num {
    ($ty:tt) => {
        impl<'p> InterpSend<'p> for $ty {
            fn get_type(program: &mut Program<'p>) -> TypeId {
                program.intern_type(TypeInfo::I64)
            }

            fn serialize(self) -> Value {
                Value::I64(self as i64)
            }

            fn deserialize(value: Value) -> Option<Self> {
                if let Value::I64(i) = value {
                    Some(i as $ty)
                } else {
                    None
                }
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
    fn get_type(program: &mut Program<'p>) -> TypeId {
        program.intern_type(TypeInfo::Bool)
    }

    fn serialize(self) -> Value {
        Value::Bool(self)
    }

    fn deserialize(value: Value) -> Option<Self> {
        if let Value::Bool(i) = value {
            Some(i)
        } else {
            None
        }
    }
}

impl<'p, A: InterpSend<'p>, B: InterpSend<'p>> InterpSend<'p> for (A, B) {
    fn get_type(program: &mut Program<'p>) -> TypeId {
        let a = A::get_type(program);
        let b = B::get_type(program);
        program.tuple_of(vec![a, b])
    }

    fn serialize(self) -> Value {
        // TODO
        Value::Tuple {
            container_type: TypeId::any(),
            values: vec![self.0.serialize(), self.1.serialize()],
        }
    }

    fn deserialize(vvalue: Value) -> Option<Self> {
        if let Value::Tuple { values, .. } = vvalue {
            let mut values = values.into_iter();
            Some((
                A::deserialize(values.next()?)?,
                B::deserialize(values.next()?)?,
            ))
        } else {
            None
        }
    }
}

impl<'p, T: InterpSend<'p>> InterpSend<'p> for Vec<T> {
    fn get_type(program: &mut Program<'p>) -> TypeId {
        let ty = T::get_type(program);
        program.intern_type(TypeInfo::Ptr(ty))
    }

    fn serialize(self) -> Value {
        let values = self.into_iter().map(|e| e.serialize()).collect();
        Value::new_box(values)
    }

    fn deserialize(value: Value) -> Option<Self> {
        if let Value::Heap {
            value,
            first,
            count,
        } = value
        {
            let value = unsafe { &mut *value };
            if value.references <= 0 {
                return None;
            }
            if first == 0 && count == value.values.len() {
                value.references -= 1;
                // TODO: don't clone if we're last
            }

            value.values[first..first + count]
                .iter()
                .map(|v| T::deserialize(v.clone()))
                .collect()
        } else {
            None
        }
    }
}

impl<'p, T: InterpSend<'p>> InterpSend<'p> for Box<T> {
    fn get_type(program: &mut Program<'p>) -> TypeId {
        let ty = T::get_type(program);
        program.intern_type(TypeInfo::Ptr(ty))
    }

    fn serialize(self) -> Value {
        (*self).serialize()
    }

    fn deserialize(value: Value) -> Option<Self> {
        T::deserialize(value).map(Box::new)
    }
}

impl<'p> InterpSend<'p> for Value {
    fn get_type(_interp: &mut Program<'p>) -> TypeId {
        TypeId::any()
    }

    fn serialize(self) -> Value {
        self
    }

    fn deserialize(value: Value) -> Option<Self> {
        Some(value)
    }
}

// TODO: this should be an enum
impl<'p, T: InterpSend<'p>> InterpSend<'p> for Option<T> {
    fn get_type(interp: &mut Program<'p>) -> TypeId {
        let t = T::get_type(interp);
        interp.tuple_of(vec![TypeId::bool(), t])
    }

    fn serialize(self) -> Value {
        match self {
            Some(v) => Value::Tuple {
                container_type: TypeId::any(),
                values: vec![Value::Bool(false), v.serialize()],
            },
            None => Value::Tuple {
                container_type: TypeId::any(),
                values: vec![Value::Bool(false), Value::Unit],
            },
        }
    }

    fn deserialize(value: Value) -> Option<Self> {
        match value {
            Value::Tuple { values, .. } => {
                let mut values = values.into_iter();
                match values.next()? {
                    Value::Bool(true) => Some(T::deserialize(Value::Tuple {
                        container_type: TypeId::any(),
                        values: values.collect(),
                    })),
                    Value::Bool(false) => Some(None),
                    _ => None,
                }
            }
            _ => None,
        }
    }
}

impl<'p> InterpSend<'p> for Span {
    fn get_type(_interp: &mut Program<'p>) -> TypeId {
        todo!()
    }

    fn serialize(self) -> Value {
        todo!()
    }

    fn deserialize(_value: Value) -> Option<Self> {
        todo!()
    }
}

impl<'p, K: InterpSend<'p> + Eq + std::hash::Hash, V: InterpSend<'p>> InterpSend<'p>
    for HashMap<K, V>
{
    fn get_type(interp: &mut Program<'p>) -> TypeId {
        Vec::<(K, V)>::get_type(interp)
    }

    fn serialize(self) -> Value {
        self.into_iter().collect::<Vec<_>>().serialize()
    }

    fn deserialize(value: Value) -> Option<Self> {
        Some(
            Vec::<(K, V)>::deserialize(value)?
                .into_iter()
                .collect::<Self>(),
        )
    }
}

impl<'p> InterpSend<'p> for String {
    fn get_type(interp: &mut Program<'p>) -> TypeId {
        Vec::<u8>::get_type(interp)
    }

    fn serialize(self) -> Value {
        Vec::<u8>::from(self).serialize()
    }

    fn deserialize(value: Value) -> Option<Self> {
        Self::from_utf8(Vec::<u8>::deserialize(value)?).ok()
    }
}

#[test]
fn interp_send() {
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
    }

    let pool = Box::leak(Box::<StringPool>::default());
    let mut p = Program::new(vec![], pool);
    let one = HelloWorld { a: 123, b: 345 };
    let two = one.serialize();
    let three = HelloWorld::deserialize(two).unwrap();
    assert_eq!(one, three);

    let four = vec![one, one, HelloWorld { a: 678, b: 910 }, one];
    let five = four.clone().serialize();
    let six = Vec::<HelloWorld>::deserialize(five).unwrap();
    assert_eq!(four, six);

    let seven = Nested {
        c: 314,
        d: one,
        e: HelloTuple(1, 2),
        f: HelloEnum::E { _f: 15, _d: true },
        g: HelloEnum::B(25),
        h: HelloEnum::G,
    };
    let eight = seven.serialize();
    let nine = Nested::deserialize(eight).unwrap();
    assert_eq!(seven, nine);

    assert_eq!(HelloWorld::get_type(&mut p), HelloWorld::get_type(&mut p));
    assert_ne!(HelloWorld::get_type(&mut p), Nested::get_type(&mut p));

    assert!(HelloWorld::deserialize(seven.serialize()).is_none());
    assert!(Nested::deserialize(one.serialize()).is_none());

    // let ty = HelloWorld::get_type(&mut p);
    // panic!("{}", p.log_type(ty));
}

#[cfg(feature = "interp_c_ffi")]
pub mod c {
    use libc::c_void;
    use libffi::middle::Arg;

    use crate::{
        ast::{Program, TypeId, TypeInfo},
        bc::Value,
        compiler::Res,
        interp::to_flat_seq,
        logging::err,
    };
    type CTy = libffi::middle::Type;

    impl<'p> Program<'p> {
        pub fn as_c_type(&self, ty: TypeId) -> Res<'p, CTy> {
            Ok(match &self.types[ty.0] {
                TypeInfo::F64 => CTy::f64(),
                TypeInfo::I64 => CTy::i64(),
                TypeInfo::Bool => CTy::c_int(),
                TypeInfo::Tuple(_) => todo!(),
                TypeInfo::Ptr(_) => CTy::pointer(),
                TypeInfo::Slice(_) => CTy::structure([CTy::pointer(), CTy::i64()]),
                TypeInfo::Enum { .. } => todo!(),
                TypeInfo::Unique(ty, _)
                | TypeInfo::Named(ty, _)
                | TypeInfo::Struct { as_tuple: ty, .. } => self.as_c_type(*ty)?,
                TypeInfo::Unit => todo!(),
                _ => err!("No c abi for {}", self.log_type(ty)),
            })
        }
    }

    pub fn to_void_ptr(v: &Value) -> Arg {
        match v {
            Value::F64(v) => Arg::new(v),
            Value::I64(v) => Arg::new(v),
            Value::CFnPtr { ptr: v, .. } => Arg::new(v),
            _ => todo!("to_void_ptr {v:?}"),
        }
    }

    pub fn call<'p>(
        program: &mut Program<'p>,
        ptr: usize,
        f_ty: crate::ast::FnType,
        arg: Value,
    ) -> Res<'p, Value> {
        let args = to_flat_seq(arg);
        use libffi::middle::{Builder, CodePtr};
        let ptr = CodePtr::from_ptr(ptr as *const std::ffi::c_void);
        let mut b = Builder::new();
        let args: Vec<_> = if f_ty.arg == TypeId::unit() {
            vec![]
        } else {
            b = b.arg(program.as_c_type(f_ty.arg)?);
            args.iter().map(crate::ffi::c::to_void_ptr).collect()
        };

        if f_ty.ret != TypeId::unit() {
            b = b.res(program.as_c_type(f_ty.ret)?)
        }

        Ok(if f_ty.ret == TypeId::unit() {
            unsafe { b.into_cif().call::<c_void>(ptr, &args) };
            Value::Unit
        } else if f_ty.ret == TypeId::i64() {
            // TODO: other return types. probably want to use the low interface so can get a void ptr and do a match on ret type to read it.
            let result: i64 = unsafe { b.into_cif().call(ptr, &args) };
            Value::I64(result)
        } else {
            todo!("unsupported c ret type {}", program.log_type(f_ty.ret))
        })
    }
}
