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
    struct Nested {
        c: i64,
        d: HelloWorld,
        e: HelloTuple,
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
