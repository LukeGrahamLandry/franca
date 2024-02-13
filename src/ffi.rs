use interp_derive::InterpSend;

use crate::{
    ast::{Program, TypeId, TypeInfo},
    bc::{InterpBox, Value},
    pool::StringPool,
};

// TODO
pub trait InterpSend<'p>: Sized {
    fn get_type(interp: &mut Program<'p>) -> TypeId;
    fn serialize(self) -> Value;
    fn deserialize(value: Value) -> Option<Self>;
}

#[derive(Debug, InterpSend, PartialEq, Copy, Clone)]
struct HelloWorld {
    a: i64,
    b: i64,
}

impl<'p> InterpSend<'p> for i64 {
    fn get_type(program: &mut Program<'p>) -> TypeId {
        program.intern_type(TypeInfo::I64)
    }

    fn serialize(self) -> Value {
        Value::I64(self)
    }

    fn deserialize(value: Value) -> Option<Self> {
        if let Value::I64(i) = value {
            Some(i)
        } else {
            None
        }
    }
}

impl<'p, T: InterpSend<'p>> InterpSend<'p> for Vec<T> {
    fn get_type(program: &mut Program<'p>) -> TypeId {
        program.intern_type(TypeInfo::I64)
    }

    fn serialize(self) -> Value {
        let count = self.len();
        let values: Vec<_> = self.into_iter().map(|e| e.serialize()).collect();
        let value = Box::new(InterpBox {
            references: 1,
            values,
        });
        Value::Heap {
            first: 0,
            count,
            value: Box::into_raw(value),
        }
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

#[test]
fn interp_send() {
    // let pool = Box::leak(Box::<StringPool>::default());
    // let mut p = Program::new(vec![], pool);
    let one = HelloWorld { a: 123, b: 345 };
    let two = one.serialize();
    let three = HelloWorld::deserialize(two).unwrap();
    assert_eq!(one, three);

    let four = vec![one, one, HelloWorld { a: 678, b: 910 }, one];
    let five = four.clone().serialize();
    let six = Vec::<HelloWorld>::deserialize(five).unwrap();
    assert_eq!(four, six);

    // let ty = HelloWorld::get_type(&mut p);
    // panic!("{}", p.log_type(ty));
}
