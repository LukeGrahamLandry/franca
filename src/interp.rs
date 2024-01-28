use crate::{
    ast::{Func, TypeId},
    pool::StringPool,
};

#[derive(Clone, PartialEq)]
pub enum Value {
    F64(f64),
    I64(i64),
    Bool(bool),
    Enum {
        container_type: TypeId,
        tag: usize,
        value: Box<Self>,
    },
    Tuple {
        container_type: TypeId,
        values: Vec<Self>,
    },
    Array {
        container_type: TypeId,
        values: Vec<Self>,
    },
    Ptr {
        container_type: TypeId,
        value: *mut Self,
    },
    // Both closures and types don't have values at runtime, all uses must be inlined.
    Fn(TypeId, usize),
    Type(TypeId),
}

pub struct Interp<'a, 'p> {
    pool: &'a StringPool<'p>,
}

impl<'a, 'p> Interp<'a, 'p> {
    pub fn new(pool: &'a StringPool<'p>) -> Self {
        Self { pool }
    }
}
