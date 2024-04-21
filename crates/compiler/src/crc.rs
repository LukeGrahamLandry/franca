use crate::ast::{Program, TypeId};
use crate::bc::Value;
use crate::ffi::InterpSend;
use std::fmt::{Debug, Formatter};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

pub struct CRc<T: Clone> {
    inner: Rc<T>,
}

impl<T: Clone> CRc<T> {
    pub fn new(t: T) -> Self {
        Self { inner: Rc::new(t) }
    }
}

impl<T: Clone> Deref for CRc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner.deref()
    }
}

impl<T: Clone> DerefMut for CRc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // This is a dumb way to write this but lifetimes don't work if you try to use match?
        if Rc::get_mut(&mut self.inner).is_none() {
            let value = self.inner.deref().clone();
            self.inner = Rc::new(value);
            Rc::get_mut(&mut self.inner).unwrap();
        }
        Rc::get_mut(&mut self.inner).unwrap()
    }
}

impl<T: Clone> Clone for CRc<T> {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}

impl<'p, T: Clone + InterpSend<'p>> InterpSend<'p> for CRc<T> {
    fn get_type_key() -> u128 {
        T::get_type_key()
    }

    fn create_type(interp: &mut Program<'p>) -> TypeId {
        T::create_type(interp)
    }

    fn serialize(self, values: &mut Vec<Value>) {
        self.inner.deref().clone().serialize(values)
    }

    fn serialize_to_ints(self, values: &mut Vec<i64>) {
        self.inner.deref().clone().serialize_to_ints(values)
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
        T::deserialize_from_ints(values).map(|t| Self::new(t))
    }

    fn size() -> usize {
        T::size()
    }
}

impl<T: Debug + Clone> Debug for CRc<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.inner.deref().fmt(f)
    }
}

impl<T: Clone + Default> Default for CRc<T> {
    fn default() -> Self {
        Self::new(T::default())
    }
}
