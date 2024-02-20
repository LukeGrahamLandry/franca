use std::{collections::HashMap, fmt::Debug, hash::Hash, marker::PhantomData, sync::RwLock};

use crate::{bc::Value, ffi::InterpSend};

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Ident<'pool>(pub usize, PhantomData<&'pool str>);

impl<'p> Ident<'p> {
    pub fn null() -> Ident<'p> {
        Ident(0, PhantomData)
    }
}

impl Debug for Ident<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "S{}", self.0)
    }
}

/// A raw pointer that uses the reference's Hash/PartialEq implementations.
struct Ptr<T: ?Sized>(*mut T);

#[derive(Default)]
pub struct StringPool<'pool> {
    lookup: RwLock<HashMap<Ptr<str>, Ident<'pool>>>,
    values: RwLock<Vec<Ptr<str>>>,
}

// There is one thread i do not care.
unsafe impl Send for StringPool<'_> {}
unsafe impl Sync for StringPool<'_> {}

impl<'pool> StringPool<'pool> {
    pub fn get(&self, i: Ident) -> &'pool str {
        let v = self.values.read().unwrap();
        // # Safety
        // Strings are not removed from the pool until its dropped.
        unsafe { &*(v.get(i.0).expect("Valid Ident").0) }
    }

    pub fn upcast(&self, i: i64) -> Option<Ident<'pool>> {
        if i > 0 && (i as usize) < self.values.read().unwrap().len() {
            Some(Ident(i as usize, PhantomData))
        } else {
            None
        }
    }

    pub fn intern(&self, s: &str) -> Ident<'pool> {
        let temp: *const str = s;
        let temp = temp as *mut str;

        // Tempting to just take read() here as a fast path but its important for thread satefy that someone else
        // doesn't get through that check while we're allocating the box below.
        let mut lookup = self.lookup.write().unwrap();
        if let Some(i) = lookup.get(&Ptr(temp)) {
            return *i;
        }

        let alloc = s.to_owned();
        let alloc: Box<str> = Box::from(alloc);
        let alloc = Ptr(Box::into_raw(alloc));

        // Delay taking this lock as long as possible to not block calls to get().
        let mut values = self.values.write().unwrap();
        let i = Ident(values.len(), PhantomData);
        values.push(alloc);
        lookup.insert(alloc, i);
        i
    }
}

impl Drop for StringPool<'_> {
    fn drop(&mut self) {
        let mut v = self.values.write().unwrap();
        for s in v.drain(0..) {
            // # Safety
            // Drop can only be called once.
            unsafe {
                drop(Box::from_raw(s.0));
            }
        }
    }
}

impl<T: Hash + ?Sized> Hash for Ptr<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // # Safety
        // Private. Inforced by lifetimes.
        let s = unsafe { &*self.0 };
        s.hash(state)
    }
}

impl<T: PartialEq + ?Sized> PartialEq for Ptr<T> {
    fn eq(&self, other: &Self) -> bool {
        // # Safety
        // Private. Inforced by lifetimes.
        let s1 = unsafe { &*self.0 };
        let s2 = unsafe { &*other.0 };
        s1 == s2
    }
}

impl<T: Eq + ?Sized> Eq for Ptr<T> {}

impl<T: ?Sized> Clone for Ptr<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: ?Sized> Copy for Ptr<T> {}

#[test]
fn string_pool() {
    let pool = StringPool::default();
    let hello = pool.intern("hello");
    let hello2 = pool.intern("hello");
    let goodbye = pool.intern("goodbye");

    assert_eq!(hello, hello2);
    assert_ne!(hello, goodbye);

    assert_eq!("hello", pool.get(hello));
    assert_eq!("hello", pool.get(hello2));
    assert_eq!("goodbye", pool.get(goodbye));
}

impl<'p> InterpSend<'p> for Ident<'p> {
    fn get_type_key() -> u128 {
        // i dare you to change the generic to Self
        unsafe { std::mem::transmute(std::any::TypeId::of::<Ident>()) }
    }

    fn create_type(interp: &mut crate::ast::Program<'p>) -> crate::ast::TypeId {
        interp.intern_type(crate::ast::TypeInfo::I64) // TODO: have a unique Symbol type
    }

    fn serialize(self, values: &mut Vec<Value>) {
        self.0.serialize(values)
    }

    fn deserialize(values: &mut impl Iterator<Item = crate::bc::Value>) -> Option<Self> {
        Some(Ident(usize::deserialize(values)?, PhantomData))
    }
}
