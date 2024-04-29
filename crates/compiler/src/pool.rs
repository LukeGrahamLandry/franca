use std::{
    cell::{SyncUnsafeCell, UnsafeCell},
    collections::HashMap,
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
    mem,
    sync::{
        atomic::{AtomicBool, Ordering},
        RwLock,
    },
    thread::yield_now,
};

use crate::{ast::Flag, bc::Value, ffi::InterpSend};

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Ident<'pool>(pub u32, pub PhantomData<&'pool str>);

impl Flag {
    pub const fn ident<'p>(self) -> Ident<'p> {
        Ident(self as u32, PhantomData)
    }
}

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

// type Map<'pool> = HashMap<Ptr<str>, Ident<'pool>, gxhash::GxBuildHasher>;
type Map<'pool> = HashMap<Ptr<str>, Ident<'pool>>;

pub struct StringPool<'pool> {
    lookup: SyncUnsafeCell<Map<'pool>>,
    values: SyncUnsafeCell<Vec<Ptr<str>>>,
    lock: AtomicBool,
}

unsafe impl Send for StringPool<'_> {}
unsafe impl Sync for StringPool<'_> {}

pub fn locked<T>(lock: &AtomicBool, f: impl FnOnce() -> T) -> T {
    loop {
        let res = lock.compare_exchange_weak(false, true, Ordering::AcqRel, Ordering::Acquire);
        if res.is_ok() {
            break;
        }
        yield_now();
    }
    let res = f();

    lock.store(false, Ordering::Release);
    res
}

impl<'pool> StringPool<'pool> {
    pub fn get(&self, i: Ident) -> &'pool str {
        let v = unsafe { &*self.values.get() };
        // # Safety
        // Strings are not removed from the pool until its dropped.
        unsafe { &*(v.get(i.0 as usize).expect("Valid Ident").0) }
    }

    pub fn upcast(&self, i: u32) -> Option<Ident<'pool>> {
        let values = unsafe { &*self.values.get() };
        if i > 0 && (i as usize) < values.len() {
            Some(Ident(i, PhantomData))
        } else {
            None
        }
    }

    pub fn intern(&self, s: &str) -> Ident<'pool> {
        locked(&self.lock, || {
            let temp: *const str = s;
            let temp = temp as *mut str;

            // Tempting to just take read() here as a fast path but its important for thread satefy that someone else
            // doesn't get through that check while we're allocating the box below.
            let lookup = unsafe { &mut *self.lookup.get() };
            if let Some(i) = lookup.get(&Ptr(temp)) {
                return *i;
            }

            let mut alloc = s.to_owned().into_bytes();
            alloc.push(0); // might as well make it a c string since have to reallocate anyway

            let alloc: Box<[u8]> = alloc.into_boxed_slice();
            let alloc = Box::into_raw(alloc);
            let alloc = Ptr(unsafe { &mut (*alloc)[..alloc.len() - 1] } as *mut [u8] as *mut str);

            // Delay taking this lock as long as possible to not block calls to get().
            let values = unsafe { &mut *self.values.get() };
            let i = Ident(values.len() as u32, PhantomData);
            values.push(alloc);
            lookup.insert(alloc, i);
            i
        })
    }

    pub fn get_c_str(&self, i: Ident<'pool>) -> *const u8 {
        let s = self.get(i);
        if unsafe { *(s.as_ptr().add(s.len())) == 0 } {
            s.as_ptr()
        } else {
            unreachable!()
            // let mut alloc = s.to_owned().into_bytes();
            // alloc.push(0); // might as well make it a c string since have to reallocate anyway

            // let alloc: Box<[u8]> = alloc.into_boxed_slice();
            // let alloc = Box::into_raw(alloc);
            // let alloc = Ptr(unsafe { &mut (*alloc)[..alloc.len() - 1] } as *mut [u8] as *mut str);
            // self.values.write().unwrap()[i.0 as usize] = alloc;
            // alloc.0.to_raw_parts().0 as *const u8
        }
    }
}

impl Drop for StringPool<'_> {
    fn drop(&mut self) {
        // now i borrow TODO: tracked owned ones seperatly
        // let mut v = self.values.write().unwrap();
        // for s in v.drain(0..) {
        //     // # Safety
        //     // Drop can only be called once.
        //     unsafe {
        //         let s = s.0 as *mut [u8];
        //         let s = core::ptr::slice_from_raw_parts_mut(s.as_mut_ptr(), s.len() + 1); // it was a c string

        //         drop(Box::from_raw(s));
        //     }
        // }
    }
}

impl<'p> Default for StringPool<'p> {
    fn default() -> Self {
        let len = Flag::_Reserved_Count_ as usize;
        let this = Self {
            lookup: SyncUnsafeCell::new(Map::with_capacity(len)),
            values: SyncUnsafeCell::new(Vec::with_capacity(len)),
            lock: AtomicBool::new(false),
        };
        for i in 0..len {
            let flag: Flag = unsafe { mem::transmute(i as u8) };
            let mut name = format!("{flag:?}");
            debug_assert!(!name.contains("::") && !name.contains("Flag") && !name.contains('{')); // TODO: debug formating is unspecified but happens to be what I want
            name.make_ascii_lowercase();
            let name = this.intern(&name);
            assert_eq!(i as u32, name.0);
        }
        this
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

    fn create_type(_: &mut crate::ast::Program<'p>) -> crate::ast::TypeId {
        crate::ast::TypeId::i64() // TODO: have a unique Symbol type
    }

    fn serialize(self, values: &mut Vec<Value>) {
        self.0.serialize(values)
    }

    fn serialize_to_ints(self, values: &mut Vec<i64>) {
        self.0.serialize_to_ints(values)
    }

    fn size() -> usize {
        1
    }

    fn deserialize_from_ints(values: &mut impl Iterator<Item = i64>) -> Option<Self> {
        let i = values.next()?;
        if i < 0 {
            return None;
        }

        Some(Ident(i as u32, PhantomData))
    }
}
