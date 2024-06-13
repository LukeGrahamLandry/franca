use std::{cell::SyncUnsafeCell, fmt::Debug, hash::Hash, io::Write, marker::PhantomData, mem};

use crate::{
    ast::{Flag, TypeId},
    ffi::InterpSend,
    Map, MY_CONST_DATA,
};

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
struct Ptr<T: ?Sized>(*const T);

pub struct StringPool<'pool> {
    lookup: SyncUnsafeCell<Map<Ptr<str>, Ident<'pool>>>,
    values: SyncUnsafeCell<Vec<Ptr<str>>>,
    constants: SyncUnsafeCell<ConstantData>,
}

unsafe impl Send for StringPool<'_> {}
unsafe impl Sync for StringPool<'_> {}

impl<'pool> StringPool<'pool> {
    pub fn use_constants<T>(&self, f: impl FnOnce(&mut ConstantData) -> T) -> T {
        let v = unsafe { &mut *self.constants.get() };
        f(v)
    }

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
        let temp: *const str = s;
        let temp = temp as *mut str;

        let lookup = unsafe { &mut *self.lookup.get() };
        if let Some(i) = lookup.get(&Ptr(temp)) {
            return *i;
        }

        let consts = unsafe { &mut *self.constants.get() }; // we already have the lock.
        let alloc = Ptr(consts.push_str_zero_term(s));

        let values = unsafe { &mut *self.values.get() };
        let i = Ident(values.len() as u32, PhantomData);
        values.push(alloc);
        lookup.insert(alloc, i);
        i
    }

    pub fn get_c_str(&self, i: Ident<'pool>) -> *const u8 {
        let s = self.get(i);
        if unsafe { *(s.as_ptr().add(s.len())) == 0 } {
            s.as_ptr()
        } else {
            unreachable!()
        }
    }
}

use std::{
    mem::align_of,
    ptr::{null_mut, slice_from_raw_parts, slice_from_raw_parts_mut},
};

pub struct ConstantData {
    _first: *mut u8,
    prev_page: *mut u8,
    pub next: *mut u8,
}

const PAGE_SIZE: isize = 16384; // TODO: where does one ask the os for this

impl Default for ConstantData {
    fn default() -> Self {
        const SIZE: usize = 1 << 22; // we work at the virtual memory factory I feel
        let next = unsafe {
            libc::mmap(
                null_mut(),
                SIZE,
                libc::PROT_WRITE | libc::PROT_READ,
                libc::MAP_ANON | libc::MAP_PRIVATE,
                -1,
                0,
            )
        } as *mut u8;

        unsafe {
            MY_CONST_DATA = (next as usize, SIZE);
        }
        Self {
            next,
            _first: next,
            prev_page: next,
        }
    }
}

impl ConstantData {
    pub fn push_i64(&mut self, i: i64) {
        self.ensure_align::<i64>();
        self.push_bytes(&i.to_le_bytes());
    }

    pub fn push_ints(&mut self, i: &[i64]) -> *const [i64] {
        self.ensure_align::<i64>();
        let bytes = unsafe { &*slice_from_raw_parts(i.as_ptr() as *const u8, i.len() * 8) };
        let ptr = self.push_bytes(bytes).as_ptr();
        unsafe { &*slice_from_raw_parts(ptr as *const i64, i.len()) }
    }

    pub fn push_str_zero_term(&mut self, s: &str) -> *const str {
        let ptr = self.push_bytes(s.as_bytes()).as_ptr();
        self.push_bytes(&[0]);
        unsafe { std::str::from_utf8_unchecked(&*slice_from_raw_parts(ptr, s.len())) }
    }

    pub fn push_bytes(&mut self, s: &[u8]) -> *const [u8] {
        let ptr = self.next;
        unsafe {
            self.next = ptr.add(s.len());
            let dest = &mut *slice_from_raw_parts_mut(ptr, s.len());
            dest.copy_from_slice(s);
            slice_from_raw_parts(ptr, s.len())
        }
    }

    pub fn ensure_align<T>(&mut self) {
        unsafe {
            let n = self.next;
            self.next = n.add(n.align_offset(align_of::<T>()));
        }
    }

    pub fn adjust_writable(&mut self) {
        unsafe {
            let prev = self.prev_page;
            let bytes = self.next.offset_from(prev);
            if bytes < PAGE_SIZE {
                return;
            }
            let pages = bytes / PAGE_SIZE;
            if pages > 0 {
                let len = (pages * PAGE_SIZE) as usize;
                libc::mprotect(prev as *mut libc::c_void, len, libc::PROT_READ);
                self.prev_page = prev.add(len);
            }
        }
    }
}

impl Write for ConstantData {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.push_bytes(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.adjust_writable();
        Ok(())
    }
}

impl<'p> Default for StringPool<'p> {
    fn default() -> Self {
        let len = Flag::_Reserved_Count_ as usize;
        let this = Self {
            lookup: SyncUnsafeCell::new(Default::default()),
            values: SyncUnsafeCell::new(Vec::with_capacity(len)),
            constants: SyncUnsafeCell::new(ConstantData::default()),
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

impl<'p> InterpSend<'p> for Ident<'p> {
    fn get_type_key() -> u128 {
        // i dare you to change the generic to Self
        unsafe { std::mem::transmute(std::any::TypeId::of::<Ident>()) }
    }

    fn create_type(_: &mut crate::ast::Program) -> crate::ast::TypeId {
        TypeId::ident
    }

    fn get_or_create_type(_: &mut crate::ast::Program) -> crate::ast::TypeId {
        TypeId::ident
    }

    fn get_type(_: &crate::ast::Program) -> crate::ast::TypeId {
        TypeId::ident
    }

    fn name() -> String {
        "Symbol".to_string()
    }
}
