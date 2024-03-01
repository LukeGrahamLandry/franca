use core::slice;
use std::fmt::Write;
use std::{
    alloc::Allocator,
    cell::UnsafeCell,
    marker::PhantomData,
    mem::{self, align_of, size_of, ManuallyDrop, MaybeUninit},
    ptr::NonNull,
};

pub struct MemoryGeorgWhoOwnsAllTheMemory {
    arenas: Vec<&'static Arena<'static>>,
}

#[repr(C)]
pub struct Arena<'p> {
    chunk: UnsafeCell<ArenaChunk>,
    _p: PhantomData<&'p ()>,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct ArenaChunk {
    start: *mut u8,
    current: *mut u8,
    end: *mut u8,
}

impl<'p> Default for Arena<'p> {
    fn default() -> Self {
        new_arena(4096)
    }
}

impl<'p> Arena<'p> {
    #[allow(clippy::mut_from_ref)]
    pub fn make<T>(&'p self, value: T) -> &'p mut T {
        let ptr = arena_alloc(self, size_of::<T>(), 1, align_of::<T>());
        let ptr = ptr as *mut T;
        unsafe {
            *ptr = value;
            &mut *ptr
        }
    }

    #[allow(clippy::mut_from_ref)]
    pub fn makes<T>(&'p self, count: usize) -> &'p mut [MaybeUninit<T>] {
        let ptr = arena_alloc(self, size_of::<T>(), count, align_of::<T>());
        let ptr = ptr as *mut MaybeUninit<T>;
        unsafe { slice::from_raw_parts_mut(ptr, count) }
    }

    pub fn log_sizes(&self) -> String {
        let mut out = String::new();
        unsafe {
            let chunk = self.chunk.get();
            let mut prev = *chunk;
            while !prev.start.is_null() {
                let capacity = prev.end.sub(prev.start as usize) as usize;
                write!(out, "{capacity}, ").unwrap();
                prev = *prev_chunk(prev);
            }
        }
        out
    }
}

// These are pub so cbindgen can find them, not because other rust code should call them.
// TODO: hoist the unsafe.

#[no_mangle]
pub extern "C" fn new_arena<'p>(initial_capacity: usize) -> Arena<'p> {
    let arena = Arena {
        chunk: UnsafeCell::new(ArenaChunk::new(initial_capacity)),
        _p: PhantomData,
    };
    let prev = arena_alloc(&arena, size_of::<ArenaChunk>(), 1, align_of::<ArenaChunk>());
    unsafe {
        debug_assert_eq!(prev, prev_chunk(*arena.chunk.get()) as *mut u8);
    }

    unsafe {
        *(prev as *mut ArenaChunk) = mem::zeroed();
    }
    arena
}

#[no_mangle]
pub extern "C" fn arena_alloc(arena: &Arena, stride: usize, count: usize, align: usize) -> *mut u8 {
    unsafe {
        let chunk = &mut *arena.chunk.get();
        let offset = chunk.current as usize % align;
        if chunk.current as usize + offset + stride * count > chunk.end as usize {
            let size = (chunk.end as usize - chunk.start as usize).max(count * stride) * 2;
            let old = *chunk;
            let new = ArenaChunk::new(size);
            *chunk = new;
            let prev = arena_alloc(arena, size_of::<ArenaChunk>(), 1, align_of::<ArenaChunk>());
            debug_assert_eq!(prev, prev_chunk(*chunk) as *mut u8);
            *(prev as *mut ArenaChunk) = old;
            return arena_alloc(arena, stride, count, align);
        }
        chunk.current = chunk.current.add(offset);
        let ptr = chunk.current;
        chunk.current = chunk.current.add(count * stride);
        ptr
    }
}

fn prev_chunk(chunk: ArenaChunk) -> *mut ArenaChunk {
    unsafe {
        chunk
            .start
            .add(chunk.start as usize % align_of::<ArenaChunk>()) as *mut ArenaChunk
    }
}

#[no_mangle]
pub extern "C" fn free_all(mut arena: Arena) {
    unsafe {
        let chunk = arena.chunk.get_mut();
        let mut prev = *chunk;
        while !prev.start.is_null() {
            let start = prev.start;
            let capacity = prev.end.sub(start as usize) as usize;
            prev = *prev_chunk(prev);
            drop(Vec::from_raw_parts(start, 0, capacity));
        }
    }
    *arena.chunk.get_mut() = unsafe { mem::zeroed() };
}

// The idea is this could be useful for debugging to make sure you're not holding something accross a reset.
// I figure all ones is less likely to run a fowl of niche optimisation than all zeros.
// It just needs to be something that will fail loudly if you try to use it for anything.
// Tho really the least and most signifigant bits aren't safe either if someone's doing fancy pointer tagging stuff.
// But you can't win 'em all so here we are.
#[no_mangle]
pub extern "C" fn set_ones_all(arena: &Arena) {
    unsafe {
        let chunk = arena.chunk.get();
        let mut prev = *chunk;

        while !prev.start.is_null() {
            // Carefully don't corrupt the prev block.
            let start = prev_chunk(prev).add(1) as *mut u8;
            let capacity = prev.end.sub(start as usize) as usize;
            prev = *prev_chunk(prev);
            let bytes = slice::from_raw_parts_mut(start, capacity);
            bytes.fill(0xFF);
        }
    }
}

impl ArenaChunk {
    fn new(initial_capacity: usize) -> ArenaChunk {
        let mut bytes = ManuallyDrop::new(Vec::<u8>::with_capacity(initial_capacity));
        ArenaChunk {
            start: bytes.as_mut_ptr(),
            current: bytes.as_mut_ptr(),
            end: unsafe { bytes.as_mut_ptr().add(initial_capacity) },
        }
    }
}

impl MemoryGeorgWhoOwnsAllTheMemory {
    pub fn pop(&mut self) -> &'static Arena<'static> {
        self.arenas
            .pop()
            .unwrap_or_else(|| Box::leak(Box::default()))
    }

    fn _push(&mut self, arena: &'static Arena<'static>) {
        self.arenas.push(arena);
    }
}

unsafe impl<'p> Allocator for &'p Arena<'p> {
    fn allocate(
        &self,
        layout: std::alloc::Layout,
    ) -> Result<NonNull<[u8]>, std::alloc::AllocError> {
        let ptr = arena_alloc(self, layout.size(), 1, layout.align());
        Ok(NonNull::slice_from_raw_parts(
            NonNull::new(ptr).unwrap(),
            layout.size(),
        ))
    }

    unsafe fn deallocate(&self, _: NonNull<u8>, _: std::alloc::Layout) {
        // no-op
    }
}

#[test]
fn self_referential() {
    let mut pool = MemoryGeorgWhoOwnsAllTheMemory {
        arenas: vec![Box::leak(Box::default())],
    };
    struct House<'p>(&'p Arena<'p>, &'p mut usize);
    let a = pool.arenas.pop().unwrap();
    let h = a.make(House(a, a.make(15)));
    assert_eq!(*h.1, 15);
    set_ones_all(h.0);
    pool.arenas.push(h.0);

    let a = Arena::default();
    let h = a.make(House(&a, a.make(15)));
    assert_eq!(*h.1, 15);
    set_ones_all(h.0);
    assert_ne!(h as *mut House as usize, usize::MAX); // h is stored on the stack, it still just points into the arena

    // doesnt work in release. i guess it assumes you can't produce an invalid pointer?
    debug_assert_eq!(h.1 as *mut usize as usize, usize::MAX); // the value there got blanked

    unsafe {
        let chunk = *a.chunk.get();
        let prev = prev_chunk(chunk);
        assert_eq!((*prev).start, std::ptr::null_mut()); // metadata not overwritten.
    }
    free_all(a);

    let a = Arena::default();
    let mut v: Vec<u32, &Arena> = Vec::new_in(&a);
    for i in 0..5000 {
        v.push(i);
    }
    assert_eq!(v[123], 123);
}
