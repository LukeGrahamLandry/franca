use std::mem::ManuallyDrop;

use crate::ast::{Program, TypeId, TypeInfo};

#[cfg(target_arch = "aarch64")]
#[no_mangle]
pub extern "C" fn copy_to_mmap_exec(code: CVec<u32>) -> Pair<CBox<memmap2::Mmap>, *const u8> {
    // TODO: emit into this thing so don't have to copy.
    use core::slice;
    let mut map = memmap2::MmapOptions::new()
        .len(code.len * 4)
        .map_anon()
        .unwrap();
    let bytes = code.ptr as *const u8;
    let bytes = unsafe { slice::from_raw_parts(bytes, code.len * 4) };
    map.copy_from_slice(bytes);
    let map = map.make_exec().unwrap();
    let ptr = map.as_ptr();
    Pair(Box::new(map).into(), ptr)
}

pub fn intern_type<'p>(program: &mut Program<'p>, ty: TypeInfo<'p>) -> TypeId {
    program.intern_type(ty)
}

#[repr(C)]
pub struct Pair<A, B>(pub A, pub B);

#[repr(C)]
pub struct CBox<T> {
    pub ptr: *mut T,
}

#[repr(C)]
pub struct CVec<T> {
    pub ptr: *mut T,
    pub len: usize,
    pub cap: usize,
}

impl<T> From<Vec<T>> for CVec<T> {
    fn from(value: Vec<T>) -> Self {
        let mut value = ManuallyDrop::new(value);
        CVec {
            ptr: value.as_mut_ptr(),
            len: value.len(),
            cap: value.capacity(),
        }
    }
}

impl<T> From<CVec<T>> for Vec<T> {
    fn from(value: CVec<T>) -> Self {
        unsafe { Vec::from_raw_parts(value.ptr, value.len, value.cap) }
    }
}

impl<T> From<Box<T>> for CBox<T> {
    fn from(value: Box<T>) -> Self {
        CBox {
            ptr: Box::into_raw(value),
        }
    }
}

impl<T> From<usize> for CBox<T> {
    fn from(value: usize) -> Self {
        CBox {
            ptr: value as *mut T,
        }
    }
}

impl<T> From<CBox<T>> for usize {
    fn from(value: CBox<T>) -> Self {
        let value = ManuallyDrop::new(value);
        value.ptr as usize
    }
}

impl<T> Drop for CBox<T> {
    fn drop(&mut self) {
        unsafe {
            drop(Box::from_raw(self.ptr));
        }
    }
}

impl<T> CBox<T> {
    pub fn to_box(self) -> Box<T> {
        let value = ManuallyDrop::new(self);
        unsafe { Box::from_raw(value.ptr) }
    }
}
