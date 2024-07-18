use std::mem;
use std::ptr::{slice_from_raw_parts, slice_from_raw_parts_mut};

use crate::assert_eq;
use crate::ast::Program;
use crate::unwrap;
use crate::{
    ast::{FuncId, TypeId},
    compiler::Res,
    err,
    ffi::InterpSend,
};

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct BakedEntry {
    opaque: [usize; 3],
}

#[repr(C, i64)]
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Values {
    Big(Vec<u8>),
    Small(i64, u8),
}

impl From<u32> for Values {
    fn from(value: u32) -> Self {
        Self::Small(value as i64, 4)
    }
}

impl Values {
    pub(crate) fn bytes(&self) -> &[u8] {
        match self {
            Values::Big(bytes) => bytes,
            Values::Small(value, len) => unsafe { &*slice_from_raw_parts(value as *const i64 as *const u8, *len as usize) },
        }
    }

    pub(crate) fn unit() -> Self {
        Self::Small(0, 0)
    }

    pub(crate) fn one(v: i64) -> Self {
        Self::Small(v, 8)
    }

    pub(crate) fn many(v: Vec<u8>) -> Self {
        if v.len() <= 8 {
            Self::from_bytes(&v)
        } else {
            Self::Big(v)
        }
    }

    pub(crate) fn from_bytes(bytes: &[u8]) -> Self {
        if bytes.len() <= 8 {
            let mut v = 0;
            let mut shift = 0;
            for b in bytes {
                v |= (*b as i64) << shift;
                shift += 8;
            }
            Self::Small(v, bytes.len() as u8)
        } else {
            Self::Big(bytes.to_vec())
        }
    }

    pub(crate) fn len(&self) -> usize {
        self.bytes().len()
    }
}

impl From<i64> for Values {
    fn from(value: i64) -> Self {
        Values::one(value)
    }
}

impl Values {
    pub(crate) fn unwrap_func_id(&self) -> FuncId {
        debug_assert_eq!(self.bytes().len(), 4);
        let i = ReadBytes { bytes: self.bytes(), i: 0 }.next_u32().unwrap();
        FuncId::from_raw(i)
    }
}

use crate::export_ffi::BigResult::*;
#[track_caller]
pub(crate) fn to_values<'p, T: InterpSend<'p>>(_program: &mut Program<'p>, mut t: T) -> Res<'p, Values> {
    let bytes = unsafe { &mut *slice_from_raw_parts_mut(&mut t as *mut T as *mut u8, mem::size_of::<T>()) };
    debug_assert_eq!(bytes.as_ptr() as usize % mem::align_of::<T>(), 0);
    debug_assert_eq!(bytes.len(), mem::size_of::<T>());
    Ok(Values::from_bytes(bytes))
}

pub(crate) fn from_values<'p, T: InterpSend<'p>>(_program: &Program<'p>, t: Values) -> Res<'p, T> {
    assert_eq!(t.bytes().len(), mem::size_of::<T>(), "from_values {t:?}");
    debug_assert_eq!(t.bytes().as_ptr() as usize % mem::align_of::<T>(), 0);
    unsafe { Ok(std::ptr::read(t.bytes().as_ptr() as *const T)) }
}

// When binding const arguments you want to split a large value into smaller ones that can be referred to by name.
pub(crate) fn chop_prefix<'p>(program: &Program<'p>, prefix: TypeId, t: &mut ReadBytes) -> Res<'p, Values> {
    let info = program.get_info(prefix);
    debug_assert_eq!(t.i % info.align_bytes as usize, 0);
    let bytes = info.stride_bytes;
    let taken = unwrap!(t.take(bytes as usize), "");
    Ok(Values::many(taken.to_vec()))
}

#[derive(Debug)]
pub struct ReadBytes<'a> {
    pub bytes: &'a [u8],
    pub i: usize,
}

impl<'a> ReadBytes<'a> {
    pub(crate) fn next_u32(&mut self) -> Option<u32> {
        debug_assert_eq!(self.i % 4, 0);
        if self.i + 3 < self.bytes.len() {
            self.i += 4;
            Some(unsafe { *(self.bytes.as_ptr().add(self.i - 4) as *const u32) })
        } else {
            None
        }
    }

    pub(crate) fn take(&mut self, len: usize) -> Option<&[u8]> {
        if self.bytes.len() - self.i >= len {
            let res = Some(&self.bytes[self.i..self.i + len]);
            self.i += len;
            res
        } else {
            None
        }
    }
}
