use core::slice;
use std::cell::{Cell, UnsafeCell};
use std::mem::{self, align_of, size_of, ManuallyDrop, MaybeUninit};
use std::ptr::{self, addr_of, NonNull};

use interp_derive::Reflect;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct RsType<'t> {
    pub name: &'t str,
    pub align: usize,
    pub stride: usize,
    pub data: RsData<'t>,
    // Does this type have a Drop or Clone implementation?
    // If so, you can't just produce extra with a memcpy, and you have to give exactly as many back to the rust side as it gave you.
    pub is_linear: bool,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum RsData<'t> {
    Struct(&'t [RsField<'t>]),
    Enum(&'t [RsVarient<'t>]),
    Ptr(&'t RsType<'t>),
    Opaque,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct RsField<'t> {
    pub name: &'t str,
    pub offset: usize,
    // This should really just be a reference to the typeinfo but that doesn't give them the same addresses.
    // It's making multiple copies of the same trait constant?
    pub ty: fn() -> &'t RsType<'t>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct RsVarient<'t> {
    pub name: &'t str,
    pub fields: &'t [RsField<'t>],
}

pub trait Reflect {
    const TYPE_INFO: &'static RsType<'static>;

    fn get_ty() -> &'static RsType<'static> {
        Self::TYPE_INFO
    }
}

/// Same as Reflect but with somewhere for the compiler to hang a vtable.
pub trait VReflect {
    fn type_info(&self) -> &'static RsType<'static>;

    fn unchecked_field(&self, index: usize) -> *const u8 {
        if let RsData::Struct(fields) = self.type_info().data {
            let offset = fields[index].offset;
            unsafe { (self as *const Self as *const u8).add(offset) }
        } else {
            todo!()
        }
    }

    fn is(&self, ty: &'static RsType<'static>) -> bool {
        ptr::eq(self.type_info(), ty)
    }

    fn checked_field(&self, index: usize, field_ty: &'static RsType) -> Option<*const u8> {
        if let RsData::Struct(fields) = self.type_info().data {
            if ptr::eq((fields[index].ty)(), field_ty) {
                unsafe {
                    let offset = fields[index].offset;
                    Some((self as *const Self as *const u8).add(offset))
                }
            } else {
                None
            }
        } else {
            todo!()
        }
    }

    fn field_ref<T: Reflect>(&self, index: usize) -> Option<&T>
    where
        Self: Sized,
    {
        self.checked_field(index, T::get_ty())
            .map(|ptr| unsafe { &*(ptr as *const T) })
    }

    fn field_mut<T: Reflect>(&mut self, index: usize) -> Option<&mut T>
    where
        Self: Sized,
    {
        self.checked_field(index, T::get_ty())
            .map(|ptr| unsafe { &mut *(ptr as *mut T) })
    }
}

impl<T: Reflect> VReflect for T {
    fn type_info(&self) -> &'static RsType<'static> {
        T::get_ty()
    }
}

// This could be a reference but I expect the main use will be passing into assembly.
pub struct AnyReflect(*mut dyn VReflect);

impl AnyReflect {
    /// # Safety
    /// This does a type check so it just needs to be a valid pointer.
    pub unsafe fn upcast<T: Reflect>(&self) -> Option<*mut T> {
        if std::ptr::eq((*self.0).type_info(), T::get_ty()) {
            let (data, _vtable) = self.0.to_raw_parts();
            Some(data as *mut T)
        } else {
            None
        }
    }
}

macro_rules! field_offset {
    ($ty:ty, $name:ident) => {
        unsafe {
            let ptr = std::mem::MaybeUninit::<$ty>::uninit().as_ptr();
            (std::ptr::addr_of!((*ptr).$name) as *const u8).offset_from(ptr as *const u8) as usize
        }
    };
}
pub(crate) use field_offset;

use crate::ffi::InterpSend;

fn _does_it_compile(_: &dyn VReflect) {}

fn none_bytes<T>() -> &'static [u8] {
    let none: ManuallyDrop<Option<T>> = ManuallyDrop::new(None);
    unsafe { slice::from_raw_parts(addr_of!(none) as *const u8, size_of::<Option<T>>()) }
}

macro_rules! opaque {
    ($ty:ty) => {
        impl Reflect for $ty {
            const TYPE_INFO: &'static RsType<'static> = &RsType {
                name: stringify!($ty),
                align: align_of::<Self>(),
                stride: size_of::<Self>(),
                data: RsData::Opaque,
                is_linear: false,
            };
        }
    };

    ($ty:ty, $($arg:tt)*) => {
        opaque!($ty);
        opaque!($($arg)*);
    };
}

opaque!(i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize, bool);

macro_rules! impl_ptr {
    ($ty:ty) => {
        impl<T: Reflect> Reflect for $ty {
            const TYPE_INFO: &'static RsType<'static> = &RsType {
                name: stringify!($ty),
                align: align_of::<Self>(),
                stride: size_of::<Self>(),
                data: RsData::Ptr(T::TYPE_INFO),
                is_linear: false,
            };
        }
    };

    ($ty:ty, $($arg:tt)*) => {
        impl_ptr!($ty);
        impl_ptr!($($arg)*);
    };
}

impl_ptr!(*const T, *mut T, NonNull<T>);

impl<T: Reflect> Reflect for Box<T> {
    const TYPE_INFO: &'static RsType<'static> = &RsType {
        name: stringify!($ty),
        align: align_of::<Self>(),
        stride: size_of::<Self>(),
        data: RsData::Ptr(T::TYPE_INFO),
        is_linear: true,
    };
}

macro_rules! transparent {
    ($ty:ty) => {
        impl<T: Reflect> Reflect for $ty {
            const TYPE_INFO: &'static RsType<'static> = T::TYPE_INFO;
        }
    };

    ($ty:ty, $($arg:tt)*) => {
        transparent!($ty);
        transparent!($($arg)*);
    };
}

transparent!(Cell<T>, UnsafeCell<T>, ManuallyDrop<T>, MaybeUninit<T>);

macro_rules! impl_slice {
    ($ty:ty) => {
        impl<T: Reflect> Reflect for $ty {
            const TYPE_INFO: &'static RsType<'static> = &RsType {
                name: "slice",
                // TODO: not this. but lifetimes make it hard.
                align: align_of::<(usize, usize)>(),
                stride: size_of::<(usize, usize)>(),
                data: RsData::Struct(&[
                    RsField {
                        name: "ptr",
                        offset: 0,
                        ty: <*const T>::get_ty,
                    },
                    RsField {
                        name: "len",
                        offset: 8,
                        ty: <usize>::get_ty,
                    },
                ]),
                is_linear: false,
            };
        }
    };

    ($ty:ty, $($arg:tt)*) => {
        impl_slice!($ty);
        impl_slice!($($arg)*);
    };
}

impl_slice!(*const [T], *mut [T], &[T], &mut [T]);

impl<T: Reflect> Reflect for Box<[T]> {
    const TYPE_INFO: &'static RsType<'static> = &RsType {
        name: "slice",
        // TODO: not this. but lifetimes make it hard.
        align: align_of::<(usize, usize)>(),
        stride: size_of::<(usize, usize)>(),
        data: RsData::Struct(&[
            RsField {
                name: "ptr",
                offset: 0,
                ty: <*const T>::get_ty,
            },
            RsField {
                name: "len",
                offset: 8,
                ty: <usize>::get_ty,
            },
        ]),
        is_linear: true,
    };
}

fn ptr_before_len<T>() -> bool {
    let (a, b, c, d, e): (&[T], &mut [T], _, _, Box<[T]>) = (
        &[],
        &mut [],
        &[] as *const [T],
        &mut [] as *mut [T],
        Box::new([]),
    );
    // This transmute can't be const.
    let (ptr_a, len_a): (usize, usize) = unsafe { mem::transmute(a) };
    let (ptr_b, len_b): (usize, usize) = unsafe { mem::transmute(b) };
    let (ptr_c, len_c): (usize, usize) = unsafe { mem::transmute(c) };
    let (ptr_d, len_d): (usize, usize) = unsafe { mem::transmute(d) };
    let (ptr_e, len_e): (usize, usize) = unsafe { mem::transmute(e) };

    let ptr_first = len_a == 0;
    if ptr_first {
        assert!(
            (len_a == len_b) && (len_a == len_c) && (len_a == len_d) && (len_a == len_e),
            "inconsitant layout"
        );
    } else {
        assert!(
            (ptr_a == ptr_b) && (ptr_a == ptr_c) && (len_a == ptr_d) && (len_a == ptr_e),
            "inconsitant layout"
        );
    }
    ptr_first
}

// So here's the question: when they say layout is unspecified, do they mean they're not telling you
// and it might change next compiler version or that trying to observe it is nasal demon UB?
// The safe answer would be use repr(C) for everything but then you have to write your own Vec and stuff.
// https://doc.rust-lang.org/nomicon/repr-rust.html
// "Rust does guarantee that two instances of A have their data laid out in exactly the same way."
// You just have to do the math for every type even if they two have the same fields.
mod test {
    #![allow(unused)]
    use super::{Reflect, RsField};
    use crate::experiments::reflect::{ptr_before_len, VReflect};
    use core::slice;
    use std::{
        fmt::Debug,
        hint::black_box,
        mem::{self, size_of, MaybeUninit},
        ptr::{addr_of, NonNull},
    };

    #[derive(Reflect)]
    struct Foo {
        a: i64,
        b: i64,
    }

    #[derive(Reflect)]
    struct Bar {
        a: Box<[u8]>,
        b: i64,
    }

    #[derive(Reflect)]
    struct Baz {
        a: bool,
        b: i64,
    }

    #[test]
    fn reflect() {
        unsafe {
            let mut ffoo = Foo { a: 123, b: 456 };
            let a_ptr = ffoo.unchecked_field(0) as *const i64;
            assert_eq!(*a_ptr, 123);
            assert_eq!(*ffoo.field_ref::<i64>(0).unwrap(), ffoo.a);
            assert_eq!(*ffoo.field_ref::<i64>(1).unwrap(), ffoo.b);
            assert_eq!(ffoo.field_ref::<i32>(1), None);
            *ffoo.field_mut::<i64>(0).unwrap() = 789;
            assert_eq!(ffoo.a, 789);
            let n: i64 = 1024;
            let n_ptr = &n as &dyn VReflect;
            println!("{:p} {:p} coconut.jpg", n_ptr.type_info(), i64::get_ty());
            // TODO: this doesnt work in release unless you force observe it with the print above.
            assert!(n_ptr.is(i64::get_ty()));
        }
        assert!(Box::<usize>::get_ty().is_linear);
        assert!(Bar::get_ty().is_linear);
        assert!(!Foo::get_ty().is_linear);
        assert!(!Baz::get_ty().is_linear);
    }

    #[test]
    fn slice_layout() {
        assert!(ptr_before_len::<u8>());
        let x: &[u8] = &[];
        let ptr = x.field_ref::<*const u8>(0).unwrap();
        assert!(!ptr.is_null());
        let len = x.field_ref::<usize>(1).unwrap();
        assert_eq!(*len, 0);
        let x: &[u8] = &[1, 2, 3];
        let len = x.field_ref::<usize>(1).unwrap();
        assert_eq!(*len, 3);
    }

    #[inline(never)]
    fn opaque_eq<T: PartialEq + Debug>(a: T, b: T) {
        assert_eq!(a, b);
    }

    struct Bool(bool);

    struct ByteInfo {
        zero_option: Box<[u8]>,
        none: Box<[u8]>,
    }

    #[test]
    fn field_layout_experiments() {
        unsafe {
            fn get_niche<T>() -> ByteInfo {
                unsafe {
                    // let zero_raw: T = mem::zeroed();
                    let none: Option<T> = None;
                    let zero_option: Option<T> = mem::zeroed();
                    // let zero_raw =
                    //     slice::from_raw_parts(addr_of!(zero_raw) as *const u8, size_of::<T>());
                    let zero_option = slice::from_raw_parts(
                        addr_of!(zero_option) as *const u8,
                        size_of::<Option<T>>(),
                    );
                    let none =
                        slice::from_raw_parts(addr_of!(none) as *const u8, size_of::<Option<T>>());
                    ByteInfo {
                        // zero_raw: zero_raw.into(),
                        zero_option: zero_option.into(),
                        none: none.into(),
                    }
                }
            }

            let ptr = MaybeUninit::<Bar>::uninit().as_ptr();
            let base = ptr as *const u8 as usize;
            let offset_a = addr_of!((*ptr).a) as *const u8 as usize - base;
            let offset_b = addr_of!((*ptr).b) as *const u8 as usize - base;
            assert_eq!(offset_a, 0);
            assert_eq!(offset_b, size_of::<Box<[u8]>>());
            assert_eq!(offset_a, field_offset!(Bar, a));
            // Niche optimisation is a thing. You can use a null pointer as the tag.
            assert_eq!(size_of::<Bar>(), size_of::<Option<Bar>>());
            let info = black_box(get_niche::<Bar>());
            let some = Some(Bar {
                a: Box::new([]),
                b: 1,
            });
            let some_bytes = slice::from_raw_parts(addr_of!(some) as *const u8, size_of::<Bar>());
            assert_eq!(info.none, info.zero_option);
            assert_ne!(some_bytes, &*info.zero_option);

            let ptr = MaybeUninit::<Baz>::uninit().as_ptr();
            let base = ptr as *const u8 as usize;
            // You have to pad out the bool field so the stride is a multiple of the alignment.
            assert_eq!(size_of::<Baz>(), size_of::<Foo>());
            // That means you can store the tag in the padding.
            assert_eq!(size_of::<Baz>(), size_of::<Option<Baz>>());

            let info = get_niche::<Baz>();
            assert_ne!(info.none, info.zero_option);

            let zero_option: Option<Baz> = mem::zeroed();
            assert!(zero_option.is_some());
            let some = Some(Baz { a: false, b: 0 });
            let none: Option<Baz> = None;

            let a_word_some = addr_of!(some.as_ref().unwrap().a) as *const i64;
            // This being assert_eq changes behaviour
            opaque_eq(*a_word_some, 0);
            let none_addr = addr_of!(none) as *const Baz;
            let a_word_none = addr_of!((*none_addr).a) as *const i64;
            let value = *a_word_none;
            // This is fragile, I think ever assuming specific bit patterns is bad.
            assert_eq!(value, 2);
            assert_eq!(*a_word_none, 2);

            let b_word_none = addr_of!((*none_addr).b);

            // This being assert_eq changes behaviour at the one above
            opaque_eq(*b_word_none, 0);

            // So then nested options can share the niche.
            assert_eq!(
                size_of::<Baz>(),
                size_of::<Option<Option<Option<Option<Baz>>>>>()
            );
            // But before when there was only the pointer and the struct filled its stride, nested options need extra space.
            assert_ne!(size_of::<Bar>(), size_of::<Option<Option<Bar>>>());
            // Now there's a whole extra word so they can nest for free.
            assert_eq!(
                size_of::<Option<Option<Bar>>>(),
                size_of::<Option<Option<Option<Option<Option<Bar>>>>>>(),
            );
        }
    }
}

/*
// Feels like this might be useful for representing padding?
// But maybe it makes more sense to do it in chunks since its generally all at the end (when not repr(C)?).
enum BitSet {
    Small(u128),
    Big(Vec<usize>),
}

impl BitSet {
    fn new(size: usize) -> Self {
        if size <= 128 {
            BitSet::Small(0)
        } else {
            BitSet::Big(vec![0; size / size_of::<usize>() + 1])
        }
    }

    fn get(&self, i: usize) -> bool {
        match self {
            BitSet::Small(v) => (v & 1 << i) != 0,
            BitSet::Big(v) => {
                let index = i / size_of::<usize>();
                let bit = i % size_of::<usize>();
                (v[index] & 1 << bit) != 0
            }
        }
    }
}
*/

impl<'p, 't> InterpSend<'p> for &'t RsType<'t> {
    fn get_type_key() -> u128 {
        todo!()
    }

    fn create_type(_: &mut crate::ast::Program<'p>) -> crate::ast::TypeId {
        todo!()
    }

    fn serialize(self, _: &mut Vec<crate::bc::Value>) {
        todo!()
    }

    fn deserialize(_: &mut impl Iterator<Item = crate::bc::Value>) -> Option<Self> {
        todo!()
    }

    fn size() -> usize {
        todo!()
    }
}
