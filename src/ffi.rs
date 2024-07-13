use std::mem::{self};

use crate::ast::{FatExpr, FuncId, LabelId, OverloadSetId, Program, ScopeId, TypeId, TypeInfo};

// TODO: figure out how to check that my garbage type keys are unique.
pub trait InterpSend<'p>: Sized + Clone {
    // TODO: could use ptr&len of a static string of the type name. but thats sad.
    //       cant use std::any because of lifetimes. tho my macro can cheat and refer to the name without lifetimes,
    //       so its jsut about the manual base impls for vec,box,option,hashmap.
    fn get_type_key() -> u128; // fuck off bro
    fn get_or_create_type(program: &mut Program) -> TypeId {
        let id = Self::get_type_key();
        program.ffi_types.get(&id).copied().unwrap_or_else(|| {
            let ty = Self::create_type(program); // Note: Not get_type!
            program.ffi_types.insert(id, ty);
            ty
        })
    }

    fn get_type(program: &Program) -> TypeId {
        // cant unwrap_or_else cause #[track_caller] on a lambda
        let Some(&ty) = program.ffi_types.get(&Self::get_type_key()) else {
            panic!("get_type before calling get_or_create_type for {}", Self::name())
        };
        let info = program.get_info(ty);
        debug_assert_eq!(info.stride_bytes as usize, mem::size_of::<Self>());
        debug_assert_eq!(
            info.align_bytes as usize,
            mem::align_of::<Self>(),
            "bad align for {}",
            program.log_type(ty)
        );
        ty
    }
    /// This should only be called once! Use get_type which caches it.
    fn create_type(interp: &mut Program) -> TypeId;

    fn name() -> String {
        String::new()
    }
}

macro_rules! send_num {
    ($ty:ty, $bits:expr, $signed:expr, $bytes:expr, $read_fn:ident, $write_fn:ident, $as_int:ty) => {
        impl<'p> InterpSend<'p> for $ty {
            fn get_type_key() -> u128 {
                unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
            }

            fn create_type(p: &mut Program) -> TypeId {
                p.intern_type(TypeInfo::Int($crate::ast::IntTypeInfo {
                    bit_count: $bits,
                    signed: $signed,
                }))
            }

            fn get_or_create_type(p: &mut Program) -> TypeId {
                p.intern_type(TypeInfo::Int($crate::ast::IntTypeInfo {
                    bit_count: $bits,
                    signed: $signed,
                }))
            }

            // TODO: use correct types
            fn get_type(p: &Program) -> TypeId {
                *p.type_lookup
                    .get(&TypeInfo::Int($crate::ast::IntTypeInfo {
                        bit_count: $bits,
                        signed: $signed,
                    }))
                    .expect("number type should be pre-interned")
            }

            fn name() -> String {
                stringify!($ty).to_string()
            }
        }
    };
}

send_num!(i64, 64, true, 8, next_i64, push_i64, i64);
send_num!(u32, 32, false, 4, next_u32, push_u32, u32);
send_num!(u16, 16, false, 2, next_u16, push_u16, u16);
send_num!(u8, 8, false, 1, next_u8, push_u8, u8);

impl<'p, T: InterpSend<'p>> InterpSend<'p> for *mut T {
    fn get_type_key() -> u128 {
        mix::<T, i64>(1274222)
    }
    fn create_type(p: &mut Program) -> TypeId {
        let t = T::get_or_create_type(p);
        p.intern_type(TypeInfo::Ptr(t))
    }

    // TODO: use correct types
    fn get_type(p: &Program) -> TypeId {
        let t = T::get_type(p);
        *p.type_lookup.get(&TypeInfo::Ptr(t)).unwrap()
    }

    fn get_or_create_type(p: &mut Program) -> TypeId {
        let t = T::get_or_create_type(p);
        p.intern_type(TypeInfo::Ptr(t))
    }

    fn name() -> String {
        format!("*{}", T::name())
    }
}

impl<'p> InterpSend<'p> for FatExpr<'p> {
    fn get_type_key() -> u128 {
        9014097120714207
    }
    fn create_type(p: &mut Program) -> TypeId {
        p.pool.env.fat_expr_type.expect("used FatExpr::InterpSend during bootstrapping")
    }

    fn get_type(p: &Program) -> TypeId {
        p.pool.env.fat_expr_type.expect("used FatExpr::InterpSend during bootstrapping")
    }

    fn get_or_create_type(p: &mut Program) -> TypeId {
        p.pool.env.fat_expr_type.expect("used FatExpr::InterpSend during bootstrapping")
    }

    fn name() -> String {
        "FatExpr".to_string()
    }
}

macro_rules! ffi_builtin_type {
    ($ty:ty, $typeid:expr, $name:expr) => {
        impl<'p> InterpSend<'p> for $ty {
            fn get_type_key() -> u128 {
                unsafe { std::mem::transmute(std::any::TypeId::of::<Self>()) }
            }
            fn create_type(_: &mut Program) -> TypeId {
                $typeid
            }

            fn get_type(_: &Program) -> TypeId {
                $typeid
            }
            fn get_or_create_type(_: &mut Program) -> TypeId {
                $typeid
            }
            fn name() -> String {
                $name.to_string()
            }
        }
    };
}

ffi_builtin_type!(OverloadSetId, TypeId::overload_set, "OverloadSet");
ffi_builtin_type!(ScopeId, TypeId::scope, "Scope");
ffi_builtin_type!(TypeId, TypeId::ty, "Type");
ffi_builtin_type!(LabelId, TypeId::label, "LabelId");
ffi_builtin_type!(FuncId, TypeId::func, "FuncId");
ffi_builtin_type!(bool, TypeId::bool(), "bool");
ffi_builtin_type!(f64, TypeId::f64(), "f64");
ffi_builtin_type!(f32, TypeId::f32(), "f32");
ffi_builtin_type!((), TypeId::unit, "void");

impl<'p, A: InterpSend<'p>, B: InterpSend<'p>> InterpSend<'p> for (A, B) {
    fn get_type_key() -> u128 {
        mix::<A, B>(6749973390999)
    }
    fn create_type(program: &mut Program) -> TypeId {
        let a = A::get_or_create_type(program);
        let b = B::get_or_create_type(program);
        program.tuple_of(vec![a, b])
    }

    fn name() -> String {
        format!("Ty({}, {})", A::name(), B::name())
    }
}

impl<'p, A: InterpSend<'p>, B: InterpSend<'p>, C: InterpSend<'p>> InterpSend<'p> for (A, B, C) {
    fn get_type_key() -> u128 {
        mix::<(A, B), C>(87986)
    }
    fn create_type(program: &mut Program) -> TypeId {
        let a = A::get_or_create_type(program);
        let b = B::get_or_create_type(program);
        let c = C::get_or_create_type(program);
        program.tuple_of(vec![a, b, c])
    }

    fn name() -> String {
        format!("Ty({}, {}, {})", A::name(), B::name(), C::name()) // TODO: unnest
    }
}

fn mix<'p, A: InterpSend<'p>, B: InterpSend<'p>>(extra: u128) -> u128 {
    A::get_type_key().wrapping_mul(B::get_type_key()).wrapping_mul(extra)
}
