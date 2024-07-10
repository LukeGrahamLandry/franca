//! Converts simple ASTs into my bytecode-ish format.
//! All comptime execution for a function is finished before it reaches this phase.
//! - Flatten nested expressions to stack operations.
//! - Convert control flow to explicit basic blocks.
//! - Bind non-local return labels.
//! - Reduce variable useage to register sized loads/stores.
//! - Convert large arguments/returns to references and remap signatures to use only register sized types.

#![allow(clippy::wrong_self_convention)]

use crate::ast::Flag;
use crate::ast::{CallConv, FnType, FuncId, Program, TypeInfo};
use crate::bc::*;
use crate::compiler::{ExecStyle, Res};
use crate::export_ffi::{BigOption, BigResult::*};
use crate::unwrap;
use crate::BitSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResultLoc {
    PushStack,
    ResAddr,
    Discard,
}

pub fn empty_fn_body<'p>(program: &Program<'p>, func: FuncId, when: ExecStyle) -> Res<'p, FnBody<'p>> {
    let f = &program[func];
    Ok(FnBody {
        is_ssa_var: BitSet::empty(),
        var_names: vec![],
        vars: Default::default(),
        when,
        func,
        blocks: vec![],
        name: f.name,
        current_block: BbId(0),
        want_log: f.has_tag(Flag::Log_Bc),
        clock: 0,
        signeture: prim_sig(program, unwrap!(f.finished_ty(), "ICE: fn type not ready"), f.cc.unwrap())?,
    })
}

pub fn prim_sig<'p>(program: &Program<'p>, f_ty: FnType, cc: CallConv) -> Res<'p, PrimSig<'p>> {
    let ret = program.get_info(f_ty.ret);

    let has_indirect_ret = ret.size_slots > 2;

    let mut sig = PrimSig {
        args: &[],
        arg_slots: 0,
        ret_slots: ret.size_slots,
        first_arg_is_indirect_return: has_indirect_ret,
        no_return: f_ty.ret.is_never(),
        return_value_bytes: ret.stride_bytes,
        ret1: BigOption::None,
        ret2: BigOption::None,
        arg_int_count: 0,
    };

    match ret.size_slots {
        0 => {}
        1 => sig.ret1 = program.prim(f_ty.ret).into(),
        2 => {
            let (a, b) = program.prim_pair(f_ty.ret)?;
            sig.ret1 = BigOption::Some(a);
            sig.ret2 = BigOption::Some(b);
        }
        _ => {
            sig.arg_slots += 1;
            sig.ret_slots = 0;
            // Note: not adding indirect pointer to sig because its handled sperately. TODO: that's kinda confusing.
        }
    }

    let mut args = vec![];

    let comp_ctx = matches!(cc, CallConv::CCallRegCt);
    if comp_ctx {
        sig.arg_slots += 1;
        args.push(Prim::P64);
    }

    let mut found_arity = 0;
    let mut push_arg = |ty| {
        found_arity += 1;
        let info = program.get_info(ty);
        if info.pass_by_ref {
            sig.arg_slots += 1;
            args.push(Prim::P64);
        } else {
            for t in program.flat_tuple_types(ty) {
                sig.arg_slots += 1;
                args.push(program.prim(t).unwrap());
            }
        }
    };

    let mut done = false;
    // TODO: if i always collapsed tuples of the same to array this would need different handling.
    if let TypeInfo::Struct { fields, is_tuple, .. } = &program[f_ty.arg] {
        if *is_tuple {
            done = true;
            for f in fields {
                push_arg(f.ty);
            }
        }
    }

    if !done {
        push_arg(f_ty.arg);
    }

    // TODO: decide what i want a tuple to be. is it a real type you can have in memory or is it the thing that multiple arguments are?
    //       those ideas should extend to return values instead of them being special.
    //       should have a spread operator for calling a function on a tuple of arguments? like 'a := (1, 2); f(..a)'
    // assert_eq!(found_arity, f_ty.arity, "TODO: fn(a: Ty(i64, i64))");

    let mut key = (f_ty.arg, sig.arg_slots, false, comp_ctx);
    program.primitives.borrow_mut().insert(key, args.clone());
    args.insert(0, Prim::P64); // sad
    key.2 = true;
    program.primitives.borrow_mut().insert(key, args); // TODO: stupid that i have to do this here
    key.2 = false;
    sig.args = program.get_primitives(key).unwrap();

    if sig.first_arg_is_indirect_return {
        debug_assert_eq!(sig.args.len() + 1, sig.arg_slots as usize);
    } else {
        debug_assert_eq!(
            sig.args.len(),
            sig.arg_slots as usize,
            "arg={}\n{sig:?}\n{:?}\nret={}",
            program.log_type(f_ty.arg),
            sig.args,
            program.log_type(f_ty.ret),
        );
    }

    sig.arg_int_count = sig.args.iter().filter(|p| !p.is_float()).count() as u8;

    Ok(sig)
}
