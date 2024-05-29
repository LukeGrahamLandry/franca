//! This generates a text file that a C compiler can convert into an executable.
//! If you look at the output you'll see why I'm reluctant to refer to it as a C program.
// TODO: safe fn names. '//' is not enough, am i totally sure i never leave a new line in there?

use crate::{
    ast::{CallConv, Flag, FuncId, FuncImpl, Program},
    bc::{Bc, FnBody},
    compiler::{Compile, ExecTime, Res},
    emit_bc::emit_bc,
    err, pops,
    reflect::BitSet,
};
use std::{any::TypeId, fmt::Write};

pub fn emit_c<'p>(comp: &mut Compile<'_, 'p>) -> Res<'p, String> {
    let mut out = CProgram::default();

    fn emit<'p>(comp: &mut Compile<'_, 'p>, out: &mut CProgram, f: FuncId) -> Res<'p, ()> {
        if out.fn_emitted.get(f.as_index()) {
            return Ok(());
        }

        if comp.program[f].has_tag(Flag::Ct) {
            return Ok(()); // TODO: why am i trying to call Unique?
        }
        for callee in comp.program[f].mutual_callees.clone() {
            if out.fn_forward.get(callee.as_index()) {
                continue;
            }
            out.fn_forward.insert(callee.as_index(), true);
            // TODO: signeture
            writeln!(out.forward, "void* _FN{}();", callee.as_index()).unwrap();
            emit(comp, out, callee)?;
        }
        for callee in comp.program[f].callees.clone() {
            emit(comp, out, callee)?;
        }

        let name = comp.program.pool.get(comp.program[f].name);

        // println!("do {}", name);
        if let Some(body) = comp.program[f].body.c_source() {
            // TODO: signeture
            writeln!(out.functions, "void* _FN{}(){{ // fn {}", f.as_index(), name).unwrap();
            writeln!(out.functions, "{}", comp.program.pool.get(*body)).unwrap();
            writeln!(out.functions, "}}").unwrap();
        } else if let FuncImpl::Normal(_) = comp.program[f].body {
            if comp.program[f].cc.unwrap() != CallConv::Inline {
                let body = emit_bc(comp, f, ExecTime::Runtime)?;
                // TODO
                let mut wip = Emit {
                    result: out,
                    program: comp.program,
                    body: &body,
                    code: String::new(),
                    blocks_done: BitSet::empty(),
                    stack: vec![],
                    indirect_ret_addr: None,
                    flat_arg_addr: None,
                    flat_args_already_offset: vec![],
                    vars: vec![],
                    f,
                    is_flat: false,
                    var_id: 0,
                };

                write!(wip.code, "char ").unwrap();
                for ty in &body.vars {
                    let bytes = wip.program.get_info(*ty).stride_bytes;
                    let var = wip.next_var();
                    write!(wip.code, "_{var}[{bytes}], ").unwrap();
                    wip.vars.push(var);
                }
                writeln!(wip.code, "__;").unwrap();
                let first_ssa = wip.var_id;
                wip.emit_block(0)?;
                // TODO: signeture
                writeln!(wip.result.functions, "void* _FN{}(){{ // fn {}", f.as_index(), name).unwrap();
                // it doesn't like if you declare variables after a goto label.
                write!(wip.result.functions, "void ").unwrap();
                for var in first_ssa..wip.var_id {
                    write!(wip.result.functions, "*_{var}, ").unwrap();
                }
                writeln!(wip.result.functions, "*_;").unwrap();
                wip.result.functions.push_str(&wip.code);
                writeln!(wip.result.functions, "}}").unwrap();
            }
        } else if comp.program[f].body.comptime_addr().is_some() {
            // TODO: know which header to import somehow
            writeln!(out.forward, "void* _FN{}();// {} (from libc i hope)", f.as_index(), name).unwrap();
        }
        out.fn_emitted.set(f.as_index());

        Ok(())
    }
    // TODO: caller should pass in the list
    for f in comp.export.clone() {
        comp.compile(f, ExecTime::Runtime)?;
        emit(comp, &mut out, f)?;
    }

    out.types.push_str(&out.forward);
    out.types.push_str(&out.functions);
    Ok(out.types)
}

// TODO: this uses a dumb amount of memory
#[derive(Default)]
struct CProgram {
    type_forward: BitSet, // all types need to be typedef-ed to thier typeid
    fn_forward: BitSet,   // anything called mutually recursivly needs to be forward declared
    fn_emitted: BitSet,   // has the implementation been emitted?
    types: String,
    forward: String,
    functions: String,
}

struct Emit<'z, 'p> {
    result: &'z mut CProgram,
    program: &'z mut Program<'p>,
    body: &'z FnBody<'p>,
    code: String,
    blocks_done: BitSet,
    stack: Vec<usize>,
    indirect_ret_addr: Option<usize>,
    flat_arg_addr: Option<usize>,
    flat_args_already_offset: Vec<usize>,
    vars: Vec<usize>,
    f: FuncId,
    is_flat: bool,
    var_id: usize,
}

impl<'z, 'p> Emit<'z, 'p> {
    fn emit_block(&mut self, b: usize) -> Res<'p, ()> {
        if self.blocks_done.get(b) {
            return Ok(());
        }
        self.blocks_done.set(b);

        writeln!(self.code, "_lbl{b}:").unwrap();
        let block = &self.body.blocks[b];
        // We set up a few vars at the beginning to use as block arguments.
        for i in 0..block.arg_slots {
            self.stack.push(i as usize)
        }
        if b == 0 && !self.is_flat {
            let arg = self.program.get_info(self.program[self.f].finished_arg.unwrap());
            // debug_assert_eq!(arg.size_slots, block.arg_slots);
            // self.cast_ret_from_float(builder, arg.size_slots, arg.float_mask);
        }

        for inst in &block.insts {
            match *inst {
                Bc::GetCompCtx => err!("GetCompCtx",),
                Bc::NameFlatCallArg { id, offset_bytes } => {
                    let Some(ptr) = self.flat_arg_addr else { err!("not flat call",) };
                    debug_assert_eq!(id as usize, self.flat_args_already_offset.len());
                    let var = self.next_var();
                    writeln!(self.code, "_{var} = _{ptr} + {offset_bytes};").unwrap();
                    self.flat_args_already_offset.push(ptr);
                }
                // TODO: tail. at least warn if it was forced?
                Bc::CallDirect { f, tail } => {
                    let f_ty = self.program[f].unwrap_ty();
                    let (mut arg, ret) = self.program.get_infos(f_ty);
                    // self.cast_args_to_float(builder, arg.size_slots, arg.float_mask);

                    let args = &self.stack[self.stack.len() - arg.size_slots as usize..self.stack.len()];

                    let ty = self.program[f].unwrap_ty();
                    // TODO
                    // assert!(
                    //     self.program[f].cc.unwrap() != CallConv::Arg8Ret1Ct,
                    //     "{}",
                    //     self.program.pool.get(self.program[f].name)
                    // );
                    let ret = self.next_var();
                    let args = &self.stack[self.stack.len() - arg.size_slots as usize..self.stack.len()];
                    if self.program.slot_count(ty.ret) != 0 {
                        write!(self.code, "_{ret} = ").unwrap();
                    }

                    write!(self.code, "_FN{}(", f.as_index()).unwrap();
                    for arg in args {
                        write!(self.code, "_{arg}, ").unwrap();
                    }

                    let name = self.program.pool.get(self.program[f].name);
                    writeln!(self.code, "); // {name}").unwrap();

                    pops(&mut self.stack, arg.size_slots as usize);
                    if self.program.slot_count(ty.ret) != 0 {
                        self.stack.push(ret);
                    }
                    if f_ty.ret.is_never() {
                        break;
                    }

                    // self.cast_ret_from_float(builder, ret_val.len() as u16, ret.float_mask);
                }
                Bc::CallDirectFlat { f } => {
                    let f_ty = self.program[f].unwrap_ty();
                    // (compiler, arg_ptr, arg_len_i64s, ret_ptr, ret_len_i64)
                    assert_eq!(self.program[f].cc.unwrap(), CallConv::Flat);
                    let (arg, ret) = self.program.get_infos(f_ty);
                    let arg_ptr = self.stack.pop().unwrap();
                    let ret_ptr = self.stack.pop().unwrap();
                    let name = self.program.pool.get(self.program[f].name);
                    writeln!(
                        self.code,
                        "_FN{}(0, _{arg_ptr}, {}, _{ret_ptr}, {}); // {name}",
                        f.as_index(),
                        arg.stride_bytes,
                        ret.stride_bytes
                    )
                    .unwrap();
                    // flat_call result goes into a variable somewhere, already setup by bc. so don't worry about return value here.
                    if f_ty.ret.is_never() {
                        break;
                    }
                }
                Bc::CallFnPtr { ty, comp_ctx } => {
                    assert!(!comp_ctx);
                    // TODO: need to cast to the right signature.

                    let (arg, ret) = self.program.get_infos(ty);
                    // self.cast_args_to_float(builder, arg.size_slots, arg.float_mask);
                    let first_arg = self.stack.len() - arg.size_slots as usize;
                    let callee = self.stack[first_arg - 1];

                    let ret = self.next_var();
                    let args = &self.stack[first_arg..self.stack.len()];
                    if self.program.slot_count(ty.ret) != 0 {
                        write!(self.code, "_{ret} = ").unwrap();
                    }

                    write!(self.code, "_{callee}(").unwrap();
                    for arg in args {
                        write!(self.code, "_{arg}, ").unwrap();
                    }
                    writeln!(self.code, ");").unwrap();

                    pops(&mut self.stack, arg.size_slots as usize);
                    if self.program.slot_count(ty.ret) != 0 {
                        self.stack.push(ret);
                    }
                    if ty.ret.is_never() {
                        break;
                    }
                    // self.cast_ret_from_float(builder, ret_val.len() as u16, ret.float_mask);
                }
                Bc::PushConstant { value } => {
                    let out = self.next_var();
                    writeln!(self.code, "_{out} = {value};").unwrap();
                    self.stack.push(out);
                }
                Bc::PushRelocatablePointer { bytes } => {
                    todo!()
                }
                Bc::JumpIf { true_ip, false_ip, slots } => {
                    debug_assert_eq!(slots, 0);
                    let cond = self.stack.pop().unwrap();
                    let stack = self.stack.clone();
                    writeln!(self.code, "if (_{cond}) goto _lbl{}; else goto _lbl{};", true_ip.0, false_ip.0).unwrap();
                    self.emit_block(true_ip.0 as usize)?;
                    self.stack = stack;
                    self.emit_block(false_ip.0 as usize)?;
                    break;
                }
                Bc::Goto { ip, slots } => {
                    let args = &self.stack[self.stack.len() - slots as usize..self.stack.len()];
                    for (i, var) in args.iter().enumerate() {
                        writeln!(self.code, "_{i} = _{var};").unwrap();
                    }
                    writeln!(self.code, "goto _lbl{};", ip.0).unwrap();
                    pops(&mut self.stack, slots as usize);
                    self.emit_block(ip.0 as usize)?;
                    break;
                }
                Bc::Ret => {
                    if self.indirect_ret_addr.is_some() {
                        // flat_call so we must have already put the values there.
                        writeln!(self.code, "return;").unwrap();
                    } else {
                        let ret = self.program.get_info(self.program[self.f].finished_ret.unwrap());
                        // self.cast_args_to_float(builder, ret.size_slots, ret.float_mask);
                        match ret.size_slots {
                            0 => writeln!(self.code, "return;").unwrap(),
                            1 => {
                                let v = self.stack.pop().unwrap();
                                writeln!(self.code, "return _{v};").unwrap()
                            }
                            _ => err!("ICE: emit_bc never does this. it used flat call instead.",),
                        };
                    }
                    break;
                }
                Bc::GetNativeFnPtr(f) => {
                    let out = self.next_var();
                    writeln!(self.code, "_{out} = &_FN{}", f.as_index()).unwrap();
                    self.stack.push(out);
                }
                Bc::Load { slots } => {
                    debug_assert_ne!(slots, 0);
                    let addr = self.stack.pop().unwrap();
                    for s in 0..slots {
                        let v = self.next_var();
                        writeln!(self.code, "_{v} = *(void**) (_{addr} + {});", s as i32 * 8).unwrap();
                        self.stack.push(v);
                    }
                }
                Bc::StorePost { slots } => {
                    debug_assert_ne!(slots, 0);
                    let addr = self.stack.pop().unwrap();
                    for s in 0..slots {
                        let v = self.stack[self.stack.len() - slots as usize + s as usize];
                        writeln!(self.code, "*(void**) (_{addr} + {}) = _{v};", s as i32 * 8).unwrap();
                    }
                    pops(&mut self.stack, slots as usize);
                }
                Bc::StorePre { slots } => {
                    debug_assert_ne!(slots, 0);
                    let addr = self.stack[self.stack.len() - slots as usize - 1];
                    for s in 0..slots {
                        let v = self.stack[self.stack.len() - slots as usize + s as usize];
                        writeln!(self.code, "*(void**) (_{addr} + {}) = _{v};", s as i32 * 8).unwrap();
                    }
                    pops(&mut self.stack, slots as usize + 1);
                }
                Bc::AddrVar { id } => {
                    if let Some(&ptr) = self.flat_args_already_offset.get(id as usize) {
                        self.stack.push(ptr);
                    } else {
                        let out = self.next_var();
                        let var = self.vars[id as usize];
                        writeln!(self.code, "_{out} = &_{var};").unwrap();
                        self.stack.push(out);
                    }
                }
                Bc::IncPtrBytes { bytes } => {
                    let ptr = self.stack.pop().unwrap();
                    let out = self.next_var();
                    writeln!(self.code, "_{out} = _{ptr} + {bytes};").unwrap();
                    self.stack.push(out);
                }
                Bc::Pop { slots } => {
                    pops(&mut self.stack, slots as usize);
                }
                Bc::TagCheck { expected: _ } => {} // TODO: !!!
                Bc::Unreachable => {
                    writeln!(self.code, "abort();").unwrap();
                    break;
                }
                Bc::NoCompile => err!("NoCompile",),
                Bc::LastUse { .. } | Bc::Noop => {}
                Bc::AddrFnResult => self.stack.push(self.indirect_ret_addr.unwrap()),
                Bc::Dup => {
                    self.stack.push(*self.stack.last().unwrap());
                }
                Bc::CopyBytesToFrom { bytes } => {
                    let from = self.stack.pop().unwrap();
                    let to = self.stack.pop().unwrap();
                    writeln!(self.code, "memcpy(_{to}, _{from}, {bytes})").unwrap();
                }
            }
        }
        Ok(())
    }

    fn next_var(&mut self) -> usize {
        self.var_id += 1;
        self.var_id - 1
    }
}
