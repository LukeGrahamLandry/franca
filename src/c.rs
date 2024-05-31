//! This generates a text file that a C compiler can convert into an executable.
//! If you look at the output you'll see why I'm reluctant to refer to it as a C program.
//! And now a small blessing: May the code in this file be as ugly as the code it generates.

// TODO: safe fn names. '//' is not enough, am i totally sure i never leave a new line in there?
// TODO: cast fn ptr arguments
use crate::{
    ast::{CallConv, Flag, FnType, FuncId, FuncImpl, Program, TypeId, TypeInfo},
    bc::{BakedVar, Bc, FnBody},
    compiler::{Compile, ExecStyle, Res},
    emit_bc::emit_bc,
    err,
    logging::PoolLog,
    pops,
    reflect::BitSet,
};
use std::fmt::Write;

// try put 'static' on everything that's not exported beacuse that makes clang -O2 not leave them in the exe if it decides to inline.
fn declare(comp: &Compile, out: &mut String, f: FuncId, use_name: bool, use_arg_names: bool) {
    let name = comp.program.pool.get(comp.program[f].name);
    let ty = comp.program[f].finished_ty().unwrap();
    if use_name {
        write!(out, "_TY{} {name}(", ty.ret.as_index()).unwrap();
    } else {
        write!(out, "/* fn {name} */ static \n_TY{} _FN{}(", ty.ret.as_index(), f.as_index()).unwrap();
    }
    if !ty.arg.is_unit() {
        if use_arg_names {
            for b in &comp.program[f].arg.bindings {
                let ty = b.ty.unwrap();
                if ty.is_unit() {
                    continue;
                }
                let name = comp.program.pool.get(b.name().unwrap());
                write!(out, "_TY{} {},", ty.as_index(), name).unwrap();
            }
        } else {
            for (i, ty) in comp.program.flat_tuple_types(ty.arg).into_iter().enumerate() {
                if ty.is_unit() {
                    continue;
                }
                write!(out, "_TY{} _arg_{i},", ty.as_index()).unwrap();
            }
        }

        if out.ends_with(',') {
            out.remove(out.len() - 1); // comma
        }
    }
    write!(out, ")").unwrap();
}

fn forward_declare(comp: &mut Compile, out: &mut CProgram, callee: FuncId) {
    if out.fn_forward.get(callee.as_index()) {
        return;
    }
    out.fn_forward.insert(callee.as_index(), true);

    let is_flat = matches!(comp.program[callee].cc.unwrap(), CallConv::Flat | CallConv::FlatCt);
    if is_flat {
        let name = comp.program.pool.get(comp.program[callee].name);
        writeln!(out.forward, " /* fn {name} */ static \nvoid _FN{}{FLAT_ARGS_SIGN}", callee.as_index()).unwrap();
    } else {
        declare(comp, &mut out.forward, callee, false, false);
    }
    writeln!(out.forward, ";").unwrap();
}

const FLAT_ARGS_SIGN: &str = "(int _a, void* _flat_arg_ptr, int _b, void* _flat_ret_ptr, int _c)";
pub fn emit_c<'p>(comp: &mut Compile<'_, 'p>) -> Res<'p, String> {
    let mut out = CProgram::default();

    fn emit<'p>(comp: &mut Compile<'_, 'p>, out: &mut CProgram, f: FuncId) -> Res<'p, ()> {
        if out.fn_emitted.get(f.as_index()) {
            return Ok(());
        }
        out.fn_emitted.set(f.as_index());

        if comp.program[f].has_tag(Flag::Ct) {
            return Ok(()); // TODO: why am i trying to call Unique?
        }
        for callee in comp.program[f].callees.clone() {
            emit(comp, out, callee)?;
        }
        let mutual_callees = comp.program[f].mutual_callees.clone();
        for callee in mutual_callees {
            forward_declare(comp, out, callee);
            emit(comp, out, callee)?;
        }
        let name = comp.program.pool.get(comp.program[f].name);

        let ty = comp.program[f].finished_ty().unwrap();
        render_typedef(comp.program, out, ty.arg)?;
        render_typedef(comp.program, out, ty.ret)?;

        // println!("do {}", name);
        if let Some(&body) = comp.program[f].body.c_source() {
            declare(comp, &mut out.functions, f, false, true);
            writeln!(out.functions, "{{\n{}", comp.program.pool.get(body)).unwrap();
            writeln!(out.functions, "}}").unwrap();
        } else if let FuncImpl::Normal(_) = comp.program[f].body {
            if comp.program[f].cc.unwrap() != CallConv::Inline {
                let is_flat = matches!(comp.program[f].cc.unwrap(), CallConv::Flat | CallConv::FlatCt);
                if is_flat {
                    writeln!(out.functions, "/* fn {name} */ static \nvoid _FN{}{FLAT_ARGS_SIGN}", f.as_index()).unwrap();
                } else {
                    declare(comp, &mut out.functions, f, false, false);
                }

                let body = emit_bc(comp, f, ExecStyle::Aot)?;
                let mut wip = Emit {
                    result: out,
                    program: comp.program,
                    body: &body,
                    code: String::new(),
                    blocks_done: BitSet::empty(),
                    stack: vec![],
                    flat_args_already_offset: vec![],
                    f,
                    is_flat,
                    var_id: STACK_ARG_SLOTS,
                };

                write!(wip.result.functions, "{{\n    ").unwrap();
                for (i, ty) in body.vars.iter().enumerate() {
                    let mut ty = *ty;
                    if ty.is_unit() || ty.is_never() {
                        // HACK
                        ty = TypeId::i64();
                    }
                    render_typedef(wip.program, wip.result, ty)?;
                    let _ = wip.next_var();
                    let size = wip.program.get_info(ty).size_slots;
                    write!(wip.result.functions, "void* _s{i}[{size}]; ").unwrap();
                }
                writeln!(wip.result.functions).unwrap();
                wip.emit_block(0)?;
                // it doesn't like if you declare variables after a goto label.
                write!(wip.result.functions, "    void ").unwrap();
                for var in 0..wip.var_id {
                    write!(wip.result.functions, "*_{var}, ").unwrap();
                }
                writeln!(wip.result.functions, "*_;").unwrap();
                write!(wip.result.functions, "    ").unwrap();
                for i in 0..body.vars.len() {
                    let v = i + STACK_ARG_SLOTS;
                    write!(wip.result.functions, "_{v} = &_s{i}; ").unwrap();
                }
                writeln!(wip.result.functions).unwrap();
                wip.result.functions.push_str(&wip.code);
                writeln!(wip.result.functions, "}}").unwrap();
            }
        } else if comp.program[f].body.comptime_addr().is_some() {
            // TODO: dont just assume that comptime_addr means its from libc. have a way to specfify who you're expecting to link against.
            declare(comp, &mut out.forward, f, true, true);
            writeln!(out.forward, ";").unwrap();

            declare(comp, &mut out.forward, f, false, true);
            writeln!(out.forward, "{{").unwrap();
            write!(out.forward, "return {name}(").unwrap();
            if !ty.arg.is_unit() {
                for b in &comp.program[f].arg.bindings {
                    let name = comp.program.pool.get(b.name().unwrap());
                    write!(out.forward, "{name},").unwrap();
                }
                out.forward.remove(out.forward.len() - 1); // comma
            }
            writeln!(out.forward, ");").unwrap();
            writeln!(out.forward, "}}").unwrap();
        } else if let FuncImpl::Redirect(target) = comp.program[f].body {
            // TODO: the frontend should just add the target as a callee instead.
            // this works because emit_bc redirects calls to the target.
            emit(comp, out, target)?
        } else {
            println!("/* ERROR: No c compatible body for \n{}*/", comp.program[f].log(comp.pool))
        }

        Ok(())
    }

    // extra calls emitted by the compiler here
    let memcpy = comp.program.find_unique_func(comp.pool.intern("memcpy")).unwrap();
    comp.compile(memcpy, ExecStyle::Aot)?;
    emit(comp, &mut out, memcpy)?;

    let mut any_called_main = false;
    let mut all_are_tests = true;
    // TODO: caller should pass in the list
    let exports = comp.export.clone();
    for f in exports.iter().copied() {
        any_called_main |= comp.program[f].name == Flag::Main.ident();
        let is_test = comp.program[f].has_tag(Flag::Test);
        all_are_tests &= is_test;
        comp.compile(f, ExecStyle::Aot)?;
        emit(comp, &mut out, f)?;

        if comp.program[f].cc.unwrap() != CallConv::Flat && !is_test {
            // TODO: this is copy paste from comptime_addr one
            declare(comp, &mut out.functions, f, true, true);
            writeln!(out.functions, "{{").unwrap();
            write!(out.functions, "return _FN{}(", f.as_index()).unwrap();
            let ty = comp.program[f].finished_ty().unwrap();
            if !ty.arg.is_unit() {
                for b in &comp.program[f].arg.bindings {
                    let name = comp.program.pool.get(b.name().unwrap());
                    let raw = comp.program.raw_type(b.ty.unwrap());
                    if let TypeInfo::Struct { fields, .. } = &comp.program[raw] {
                        for i in 0..fields.len() {
                            write!(out.functions, "{name}._{i},").unwrap();
                        }
                    } else {
                        write!(out.functions, "{name},").unwrap();
                    }
                }
                out.functions.remove(out.functions.len() - 1); // comma
            }
            writeln!(out.functions, ");").unwrap();
            writeln!(out.functions, "}}").unwrap();
        } else {
            writeln!(out.functions, "// Skip exporting flat call: fn {}", comp.pool.get(comp.program[f].name)).unwrap();
        }
    }

    if all_are_tests {
        writeln!(out.functions, "int main(){{").unwrap();
        for f in exports {
            if comp.program[f].finished_arg.unwrap().is_unit() {
                writeln!(out.functions, "_FN{}();", f.as_index()).unwrap();
            } else {
                writeln!(out.functions, "_FN{}(0);", f.as_index()).unwrap();
            }
        }
        writeln!(out.functions, "return 0;}}").unwrap();
    }

    // This has to be done after compiling all the functions so we know all the required constants.
    let mut constants = String::new();
    // Since we emit more functions in the body of the loop (since constants might not be in anyone's callee list),
    // need to check against the list's length every iteration.
    let mut i = 0;
    loop {
        let v = comp.program.baked.values.borrow();

        let Some((data, _)) = v.get(i) else { break };

        match data {
            BakedVar::Zeros { .. } => todo!(),
            BakedVar::Bytes(bytes) => {
                let len = bytes.len().max(8);
                writeln!(constants, "    static unsigned char _const{i}[{}] = {{", len).unwrap();
                for b in bytes {
                    write!(constants, "{b},").unwrap();
                }
                if bytes.len() < 8 {
                    // HACK. it always tries to load as void* but emits bool constants like this stupidly.
                    for _ in bytes.len()..8 {
                        write!(constants, "0,").unwrap();
                    }
                }
                constants.remove(constants.len() - 1); // comma
                writeln!(constants, "}};").unwrap();
            }
            &BakedVar::FnPtr(f) => {
                drop(v);
                writeln!(constants, "    static void* _const{i} = (void*) &_FN{};", f.as_index()).unwrap();
                forward_declare(comp, &mut out, f);
                comp.compile(f, ExecStyle::Aot)?;
                emit(comp, &mut out, f)?;
            }
            BakedVar::AddrOf(id) => {
                writeln!(constants, "    static const void* _const{i} = (void*) &_const{};", id.0).unwrap();
            }
            BakedVar::VoidPtrArray(parts) => {
                let parts = parts.clone();
                drop(v);
                writeln!(constants, "    static void *_const{i}[{}] = {{", parts.len()).unwrap();
                for p in parts {
                    let v = comp.program.baked.values.borrow();
                    match v[p.0 as usize].0 {
                        BakedVar::Zeros { .. } => todo!(),
                        BakedVar::Bytes(ref b) => {
                            if b.len() == 8 {
                                let v = u64::from_ne_bytes([b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7]]); //fuck
                                write!(constants, "(void*){v},").unwrap()
                            } else {
                                todo!("{b:?}")
                            }
                        }
                        BakedVar::Num(v) => write!(constants, "(void*){v},").unwrap(),
                        BakedVar::FnPtr(f) => {
                            write!(constants, "(void*)&_FN{},", f.as_index()).unwrap();
                            drop(v);
                            forward_declare(comp, &mut out, f);
                            comp.compile(f, ExecStyle::Aot)?;
                            emit(comp, &mut out, f)?;
                        }
                        BakedVar::AddrOf(v) => write!(constants, "(void*)&_const{},", v.0).unwrap(),
                        BakedVar::VoidPtrArray(_) => todo!(),
                    }
                }
                constants.remove(constants.len() - 1); // comma
                writeln!(constants, "}};").unwrap();
            }
            BakedVar::Num(v) => {
                writeln!(constants, "    static void* _const{i} = (void*) {v};").unwrap();
            }
        }
        i += 1;
    }

    out.types.push_str(&out.forward);
    out.types.push_str(&constants);
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
    flat_args_already_offset: Vec<usize>,
    f: FuncId,
    is_flat: bool,
    var_id: usize,
}

const STACK_ARG_SLOTS: usize = 8;

impl<'z, 'p> Emit<'z, 'p> {
    fn emit_block(&mut self, b: usize) -> Res<'p, ()> {
        if self.blocks_done.get(b) {
            return Ok(());
        }
        self.blocks_done.set(b);

        let block = &self.body.blocks[b];

        if b == 0 && !self.is_flat {
            let arg = self.program.get_info(self.program[self.f].finished_arg.unwrap());
            debug_assert_eq!(arg.size_slots, block.arg_slots);
            write!(self.code, "    ").unwrap();
            for i in 0..block.arg_slots {
                write!(self.code, "_{i} = (void*) _arg_{i}; ").unwrap();
            }
            writeln!(self.code).unwrap();
        }

        writeln!(self.code, "_lbl{b}: // ({})", block.arg_slots).unwrap();
        // We set up a few vars at the beginning to use as block arguments.
        assert!((block.arg_slots as usize) < STACK_ARG_SLOTS);
        for i in 0..block.arg_slots {
            self.stack.push(i as usize)
        }
        for inst in &block.insts {
            // TODO: if-def this.
            write!(self.code, "     // {inst:?}").unwrap();
            if let Bc::AddrVar { id } = inst {
                if let Some(Some(name)) = self.body.var_names.get(*id as usize) {
                    write!(self.code, " {}", self.program.pool.get(name.name)).unwrap();
                }
            }
            writeln!(self.code).unwrap();

            match *inst {
                Bc::GetCompCtx => {
                    // err!("ICE: GetCompCtx at runtime doesn't make sense",)
                    self.stack.push(0);
                    self.code.push_str("GetCompCtx at runtime doesn't make sense. you probably forgot to mark something that should only be called at compile-time as #fold\n");
                }
                Bc::NameFlatCallArg { id, offset_bytes } => {
                    assert!(self.is_flat);
                    debug_assert_eq!(id as usize, self.flat_args_already_offset.len());
                    let var = self.next_var();
                    writeln!(self.code, "    _{var} = _flat_arg_ptr + {offset_bytes};").unwrap();
                    self.flat_args_already_offset.push(var);
                }
                // TODO: tail. at least warn if it was forced?
                Bc::CallDirect { f, .. } => {
                    let f_ty = self.program[f].unwrap_ty();
                    render_typedef(self.program, self.result, f_ty.arg)?;
                    render_typedef(self.program, self.result, f_ty.ret)?;

                    let name = self.program.pool.get(self.program[f].name);
                    self.do_c_call(f_ty, |s| {
                        write!(s.code, "/*{name}*/ _FN{}", f.as_index()).unwrap();
                    })?;
                    if f_ty.ret.is_never() {
                        break;
                    }

                    // self.cast_ret_from_float(builder, ret_val.len() as u16, ret.float_mask);
                }
                Bc::CallDirectFlat { f } => {
                    let f_ty = self.program[f].unwrap_ty();
                    assert_eq!(self.program[f].cc.unwrap(), CallConv::Flat);
                    let name = self.program.pool.get(self.program[f].name);
                    self.do_flat_call(f_ty, |s| write!(s.code, "/*{name}*/ _FN{}", f.as_index()).unwrap());
                    if f_ty.ret.is_never() {
                        break;
                    }
                }
                Bc::CallFnPtr { ty, cc } => {
                    let ptr_ty = self.program.intern_type(TypeInfo::FnPtr { ty, cc });
                    render_typedef(self.program, self.result, ptr_ty)?;
                    match cc {
                        CallConv::CCallReg => self.do_c_call(ty, |s| {
                            let callee = s.stack.pop().unwrap();
                            write!(s.code, "((_TY{})_{callee})", ptr_ty.as_index()).unwrap();
                        })?,
                        CallConv::Flat => self.do_flat_call(ty, |s| {
                            let callee = s.stack.pop().unwrap();
                            write!(s.code, "((_TY{})_{callee})", ptr_ty.as_index()).unwrap();
                        }),
                        _ => todo!(),
                    }
                    if ty.ret.is_never() {
                        break;
                    }
                }
                Bc::PushConstant { value } => {
                    let out = self.next_var();
                    writeln!(self.code, "    _{out} = (void *) {value};").unwrap();
                    self.stack.push(out);
                }
                Bc::PushGlobalAddr { id } => {
                    let var = self.next_var();
                    writeln!(self.code, "    _{var} = &_const{};", id.0).unwrap();
                    self.stack.push(var);
                }
                Bc::JumpIf { true_ip, false_ip, slots } => {
                    debug_assert_eq!(slots, 0);
                    let cond = self.stack.pop().unwrap();
                    let stack = self.stack.clone();
                    writeln!(self.code, "    if (_{cond}) goto _lbl{}; else goto _lbl{};", true_ip.0, false_ip.0).unwrap();
                    self.emit_block(true_ip.0 as usize)?;
                    self.stack = stack;
                    self.emit_block(false_ip.0 as usize)?;
                    break;
                }
                Bc::Goto { ip, slots } => {
                    let args = &self.stack[self.stack.len() - slots as usize..self.stack.len()];
                    for (i, var) in args.iter().enumerate() {
                        writeln!(self.code, "    _{i} = _{var};").unwrap();
                    }
                    writeln!(self.code, "    goto _lbl{};", ip.0).unwrap();
                    pops(&mut self.stack, slots as usize);
                    self.emit_block(ip.0 as usize)?;
                    break;
                }
                Bc::Ret => {
                    if self.is_flat {
                        // flat_call so we must have already put the values there.
                        writeln!(self.code, "    return;").unwrap();
                    } else {
                        let ret_ty = self.program[self.f].finished_ret.unwrap();
                        let ret = self.program.get_info(ret_ty);
                        // self.cast_args_to_float(builder, ret.size_slots, ret.float_mask);

                        match ret.size_slots {
                            0 => writeln!(self.code, "    return;").unwrap(),
                            1 => {
                                let v = self.stack.pop().unwrap();
                                writeln!(self.code, "    return (_TY{}) _{v};", ret_ty.as_index()).unwrap()
                            }
                            2 => {
                                let snd = self.stack.pop().unwrap();
                                let fst = self.stack.pop().unwrap();
                                writeln!(self.code, "    return (_TY{}) {{ _{fst}, _{snd} }};", ret_ty.as_index()).unwrap()
                            }
                            _ => err!("ICE: emit_bc never does this. it used flat call instead.",),
                        };
                    }
                    break;
                }
                Bc::GetNativeFnPtr(f) => {
                    // TODO: it might not be in the callees because it might be from an emit_relocatable_pointer because someone used a @static as a hacky forward declaration.
                    let out = self.next_var();
                    writeln!(self.code, "    _{out} = (void*) &_FN{};", f.as_index()).unwrap();
                    self.stack.push(out);
                }
                Bc::Load { slots } => {
                    debug_assert_ne!(slots, 0);
                    let addr = self.stack.pop().unwrap();
                    for s in 0..slots {
                        let v = self.next_var();
                        writeln!(self.code, "    _{v} = *(void**) (_{addr} + {});", s as i32 * 8).unwrap();
                        self.stack.push(v);
                    }
                }
                Bc::StorePost { slots } => {
                    debug_assert_ne!(slots, 0);
                    let addr = self.stack.pop().unwrap();
                    for s in 0..slots {
                        let v = self.stack[self.stack.len() - slots as usize + s as usize];
                        writeln!(self.code, "    *(void**) (_{addr} + {}) = _{v};", s as i32 * 8).unwrap();
                    }
                    pops(&mut self.stack, slots as usize);
                }
                Bc::StorePre { slots } => {
                    debug_assert_ne!(slots, 0);
                    let addr = self.stack[self.stack.len() - slots as usize - 1];
                    for s in 0..slots {
                        let v = self.stack[self.stack.len() - slots as usize + s as usize];
                        writeln!(self.code, "    *(void**) (_{addr} + {}) = _{v};", s as i32 * 8).unwrap();
                    }
                    pops(&mut self.stack, slots as usize + 1);
                }
                Bc::AddrVar { id } => {
                    if let Some(&ptr) = self.flat_args_already_offset.get(id as usize) {
                        self.stack.push(ptr);
                    } else {
                        self.stack.push(id as usize + STACK_ARG_SLOTS);
                    }
                }
                Bc::IncPtrBytes { bytes } => {
                    let ptr = self.stack.pop().unwrap();
                    let out = self.next_var();
                    writeln!(self.code, "    _{out} = _{ptr} + {bytes};").unwrap();
                    self.stack.push(out);
                }
                Bc::Pop { slots } => {
                    pops(&mut self.stack, slots as usize);
                }
                Bc::TagCheck { expected: _ } => {} // TODO: !!!
                Bc::Unreachable => {
                    writeln!(self.code, "    abort();").unwrap();
                    break;
                }
                Bc::NoCompile => err!("NoCompile",),
                Bc::LastUse { .. } | Bc::Noop => {}
                Bc::AddrFnResult => {
                    let var = self.next_var();
                    writeln!(self.code, "    _{var} = _flat_ret_ptr;").unwrap();
                    self.stack.push(var)
                }
                Bc::Dup => {
                    self.stack.push(*self.stack.last().unwrap());
                }
                Bc::CopyBytesToFrom { bytes } => {
                    let from = self.stack.pop().unwrap();
                    let to = self.stack.pop().unwrap();
                    // TODO: is this worth it?
                    // if bytes == 1 {
                    //     // HACK that makes bool vars less stupid
                    //     writeln!(self.code, "  *(cast)  _{to} = *(cast){from};").unwrap();
                    // }
                    if bytes % 8 == 0 && bytes <= 32 {
                        for i in 0..bytes / 8 {
                            writeln!(self.code, "    *(((void**)_{to}) + {i}) = *(((void**)_{from}) + {i});").unwrap();
                        }
                    } else {
                        writeln!(self.code, "    memcpy(_{to}, _{from}, {bytes});").unwrap();
                    }
                }
            }
        }
        Ok(())
    }

    fn next_var(&mut self) -> usize {
        self.var_id += 1;
        self.var_id - 1
    }

    // (pop args), write_fn_ref, (push ret)
    fn do_c_call(&mut self, f_ty: FnType, write_fn_ref: impl FnOnce(&mut Self)) -> Res<'p, ()> {
        let (arg, ret) = self.program.get_infos(f_ty);
        // self.cast_args_to_float(builder, arg.size_slots, arg.float_mask);

        let ret_var = self.next_var();
        match ret.size_slots {
            0 => write!(self.code, "    ").unwrap(),
            // TODO: bit cast float??
            1 => write!(self.code, "    _{ret_var} = (void*)").unwrap(),
            2 => write!(self.code, "    {{ _TY{} tmp = ", f_ty.ret.as_index()).unwrap(),
            _ => err!("ICE: emit bc never does this. it used flat call instead",),
        }
        // TODO: dumb to allocate but its easier if the fn ptr version can pop from the stack.
        let args = &self.stack[self.stack.len() - arg.size_slots as usize..self.stack.len()].to_vec();
        pops(&mut self.stack, arg.size_slots as usize);
        write_fn_ref(self);
        write!(self.code, "(").unwrap();

        if !args.is_empty() {
            let types = self.program.flat_tuple_types(f_ty.arg);
            assert_eq!(types.len(), args.len(), "{}", self.program.log_type(f_ty.arg));
            for (arg, ty) in args.iter().zip(types) {
                write!(self.code, "(_TY{}) _{arg},", ty.as_index()).unwrap();
            }
            self.code.remove(self.code.len() - 1); // comma
        }

        writeln!(self.code, ");").unwrap();

        if ret.size_slots != 0 {
            match ret.size_slots {
                0 => {}
                // TODO: bit cast float??
                1 => {
                    self.stack.push(ret_var);
                }
                2 => {
                    let ret2 = self.next_var();
                    self.stack.push(ret_var);
                    self.stack.push(ret2);
                    writeln!(self.code, " _{ret_var} = tmp._0; _{ret2} = tmp._1; }} ").unwrap();
                }
                _ => unreachable!(),
            }
        }
        Ok(())
    }

    // (pop ptrs), write_fn_ref
    fn do_flat_call(&mut self, f_ty: FnType, write_fn_ref: impl FnOnce(&mut Self)) {
        // (compiler, arg_ptr, arg_len_i64s, ret_ptr, ret_len_i64)
        let (arg, ret) = self.program.get_infos(f_ty);
        let arg_ptr = self.stack.pop().unwrap();
        let ret_ptr = self.stack.pop().unwrap();
        write!(self.code, "    ").unwrap();
        // flat_call result goes into a variable somewhere, already setup by bc. so don't worry about return value here.
        write_fn_ref(self);
        writeln!(self.code, "(0, _{arg_ptr}, {}, _{ret_ptr}, {});", arg.stride_bytes, ret.stride_bytes).unwrap();
    }
}

// TODO: forward declare for self referential
fn render_typedef(program: &mut Program, out: &mut CProgram, ty: TypeId) -> Res<'static, ()> {
    if out.type_forward.get(ty.as_index()) {
        return Ok(());
    }
    out.type_forward.set(ty.as_index());
    match &program[ty] {
        TypeInfo::Unknown => err!("unknown",),
        TypeInfo::Never => {
            // TODO: put _Noreturn on functions.
            writeln!(out.types, "typedef void _TY{};", ty.as_index()).unwrap();
        }
        TypeInfo::F64 => {
            writeln!(out.types, "typedef double _TY{};", ty.as_index()).unwrap();
        }
        TypeInfo::Int(int) => {
            write!(out.types, "typedef").unwrap();
            if int.signed {
                write!(out.types, " unsigned").unwrap();
            } else {
                write!(out.types, " signed").unwrap();
            }
            // TODO: use the fixed width types.
            match int.bit_count {
                8 => write!(out.types, " char").unwrap(),
                16 => write!(out.types, " short").unwrap(),
                32 => write!(out.types, " int").unwrap(),
                _ => write!(out.types, " long").unwrap(),
            }
            writeln!(out.types, " _TY{};", ty.as_index()).unwrap();
        }
        TypeInfo::Bool => {
            writeln!(out.types, "typedef _Bool _TY{};", ty.as_index()).unwrap();
        }
        &TypeInfo::FnPtr { ty: f, cc } => {
            assert!(cc == CallConv::CCallReg); // TODO: flat call sig
            render_typedef(program, out, f.arg)?;
            render_typedef(program, out, f.ret)?;

            write!(out.types, "typedef _TY{} (*_TY{})(", f.ret.as_index(), ty.as_index()).unwrap();
            if !f.arg.is_unit() {
                for ty in program.flat_tuple_types(f.arg) {
                    write!(out.types, "_TY{},", ty.as_index()).unwrap();
                }
                out.types.remove(out.types.len() - 1); // comma
            }
            writeln!(out.types, ");").unwrap();
        }
        &TypeInfo::Ptr(inner) => {
            render_typedef(program, out, inner)?;
            writeln!(out.types, "typedef _TY{} *_TY{};", inner.as_index(), ty.as_index()).unwrap();
        }
        &TypeInfo::Array { inner, len } => {
            render_typedef(program, out, inner)?;
            writeln!(out.types, "typedef _TY{} _TY{}[{len}];", inner.as_index(), ty.as_index()).unwrap();
        }
        TypeInfo::Struct { fields, .. } => {
            let fields = fields.clone();
            for f in &fields {
                render_typedef(program, out, f.ty)?;
            }
            write!(out.types, "typedef struct {{ ").unwrap();
            for (i, f) in fields.iter().enumerate() {
                if !f.ty.is_unit() {
                    write!(out.types, "_TY{} _{}; ", f.ty.as_index(), i).unwrap();
                }
            }
            writeln!(out.types, "}} _TY{};", ty.as_index()).unwrap();
        }
        TypeInfo::Tagged { cases } => {
            let cases = cases.clone();
            for c in &cases {
                render_typedef(program, out, c.1)?;
            }
            write!(out.types, "typedef struct {{ long tag; union {{").unwrap();
            for (i, c) in cases.iter().enumerate() {
                if !c.1.is_unit() {
                    write!(out.types, "_TY{} _{}; ", c.1.as_index(), i).unwrap();
                }
            }
            writeln!(out.types, "}}; }} _TY{};", ty.as_index()).unwrap();
        }
        &TypeInfo::Enum { raw: inner, .. } | &TypeInfo::Named(inner, _) | &TypeInfo::Unique(inner, _) => {
            render_typedef(program, out, inner)?;
            writeln!(out.types, "typedef _TY{} _TY{};", inner.as_index(), ty.as_index()).unwrap();
        }
        TypeInfo::Unit => {
            writeln!(out.types, "typedef void _TY{};", ty.as_index()).unwrap();
        }
        TypeInfo::VoidPtr => {
            writeln!(out.types, "typedef void *_TY{};", ty.as_index()).unwrap();
        }
        TypeInfo::Type | TypeInfo::OverloadSet | TypeInfo::Scope | TypeInfo::Fn(_) | TypeInfo::Label(_) => {
            writeln!(out.types, "typedef unsigned int _TY{};", ty.as_index()).unwrap();
        }
    }

    Ok(())
}
