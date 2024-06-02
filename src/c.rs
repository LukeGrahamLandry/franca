//! This generates a text file that a C compiler can convert into an executable.
//! If you look at the output you'll see why I'm reluctant to refer to it as a C program.
//! And now a small blessing: May the code in this file be as ugly as the code it generates.

// TODO: safe fn names. '//' is not enough, am i totally sure i never leave a new line in there?
// TODO: cast fn ptr arguments
// TODO: functions with const arguments that evaluate to const value body should be inlined in compiler.
//       currently each unique character constant emits a function. im sure llvm will inline it but its still dumb to make it waste its time on that.
use crate::{
    ast::{CallConv, Flag, FnType, FuncId, FuncImpl, Program, TypeId, TypeInfo},
    bc::{BakedVar, Bc, FnBody, Prim},
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
    let has_indirect_return = comp.program.slot_count(ty.ret) > 2;

    if use_name {
        write!(out, "_TY{} {name}(", ty.ret.as_index()).unwrap();
    } else if has_indirect_return {
        write!(out, "/* fn {name} */ static \nvoid _FN{}(", f.as_index()).unwrap();
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
            if has_indirect_return {
                write!(out, "_TY{} *_arg_0,", ty.ret.as_index()).unwrap();
            }
            for (mut i, ty) in comp.program.flat_tuple_types(ty.arg).into_iter().enumerate() {
                if ty.is_unit() {
                    continue;
                }
                if has_indirect_return {
                    i += 1;
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

const FLAT_ARGS_SIGN: &str = "( void* _flat_ret_ptr, int _a, void* _flat_arg_ptr, int _b, int _c)";
pub fn emit_c<'p>(comp: &mut Compile<'_, 'p>, functions: Vec<FuncId>, test_runner_main: bool) -> Res<'p, String> {
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
                    write!(wip.result.functions, "void* _s{i}[{size}] = {{0}}; ").unwrap();
                }
                writeln!(wip.result.functions).unwrap();
                wip.emit_block(0)?;
                // it doesn't like if you declare variables after a goto label.
                write!(wip.result.functions, "    void ").unwrap();
                for var in 0..wip.var_id {
                    write!(wip.result.functions, "*_{var} = 0, ").unwrap();
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
            emit_named_redirect_body(comp, &mut out.forward, f, true);
        } else if let FuncImpl::Redirect(target) = comp.program[f].body {
            // TODO: the frontend should just add the target as a callee instead.
            // this works because emit_bc redirects calls to the target.
            emit(comp, out, target)?
        } else {
            println!("/* ERROR: No c compatible body for \n{}*/", comp.program[f].log(comp.pool))
        }

        Ok(())
    }

    // *int*_t because we have a little self respect remaining...
    out.types.push_str("#include <stdint.h>\n");

    // extra calls emitted by the compiler here
    let memcpy = comp.program.find_unique_func(comp.pool.intern("memcpy")).unwrap();
    comp.compile(memcpy, ExecStyle::Aot)?;
    emit(comp, &mut out, memcpy)?;

    for f in functions.iter().copied() {
        comp.compile(f, ExecStyle::Aot)?;
        emit(comp, &mut out, f)?;

        if comp.program[f].cc.unwrap() != CallConv::Flat && !test_runner_main {
            declare(comp, &mut out.functions, f, true, true);
            emit_named_redirect_body(comp, &mut out.functions, f, false);
        } else {
            writeln!(out.functions, "// Skip exporting flat call: fn {}", comp.pool.get(comp.program[f].name)).unwrap();
        }
    }

    if test_runner_main {
        writeln!(out.functions, "int main(){{").unwrap();
        for f in &functions {
            let name = comp.pool.get(comp.program[*f].name);
            let msg = format!("{name};\\n");
            writeln!(out.functions, "write(1, (int8_t*)\"{msg}\", {});", msg.len() - 1).unwrap();

            if comp.program[*f].finished_arg.unwrap().is_unit() {
                writeln!(out.functions, "_FN{}();", f.as_index()).unwrap();
            } else {
                writeln!(out.functions, "_FN{}(0);", f.as_index()).unwrap();
            }
        }
        let msg = format!("Passed {} tests.\\n", functions.len());
        writeln!(out.functions, "write(1, (int8_t*)\"{msg}\", {});", msg.len() - 1).unwrap();
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
                writeln!(constants, "    static uint8_t _const{i}[{}] = {{", len).unwrap();
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

fn emit_named_redirect_body(comp: &mut Compile, out: &mut String, f: FuncId, callee_use_name: bool) {
    writeln!(out, "{{").unwrap();
    if callee_use_name {
        let name = comp.program[f].name;
        write!(out, "return {}(", comp.pool.get(name)).unwrap();
    } else {
        write!(out, "return _FN{}(", f.as_index()).unwrap();
    }
    let ty = comp.program[f].finished_ty().unwrap();
    if !ty.arg.is_unit() {
        for b in &comp.program[f].arg.bindings {
            let name = comp.program.pool.get(b.name().unwrap());
            let raw = comp.program.raw_type(b.ty.unwrap());
            if let TypeInfo::Struct { fields, .. } = &comp.program[raw] {
                for i in 0..fields.len() {
                    write!(out, "{name}._{i},").unwrap();
                }
            } else {
                write!(out, "{name},").unwrap();
            }
        }
        out.remove(out.len() - 1); // comma
    }
    writeln!(out, ");").unwrap();
    writeln!(out, "}}").unwrap();
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
    stack: Vec<Val>,
    flat_args_already_offset: Vec<Val>,
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
            // debug_assert_eq!(arg.size_slots, block.arg_slots); // not true because of indirect returns
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
            self.stack.push(Val::of_var(i as usize))
        }
        for inst in &block.insts {
            // TODO: if-def this.
            // write!(self.code, "     // {inst:?}").unwrap();
            // if let Bc::AddrVar { id } = inst {
            //     if let Some(Some(name)) = self.body.var_names.get(*id as usize) {
            //         write!(self.code, " {}", self.program.pool.get(name.name)).unwrap();
            //     }
            // }
            // writeln!(self.code).unwrap();

            match *inst {
                Bc::GetCompCtx => {
                    // err!("ICE: GetCompCtx at runtime doesn't make sense",)
                    self.stack.push(Val::literal(0));
                    self.code.push_str("// GetCompCtx at runtime doesn't make sense. you probably forgot to mark something that should only be called at compile-time as #fold. actually flat call always emits this now, doesnt matter.\n");
                }
                Bc::NameFlatCallArg { id, offset_bytes } => {
                    assert!(self.is_flat);
                    debug_assert_eq!(id as usize, self.flat_args_already_offset.len());
                    self.flat_args_already_offset.push(Val {
                        _ty: Some(Prim::I64),
                        refer: "_flat_arg_ptr".to_string(),
                        offset: offset_bytes,
                    });
                }
                // TODO: tail. at least warn if it was forced?
                Bc::CallDirect { f, ty: f_ty, .. } => {
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
                Bc::CallFnPtr { ty, cc } => {
                    let ptr_ty = self.program.intern_type(TypeInfo::FnPtr { ty, cc });
                    render_typedef(self.program, self.result, ptr_ty)?;
                    debug_assert_ne!(cc, CallConv::CCallRegCt);
                    self.do_c_call(ty, |s| {
                        let callee = s.stack.pop().unwrap();
                        write!(s.code, "((_TY{}) {callee})", ptr_ty.as_index()).unwrap();
                    })?;
                    if ty.ret.is_never() {
                        break;
                    }
                }
                Bc::PushConstant { value } => {
                    self.stack.push(Val::literal(value));
                }
                Bc::PushGlobalAddr { id } => {
                    self.stack.push(Val {
                        _ty: None,
                        refer: format!("(&_const{})", id.0),
                        offset: 0,
                    });
                }
                Bc::JumpIf { true_ip, false_ip, slots } => {
                    debug_assert_eq!(slots, 0);
                    let cond = self.stack.pop().unwrap();
                    let stack = self.stack.clone();
                    writeln!(self.code, "    if ({cond}) goto _lbl{}; else goto _lbl{};", true_ip.0, false_ip.0).unwrap();
                    self.emit_block(true_ip.0 as usize)?;
                    self.stack = stack;
                    self.emit_block(false_ip.0 as usize)?;
                    break;
                }
                Bc::Goto { ip, slots } => {
                    let args = &self.stack[self.stack.len() - slots as usize..self.stack.len()];
                    for (i, var) in args.iter().enumerate() {
                        writeln!(self.code, "    _{i} = {var};").unwrap();
                    }
                    writeln!(self.code, "    goto _lbl{};", ip.0).unwrap();
                    pops(&mut self.stack, slots as usize);
                    self.emit_block(ip.0 as usize)?;
                    break;
                }
                Bc::Ret0 => {
                    writeln!(self.code, "    return;").unwrap();
                    break;
                }
                Bc::Ret1(prim) => {
                    let v = self.stack.pop().unwrap();
                    writeln!(self.code, "    return ({}) {v};", c_type_spec(prim)).unwrap();
                    break;
                }
                Bc::Ret2(_) => {
                    let snd = self.stack.pop().unwrap();
                    let fst = self.stack.pop().unwrap();
                    let ty = self.program[self.f].finished_ret.unwrap().as_index(); // :(... its not worse than it was before tho.
                    writeln!(self.code, "    return (_TY{ty}) {{ {fst}, {snd} }};",).unwrap();
                    break;
                }
                Bc::GetNativeFnPtr(f) => {
                    // TODO: it might not be in the callees because it might be from an emit_relocatable_pointer because someone used a @static as a hacky forward declaration.
                    assert!(self.program[self.f].callees.contains(&f));
                    self.stack.push(Val {
                        _ty: Some(Prim::I64),
                        refer: format!("&_FN{}", f.as_index()),
                        offset: 0,
                    });
                }
                Bc::Load { ty } => {
                    let v = self.next_var();
                    let addr = self.stack.pop().unwrap();
                    let ty = c_type_spec(ty);
                    writeln!(self.code, "    _{v} = *({ty}*) ({addr});").unwrap();
                    self.stack.push(Val::of_var(v));
                }
                Bc::StorePost { ty } => {
                    let addr = self.stack.pop().unwrap();
                    let value = self.stack.pop().unwrap();
                    let ty = c_type_spec(ty);
                    writeln!(self.code, "    *({ty}*) ({addr}) = ({ty}) {value};").unwrap();
                }
                Bc::StorePre { ty } => {
                    let value = self.stack.pop().unwrap();
                    let addr = self.stack.pop().unwrap();
                    let ty = c_type_spec(ty);
                    writeln!(self.code, "    *({ty}*) ({addr}) = ({ty}) {value};").unwrap();
                }
                Bc::AddrVar { id } => {
                    if let Some(ptr) = self.flat_args_already_offset.get(id as usize) {
                        self.stack.push(ptr.clone());
                    } else {
                        self.stack.push(Val::of_var(id as usize + STACK_ARG_SLOTS));
                    }
                }
                Bc::IncPtrBytes { bytes } => {
                    self.stack.last_mut().unwrap().offset += bytes;
                }
                Bc::TagCheck { expected: _ } => {} // TODO: !!!
                Bc::Unreachable => {
                    writeln!(self.code, "    abort();").unwrap();
                    break;
                }
                Bc::NoCompile => err!("NoCompile",),
                Bc::LastUse { .. } => {}
                Bc::AddrFnResult => {
                    self.stack.push(Val {
                        _ty: Some(Prim::I64),
                        refer: String::from("_flat_ret_ptr"),
                        offset: 0,
                    });
                }
                Bc::PeekDup(skip) => {
                    self.stack.push(self.stack[self.stack.len() - skip as usize - 1].clone());
                }
                Bc::Snipe(skip) => {
                    self.stack.remove(self.stack.len() - skip as usize - 1);
                }
                Bc::CopyBytesToFrom { bytes } => {
                    let from = self.stack.pop().unwrap();
                    let to = self.stack.pop().unwrap();
                    // TODO: don't call memcpy on every var assignment.
                    writeln!(self.code, "    memcpy({to}, {from}, {bytes});").unwrap();
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
        let (mut arg, ret) = self.program.get_infos(f_ty);
        // self.cast_args_to_float(builder, arg.size_slots, arg.float_mask);

        let ret_var = self.next_var();
        match ret.size_slots {
            // TODO: bit cast float??
            1 => write!(self.code, "    _{ret_var} = (void*)").unwrap(),
            2 => write!(self.code, "    {{ _TY{} tmp = ", f_ty.ret.as_index()).unwrap(),
            _ => write!(self.code, "    ").unwrap(),
        }
        if ret.size_slots > 2 {
            arg.size_slots += 1;
        }
        // TODO: dumb to allocate but its easier if the fn ptr version can pop from the stack.
        let args = &self.stack[self.stack.len() - arg.size_slots as usize..self.stack.len()].to_vec();
        pops(&mut self.stack, arg.size_slots as usize);
        write_fn_ref(self);
        write!(self.code, "(").unwrap();

        if !args.is_empty() {
            let mut types = self.program.flat_tuple_types(f_ty.arg);
            if ret.size_slots > 2 {
                // TODO: sad shifting
                types.insert(0, self.program.intern_type(TypeInfo::Ptr(f_ty.ret)));
                // indirect return
            }
            assert_eq!(types.len(), args.len(), "{}", self.program.log_type(f_ty.arg));
            for (arg, ty) in args.iter().zip(types) {
                write!(self.code, "(_TY{}) {arg},", ty.as_index()).unwrap();
            }
            self.code.remove(self.code.len() - 1); // comma
        }

        writeln!(self.code, ");").unwrap();

        // TODO: we know the types here.
        match ret.size_slots {
            // TODO: bit cast float??
            1 => self.stack.push(Val::of_var(ret_var)),
            2 => {
                let ret2 = self.next_var();
                self.stack.push(Val::of_var(ret_var));
                self.stack.push(Val::of_var(ret2));
                writeln!(self.code, " _{ret_var} = tmp._0; _{ret2} = tmp._1; }} ").unwrap();
            }
            _ => {}
        }
        Ok(())
    }
}

fn c_type_spec(ty: Prim) -> &'static str {
    match ty {
        Prim::I8 => "uint8_t",
        Prim::I16 => "uint16_t",
        Prim::I32 => "uint32_t",
        Prim::I64 => "uint64_t",
        Prim::F64 => "double",
        Prim::P64 => "void*",
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
                write!(out.types, " u").unwrap();
            } else {
                write!(out.types, " ").unwrap();
            }
            // TODO: use the fixed width types.
            match int.bit_count {
                8 => write!(out.types, "int8_t").unwrap(),
                16 => write!(out.types, "int16_t").unwrap(),
                32 => write!(out.types, "int32_t").unwrap(),
                _ => write!(out.types, "int64_t").unwrap(),
            }
            writeln!(out.types, " _TY{};", ty.as_index()).unwrap();
        }
        TypeInfo::Bool => {
            writeln!(out.types, "typedef uint8_t /*bool*/ _TY{};", ty.as_index()).unwrap();
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

#[derive(Debug, Clone)]
struct Val {
    _ty: Option<Prim>,
    refer: String,
    offset: u16,
}

impl Val {
    fn of_var(id: usize) -> Self {
        Self {
            _ty: None,
            refer: format!("_{id}"),
            offset: 0,
        }
    }
    fn literal(value: i64) -> Self {
        Self {
            _ty: None,
            refer: format!("{value:#0x}"),
            offset: 0,
        }
    }
}

impl std::fmt::Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.offset == 0 {
            write!(f, "{}", self.refer)
        } else {
            write!(f, "(((void*){}) + {})", self.refer, self.offset)
        }
    }
}
