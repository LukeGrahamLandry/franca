//! This generates a text file that a C compiler can convert into an executable.
//! If you look at the output you'll see why I'm reluctant to refer to it as a C program.
//! And now a small blessing: May the code in this file be as ugly as the code it generates.

// TODO: safe fn names. '//' is not enough, am i totally sure i never leave a new line in there?
// TODO: cast fn ptr arguments
// TODO: functions with const arguments that evaluate to const value body should be inlined in compiler.
//       currently each unique character constant emits a function. im sure llvm will inline it but its still dumb to make it waste its time on that.
use crate::{
    ast::{CallConv, Flag, FuncId, FuncImpl, Program, TypeId, TypeInfo},
    bc::{is_float, BakedVar, Bc, FnBody, Prim, PrimSig},
    compiler::{Compile, ExecStyle, Res},
    emit_bc::{emit_bc, prim_sig},
    err,
    logging::PoolLog,
    pops,
    reflect::BitSet,
};
use std::{collections::HashMap, fmt::Write};

// try put 'static' on everything that's not exported beacuse that makes clang -O2 not leave them in the exe if it decides to inline.
fn declare(comp: &Compile, out: &mut String, f: FuncId, use_name: bool, use_arg_names: bool) {
    let name = comp.program.pool.get(comp.program[f].name);
    let ty = comp.program[f].finished_ty().unwrap();
    let sig = prim_sig(comp.program, ty, comp.program[f].cc.unwrap()).unwrap();

    // TODO: use sig.no_return for _Noreturn or whatever the syntax is
    if use_name {
        write!(out, "{} {name}(", ret_spec(sig)).unwrap();
    } else {
        write!(
            out,
            "/* fn {name} -> {} */ static \n{} _FN{}(",
            comp.program.log_type(comp.program[f].finished_ret.unwrap()),
            ret_spec(sig),
            f.as_index()
        )
        .unwrap();
    }
    if use_arg_names {
        if sig.first_arg_is_indirect_return {
            write!(out, "void *_ret_ptr,").unwrap();
        }
        for b in comp.program[f].arg.bindings.iter() {
            let ty = b.ty.unwrap();
            if ty.is_unit() {
                continue;
            }
            let name = comp.program.pool.get(b.name().unwrap());
            if let TypeInfo::Ptr(inner) = comp.program[ty] {
                let prim = comp.program.prim(inner);
                write!(out, "{} *{},", c_type_spec(prim), name).unwrap();
            } else {
                let prim = comp.program.prim(ty);
                write!(out, "{} {},", c_type_spec(prim), name).unwrap();
            }
        }
    } else {
        for i in 0..sig.arg_slots as usize {
            write!(out, "{} _arg_{i},", ty_spec(is_float(i, sig.arg_slots, sig.arg_float_mask))).unwrap();
        }
    }
    if out.ends_with(',') {
        out.remove(out.len() - 1); // comma
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
                    flat_args_already_offset: Default::default(),
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

    out.types.push_str("typedef struct { void *_0; void *_1; } Reti64i64;\n");
    out.types.push_str("typedef struct { void *_0; double _1; } Reti64f64;\n");
    out.types.push_str("typedef struct { double _0; void *_1; } Retf64i64;\n");
    out.types.push_str("typedef struct { double _0; double _1; } Retf64f64;\n");

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
                // HACK: because i use single field structs for unique types but flaten them to ints for args. really c should get the real struct definitions.
                if fields.len() > 1 {
                    for i in 0..fields.len() {
                        write!(out, "{name}._{i},").unwrap();
                    }
                    continue;
                }
            }
            write!(out, "{name},").unwrap();
        }
        out.remove(out.len() - 1); // comma
    }
    writeln!(out, ");").unwrap();
    writeln!(out, "}}").unwrap();
}

// TODO: this uses a dumb amount of memory
#[derive(Default)]
struct CProgram {
    fn_forward: BitSet, // anything called mutually recursivly needs to be forward declared
    fn_emitted: BitSet, // has the implementation been emitted?
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
    flat_args_already_offset: HashMap<u16, Val>,
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
                // TODO: tail. at least warn if it was forced?
                Bc::CallDirect { f, sig, .. } => {
                    let name = self.program.pool.get(self.program[f].name);
                    self.do_c_call(sig, |s| {
                        write!(s.code, "/*{name}*/ _FN{}", f.as_index()).unwrap();
                    })?;
                    if sig.no_return {
                        break;
                    }
                }
                Bc::CallFnPtr { sig } => {
                    self.do_c_call(sig, |s| {
                        let callee = s.stack.pop().unwrap();
                        write!(s.code, "(({}(*)(", ret_spec(sig)).unwrap();
                        for i in 0..sig.arg_slots as usize {
                            write!(s.code, "{} _arg_{i},", ty_spec(is_float(i, sig.arg_slots, sig.arg_float_mask))).unwrap();
                        }
                        if s.code.ends_with(',') {
                            s.code.remove(s.code.len() - 1); // comma
                        }
                        write!(s.code, ")) {callee})").unwrap();
                    })?;
                    if sig.no_return {
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
                Bc::Ret1(_) => {
                    let v = self.stack.pop().unwrap();
                    writeln!(self.code, "    return {v};").unwrap();
                    break;
                }
                Bc::Ret2(_) => {
                    let snd = self.stack.pop().unwrap();
                    let fst = self.stack.pop().unwrap();
                    writeln!(self.code, "    return ({}) {{ {fst}, {snd} }};", ret_spec(self.body.signeture)).unwrap();
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
                    self.stack.push(Val::of_var(id as usize + STACK_ARG_SLOTS));
                }
                Bc::SaveSsa { id, .. } => {
                    self.flat_args_already_offset.insert(id, self.stack.pop().unwrap());
                }
                Bc::LoadSsa { id, .. } => {
                    let val = self.flat_args_already_offset.get(&id).unwrap().clone();
                    self.stack.push(val);
                }
                Bc::IncPtrBytes { bytes } => {
                    self.stack.last_mut().unwrap().offset += bytes;
                }
                Bc::Unreachable => {
                    writeln!(self.code, "    abort();").unwrap();
                    break;
                }
                Bc::NoCompile => err!("NoCompile",),
                Bc::LastUse { .. } => {}
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
    fn do_c_call(&mut self, sig: PrimSig, write_fn_ref: impl FnOnce(&mut Self)) -> Res<'p, ()> {
        let ret_var = self.next_var();
        match sig.ret_slots {
            // TODO: bit cast float??
            1 => write!(self.code, "    _{ret_var} = (void*)").unwrap(),
            2 => write!(self.code, "    {{ {} tmp = ", ret_spec(sig)).unwrap(),
            _ => write!(self.code, "    ").unwrap(),
        }
        // TODO: dumb to allocate but its easier if the fn ptr version can pop from the stack.
        let args = &self.stack[self.stack.len() - sig.arg_slots as usize..self.stack.len()].to_vec();
        pops(&mut self.stack, sig.arg_slots as usize);
        write_fn_ref(self);
        write!(self.code, "(").unwrap();

        if !args.is_empty() {
            for (i, arg) in args.iter().enumerate() {
                // TODO: float bit cast
                let _ = is_float(i, sig.arg_slots, sig.arg_float_mask);
                write!(self.code, "{arg},").unwrap();
            }
            self.code.remove(self.code.len() - 1); // comma
        }

        writeln!(self.code, ");").unwrap();

        match sig.ret_slots {
            0 => {}
            // TODO: bit cast float??
            1 => self.stack.push(Val::of_var(ret_var)),
            2 => {
                let ret2 = self.next_var();
                self.stack.push(Val::of_var(ret_var));
                self.stack.push(Val::of_var(ret2));
                writeln!(self.code, " _{ret_var} = tmp._0; _{ret2} = tmp._1; }} ").unwrap();
            }
            _ => unreachable!(),
        }
        Ok(())
    }
}

fn ty_spec(is_float: bool) -> &'static str {
    if is_float {
        "double"
    } else {
        "void*"
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

fn ret_spec(sig: PrimSig) -> &'static str {
    match sig.ret_slots {
        0 => "void",
        1 => ty_spec(is_float(0, sig.ret_slots, sig.ret_float_mask)),
        2 => {
            let fst = is_float(0, sig.ret_slots, sig.ret_float_mask);
            let snd = is_float(1, sig.ret_slots, sig.ret_float_mask);
            match (fst, snd) {
                (true, true) => "Retf64f64",
                (true, false) => "Retf64i64",
                (false, true) => "Reti64f64",
                (false, false) => "Reti64i64",
            }
        }
        _ => unreachable!(),
    }
}
