use crate::{
    ast::{Func, FuncId, Program, TypeId},
    pool::StringPool,
};

#[derive(Clone, PartialEq)]
pub enum Value {
    F64(f64),
    I64(i64),
    Bool(bool),
    Enum {
        container_type: TypeId,
        tag: usize,
        value: Box<Self>,
    },
    Tuple {
        container_type: TypeId,
        values: Vec<Self>,
    },
    Array {
        container_type: TypeId,
        values: Vec<Self>,
    },
    Ptr {
        container_type: TypeId,
        value: *mut Self,
    },
    // Both closures and types don't have values at runtime, all uses must be inlined.
    Fn(TypeId, usize),
    Type(TypeId),
    Unit,
}

#[derive(Debug, Copy, Clone)]
struct StackOffset(usize);

#[derive(Debug, Copy, Clone)]
enum IntrinsicFn {
    AddI64,
    SubI64,
    MulI64,
    DivI64,
    AddF64,
    SubF64,
    MulF64,
    DivF64,
    IsNan,
    EqI64,
    EqF64,
    EqBool,
    RawEqPtr,
    Malloc,
    Free,
    Transmute,
    Debug,
    Stride,
    CastF64I64,
    CastI64F64,
    SetPtr,
    GetPtr,
    Unreachable,
    System,
    CliArgs,
    ReadAllFile,
    WriteAllFile,
    GetTimestamp,
}

enum Bc {
    Call {
        f: FuncId,
        return_slot: StackOffset,
        arg_slot: StackOffset,
        stack_size: usize,
    },
    CallIntrinsic {
        name: IntrinsicFn,
        return_slot: StackOffset,
        arg_slot: StackOffset,
    },
    LoadConstant {
        slot: StackOffset,
        value: Value,
    },
    JumpIf {
        cond: StackOffset,
        true_ip: usize,
        false_ip: usize,
    },
    Ret(StackOffset),
}

struct CallFrame {
    stack_base: usize,
    current_func: FuncId,
    current_ip: usize,
    return_slot: StackOffset, // relative to the previous base
}

// TODO: any time you try to call a function it might not be ready yet
//       and compiling it might require running other comptime functions
// TODO: bucket array for stack so you can take pointers into it
pub struct Interp<'a, 'p> {
    pool: &'a StringPool<'p>,
    value_stack: Vec<Value>,
    call_stack: Vec<CallFrame>,
    program: &'a mut Program<'p>,
    ready: Vec<Option<Vec<Bc>>>,
}

impl<'a, 'p> Interp<'a, 'p> {
    pub fn new(pool: &'a StringPool<'p>, program: &'a mut Program<'p>) -> Self {
        Self {
            pool,
            value_stack: vec![],
            call_stack: vec![],
            program,
            ready: vec![],
        }
    }

    pub fn run(&mut self, f: FuncId, arg: Value) -> Value {
        // TODO: typecheck
        self.value_stack.push(Value::Unit);
        self.value_stack.push(arg);
        self.call_stack.push(CallFrame {
            stack_base: 1,
            current_func: f,
            current_ip: 0,
            return_slot: StackOffset(0),
        });

        while self.value_stack.len() > 1 {
            let i = self.next_inst();
            match i {
                &Bc::Call {
                    f,
                    return_slot,
                    arg_slot,
                    stack_size,
                } => todo!(),
                Bc::LoadConstant { slot, value } => {
                    let slot = *slot;
                    let value = value.clone();
                    let frame = self.call_stack.last_mut().unwrap();
                    let slot = frame.stack_base + slot.0;
                    self.value_stack[slot] = value;
                    frame.current_ip += 1;
                }
                &Bc::JumpIf {
                    cond,
                    true_ip,
                    false_ip,
                } => {
                    let frame = self.call_stack.last_mut().unwrap();
                    let slot = frame.stack_base + cond.0;
                    if let Value::Bool(cond) = self.value_stack[slot] {
                        let next_ip = if cond { true_ip } else { false_ip };
                        frame.current_ip = next_ip;
                    } else {
                        panic!("ICE: JumpIf on {:?}", cond)
                    }
                }
                &Bc::CallIntrinsic {
                    name,
                    return_slot,
                    arg_slot,
                } => todo!(),
                &Bc::Ret(_) => todo!(),
            }
        }
        self.value_stack.pop().unwrap()
    }

    fn ensure_compiled(&mut self, FuncId(index): FuncId) {
        if let Some(Some(_)) = self.ready.get(index) {
            return;
        }
        while self.ready.len() <= index {
            self.ready.push(None);
        }

        let func = &self.program.funcs[index];

        todo!()
    }

    fn next_inst(&self) -> &Bc {
        todo!()
    }
}
