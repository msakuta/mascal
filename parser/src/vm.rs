//! Bytecode interpreter, aka a Virtual Machine.

use std::collections::HashMap;

use crate::{
    bytecode::{Bytecode, FnBytecode, FnProto, FnProtos, OpCode},
    interpreter::{
        binary_op, binary_op_int, binary_op_str, coerce_f64, coerce_i64, coerce_type, truthy,
        EvalError, EvalResult,
    },
    type_decl::TypeDecl,
    Value,
};

macro_rules! dbg_println {
    ($($rest:tt)*) => {
        #[cfg(debug_assertions)]
        std::println!($($rest)*)
    }
}

pub fn interpret(bytecode: &Bytecode) -> Result<Value, EvalError> {
    if let Some(FnProto::Code(main)) = bytecode.functions.get("") {
        interpret_fn(main, &bytecode.functions)
    } else {
        Err(EvalError::NoMainFound)
    }
}

pub struct CallInfo<'a> {
    fun: &'a FnBytecode,
    ip: usize,
    stack_size: usize,
    stack_base: usize,
}

impl<'a> CallInfo<'a> {
    pub fn instuction_ptr(&self) -> usize {
        self.ip
    }

    pub fn has_next_inst(&self) -> bool {
        self.ip < self.fun.instructions.len()
    }
}

/// The virtual machine state run by the bytecode.
/// Now it is a full state machine that has complete information to suspend and resume at any time,
/// given that the lifetime of the functions are valid.
pub struct Vm<'a> {
    /// A stack for function call stack frames.
    stack: Vec<Value>,
    /// The stack base address of the currently running function.
    stack_base: usize,
    call_stack: Vec<CallInfo<'a>>,
    /// A special register to remember the target index in Set instruction, updated by SetReg instruction.
    /// Similar to x64's RSI or RDI, it indicates the index of the array to set, because we need more arguments than
    /// a fixed length arguments in an instruction for Set operation.
    set_register: usize,
    functions: &'a FnProtos,
}

impl<'a> Vm<'a> {
    fn new(bytecode: &'a FnBytecode, functions: &'a FnProtos) -> Self {
        let stack_size = bytecode.stack_size;
        Self {
            stack: vec![Value::I64(0); stack_size],
            call_stack: vec![CallInfo {
                fun: bytecode,
                ip: 0,
                stack_size: stack_size,
                stack_base: 0,
            }],
            stack_base: 0,
            set_register: 0,
            functions,
        }
    }

    pub fn start_main(bytecode: &'a Bytecode) -> EvalResult<Self> {
        if let Some(FnProto::Code(main)) = bytecode.functions.get("") {
            Ok(Self::new(main, &bytecode.functions))
        } else {
            Err(EvalError::NoMainFound)
        }
    }

    fn get(&self, idx: impl Into<usize>) -> &Value {
        &self.stack[self.stack_base + idx.into()]
    }

    fn get_mut(&mut self, idx: impl Into<usize>) -> &mut Value {
        &mut self.stack[self.stack_base + idx.into()]
    }

    fn set(&mut self, idx: impl Into<usize>, val: Value) {
        self.stack[self.stack_base + idx.into()] = val;
    }

    fn slice(&self, from: usize, to: usize) -> &[Value] {
        &self.stack[self.stack_base + from..self.stack_base + to]
    }

    fn dump_stack(&self) {
        dbg_println!(
            "stack[{}..{}]: {}",
            self.stack_base,
            self.stack.len(),
            self.stack[self.stack_base..]
                .iter()
                .fold("".to_string(), |acc, cur: &Value| {
                    if acc.is_empty() {
                        cur.to_string()
                    } else {
                        acc + ", " + &cur.to_string()
                    }
                })
        );
    }

    pub fn top_call_info(&self) -> Option<&CallInfo> {
        self.call_stack.last()
    }
}

/// An extension trait for `Vec` to write a shorthand for
/// `values.last().ok_or_else(|| EvalError::CallStackUndeflow)`, because
/// it's too long and shows up too often behind Rc.
pub(crate) trait CallStackLastExt<T> {
    fn clast(&self) -> EvalResult<&T>;
    fn clast_mut(&mut self) -> EvalResult<&mut T>;
}

impl<T> CallStackLastExt<T> for Vec<T> {
    fn clast(&self) -> EvalResult<&T> {
        self.last().ok_or_else(|| EvalError::CallStackUndeflow)
    }

    fn clast_mut(&mut self) -> EvalResult<&mut T> {
        self.last_mut().ok_or_else(|| EvalError::CallStackUndeflow)
    }
}

fn interpret_fn(
    bytecode: &FnBytecode,
    functions: &HashMap<String, FnProto>,
) -> Result<Value, EvalError> {
    dbg_println!("size inst: {}", std::mem::size_of::<crate::Instruction>());
    dbg_println!("size value: {}", std::mem::size_of::<Value>());
    dbg_println!(
        "size RefCell<Value>: {}",
        std::mem::size_of::<std::cell::RefCell<Value>>()
    );
    dbg_println!("size callInfo: {}", std::mem::size_of::<CallInfo>());
    dbg_println!("literals: {:?}", bytecode.literals);
    let mut vm = Vm::new(bytecode, functions);
    let value = loop {
        if let Some(res) = vm.next_inst()? {
            break res;
        }
    };
    Ok(value)
}

impl<'a> Vm<'a> {
    pub fn next_inst(&mut self) -> Result<Option<Value>, EvalError> {
        if !self.call_stack.clast()?.has_next_inst() {
            return Err(EvalError::PrematureEnd);
        }
        let ci = self.call_stack.clast()?;
        let ip = ci.ip;
        let inst = ci.fun.instructions[ip];

        dbg_println!("inst[{ip}]: {inst:?}");

        match inst.op {
            OpCode::LoadLiteral => {
                self.set(inst.arg1, ci.fun.literals[inst.arg0 as usize].clone());
            }
            OpCode::Move => {
                if let (Value::Array(lhs), Value::Array(rhs)) =
                    (self.get(inst.arg0), self.get(inst.arg1))
                {
                    if lhs as *const _ == rhs as *const _ {
                        println!("Self-assignment!");
                        self.call_stack.clast_mut()?.ip += 1;
                        return Ok(None);
                    }
                }
                let val = self.get(inst.arg0).clone();
                self.set(inst.arg1, val);
            }
            OpCode::Incr => {
                let val = self.get_mut(inst.arg0);
                fn incr(val: &mut Value) -> Result<(), String> {
                    match val {
                        Value::I64(i) => *i += 1,
                        Value::I32(i) => *i += 1,
                        Value::F64(i) => *i += 1.,
                        Value::F32(i) => *i += 1.,
                        _ => {
                            return Err(format!(
                                "Attempt to increment non-numerical value {:?}",
                                val
                            ))
                        }
                    }
                    Ok(())
                }
                incr(val)?;
            }
            OpCode::Add => {
                let result = binary_op_str(
                    &self.get(inst.arg0),
                    &self.get(inst.arg1),
                    |lhs, rhs| Ok(lhs + rhs),
                    |lhs, rhs| lhs + rhs,
                    |lhs: &str, rhs: &str| Ok(lhs.to_string() + rhs),
                )?;
                self.set(inst.arg0, result);
            }
            OpCode::Sub => {
                let result = binary_op(
                    &self.get(inst.arg0),
                    &self.get(inst.arg1),
                    |lhs, rhs| lhs - rhs,
                    |lhs, rhs| lhs - rhs,
                )?;
                self.set(inst.arg0, result);
            }
            OpCode::Mul => {
                let result = binary_op(
                    &self.get(inst.arg0),
                    &self.get(inst.arg1),
                    |lhs, rhs| lhs * rhs,
                    |lhs, rhs| lhs * rhs,
                )?;
                self.set(inst.arg0, result);
            }
            OpCode::Div => {
                let result = binary_op(
                    &self.get(inst.arg0),
                    &self.get(inst.arg1),
                    |lhs, rhs| lhs / rhs,
                    |lhs, rhs| lhs / rhs,
                )?;
                self.set(inst.arg0, result);
            }
            OpCode::BitAnd => {
                let result =
                    binary_op_int(&self.get(inst.arg0), &self.get(inst.arg1), |lhs, rhs| {
                        lhs & rhs
                    })?;
                self.set(inst.arg0, result);
            }
            OpCode::BitXor => {
                let result =
                    binary_op_int(&self.get(inst.arg0), &self.get(inst.arg1), |lhs, rhs| {
                        lhs ^ rhs
                    })?;
                self.set(inst.arg0, result);
            }
            OpCode::BitOr => {
                let result =
                    binary_op_int(&self.get(inst.arg0), &self.get(inst.arg1), |lhs, rhs| {
                        lhs | rhs
                    })?;
                self.set(inst.arg0, result);
            }
            OpCode::And => {
                let result = truthy(&self.get(inst.arg0)) && truthy(&self.get(inst.arg1));
                self.set(inst.arg0, Value::I32(result as i32));
            }
            OpCode::Or => {
                let result = truthy(&self.get(inst.arg0)) || truthy(&self.get(inst.arg1));
                self.set(inst.arg0, Value::I32(result as i32));
            }
            OpCode::Not => {
                let result = !truthy(&self.get(inst.arg0));
                self.set(inst.arg0, Value::I32(result as i32));
            }
            OpCode::BitNot => {
                let val = self.get(inst.arg0);
                let result = match val {
                    Value::I32(i) => Value::I32(!i),
                    Value::I64(i) => Value::I64(!i),
                    _ => return Err(EvalError::NonIntegerBitwise(format!("{val:?}"))),
                };
                self.set(inst.arg0, result);
            }
            OpCode::Get => {
                let target_collection = &self.get(inst.arg0);
                let target_index = &self.get(inst.arg1);
                let index = coerce_i64(target_index)? as u64;
                let new_val = target_collection.array_get(index).or_else(|_| {
                    target_collection.tuple_get(index)
                }).map_err(|e| {
                    format!("Get instruction failed with {target_collection:?} and {target_index:?}: {e:?}")
                })?;
                self.set(inst.arg1, new_val);
            }
            OpCode::Set => {
                let target_collection = &self.get(inst.arg0);
                let value = self.get(inst.arg1);
                let index = self.set_register;
                target_collection.array_assign(index, value.clone())?;
            }
            OpCode::SetReg => {
                self.set_register = coerce_i64(self.get(inst.arg0 as usize))? as usize;
            }
            OpCode::Lt => {
                let result = compare_op(
                    &self.get(inst.arg0),
                    &self.get(inst.arg1),
                    |lhs, rhs| lhs.lt(&rhs),
                    |lhs, rhs| lhs.lt(&rhs),
                )?;
                self.set(inst.arg0, Value::I64(result as i64));
            }
            OpCode::Gt => {
                let result = compare_op(
                    &self.get(inst.arg0),
                    &self.get(inst.arg1),
                    |lhs, rhs| lhs.gt(&rhs),
                    |lhs, rhs| lhs.gt(&rhs),
                )?;
                self.set(inst.arg0, Value::I64(result as i64));
            }
            OpCode::Jmp => {
                dbg_println!("[{ip}] Jumping by Jmp to {}", inst.arg1);
                self.call_stack.clast_mut()?.ip = inst.arg1 as usize;
                return Ok(None);
            }
            OpCode::Jt => {
                if truthy(&self.get(inst.arg0)) {
                    dbg_println!("[{ip}] Jumping by Jt to {}", inst.arg1);
                    self.call_stack.clast_mut()?.ip = inst.arg1 as usize;
                    return Ok(None);
                }
            }
            OpCode::Jf => {
                if !truthy(&self.get(inst.arg0)) {
                    dbg_println!("[{ip}] Jumping by Jf to {}", inst.arg1);
                    self.call_stack.clast_mut()?.ip = inst.arg1 as usize;
                    return Ok(None);
                }
            }
            OpCode::Call => {
                let arg_name = self.get(inst.arg1);
                let arg_name = if let Value::Str(s) = arg_name {
                    s
                } else {
                    return Err(EvalError::NonNameFnRef(format!("{arg_name:?}")));
                };
                let fun = self.functions.iter().find(|(fname, _)| *fname == arg_name);
                if let Some((_, fun)) = fun {
                    match fun {
                        FnProto::Code(fun) => {
                            dbg_println!("Calling code function with stack size (base:{}) + (fn: 1) + (params: {}) + (cur stack:{})", inst.arg1, inst.arg0, fun.stack_size);
                            // +1 for function name and return slot
                            self.stack_base += inst.arg1 as usize;
                            self.stack.resize(
                                self.stack_base + inst.arg0 as usize + fun.stack_size + 1,
                                Value::default(),
                            );
                            self.call_stack.push(CallInfo {
                                fun,
                                ip: 0,
                                stack_size: self.stack.len(),
                                stack_base: self.stack_base,
                            });
                            return Ok(None);
                        }
                        FnProto::Native(nat) => {
                            let ret = nat(&self.slice(
                                inst.arg1 as usize + 1,
                                inst.arg1 as usize + 1 + inst.arg0 as usize,
                            ));
                            self.set(inst.arg1, ret?);
                        }
                    }
                } else {
                    return Err(EvalError::FnNotFound(arg_name.clone()));
                }
            }
            OpCode::Ret => {
                let retval = self.stack_base + inst.arg1 as usize;
                if let Some(prev_ci) = self.call_stack.pop() {
                    if self.call_stack.is_empty() {
                        return Ok(Some(self.get(inst.arg1).clone()));
                    } else {
                        let ci = self.call_stack.clast()?;
                        self.stack_base = ci.stack_base;
                        self.stack[prev_ci.stack_base] = self.stack[retval].clone();
                        self.stack.resize(ci.stack_size, Value::default());
                        self.dump_stack();
                    }
                } else {
                    return Err(EvalError::CallStackUndeflow);
                }
            }
            OpCode::Cast => {
                let target_var = &self.get(inst.arg0);
                let target_type = coerce_i64(self.get(inst.arg1))
                    .map_err(|e| format!("arg1 of Cast was not a number: {e:?}"))?;
                let tt_buf = target_type.to_le_bytes();
                let tt = TypeDecl::deserialize(&mut &tt_buf[..])
                    .map_err(|e| format!("arg1 of Cast was not a TypeDecl: {e:?}"))?;
                let new_val = coerce_type(target_var, &tt)?;
                self.set(inst.arg0, new_val);
            }
        }

        self.dump_stack();

        self.call_stack.clast_mut()?.ip += 1;

        Ok(None)
    }

    // dbg_println!("Final stack: {:?}", self.stack);
    // Ok(Some(Value::I64(0))
}

fn compare_op(
    lhs: &Value,
    rhs: &Value,
    d: impl Fn(f64, f64) -> bool,
    i: impl Fn(i64, i64) -> bool,
) -> Result<bool, EvalError> {
    Ok(match (lhs.clone(), rhs.clone()) {
        (Value::F64(lhs), rhs) => d(lhs, coerce_f64(&rhs)?),
        (lhs, Value::F64(rhs)) => d(coerce_f64(&lhs)?, rhs),
        (Value::F32(lhs), rhs) => d(lhs as f64, coerce_f64(&rhs)?),
        (lhs, Value::F32(rhs)) => d(coerce_f64(&lhs)?, rhs as f64),
        (Value::I64(lhs), Value::I64(rhs)) => i(lhs, rhs),
        (Value::I64(lhs), Value::I32(rhs)) => i(lhs, rhs as i64),
        (Value::I32(lhs), Value::I64(rhs)) => i(lhs as i64, rhs),
        (Value::I32(lhs), Value::I32(rhs)) => i(lhs as i64, rhs as i64),
        _ => return Err(EvalError::OpError(lhs.to_string(), rhs.to_string())),
    })
}

#[cfg(test)]
mod test;
