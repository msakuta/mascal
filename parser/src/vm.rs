//! Bytecode interpreter, aka a Virtual Machine.

use std::{collections::HashMap, io::Write};

use crate::{
    bytecode::{Bytecode, FnBytecode, FnProto, FnProtos, OpCode},
    coercion::{coerce_i64, coerce_type},
    interpreter::{
        binary_op, binary_op_int, binary_op_str, compare_op, truthy, EvalError, EvalResult,
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

#[derive(Clone)]
pub struct CallInfo<'a> {
    fun: &'a FnBytecode,
    ip: usize,
    stack_size: usize,
    stack_base: usize,
}

impl<'a> CallInfo<'a> {
    pub fn bytecode(&self) -> &'a FnBytecode {
        self.fun
    }

    pub fn instruction_ptr(&self) -> usize {
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

    pub fn dump_stack(&self, f: &mut impl Write) -> std::io::Result<()> {
        writeln!(
            f,
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
        )
    }

    pub fn call_stack(&self) -> &[CallInfo<'_>] {
        &self.call_stack
    }

    pub fn call_info(&self, level: usize) -> Option<&CallInfo<'_>> {
        self.call_stack
            .get(self.call_stack.len().saturating_sub(level + 1))
    }

    pub fn format_current_inst(
        &self,
        f: &mut impl Write,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let ci = self.call_stack.clast()?;
        if !self.call_stack.clast()?.has_next_inst() {
            return Err("PrematureEnd".into());
        }
        let ip = ci.ip;
        let inst = ci.fun.instructions[ip];
        writeln!(f, "inst[{ip}]: {inst}")?;
        Ok(())
    }

    pub fn stack_trace(&self, level: usize, f: &mut impl Write) -> std::io::Result<()> {
        for (i, frame) in self.call_stack.iter().enumerate() {
            let name = if frame.fun.name.is_empty() {
                "<toplevel>"
            } else {
                &frame.fun.name
            };
            let current = if level == self.call_stack.len().saturating_sub(i + 1) {
                ">"
            } else {
                " "
            };
            writeln!(f, "{current}  [{i}]: {name} ip: {}", frame.ip)?;
        }
        Ok(())
    }

    pub fn print_stack(&self, f: &mut impl Write, level: usize) -> std::io::Result<()> {
        let Some(ci) = self.call_info(level) else {
            // Empty call stack is not an error
            return Ok(());
        };
        let top = (ci.stack_base + ci.stack_size).min(self.stack.len());
        for (i, value) in self.stack[ci.stack_base..top].iter().enumerate() {
            writeln!(f, "  [{i}] {value}")?;
        }
        Ok(())
    }

    pub fn iter_stack(&self, level: usize) -> Option<impl Iterator<Item = &Value>> {
        let Some(ci) = self.call_info(level) else {
            // Empty call stack is not an error
            return None;
        };
        let top = (ci.stack_base + ci.stack_size).min(self.stack.len());
        Some(self.stack[ci.stack_base..top].iter())
    }

    pub fn deepclone(&self) -> Self {
        Self {
            stack: self.stack.iter().map(|v| v.deepclone()).collect(),
            stack_base: self.stack_base,
            call_stack: self.call_stack.clone(),
            set_register: self.set_register,
            functions: self.functions,
        }
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
    #[cfg(debug_assertions)]
    {
        let mut buf = vec![0u8; 0];
        bytecode.disasm(&mut buf).unwrap();
        if let Ok(s) = String::from_utf8(buf) {
            dbg_println!("instructions: {}", s);
        }
    }
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
            OpCode::Neg => {
                let val = self.get(inst.arg0);
                let result = match val {
                    Value::I32(i) => Value::I32(-i),
                    Value::I64(i) => Value::I64(-i),
                    Value::F32(i) => Value::F32(-i),
                    Value::F64(i) => Value::F64(-i),
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
                let result =
                    compare_op(&self.get(inst.arg0), &self.get(inst.arg1), f64::lt, i64::lt)?;
                self.set(inst.arg0, result);
            }
            OpCode::Le => {
                let result =
                    compare_op(&self.get(inst.arg0), &self.get(inst.arg1), f64::le, i64::le)?;
                self.set(inst.arg0, result);
            }
            OpCode::Gt => {
                let result =
                    compare_op(&self.get(inst.arg0), &self.get(inst.arg1), f64::gt, i64::gt)?;
                self.set(inst.arg0, result);
            }
            OpCode::Ge => {
                let result =
                    compare_op(&self.get(inst.arg0), &self.get(inst.arg1), f64::ge, i64::ge)?;
                self.set(inst.arg0, result);
            }
            OpCode::Eq => {
                let result =
                    compare_op(&self.get(inst.arg0), &self.get(inst.arg1), f64::eq, i64::eq)?;
                self.set(inst.arg0, result);
            }
            OpCode::Jmp => {
                self.call_stack.clast_mut()?.ip = inst.arg1 as usize;
                return Ok(None);
            }
            OpCode::Jt => {
                if truthy(&self.get(inst.arg0)) {
                    self.call_stack.clast_mut()?.ip = inst.arg1 as usize;
                    return Ok(None);
                }
            }
            OpCode::Jf => {
                if !truthy(&self.get(inst.arg0)) {
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
                            // dbg_println!("Calling code function with stack size (base:{}) + (fn: 1) + (params: {}) + (cur stack:{})", inst.arg1, inst.arg0, fun.stack_size);
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
                        // self.dump_stack();
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

        // self.dump_stack();

        self.call_stack.clast_mut()?.ip += 1;

        Ok(None)
    }
}

#[cfg(test)]
mod test;
