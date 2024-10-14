//! The definition of bytecode data structure that is shared among the bytecode compiler and the interpreter (vm.rs)

use std::{
    collections::HashMap,
    convert::{TryFrom, TryInto},
    io::{Read, Write},
};

use crate::{
    interpreter::{s_hex_string, s_len, s_print, s_push, s_type, EvalError},
    parser::ReadError,
    value::Value,
};

/// Operational codes for an instruction. Supposed to fit in an u8.
#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
    LoadLiteral,
    /// Move values between stack elements, from arg0 to arg1.
    Move,
    /// Increment the operand arg0
    Incr,
    Add,
    Sub,
    Mul,
    Div,
    /// Bitwise and (&)
    BitAnd,
    /// Bitwise xor (^)
    BitXor,
    /// Bitwise or (|)
    BitOr,
    /// Logical and (&&)
    And,
    /// Logical or (||)
    Or,
    /// Logical not (!)
    Not,
    /// Bitwise not (~). Interestingly, Rust does not have dedicated bitwise not operator, because
    /// it has bool type. It can distinguish logical or bitwise operation by the operand type.
    /// However, we do not have bool type (yet), so we need a dedicated operator for bitwise not, like C.
    BitNot,
    /// Get an element of an array (or a table in the future) at arg0 with the key at arg1, and make a copy at arg1.
    /// Array elements are always Rc wrapped, so the user can assign into it.
    Get,
    Set,
    /// Set a value to the special register to use in later instructions.
    /// arg0 is the value, arg1 is the register id, which is currently only allowed 0.
    SetReg,
    /// Compare arg0 and arg1, sets result -1, 0 or 1 to arg0, meaning less, equal and more, respectively
    // Cmp,
    Lt,
    Gt,
    /// Unconditional jump to arg1.
    Jmp,
    /// Conditional jump. If arg0 is truthy, jump to arg1.
    Jt,
    /// Conditional jump. If arg0 is falthy, jump to arg1.
    Jf,
    /// Call a function with arg0 aruguments on the stack with index arg1.
    Call,
    /// Returns from current call stack.
    Ret,
    /// Casts a value at arg0 to a type indicated by arg1. I'm feeling this should be a standard library function
    /// rather than a opcode, but let's finish implementation compatible with AST interpreter first.
    Cast,
}

macro_rules! impl_op_from {
    ($($op:ident),*) => {
        impl TryFrom<u8> for OpCode {
            type Error = ReadError;

            #[allow(non_upper_case_globals)]
            fn try_from(o: u8) -> Result<Self, Self::Error> {
                $(const $op: u8 = OpCode::$op as u8;)*

                match o {
                    $($op => Ok(Self::$op),)*
                    _ => Err(ReadError::UndefinedOpCode(o)),
                }
            }
        }
    }
}

impl_op_from!(
    LoadLiteral,
    Move,
    Incr,
    Add,
    Sub,
    Mul,
    Div,
    BitAnd,
    BitXor,
    BitOr,
    And,
    Or,
    Not,
    BitNot,
    Get,
    Set,
    Lt,
    Gt,
    Jmp,
    Jt,
    Jf,
    Call,
    Ret,
    Cast
);

/// A single instruction in a bytecode. OpCodes can have 0 to 2 arguments.
#[derive(Debug, Clone, Copy)]
pub struct Instruction {
    pub(crate) op: OpCode,
    pub(crate) arg0: u8,
    pub(crate) arg1: u16,
}

impl Instruction {
    pub(crate) fn new(op: OpCode, arg0: u8, arg1: u16) -> Self {
        Self { op, arg0, arg1 }
    }
    pub(crate) fn serialize(&self, writer: &mut impl Write) -> std::io::Result<()> {
        writer.write_all(&(self.op as u8).to_le_bytes())?;
        writer.write_all(&self.arg0.to_le_bytes())?;
        writer.write_all(&self.arg1.to_le_bytes())?;
        Ok(())
    }

    pub(crate) fn deserialize(reader: &mut impl Read) -> Result<Self, ReadError> {
        let mut op = [0u8; std::mem::size_of::<u8>()];
        reader.read_exact(&mut op)?;
        let mut arg0 = [0u8; std::mem::size_of::<u8>()];
        reader.read_exact(&mut arg0)?;
        let mut arg1 = [0u8; std::mem::size_of::<u16>()];
        reader.read_exact(&mut arg1)?;
        Ok(Self {
            op: u8::from_le_bytes(op).try_into()?,
            arg0: u8::from_le_bytes(arg0),
            arg1: u16::from_le_bytes(arg1),
        })
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {} {}", self.op, self.arg0, self.arg1)
    }
}

fn write_str(s: &str, writer: &mut impl Write) -> std::io::Result<()> {
    writer.write_all(&s.len().to_le_bytes())?;
    writer.write_all(&s.as_bytes())?;
    Ok(())
}

fn read_str(reader: &mut impl Read) -> Result<String, ReadError> {
    let mut len = [0u8; std::mem::size_of::<usize>()];
    reader.read_exact(&mut len)?;
    let len = usize::from_le_bytes(len);
    let mut buf = vec![0u8; len];
    reader.read_exact(&mut buf)?;
    Ok(String::from_utf8(buf)?)
}

pub(crate) fn write_bool(b: bool, writer: &mut impl Write) -> std::io::Result<()> {
    writer.write_all(&[if b { 1u8 } else { 0u8 }])
}

pub(crate) fn read_bool(reader: &mut impl Read) -> Result<bool, ReadError> {
    let mut buf = [0u8; 1];
    reader.read_exact(&mut buf)?;
    Ok(buf[0] != 0)
}

pub(crate) fn write_opt_value(
    value: &Option<Value>,
    writer: &mut impl Write,
) -> std::io::Result<()> {
    write_bool(value.is_some(), writer)?;
    if let Some(value) = value {
        value.serialize(writer)?;
    }
    Ok(())
}

pub(crate) fn read_opt_value(reader: &mut impl Read) -> Result<Option<Value>, ReadError> {
    let has_value = read_bool(reader)?;
    Ok(if has_value {
        Some(Value::deserialize(reader)?)
    } else {
        None
    })
}

pub type NativeFn = Box<dyn Fn(&[Value]) -> Result<Value, EvalError>>;

/// A mapping from function name to function prototypes, which can be a bytecode or a native extension.
pub(crate) type FnProtos = HashMap<String, FnProto>;

pub(crate) enum FnProto {
    Code(FnBytecode),
    Native(NativeFn),
}

impl FnProto {
    pub fn args(&self) -> Option<&[BytecodeArg]> {
        match self {
            Self::Code(bytecode) => Some(&bytecode.args),
            _ => None,
        }
    }
}

pub struct Bytecode {
    pub(crate) functions: HashMap<String, FnProto>,
}

impl Bytecode {
    /// Add a user-application provided native function to this bytecode.
    pub fn add_ext_fn(
        &mut self,
        name: String,
        f: Box<dyn Fn(&[Value]) -> Result<Value, EvalError>>,
    ) {
        self.functions.insert(name, FnProto::Native(f));
    }

    pub fn add_std_fn(&mut self) {
        std_functions(&mut |name, f| self.add_ext_fn(name, f));
    }

    pub fn write(&self, writer: &mut impl Write) -> std::io::Result<()> {
        writer.write_all(
            &self
                .functions
                .iter()
                .filter(|f| matches!(f.1, FnProto::Code(_)))
                .count()
                .to_le_bytes(),
        )?;
        for (fname, func) in self.functions.iter() {
            if let FnProto::Code(func) = func {
                write_str(fname, writer)?;
                func.write(writer)?;
            }
        }
        Ok(())
    }

    pub fn read(reader: &mut impl Read) -> Result<Self, ReadError> {
        let mut len = [0u8; std::mem::size_of::<usize>()];
        reader.read_exact(&mut len)?;
        let len = usize::from_le_bytes(len);
        let ret = Bytecode {
            functions: (0..len)
                .map(|_| -> Result<(String, FnProto), ReadError> {
                    let name = read_str(reader)?;
                    Ok((name.clone(), FnProto::Code(FnBytecode::read(name, reader)?)))
                })
                .collect::<Result<HashMap<_, _>, ReadError>>()?,
        };
        dbg_println!("loaded {} functions", ret.functions.len());
        let loaded_fn = ret
            .functions
            .iter()
            .find(|(name, _)| *name == "")
            .ok_or(ReadError::NoMainFound)?;
        if let FnProto::Code(ref _code) = loaded_fn.1 {
            dbg_println!("instructions: {:#?}", _code.instructions);
        }
        Ok(ret)
    }

    pub fn disasm(&self, out: &mut impl Write) -> std::io::Result<()> {
        for (fname, fnproto) in &self.functions {
            if let FnProto::Code(bytecode) = fnproto {
                if fname.is_empty() {
                    writeln!(out, "\nFunction <toplevel> disassembly:")?;
                } else {
                    writeln!(out, "\nFunction {fname} disassembly:")?;
                }
                bytecode.disasm(out)?;
            }
        }
        Ok(())
    }

    pub fn signatures(&self, out: &mut impl Write) -> std::io::Result<()> {
        for (fname, fnproto) in &self.functions {
            match fnproto {
                FnProto::Code(code) => {
                    write!(out, "fn {}(", fname)?;
                    for (i, arg) in code.args.iter().enumerate() {
                        if i != 0 {
                            write!(out, ", ")?;
                        }
                        if let Some(init) = &arg.init {
                            write!(out, "{} = {}", arg.name, init)?;
                        } else {
                            write!(out, "{}", arg.name)?;
                        }
                    }
                    writeln!(out, ")")?;
                }
                FnProto::Native(_) => writeln!(out, "fn {} -> <Native>", fname)?,
            }
        }
        Ok(())
    }
}

/// Add standard common functions, such as `print`, `len` and `push`, to this bytecode.
pub fn std_functions(
    f: &mut impl FnMut(String, Box<dyn Fn(&[Value]) -> Result<Value, EvalError>>),
) {
    f("print".to_string(), Box::new(s_print));
    f(
        "puts".to_string(),
        Box::new(|values: &[Value]| -> Result<Value, EvalError> {
            print!(
                "{}",
                values.iter().fold("".to_string(), |acc, cur: &Value| {
                    if acc.is_empty() {
                        cur.to_string()
                    } else {
                        acc + &cur.to_string()
                    }
                })
            );
            Ok(Value::I64(0))
        }),
    );
    f("type".to_string(), Box::new(&s_type));
    f("len".to_string(), Box::new(s_len));
    f("push".to_string(), Box::new(s_push));
    f("hex_string".to_string(), Box::new(s_hex_string));
}

#[derive(Debug, Clone)]
pub struct BytecodeArg {
    pub(crate) name: String,
    pub(crate) init: Option<Value>,
}

impl BytecodeArg {
    pub(crate) fn new(name: String, init: Option<Value>) -> Self {
        Self { name, init }
    }
}

#[derive(Debug, Clone)]
pub struct FnBytecode {
    /// Name is technically not required here, since it is also the key to a hash map,
    /// but it is convenient to have this in bytecode for debugging.
    pub(crate) name: String,
    pub(crate) literals: Vec<Value>,
    pub(crate) args: Vec<BytecodeArg>,
    pub(crate) instructions: Vec<Instruction>,
    pub(crate) stack_size: usize,
}

impl FnBytecode {
    /// Create a placeholder entry that will be filled later.
    pub(crate) fn proto(name: String, args: Vec<String>) -> Self {
        Self {
            name,
            literals: vec![],
            args: args
                .into_iter()
                .map(|arg| BytecodeArg {
                    name: arg,
                    init: None,
                })
                .collect(),
            instructions: vec![],
            stack_size: 0,
        }
    }

    pub(crate) fn push_inst(&mut self, op: OpCode, arg0: u8, arg1: u16) -> usize {
        let ret = self.instructions.len();
        self.instructions.push(Instruction::new(op, arg0, arg1));
        ret
    }

    pub fn write(&self, writer: &mut impl Write) -> std::io::Result<()> {
        writer.write_all(&self.stack_size.to_le_bytes())?;
        writer.write_all(&self.literals.len().to_le_bytes())?;
        for literal in &self.literals {
            literal.serialize(writer)?;
        }
        writer.write_all(&self.args.len().to_le_bytes())?;
        for arg in &self.args {
            write_str(&arg.name, writer)?;
            write_opt_value(&arg.init, writer)?;
        }
        writer.write_all(&self.instructions.len().to_le_bytes())?;
        for inst in &self.instructions {
            inst.serialize(writer)?;
        }
        Ok(())
    }

    pub fn read(name: String, reader: &mut impl Read) -> Result<Self, ReadError> {
        let mut stack_size = [0u8; std::mem::size_of::<usize>()];
        reader.read_exact(&mut stack_size)?;

        let mut literals = [0u8; std::mem::size_of::<usize>()];
        reader.read_exact(&mut literals)?;
        let literals = usize::from_le_bytes(literals);
        let literals = (0..literals)
            .map(|_| Value::deserialize(reader))
            .collect::<Result<Vec<_>, _>>()?;

        let mut args = [0u8; std::mem::size_of::<usize>()];
        reader.read_exact(&mut args)?;
        let num_args = usize::from_le_bytes(args);
        let args = (0..num_args)
            .map(|_| -> Result<_, ReadError> {
                let name = read_str(reader)?;
                let init = read_opt_value(reader)?;
                Ok(BytecodeArg { name, init })
            })
            .collect::<Result<Vec<_>, _>>()?;

        let mut instructions = [0u8; std::mem::size_of::<usize>()];
        reader.read_exact(&mut instructions)?;
        let instructions = usize::from_le_bytes(instructions);
        let instructions = (0..instructions)
            .map(|_| Instruction::deserialize(reader))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Self {
            name,
            literals,
            args,
            instructions,
            stack_size: usize::from_le_bytes(stack_size),
        })
    }

    pub fn disasm(&self, f: &mut impl std::io::Write) -> Result<(), std::io::Error> {
        writeln!(f, "Stack size: {}", self.stack_size)?;
        writeln!(f, "Literals({}):", self.literals.len())?;
        for (i, literal) in self.literals.iter().enumerate() {
            writeln!(f, "  [{}] {}", i, literal)?;
        }
        writeln!(f, "Args({}):", self.args.len())?;
        for (i, arg) in self.args.iter().enumerate() {
            writeln!(f, "  [{}] {} = {:?}", i, arg.name, arg.init)?;
        }
        writeln!(f, "Instructions({}):", self.instructions.len())?;
        for (i, inst) in self.instructions.iter().enumerate() {
            match inst.op {
                OpCode::LoadLiteral => {
                    if let Some(literal) = self.literals.get(inst.arg0 as usize) {
                        writeln!(f, "  [{}] {} ({:?})", i, inst, literal)?;
                    } else {
                        writeln!(f, "  [{}] {} ? (Literal index out of bound)", i, inst)?;
                    }
                }
                _ => writeln!(f, "  [{}] {}", i, inst)?,
            }
        }
        Ok(())
    }
}
