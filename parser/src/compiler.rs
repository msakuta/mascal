mod error;
mod lvalue;

use std::{cell::RefCell, collections::HashMap, io::Write, rc::Rc};

pub(crate) use self::error::CompileError;
use self::{
    error::CompileErrorKind as CEK,
    lvalue::{emit_lvalue, LValue},
};

use crate::{
    bytecode::{
        std_functions, Bytecode, BytecodeArg, FnBytecode, FnProto, FunctionInfo, Instruction,
        LineInfo, NativeFn, OpCode,
    },
    format_ast::format_expr,
    interpreter::{eval, EvalContext, RunResult, TypeMapRc},
    parser::{ExprEnum, Expression, Statement, StructDecl, StructField},
    value::{ArrayInt, TupleEntry},
    DebugInfo, Span, TypeDecl, Value,
};

/// Extracted info of a field of a struct. Some information is determined by a position in the struct,
/// referring to other types, so they are not part of [`StructField`].
struct FieldInfo<'src> {
    field: &'src StructField<'src>,
    offset: usize,
    size: usize,
}

impl<'src> StructDecl<'src> {
    fn field_info(&'src self, name: &str, typedefs: &TypeMapRc) -> Option<FieldInfo<'src>> {
        let mut offset = 0;
        for field in &self.fields {
            let size = ty_size_of(Span::new(""), &field.ty, typedefs).ok()?;
            if *field.name == name {
                println!("offsetof({}, {name}) = {offset}", self.name);
                return Some(FieldInfo {
                    field,
                    offset,
                    size,
                });
            }
            offset += size;
        }
        None
    }

    pub(crate) fn offset_of(&'src self, name: &str, typedefs: &TypeMapRc) -> Option<usize> {
        self.field_info(name, typedefs).map(|info| info.offset)
    }
}

#[derive(Debug, Clone, Copy)]
enum Target {
    /// A temporary neither literal nor local
    None,
    /// Literal value with literal index.
    /// Right now whether the target is literal is not utilized, but we may use it for optimization.
    #[allow(unused)]
    Literal(usize),
    /// If it is an allocated stack slot for a local variable, it will contain the index
    /// into locals array. We use it to keep track of which is local variable so that
    /// we won't destroy it by accident.
    /// **NOTE** that it is not a stack index.
    #[allow(unused)]
    Local(usize),
    /// Inlined struct field. First
    StructField { start: usize, idx: usize },
}

impl Default for Target {
    fn default() -> Target {
        Self::None
    }
}

#[derive(Clone, Debug)]
struct LocalVar {
    name: String,
    stack_idx: usize,
    size: usize,
}

impl From<&LocalVar> for RValue {
    fn from(value: &LocalVar) -> Self {
        Self {
            start: value.stack_idx,
            size: value.size,
        }
    }
}

struct CompilerEnv<'src> {
    functions: HashMap<String, FnProto>,
    debug: HashMap<String, FunctionInfo>,
    typedefs: TypeMapRc<'src>,
}

impl<'src> CompilerEnv<'src> {
    fn new(mut functions: HashMap<String, FnProto>, debug: HashMap<String, FunctionInfo>) -> Self {
        let out = Rc::new(RefCell::new(std::io::stdout()));
        std_functions(out, &mut |name, f| {
            functions.insert(name, FnProto::Native(f));
        });
        Self {
            functions,
            debug,
            typedefs: TypeMapRc::new(),
        }
    }
}

struct Compiler<'a, 'src> {
    env: &'a mut CompilerEnv<'src>,
    bytecode: FnBytecode,
    target_stack: Vec<Target>,
    locals: Vec<Vec<LocalVar>>,
    break_ips: Vec<usize>,
    current_pos: Option<(u32, CompSourcePos)>,
    line_info: HashMap<u32, CompSourcePos>,
}

#[derive(Clone, Copy)]
struct CompSourcePos {
    line: u32,
    column: u32,
    len: u32,
}

impl<'a> From<Span<'a>> for CompSourcePos {
    fn from(value: Span) -> Self {
        Self {
            line: value.location_line(),
            column: value.get_column() as u32,
            len: value.len() as u32,
        }
    }
}

impl<'a, 'src> Compiler<'a, 'src> {
    fn new(args: Vec<LocalVar>, fn_args: Vec<BytecodeArg>, env: &'a mut CompilerEnv<'src>) -> Self {
        Self {
            env,
            bytecode: FnBytecode {
                // By convention, the toplevel function (entry point) has an empty name
                name: "".to_string(),
                literals: vec![],
                args: fn_args,
                instructions: vec![],
                stack_size: 0,
            },
            target_stack: std::iter::once(Target::None)
                .chain(
                    args.iter()
                        .enumerate()
                        .map(|(i, arg)| std::iter::repeat(Target::Local(i)).take(arg.size))
                        .flatten(),
                )
                .collect(),
            locals: vec![args],
            break_ips: vec![],
            current_pos: None,
            line_info: HashMap::new(),
        }
    }

    /// Fixup the jump address for the break statements in the previous loop to current instruction pointer.
    /// Call it just after leaving loop body.
    fn fixup_breaks(&mut self) {
        let break_jmp_addr = self.bytecode.instructions.len();
        for ip in &self.break_ips {
            self.bytecode.instructions[*ip].arg1 = break_jmp_addr as u16;
        }
        self.break_ips.clear();
    }

    /// Returns a stack index, removing potential duplicate values
    fn find_or_create_literal(&mut self, value: &Value) -> usize {
        let bytecode = &mut self.bytecode;
        let literal = if let Some((i, _lit)) = bytecode
            .literals
            .iter()
            .enumerate()
            .find(|(_, lit)| *lit == value)
        {
            i
        } else {
            let literal = bytecode.literals.len();
            bytecode.literals.push(value.clone());
            literal
        };

        let stk_target = self.target_stack.len();
        self.target_stack.push(Target::Literal(literal));
        self.push_inst(OpCode::LoadLiteral, literal as u8, stk_target as u16);
        stk_target
    }

    fn find_local(&self, name: &str, span: Span<'src>) -> CompileResult<'src, &LocalVar> {
        self.locals
            .iter()
            .rev()
            .fold(None, |acc, rhs| {
                if acc.is_some() {
                    acc
                } else {
                    rhs.iter().rev().find(|lo| lo.name == name)
                }
            })
            .ok_or_else(|| CompileError::new(span, CEK::VarNotFound(name.to_string())))
    }

    fn push_inst(&mut self, op: OpCode, arg0: u8, arg1: u16) -> usize {
        let ret = self.bytecode.push_inst(op, arg0, arg1);
        if let Some((_, src_pos)) = self.current_pos {
            self.line_info.insert(ret as u32, src_pos);
        }
        ret
    }

    fn start_src_pos(&mut self, src_pos: CompSourcePos) {
        self.current_pos = Some((self.bytecode.instructions.len() as u32, src_pos));
    }

    fn end_src_pos(&mut self, src_pos: CompSourcePos) {
        self.current_pos = Some((self.bytecode.instructions.len() as u32, src_pos));
    }

    fn source_map(&self) -> FunctionInfo {
        let vars = self.locals.last().map_or(HashMap::new(), |locals| {
            locals
                .iter()
                .map(|var| (var.name.clone(), var.stack_idx))
                .collect::<HashMap<_, _>>()
        });

        let mut source_map = vec![];
        for i in 0..self.bytecode.instructions.len() {
            let Some(src_pos) = self.line_info.get(&(i as u32)) else {
                continue;
            };

            source_map.push(LineInfo {
                instruction: i as u32,
                src_line: src_pos.line,
                src_column: src_pos.column,
                src_len: src_pos.len,
            });
        }

        FunctionInfo::new(vars, source_map)
    }
}

type CompileResult<'src, T> = Result<T, CompileError<'src>>;

pub fn compile<'src, 'ast>(
    stmts: &'ast [Statement<'src>],
    functions: HashMap<String, NativeFn>,
) -> CompileResult<'src, Bytecode> {
    CompilerBuilder::new(stmts)
        .functions(functions)
        .compile(&mut std::io::sink())
}

pub fn disasm<'src, 'ast>(
    stmts: &'ast [Statement<'src>],
    functions: HashMap<String, NativeFn>,
) -> CompileResult<'src, String> {
    let mut disasm = Vec::<u8>::new();
    let mut cursor = std::io::Cursor::new(&mut disasm);

    CompilerBuilder::new(stmts)
        .functions(functions)
        .compile(&mut cursor)?;

    Ok(String::from_utf8(disasm)?)
}

pub struct CompilerBuilder<'src, 'ast> {
    stmts: &'ast [Statement<'src>],
    functions: HashMap<String, NativeFn>,
    enable_debug: bool,
}

impl<'src, 'ast> CompilerBuilder<'src, 'ast> {
    pub fn new(stmts: &'ast [Statement<'src>]) -> Self {
        Self {
            stmts,
            functions: HashMap::new(),
            enable_debug: false,
        }
    }

    pub fn functions(mut self, funcs: HashMap<String, NativeFn>) -> Self {
        self.functions = funcs;
        self
    }

    pub fn enable_debug(mut self, v: bool) -> Self {
        self.enable_debug = v;
        self
    }

    pub fn compile(self, disasm: &mut impl Write) -> CompileResult<'src, Bytecode> {
        let functions = self
            .functions
            .into_iter()
            .map(|(k, v)| (k, FnProto::Native(v)))
            .collect();

        // Built-in functions don't have line number information.
        let mut env = CompilerEnv::new(functions, HashMap::new());

        retrieve_fn_signatures(self.stmts, &mut env);

        let mut compiler = Compiler::new(vec![], vec![], &mut env);

        // In the current VM model, there is always a return value, even if the function ends with a semicolon.
        // Void return type is only effective in type system. So we return _something_ but its value should not be
        // evaluated.
        let last_target = emit_stmts(self.stmts, &mut compiler)?.map_or(0, |res| res.start);
        compiler
            .bytecode
            .instructions
            .push(Instruction::new(OpCode::Ret, 0, last_target as u16));
        compiler.bytecode.stack_size = compiler.target_stack.len();

        let source_map = if self.enable_debug {
            Some(compiler.source_map())
        } else {
            None
        };

        let bytecode = FnProto::Code(compiler.bytecode);

        if let Some(source_map) = source_map {
            env.debug.insert("".to_string(), source_map);
        }

        let mut functions = env.functions;
        functions.insert("".to_string(), bytecode);

        for (fname, fnproto) in &functions {
            if let FnProto::Code(bytecode) = fnproto {
                if fname.is_empty() {
                    writeln!(disasm, "\nFunction <toplevel> disassembly:")?;
                } else {
                    writeln!(disasm, "\nFunction {fname} disassembly:")?;
                }
                bytecode.disasm(disasm)?;
            }
        }

        Ok(Bytecode {
            functions,
            debug: if self.enable_debug {
                Some(DebugInfo::new(env.debug))
            } else {
                None
            },
        })
    }
}

fn compile_fn<'src, 'ast>(
    env: &mut CompilerEnv<'src>,
    name: String,
    src_pos: &CompSourcePos,
    stmts: &'ast [Statement<'src>],
    args: Vec<LocalVar>,
    fn_args: Vec<BytecodeArg>,
) -> CompileResult<'src, (FnProto, FunctionInfo)> {
    let mut compiler = Compiler::new(args, fn_args, env);
    compiler.start_src_pos(*src_pos);

    // In the current VM model, there is always a return value, even if the function ends with a semicolon.
    // Void return type is only effective in type system. So we return _something_ but its value should not be
    // evaluated. We may revisit this design and introduce RET_VOID instruction.
    let last_target = emit_stmts(stmts, &mut compiler)?.map_or(0, |res| res.start);
    compiler
        .bytecode
        .instructions
        .push(Instruction::new(OpCode::Ret, 0, last_target as u16));

    compiler.bytecode.stack_size = compiler.target_stack.len();
    compiler.bytecode.name = name;
    let source_map = compiler.source_map();

    Ok((FnProto::Code(compiler.bytecode), source_map))
}

fn retrieve_fn_signatures(stmts: &[Statement], env: &mut CompilerEnv) {
    for stmt in stmts {
        match stmt {
            Statement::FnDecl {
                name, args, stmts, ..
            } => {
                let args = args.iter().map(|arg| arg.name.to_string()).collect();
                let bytecode = FnBytecode::proto(name.to_string(), args);
                env.functions
                    .insert(name.to_string(), FnProto::Code(bytecode));
                retrieve_fn_signatures(stmts, env);
            }
            _ => {}
        }
    }
}

fn emit_stmts<'src>(
    stmts: &[Statement<'src>],
    compiler: &mut Compiler<'_, 'src>,
) -> CompileResult<'src, Option<RValue>> {
    let mut last_target = None;
    for stmt in stmts {
        match stmt {
            Statement::VarDecl {
                name: var,
                init,
                ty,
                ..
            } => {
                let locals = compiler
                    .locals
                    .last()
                    .ok_or_else(|| CompileError::new(*var, CEK::LocalsStackUnderflow))?
                    .len();
                let init_val = if let Some(init_expr) = init {
                    let stk_var = emit_expr(init_expr, compiler)?;
                    for target_stack in
                        &mut compiler.target_stack[stk_var.start..stk_var.start + stk_var.size]
                    {
                        *target_stack = Target::Local(locals);
                    }
                    stk_var
                } else {
                    let stk_var = compiler.target_stack.len();
                    let size = ty_size_of(*var, ty, &compiler.env.typedefs)?;
                    compiler.target_stack.push(Target::Local(locals));
                    RValue {
                        start: stk_var,
                        size,
                    }
                };
                let locals = compiler
                    .locals
                    .last_mut()
                    .ok_or_else(|| CompileError::new(*var, CEK::LocalsStackUnderflow))?;
                locals.push(LocalVar {
                    name: var.to_string(),
                    stack_idx: init_val.start,
                    size: init_val.size,
                });
                dbg_println!("Locals: {:?}", compiler.locals);
            }
            Statement::FnDecl {
                name, args, stmts, ..
            } => {
                dbg_println!("FnDecl: Args: {:?}", args);
                let mut offset = 1;
                let a_args = args
                    .iter()
                    .enumerate()
                    .map(|(idx, arg)| {
                        let size = ty_size_of(arg.name, &arg.ty, &compiler.env.typedefs)?;
                        // The 0th index is used for function name / return value, so the args start with 1.
                        let local = LocalVar {
                            name: arg.name.to_string(),
                            stack_idx: offset,
                            size,
                        };
                        compiler
                            .target_stack
                            .extend(std::iter::repeat(Target::Local(idx + 1)).take(size));
                        offset += size;
                        Ok(local)
                    })
                    .collect::<Result<Vec<_>, CompileError>>()?;
                let fn_args = args
                    .iter()
                    .map(|arg| {
                        let init = if let Some(ref init) = arg.init {
                            // Run the interpreter to fold the constant expression into a value.
                            // Note that the interpreter has an empty context, so it cannot access any
                            // global variables or user defined functions.
                            match eval(init, &mut EvalContext::new())
                                .map_err(|e| CompileError::new(*name, CEK::EvalError(e)))?
                            {
                                RunResult::Yield(val) => Some(val),
                                _ => return Err(CompileError::new(*name, CEK::DisallowedBreak)),
                            }
                        } else {
                            None
                        };
                        Ok(BytecodeArg::new(arg.name.to_string(), arg.ty.clone(), init))
                    })
                    .collect::<Result<_, _>>()?;
                dbg_println!("FnDecl actual args: {:?} fn_args: {:?}", a_args, fn_args);
                let src_pos = (*name).into();
                let (fun, debug) = compile_fn(
                    &mut compiler.env,
                    name.to_string(),
                    &src_pos,
                    stmts,
                    a_args,
                    fn_args,
                )?;
                compiler.env.functions.insert(name.to_string(), fun);
                compiler.env.debug.insert(name.to_string(), debug);
            }
            Statement::Expression { ref ex, semicolon } => {
                let res = emit_expr(ex, compiler)?;
                last_target = if *semicolon { None } else { Some(res) };
            }
            Statement::Loop(stmts) => {
                let inst_loop_start = compiler.bytecode.instructions.len();
                last_target = emit_stmts(stmts, compiler)?;
                compiler.push_inst(OpCode::Jmp, 0, inst_loop_start as u16);
                compiler.fixup_breaks();
            }
            Statement::While(cond, stmts) => {
                let inst_loop_start = compiler.bytecode.instructions.len();
                let stk_cond = scalar(cond.span, emit_expr(cond, compiler)?)?;
                let inst_break = compiler.push_inst(OpCode::Jf, stk_cond as u8, 0);
                last_target = emit_stmts(stmts, compiler)?;
                compiler.push_inst(OpCode::Jmp, 0, inst_loop_start as u16);
                compiler.bytecode.instructions[inst_break].arg1 =
                    compiler.bytecode.instructions.len() as u16;
                compiler.fixup_breaks();
            }
            Statement::For {
                var,
                start,
                end,
                stmts,
                ..
            } => {
                let stk_from = scalar(start.span, emit_expr(start, compiler)?)?;
                let stk_to = scalar(end.span, emit_expr(end, compiler)?)?;
                let local_iter = compiler
                    .locals
                    .last()
                    .ok_or_else(|| CompileError::new(*var, CEK::LocalsStackUnderflow))?
                    .len();
                let stk_check = compiler.target_stack.len();

                // stack: [stk_from, stk_to, stk_check]
                //   where stk_from is the starting variable being incremented until stk_to,
                //         stk_to is the end value
                //     and stk_check is the value to store the result of comparison

                let inst_loop_start = compiler.bytecode.instructions.len();
                compiler
                    .locals
                    .last_mut()
                    .ok_or_else(|| CompileError::new(*var, CEK::LocalsStackUnderflow))?
                    .push(LocalVar {
                        name: var.to_string(),
                        stack_idx: stk_from,
                        size: 1,
                    });
                compiler.target_stack[stk_from] = Target::Local(local_iter);
                compiler.target_stack.push(Target::None);
                compiler.push_inst(OpCode::Move, stk_from as u8, stk_check as u16);
                compiler.push_inst(OpCode::Lt, stk_check as u8, stk_to as u16);
                let inst_break = compiler.push_inst(OpCode::Jf, stk_check as u8, 0);
                last_target = emit_stmts(stmts, compiler)?;
                compiler.push_inst(OpCode::Incr, stk_from as u8, 0);
                compiler.push_inst(OpCode::Jmp, 0, inst_loop_start as u16);
                compiler.bytecode.instructions[inst_break].arg1 =
                    compiler.bytecode.instructions.len() as u16;
                compiler.fixup_breaks();
            }
            Statement::Break => {
                let break_ip = compiler.bytecode.instructions.len();
                compiler.push_inst(OpCode::Jmp, 0, 0);
                compiler.break_ips.push(break_ip);
            }
            // For now, structs are concepts only at compile time.
            Statement::Struct(st) => {
                compiler
                    .env
                    .typedefs
                    .insert(st.name.to_string(), Rc::new(st.clone()));
            }
            Statement::Comment(_) => (),
        }
    }
    Ok(last_target)
}

/// Internal to the compiler
#[derive(Clone, Copy, Debug)]
pub(self) struct RValue {
    start: usize,
    size: usize,
}

impl RValue {
    fn range(&self) -> impl Iterator<Item = usize> {
        self.start..self.start + self.size
    }
}

impl From<usize> for RValue {
    fn from(value: usize) -> Self {
        Self {
            start: value,
            size: 1,
        }
    }
}

fn scalar(span: Span, value: RValue) -> Result<usize, CompileError> {
    if value.size != 1 {
        return Err(CompileError::new(span, CEK::DisallowedAggregate));
    }
    Ok(value.start)
}

fn emit_expr<'src>(
    expr: &Expression<'src>,
    compiler: &mut Compiler<'_, 'src>,
) -> CompileResult<'src, RValue> {
    compiler.start_src_pos(expr.span.into());
    let res: CompileResult<RValue> = match &expr.expr {
        ExprEnum::NumLiteral(val, _) => Ok(compiler.find_or_create_literal(val).into()),
        ExprEnum::StrLiteral(val) => Ok(compiler
            .find_or_create_literal(&Value::Str(val.clone()))
            .into()),
        ExprEnum::ArrLiteral(val) => {
            let mut ctx = EvalContext::new();
            let val = Value::Array(Rc::new(RefCell::new(ArrayInt {
                type_decl: TypeDecl::Any,
                values: val
                    .iter()
                    .map(|v| {
                        if let RunResult::Yield(y) = eval(v, &mut ctx)
                            .map_err(|e| CompileError::new(expr.span, CEK::EvalError(e)))?
                        {
                            Ok(y)
                        } else {
                            Err(CompileError::new(expr.span, CEK::BreakInArrayLiteral))
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            })));
            Ok(compiler.find_or_create_literal(&val).into())
        }
        ExprEnum::TupleLiteral(values) => {
            let mut ctx = EvalContext::new();
            let val = values
                .iter()
                .map(|v| {
                    if let RunResult::Yield(y) = eval(v, &mut ctx)
                        .map_err(|e| CompileError::new(expr.span, CEK::EvalError(e)))?
                    {
                        Ok(TupleEntry {
                            decl: TypeDecl::from_value(&y),
                            value: y,
                        })
                    } else {
                        Err(CompileError::new(expr.span, CEK::BreakInArrayLiteral))
                    }
                })
                .collect::<Result<Vec<_>, _>>();

            // If the tuple literal is a constant expression, put it into the literal table.
            if let Ok(val) = val {
                let val = Value::Tuple(Rc::new(RefCell::new(val)));
                Ok(compiler.find_or_create_literal(&val).into())
            } else {
                // If it is not a constant expression, build the expression to construct a tuple at runtime.

                // Emit expressions in each element of the tuple.
                let arg_values = values
                    .iter()
                    .map(|value| emit_expr(&value, compiler))
                    .collect::<CompileResult<Vec<_>>>()?;

                // Reserve the stack slot to return the tuple.
                let stk_ret = compiler.target_stack.len();
                compiler.target_stack.push(Target::None);

                // Copy the elements in a contiguous region of the stack.
                // It is important to not call emit_expr between those elements, since they can push temporary variables.
                for arg in &arg_values {
                    let arg_target = compiler.target_stack.len();
                    for offset in 0..arg.size {
                        compiler.target_stack.push(Target::None);
                        compiler.push_inst(
                            OpCode::Move,
                            (arg.start + offset) as u8,
                            arg_target as u16,
                        );
                    }
                }

                compiler.push_inst(OpCode::MakeTuple, arg_values.len() as u8, stk_ret as u16);
                Ok(stk_ret.into())
            }
        }
        ExprEnum::Variable(str) => {
            let local = compiler.find_local(*str, expr.span)?;
            Ok(local.into())
        }
        ExprEnum::Cast(ex, decl) => {
            let val = scalar(ex.span, emit_expr(ex, compiler)?)?;
            // Make a copy of the value to avoid overwriting original variable
            let val_copy = compiler.target_stack.len();
            compiler.push_inst(OpCode::Move, val as u8, val_copy as u16);
            compiler.target_stack.push(Target::None);
            let mut decl_buf = [0u8; std::mem::size_of::<i64>()];
            decl.serialize(&mut std::io::Cursor::new(&mut decl_buf[..]))?;
            let decl_stk =
                compiler.find_or_create_literal(&Value::I64(i64::from_le_bytes(decl_buf)));
            compiler.push_inst(OpCode::Cast, val_copy as u8, decl_stk as u16);
            Ok(val_copy.into())
        }
        ExprEnum::Not(val) => {
            let val = scalar(val.span, emit_expr(val, compiler)?)?;
            compiler.push_inst(OpCode::Not, val as u8, 0);
            Ok(val.into())
        }
        ExprEnum::BitNot(val) => {
            let val = scalar(val.span, emit_expr(val, compiler)?)?;
            compiler.push_inst(OpCode::BitNot, val as u8, 0);
            Ok(val.into())
        }
        ExprEnum::Neg(val) => {
            let val = scalar(val.span, emit_expr(val, compiler)?)?;
            compiler.push_inst(OpCode::Neg, val as u8, 0);
            Ok(val.into())
        }
        ExprEnum::Add(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::Add, lhs, rhs)?.into()),
        ExprEnum::Sub(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::Sub, lhs, rhs)?.into()),
        ExprEnum::Mult(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::Mul, lhs, rhs)?.into()),
        ExprEnum::Div(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::Div, lhs, rhs)?.into()),
        ExprEnum::VarAssign(lhs, rhs) => {
            let lhs_result = emit_lvalue(lhs, compiler)?;
            // TODO: assignment should work for aggregate types
            let rhs_result = emit_expr(rhs, compiler)?;
            match lhs_result {
                LValue::Variable(name) => {
                    let local = compiler.find_local(&name, expr.span)?.clone();
                    for (dst, src) in (local.stack_idx..).zip(rhs_result.range()) {
                        compiler.push_inst(OpCode::Move, src as u8, dst as u16);
                    }
                    Ok(RValue {
                        start: local.stack_idx,
                        size: local.size,
                    })
                }
                LValue::ArrayRef(arr, subidx) => {
                    let rhs_result = scalar(rhs.span, rhs_result)?;
                    let value_copy = compiler.target_stack.len();
                    compiler.target_stack.push(Target::None);

                    // First, copy the value to be overwritten by Get instruction
                    compiler.push_inst(OpCode::Move, rhs_result as u8, value_copy as u16);

                    // Second, assign the target index into the set register
                    compiler.push_inst(OpCode::SetReg, subidx as u8, 0);

                    // Third, get the element from the array reference
                    compiler.push_inst(OpCode::Set, arr as u8, value_copy as u16);

                    Ok(value_copy.into())
                }
                LValue::StructRef(_st_ty, start, size) => {
                    for (dst, src) in (start..start + size).zip(rhs_result.range()) {
                        compiler.push_inst(OpCode::Move, src as u8, dst as u16);
                    }
                    Ok(start.into())
                }
            }
        }
        ExprEnum::FnInvoke(fname, args) => {
            let params = {
                let fun = compiler.env.functions.get(*fname).ok_or_else(|| {
                    CompileError::new(expr.span, CEK::FnNotFound(fname.to_string()))
                })?;

                fun.args().map(|args| args.to_vec())
            };

            // The size of the arguemnts are not easy to calculate, because it depends on the individual type of the
            // args. Here we calculate the total size of the arguments by computing size_of and accumulating them.
            // let args_size = params.as_ref().map_or(Ok(args.len()), |params| {
            //     params.iter().zip(args.iter()).fold(
            //         Ok(0),
            //         |acc, cur| -> Result<usize, CompileError> {
            //             Ok(acc? + ty_size_of(cur.1.expr.span, &cur.0.ty, &compiler.env.typedefs)?)
            //         },
            //     )
            // })?;
            let args_size = params.as_ref().map_or(args.len(), |params| params.len());

            // Prepare a buffer for actual arguments. It could be a mix of unnamed and named arguments.
            // Unnamed arguments are indexed from 0, while named arguments can appear at any index.
            let mut arg_values = vec![None; args_size];

            // First, fill the buffer with unnamed arguments. Technically it could be more optimized by
            // allocating and initializing at the same time, but we do not pursue performance that much yet.
            for (arg_value, arg) in arg_values.iter_mut().zip(args.iter()) {
                if arg.name.is_some() {
                    continue;
                }
                *arg_value = Some(emit_expr(&arg.expr, compiler)?);
            }

            // Second, fill the buffer with named arguments.
            for named_arg in args.iter() {
                if let Some(name) = named_arg.name.as_ref() {
                    let Some(params) = params.as_ref() else {
                        return Err(CompileError::new(expr.span, CEK::UnknownNamedArg));
                    };
                    if let Some((param_idx, _)) =
                        params.iter().enumerate().find(|(_, p)| p.name == **name)
                    {
                        arg_values[param_idx] = Some(emit_expr(&named_arg.expr, compiler)?);
                    }
                }
            }

            if let Some(params) = params.as_ref() {
                for (param, arg_value) in params.iter().zip(arg_values.iter_mut()) {
                    if arg_value.is_some() {
                        continue;
                    }
                    if let Some(default_val) = param.init.as_ref() {
                        let default_val = compiler.find_or_create_literal(default_val);
                        *arg_value = Some(default_val.into());
                    }
                }
            }

            let stk_fname = compiler.find_or_create_literal(&Value::Str(fname.to_string()));

            let Some(_fun) = compiler.env.functions.get(*fname) else {
                return Err(CompileError::new(
                    expr.span,
                    CEK::FnNotFound(fname.to_string()),
                ));
            };

            dbg_println!("FnProto found for: {fname}, args: {:?}", _fun.args());

            // If a named argument is duplicate, you would have a hole in actual args.
            // Until we have the default parameter value, it would be a compile error.
            let arg_values = arg_values
                .into_iter()
                .collect::<Option<Vec<_>>>()
                .ok_or_else(|| CompileError::new(expr.span, CEK::InsufficientNamedArgs))?;

            // Align arguments to the stack to prepare a call.
            for arg in &arg_values {
                for src in arg.range() {
                    let arg_target = compiler.target_stack.len();
                    compiler.target_stack.push(Target::None);
                    compiler.push_inst(OpCode::Move, src as u8, arg_target as u16);
                }
            }

            compiler.push_inst(OpCode::Call, arg_values.len() as u8, stk_fname as u16);
            compiler.target_stack.push(Target::None);
            Ok(stk_fname.into())
        }
        ExprEnum::ArrIndex(ex, args) => {
            let stk_ex = scalar(ex.span, emit_expr(ex, compiler)?)?;
            let args = args
                .iter()
                .map(|v| emit_expr(v, compiler))
                .collect::<Result<Vec<_>, _>>()?;
            let arg = scalar(ex.span, args[0])?;
            let arg = if matches!(compiler.target_stack[arg], Target::Local(_)) {
                // We move the local variable to another slot because our instructions are destructive
                let top = compiler.target_stack.len();
                compiler.push_inst(OpCode::Move, arg as u8, top as u16);
                compiler.target_stack.push(Target::None);
                top
            } else {
                arg
            };
            compiler.push_inst(OpCode::Get, stk_ex as u8, arg as u16);
            Ok(arg.into())
        }
        ExprEnum::TupleIndex(ex, index) => {
            let stk_ex = scalar(ex.span, emit_expr(ex, compiler)?)?;
            let stk_idx = compiler.find_or_create_literal(&Value::I64(*index as i64));
            let stk_idx_copy = compiler.target_stack.len();
            compiler.target_stack.push(Target::None);
            compiler.push_inst(OpCode::Move, stk_idx as u8, stk_idx_copy as u16);

            compiler.push_inst(OpCode::Get, stk_ex as u8, stk_idx_copy as u16);

            Ok(stk_idx_copy.into())
        }
        ExprEnum::FieldAccess {
            prefix,
            postfix,
            def,
        } => {
            let def = def;
            let st_ty = def.as_ref().ok_or_else(|| {
                CompileError::new(prefix.span, CEK::TypeNameNotFound(expr_to_string(prefix)))
            })?;

            let field_info = st_ty
                .field_info(postfix, &compiler.env.typedefs)
                .ok_or_else(|| {
                    CompileError::new(prefix.span, CEK::FieldNotFound(postfix.to_string()))
                })?;

            let stk_ex = emit_expr(prefix, compiler)?;
            // let stk_idx = compiler.find_or_create_literal(&Value::I64(idx as i64));
            // let stk_idx_copy = compiler.target_stack.len();
            // compiler.target_stack.push(Target::None);
            // compiler.push_inst(OpCode::Move, stk_idx as u8, stk_idx_copy as u16);

            // compiler.push_inst(OpCode::Get, stk_ex as u8, stk_idx_copy as u16);

            Ok(RValue {
                start: stk_ex.start + field_info.offset,
                size: ty_size_of(expr.span, &field_info.field.ty, &compiler.env.typedefs)?,
            })
        }
        ExprEnum::LT(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::Lt, lhs, rhs)?.into()),
        ExprEnum::LE(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::Le, lhs, rhs)?.into()),
        ExprEnum::GT(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::Gt, lhs, rhs)?.into()),
        ExprEnum::GE(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::Ge, lhs, rhs)?.into()),
        ExprEnum::EQ(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::Eq, lhs, rhs)?.into()),
        ExprEnum::NE(lhs, rhs) => {
            let val = emit_binary_op(compiler, OpCode::Eq, lhs, rhs)?;
            compiler.push_inst(OpCode::Not, val as u8, 0);
            Ok(val.into())
        }
        ExprEnum::BitAnd(lhs, rhs) => {
            Ok(emit_binary_op(compiler, OpCode::BitAnd, lhs, rhs)?.into())
        }
        ExprEnum::BitXor(lhs, rhs) => {
            Ok(emit_binary_op(compiler, OpCode::BitXor, lhs, rhs)?.into())
        }
        ExprEnum::BitOr(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::BitOr, lhs, rhs)?.into()),
        ExprEnum::And(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::And, lhs, rhs)?.into()),
        ExprEnum::Or(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::Or, lhs, rhs)?.into()),
        ExprEnum::Conditional(cond, true_branch, false_branch) => {
            let span = cond.span;
            let cond = scalar(cond.span, emit_expr(cond, compiler)?)?;
            let cond_inst_idx = compiler.bytecode.instructions.len();
            compiler.push_inst(OpCode::Jf, cond as u8, 0);
            let true_branch = emit_stmts(true_branch, compiler)?;
            if let Some(false_branch) = false_branch {
                let true_inst_idx = compiler.bytecode.instructions.len();
                compiler.push_inst(OpCode::Jmp, 0, 0);
                compiler.bytecode.instructions[cond_inst_idx].arg1 =
                    compiler.bytecode.instructions.len() as u16;
                if let Some((false_branch, true_branch)) =
                    emit_stmts(false_branch, compiler)?.zip(true_branch)
                {
                    if false_branch.size != true_branch.size {
                        return Err(CompileError::new(span, CEK::DisallowedAggregate));
                    }
                    for i in 0..false_branch.size {
                        compiler.push_inst(
                            OpCode::Move,
                            (false_branch.start + i) as u8,
                            (true_branch.start + i) as u16,
                        );
                    }
                }
                compiler.bytecode.instructions[true_inst_idx].arg1 =
                    compiler.bytecode.instructions.len() as u16;
            } else {
                compiler.bytecode.instructions[cond_inst_idx].arg1 =
                    compiler.bytecode.instructions.len() as u16;
            }
            Ok(true_branch.unwrap_or(0.into()))
        }
        ExprEnum::Brace(stmts) => {
            compiler.locals.push(vec![]);
            let res = emit_stmts(stmts, compiler)?.unwrap_or_else(|| 0.into());
            compiler.locals.pop();
            Ok(res)
        }
        ExprEnum::StructLiteral {
            name: st_name,
            fields,
            def,
        } => {
            // TODO: work around clone for the borrow checker
            let st_ty = def.as_ref().ok_or_else(|| {
                CompileError::new(*st_name, CEK::TypeNameNotFound(st_name.to_string()))
            })?;

            let mut arg_values =
                vec![None; size_of_struct(*st_name, st_ty, &compiler.env.typedefs)?];

            // Emit expressions in each field of the struct.
            for (name, ex) in fields {
                let idx = st_ty
                    .offset_of(name, &compiler.env.typedefs)
                    .ok_or_else(|| {
                        CompileError::new(*name, CEK::FieldNotFound(name.to_string()))
                    })?;

                let stk_from = emit_expr(ex, compiler)?;
                for (arg, from) in arg_values[idx..]
                    .iter_mut()
                    .zip(stk_from.start..stk_from.start + stk_from.size)
                {
                    *arg = Some(from);
                }
            }

            // // Reserve the stack slot to return and load the struct name at the same time.
            // let stk_ret = compiler.find_or_create_literal(&Value::Str(st_name.to_string()));

            // If a named argument is duplicate, you would have a hole in actual args.
            // Until we have the default parameter value, it would be a compile error.
            let arg_values = arg_values
                .into_iter()
                .collect::<Option<Vec<_>>>()
                .ok_or_else(|| {
                    CompileError::new(expr.span, CEK::FieldNotInitialized(st_name.to_string()))
                })?;

            let struct_start = compiler.target_stack.len();

            // Copy the elements in a contiguous region of the stack.
            // It is important to not call emit_expr between those elements, since they can push temporary variables.
            for (i, arg) in arg_values.iter().enumerate() {
                let arg_target = compiler.target_stack.len();
                compiler.target_stack.push(Target::StructField {
                    start: struct_start,
                    idx: i,
                });
                compiler.push_inst(OpCode::Move, *arg as u8, arg_target as u16);
            }

            // compiler.push_inst(OpCode::MakeStruct, arg_values.len() as u8, stk_ret as u16);
            Ok(RValue {
                start: struct_start,
                size: arg_values.len(),
            })
        }
    };
    compiler.end_src_pos(expr.span.into());
    res
}

fn emit_binary_op<'src>(
    compiler: &mut Compiler<'_, 'src>,
    op: OpCode,
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
) -> CompileResult<'src, usize> {
    let lhs = scalar(lhs.span, emit_expr(&lhs, compiler)?)?;
    let rhs = scalar(rhs.span, emit_expr(&rhs, compiler)?)?;
    let lhs = if matches!(compiler.target_stack[lhs], Target::Local(_)) {
        // We move the local variable to another slot because our instructions are destructive
        let top = compiler.target_stack.len();
        compiler.push_inst(OpCode::Move, lhs as u8, top as u16);
        compiler.target_stack.push(Target::None);
        top
    } else {
        lhs
    };
    compiler.push_inst(op, lhs as u8, rhs as u16);
    Ok(lhs)
}

fn ty_size_of<'src>(
    span: Span<'src>,
    ty: &TypeDecl,
    typedefs: &TypeMapRc,
) -> Result<usize, CompileError<'src>> {
    use TypeDecl::*;
    match ty {
        Any | F32 | I32 | F64 | I64 | Str | Array(_, _) | Tuple(_) => Ok(1),
        TypeName(name) => {
            let st_ty = typedefs
                .get(name)
                .ok_or_else(|| CompileError::new(span, CEK::DisallowedAggregate))?;
            size_of_struct(span, st_ty, typedefs)
        }
    }
}

fn size_of_struct<'src>(
    span: Span<'src>,
    st_ty: &StructDecl,
    typedefs: &TypeMapRc,
) -> Result<usize, CompileError<'src>> {
    let ret = st_ty
        .fields
        .iter()
        .fold(Ok(0), |acc, cur| -> Result<usize, CompileError> {
            acc.and_then(|acc| Ok(acc + ty_size_of(span, &cur.ty, typedefs)?))
        })?;
    dbg_println!("sizeof({}) = {ret}", st_ty.name);
    Ok(ret)
}

fn expr_to_string(ex: &Expression) -> String {
    let mut buf = vec![0u8; 0];
    if format_expr(ex, 0, &mut buf).is_ok() {
        String::from_utf8_lossy(&buf).to_string()
    } else {
        "???".to_string()
    }
}
