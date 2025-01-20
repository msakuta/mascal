mod error;
mod lvalue;

use std::{cell::RefCell, collections::HashMap, io::Write, rc::Rc};

use self::{
    error::{CompileError, CompileErrorKind as CEK},
    lvalue::{emit_lvalue, LValue},
};

use crate::{
    bytecode::{
        std_functions, Bytecode, BytecodeArg, FnBytecode, FnProto, FunctionInfo, Instruction,
        LineInfo, NativeFn, OpCode,
    },
    interpreter::{eval, EvalContext, RunResult},
    parser::{ExprEnum, Expression, Statement},
    value::{ArrayInt, TupleEntry},
    DebugInfo, Span, TypeDecl, Value,
};

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
}

impl Default for Target {
    fn default() -> Target {
        Self::None
    }
}

#[derive(Debug)]
struct LocalVar {
    name: String,
    stack_idx: usize,
}

struct CompilerEnv {
    functions: HashMap<String, FnProto>,
    debug: HashMap<String, FunctionInfo>,
}

impl CompilerEnv {
    fn new(mut functions: HashMap<String, FnProto>, debug: HashMap<String, FunctionInfo>) -> Self {
        let out = Rc::new(RefCell::new(std::io::stdout()));
        std_functions(out, &mut |name, f| {
            functions.insert(name, FnProto::Native(f));
        });
        Self { functions, debug }
    }
}

struct Compiler<'a> {
    env: &'a mut CompilerEnv,
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

impl<'a> Compiler<'a> {
    fn new(args: Vec<LocalVar>, fn_args: Vec<BytecodeArg>, env: &'a mut CompilerEnv) -> Self {
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
            target_stack: (0..args.len() + 1)
                .map(|i| {
                    if i == 0 {
                        Target::None
                    } else {
                        Target::Local(i - 1)
                    }
                })
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

    fn find_local<'src>(&self, name: &str, span: Span<'src>) -> CompileResult<'src, &LocalVar> {
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
        if let Some(last_target) = emit_stmts(self.stmts, &mut compiler)? {
            compiler.bytecode.instructions.push(Instruction::new(
                OpCode::Ret,
                0,
                last_target as u16,
            ));
        }
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
    env: &mut CompilerEnv,
    name: String,
    src_pos: &CompSourcePos,
    stmts: &'ast [Statement<'src>],
    args: Vec<LocalVar>,
    fn_args: Vec<BytecodeArg>,
) -> CompileResult<'src, (FnProto, FunctionInfo)> {
    let mut compiler = Compiler::new(args, fn_args, env);
    compiler.start_src_pos(*src_pos);
    if let Some(last_target) = emit_stmts(stmts, &mut compiler)? {
        compiler
            .bytecode
            .instructions
            .push(Instruction::new(OpCode::Ret, 0, last_target as u16));
    }
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
    compiler: &mut Compiler,
) -> CompileResult<'src, Option<usize>> {
    let mut last_target = None;
    for stmt in stmts {
        match stmt {
            Statement::VarDecl(var, _type, initializer) => {
                let locals = compiler
                    .locals
                    .last()
                    .ok_or_else(|| CompileError::new(*var, CEK::LocalsStackUnderflow))?
                    .len();
                let init_val = if let Some(init_expr) = initializer {
                    let stk_var = emit_expr(init_expr, compiler)?;
                    compiler.target_stack[stk_var] = Target::Local(locals);
                    stk_var
                } else {
                    let stk_var = compiler.target_stack.len();
                    compiler.target_stack.push(Target::Local(locals));
                    stk_var
                };
                let locals = compiler
                    .locals
                    .last_mut()
                    .ok_or_else(|| CompileError::new(*var, CEK::LocalsStackUnderflow))?;
                locals.push(LocalVar {
                    name: var.to_string(),
                    stack_idx: init_val,
                });
                dbg_println!("Locals: {:?}", compiler.locals);
            }
            Statement::FnDecl {
                name, args, stmts, ..
            } => {
                dbg_println!("FnDecl: Args: {:?}", args);
                let a_args = args
                    .iter()
                    .enumerate()
                    .map(|(idx, arg)| {
                        // The 0th index is used for function name / return value, so the args start with 1.
                        let target = idx + 1;
                        let local = LocalVar {
                            name: arg.name.to_owned(),
                            stack_idx: target,
                        };
                        compiler.target_stack.push(Target::Local(target));
                        local
                    })
                    .collect();
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
                        Ok(BytecodeArg::new(arg.name.to_owned(), init))
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
            Statement::Expression(ref ex) => {
                last_target = Some(emit_expr(ex, compiler)?);
            }
            Statement::Loop(stmts) => {
                let inst_loop_start = compiler.bytecode.instructions.len();
                last_target = emit_stmts(stmts, compiler)?;
                compiler.push_inst(OpCode::Jmp, 0, inst_loop_start as u16);
                compiler.fixup_breaks();
            }
            Statement::While(cond, stmts) => {
                let inst_loop_start = compiler.bytecode.instructions.len();
                let stk_cond = emit_expr(cond, compiler)?;
                let inst_break = compiler.push_inst(OpCode::Jf, stk_cond as u8, 0);
                last_target = emit_stmts(stmts, compiler)?;
                compiler.push_inst(OpCode::Jmp, 0, inst_loop_start as u16);
                compiler.bytecode.instructions[inst_break].arg1 =
                    compiler.bytecode.instructions.len() as u16;
                compiler.fixup_breaks();
            }
            Statement::For(iter, from, to, stmts) => {
                let stk_from = emit_expr(from, compiler)?;
                let stk_to = emit_expr(to, compiler)?;
                let local_iter = compiler
                    .locals
                    .last()
                    .ok_or_else(|| CompileError::new(*iter, CEK::LocalsStackUnderflow))?
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
                    .ok_or_else(|| CompileError::new(*iter, CEK::LocalsStackUnderflow))?
                    .push(LocalVar {
                        name: iter.to_string(),
                        stack_idx: stk_from,
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
            Statement::Comment(_) => (),
        }
    }
    Ok(last_target)
}

fn emit_expr<'src>(expr: &Expression<'src>, compiler: &mut Compiler) -> CompileResult<'src, usize> {
    compiler.start_src_pos(expr.span.into());
    let res = match &expr.expr {
        ExprEnum::NumLiteral(val, _) => Ok(compiler.find_or_create_literal(val)),
        ExprEnum::StrLiteral(val) => Ok(compiler.find_or_create_literal(&Value::Str(val.clone()))),
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
            Ok(compiler.find_or_create_literal(&val))
        }
        ExprEnum::TupleLiteral(val) => {
            let mut ctx = EvalContext::new();
            let val = Value::Tuple(Rc::new(RefCell::new(
                val.iter()
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
                    .collect::<Result<Vec<_>, _>>()?,
            )));
            Ok(compiler.find_or_create_literal(&val))
        }
        ExprEnum::Variable(str) => {
            let local = compiler.find_local(*str, expr.span)?;
            Ok(local.stack_idx)
        }
        ExprEnum::Cast(ex, decl) => {
            let val = emit_expr(ex, compiler)?;
            // Make a copy of the value to avoid overwriting original variable
            let val_copy = compiler.target_stack.len();
            compiler.push_inst(OpCode::Move, val as u8, val_copy as u16);
            compiler.target_stack.push(Target::None);
            let mut decl_buf = [0u8; std::mem::size_of::<i64>()];
            decl.serialize(&mut std::io::Cursor::new(&mut decl_buf[..]))?;
            let decl_stk =
                compiler.find_or_create_literal(&Value::I64(i64::from_le_bytes(decl_buf)));
            compiler.push_inst(OpCode::Cast, val_copy as u8, decl_stk as u16);
            Ok(val_copy)
        }
        ExprEnum::Not(val) => {
            let val = emit_expr(val, compiler)?;
            compiler.push_inst(OpCode::Not, val as u8, 0);
            Ok(val)
        }
        ExprEnum::BitNot(val) => {
            let val = emit_expr(val, compiler)?;
            compiler.push_inst(OpCode::BitNot, val as u8, 0);
            Ok(val)
        }
        ExprEnum::Neg(val) => {
            let val = emit_expr(val, compiler)?;
            compiler.push_inst(OpCode::Neg, val as u8, 0);
            Ok(val)
        }
        ExprEnum::Add(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::Add, lhs, rhs)?),
        ExprEnum::Sub(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::Sub, lhs, rhs)?),
        ExprEnum::Mult(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::Mul, lhs, rhs)?),
        ExprEnum::Div(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::Div, lhs, rhs)?),
        ExprEnum::VarAssign(lhs, rhs) => {
            let lhs_result = emit_lvalue(lhs, compiler)?;
            let rhs_result = emit_expr(rhs, compiler)?;
            match lhs_result {
                LValue::Variable(name) => {
                    let local_idx = compiler.find_local(&name, expr.span)?.stack_idx;
                    compiler.push_inst(OpCode::Move, rhs_result as u8, local_idx as u16);
                    Ok(local_idx)
                }
                LValue::ArrayRef(arr, subidx) => {
                    let value_copy = compiler.target_stack.len();
                    compiler.target_stack.push(Target::None);

                    // First, copy the value to be overwritten by Get instruction
                    compiler.push_inst(OpCode::Move, rhs_result as u8, value_copy as u16);

                    // Second, assign the target index into the set register
                    compiler.push_inst(OpCode::SetReg, subidx as u8, 0);

                    // Third, get the element from the array reference
                    compiler.push_inst(OpCode::Set, arr as u8, value_copy as u16);

                    Ok(value_copy)
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

            // Prepare a buffer for actual arguments. It could be a mix of unnamed and named arguments.
            // Unnamed arguments are indexed from 0, while named arguments can appear at any index.
            let mut arg_values =
                vec![None; params.as_ref().map_or(args.len(), |params| params.len())];

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
                        *arg_value = Some(default_val);
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
                let arg_target = compiler.target_stack.len();
                compiler.target_stack.push(Target::None);
                compiler.push_inst(OpCode::Move, *arg as u8, arg_target as u16);
            }

            compiler.push_inst(OpCode::Call, arg_values.len() as u8, stk_fname as u16);
            compiler.target_stack.push(Target::None);
            Ok(stk_fname)
        }
        ExprEnum::ArrIndex(ex, args) => {
            let stk_ex = emit_expr(ex, compiler)?;
            let args = args
                .iter()
                .map(|v| emit_expr(v, compiler))
                .collect::<Result<Vec<_>, _>>()?;
            let arg = args[0];
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
            Ok(arg)
        }
        ExprEnum::TupleIndex(ex, index) => {
            let stk_ex = emit_expr(ex, compiler)?;
            let stk_idx = compiler.find_or_create_literal(&Value::I64(*index as i64));
            let stk_idx_copy = compiler.target_stack.len();
            compiler.target_stack.push(Target::None);
            compiler.push_inst(OpCode::Move, stk_idx as u8, stk_idx_copy as u16);

            compiler.push_inst(OpCode::Get, stk_ex as u8, stk_idx_copy as u16);

            Ok(stk_idx_copy)
        }
        ExprEnum::LT(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::Lt, lhs, rhs)?),
        ExprEnum::GT(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::Gt, lhs, rhs)?),
        ExprEnum::BitAnd(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::BitAnd, lhs, rhs)?),
        ExprEnum::BitXor(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::BitXor, lhs, rhs)?),
        ExprEnum::BitOr(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::BitOr, lhs, rhs)?),
        ExprEnum::And(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::And, lhs, rhs)?),
        ExprEnum::Or(lhs, rhs) => Ok(emit_binary_op(compiler, OpCode::Or, lhs, rhs)?),
        ExprEnum::Conditional(cond, true_branch, false_branch) => {
            let cond = emit_expr(cond, compiler)?;
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
                    compiler.push_inst(OpCode::Move, false_branch as u8, true_branch as u16);
                }
                compiler.bytecode.instructions[true_inst_idx].arg1 =
                    compiler.bytecode.instructions.len() as u16;
            } else {
                compiler.bytecode.instructions[cond_inst_idx].arg1 =
                    compiler.bytecode.instructions.len() as u16;
            }
            Ok(true_branch.unwrap_or(0))
        }
        ExprEnum::Brace(stmts) => {
            compiler.locals.push(vec![]);
            let res = emit_stmts(stmts, compiler)?.unwrap_or(0);
            compiler.locals.pop();
            Ok(res)
        }
    };
    compiler.end_src_pos(expr.span.into());
    res
}

fn emit_binary_op<'src>(
    compiler: &mut Compiler,
    op: OpCode,
    lhs: &Expression<'src>,
    rhs: &Expression<'src>,
) -> CompileResult<'src, usize> {
    let lhs = emit_expr(&lhs, compiler)?;
    let rhs = emit_expr(&rhs, compiler)?;
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
