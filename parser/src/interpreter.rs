mod lvalue;

use self::lvalue::{eval_lvalue, LValue};

use crate::{
    coercion::{coerce_f64, coerce_i64, coerce_type},
    eval_error::EvalError,
    parser::*,
    std_fns::std_functions,
    value::{ArrayInt, StructInt, TupleEntry},
    TypeDecl, Value,
};
use std::{cell::RefCell, collections::HashMap, io::Write, rc::Rc};

#[derive(Debug, PartialEq, Clone)]
pub enum RunResult {
    Yield(Value),
    Break,
}

pub type EvalResult<T> = Result<T, EvalError>;

/// An extension trait for `Vec` to write a shorthand for
/// `values.get(idx).ok_or_else(|| EvalError::ArrayOutOfBounds(idx, values.len()))`, because
/// it's too long and shows up too often behind Rc.
pub(crate) trait EGetExt<T> {
    fn eget(&self, idx: usize) -> EvalResult<&T>;
    #[allow(dead_code)]
    fn eget_mut(&mut self, idx: usize) -> EvalResult<&mut T>;
}

impl<T> EGetExt<T> for Vec<T> {
    fn eget(&self, idx: usize) -> EvalResult<&T> {
        self.get(idx)
            .ok_or_else(|| EvalError::ArrayOutOfBounds(idx, self.len()))
    }

    fn eget_mut(&mut self, idx: usize) -> EvalResult<&mut T> {
        let len = self.len();
        self.get_mut(idx)
            .ok_or_else(|| EvalError::ArrayOutOfBounds(idx, len))
    }
}

fn unwrap_deref(e: RunResult) -> EvalResult<RunResult> {
    match &e {
        RunResult::Break => return Ok(RunResult::Break),
        _ => (),
    }
    Ok(e)
}

macro_rules! unwrap_run {
    ($e:expr) => {
        match unwrap_deref($e)? {
            RunResult::Yield(v) => v,
            RunResult::Break => return Ok(RunResult::Break),
        }
    };
}

pub(crate) fn binary_op_str(
    lhs: &Value,
    rhs: &Value,
    d: impl Fn(f64, f64) -> Result<f64, EvalError>,
    i: impl Fn(i64, i64) -> i64,
    s: impl Fn(&str, &str) -> Result<String, EvalError>,
) -> EvalResult<Value> {
    Ok(match (lhs, rhs) {
        (Value::F64(lhs), rhs) => Value::F64(d(*lhs, coerce_f64(&rhs)?)?),
        (lhs, Value::F64(rhs)) => Value::F64(d(coerce_f64(&lhs)?, *rhs)?),
        (Value::F32(lhs), rhs) => Value::F32(d(*lhs as f64, coerce_f64(&rhs)?)? as f32),
        (lhs, Value::F32(rhs)) => Value::F32(d(coerce_f64(&lhs)?, *rhs as f64)? as f32),
        (Value::I64(lhs), Value::I64(rhs)) => Value::I64(i(*lhs, *rhs)),
        (Value::I64(lhs), Value::I32(rhs)) => Value::I64(i(*lhs, *rhs as i64)),
        (Value::I32(lhs), Value::I64(rhs)) => Value::I64(i(*lhs as i64, *rhs)),
        (Value::I32(lhs), Value::I32(rhs)) => Value::I32(i(*lhs as i64, *rhs as i64) as i32),
        (Value::Str(lhs), Value::Str(rhs)) => Value::Str(s(lhs, rhs)?),
        _ => return Err(EvalError::OpError(lhs.to_string(), rhs.to_string())),
    })
}

pub(crate) fn binary_op(
    lhs: &Value,
    rhs: &Value,
    d: impl Fn(f64, f64) -> f64,
    i: impl Fn(i64, i64) -> i64,
) -> EvalResult<Value> {
    binary_op_str(
        lhs,
        rhs,
        |lhs, rhs| Ok(d(lhs, rhs)),
        i,
        |lhs, rhs| Err(EvalError::StrOpError(lhs.to_string(), rhs.to_string())),
    )
}

pub(crate) fn compare_op(
    lhs: &Value,
    rhs: &Value,
    d: impl Fn(&f64, &f64) -> bool,
    i: impl Fn(&i64, &i64) -> bool,
) -> Result<Value, EvalError> {
    let d = |lhs, rhs| d(&lhs, &rhs);
    let i = |lhs, rhs| i(&lhs, &rhs);
    Ok(Value::I32(match (lhs.clone(), rhs.clone()) {
        (Value::F64(lhs), rhs) => d(lhs, coerce_f64(&rhs)?),
        (lhs, Value::F64(rhs)) => d(coerce_f64(&lhs)?, rhs),
        (Value::F32(lhs), rhs) => d(lhs as f64, coerce_f64(&rhs)?),
        (lhs, Value::F32(rhs)) => d(coerce_f64(&lhs)?, rhs as f64),
        (Value::I64(lhs), Value::I64(rhs)) => i(lhs, rhs),
        (Value::I64(lhs), Value::I32(rhs)) => i(lhs, rhs as i64),
        (Value::I32(lhs), Value::I64(rhs)) => i(lhs as i64, rhs),
        (Value::I32(lhs), Value::I32(rhs)) => i(lhs as i64, rhs as i64),
        _ => return Err(EvalError::OpError(lhs.to_string(), rhs.to_string())),
    } as i32))
}

pub(crate) fn binary_op_int(
    lhs: &Value,
    rhs: &Value,
    i: impl Fn(i64, i64) -> i64,
) -> EvalResult<Value> {
    binary_op_str(
        lhs,
        rhs,
        |lhs, rhs| Err(EvalError::FloatOpError(lhs.to_string(), rhs.to_string())),
        i,
        |lhs, rhs| Err(EvalError::StrOpError(lhs.to_string(), rhs.to_string())),
    )
}

pub(crate) fn truthy(a: &Value) -> bool {
    match a {
        Value::F64(v) => *v != 0.,
        Value::F32(v) => *v != 0.,
        Value::I64(v) => *v != 0,
        Value::I32(v) => *v != 0,
        _ => false,
    }
}

fn eval_array_literal<'src, 'native>(
    val: &[Vec<Expression<'src>>],
    ctx: &mut EvalContext<'src, 'native, '_>,
) -> EvalResult<RunResult>
where
    'native: 'src,
{
    let Some(cols) = val.first().map(|row| row.len()) else {
        // An empty array has 1 dimension by convention
        let int = ArrayInt::new(TypeDecl::Any, vec![0], vec![]);
        return Ok(RunResult::Yield(Value::Array(int)));
    };

    let total_size = val.len() * cols;

    // Validate array shape
    for row in val {
        if row.len() != cols {
            return Err(EvalError::NonRectangularArray);
        }
    }

    let mut rows = Vec::with_capacity(total_size);
    for row in val.iter() {
        for cell in row.iter() {
            if let RunResult::Yield(y) = eval(cell, ctx)? {
                rows.push(y);
            } else {
                return Err(EvalError::DisallowedBreak);
            }
        }
    }

    let shape = if val.len() == 1 {
        vec![cols]
    } else {
        vec![val.len(), cols]
    };

    Ok(RunResult::Yield(Value::Array(ArrayInt::new(
        rows.first().map_or(TypeDecl::Any, TypeDecl::from_value),
        shape,
        rows,
    ))))
}

pub(crate) fn eval<'src, 'native>(
    e: &Expression<'src>,
    ctx: &mut EvalContext<'src, 'native, '_>,
) -> EvalResult<RunResult>
where
    'native: 'src,
{
    Ok(match &e.expr {
        ExprEnum::NumLiteral(val, _) => RunResult::Yield(val.clone()),
        ExprEnum::StrLiteral(val) => RunResult::Yield(Value::Str(val.clone())),
        ExprEnum::ArrLiteral(val) => eval_array_literal(val, ctx)?,
        ExprEnum::TupleLiteral(val) => RunResult::Yield(Value::Tuple(Rc::new(RefCell::new(
            val.iter()
                .map(|v| {
                    if let RunResult::Yield(y) = unwrap_deref(eval(v, ctx)?)? {
                        Ok(TupleEntry {
                            decl: TypeDecl::from_value(&y),
                            value: y,
                        })
                    } else {
                        Err(EvalError::DisallowedBreak)
                    }
                })
                .collect::<Result<Vec<_>, _>>()?,
        )))),
        ExprEnum::StructLiteral { name, fields, .. } => {
            // TODO: work around clone for the borrow checker
            let st_ty = ctx
                .get_type(**name)
                .ok_or_else(|| EvalError::NoStructFound(name.to_string()))?
                .clone();

            RunResult::Yield(Value::Struct(Rc::new(RefCell::new(StructInt {
                name: name.to_string(),
                fields: st_ty
                    .fields
                    .iter()
                    .map(|field_ty| {
                        let (_, ex) = fields
                            .iter()
                            .find(|field| *field.0 == *field_ty.name)
                            .ok_or_else(|| EvalError::NoFieldFound(field_ty.name.to_string()))?;

                        if let RunResult::Yield(y) = unwrap_deref(eval(ex, ctx)?)? {
                            Ok(y)
                        } else {
                            Err(EvalError::DisallowedBreak)
                        }
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            }))))
        }
        ExprEnum::Variable(str) => RunResult::Yield(
            ctx.get_var(str)
                .ok_or_else(|| EvalError::VarNotFound(str.to_string()))?,
        ),
        ExprEnum::Cast(ex, decl) => {
            RunResult::Yield(coerce_type(&unwrap_run!(eval(ex, ctx)?), decl)?)
        }
        ExprEnum::VarAssign(lhs, rhs) => {
            let rhs_value = unwrap_run!(eval(rhs, ctx)?);
            let lhs_result = eval_lvalue(lhs, ctx)?;
            match lhs_result {
                LValue::Variable(name) => {
                    if let Some(var) = ctx.variables.borrow_mut().get_mut(name.as_str()) {
                        *var.borrow_mut() = rhs_value.clone();
                    }
                }
                LValue::ArrayRef(arr, idx) => arr.borrow_mut().values[idx] = rhs_value.clone(),
                LValue::StructRef(st, idx) => st.borrow_mut().fields[idx] = rhs_value.clone(),
            }
            RunResult::Yield(rhs_value)
        }
        ExprEnum::FnInvoke(fname, args) => {
            let fn_args = ctx
                .get_fn(*fname)
                .ok_or_else(|| EvalError::FnNotFound(fname.to_string()))?
                .args()
                .clone();

            let mut eval_args = vec![None; fn_args.len().max(args.len())];

            // Fill unnamed args
            for (arg, eval_arg) in args.iter().zip(eval_args.iter_mut()) {
                if arg.name.is_none() {
                    *eval_arg = Some(eval(&arg.expr, ctx)?);
                }
            }

            // Find and assign named args
            for arg in args.iter() {
                if let Some(name) = arg.name {
                    if let Some(eval_arg) = fn_args
                        .iter()
                        .enumerate()
                        .find(|f| *f.1.name == *name)
                        .and_then(|(i, _)| eval_args.get_mut(i))
                    {
                        *eval_arg = Some(eval(&arg.expr, ctx)?);
                    } else {
                        return Err(EvalError::VarNotFound(name.to_string()));
                    }
                }
            }

            for (arg, fn_arg) in eval_args.iter_mut().zip(fn_args.iter()) {
                if arg.is_some() {
                    continue;
                }
                if let Some(ref init) = fn_arg.init {
                    // We use a new temporary EvalContext to avoid referencing outer variables, i.e. make it
                    // a constant expression, in order to match the semantics with the bytecode compiler.
                    // Theoretically, it is possible to evaluate the expression ahead of time to reduce
                    // computation, but our priority is bytecode compiler which already does constant folding.
                    *arg = Some(eval(init, &mut EvalContext::new())?);
                }
            }

            let func = ctx
                .get_fn(*fname)
                .ok_or_else(|| EvalError::FnNotFound(fname.to_string()))?;

            let mut subctx = EvalContext::push_stack(ctx);
            match func {
                FuncDef::Code(func) => {
                    for (k, v) in func.args.iter().zip(&eval_args) {
                        if let Some(v) = v {
                            subctx.variables.borrow_mut().insert(
                                *k.name,
                                Rc::new(RefCell::new(coerce_type(&unwrap_run!(v.clone()), &k.ty)?)),
                            );
                        } else {
                            return Err(EvalError::MissingArg(k.name.to_string()));
                        }
                    }
                    let run_result = run(&func.stmts, &mut subctx)?;
                    match unwrap_deref(run_result)? {
                        RunResult::Yield(v) => match &func.ret_type {
                            RetType::Some(ty) => RunResult::Yield(coerce_type(&v, ty)?),
                            RetType::Void => RunResult::Yield(v),
                        },
                        RunResult::Break => return Err(EvalError::BreakInToplevel),
                    }
                }
                FuncDef::Native(native) => RunResult::Yield((native.code)(
                    &eval_args
                        .into_iter()
                        .map(|e| match e {
                            Some(RunResult::Yield(v)) => Ok(v.clone()),
                            Some(RunResult::Break) => Err(EvalError::BreakInFnArg),
                            _ => Err(EvalError::MissingArg("arg".to_string())),
                        })
                        .collect::<Result<Vec<_>, _>>()?,
                )?),
            }
        }
        ExprEnum::ArrIndex(ex, args) => {
            let args = args
                .iter()
                .map(|v| eval(v, ctx))
                .collect::<Result<Vec<_>, _>>()?;
            let arg0 = match unwrap_deref(args[0].clone())? {
                RunResult::Yield(v) => {
                    if let Value::I64(idx) = coerce_type(&v, &TypeDecl::I64)? {
                        idx as u64
                    } else {
                        return Err(EvalError::NonIntegerIndex);
                    }
                }
                RunResult::Break => {
                    return Ok(RunResult::Break);
                }
            };
            let result = unwrap_run!(eval(ex, ctx)?);
            RunResult::Yield(result.array_get(arg0)?)
        }
        ExprEnum::TupleIndex(ex, index) => {
            let result = unwrap_run!(eval(ex, ctx)?);
            RunResult::Yield(result.tuple_get(*index as u64)?)
        }
        ExprEnum::FieldAccess {
            prefix: ex,
            postfix: field_name,
            ..
        } => {
            let result = unwrap_run!(eval(ex, ctx)?);

            let st_ty = TypeDecl::from_value(&result);
            let TypeDecl::TypeName(st_name) = st_ty else {
                return Err(EvalError::TypeCheck("Type must be named".to_string()));
            };
            let st_ty = &ctx
                .get_type(&st_name)
                .ok_or_else(|| EvalError::NoStructFound(st_name.to_string()))?;
            let (idx, _) = st_ty
                .fields
                .iter()
                .enumerate()
                .find(|(_, field)| *field.name == **field_name)
                .ok_or_else(|| EvalError::NoFieldFound(field_name.to_string()))?;

            RunResult::Yield(result.struct_field(idx as u64)?)
        }
        ExprEnum::Not(val) => {
            RunResult::Yield(Value::I32(if truthy(&unwrap_run!(eval(val, ctx)?)) {
                0
            } else {
                1
            }))
        }
        ExprEnum::BitNot(val) => {
            let val = unwrap_run!(eval(val, ctx)?);
            RunResult::Yield(match val {
                Value::I32(i) => Value::I32(!i),
                Value::I64(i) => Value::I64(!i),
                _ => return Err(EvalError::NonIntegerBitwise(format!("{val:?}"))),
            })
        }
        ExprEnum::Neg(val) => {
            let val = unwrap_run!(eval(val, ctx)?);
            RunResult::Yield(match val {
                Value::I32(i) => Value::I32(-i),
                Value::I64(i) => Value::I64(-i),
                Value::F32(i) => Value::F32(-i),
                Value::F64(i) => Value::F64(-i),
                _ => return Err(EvalError::NonIntegerBitwise(format!("{val:?}"))),
            })
        }
        ExprEnum::Add(lhs, rhs) => {
            let res = RunResult::Yield(binary_op_str(
                &unwrap_run!(eval(lhs, ctx)?),
                &unwrap_run!(eval(rhs, ctx)?),
                |lhs, rhs| Ok(lhs + rhs),
                |lhs, rhs| lhs + rhs,
                |lhs: &str, rhs: &str| Ok(lhs.to_string() + rhs),
            )?);
            res
        }
        ExprEnum::Sub(lhs, rhs) => RunResult::Yield(binary_op(
            &unwrap_run!(eval(lhs, ctx)?),
            &unwrap_run!(eval(rhs, ctx)?),
            |lhs, rhs| lhs - rhs,
            |lhs, rhs| lhs - rhs,
        )?),
        ExprEnum::Mult(lhs, rhs) => RunResult::Yield(binary_op(
            &unwrap_run!(eval(lhs, ctx)?),
            &unwrap_run!(eval(rhs, ctx)?),
            |lhs, rhs| lhs * rhs,
            |lhs, rhs| lhs * rhs,
        )?),
        ExprEnum::Div(lhs, rhs) => RunResult::Yield(binary_op(
            &unwrap_run!(eval(lhs, ctx)?),
            &unwrap_run!(eval(rhs, ctx)?),
            |lhs, rhs| lhs / rhs,
            |lhs, rhs| lhs / rhs,
        )?),
        ExprEnum::LT(lhs, rhs) => RunResult::Yield(compare_op(
            &unwrap_run!(eval(lhs, ctx)?),
            &unwrap_run!(eval(rhs, ctx)?),
            f64::lt,
            i64::lt,
        )?),
        ExprEnum::LE(lhs, rhs) => RunResult::Yield(compare_op(
            &unwrap_run!(eval(lhs, ctx)?),
            &unwrap_run!(eval(rhs, ctx)?),
            f64::le,
            i64::le,
        )?),
        ExprEnum::GT(lhs, rhs) => RunResult::Yield(compare_op(
            &unwrap_run!(eval(lhs, ctx)?),
            &unwrap_run!(eval(rhs, ctx)?),
            f64::gt,
            i64::gt,
        )?),
        ExprEnum::GE(lhs, rhs) => RunResult::Yield(compare_op(
            &unwrap_run!(eval(lhs, ctx)?),
            &unwrap_run!(eval(rhs, ctx)?),
            f64::ge,
            i64::ge,
        )?),
        ExprEnum::EQ(lhs, rhs) => RunResult::Yield(compare_op(
            &unwrap_run!(eval(lhs, ctx)?),
            &unwrap_run!(eval(rhs, ctx)?),
            f64::eq,
            i64::eq,
        )?),
        ExprEnum::NE(lhs, rhs) => RunResult::Yield(compare_op(
            &unwrap_run!(eval(lhs, ctx)?),
            &unwrap_run!(eval(rhs, ctx)?),
            f64::ne,
            i64::ne,
        )?),
        ExprEnum::BitAnd(lhs, rhs) => RunResult::Yield(binary_op_int(
            &unwrap_run!(eval(lhs, ctx)?),
            &unwrap_run!(eval(rhs, ctx)?),
            |lhs, rhs| lhs & rhs,
        )?),
        ExprEnum::BitXor(lhs, rhs) => RunResult::Yield(binary_op_int(
            &unwrap_run!(eval(lhs, ctx)?),
            &unwrap_run!(eval(rhs, ctx)?),
            |lhs, rhs| lhs ^ rhs,
        )?),
        ExprEnum::BitOr(lhs, rhs) => RunResult::Yield(binary_op_int(
            &unwrap_run!(eval(lhs, ctx)?),
            &unwrap_run!(eval(rhs, ctx)?),
            |lhs, rhs| lhs | rhs,
        )?),
        ExprEnum::And(lhs, rhs) => RunResult::Yield(Value::I32(
            if truthy(&unwrap_run!(eval(lhs, ctx)?)) && truthy(&unwrap_run!(eval(rhs, ctx)?)) {
                1
            } else {
                0
            },
        )),
        ExprEnum::Or(lhs, rhs) => RunResult::Yield(Value::I32(
            if truthy(&unwrap_run!(eval(lhs, ctx)?)) || truthy(&unwrap_run!(eval(rhs, ctx)?)) {
                1
            } else {
                0
            },
        )),
        ExprEnum::Conditional(cond, true_branch, false_branch) => {
            if truthy(&unwrap_run!(eval(cond, ctx)?)) {
                run(true_branch, ctx)?
            } else if let Some(ast) = false_branch {
                run(ast, ctx)?
            } else {
                RunResult::Yield(Value::I32(0))
            }
        }
        ExprEnum::Brace(stmts) => {
            let mut subctx = EvalContext::push_stack(ctx);
            run(stmts, &mut subctx)?
        }
    })
}

#[derive(Clone)]
pub struct FuncCode<'src> {
    args: Vec<ArgDecl<'src>>,
    pub(crate) ret_type: RetType,
    /// Owning a clone of AST of statements is not quite efficient, but we could not get
    /// around the borrow checker.
    stmts: Rc<Vec<Statement<'src>>>,
}

impl<'src> FuncCode<'src> {
    pub(crate) fn new(
        stmts: Rc<Vec<Statement<'src>>>,
        args: Vec<ArgDecl<'src>>,
        ret_type: RetType,
    ) -> Self {
        Self {
            args,
            ret_type,
            stmts,
        }
    }
}

/// A type for function return types. It has one extra state to usual TypeDef,
/// which is Void. It merely wraps TypeDecl and Void in an enum.
/// It is almost equivalent to std::option::Option, except it has intention to
/// indicate Void-able type.
#[derive(Debug, PartialEq, Clone)]
pub enum RetType {
    Void,
    Some(TypeDecl),
}

impl RetType {
    pub fn unwrap_or(&self, default: TypeDecl) -> TypeDecl {
        match self {
            Self::Void => default,
            Self::Some(val) => val.clone(),
        }
    }

    pub fn unwrap_or_any(&self) -> TypeDecl {
        self.unwrap_or(TypeDecl::Any)
    }

    pub fn ok_or_else<E>(&self, f: impl FnOnce() -> E) -> Result<TypeDecl, E> {
        match self {
            Self::Void => Err(f()),
            Self::Some(val) => Ok(val.clone()),
        }
    }

    pub fn as_opt(&self) -> Option<&TypeDecl> {
        match self {
            Self::Void => None,
            Self::Some(val) => Some(val),
        }
    }
}

impl std::fmt::Display for RetType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Some(v) => v.fmt(f),
            _ => write!(f, "void"),
        }
    }
}

#[derive(Clone)]
pub struct NativeCode<'native> {
    args: Vec<ArgDecl<'native>>,
    pub(crate) ret_type: Option<TypeDecl>,
    code: &'native dyn Fn(&[Value]) -> Result<Value, EvalError>,
}

impl<'native> NativeCode<'native> {
    pub(crate) fn new(
        code: &'native dyn Fn(&[Value]) -> Result<Value, EvalError>,
        args: Vec<ArgDecl<'native>>,
        ret_type: Option<TypeDecl>,
    ) -> Self {
        Self {
            args,
            ret_type,
            code,
        }
    }
}

#[derive(Clone)]
pub enum FuncDef<'src, 'native> {
    Code(FuncCode<'src>),
    Native(NativeCode<'native>),
}

impl<'src, 'native> FuncDef<'src, 'native> {
    pub fn new_native(
        code: &'native dyn Fn(&[Value]) -> Result<Value, EvalError>,
        args: Vec<ArgDecl<'native>>,
        ret_type: Option<TypeDecl>,
    ) -> Self {
        Self::Native(NativeCode::new(code, args, ret_type))
    }

    pub(crate) fn args(&self) -> &Vec<ArgDecl<'src>>
    where
        'native: 'src,
    {
        match self {
            FuncDef::Code(FuncCode { args, .. }) => args,
            FuncDef::Native(NativeCode { args, .. }) => args,
        }
    }
}

pub(crate) type TypeMap<'src> = HashMap<String, StructDecl<'src>>;
pub(crate) type TypeMapRc<'src> = HashMap<String, Rc<StructDecl<'src>>>;

/// A context stat for evaluating a script.
///
/// It has 3 lifetime arguments:
///  * the source code ('src)
///  * the native function code ('native) and
///  * the parent eval context ('ctx)
///
/// In general, they all can have different lifetimes.
#[derive(Clone)]
pub struct EvalContext<'src, 'native, 'ctx> {
    /// RefCell to allow mutation in super context.
    /// Also, the inner values must be Rc of RefCell because a reference could be returned from
    /// a function so that the variable scope may have been ended.
    variables: RefCell<HashMap<&'src str, Rc<RefCell<Value>>>>,
    /// Function names are owned strings because it can be either from source or native.
    /// Unlike variables, functions cannot be overwritten in the outer scope, so it does not
    /// need to be wrapped in a RefCell.
    functions: HashMap<String, FuncDef<'src, 'native>>,
    typedefs: TypeMap<'src>,
    super_context: Option<&'ctx EvalContext<'src, 'native, 'ctx>>,
}

impl<'src, 'ast, 'native, 'ctx> EvalContext<'src, 'native, 'ctx> {
    pub fn new() -> Self {
        Self {
            variables: RefCell::new(HashMap::new()),
            functions: std_functions(),
            typedefs: HashMap::new(),
            super_context: None,
        }
    }

    pub fn set_fn(&mut self, name: &str, fun: FuncDef<'src, 'native>) {
        self.functions.insert(name.to_string(), fun);
    }

    fn push_stack(super_ctx: &'ctx Self) -> Self {
        Self {
            variables: RefCell::new(HashMap::new()),
            functions: HashMap::new(),
            typedefs: HashMap::new(),
            super_context: Some(super_ctx),
        }
    }

    fn get_var(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.variables.borrow().get(name) {
            Some(val.borrow().clone())
        } else if let Some(super_ctx) = self.super_context {
            super_ctx.get_var(name)
        } else {
            None
        }
    }

    fn _get_var_rc(&self, name: &str) -> Option<Rc<RefCell<Value>>> {
        if let Some(val) = self.variables.borrow().get(name) {
            Some(val.clone())
        } else if let Some(super_ctx) = self.super_context {
            super_ctx._get_var_rc(name)
        } else {
            None
        }
    }

    fn get_fn(&self, name: &str) -> Option<&FuncDef<'src, 'native>> {
        if let Some(val) = self.functions.get(name) {
            Some(val)
        } else if let Some(super_ctx) = self.super_context {
            super_ctx.get_fn(name)
        } else {
            None
        }
    }

    fn get_type(&self, name: &str) -> Option<&StructDecl<'src>> {
        self.typedefs
            .get(name)
            .or_else(|| self.super_context.and_then(|sc| sc.get_type(name)))
    }
}

macro_rules! unwrap_break {
    ($e:expr) => {
        match $e {
            RunResult::Yield(v) => v,
            RunResult::Break => break,
        }
    };
}

pub fn run<'src, 'native>(
    stmts: &Vec<Statement<'src>>,
    ctx: &mut EvalContext<'src, 'native, '_>,
) -> Result<RunResult, EvalError>
where
    'native: 'src,
{
    let mut res = RunResult::Yield(Value::I32(0));
    for stmt in stmts {
        match stmt {
            Statement::VarDecl {
                name: var,
                ty,
                init,
                ..
            } => {
                let init_val = if let Some(init_expr) = init {
                    unwrap_break!(eval(init_expr, ctx)?)
                } else {
                    Value::I32(0)
                };
                let init_val = coerce_type(&init_val, ty)?;
                ctx.variables
                    .borrow_mut()
                    .insert(**var, Rc::new(RefCell::new(init_val)));
            }
            Statement::FnDecl {
                name,
                args,
                ret_type,
                stmts,
            } => {
                ctx.functions.insert(
                    name.to_string(),
                    FuncDef::Code(FuncCode::new(stmts.clone(), args.clone(), ret_type.clone())),
                );
            }
            Statement::Expression { ex, semicolon } => {
                let ex_res = eval(&ex, ctx)?;
                match ex_res {
                    RunResult::Yield(ex_res) => {
                        if *semicolon {
                            res = RunResult::Yield(ex_res);
                        } else {
                            res = RunResult::Yield(ex_res);
                        }
                    }
                    RunResult::Break => return Ok(ex_res),
                }
                // println!("Expression evaluates to: {:?}", res);
            }
            Statement::Loop(e) => loop {
                res = RunResult::Yield(unwrap_break!(run(e, ctx)?));
            },
            Statement::While(cond, e) => loop {
                match unwrap_deref(eval(cond, ctx)?)? {
                    RunResult::Yield(v) => {
                        if !truthy(&v) {
                            break;
                        }
                    }
                    RunResult::Break => break,
                }
                res = match unwrap_deref(run(e, ctx)?)? {
                    RunResult::Yield(v) => RunResult::Yield(v),
                    RunResult::Break => break,
                };
            },
            Statement::For {
                var,
                start,
                end,
                stmts,
                ..
            } => {
                let from_res = coerce_i64(&unwrap_break!(eval(start, ctx)?))? as i64;
                let to_res = coerce_i64(&unwrap_break!(eval(end, ctx)?))? as i64;
                for i in from_res..to_res {
                    ctx.variables
                        .borrow_mut()
                        .insert(var, Rc::new(RefCell::new(Value::I64(i))));
                    res = RunResult::Yield(unwrap_break!(run(stmts, ctx)?));
                }
            }
            Statement::Break => {
                return Ok(RunResult::Break);
            }
            Statement::Struct(str) => {
                ctx.typedefs.insert(str.name.to_string(), str.clone());
            }
            _ => {}
        }
    }
    Ok(res)
}

#[cfg(test)]
mod test;
