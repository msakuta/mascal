use crate::{
    parser::*,
    std_fns::std_functions,
    type_decl::ArraySizeAxis,
    value::{ArrayInt, TupleEntry},
    TypeDecl, Value,
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, PartialEq, Clone)]
pub enum RunResult {
    Yield(Value),
    Break,
}

pub type EvalResult<T> = Result<T, EvalError>;

/// Error type for the AST intepreter and bytecode interpreter.
/// Note that it is shared among 2 kinds of interpreters, so some of them only happen in either kind.
/// Also note that it is supposed to be displayed with Display or "{}" format, not with Debug or "{:?}".
///
/// It owns the value so it is not bounded by a lifetime.
/// The information about the error shold be converted to a string (by `format!("{:?}")`) before wrapping it
/// into `EvalError`.
#[non_exhaustive]
#[derive(Debug, PartialEq, Eq)]
pub enum EvalError {
    Other(String),
    CoerceError(String, String),
    OpError(String, String),
    CmpError(String, String),
    FloatOpError(String, String),
    StrOpError(String, String),
    DisallowedBreak,
    VarNotFound(String),
    FnNotFound(String),
    ArrayOutOfBounds(usize, usize),
    NonRectangularArray,
    TupleOutOfBounds(usize, usize),
    IndexNonArray,
    NeedRef(String),
    NoMatchingArg(String, String),
    MissingArg(String),
    BreakInToplevel,
    BreakInFnArg,
    NonIntegerIndex,
    NonIntegerBitwise(String),
    NoMainFound,
    NonNameFnRef(String),
    CallStackUndeflow,
    IncompatibleArrayLength(usize, usize),
    /// Some other error that happened in a library code.
    RuntimeError(String),
}

impl std::error::Error for EvalError {}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Other(e) => write!(f, "Unknown error: {e}"),
            Self::CoerceError(from, to) => {
                write!(f, "Coercing from {from:?} to {to:?} is disallowed")
            }
            Self::OpError(lhs, rhs) => {
                write!(f, "Unsupported operation between {lhs:?} and {rhs:?}")
            }
            Self::CmpError(lhs, rhs) => {
                write!(f, "Unsupported comparison between {lhs:?} and {rhs:?}",)
            }
            Self::FloatOpError(lhs, rhs) => {
                write!(f, "Unsupported float operation between {lhs:?} and {rhs:?}")
            }
            Self::StrOpError(lhs, rhs) => write!(
                f,
                "Unsupported string operation between {lhs:?} and {rhs:?}"
            ),
            Self::DisallowedBreak => write!(f, "Break in array literal not supported"),
            Self::VarNotFound(name) => write!(f, "Variable {name} not found in scope"),
            Self::FnNotFound(name) => write!(f, "Function {name} not found in scope"),
            Self::ArrayOutOfBounds(idx, len) => write!(
                f,
                "ArrayRef index out of range: {idx} is larger than array length {len}"
            ),
            Self::NonRectangularArray => {
                write!(f, "The array has different number of columns among rows")
            }
            Self::TupleOutOfBounds(idx, len) => write!(
                f,
                "Tuple index out of range: {idx} is larger than tuple length {len}"
            ),
            Self::IndexNonArray => write!(f, "array index must be called for an array"),
            Self::NeedRef(name) => write!(
                f,
                "We need variable reference on lhs to assign. Actually we got {name:?}"
            ),
            Self::NoMatchingArg(arg, fun) => write!(
                f,
                "No matching named parameter \"{arg}\" is found in function \"{fun}\""
            ),
            Self::MissingArg(arg) => write!(f, "No argument is given to \"{arg}\""),
            Self::BreakInToplevel => write!(f, "break in function toplevel"),
            Self::BreakInFnArg => write!(f, "Break in function argument is not supported yet!"),
            Self::NonIntegerIndex => write!(f, "Subscript type should be integer types"),
            Self::NonIntegerBitwise(val) => {
                write!(f, "Bitwise operation is not supported for {val}")
            }
            Self::NoMainFound => write!(f, "No main function found"),
            Self::NonNameFnRef(val) => write!(
                f,
                "Function can be only specified by a name (yet), but got {val}"
            ),
            Self::CallStackUndeflow => write!(f, "Call stack underflow!"),
            Self::IncompatibleArrayLength(dst, src) => write!(
                f,
                "Array length is incompatible; tried to assign {src} to {dst}"
            ),
            Self::RuntimeError(e) => write!(f, "Runtime error: {e}"),
        }
    }
}

impl From<String> for EvalError {
    fn from(value: String) -> Self {
        Self::Other(value)
    }
}

/// An extension trait for `Vec` to write a shorthand for
/// `values.get(idx).ok_or_else(|| EvalError::ArrayOutOfBounds(idx, values.len()))`, because
/// it's too long and shows up too often behind Rc.
pub(crate) trait EGetExt<T> {
    fn eget(&self, idx: usize) -> EvalResult<&T>;
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
        RunResult::Yield(Value::Ref(vref)) => {
            let r = vref.borrow();
            return Ok(RunResult::Yield(r.clone()));
        }
        RunResult::Yield(Value::ArrayRef(a, idx)) => {
            let a = a.borrow();
            let value = a.values.eget(*idx)?;
            return Ok(RunResult::Yield(value.clone()));
        }
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
        // "Deref" the references before binary
        (Value::Ref(lhs), ref rhs) => binary_op_str(&lhs.borrow(), rhs, d, i, s)?,
        (ref lhs, Value::Ref(rhs)) => binary_op_str(lhs, &rhs.borrow(), d, i, s)?,
        (Value::ArrayRef(lhs, idx), ref rhs) => {
            binary_op_str(lhs.borrow().values.eget(*idx)?, rhs, d, i, s)?
        }
        (ref lhs, Value::ArrayRef(rhs, idx)) => {
            binary_op_str(lhs, rhs.borrow().values.eget(*idx)?, d, i, s)?
        }
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
        Value::Ref(r) => truthy(&r.borrow()),
        _ => false,
    }
}

pub(crate) fn coerce_f64(a: &Value) -> EvalResult<f64> {
    Ok(match a {
        Value::F64(v) => *v as f64,
        Value::F32(v) => *v as f64,
        Value::I64(v) => *v as f64,
        Value::I32(v) => *v as f64,
        Value::Ref(r) => coerce_f64(&r.borrow())?,
        _ => 0.,
    })
}

pub(crate) fn coerce_i64(a: &Value) -> EvalResult<i64> {
    Ok(match a {
        Value::F64(v) => *v as i64,
        Value::F32(v) => *v as i64,
        Value::I64(v) => *v as i64,
        Value::I32(v) => *v as i64,
        Value::Ref(r) => coerce_i64(&r.borrow())?,
        _ => 0,
    })
}

fn coerce_str(a: &Value) -> EvalResult<String> {
    Ok(match a {
        Value::F64(v) => v.to_string(),
        Value::F32(v) => v.to_string(),
        Value::I64(v) => v.to_string(),
        Value::I32(v) => v.to_string(),
        Value::Str(v) => v.clone(),
        _ => {
            return Err(EvalError::CoerceError(
                TypeDecl::from_value(a).to_string(),
                "str".to_string(),
            ))
        }
    })
}

fn _coerce_var(value: &Value, target: &Value) -> Result<Value, EvalError> {
    Ok(match target {
        Value::F64(_) => Value::F64(coerce_f64(value)?),
        Value::F32(_) => Value::F32(coerce_f64(value)? as f32),
        Value::I64(_) => Value::I64(coerce_i64(value)?),
        Value::I32(_) => Value::I32(coerce_i64(value)? as i32),
        Value::Str(_) => Value::Str(coerce_str(value)?),
        Value::Array(array) => {
            let ArrayInt {
                type_decl: inner_type,
                shape,
                values: inner,
            } = &array.borrow() as &ArrayInt;
            if inner.len() == 0 {
                if let Value::Array(array) = value {
                    if array.borrow().values.len() == 0 {
                        return Ok(value.clone());
                    }
                }
                return Err(EvalError::CoerceError(
                    "array".to_string(),
                    "empty array".to_string(),
                ));
            } else {
                if let Value::Array(array) = value {
                    Value::Array(ArrayInt::new(
                        inner_type.clone(),
                        shape.clone(),
                        array
                            .borrow()
                            .values
                            .iter()
                            .map(|val| -> EvalResult<_> { Ok(coerce_type(val, inner_type)?) })
                            .collect::<Result<_, _>>()?,
                    ))
                } else {
                    return Err(EvalError::CoerceError(
                        "scalar".to_string(),
                        "array".to_string(),
                    ));
                }
            }
        }
        // We usually don't care about coercion
        Value::Ref(_) => value.clone(),
        Value::ArrayRef(_, _) => value.clone(),
        Value::Tuple(tuple) => {
            let target_elems = tuple.borrow();
            if target_elems.len() == 0 {
                if let Value::Tuple(value_elems) = value {
                    if value_elems.borrow().len() == 0 {
                        return Ok(value.clone());
                    }
                }
                return Err(EvalError::CoerceError(
                    "array".to_string(),
                    "empty array".to_string(),
                ));
            } else {
                if let Value::Tuple(value_elems) = value {
                    Value::Tuple(Rc::new(RefCell::new(
                        value_elems
                            .borrow()
                            .iter()
                            .zip(target_elems.iter())
                            .map(|(val, tgt)| -> EvalResult<_> {
                                Ok(TupleEntry {
                                    decl: tgt.decl.clone(),
                                    value: coerce_type(&val.value, &tgt.decl)?,
                                })
                            })
                            .collect::<Result<_, _>>()?,
                    )))
                } else {
                    return Err(EvalError::CoerceError(
                        "scalar".to_string(),
                        "array".to_string(),
                    ));
                }
            }
        }
    })
}

pub fn coerce_type(value: &Value, target: &TypeDecl) -> Result<Value, EvalError> {
    Ok(match target {
        TypeDecl::Any => value.clone(),
        TypeDecl::F64 => Value::F64(coerce_f64(value)?),
        TypeDecl::F32 => Value::F32(coerce_f64(value)? as f32),
        TypeDecl::I64 => Value::I64(coerce_i64(value)?),
        TypeDecl::I32 => Value::I32(coerce_i64(value)? as i32),
        TypeDecl::Str => Value::Str(coerce_str(value)?),
        TypeDecl::Array(inner, len) => {
            if let Value::Array(array) = value {
                let array = array.borrow();
                for (v_axis, t_axis) in array.shape.iter().zip(len.0.iter()) {
                    match t_axis {
                        ArraySizeAxis::Fixed(len) => {
                            if *len != *v_axis {
                                return Err(EvalError::IncompatibleArrayLength(
                                    *len,
                                    array.values.len(),
                                ));
                            }
                        }
                        ArraySizeAxis::Range(range) => {
                            if *v_axis < range.start {
                                return Err(EvalError::IncompatibleArrayLength(
                                    range.start,
                                    *v_axis,
                                ));
                            }
                            if range.end < *v_axis {
                                return Err(EvalError::IncompatibleArrayLength(range.end, *v_axis));
                            }
                        }
                        _ => {}
                    }
                }
                Value::Array(ArrayInt::new(
                    (**inner).clone(),
                    array.shape.clone(),
                    array
                        .values
                        .iter()
                        .map(|value_elem| -> Result<_, EvalError> {
                            Ok(coerce_type(value_elem, inner)?)
                        })
                        .collect::<Result<Vec<_>, _>>()?,
                ))
            } else {
                return Err(EvalError::CoerceError(
                    value.to_string(),
                    "array".to_string(),
                ));
            }
        }
        TypeDecl::Float => Value::F64(coerce_f64(value)?),
        TypeDecl::Integer => Value::I64(coerce_i64(value)?),
        TypeDecl::Tuple(inner) => {
            if let Value::Tuple(value) = value {
                Value::Tuple(Rc::new(RefCell::new(
                    value
                        .borrow()
                        .iter()
                        .zip(inner.iter())
                        .map(|(value_elem, inner_elem)| -> Result<_, EvalError> {
                            Ok(TupleEntry {
                                decl: inner_elem.clone(),
                                value: coerce_type(&value_elem.value, inner_elem)?,
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?,
                )))
            } else {
                return Err(EvalError::CoerceError(
                    value.to_string(),
                    "array".to_string(),
                ));
            }
        }
    })
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
        TypeDecl::Any,
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
        ExprEnum::NumLiteral(val) => RunResult::Yield(val.clone()),
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
        ExprEnum::Variable(str) => RunResult::Yield(Value::Ref(
            ctx.get_var_rc(str)
                .ok_or_else(|| EvalError::VarNotFound(str.to_string()))?,
        )),
        ExprEnum::Cast(ex, decl) => {
            RunResult::Yield(coerce_type(&unwrap_run!(eval(ex, ctx)?), decl)?)
        }
        ExprEnum::VarAssign(lhs, rhs) => {
            let lhs_result = eval(lhs, ctx)?;
            let result = match lhs_result {
                RunResult::Yield(Value::Ref(rc)) => {
                    let rhs_value = unwrap_run!(eval(rhs, ctx)?);
                    *rc.borrow_mut() = rhs_value.clone();
                    rhs_value
                }
                RunResult::Yield(Value::ArrayRef(rc, idx)) => {
                    let mut array_int = rc.borrow_mut();
                    let mref = array_int.values.eget_mut(idx)?;
                    let rhs_value = unwrap_run!(eval(rhs, ctx)?);
                    *mref = rhs_value.clone();
                    rhs_value
                }
                _ => {
                    return Err(EvalError::NeedRef(format!("{lhs_result:?}")));
                }
            };
            RunResult::Yield(result)
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
                        .find(|f| f.1.name == *name)
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
                                k.name,
                                Rc::new(RefCell::new(coerce_type(&unwrap_run!(v.clone()), &k.ty)?)),
                            );
                        } else {
                            return Err(EvalError::MissingArg(k.name.to_string()));
                        }
                    }
                    let run_result = run(&func.stmts, &mut subctx)?;
                    match unwrap_deref(run_result)? {
                        RunResult::Yield(v) => match &func.ret_type {
                            Some(ty) => RunResult::Yield(coerce_type(&v, ty)?),
                            None => RunResult::Yield(v),
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
            RunResult::Yield(result.array_get_ref(arg0)?)
        }
        ExprEnum::TupleIndex(ex, index) => {
            let result = unwrap_run!(eval(ex, ctx)?);
            RunResult::Yield(result.tuple_get(*index as u64)?)
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
        ExprEnum::LT(lhs, rhs) => RunResult::Yield(binary_op(
            &unwrap_run!(eval(lhs, ctx)?),
            &unwrap_run!(eval(rhs, ctx)?),
            |lhs, rhs| if lhs < rhs { 1. } else { 0. },
            |lhs, rhs| if lhs < rhs { 1 } else { 0 },
        )?),
        ExprEnum::GT(lhs, rhs) => RunResult::Yield(binary_op(
            &unwrap_run!(eval(lhs, ctx)?),
            &unwrap_run!(eval(rhs, ctx)?),
            |lhs, rhs| if lhs > rhs { 1. } else { 0. },
            |lhs, rhs| if lhs > rhs { 1 } else { 0 },
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
            let res = run(stmts, &mut subctx)?;
            if let RunResult::Yield(Value::Ref(res)) = res {
                // "Dereference" if the result was a reference
                RunResult::Yield(std::mem::take(&mut res.borrow_mut()))
            } else {
                res
            }
        }
    })
}

#[derive(Clone)]
pub struct FuncCode<'src> {
    args: Vec<ArgDecl<'src>>,
    pub(crate) ret_type: Option<TypeDecl>,
    /// Owning a clone of AST of statements is not quite efficient, but we could not get
    /// around the borrow checker.
    stmts: Rc<Vec<Statement<'src>>>,
}

impl<'src> FuncCode<'src> {
    pub(crate) fn new(
        stmts: Rc<Vec<Statement<'src>>>,
        args: Vec<ArgDecl<'src>>,
        ret_type: Option<TypeDecl>,
    ) -> Self {
        Self {
            args,
            ret_type,
            stmts,
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
    super_context: Option<&'ctx EvalContext<'src, 'native, 'ctx>>,
}

impl<'src, 'ast, 'native, 'ctx> EvalContext<'src, 'native, 'ctx> {
    pub fn new() -> Self {
        Self {
            variables: RefCell::new(HashMap::new()),
            functions: std_functions(),
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
            super_context: Some(super_ctx),
        }
    }

    fn _get_var(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.variables.borrow().get(name) {
            Some(val.borrow().clone())
        } else if let Some(super_ctx) = self.super_context {
            super_ctx._get_var(name)
        } else {
            None
        }
    }

    fn get_var_rc(&self, name: &str) -> Option<Rc<RefCell<Value>>> {
        if let Some(val) = self.variables.borrow().get(name) {
            Some(val.clone())
        } else if let Some(super_ctx) = self.super_context {
            super_ctx.get_var_rc(name)
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
}

macro_rules! unwrap_break {
    ($e:expr) => {
        match unwrap_deref($e)? {
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
            Statement::VarDecl(var, type_, initializer) => {
                let init_val = if let Some(init_expr) = initializer {
                    unwrap_break!(eval(init_expr, ctx)?)
                } else {
                    Value::I32(0)
                };
                let init_val = coerce_type(&init_val, type_)?;
                ctx.variables
                    .borrow_mut()
                    .insert(*var, Rc::new(RefCell::new(init_val)));
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
            Statement::Expression(e) => {
                res = eval(&e, ctx)?;
                if let RunResult::Break = res {
                    return Ok(res);
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
            Statement::For(iter, from, to, e) => {
                let from_res = coerce_i64(&unwrap_break!(eval(from, ctx)?))? as i64;
                let to_res = coerce_i64(&unwrap_break!(eval(to, ctx)?))? as i64;
                for i in from_res..to_res {
                    ctx.variables
                        .borrow_mut()
                        .insert(iter, Rc::new(RefCell::new(Value::I64(i))));
                    res = RunResult::Yield(unwrap_break!(run(e, ctx)?));
                }
            }
            Statement::Break => {
                return Ok(RunResult::Break);
            }
            _ => {}
        }
    }
    Ok(res)
}

#[cfg(test)]
mod test;
