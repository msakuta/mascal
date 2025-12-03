//! LValue-related types and functions.
//!
//! LValue is a concept in C language family. It's one of 2 kinds of value classification and the other kind is RValue.

use std::{cell::RefCell, rc::Rc};

use super::{eval, EvalContext, EvalError, EvalResult, Expression, RunResult};
use crate::{
    value::{ArrayInt, StructInt},
    TypeDecl, Value,
};

/// An LValue is a description of a target memory to be written to.
pub(super) enum LValue {
    /// A variable identified by a name
    Variable(String),
    /// Reference to a refcounted variable, e.g. an array element.
    ArrayRef(Rc<RefCell<ArrayInt>>, usize),
    /// Reference to a field of a refcounted struct.
    StructRef(Rc<RefCell<StructInt>>, usize),
}

impl Value {
    pub(super) fn array_get_lvalue(&self, idx: u64) -> Result<LValue, EvalError> {
        Ok(match self {
            Value::Array(array) => {
                let array_int = array.borrow();
                if (idx as usize) < array_int.values.len() {
                    LValue::ArrayRef(array.clone(), idx as usize)
                } else {
                    return Err(EvalError::ArrayOutOfBounds(
                        idx as usize,
                        array_int.values.len(),
                    ));
                }
            }
            _ => return Err(EvalError::IndexNonArray),
        })
    }

    pub(super) fn struct_get_lvalue(&self, idx: usize) -> Result<LValue, EvalError> {
        Ok(match self {
            Value::Struct(str) => {
                let str_int = str.borrow();
                if (idx as usize) < str_int.fields.len() {
                    LValue::StructRef(str.clone(), idx as usize)
                } else {
                    return Err(EvalError::ArrayOutOfBounds(
                        idx as usize,
                        str_int.fields.len(),
                    ));
                }
            }
            _ => return Err(EvalError::IndexNonArray),
        })
    }
}

pub(super) fn eval_lvalue<'src, 'native, 'ctx>(
    expr: &Expression<'src>,
    ctx: &'ctx mut EvalContext<'src, 'native, '_>,
) -> EvalResult<LValue>
where
    'native: 'src,
{
    use super::ExprEnum::*;
    match &expr.expr {
        NumLiteral(_, _) | StrLiteral(_) | ArrLiteral(_) => {
            Err(EvalError::AssignToLiteral(expr.span.to_string()))
        }
        Variable(name) => Ok(LValue::Variable(name.to_string())),
        ArrIndex(ex, idx) => {
            let idx = match eval(&idx[0], ctx)? {
                RunResult::Yield(Value::I32(val)) => val as u64,
                RunResult::Yield(Value::I64(val)) => val as u64,
                RunResult::Yield(_) => return Err(EvalError::IndexNonNum),
                RunResult::Break => return Err(EvalError::BreakInFnArg),
            };
            let arr = eval_lvalue(ex, ctx)?;
            Ok(match arr {
                LValue::Variable(name) => ctx
                    .variables
                    .borrow_mut()
                    .get(name.as_str())
                    .ok_or_else(|| EvalError::VarNotFound(name))?
                    .borrow_mut()
                    .array_get_lvalue(idx)?,
                LValue::ArrayRef(value, subidx) => {
                    let elem = RefCell::borrow(&value).get(subidx)?;
                    elem.array_get_lvalue(idx)?
                }
                LValue::StructRef(value, subidx) => {
                    let field = RefCell::borrow(&value).get(subidx)?;
                    field.array_get_lvalue(idx)?
                }
            })
        }
        FieldAccess {
            prefix: ex,
            postfix: field_name,
            ..
        } => {
            let val = eval_lvalue(ex, ctx)?;

            let resolve_struct = |var: &Value| {
                let Value::Struct(s) = var else {
                    return Err(EvalError::ExpectStruct(TypeDecl::from_value(var)));
                };
                let st_borrow = s.borrow();
                let Some(st_ty) = ctx.typedefs.get(&st_borrow.name) else {
                    return Err(EvalError::NoStructFound(st_borrow.name.clone()));
                };
                let (idx, _) = st_ty
                    .fields
                    .iter()
                    .enumerate()
                    .find(|(_, field)| *field.name == **field_name)
                    .ok_or_else(|| EvalError::NoFieldFound(field_name.to_string()))?;
                var.struct_get_lvalue(idx)
            };

            match val {
                LValue::Variable(name) => {
                    let variables = ctx.variables.borrow();
                    let var = variables
                        .get(name.as_str())
                        .ok_or_else(|| EvalError::VarNotFound(name))?
                        .borrow();
                    resolve_struct(&*var)
                }
                LValue::ArrayRef(arr, subidx) => {
                    let var = RefCell::borrow(&arr).get(subidx)?;
                    resolve_struct(&var)
                }
                LValue::StructRef(st, subidx) => {
                    let var = RefCell::borrow(&st).get(subidx)?;
                    resolve_struct(&var)
                }
            }
        }
        _ => Err(EvalError::NonLValue(expr.span.to_string())),
    }
}
