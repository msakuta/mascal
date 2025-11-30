use std::{cell::RefCell, rc::Rc};

use crate::{
    interpreter::{EvalResult, TypeMap},
    type_decl::ArraySize,
    value::{ArrayInt, TupleEntry},
    EvalError, TypeDecl, Value,
};

pub(crate) fn coerce_f64(a: &Value) -> EvalResult<f64> {
    Ok(match a {
        Value::F64(v) => *v as f64,
        Value::F32(v) => *v as f64,
        Value::I64(v) => *v as f64,
        Value::I32(v) => *v as f64,
        _ => 0.,
    })
}

pub(crate) fn coerce_f32(a: &Value) -> EvalResult<f32> {
    Ok(match a {
        Value::F64(v) => *v as f32,
        Value::F32(v) => *v as f32,
        Value::I64(v) => *v as f32,
        Value::I32(v) => *v as f32,
        _ => 0.,
    })
}

pub(crate) fn coerce_i64(a: &Value) -> EvalResult<i64> {
    Ok(match a {
        Value::F64(v) => *v as i64,
        Value::F32(v) => *v as i64,
        Value::I64(v) => *v as i64,
        Value::I32(v) => *v as i64,
        _ => 0,
    })
}

pub(crate) fn coerce_i32(a: &Value) -> EvalResult<i32> {
    Ok(match a {
        Value::F64(v) => *v as i32,
        Value::F32(v) => *v as i32,
        Value::I64(v) => *v as i32,
        Value::I32(v) => *v as i32,
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

fn _coerce_var(value: &Value, target: &Value, typedefs: &TypeMap) -> Result<Value, EvalError> {
    Ok(match target {
        Value::F64(_) => Value::F64(coerce_f64(value)?),
        Value::F32(_) => Value::F32(coerce_f64(value)? as f32),
        Value::I64(_) => Value::I64(coerce_i64(value)?),
        Value::I32(_) => Value::I32(coerce_i64(value)? as i32),
        Value::Str(_) => Value::Str(coerce_str(value)?),
        Value::Array(array) => {
            let ArrayInt {
                type_decl: inner_type,
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
                        array
                            .borrow()
                            .values
                            .iter()
                            .map(|val| -> EvalResult<_> {
                                Ok(coerce_type(val, inner_type, typedefs)?)
                            })
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
                                    value: coerce_type(&val.value, &tgt.decl, typedefs)?,
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

pub fn coerce_type(
    value: &Value,
    target: &TypeDecl,
    typedefs: &TypeMap,
) -> Result<Value, EvalError> {
    Ok(match target {
        TypeDecl::Any => value.clone(),
        TypeDecl::F64 => Value::F64(coerce_f64(value)?),
        TypeDecl::F32 => Value::F32(coerce_f64(value)? as f32),
        TypeDecl::I64 => Value::I64(coerce_i64(value)?),
        TypeDecl::I32 => Value::I32(coerce_i64(value)? as i32),
        TypeDecl::Str => Value::Str(coerce_str(value)?),
        TypeDecl::Array(_, len) => {
            if let Value::Array(array) = value {
                let array = array.borrow();
                if let ArraySize::Fixed(len) = len {
                    if *len != array.values.len() {
                        return Err(EvalError::IncompatibleArrayLength(*len, array.values.len()));
                    }
                }
                // Type coercion should not alter the referenced value, i.e. array elements
                return Ok(value.clone());
            } else {
                return Err(EvalError::CoerceError(
                    value.to_string(),
                    "array".to_string(),
                ));
            }
        }
        TypeDecl::Tuple(_) => {
            if let Value::Tuple(_) = value {
                return Ok(value.clone());
            } else {
                return Err(EvalError::CoerceError(
                    value.to_string(),
                    "tuple".to_string(),
                ));
            }
        }
        TypeDecl::TypeName(name) => {
            let _struct_decl = typedefs
                .get(name)
                .ok_or_else(|| EvalError::NoStructFound(name.clone()))?;
            if let Value::Tuple(_) = value {
                return Ok(value.clone());
            } else {
                return Err(EvalError::CoerceError(
                    value.to_string(),
                    format!("typename {name}"),
                ));
            }
        }
    })
}
