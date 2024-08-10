use std::{cell::Ref, collections::HashMap};

use crate::{
    coerce_type,
    interpreter::{coerce_i64, EvalResult},
    type_decl::{ArraySize, ArraySizeAxis},
    value::ArrayInt,
    ArgDecl, EvalError, FuncDef, TypeDecl, Value,
};

pub(crate) fn s_print(vals: &[Value]) -> EvalResult<Value> {
    println!("print:");
    for val in vals {
        // Put a space between tokens
        print!(" {val}");
    }
    print!("\n");
    Ok(Value::I32(0))
}

fn s_puts(vals: &[Value]) -> Result<Value, EvalError> {
    fn puts_inner<'a>(vals: &mut dyn Iterator<Item = &'a Value>) {
        for val in vals {
            match val {
                Value::F64(val) => print!("{}", val),
                Value::F32(val) => print!("{}", val),
                Value::I64(val) => print!("{}", val),
                Value::I32(val) => print!("{}", val),
                Value::Str(val) => print!("{}", val),
                Value::Array(val) => puts_inner(&mut val.borrow().values.iter()),
                Value::Ref(r) => {
                    let v: &Value = &r.borrow();
                    puts_inner(&mut std::iter::once(v))
                }
                Value::ArrayRef(r, idx) => {
                    if let Some(r) = r.borrow().values.get(*idx) {
                        puts_inner(&mut std::iter::once(r))
                    }
                }
                Value::Tuple(val) => puts_inner(&mut val.borrow().iter().map(|v| &v.value)),
            }
        }
    }
    puts_inner(&mut vals.iter());
    Ok(Value::I32(0))
}

pub(crate) fn s_type(vals: &[Value]) -> Result<Value, EvalError> {
    fn type_str(val: &Value) -> String {
        match val {
            Value::F64(_) => "f64".to_string(),
            Value::F32(_) => "f32".to_string(),
            Value::I64(_) => "i64".to_string(),
            Value::I32(_) => "i32".to_string(),
            Value::Str(_) => "str".to_string(),
            Value::Array(inner) => format!("[{}]", inner.borrow().type_decl),
            Value::Ref(r) => format!("ref[{}]", type_str(&r.borrow())),
            Value::ArrayRef(r, _) => format!("aref[{}]", r.borrow().type_decl),
            Value::Tuple(inner) => format!(
                "({})",
                &inner.borrow().iter().fold(String::new(), |acc, cur| {
                    if acc.is_empty() {
                        cur.decl.to_string()
                    } else {
                        acc + ", " + &cur.decl.to_string()
                    }
                })
            ),
        }
    }
    if let [val, ..] = vals {
        Ok(Value::Str(type_str(val)))
    } else {
        Ok(Value::I32(0))
    }
}

pub(crate) fn s_len(vals: &[Value]) -> Result<Value, EvalError> {
    if let [val, ..] = vals {
        Ok(Value::I64(val.array_len()? as i64))
    } else {
        Ok(Value::I32(0))
    }
}

pub(crate) fn s_push(vals: &[Value]) -> Result<Value, EvalError> {
    if let [arr, val, ..] = vals {
        let val = val.clone().deref()?;
        arr.array_push(val).map(|_| Value::I32(0))
    } else {
        Ok(Value::I32(0))
    }
}

fn s_shape(vals: &[Value]) -> Result<Value, EvalError> {
    let [arr, ..] = vals else {
        return Err(EvalError::RuntimeError(
            "transpose does not have enough arguments".to_string(),
        ));
    };
    let Value::Array(arr) = arr.clone().deref()? else {
        return Err(EvalError::RuntimeError(
            "transpose's argument (array) must be of type array".to_string(),
        ));
    };
    let arr = arr
        .try_borrow()
        .map_err(|e| EvalError::RuntimeError(e.to_string()))?;
    let shape = arr
        .shape
        .iter()
        .map(|v| Value::I64(*v as i64))
        .collect::<Vec<_>>();
    Ok(Value::Array(ArrayInt::new(
        TypeDecl::I64,
        vec![shape.len()],
        shape,
    )))
}

fn s_transpose(vals: &[Value]) -> Result<Value, EvalError> {
    let [arr, ..] = vals else {
        return Err(EvalError::RuntimeError(
            "transpose does not have enough arguments".to_string(),
        ));
    };
    let Value::Array(arr) = arr.clone().deref()? else {
        return Err(EvalError::RuntimeError(
            "transpose's argument (array) must be of type array".to_string(),
        ));
    };
    let arr = arr
        .try_borrow()
        .map_err(|e| EvalError::RuntimeError(e.to_string()))?;
    let cols = arr.shape.last().ok_or_else(|| {
        EvalError::RuntimeError("transpose's argument must have at least one dimension".to_string())
    })?;
    let mut new_shape = arr.shape.clone();
    new_shape.reverse();
    if new_shape.len() == 1 {
        new_shape = vec![new_shape[0], 1];
    }
    let new_cols = new_shape.last().ok_or_else(|| {
        EvalError::RuntimeError("transpose's argument must have at least one dimension".to_string())
    })?;
    let new_values = (0..arr.values.len())
        .map(|i| {
            let col = i % new_cols;
            let row = i / new_cols;
            arr.values[col * cols + row].clone()
        })
        .collect();
    Ok(Value::Array(ArrayInt::new(
        arr.type_decl.clone(),
        new_shape,
        new_values,
    )))
}

/// Reshape a given array with a new shape.
fn s_reshape(vals: &[Value]) -> EvalResult<Value> {
    let [arr, shape, ..] = vals else {
        return Err(EvalError::RuntimeError(
            "reshape does not have enough arguments".to_string(),
        ));
    };
    let shape = shape.clone().deref()?;
    let Value::Array(shape) = shape else {
        return Err(EvalError::RuntimeError(
            "reshape's second argument (shape) must be of type array".to_string(),
        ));
    };
    let shape = shape.borrow();
    let mut shape = shape
        .values
        .iter()
        .map(|val| coerce_i64(val).map(|val| val))
        .collect::<Result<Vec<_>, _>>()?;
    let Value::Array(arr) = arr.clone().deref()? else {
        return Err(EvalError::RuntimeError(
            "reshape's first argument (array) must be of type array".to_string(),
        ));
    };
    let arr = arr
        .try_borrow()
        .map_err(|e| EvalError::RuntimeError(e.to_string()))?;
    let arr_elems: usize = arr.shape.iter().copied().product();
    let shape_known_elems: usize =
        shape.iter().copied().filter(|v| 0 < *v).product::<i64>() as usize;
    let mut shape_unknown_elem = None;
    for v in shape.iter_mut().filter(|v| **v < 0) {
        if shape_unknown_elem.is_some() {
            return Err(EvalError::RuntimeError(
                "reshape can only specify one unknown dimension".to_string(),
            ));
        }
        if arr_elems % shape_known_elems != 0 {
            return Err(EvalError::RuntimeError(format!(
                "reshape cannot reshape array of size {} into shape {:?}",
                arr_elems, shape
            )));
        }
        *v = (arr_elems / shape_known_elems) as i64;
        shape_unknown_elem = Some(*v);
    }
    let shape_elems: usize = shape.iter().copied().product::<i64>() as usize;

    if arr_elems != shape_elems {
        return Err(EvalError::RuntimeError(format!(
            "reshape's array ({:?}) and new shape ({:?}) does not have the same number of elements",
            arr.shape, shape
        )));
    }
    let new_values = arr.values.clone();
    Ok(Value::Array(ArrayInt::new(
        arr.type_decl.clone(),
        shape.into_iter().map(|v| v as usize).collect(),
        new_values,
    )))
}

/// Stack 2 arrays vertically
fn s_vstack(vals: &[Value]) -> Result<Value, EvalError> {
    let [a, b, ..] = vals else {
        return Err(EvalError::RuntimeError(
            "vstack does not have enough arguments".to_string(),
        ));
    };

    fn borrow<'a>(a: &'a Value, name: &str) -> EvalResult<Ref<'a, ArrayInt>> {
        let Value::Array(a) = a else {
            return Err(EvalError::RuntimeError(format!(
                "vstack's argument {} must be of type array",
                name
            )));
        };
        Ok(a.borrow())
    }

    fn read_raw_array<'a>(
        a: &'a Ref<'a, ArrayInt>,
        name: &str,
    ) -> EvalResult<(TypeDecl, &'a Vec<Value>, Vec<usize>)> {
        let ty = a.type_decl.clone();
        let mut shape = a.shape.clone();
        if shape.len() == 0 {
            return Err(EvalError::RuntimeError(format!(
                "vstack's argument {} has empty shape",
                name
            )));
        }
        if shape.len() == 1 {
            shape = vec![1, shape[0]];
        }
        let values = &a.values;
        Ok((ty, values, shape))
    }

    let a = a.clone().deref()?;
    let a = borrow(&a, "a")?;
    let (a_type, a, a_shape) = read_raw_array(&a, "a")?;
    let b = b.clone().deref()?;
    let b = borrow(&b, "b")?;
    let (b_type, b, b_shape) = read_raw_array(&b, "b")?;

    if a_shape[1..] != b_shape[1..] {
        return Err(EvalError::RuntimeError(format!(
            "vstack's arguments have incompatible shape: {:?} and {:?}",
            a_shape, b_shape
        )));
    }

    if a_type != b_type {
        return Err(EvalError::RuntimeError(format!(
            "vstack's arguments have incompatible type: {:?} and {:?}",
            a_type, b_type
        )));
    }

    let mut values = a.clone();
    values.extend_from_slice(&b);

    let mut shape = vec![a_shape[0] + b_shape[0]];
    shape.extend_from_slice(&a_shape[1..]);

    Ok(Value::Array(ArrayInt::new(a_type, shape, values)))
}

pub(crate) fn s_hex_string(vals: &[Value]) -> Result<Value, EvalError> {
    if let [val, ..] = vals {
        match coerce_type(val, &TypeDecl::I64)? {
            Value::I64(i) => Ok(Value::Str(format!("{:02x}", i))),
            _ => Err(EvalError::Other(
                "hex_string() could not convert argument to i64".to_string(),
            )),
        }
    } else {
        Ok(Value::Str("".to_string()))
    }
}

pub(crate) fn std_functions<'src, 'native>() -> HashMap<String, FuncDef<'src, 'native>> {
    let mut ret = HashMap::new();
    std_functions_gen(&mut |name, code, args, ret_type| {
        ret.insert(name.to_string(), FuncDef::new_native(code, args, ret_type));
    });
    ret
}

pub(crate) fn std_functions_gen<'src, 'native>(
    set_fn: &mut impl FnMut(
        &str,
        &'native dyn Fn(&[Value]) -> Result<Value, EvalError>,
        Vec<ArgDecl<'native>>,
        Option<TypeDecl>,
    ),
) {
    // let mut functions = HashMap::new();
    set_fn("print", &s_print, vec![], None);
    set_fn(
        "puts",
        &s_puts,
        vec![ArgDecl::new("val", TypeDecl::Any)],
        None,
    );
    set_fn(
        "type",
        &s_type,
        vec![ArgDecl::new("value", TypeDecl::Any)],
        Some(TypeDecl::Str),
    );
    set_fn(
        "len",
        &s_len,
        vec![ArgDecl::new(
            "array",
            TypeDecl::Array(Box::new(TypeDecl::Any), ArraySize::default()),
        )],
        Some(TypeDecl::I64),
    );
    set_fn(
        "push",
        &s_push,
        vec![
            ArgDecl::new(
                "array",
                TypeDecl::Array(
                    Box::new(TypeDecl::Any),
                    ArraySize(vec![ArraySizeAxis::Range(0..usize::MAX)]),
                ),
            ),
            ArgDecl::new("value", TypeDecl::Any),
        ],
        None,
    );
    set_fn(
        "shape",
        &s_shape,
        vec![ArgDecl::new(
            "array",
            TypeDecl::Array(Box::new(TypeDecl::Any), ArraySize::all_dyn()),
        )],
        Some(TypeDecl::Array(
            Box::new(TypeDecl::I64),
            ArraySize::all_dyn(),
        )),
    );
    set_fn(
        "transpose",
        &s_transpose,
        vec![ArgDecl::new(
            "array",
            TypeDecl::Array(Box::new(TypeDecl::Any), ArraySize::all_dyn()),
        )],
        None,
    );
    set_fn(
        "reshape",
        &s_reshape,
        vec![
            ArgDecl::new(
                "array",
                TypeDecl::Array(Box::new(TypeDecl::Any), ArraySize::all_dyn()),
            ),
            ArgDecl::new(
                "shape",
                TypeDecl::Array(Box::new(TypeDecl::Integer), ArraySize::all_dyn()),
            ),
        ],
        None,
    );
    set_fn(
        "vstack",
        &s_vstack,
        vec![
            ArgDecl::new(
                "a",
                TypeDecl::Array(Box::new(TypeDecl::Any), ArraySize::all_dyn()),
            ),
            ArgDecl::new(
                "b",
                TypeDecl::Array(Box::new(TypeDecl::Any), ArraySize::all_dyn()),
            ),
        ],
        None,
    );
    set_fn(
        "hex_string",
        &s_hex_string,
        vec![ArgDecl::new("value", TypeDecl::I64)],
        Some(TypeDecl::Str),
    );
}
