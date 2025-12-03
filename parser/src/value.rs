use std::{
    cell::RefCell,
    io::{Read, Write},
    rc::Rc,
};

use crate::{
    interpreter::{EGetExt, EvalResult},
    type_decl::{ArraySize, TypeDecl},
    type_tags::*,
    EvalError, ReadError,
};

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayInt {
    pub(crate) type_decl: TypeDecl,
    pub(crate) values: Vec<Value>,
}

impl ArrayInt {
    pub(crate) fn new(type_decl: TypeDecl, values: Vec<Value>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self { type_decl, values }))
    }

    pub fn values(&self) -> &[Value] {
        &self.values
    }

    pub fn get(&self, idx: usize) -> EvalResult<Value> {
        self.values
            .get(idx)
            .ok_or_else(|| EvalError::ArrayOutOfBounds(self.values.len(), idx))
            .cloned()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    F64(f64),
    F32(f32),
    I64(i64),
    I32(i32),
    Str(String),
    Array(Rc<RefCell<ArrayInt>>),
    Tuple(Rc<RefCell<TupleInt>>),
    Struct(Rc<RefCell<StructInt>>),
}

impl Default for Value {
    fn default() -> Self {
        Self::I64(0)
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::F64(v) => write!(f, "{v}"),
            Self::F32(v) => write!(f, "{v}"),
            Self::I64(v) => write!(f, "{v}"),
            Self::I32(v) => write!(f, "{v}"),
            Self::Str(v) => write!(f, "{v}"),
            Self::Array(v) => write!(
                f,
                "[{}]",
                &v.borrow().values.iter().fold("".to_string(), |acc, cur| {
                    if acc.is_empty() {
                        cur.to_string()
                    } else {
                        acc + ", " + &cur.to_string()
                    }
                })
            ),
            Self::Tuple(v) => write!(
                f,
                "({})",
                &v.borrow().iter().fold("".to_string(), |acc, cur| {
                    if acc.is_empty() {
                        cur.value.to_string()
                    } else {
                        acc + ", " + &cur.value.to_string()
                    }
                })
            ),
            Self::Struct(v) => {
                let borrow = v.borrow();
                write!(
                    f,
                    "{}({})",
                    borrow.name,
                    &borrow.fields.iter().fold("".to_string(), |acc, cur| {
                        if acc.is_empty() {
                            cur.to_string()
                        } else {
                            acc + ", " + &cur.to_string()
                        }
                    })
                )
            }
        }
    }
}

impl Value {
    pub(crate) fn serialize(&self, writer: &mut impl Write) -> std::io::Result<()> {
        macro_rules! serialize_with_tag {
            ($tag:ident, $val:expr) => {{
                writer.write_all(&$tag.to_le_bytes())?;
                writer.write_all(&$val.to_le_bytes())?;
                Ok(())
            }};
        }

        match self {
            Self::F64(val) => serialize_with_tag!(F64_TAG, val),
            Self::F32(val) => serialize_with_tag!(F32_TAG, val),
            Self::I64(val) => serialize_with_tag!(I64_TAG, val),
            Self::I32(val) => serialize_with_tag!(I32_TAG, val),
            Self::Str(val) => {
                writer.write_all(&STR_TAG.to_le_bytes())?;
                writer.write_all(&(val.len() as u32).to_le_bytes())?;
                writer.write_all(val.as_bytes())?;
                Ok(())
            }
            Self::Array(rc) => {
                let ArrayInt {
                    type_decl: decl,
                    values,
                } = &rc.borrow() as &ArrayInt;
                writer.write_all(&ARRAY_TAG.to_le_bytes())?;
                writer.write_all(&values.len().to_le_bytes())?;
                decl.serialize(writer)?;
                for value in values {
                    value.serialize(writer)?;
                }
                Ok(())
            }
            Self::Tuple(rc) => {
                let values = rc.borrow();
                writer.write_all(&TUPLE_TAG.to_le_bytes())?;
                writer.write_all(&values.len().to_le_bytes())?;
                for entry in values.iter() {
                    entry.decl.serialize(writer)?;
                    entry.value.serialize(writer)?;
                }
                Ok(())
            }
            Self::Struct(rc) => {
                let values = rc.borrow();
                writer.write_all(&STRUCT_TAG.to_le_bytes())?;
                writer.write_all(&(values.name.len() as u32).to_le_bytes())?;
                writer.write_all(values.name.as_bytes())?;
                writer.write_all(&values.fields.len().to_le_bytes())?;
                for value in values.fields.iter() {
                    value.serialize(writer)?;
                }
                Ok(())
            }
        }
    }

    pub(crate) fn deserialize(reader: &mut impl Read) -> Result<Self, ReadError> {
        let mut tag = [0u8; 1];
        reader.read_exact(&mut tag)?;

        macro_rules! parse {
            ($typ:ty) => {{
                let mut buf = [0u8; std::mem::size_of::<$typ>()];
                reader.read_exact(&mut buf)?;
                <$typ>::from_le_bytes(buf)
            }};
        }

        Ok(match tag[0] {
            F64_TAG => Value::F64(parse!(f64)),
            F32_TAG => Value::F32(parse!(f32)),
            I64_TAG => Value::I64(parse!(i64)),
            I32_TAG => Value::I32(parse!(i32)),
            STR_TAG => Value::Str({
                let len = parse!(u32);
                let mut buf = vec![0u8; len as usize];
                reader.read_exact(&mut buf)?;
                String::from_utf8(buf)?
            }),
            ARRAY_TAG => {
                let value_count = parse!(usize);
                let decl = TypeDecl::deserialize(reader)?;
                let values = (0..value_count)
                    .map(|_| Value::deserialize(reader))
                    .collect::<Result<_, _>>()?;
                Self::Array(ArrayInt::new(decl, values))
            }
            TUPLE_TAG => {
                let value_count = parse!(usize);
                let values = (0..value_count)
                    .map(|_| -> Result<_, ReadError> {
                        Ok(TupleEntry {
                            decl: TypeDecl::deserialize(reader)?,
                            value: Value::deserialize(reader)?,
                        })
                    })
                    .collect::<Result<_, _>>()?;
                Self::Tuple(Rc::new(RefCell::new(values)))
            }
            tag_byte => return Err(ReadError::UnknownTypeTag(tag_byte)),
        })
    }

    pub fn str_len(&self) -> EvalResult<usize> {
        if let Self::Str(str) = self {
            Ok(str.len())
        } else {
            Err(EvalError::WrongArgType(
                "str".to_string(),
                "str".to_string(),
            ))
        }
    }

    pub fn array_assign(&self, idx: usize, value: Value) -> EvalResult<()> {
        match self {
            Value::Array(array) => {
                *array.borrow_mut().values.eget_mut(idx)? = value;
            }
            Value::Struct(st) => {
                *st.borrow_mut().fields.eget_mut(idx)? = value;
            }
            _ => return Err(EvalError::IndexNonArray),
        }
        Ok(())
    }

    pub fn array_get(&self, idx: u64) -> EvalResult<Value> {
        match self {
            Value::Array(array) => Ok(array.borrow_mut().values.eget(idx as usize)?.clone()),
            Value::Struct(st) => Ok(st.borrow_mut().fields.eget(idx as usize)?.clone()),
            _ => Err(EvalError::IndexNonArray),
        }
    }

    pub fn array_push(&self, value: Value) -> Result<(), EvalError> {
        match self {
            Value::Array(array) => {
                let mut array_int = array.borrow_mut();
                array_int.values.push(value);
                Ok(())
            }
            _ => Err("push() must be called for an array".to_string().into()),
        }
    }

    /// Returns the length of an array, dereferencing recursively if the value was a reference.
    pub fn array_len(&self) -> EvalResult<usize> {
        match self {
            Value::Array(array) => Ok(array.borrow().values.len()),
            _ => Err("len() must be called for an array".to_string().into()),
        }
    }

    pub fn array_resize(&self, new_len: usize, val: &Value) -> EvalResult<()> {
        match self {
            Value::Array(array) => {
                array.borrow_mut().values.resize(new_len, val.clone());
                Ok(())
            }
            _ => Err("len() must be called for an array".to_string().into()),
        }
    }

    pub fn tuple_get(&self, idx: u64) -> Result<Value, EvalError> {
        Ok(match self {
            Value::Tuple(tuple) => {
                let tuple_int = tuple.borrow();
                tuple_int
                    .get(idx as usize)
                    .ok_or_else(|| EvalError::TupleOutOfBounds(idx as usize, tuple_int.len()))?
                    .value
                    .clone()
            }
            _ => return Err(EvalError::IndexNonArray),
        })
    }

    pub fn struct_field(&self, field_idx: u64) -> Result<Value, EvalError> {
        Ok(match self {
            Value::Struct(str) => {
                let str = str.borrow();
                str.fields
                    .get(field_idx as usize)
                    .ok_or_else(|| {
                        EvalError::TupleOutOfBounds(field_idx as usize, str.fields.len())
                    })?
                    .clone()
            }
            _ => return Err(EvalError::ExpectStruct(TypeDecl::from_value(self))),
        })
    }

    pub fn deepclone(&self) -> Self {
        match self {
            Self::Array(a) => {
                let a = a.borrow();
                let values = a.values.iter().map(|v| v.deepclone()).collect();
                Self::Array(Rc::new(RefCell::new(ArrayInt {
                    type_decl: a.type_decl.clone(),
                    values,
                })))
            }
            Self::Tuple(a) => {
                let a = a.borrow();
                let values = a
                    .iter()
                    .map(|v| TupleEntry {
                        decl: v.decl.clone(),
                        value: v.value.deepclone(),
                    })
                    .collect();
                Self::Tuple(Rc::new(RefCell::new(values)))
            }
            _ => self.clone(),
        }
    }
}

impl std::convert::TryFrom<&Value> for usize {
    type Error = ValueError;
    fn try_from(value: &Value) -> Result<Self, Self::Error> {
        match value {
            Value::F64(val) => {
                if *val < 0. {
                    Err(ValueError::Domain)
                } else {
                    Ok(*val as usize)
                }
            }
            Value::F32(val) => {
                if *val < 0. {
                    Err(ValueError::Domain)
                } else {
                    Ok(*val as usize)
                }
            }
            Value::I64(val) => Ok(*val as usize),
            Value::I32(val) => Ok(*val as usize),
            Value::Str(_) => Err(ValueError::Invalid(TypeDecl::Str, TypeDecl::I64)),
            Value::Array(rc) => {
                let arr = rc.borrow();
                Err(ValueError::Invalid(
                    TypeDecl::Array(Box::new(arr.type_decl.clone()), ArraySize::Any),
                    TypeDecl::I64,
                ))
            }
            Value::Tuple(rc) => {
                let tup = rc.borrow();
                Err(ValueError::Invalid(
                    TypeDecl::Tuple(tup.iter().map(|val| val.decl.clone()).collect()),
                    TypeDecl::I64,
                ))
            }
            Value::Struct(rc) => {
                let str = rc.borrow();
                Err(ValueError::Invalid(
                    TypeDecl::TypeName(str.name.clone()),
                    TypeDecl::I64,
                ))
            }
        }
    }
}

pub type TupleInt = Vec<TupleEntry>;

#[derive(Debug, PartialEq, Clone)]
pub struct TupleEntry {
    pub(crate) decl: TypeDecl,
    pub(crate) value: Value,
}

impl TupleEntry {
    pub fn value(&self) -> &Value {
        &self.value
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructInt {
    /// Type name of the struct
    pub(crate) name: String,
    pub(crate) fields: Vec<Value>,
}

impl StructInt {
    pub fn get(&self, idx: usize) -> EvalResult<Value> {
        self.fields
            .get(idx)
            .ok_or_else(|| EvalError::ArrayOutOfBounds(self.fields.len(), idx))
            .cloned()
    }

    pub fn fields(&self) -> &[Value] {
        &self.fields
    }
}

#[derive(Debug)]
pub enum ValueError {
    Domain,
    Invalid(TypeDecl, TypeDecl),
}

impl std::fmt::Display for ValueError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Domain => write!(f, "Domain error"),
            Self::Invalid(from, to) => write!(
                f,
                "Invalid conversion between types error from {from} to {to}"
            ),
        }
    }
}

impl std::error::Error for ValueError {}
