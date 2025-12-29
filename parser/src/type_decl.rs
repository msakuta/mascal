mod array_size;

pub use self::array_size::ArraySize;
use self::array_size::{read_array_size, write_array_size};
use std::io::{Read, Write};

use crate::{
    bytecode::{read_bool, write_bool},
    type_tags::*,
    ReadError, Value,
};

#[derive(Debug, PartialEq, Clone)]
#[repr(u8)]
pub enum TypeDecl {
    Any,
    F64,
    F32,
    I64,
    I32,
    Str,
    Array(Box<TypeDecl>, ArraySize),
    Tuple(Vec<TypeDecl>),
    TypeName(String),
    Func,
}

impl TypeDecl {
    pub(crate) fn from_value(value: &Value) -> Self {
        match value {
            Value::F64(_) => Self::F64,
            Value::F32(_) => Self::F32,
            Value::I32(_) => Self::I32,
            Value::I64(_) => Self::I64,
            Value::Str(_) => Self::Str,
            Value::Array(a) => Self::Array(Box::new(a.borrow().type_decl.clone()), ArraySize::Any),
            Value::Tuple(a) => Self::Tuple(
                a.borrow()
                    .iter()
                    .map(|val| Self::from_value(&val.value))
                    .collect(),
            ),
            Value::Struct(a) => Self::TypeName(a.borrow().name.clone()),
            Value::Func(_) => Self::Func,
        }
    }

    pub(crate) fn serialize(&self, writer: &mut impl Write) -> std::io::Result<()> {
        let tag = match self {
            Self::Any => 0xff,
            Self::F64 => F64_TAG,
            Self::F32 => F32_TAG,
            Self::I64 => I64_TAG,
            Self::I32 => I32_TAG,
            Self::Str => STR_TAG,
            Self::Array(inner, len) => {
                writer.write_all(&ARRAY_TAG.to_le_bytes())?;
                write_array_size(len, writer)?;
                inner.serialize(writer)?;
                return Ok(());
            }
            Self::Tuple(inner) => {
                writer.write_all(&TUPLE_TAG.to_le_bytes())?;
                for decl in inner {
                    decl.serialize(writer)?;
                }
                return Ok(());
            }
            Self::TypeName(_) => todo!(),
            Self::Func => FUNC_TAG,
        };
        writer.write_all(&tag.to_le_bytes())?;
        Ok(())
    }

    pub(crate) fn deserialize(reader: &mut impl Read) -> std::io::Result<Self> {
        macro_rules! read {
            ($ty:ty) => {{
                let mut buf = [0u8; std::mem::size_of::<$ty>()];
                reader.read_exact(&mut buf)?;
                <$ty>::from_le_bytes(buf)
            }};
        }

        let tag = read!(u8);
        Ok(match tag {
            0xff => Self::Any,
            F64_TAG => Self::F64,
            F32_TAG => Self::F32,
            I64_TAG => Self::I64,
            I32_TAG => Self::I32,
            STR_TAG => Self::Str,
            ARRAY_TAG => Self::Array(
                Box::new(Self::deserialize(reader)?),
                read_array_size(reader).map_err(|e| {
                    let ReadError::IO(e) = e else { panic!() };
                    e
                })?,
            ),
            _ => unreachable!(),
        })
    }
}

impl std::fmt::Display for TypeDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeDecl::Any => write!(f, "any")?,
            TypeDecl::F64 => write!(f, "f64")?,
            TypeDecl::F32 => write!(f, "f32")?,
            TypeDecl::I64 => write!(f, "i64")?,
            TypeDecl::I32 => write!(f, "i32")?,
            TypeDecl::Str => write!(f, "str")?,
            TypeDecl::Array(inner, len) => match len {
                ArraySize::Any => write!(f, "[{}]", inner)?,
                _ => write!(f, "[{}; {}]", inner, len)?,
            },
            TypeDecl::Tuple(inner) => write!(
                f,
                "({})",
                inner.iter().fold(String::new(), |acc, cur| {
                    let str = cur.to_string();
                    if acc.is_empty() {
                        str
                    } else {
                        acc + ", " + &str
                    }
                })
            )?,
            Self::TypeName(name) => write!(f, "{name}")?,
            Self::Func => write!(f, "fn()")?,
        }
        Ok(())
    }
}

#[allow(dead_code)]
fn write_opt_usize(value: &Option<usize>, writer: &mut impl Write) -> std::io::Result<()> {
    write_bool(value.is_some(), writer)?;
    if let Some(value) = value {
        writer.write_all(&value.to_le_bytes())?;
    }
    Ok(())
}

#[allow(dead_code)]
fn read_opt_usize(reader: &mut impl Read) -> Result<Option<usize>, ReadError> {
    let has_value = read_bool(reader)?;
    Ok(if has_value {
        let mut buf = [0u8; std::mem::size_of::<usize>()];
        reader.read_exact(&mut buf)?;
        Some(usize::from_le_bytes(buf))
    } else {
        None
    })
}
