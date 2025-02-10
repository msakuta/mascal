use crate::{type_checker::tc_array_size, type_decl::ArraySize, TypeDecl};

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub enum TypeSet {
    /// "Any" type set is used to avoid infinite recursion of nested array types.
    #[default]
    Any,
    Set(TypeSetFlags),
}

impl TypeSet {
    pub fn map<T>(&self, f: impl Fn(&TypeSetFlags) -> T) -> Option<T> {
        match self {
            Self::Any => None,
            Self::Set(set) => Some(f(set)),
        }
    }

    pub fn and_then<'a, T: 'a>(&'a self, f: impl Fn(&'a TypeSetFlags) -> Option<T>) -> Option<T> {
        match self {
            Self::Any => None,
            Self::Set(set) => f(set),
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct TypeSetFlags {
    pub i32: bool,
    pub i64: bool,
    pub f32: bool,
    pub f64: bool,
    pub void: bool,
    pub string: bool,
    pub array: Option<(Box<TypeSet>, ArraySize)>,
    pub tuple: Option<Vec<TypeSet>>,
}

impl TypeSet {
    pub fn i32() -> Self {
        Self::Set(TypeSetFlags {
            i32: true,
            ..TypeSetFlags::default()
        })
    }

    pub fn i64() -> Self {
        Self::Set(TypeSetFlags {
            i64: true,
            ..TypeSetFlags::default()
        })
    }

    pub fn int() -> Self {
        Self::Set(TypeSetFlags {
            i32: true,
            i64: true,
            ..TypeSetFlags::default()
        })
    }

    pub fn f32() -> Self {
        Self::Set(TypeSetFlags {
            f32: true,
            ..TypeSetFlags::default()
        })
    }

    pub fn f64() -> Self {
        Self::Set(TypeSetFlags {
            f64: true,
            ..TypeSetFlags::default()
        })
    }

    pub fn float() -> Self {
        Self::Set(TypeSetFlags {
            f32: true,
            f64: true,
            ..TypeSetFlags::default()
        })
    }

    pub fn void() -> Self {
        Self::Set(TypeSetFlags {
            void: true,
            ..TypeSetFlags::default()
        })
    }

    pub fn str() -> Self {
        Self::Set(TypeSetFlags {
            string: true,
            ..TypeSetFlags::default()
        })
    }

    pub fn array(ty: TypeSet, size: ArraySize) -> Self {
        Self::Set(TypeSetFlags {
            array: Some((Box::new(ty), size)),
            ..TypeSetFlags::default()
        })
    }

    pub fn tuple(type_sets: Vec<TypeSet>) -> Self {
        Self::Set(TypeSetFlags {
            tuple: Some(type_sets),
            ..TypeSetFlags::default()
        })
    }

    pub fn all() -> Self {
        Self::Any
    }

    pub fn is_none(&self) -> bool {
        match self {
            Self::Any => false,
            Self::Set(set) => {
                !set.i32
                    && !set.i64
                    && !set.f32
                    && !set.f64
                    && !set.string
                    && !set.void
                    && set.array.is_none()
                    && set.tuple.is_none()
            }
        }
    }

    pub fn determine(&self) -> Option<TypeDecl> {
        if self == &TypeDecl::I32.into() {
            return Some(TypeDecl::I32);
        } else if self == &TypeDecl::I64.into() {
            return Some(TypeDecl::I64);
        } else if self == &TypeDecl::F32.into() {
            return Some(TypeDecl::F32);
        } else if self == &TypeDecl::F64.into() {
            return Some(TypeDecl::F64);
        } else if self == &TypeDecl::Str.into() {
            return Some(TypeDecl::Str);
        } else if let TypeSet::Set(set) = self {
            if !set.i32 && !set.i64 && !set.f32 && !set.f64 {
                if let Some((ty, size)) = set
                    .array
                    .as_ref()
                    .and_then(|a| Some((a.0.determine()?, a.1.clone())))
                {
                    return Some(TypeDecl::Array(Box::new(ty), size));
                } else if let Some(tuple) = &set.tuple {
                    let type_sets: Vec<TypeDecl> =
                        tuple.iter().map(|a| a.determine()).collect::<Option<_>>()?;
                    return Some(TypeDecl::Tuple(type_sets));
                }
            }
        }
        None
    }
}

// impl std::ops::BitOr for TypeSet {
//     type Output = Self;
//     fn bitor(self, rhs: Self) -> Self::Output {
//         let TypeSet::Set(set) = self else { return rhs };
//         let TypeSet::Set(rhs) = rhs else { return self };
//         Self::Set(TypeSetFlags {
//             i32: set.i32 | rhs.i32,
//             i64: set.i64 | rhs.i64,
//             f32: set.f32 | rhs.f32,
//             f64: set.f64 | rhs.f64,
//             void: set.void | rhs.void,
//             string: set.string | rhs.string,
//             array: set.array.zip(rhs.array).map(|(set, rhs)| *set | *rhs),
//         })
//     }
// }

impl std::ops::BitAnd for TypeSet {
    type Output = Self;
    fn bitand(mut self, rhs: Self) -> Self::Output {
        let TypeSet::Set(set) = &mut self else {
            return rhs;
        };
        let TypeSet::Set(rhs) = &rhs else { return self };
        let array = set
            .array
            .as_mut()
            .zip(rhs.array.as_ref())
            .and_then(|(set, rhs)| {
                // The element type of the array has to be "Any" or determined, not a mix of 2 types
                if set.0 != rhs.0 {
                    return None;
                }
                let size = set.1.try_and(&rhs.1)?;
                Some((std::mem::take(&mut set.0), size))
            });
        let tuple = set
            .tuple
            .as_mut()
            .zip(rhs.tuple.as_ref())
            .and_then(|(set, rhs)| {
                if set.len() != rhs.len() {
                    return None;
                }
                Some(
                    std::mem::take(set)
                        .into_iter()
                        .zip(rhs.iter())
                        .map(|(set, rhs)| set & rhs.clone())
                        .collect(),
                )
            });
        Self::Set(TypeSetFlags {
            i32: set.i32 & rhs.i32,
            i64: set.i64 & rhs.i64,
            f32: set.f32 & rhs.f32,
            f64: set.f64 & rhs.f64,
            void: set.void & rhs.void,
            string: set.string & rhs.string,
            array,
            tuple,
        })
    }
}

impl TypeSet {
    /// Try to retrieve intersection between operands, returning error when it yields empty set.
    /// Logically it is similar to BitAnd operator trait, but we want to return the cause if we fail.
    /// Note that `void` is a valid TypeSet, so it won't be an error.
    pub fn try_intersect(&self, rhs: &Self) -> Result<Self, String> {
        // It's a bit annoying to repeat a similar logic in `BitAnd` for `TypeSet`, but we don't want to clone
        // when it's possible to avoid.
        let TypeSet::Set(set) = self else {
            return Ok(rhs.clone());
        };
        let TypeSet::Set(rhs) = rhs else {
            return Ok(self.clone());
        };
        let pair = set.array.as_ref().zip(rhs.array.as_ref());
        let array = if let Some((set, rhs)) = pair {
            // The element type of the array has to be "Any" or determined, not a mix of 2 types
            println!("bitand {:?} & {:?}", self, rhs);
            let ty = set.0.as_ref().try_intersect(&rhs.0)?;
            tc_array_size(&set.1, &rhs.1)?;
            if let Some(size) = set.1.try_and(&rhs.1) {
                let res = (Box::new(ty), size);
                println!("Array bitand result: {:?}", res);
                Some(res)
            } else {
                None
            }
        } else {
            None
        };

        let tuple = if let Some((set, rhs)) = set.tuple.as_ref().zip(rhs.tuple.as_ref()) {
            if set.len() != rhs.len() {
                return Err(format!(
                    "Tuple size is not the same: {} != {}",
                    set.len(),
                    rhs.len()
                ));
            }
            Some(
                set.iter()
                    .zip(rhs.iter())
                    .map(|(set, rhs)| set.try_intersect(rhs))
                    .collect::<Result<_, _>>()?,
            )
        } else {
            None
        };

        Ok(TypeSet::Set(TypeSetFlags {
            i32: set.i32 & rhs.i32,
            i64: set.i64 & rhs.i64,
            f32: set.f32 & rhs.f32,
            f64: set.f64 & rhs.f64,
            void: set.void & rhs.void,
            string: set.string & rhs.string,
            array,
            tuple,
        }))
    }
}

impl From<&TypeDecl> for TypeSet {
    fn from(value: &TypeDecl) -> Self {
        if matches!(value, TypeDecl::Any) {
            return TypeSet::Any;
        }
        let mut ret = TypeSetFlags::default();
        match value {
            TypeDecl::I32 => ret.i32 = true,
            TypeDecl::I64 => ret.i64 = true,
            TypeDecl::F32 => ret.f32 = true,
            TypeDecl::F64 => ret.f64 = true,
            TypeDecl::Any => {
                ret.i32 = true;
                ret.i64 = true;
                ret.f32 = true;
                ret.f64 = true;
            }
            TypeDecl::Str => ret.string = true,
            TypeDecl::Array(ty, size) => return TypeSet::array(ty.as_ref().into(), size.clone()),
            TypeDecl::Tuple(types) => {
                return TypeSet::tuple(types.iter().map(|ty| ty.into()).collect())
            }
        }
        TypeSet::Set(ret)
    }
}

impl From<TypeDecl> for TypeSet {
    fn from(value: TypeDecl) -> Self {
        Self::from(&value)
    }
}

impl From<&Option<TypeDecl>> for TypeSet {
    fn from(value: &Option<TypeDecl>) -> Self {
        match value {
            Some(ref value) => Self::from(value),
            None => Self::all(),
        }
    }
}

impl From<&mut Option<TypeDecl>> for TypeSet {
    fn from(value: &mut Option<TypeDecl>) -> Self {
        match value {
            Some(ref value) => Self::from(value),
            None => Self::all(),
        }
    }
}

impl std::fmt::Display for TypeSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self::Set(set) = self else {
            return write!(f, "any");
        };
        let mut written = false;
        let mut write_ty = |val, name| {
            if val {
                if written {
                    write!(f, "|")?;
                }
                write!(f, "{name}")?;
                written = true;
            }
            Ok(())
        };
        write_ty(set.i32, "i32")?;
        write_ty(set.i64, "i64")?;
        write_ty(set.f32, "f32")?;
        write_ty(set.f64, "f64")?;
        write_ty(set.void, "void")?;
        write_ty(set.string, "str")?;
        if let Some(array) = set.array.as_ref() {
            write_ty(true, &array_size_to_string(array))?;
        }

        // for st in &self.structs {
        //     write_ty(true, st)?;
        // }

        if !written {
            write!(f, "(none)")?;
        }
        Ok(())
    }
}

fn array_size_to_string(this: &(Box<TypeSet>, ArraySize)) -> String {
    match &this.1 {
        ArraySize::Fixed(size) => format!("[{}; {}]", this.0, size),
        ArraySize::Range(range) => format!("[{}; {:?}]", this.0, range),
        _ => format!("[{}]", this.0),
    }
}
