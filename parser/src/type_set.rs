use crate::TypeDecl;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub enum TypeSet {
    /// "Any" type set is used to avoid infinite recursion of nested array types.
    #[default]
    Any,
    Set(TypeSetFlags),
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct TypeSetFlags {
    pub i32: bool,
    pub i64: bool,
    pub f32: bool,
    pub f64: bool,
    pub void: bool,
    pub string: bool,
    pub array: Option<Box<ArrayTypeSet>>,
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

    pub fn array(ty: TypeSet, size: Option<usize>) -> Self {
        Self::Set(TypeSetFlags {
            array: Some(Box::new(ArrayTypeSet { ty, size })),
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
                !set.i32 && !set.i64 && !set.f32 && !set.f64 && !set.string && !set.void
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
                if set.ty != rhs.ty
                    || set.size.is_some() && rhs.size.is_some() && set.size != rhs.size
                {
                    return None;
                }
                Some(Box::new(ArrayTypeSet {
                    ty: std::mem::take(&mut set.ty),
                    size: if set.size.is_some() {
                        set.size
                    } else {
                        rhs.size
                    },
                }))
            });
        Self::Set(TypeSetFlags {
            i32: set.i32 & rhs.i32,
            i64: set.i64 & rhs.i64,
            f32: set.f32 & rhs.f32,
            f64: set.f64 & rhs.f64,
            void: set.void & rhs.void,
            string: set.string & rhs.string,
            array,
        })
    }
}

impl std::ops::BitAnd for &TypeSet {
    type Output = TypeSet;
    fn bitand(self, rhs: Self) -> Self::Output {
        // It's a bit annoying to repeat a similar logic in `BitAnd` for `TypeSet`, but we don't want to clone
        // when it's possible to avoid.
        let TypeSet::Set(set) = self else {
            return rhs.clone();
        };
        let TypeSet::Set(rhs) = rhs else {
            return self.clone();
        };
        let array = set
            .array
            .as_ref()
            .zip(rhs.array.as_ref())
            .and_then(|(set, rhs)| {
                // The element type of the array has to be "Any" or determined, not a mix of 2 types
                println!("bitand {} & {}", self, rhs);
                if set.ty != rhs.ty
                    || set.size.is_some() && rhs.size.is_some() && set.size != rhs.size
                {
                    return None;
                }
                Some(Box::new(ArrayTypeSet {
                    ty: set.ty.clone(),
                    size: if set.size.is_some() {
                        set.size
                    } else {
                        rhs.size
                    },
                }))
            });
        TypeSet::Set(TypeSetFlags {
            i32: set.i32 & rhs.i32,
            i64: set.i64 & rhs.i64,
            f32: set.f32 & rhs.f32,
            f64: set.f64 & rhs.f64,
            void: set.void & rhs.void,
            string: set.string & rhs.string,
            array,
        })
    }
}

impl From<&TypeDecl> for TypeSet {
    fn from(value: &TypeDecl) -> Self {
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
            TypeDecl::Array(ty, _) => return TypeSet::array(ty.as_ref().into(), None),
            TypeDecl::Tuple(_) => todo!(),
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
            write_ty(true, &array.to_string())?;
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

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct ArrayTypeSet {
    ty: TypeSet,
    size: Option<usize>,
}

impl std::fmt::Display for ArrayTypeSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.size {
            Some(size) => write!(f, "[{}; {}]", self.ty, size),
            None => write!(f, "[{}]", self.ty),
        }
    }
}
