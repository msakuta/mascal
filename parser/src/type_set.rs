use crate::TypeDecl;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct TypeSet {
    pub i32: bool,
    pub i64: bool,
    pub f32: bool,
    pub f64: bool,
    pub void: bool,
    pub string: bool,
}

impl TypeSet {
    pub fn i32() -> Self {
        Self {
            i32: true,
            ..Self::default()
        }
    }

    pub fn i64() -> Self {
        Self {
            i64: true,
            ..Self::default()
        }
    }

    pub fn int() -> Self {
        Self {
            i32: true,
            i64: true,
            ..Self::default()
        }
    }

    pub fn f32() -> Self {
        Self {
            f32: true,
            ..Self::default()
        }
    }

    pub fn f64() -> Self {
        Self {
            f64: true,
            ..Self::default()
        }
    }

    pub fn void() -> Self {
        Self {
            void: true,
            ..Self::default()
        }
    }

    pub fn str() -> Self {
        Self {
            string: true,
            ..Self::default()
        }
    }

    pub fn all() -> Self {
        Self {
            i32: true,
            i64: true,
            f32: true,
            f64: true,
            void: true,
            string: true,
        }
    }

    pub fn is_none(&self) -> bool {
        !self.i32 && !self.i64 && !self.f32 && !self.f64 && !self.string && !self.void
    }

    pub fn iter_primitives(&self) -> impl Iterator<Item = (TypeDecl, bool)> {
        std::iter::once((TypeDecl::I32, self.i32))
            .chain(std::iter::once((TypeDecl::I64, self.i64)))
            .chain(std::iter::once((TypeDecl::F32, self.f32)))
            .chain(std::iter::once((TypeDecl::F64, self.f64)))
            .chain(std::iter::once((TypeDecl::Str, self.string)))
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

impl std::ops::BitOr for TypeSet {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        Self {
            i32: self.i32 | rhs.i32,
            i64: self.i64 | rhs.i64,
            f32: self.f32 | rhs.f32,
            f64: self.f64 | rhs.f64,
            void: self.void | rhs.void,
            string: self.string | rhs.string,
        }
    }
}

impl std::ops::BitAnd for TypeSet {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        Self {
            i32: self.i32 & rhs.i32,
            i64: self.i64 & rhs.i64,
            f32: self.f32 & rhs.f32,
            f64: self.f64 & rhs.f64,
            void: self.void & rhs.void,
            string: self.string & rhs.string,
        }
    }
}

impl std::ops::BitAnd for &TypeSet {
    type Output = TypeSet;
    fn bitand(self, rhs: Self) -> Self::Output {
        TypeSet {
            i32: self.i32 & rhs.i32,
            i64: self.i64 & rhs.i64,
            f32: self.f32 & rhs.f32,
            f64: self.f64 & rhs.f64,
            void: self.void & rhs.void,
            string: self.string & rhs.string,
        }
    }
}

impl From<&TypeDecl> for TypeSet {
    fn from(value: &TypeDecl) -> Self {
        let mut ret = Self::default();
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
            TypeDecl::Array(_, _) => todo!(),
            TypeDecl::Tuple(_) => todo!(),
        }
        ret
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
        if self.i32 & self.i64 & self.f32 & self.f64 & self.void & self.string
        /* && self.structs.is_any() */
        {
            return write!(f, "any");
        }
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
        write_ty(self.i32, "i32")?;
        write_ty(self.i64, "i64")?;
        write_ty(self.f32, "f32")?;
        write_ty(self.f64, "f64")?;
        write_ty(self.void, "void")?;
        write_ty(self.string, "str")?;

        // for st in &self.structs {
        //     write_ty(true, st)?;
        // }

        if !written {
            write!(f, "(none)")?;
        }
        Ok(())
    }
}
