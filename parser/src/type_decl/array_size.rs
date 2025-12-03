use std::io::{Read, Write};

use crate::ReadError;

/// Array size that possibly define the shape of multi-dimensional arrays.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ArraySize(pub Vec<ArraySizeAxis>);

impl Default for ArraySize {
    fn default() -> Self {
        Self(vec![ArraySizeAxis::Any])
    }
}

impl std::ops::Deref for ArraySize {
    type Target = Vec<ArraySizeAxis>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ArraySize {
    /// Returns a 1-dimensional array with unbounded size.
    pub fn all_dyn() -> Self {
        Self(vec![ArraySizeAxis::Range(0..usize::MAX)])
    }

    pub fn try_and(&self, other: &Self) -> Self {
        Self(
            self.iter()
                .zip(other.iter())
                .map(|(s, r)| s.try_and(&r).unwrap_or(ArraySizeAxis::Fixed(0)))
                .collect::<Vec<_>>(),
        )
    }
}

impl std::fmt::Display for ArraySize {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, a) in self.0.iter().enumerate() {
            if i != 0 {
                write!(f, ", {}", a)?;
            } else {
                write!(f, "{}", a)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ArraySizeAxis {
    /// Either dynamic or fixed array
    Any,
    /// Fixed array with a length
    Fixed(usize),
    /// Dynamic array with specified range
    Range(std::ops::Range<usize>),
}

impl ArraySizeAxis {
    fn tag(&self) -> u8 {
        match self {
            Self::Any => 0,
            Self::Fixed(_) => 1,
            Self::Range(_) => 2,
        }
    }

    pub fn zip(&self, other: &Self) -> Option<(usize, usize)> {
        match (self, other) {
            (Self::Fixed(lhs), Self::Fixed(rhs)) => Some((*lhs, *rhs)),
            _ => None,
        }
    }

    pub fn ok(&self) -> Option<usize> {
        match self {
            Self::Fixed(len) => Some(*len),
            _ => None,
        }
    }

    pub fn or(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Fixed(lhs), _) => Self::Fixed(*lhs),
            (_, Self::Fixed(rhs)) => Self::Fixed(*rhs),
            (Self::Range(lhs), Self::Range(rhs)) => {
                Self::Range(lhs.start.min(rhs.start)..lhs.end.max(rhs.end))
            }
            _ => Self::Any,
        }
    }

    /// It returns None when there is no valid range in the intersection of 2 ranges
    pub fn try_and(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Any, other) => Some(other.clone()),
            (this, Self::Any) => Some(this.clone()),
            (Self::Range(lhs), Self::Range(rhs)) => {
                let min = lhs.clone().min()?.max(rhs.clone().min()?);
                let max = lhs.clone().max()?.min(rhs.clone().max()?);
                Some(if max - min <= 1 {
                    Self::Fixed(min)
                } else {
                    Self::Range(min..max + 1)
                })
            }
            (Self::Range(lhs), Self::Fixed(rhs)) => {
                if lhs.contains(&rhs) {
                    Some(other.clone())
                } else {
                    None
                }
            }
            (Self::Fixed(lhs), Self::Range(rhs)) => {
                if rhs.contains(&lhs) {
                    Some(self.clone())
                } else {
                    None
                }
            }
            (Self::Fixed(lhs), Self::Fixed(rhs)) => {
                if lhs == rhs {
                    Some(self.clone())
                } else {
                    None
                }
            }
        }
    }

    pub fn contains(&self, size: usize) -> bool {
        match self {
            Self::Any => true,
            Self::Fixed(fixed) => *fixed == size,
            Self::Range(range) => range.contains(&size),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Self::Fixed(0) => true,
            _ => false,
        }
    }
}

impl std::fmt::Display for ArraySizeAxis {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Any => write!(f, "_"),
            Self::Fixed(v) => write!(f, "{v}"),
            Self::Range(range) => match (range.start, range.end) {
                (0, usize::MAX) => write!(f, ".."),
                (start, usize::MAX) => write!(f, "{start}.."),
                (0, end) => write!(f, "..{end}"),
                (start, end) => write!(f, "{start}..{end}"),
            },
        }
    }
}

pub(super) fn write_array_size(value: &ArraySize, writer: &mut impl Write) -> std::io::Result<()> {
    writer.write_all(&(value.0.len() as u64).to_le_bytes())?;
    for v in &value.0 {
        write_array_size_axis(v, writer)?;
    }
    Ok(())
}

pub(super) fn write_array_size_axis(
    value: &ArraySizeAxis,
    writer: &mut impl Write,
) -> std::io::Result<()> {
    writer.write_all(&mut [value.tag()])?;
    match value {
        ArraySizeAxis::Fixed(value) => writer.write_all(&(*value as u64).to_le_bytes())?,
        ArraySizeAxis::Range(range) => {
            writer.write_all(&(range.start as u64).to_le_bytes())?;
            writer.write_all(&(range.end as u64).to_le_bytes())?;
        }
        _ => {}
    }
    Ok(())
}

pub(super) fn read_array_size(reader: &mut impl Read) -> Result<ArraySize, ReadError> {
    let mut buf = [0u8; std::mem::size_of::<u64>()];
    reader.read_exact(&mut buf)?;
    let size = u64::from_le_bytes(buf) as usize;
    let values = (0..size)
        .map(|_| read_array_size_axis(reader))
        .collect::<Result<_, _>>()?;
    Ok(ArraySize(values))
}

pub(super) fn read_array_size_axis(reader: &mut impl Read) -> Result<ArraySizeAxis, ReadError> {
    let mut tag = [0u8; 1];
    reader.read_exact(&mut tag)?;
    Ok(match tag[0] {
        0 => ArraySizeAxis::Any,
        1 => {
            let mut buf = [0u8; std::mem::size_of::<u64>()];
            reader.read_exact(&mut buf)?;
            ArraySizeAxis::Fixed(u64::from_le_bytes(buf) as usize)
        }
        2 => {
            let mut buf = [0u8; std::mem::size_of::<u64>()];
            reader.read_exact(&mut buf)?;
            let start = u64::from_le_bytes(buf) as usize;
            reader.read_exact(&mut buf)?;
            let end = u64::from_le_bytes(buf) as usize;
            ArraySizeAxis::Range(start..end)
        }
        _ => return Err(ReadError::UndefinedOpCode(tag[0])),
    })
}
