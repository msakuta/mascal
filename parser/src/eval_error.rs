use crate::{value::ValueError, TypeDecl};

/// Error type for the AST intepreter and bytecode interpreter.
/// Note that it is shared among 2 kinds of interpreters, so some of them only happen in either kind.
/// Also note that it is supposed to be displayed with Display or "{}" format, not with Debug or "{:?}".
///
/// It owns the value so it is not bounded by a lifetime.
/// The information about the error shold be converted to a string (by `format!("{:?}")`) before wrapping it
/// into `EvalError`.
#[non_exhaustive]
#[derive(Debug)]
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
    AssignToLiteral(String),
    IndexNonNum,
    NonLValue(String),
    PrematureEnd,
    WrongArgType(String, String),
    IOError(std::io::Error),
    ValueError(ValueError),
    NoStructFound(String),
    NoFieldFound(String),
    TypeCheck(String),
    ExpectStruct(TypeDecl),
    ExpectFn(String),
}

impl std::convert::From<ValueError> for EvalError {
    fn from(value: ValueError) -> Self {
        Self::ValueError(value)
    }
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
            Self::AssignToLiteral(name) => write!(f, "Cannot assign to a literal: {}", name),
            Self::IndexNonNum => write!(f, "Indexed an array with a non-number"),
            Self::NonLValue(ex) => write!(f, "Expression {} is not an lvalue.", ex),
            Self::PrematureEnd => {
                write!(f, "End of input bytecode encountered before seeing a Ret")
            }
            Self::WrongArgType(arg, expected) => {
                write!(f, "Argument {arg} type expected {expected}")
            }
            Self::IOError(e) => e.fmt(f),
            Self::ValueError(e) => e.fmt(f),
            Self::NoStructFound(name) => write!(f, "Struct {name} not found"),
            Self::NoFieldFound(name) => write!(f, "Field {name} not found"),
            Self::TypeCheck(name) => write!(f, "Type check error: {name}"),
            Self::ExpectStruct(ty) => write!(f, "Expect a struct, but got a {ty}"),
            Self::ExpectFn(ty) => write!(f, "Expect a function, but got a {ty}"),
        }
    }
}

impl From<String> for EvalError {
    fn from(value: String) -> Self {
        Self::Other(value)
    }
}

impl From<std::io::Error> for EvalError {
    fn from(value: std::io::Error) -> Self {
        Self::IOError(value)
    }
}
