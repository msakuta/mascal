use crate::{EvalError, Span};

#[non_exhaustive]
#[derive(Debug)]
pub enum CompileErrorKind {
    LocalsStackUnderflow,
    BreakInArrayLiteral,
    DisallowedBreak,
    EvalError(EvalError),
    VarNotFound(String),
    FnNotFound(String),
    InsufficientNamedArgs,
    UnknownNamedArg,
    AssignToLiteral(String),
    NonLValue(String),
    FromUtf8Error(std::string::FromUtf8Error),
    IoError(std::io::Error),
}

#[derive(Debug)]
pub struct CompileError<'src> {
    span: Option<Span<'src>>,
    kind: CompileErrorKind,
}

impl<'src> CompileError<'src> {
    pub fn new(span: Span<'src>, kind: CompileErrorKind) -> Self {
        Self {
            span: Some(span),
            kind,
        }
    }

    pub fn new_nospan(kind: CompileErrorKind) -> Self {
        Self { span: None, kind }
    }
}

impl<'src> std::error::Error for CompileError<'src> {}

impl std::fmt::Display for CompileErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LocalsStackUnderflow => write!(f, "Local variables stack underflow"),
            Self::BreakInArrayLiteral => write!(f, "Break in array literal not supported"),
            Self::DisallowedBreak => write!(f, "Break in function default arg is not allowed"),
            Self::EvalError(e) => write!(f, "Evaluation error: {e}"),
            Self::VarNotFound(name) => write!(f, "Variable {name} not found in scope"),
            Self::FnNotFound(name) => write!(f, "Function {name} is not defined"),
            Self::InsufficientNamedArgs => {
                write!(f, "Named arguments does not cover all required args")
            }
            Self::UnknownNamedArg => write!(f, "An unknown named argument"),
            Self::AssignToLiteral(name) => write!(f, "Cannot assign to a literal: {}", name),
            Self::NonLValue(ex) => write!(
                f,
                "Attempt assignment to expression {} which is not an lvalue.",
                ex
            ),
            Self::FromUtf8Error(e) => e.fmt(f),
            Self::IoError(e) => e.fmt(f),
        }
    }
}

impl<'src> std::fmt::Display for CompileError<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(span) = self.span {
            write!(
                f,
                "{}:{}: {}",
                span.location_line(),
                span.get_utf8_column(),
                self.kind
            )
        } else {
            self.kind.fmt(f)
        }
    }
}

impl<'src> From<std::string::FromUtf8Error> for CompileError<'src> {
    fn from(value: std::string::FromUtf8Error) -> Self {
        Self {
            span: None,
            kind: CompileErrorKind::FromUtf8Error(value),
        }
    }
}

impl<'src> From<std::io::Error> for CompileError<'src> {
    fn from(value: std::io::Error) -> Self {
        Self {
            span: None,
            kind: CompileErrorKind::IoError(value),
        }
    }
}
