use core::fmt;
use std::borrow::Cow;

use crate::common::Span;
use crate::compiler::error::StaticError;
use crate::value::InternedString;

#[derive(thiserror::Error, miette::Diagnostic, Debug, Clone)]
pub enum InterpretError<'a> {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Static(StaticError<'a>),

    #[error(transparent)]
    #[diagnostic(transparent)]
    Runtime(RuntimeError),
}

impl<'a> From<StaticError<'a>> for InterpretError<'a> {
    fn from(err: StaticError<'a>) -> Self {
        Self::Static(err)
    }
}

impl<'a> From<RuntimeError> for InterpretError<'a> {
    fn from(err: RuntimeError) -> Self {
        Self::Runtime(err)
    }
}

#[derive(Debug, thiserror::Error, miette::Diagnostic, Clone)]
#[error("{kind}")]
pub struct RuntimeError {
    #[label("here")]
    pub span: Option<miette::SourceSpan>,
    pub kind: RuntimeErrorKind,
}

impl RuntimeError {
    pub fn to_owned(self) -> RuntimeError {
        RuntimeError {
            span: self.span,
            kind: self.kind,
        }
    }
}

#[derive(Debug, Clone)]
pub enum RuntimeErrorKind {
    InvalidOperand { expected: &'static str },

    InvalidOperands { expected: &'static str },
    UndefinedVariable { name: InternedString },
    WrongNumberOfArguments { expected: usize, got: usize },
    Msg(Cow<'static, str>),
}

impl fmt::Display for RuntimeErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RuntimeErrorKind::InvalidOperand { expected } => {
                write!(f, "invalid operand, expected a {}", expected)
            }

            RuntimeErrorKind::InvalidOperands { expected } => {
                write!(f, "invalid operands, expected {}", expected)
            }

            RuntimeErrorKind::UndefinedVariable { name } => {
                write!(f, "undefined variable `{}`", **name)
            }

            RuntimeErrorKind::WrongNumberOfArguments { expected, got } => {
                write!(
                    f,
                    "wrong number of arguments, expected {}, got {}",
                    expected, got
                )
            }

            RuntimeErrorKind::Msg(msg) => write!(f, "{}", msg),
        }
    }
}
