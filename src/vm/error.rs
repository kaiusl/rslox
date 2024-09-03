use core::fmt;
use std::borrow::Cow;

use crate::common::Span;
use crate::compiler::error::StaticError;

#[derive(thiserror::Error, miette::Diagnostic, Debug, Clone)]
pub enum InterpretError<'a> {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Static(StaticError<'a>),

    #[error(transparent)]
    #[diagnostic(transparent)]
    Runtime(RuntimeError<'a>),
}

impl<'a> From<StaticError<'a>> for InterpretError<'a> {
    fn from(err: StaticError<'a>) -> Self {
        Self::Static(err)
    }
}

impl<'a> From<RuntimeError<'a>> for InterpretError<'a> {
    fn from(err: RuntimeError<'a>) -> Self {
        Self::Runtime(err)
    }
}

#[derive(Debug, thiserror::Error, miette::Diagnostic, Clone)]
#[error("{kind}")]
pub struct RuntimeError<'a> {
    #[source_code]
    pub src: Cow<'a, str>,
    #[label("here")]
    pub span: Span,
    pub kind: RuntimeErrorKind,
}

#[derive(Debug, Clone)]
pub enum RuntimeErrorKind {
    InvalidOperand { expected: &'static str },

    InvalidOperands { expected: &'static str },
    MissingOperand { expected: &'static str },
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

            RuntimeErrorKind::MissingOperand { expected } => {
                write!(f, "missing operand, expected {}", expected)
            }
        }
    }
}
