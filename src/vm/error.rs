use core::fmt;
use std::borrow::Cow;

#[derive(Debug, thiserror::Error, miette::Diagnostic, Clone)]
#[error(transparent)]
#[diagnostic(transparent)]
pub struct RuntimeError(Box<RuntimeErrorCore>);

impl RuntimeError {
    pub fn new(kind: RuntimeErrorKind, span: Option<miette::SourceSpan>) -> Self {
        Self(Box::new(RuntimeErrorCore { kind, span }))
    }
}

#[derive(Debug, thiserror::Error, miette::Diagnostic, Clone)]
#[error("{kind}")]
pub struct RuntimeErrorCore {
    #[label("here")]
    pub span: Option<miette::SourceSpan>,
    pub kind: RuntimeErrorKind,
}

#[derive(Debug, Clone)]
pub enum RuntimeErrorKind {
    InvalidOperand { expected: &'static str },

    InvalidOperands { expected: &'static str },
    UndefinedVariable { name: String },
    UndefinedProperty { name: String },
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
                write!(f, "undefined variable `{}`", name)
            }

            RuntimeErrorKind::UndefinedProperty { name } => {
                write!(f, "undefined property `{}`", name)
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
