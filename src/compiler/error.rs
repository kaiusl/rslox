use core::fmt;
use std::borrow::Cow;
use std::cell::RefCell;
use std::ops::DerefMut;

use crate::common::Span;
use crate::lexer::{LexerError, Token, TokenKind};

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("Static errors")]
pub struct StaticErrors<'b> {
    #[source_code]
    src: Cow<'b, str>,
    #[related]
    errors: Vec<StaticError<'b>>,
}

impl<'b> StaticErrors<'b> {
    pub fn new(src: &'b str) -> Self {
        Self {
            src: src.into(),
            errors: Vec::new(),
        }
    }

    pub fn push(&mut self, error: StaticError<'b>) {
        self.errors.push(error);
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn to_owned(&self) -> StaticErrors<'static> {
        StaticErrors {
            src: Cow::Owned(self.src.to_string()),
            errors: self.errors.iter().map(StaticError::to_owned).collect(),
        }
    }
}

#[derive(Debug, thiserror::Error, miette::Diagnostic, Clone)]
pub enum StaticError<'a> {
    #[error(transparent)]
    #[diagnostic(transparent)]
    Lexer(LexerError<'a>),

    #[error(transparent)]
    #[diagnostic(transparent)]
    Compile(CompileError<'a>),
}

impl StaticError<'_> {
    pub fn to_owned(&self) -> StaticError<'static> {
        match self {
            StaticError::Lexer(err) => StaticError::Lexer(err.to_owned()),
            StaticError::Compile(err) => StaticError::Compile(err.to_owned()),
        }
    }
}

impl<'a> From<CompileError<'a>> for StaticError<'a> {
    fn from(err: CompileError<'a>) -> Self {
        Self::Compile(err)
    }
}

impl<'a> From<LexerError<'a>> for StaticError<'a> {
    fn from(err: LexerError<'a>) -> Self {
        Self::Lexer(err)
    }
}

#[derive(Debug, thiserror::Error, miette::Diagnostic, Clone)]
#[error("{kind}")]
pub struct CompileError<'a> {
    #[label("here")]
    pub span: Span,
    pub kind: CompileErrorKind,
    marker: std::marker::PhantomData<&'a ()>,
}

impl<'a> CompileError<'a> {
    pub fn new(kind: CompileErrorKind, span: Span) -> Self {
        Self {
            span,
            kind,
            marker: std::marker::PhantomData,
        }
    }

    pub fn to_owned(&self) -> CompileError<'static> {
        CompileError {
            span: self.span.clone(),
            kind: self.kind.to_owned(),
            marker: std::marker::PhantomData,
        }
    }
}

#[derive(Debug, Clone)]
pub enum CompileErrorKind {
    UnexpectedToken {
        expected: &'static [TokenKind],
        found: TokenKind,
    },

    InvalidAssignmentTarget,
}

impl fmt::Display for CompileErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompileErrorKind::UnexpectedToken { expected, found } => {
                assert!(!expected.is_empty(), "expected at least one expected token");
                let mut expected = expected.iter();

                match expected.len() {
                    1 => write!(
                        f,
                        "expected {:?}, found '{}'",
                        expected.next().unwrap(),
                        found
                    ),
                    _ => write!(
                        f,
                        "expected one of the following: '{}', found '{}'",
                        DisplayIterAsSeparatedList::new(expected, ", "),
                        found
                    ),
                }
            }

            CompileErrorKind::InvalidAssignmentTarget => write!(f, "invalid assignment target"),
        }
    }
}

struct DisplayIterAsSeparatedList<T> {
    iter: RefCell<T>,
    sep: &'static str,
}

impl<T> DisplayIterAsSeparatedList<T> {
    fn new(iter: T, sep: &'static str) -> Self {
        Self {
            iter: RefCell::new(iter),
            sep,
        }
    }
}

impl<T> fmt::Display for DisplayIterAsSeparatedList<T>
where
    T: Iterator,
    T::Item: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.iter.borrow_mut();

        let first = iter.next();
        if let Some(first) = first {
            write!(f, "{}", first)?;
            for item in &mut *iter {
                write!(f, "{}{}", self.sep, item)?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unexpected_token() -> miette::Result<()> {
        let kind = CompileErrorKind::UnexpectedToken {
            expected: &[TokenKind::Eof],
            found: TokenKind::Semicolon,
        };

        let err = CompileError::new(kind, Span::from_len(0, 1, 1));
        Err(miette::Error::from(err).with_source_code("1.2;"))
    }

    #[test]
    fn unexpected_tokens() -> miette::Result<()> {
        let kind = CompileErrorKind::UnexpectedToken {
            expected: &[TokenKind::Number, TokenKind::Eof],
            found: TokenKind::Semicolon,
        };

        let err = CompileError::new(kind, Span::from_len(0, 1, 1));

        Err(miette::Error::from(err).with_source_code("1.2;"))
    }
}
