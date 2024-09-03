use core::fmt;
use std::borrow::Cow;
use std::cell::RefCell;
use std::ops::DerefMut;

use crate::common::Span;
use crate::lexer::{LexerError, Token};

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("Static errors")]
pub struct StaticErrors<'b> {
    #[source_code]
    src: &'b str,
    #[related]
    errors: Vec<StaticError<'b>>,
}

impl<'b> StaticErrors<'b> {
    pub fn new(src: &'b str) -> Self {
        Self {
            src,
            errors: Vec::new(),
        }
    }

    pub fn push(&mut self, error: StaticError<'b>) {
        self.errors.push(error);
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
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
    pub kind: CompileErrorKind<'a>,
}

#[derive(Debug, Clone)]
pub enum CompileErrorKind<'src> {
    UnexpectedToken {
        expected: &'static [Token<'static>],
        found: Token<'src>,
    },
}

impl fmt::Display for CompileErrorKind<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompileErrorKind::UnexpectedToken { expected, found } => {
                assert!(!expected.is_empty(), "expected at least one expected token");
                let mut expected = expected.iter().map(|tok| match tok {
                    Token::Eof => "eof",
                    Token::Number { .. } => "number",
                    Token::String { .. } => "string",
                    Token::Ident { .. } => "identifier",
                    _ => tok.lexeme(),
                });

                match expected.len() {
                    1 => write!(
                        f,
                        "expected {:?}, found '{}'",
                        expected.next().unwrap(),
                        found.lexeme()
                    ),
                    _ => write!(
                        f,
                        "expected one of the following: '{}', found '{}'",
                        DisplayIterAsSeparatedList::new(expected, ", "),
                        found.lexeme()
                    ),
                }
            }
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
            expected: &[Token::Eof],
            found: Token::Semicolon,
        };

        let err = CompileError {
            span: Span::from_len(0, 1, 1),
            kind,
        };

        Err(miette::Error::from(err).with_source_code("1.2;"))
    }

    #[test]
    fn unexpected_tokens() -> miette::Result<()> {
        let kind = CompileErrorKind::UnexpectedToken {
            expected: &[
                Token::Number {
                    lexeme: "1.2",
                    value: 1.2,
                },
                Token::Eof,
            ],
            found: Token::Semicolon,
        };

        let err = CompileError {
            span: Span::from_len(0, 1, 1),
            kind,
        };

        Err(miette::Error::from(err).with_source_code("1.2;"))
    }
}
