use std::borrow::Cow;

#[derive(Debug, Clone, thiserror::Error, miette::Diagnostic)]
pub enum LexerError<'a> {
    #[error(transparent)]
    #[diagnostic(transparent)]
    UnknownToken(UnexpectedCharacterError<'a>),

    #[error(transparent)]
    #[diagnostic(transparent)]
    UnterminatedString(UnterminatedStringError<'a>),
}

impl<'a> LexerError<'a> {

    pub fn to_owned(&self) -> LexerError<'static> {
        match self {
            LexerError::UnknownToken(err) => LexerError::UnknownToken(err.to_owned()),
            LexerError::UnterminatedString(err) => LexerError::UnterminatedString(err.to_owned()),
        }
    }
}

#[derive(Debug, Clone, thiserror::Error, miette::Diagnostic)]
#[error("[line {line_number}] Error: Unexpected character: {token}")]
pub struct UnexpectedCharacterError<'a> {
    #[source_code]
    line: Cow<'a, str>,
    line_number: usize,
    token: char,
    #[label("here")]
    span: miette::SourceSpan,
}

impl<'a> UnexpectedCharacterError<'a> {
    pub fn new(line: &'a str, line_number: usize, token: char, span: miette::SourceSpan) -> Self {
        Self {
            line: line.into(),
            line_number,
            token,
            span,
        }
    }

    pub fn token(&self) -> char {
        self.token
    }

    pub fn to_owned(&self) -> UnexpectedCharacterError<'static> {
        UnexpectedCharacterError {
            line: Cow::Owned(self.line.to_string()),
            line_number: self.line_number,
            token: self.token,
            span: self.span,
        }
    }
}

#[derive(Debug, Clone, thiserror::Error, miette::Diagnostic)]
#[error("[line {line_number}] Error: Unterminated string.")]
pub struct UnterminatedStringError<'a> {
    #[source_code]
    line: Cow<'a, str>,
    line_number: usize,
    #[label("here")]
    span: miette::SourceSpan,
}

impl<'a> UnterminatedStringError<'a> {
    pub fn new(line: &'a str, line_number: usize, span: miette::SourceSpan) -> Self {
        Self {
            line: line.into(),
            line_number,
            span,
        }
    }

    pub fn to_owned(&self) -> UnterminatedStringError<'static> {
        UnterminatedStringError {
            line: Cow::Owned(self.line.to_string()),
            line_number: self.line_number,
            span: self.span,
        }
    }

    pub fn span(&self) -> miette::SourceSpan {
        self.span
    }
}
