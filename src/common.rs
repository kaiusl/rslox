use std::{fmt, ops};

#[derive(Debug, PartialEq, Clone)]
pub struct Span {
    pub line: usize,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(line: usize, start: usize, end: usize) -> Self {
        Self { line, start, end }
    }

    pub fn from_len(line: usize, start: usize, len: usize) -> Self {
        Self::new(line, start, start + len)
    }

    pub fn combine(&self, other: &Self) -> Self {
        Self::new(
            usize::min(self.line, other.line), // start line number
            usize::min(self.start, other.start),
            usize::max(self.end, other.end),
        )
    }
}

impl From<Span> for miette::SourceSpan {
    fn from(span: Span) -> Self {
        miette::SourceSpan::from(span.start..span.end)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Spanned<T> {
    pub item: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(item: T, span: Span) -> Self {
        Self { item, span }
    }

    pub fn map<U>(&self, f: impl Fn(&T) -> U) -> Spanned<U> {
        Spanned::new(f(&self.item), self.span.clone())
    }

    pub fn map_into<U>(self, f: impl Fn(T) -> U) -> Spanned<U> {
        Spanned::new(f(self.item), self.span)
    }
}

impl<T> ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.item
    }
}

impl<T> ops::DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.item
    }
}

impl<T> fmt::Display for Spanned<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.item)
    }
}
