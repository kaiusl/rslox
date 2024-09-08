use std::borrow::Cow;
use std::str::FromStr;
use std::{fmt, ops};

mod error;
#[cfg(test)]
mod tests;

pub use error::*;

use crate::common::{Span, Spanned};

pub struct PeekableLexer<'a> {
    lexer: Lexer<'a>,
    peeked: Option<Result<Option<Spanned<Token<'a>>>, LexerError<'a>>>,
    line: usize,
}

impl<'a> PeekableLexer<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
            peeked: None,
            line: 0,
        }
    }

    pub fn peek(&mut self) -> Result<&Option<Spanned<Token<'a>>>, LexerError<'a>> {
        if self.peeked.is_none() {
            self.peeked = Some(self.lexer.next());
        }
        match self.peeked {
            Some(Ok(ref res)) => Ok(res),
            Some(Err(ref e)) => Err(e.clone()),
            None => unreachable!("peeked should be set"),
        }
    }

    pub fn next_if<F>(&mut self, f: F) -> Result<Option<Spanned<Token<'a>>>, LexerError<'a>>
    where
        F: Fn(&Token<'a>) -> bool,
    {
        match self.next()? {
            Some(t) if f(&t) => Ok(Some(t)),
            Some(t) => {
                self.peeked = Some(Ok(Some(t)));
                Ok(None)
            }
            None => Ok(None),
        }
    }

    pub fn is_next<F>(&mut self, f: F) -> Result<bool, LexerError<'a>>
    where
        F: Fn(&Token<'a>) -> bool,
    {
        match self.peek() {
            Ok(Some(t)) => Ok(f(t)),
            Ok(None) => Ok(false),
            Err(e) => Err(e.clone()),
        }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn next(&mut self) -> Result<Option<Spanned<Token<'a>>>, LexerError<'a>> {
        let result = if self.peeked.is_some() {
            self.peeked.take().unwrap()
        } else {
            self.lexer.next()
        };
        self.line = self.lexer.line();

        result
    }
}

#[derive(Debug, Clone)]
pub struct Chars<'a> {
    input: &'a str,
    inner: std::iter::Peekable<std::str::Chars<'a>>,
    byte_index: usize,
}

impl<'a> Chars<'a> {
    pub fn new(input: &'a str) -> Self {
        Chars {
            input,
            inner: input.chars().peekable(),
            byte_index: 0,
        }
    }

    #[inline]
    fn next_if_eq(&mut self, c: char) -> Option<(usize, char)> {
        self.next_if(|next| c == next)
    }

    #[inline]
    fn next_if_not_eq(&mut self, c: char) -> Option<(usize, char)> {
        self.next_if(|next| c != next)
    }

    #[inline]
    fn peek(&mut self) -> Option<(usize, &char)> {
        self.inner.peek().map(|c| (self.byte_index, c))
    }

    #[inline]
    fn peek_if_eq(&mut self, c: char) -> Option<(usize, &char)> {
        self.peek().filter(|(_, next)| c == **next)
    }

    #[inline]
    fn next_if<F>(&mut self, f: F) -> Option<(usize, char)>
    where
        F: Fn(char) -> bool,
    {
        self.inner.next_if(|next| f(*next)).map(|c| {
            let result = (self.byte_index, c);
            self.byte_index += c.len_utf8();
            result
        })
    }
}

impl<'a> Iterator for Chars<'a> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|c| {
            let result = (self.byte_index, c);
            self.byte_index += c.len_utf8();
            result
        })
    }
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    input: &'a str,
    chars: Chars<'a>,
    line: usize,
    eof: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input,
            chars: Chars::new(input),
            line: 1,
            eof: false,
        }
    }

    pub fn input(&self) -> &'a str {
        self.input
    }

    pub fn line(&self) -> usize {
        self.line
    }

    fn next(&mut self) -> Result<Option<Spanned<Token<'a>>>, LexerError<'a>> {
        loop {
            let Some((i, c)) = self.chars.next() else {
                if self.eof {
                    return Ok(None);
                } else {
                    self.eof = true;
                    let span = Span::from_len(self.line, self.input.len(), 1);
                    return Ok(Some(Spanned::new(Token::Eof, span)));
                }
            };

            let start = i;
            let token = match c {
                '(' => Token::LParen,
                ')' => Token::RParen,
                '{' => Token::LBrace,
                '}' => Token::RBrace,
                ',' => Token::Comma,
                '.' => Token::Dot,
                '-' => Token::Minus,
                '+' => Token::Plus,
                ';' => Token::Semicolon,
                '*' => Token::Star,
                '!' if self.chars.next_if_eq('=').is_some() => Token::BangEq,
                '!' => Token::Bang,
                '=' if self.chars.next_if_eq('=').is_some() => Token::EqEq,
                '=' => Token::Eq,
                '<' if self.chars.next_if_eq('=').is_some() => Token::LtEq,
                '<' => Token::Lt,
                '>' if self.chars.next_if_eq('=').is_some() => Token::GtEq,
                '>' => Token::Gt,
                '/' if self.chars.next_if_eq('/').is_some() => {
                    // comment, ignore until end of line
                    while let Some((_, _)) = self.chars.next_if_not_eq('\n') {}
                    // Next character should be 'newline' or we reached the end of input
                    assert!(
                        self.chars.peek().map(|(_, c)| c) == Some(&'\n')
                            || self.chars.peek().is_none()
                    );

                    continue;
                }
                '/' => Token::Slash,
                '"' => {
                    let start = i;
                    let mut end = i;
                    // TODO: support escape sequences
                    while let Some((i, c)) = self.chars.next_if_not_eq('"') {
                        // i is the index to the first byte of character,
                        // + c.len_utf8(), is the index to the first byte of next char
                        // hence `end` is the index to the last byte of `c`
                        end = i + c.len_utf8() - 1;
                        if c == '\n' {
                            self.line += 1;
                        }
                    }

                    if self.chars.peek().is_none() {
                        let err = UnterminatedStringError::new(
                            self.input,
                            self.line,
                            (i..end + 1).into(),
                        );
                        return Err(LexerError::UnterminatedString(err));
                    }

                    assert!(self.chars.next_if_eq('"').is_some());

                    let lexeme = &self.input[start..=end + 1];
                    let value = &self.input[start + 1..=end];

                    Token::String { lexeme, value }
                }
                '\n' => {
                    self.line += 1;
                    continue;
                }
                ' ' | '\r' | '\t' => continue,
                c if c.is_ascii_digit() => {
                    let start = i;
                    let mut end = i;
                    while let Some((i, _)) = self.chars.next_if(|c| c.is_ascii_digit()) {
                        end = i;
                    }

                    'decimal: {
                        let Some((i, _)) = self.chars.peek_if_eq('.') else {
                            break 'decimal;
                        };

                        let Some(c) = self.input.get(i + 1..).and_then(|s| s.chars().next()) else {
                            break 'decimal;
                        };

                        if !c.is_ascii_digit() {
                            break 'decimal;
                        }

                        // floating point
                        self.chars.next(); // consume .
                        while let Some((i, _)) = self.chars.next_if(|c| c.is_ascii_digit()) {
                            end = i;
                        }
                    }

                    let lexeme = &self.input[start..=end];

                    Token::Number {
                        lexeme,
                        value: lexeme.parse().unwrap(),
                    }
                }
                c if c.is_ascii_alphabetic() || c == '_' => {
                    let start = i;
                    let mut end = i;
                    while let Some((i, _)) = self
                        .chars
                        .next_if(|c| c.is_ascii_alphanumeric() || c == '_')
                    {
                        end = i;
                    }

                    let ident = &self.input[start..=end];
                    if let Ok(kw) = Keyword::from_str(ident) {
                        Token::Keyword(kw)
                    } else {
                        Token::Ident(ident)
                    }
                }
                _ => {
                    let err =
                        UnexpectedCharacterError::new(self.input, self.line, c, (i, 1).into());
                    return Err(LexerError::UnknownToken(err));
                }
            };

            let span = if token.is_eof() {
                Span::from_len(self.line, start.saturating_sub(2), 1)
            } else {
                Span::from_len(self.line, start, token.lexeme().len())
            };
            return Ok(Some(Spanned::new(token, span)));
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token<'a> {
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,
    Slash,
    Eq,
    EqEq,
    Bang,
    BangEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    // Literals
    String { lexeme: &'a str, value: &'a str },
    Number { lexeme: &'a str, value: f64 },
    Ident(&'a str),
    Keyword(Keyword),
    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,
    Slash,
    Eq,
    EqEq,
    Bang,
    BangEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    // Literals
    String,
    Number,
    Ident,
    Keyword(Keyword),
    Eof,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let rep = match self {
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LBrace => "{",
            TokenKind::RBrace => "}",
            TokenKind::Comma => ",",
            TokenKind::Dot => ".",
            TokenKind::Minus => "-",
            TokenKind::Plus => "+",
            TokenKind::Semicolon => ";",
            TokenKind::Star => "*",
            TokenKind::Slash => "/",
            TokenKind::Eq => "=",
            TokenKind::EqEq => "==",
            TokenKind::Bang => "!",
            TokenKind::BangEq => "!=",
            TokenKind::Lt => "<",
            TokenKind::LtEq => "<=",
            TokenKind::Gt => ">",
            TokenKind::GtEq => ">=",
            TokenKind::String => "string",
            TokenKind::Number => "number",
            TokenKind::Ident => "ident",
            TokenKind::Keyword(kw) => kw.lexeme(),
            TokenKind::Eof => "eof",
        };

        write!(f, "{}", rep)
    }
}

impl<'a> Token<'a> {
    pub const fn as_kind(&self) -> TokenKind {
        match self {
            Token::LParen => TokenKind::LParen,
            Token::RParen => TokenKind::RParen,
            Token::LBrace => TokenKind::LBrace,
            Token::RBrace => TokenKind::RBrace,
            Token::Comma => TokenKind::Comma,
            Token::Dot => TokenKind::Dot,
            Token::Minus => TokenKind::Minus,
            Token::Plus => TokenKind::Plus,
            Token::Semicolon => TokenKind::Semicolon,
            Token::Star => TokenKind::Star,
            Token::Slash => TokenKind::Slash,
            Token::Eq => TokenKind::Eq,
            Token::EqEq => TokenKind::EqEq,
            Token::Bang => TokenKind::Bang,
            Token::BangEq => TokenKind::BangEq,
            Token::Lt => TokenKind::Lt,
            Token::LtEq => TokenKind::LtEq,
            Token::Gt => TokenKind::Gt,
            Token::GtEq => TokenKind::GtEq,
            Token::String { .. } => TokenKind::String,
            Token::Number { .. } => TokenKind::Number,
            Token::Ident(_) => TokenKind::Ident,
            Token::Keyword(kw) => TokenKind::Keyword(*kw),
            Token::Eof => TokenKind::Eof,
        }
    }

    /// Returns `true` if the token is [`LParen`].
    ///
    /// [`LParen`]: Token::LParen
    #[must_use]
    pub fn is_lparen(&self) -> bool {
        matches!(self, Self::LParen)
    }

    /// Returns `true` if the token is [`RParen`].
    ///
    /// [`RParen`]: Token::RParen
    #[must_use]
    pub fn is_rparen(&self) -> bool {
        matches!(self, Self::RParen)
    }

    /// Returns `true` if the token is [`RBrace`].
    ///
    /// [`RBrace`]: Token::RBrace
    #[must_use]
    pub fn is_rbrace(&self) -> bool {
        matches!(self, Self::RBrace)
    }

    /// Returns `true` if the token is [`Semicolon`].
    ///
    /// [`Semicolon`]: Token::Semicolon
    #[must_use]
    pub fn is_semicolon(&self) -> bool {
        matches!(self, Self::Semicolon)
    }

    /// Returns `true` if the token is [`Ident`].
    ///
    /// [`Ident`]: Token::Ident
    #[must_use]
    pub fn is_ident(&self) -> bool {
        matches!(self, Self::Ident(..))
    }

    /// Returns `true` if the token is [`Eof`].
    ///
    /// [`Eof`]: Token::Eof
    #[must_use]
    pub fn is_eof(&self) -> bool {
        matches!(self, Self::Eof)
    }

    /// Returns `true` if the token is [`Eq`].
    ///
    /// [`Eq`]: Token::Eq
    #[must_use]
    pub fn is_eq(&self) -> bool {
        matches!(self, Self::Eq)
    }

    #[must_use]
    pub fn is_var(&self) -> bool {
        matches!(self, Self::Keyword(Keyword::Var))
    }

    #[must_use]
    pub fn is_else(&self) -> bool {
        matches!(self, Self::Keyword(Keyword::Else))
    }

    /// Returns `true` if the token is [`LBrace`].
    ///
    /// [`LBrace`]: Token::LBrace
    #[must_use]
    pub fn is_lbrace(&self) -> bool {
        matches!(self, Self::LBrace)
    }

    /// Returns `true` if the token is [`Comma`].
    ///
    /// [`Comma`]: Token::Comma
    #[must_use]
    pub fn is_comma(&self) -> bool {
        matches!(self, Self::Comma)
    }

    /// Returns `true` if the token is [`Lt`].
    ///
    /// [`Lt`]: Token::Lt
    #[must_use]
    pub fn is_lt(&self) -> bool {
        matches!(self, Self::Lt)
    }

    /// Returns `true` if the token is [`Dot`].
    ///
    /// [`Dot`]: Token::Dot
    #[must_use]
    pub fn is_dot(&self) -> bool {
        matches!(self, Self::Dot)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

impl<'a> Token<'a> {
    fn book_type_name(&self) -> &'static str {
        match self {
            Token::LParen => "LEFT_PAREN",
            Token::RParen => "RIGHT_PAREN",
            Token::LBrace => "LEFT_BRACE",
            Token::RBrace => "RIGHT_BRACE",
            Token::Comma => "COMMA",
            Token::Dot => "DOT",
            Token::Minus => "MINUS",
            Token::Plus => "PLUS",
            Token::Semicolon => "SEMICOLON",
            Token::Star => "STAR",
            Token::Eq => "EQUAL",
            Token::EqEq => "EQUAL_EQUAL",
            Token::Bang => "BANG",
            Token::BangEq => "BANG_EQUAL",
            Token::Lt => "LESS",
            Token::LtEq => "LESS_EQUAL",
            Token::Gt => "GREATER",
            Token::GtEq => "GREATER_EQUAL",
            Token::Slash => "SLASH",
            Token::String { .. } => "STRING",
            Token::Number { .. } => "NUMBER",
            Token::Ident { .. } => "IDENTIFIER",
            Token::Keyword(k) => k.book_type_name(),
            Token::Eof => "EOF",
        }
    }

    pub fn lexeme(&self) -> &'_ str {
        match self {
            Token::LParen => "(",
            Token::RParen => ")",
            Token::LBrace => "{",
            Token::RBrace => "}",
            Token::Comma => ",",
            Token::Dot => ".",
            Token::Minus => "-",
            Token::Plus => "+",
            Token::Semicolon => ";",
            Token::Star => "*",
            Token::Slash => "/",
            Token::Eq => "=",
            Token::EqEq => "==",
            Token::Bang => "!",
            Token::BangEq => "!=",
            Token::Lt => "<",
            Token::LtEq => "<=",
            Token::Gt => ">",
            Token::GtEq => ">=",
            Token::String { lexeme, .. } | Token::Number { lexeme, .. } | Token::Ident(lexeme) => {
                lexeme
            }
            Token::Keyword(keyword) => keyword.lexeme(),

            Token::Eof => "",
        }
    }

    fn literal(&self) -> Cow<'_, str> {
        match self {
            Token::Number { value, .. } if value.fract() == 0.0 => {
                Cow::Owned(format!("{}.0", value))
            }
            Token::String { value, .. } => Cow::Borrowed(value),
            Token::Number { lexeme: value, .. } => {
                Cow::Borrowed(value.trim_end_matches('0').trim_start_matches('0'))
            }

            _ => Cow::Borrowed("null"),
        }
    }

    pub fn fmt_as_book(&self) -> BookTokenFmt<'_> {
        BookTokenFmt { token: self }
    }

    #[must_use]
    pub fn try_into_ident(self) -> Result<&'a str, Self> {
        if let Self::Ident(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }
}

impl Keyword {
    pub fn book_type_name(&self) -> &'static str {
        match self {
            Keyword::And => "AND",
            Keyword::Class => "CLASS",
            Keyword::Else => "ELSE",
            Keyword::False => "FALSE",
            Keyword::For => "FOR",
            Keyword::Fun => "FUN",
            Keyword::If => "IF",
            Keyword::Nil => "NIL",
            Keyword::Or => "OR",
            Keyword::Print => "PRINT",
            Keyword::Return => "RETURN",
            Keyword::Super => "SUPER",
            Keyword::This => "THIS",
            Keyword::True => "TRUE",
            Keyword::Var => "VAR",
            Keyword::While => "WHILE",
        }
    }

    pub fn lexeme(&self) -> &'static str {
        match self {
            Keyword::And => "and",
            Keyword::Class => "class",
            Keyword::Else => "else",
            Keyword::False => "false",
            Keyword::For => "for",
            Keyword::Fun => "fun",
            Keyword::If => "if",
            Keyword::Nil => "nil",
            Keyword::Or => "or",
            Keyword::Print => "print",
            Keyword::Return => "return",
            Keyword::Super => "super",
            Keyword::This => "this",
            Keyword::True => "true",
            Keyword::Var => "var",
            Keyword::While => "while",
        }
    }
}

impl FromStr for Keyword {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "and" => Ok(Keyword::And),
            "class" => Ok(Keyword::Class),
            "else" => Ok(Keyword::Else),
            "false" => Ok(Keyword::False),
            "for" => Ok(Keyword::For),
            "fun" => Ok(Keyword::Fun),
            "if" => Ok(Keyword::If),
            "nil" => Ok(Keyword::Nil),
            "or" => Ok(Keyword::Or),
            "print" => Ok(Keyword::Print),
            "return" => Ok(Keyword::Return),
            "super" => Ok(Keyword::Super),
            "this" => Ok(Keyword::This),
            "true" => Ok(Keyword::True),
            "var" => Ok(Keyword::Var),
            "while" => Ok(Keyword::While),
            _ => Err(()),
        }
    }
}

pub struct BookTokenFmt<'a> {
    token: &'a Token<'a>,
}

impl fmt::Display for BookTokenFmt<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.token.book_type_name(),
            self.token.lexeme(),
            self.token.literal()
        )
    }
}
