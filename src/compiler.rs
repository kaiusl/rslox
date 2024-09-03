use crate::bytecode::{ByteCode, Instruction};
use crate::common::{Span, Spanned};
use crate::lexer::{Keyword, Lexer, PeekableLexer, Token};
use crate::value::{InternedString, Object, Value};

use num_traits::FromPrimitive;

pub mod error;
use error::StaticError;

use self::error::{CompileError, CompileErrorKind, StaticErrors};

pub struct Compiler<'a> {
    pub source: &'a str,
    pub lexer: PeekableLexer<'a>,
    pub bytecode: ByteCode,
    pub errors: StaticErrors<'a>,
}

impl<'a> Compiler<'a> {
    pub fn from_str(source: &'a str) -> Self {
        Self {
            source,
            lexer: PeekableLexer::new(Lexer::new(source)),
            bytecode: ByteCode::new(),
            errors: StaticErrors::new(source),
        }
    }

    pub fn compile(mut self) -> Result<ByteCode, StaticErrors<'a>> {
        while self.lexer.is_next(|tok| !matches!(tok, Token::Eof)) {
            match self.compile_declaration() {
                Ok(()) => (),
                Err(err) => self.errors.push(err),
            }
        }

        match self.consume(|token| matches!(token, Token::Eof), || &[Token::Eof]) {
            Ok(_) => (),
            Err(err) => self.errors.push(err),
        };

        self.emit(
            Instruction::Return,
            Span::from_len(0, self.source.len() - 1, 1),
        );

        if !self.errors.is_empty() {
            Err(self.errors)
        } else {
            Ok(self.bytecode)
        }
    }

    fn compile_declaration(&mut self) -> Result<(), StaticError<'a>> {
        let result = match self.consume_if(|tok| matches!(tok, Token::Keyword(Keyword::Var)))? {
            Some(tok) => self.compile_var_decl(tok),
            None => self.compile_stmt(),
        };

        match result {
            Ok(()) => Ok(()),
            Err(err) => {
                self.synchronize();
                Err(err)
            }
        }
    }

    fn compile_var_decl(&mut self, var_kw: Spanned<Token<'a>>) -> Result<(), StaticError<'a>> {
        let const_idx = self.compile_variable_name()?;

        if let Some(eq) = self.consume_if(|tok| matches!(tok, Token::Eq))? {
            self.compile_expr()?;
        } else {
            self.emit(Instruction::Nil, var_kw.span.combine(&const_idx.span))
        }

        self.consume(
            |token| matches!(token, Token::Semicolon),
            || &[Token::Semicolon],
        )?;

        self.compile_define_variable(const_idx);

        Ok(())
    }

    fn compile_define_variable(&mut self, name: Spanned<u8>) {
        self.emit(Instruction::DefineGlobal(name.item), name.span);
    }

    fn compile_variable_name(&mut self) -> Result<Spanned<u8>, StaticError<'a>> {
        let ident = self.consume(
            |tok| matches!(tok, Token::Ident { .. }),
            || &[Token::Ident("")],
        )?;

        match ident.item {
            Token::Ident(s) => {
                let result = self.compile_constant(
                    Value::new_object(Object::String(InternedString::new(s.to_string()))),
                    ident.span.clone(),
                )?;

                Ok(Spanned::new(result, ident.span))
            }
            _ => unreachable!(),
        }
    }

    fn synchronize(&mut self) {
        loop {
            match self.lexer.peek() {
                Some(Ok(tok)) => match tok.item {
                    Token::Eof | Token::Semicolon => {
                        self.lexer.next();
                        break;
                    }
                    Token::Keyword(
                        Keyword::Class
                        | Keyword::Fun
                        | Keyword::Var
                        | Keyword::For
                        | Keyword::If
                        | Keyword::While
                        | Keyword::Print
                        | Keyword::Return,
                    ) => break,
                    _ => {
                        self.lexer.next();
                    }
                },
                None => break,
                Some(Err(_)) => {
                    // ignore following error before synchronization point
                    self.lexer.next();
                }
            }
        }
    }

    fn compile_stmt(&mut self) -> Result<(), StaticError<'a>> {
        let tok = match self.lexer.peek() {
            Some(Ok(tok)) => tok,
            _ => todo!(),
        };

        let Some(tok) = self.consume_if(|tok| matches!(tok, Token::Keyword(Keyword::Print)))?
        else {
            return self.compile_expr_stmt();
        };

        match tok.item {
            Token::Keyword(Keyword::Print) => self.compile_print_stmt(tok),
            _ => unreachable!(),
        }
    }

    fn compile_expr_stmt(&mut self) -> Result<(), StaticError<'a>> {
        self.compile_expr()?;
        let semicolon = self.consume(
            |tok| matches!(tok, Token::Semicolon),
            || &[Token::Semicolon],
        )?;
        self.emit(Instruction::Pop, semicolon.span);
        Ok(())
    }

    fn compile_print_stmt(&mut self, print: Spanned<Token<'a>>) -> Result<(), StaticError<'a>> {
        self.compile_expr()?;
        self.consume(
            |tok| matches!(tok, Token::Semicolon),
            || &[Token::Semicolon],
        )?;

        self.emit(Instruction::Print, print.span);
        Ok(())
    }

    fn emit(&mut self, instruction: Instruction, span: Span) {
        self.bytecode.push(instruction, span);
    }

    pub fn consume_if(
        &mut self,
        predicate: impl Fn(&Token<'_>) -> bool,
    ) -> Result<Option<Spanned<Token<'a>>>, StaticError<'a>> {
        match self.lexer.peek() {
            Some(Ok(token)) if predicate(token) => Ok(Some(self.lexer.next().unwrap().unwrap())),
            Some(Ok(_)) => Ok(None),
            Some(Err(_)) => Err(self.lexer.next().unwrap().unwrap_err().into()),
            None => Ok(None),
        }
    }

    pub fn consume(
        &mut self,
        predicate: impl Fn(&Token<'_>) -> bool,
        expected_tokens: impl Fn() -> &'static [Token<'static>],
    ) -> Result<Spanned<Token<'a>>, StaticError<'a>> {
        match self.lexer.next() {
            Some(Ok(token)) if predicate(&token) => Ok(token),

            // Handle errors
            Some(Ok(token)) => {
                // Unexpected token
                let kind = CompileErrorKind::UnexpectedToken {
                    expected: expected_tokens(),
                    found: token.item,
                };
                let err = CompileError {
                    span: token.span,
                    kind,
                };

                Err(err.into())
            }
            Some(Err(err)) => Err(err.into()), // Lexer error
            None => {
                // Unexpected EOF
                let kind = CompileErrorKind::UnexpectedToken {
                    expected: expected_tokens(),
                    found: Token::Eof,
                };
                let err = CompileError {
                    span: Span::from_len(self.lexer.line(), self.source.len() - 1, 1),
                    kind,
                };

                Err(err.into())
            }
        }
    }

    pub fn compile_expr(&mut self) -> Result<(), StaticError<'a>> {
        self.compile_precedence(Precedence::Assignment)
    }

    fn compile_constant(&mut self, value: Value, span: Span) -> Result<u8, StaticError<'a>> {
        let idx = self.bytecode.add_constant(value);
        let Ok(idx) = u8::try_from(idx) else {
            todo!("Too many constants. Add another op to support more constants.");
        };

        self.emit(Instruction::Constant(idx), span);

        Ok(idx)
    }

    fn compile_grouping(&mut self, lparen: Spanned<Token<'a>>) -> Result<(), StaticError<'a>> {
        self.compile_expr()?;

        self.consume(|token| matches!(token, Token::RParen), || &[Token::RParen])?;

        Ok(())
    }

    fn compile_unary(&mut self, operator: Spanned<Token<'a>>) -> Result<(), StaticError<'a>> {
        self.compile_precedence(Precedence::Unary)?; // operand

        match operator.item {
            Token::Minus => self.emit(Instruction::Negate, operator.span),
            Token::Bang => self.emit(Instruction::Not, operator.span),
            _ => unreachable!("Invalid unary operator."),
        }

        Ok(())
    }

    fn compile_binary(&mut self, operator: Spanned<Token<'a>>) -> Result<(), StaticError<'a>> {
        let precedence = Precedence::from_token(&operator);

        self.compile_precedence(precedence.next().unwrap())?;

        match operator.item {
            Token::Plus => self.emit(Instruction::Add, operator.span),
            Token::Minus => self.emit(Instruction::Subtract, operator.span),
            Token::Star => self.emit(Instruction::Multiply, operator.span),
            Token::Slash => self.emit(Instruction::Divide, operator.span),
            Token::EqEq => self.emit(Instruction::Eq, operator.span),
            Token::BangEq => {
                self.emit(Instruction::Eq, operator.span.clone());
                self.emit(Instruction::Not, operator.span);
            }
            Token::Lt => self.emit(Instruction::Lt, operator.span),
            Token::LtEq => {
                self.emit(Instruction::Gt, operator.span.clone());
                self.emit(Instruction::Not, operator.span);
            }
            Token::Gt => self.emit(Instruction::Gt, operator.span),
            Token::GtEq => {
                self.emit(Instruction::Lt, operator.span.clone());
                self.emit(Instruction::Not, operator.span);
            }
            _ => unreachable!("Invalid binary operator."),
        }

        Ok(())
    }

    fn compile_precedence(&mut self, precedence: Precedence) -> Result<(), StaticError<'a>> {
        let token = self.consume(Self::has_prefix_rule, Self::prefix_tokens)?;

        self.compile_prefix(token)?;

        while let Some(Ok(token)) = self.lexer.peek() {
            if precedence > Precedence::from_token(token) {
                break;
            }
            let Some(Ok(infix)) = self.lexer.next() else {
                unreachable!()
            };

            self.compile_infix(infix)?;
        }

        Ok(())
    }

    fn compile_prefix(&mut self, prefix: Spanned<Token<'a>>) -> Result<(), StaticError<'a>> {
        match prefix.item {
            Token::Number { value, .. } => self
                .compile_constant(Value::Number(value), prefix.span)
                .map(|_| ()),
            Token::LParen => self.compile_grouping(prefix),
            Token::Minus | Token::Bang => self.compile_unary(prefix),
            Token::Keyword(Keyword::Nil) => Ok(self.emit(Instruction::Nil, prefix.span)),
            Token::Keyword(Keyword::True) => Ok(self.emit(Instruction::True, prefix.span)),
            Token::Keyword(Keyword::False) => Ok(self.emit(Instruction::False, prefix.span)),
            Token::String { value, .. } => self
                .compile_constant(
                    Value::new_object(Object::String(InternedString::new(value.to_string()))),
                    prefix.span,
                )
                .map(|_| ()),
            _ => unreachable!("Invalid prefix operator."),
        }
    }

    fn has_prefix_rule(token: &Token<'_>) -> bool {
        matches!(
            token,
            Token::Number { .. }
                | Token::LParen
                | Token::Minus
                | Token::Bang
                | Token::String { .. }
                | Token::Keyword(Keyword::Nil | Keyword::True | Keyword::False)
        )
    }

    const fn prefix_tokens() -> &'static [Token<'static>] {
        &[
            Token::Number {
                value: 0.0,
                lexeme: "0.0",
            },
            Token::LParen,
            Token::Minus,
            Token::Bang,
            Token::String {
                lexeme: "",
                value: "",
            },
            Token::Keyword(Keyword::Nil),
            Token::Keyword(Keyword::True),
            Token::Keyword(Keyword::False),
        ]
    }

    const fn any_token() -> &'static [Token<'static>] {
        &[
            Token::Number {
                value: 0.0,
                lexeme: "0.0",
            },
            Token::LParen,
            Token::Minus,
            Token::Bang,
            Token::Plus,
            Token::Slash,
            Token::Star,
            Token::Keyword(Keyword::Nil),
            Token::Keyword(Keyword::True),
            Token::Keyword(Keyword::False),
        ]
    }

    fn compile_infix(&mut self, infix: Spanned<Token<'a>>) -> Result<(), StaticError<'a>> {
        match infix.item {
            Token::Plus
            | Token::Minus
            | Token::Star
            | Token::Slash
            | Token::EqEq
            | Token::BangEq
            | Token::Gt
            | Token::GtEq
            | Token::Lt
            | Token::LtEq => self.compile_binary(infix),
            _ => unreachable!("Invalid infix operator."),
        }
    }
}

#[derive(Debug, Clone, Copy, num_derive::FromPrimitive, PartialEq, PartialOrd, Eq, Ord)]
enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    fn next(self) -> Option<Self> {
        Self::from_u8(self as u8 + 1)
    }

    fn from_token(token: &Token<'_>) -> Self {
        match token {
            Token::Plus | Token::Minus => Self::Term,
            Token::Star | Token::Slash => Self::Factor,
            Token::BangEq | Token::EqEq => Precedence::Equality,
            Token::Lt | Token::LtEq | Token::Gt | Token::GtEq => Precedence::Comparison,

            _ => Self::None,
        }
    }
}
