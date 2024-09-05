use crate::bytecode::{ByteCode, Instruction};
use crate::common::{Span, Spanned};
use crate::lexer::{Keyword, Lexer, PeekableLexer, Token, TokenKind};
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

    pub locals: Vec<Local>,
    pub scope_depth: usize,
}

impl<'a> Compiler<'a> {
    pub fn from_str(source: &'a str) -> Self {
        Self {
            source,
            lexer: PeekableLexer::new(Lexer::new(source)),
            bytecode: ByteCode::new(),
            errors: StaticErrors::new(source),

            locals: Vec::new(),
            scope_depth: 0,
        }
    }

    pub fn compile(mut self) -> Result<ByteCode, StaticErrors<'a>> {
        loop {
            match self.lexer.peek() {
                Ok(Some(tok)) if **tok == Token::Eof => break,
                Ok(None) => unreachable!(), // Token::Eof should be returned before end, thus None can never happen here
                Ok(Some(_)) => match self.compile_declaration() {
                    Ok(()) => (),
                    Err(err) => self.errors.push(err),
                },
                Err(err) => {
                    self.errors.push(err.into());
                    self.synchronize();
                }
            }
        }

        match self.consume(|token| matches!(token, Token::Eof), || &[TokenKind::Eof]) {
            Ok(_) => (),
            Err(err) => self.errors.push(err),
        };

        self.emit(
            Instruction::Return,
            Span::from_len(0, self.source.len().saturating_sub(1), 0),
        );

        if !self.errors.is_empty() {
            Err(self.errors)
        } else {
            Ok(self.bytecode)
        }
    }

    fn compile_declaration(&mut self) -> Result<(), StaticError<'a>> {
        let result = match self
            .lexer
            .next_if(|tok| matches!(tok, Token::Keyword(Keyword::Var)))?
        {
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

        if let Some(eq) = self.lexer.next_if(|tok| matches!(tok, Token::Eq))? {
            self.compile_expr()?;
        } else {
            self.emit(Instruction::Nil, var_kw.span.combine(&const_idx.span))
        }

        self.consume(
            |token| matches!(token, Token::Semicolon),
            || &[TokenKind::Semicolon],
        )?;

        self.compile_define_variable(const_idx);

        Ok(())
    }

    fn compile_variable_name(&mut self) -> Result<Spanned<u8>, StaticError<'a>> {
        let ident = self.consume(
            |tok| matches!(tok, Token::Ident { .. }),
            || &[TokenKind::Ident],
        )?;

        self.declare_variable(
            ident.map(|ident| ident.clone().try_into_ident().unwrap().to_string()),
        )?;
        if self.scope_depth > 0 {
            return Ok(ident.map_into(|_| 0));
        }

        match ident.item {
            Token::Ident(s) => self.compile_ident_constant(s, ident.span),
            _ => unreachable!(),
        }
    }

    fn compile_define_variable(&mut self, name: Spanned<u8>) {
        if self.scope_depth > 0 {
            return;
        }

        self.emit(Instruction::DefineGlobal(name.item), name.span);
    }

    fn declare_variable(&mut self, ident: Spanned<String>) -> Result<(), StaticError<'a>> {
        if self.scope_depth == 0 {
            return Ok(());
        }

        if self.locals.len() == u8::MAX as usize {
            todo!("Support more than 255 locals")
        }

        for local in self.locals.iter().rev() {
            if local.depth < self.scope_depth {
                break;
            }
            if local.ident.item == ident.item {
                let kind =
                    CompileErrorKind::ReassignmentOfVariableInLocalScope { ident: ident.item };
                let err = CompileError::new(kind, ident.span);
                return Err(err.into());
            }
        }

        let local = Local {
            ident,
            depth: self.scope_depth,
        };
        self.locals.push(local);

        Ok(())
    }

    fn compile_ident_constant(
        &mut self,
        ident: &str,
        span: Span,
    ) -> Result<Spanned<u8>, StaticError<'a>> {
        self.compile_constant(
            Value::new_object(Object::String(InternedString::new(ident.to_string()))),
            span.clone(),
        )
        .map(|idx| Spanned::new(idx, span))
    }

    fn synchronize(&mut self) {
        loop {
            match self.lexer.peek() {
                Ok(Some(tok)) => match tok.item {
                    Token::Semicolon => {
                        // Semicolon end a statement, so next token is a new statement and we can try parsing again
                        self.lexer.next().expect("we just checked that it's ok");
                        break;
                    }
                    Token::Eof // Don't consume EOF, outer loop expects to find one after all the statements have been parsed
                    | Token::Keyword( // following keywords start a new block/section and we can try parsing again
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
                        // consume all tokens before next synchronization point
                         let _ = self.lexer.next();
                    }
                },
                Ok(None) => unreachable!(
                    "Token::Eof should be returned before end, thus None can never happen here"
                ),
                Err(_) => {
                    // ignore following error before synchronization point
                    let _ = self.lexer.next();
                }
            }
        }
    }

    fn compile_stmt(&mut self) -> Result<(), StaticError<'a>> {
        let Some(tok) = self
            .lexer
            .next_if(|tok| matches!(tok, Token::Keyword(Keyword::Print) | Token::LBrace))?
        else {
            return self.compile_expr_stmt();
        };

        match tok.item {
            Token::Keyword(Keyword::Print) => self.compile_print_stmt(tok),
            Token::LBrace => self.compile_block_stmt(tok),
            _ => unreachable!(),
        }
    }

    fn compile_block_stmt(&mut self, lbrace: Spanned<Token<'a>>) -> Result<(), StaticError<'a>> {
        self.enter_scope();
        self.compile_block()?;
        self.exit_scope();
        Ok(())
    }

    fn compile_block(&mut self) -> Result<(), StaticError<'a>> {
        while self
            .lexer
            .is_next(|tok| !matches!(tok, Token::RBrace | Token::Eof))?
        {
            self.compile_declaration()?;
        }

        self.consume(|tok| matches!(tok, Token::RBrace), || &[TokenKind::RBrace])?;
        Ok(())
    }

    fn enter_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn exit_scope(&mut self) {
        while self.locals.last().map(|l| l.depth) == Some(self.scope_depth) {
            self.locals.pop();

            self.emit(Instruction::Pop, Span::new(0, 0, 0));
        }

        self.scope_depth -= 1;
    }

    fn compile_expr_stmt(&mut self) -> Result<(), StaticError<'a>> {
        self.compile_expr()?;
        let semicolon = self.consume(
            |tok| matches!(tok, Token::Semicolon),
            || &[TokenKind::Semicolon],
        )?;
        self.emit(Instruction::Pop, semicolon.span);
        Ok(())
    }

    fn compile_print_stmt(&mut self, print: Spanned<Token<'a>>) -> Result<(), StaticError<'a>> {
        self.compile_expr()?;
        self.consume(
            |tok| matches!(tok, Token::Semicolon),
            || &[TokenKind::Semicolon],
        )?;

        self.emit(Instruction::Print, print.span);
        Ok(())
    }

    fn emit(&mut self, instruction: Instruction, span: Span) {
        self.bytecode.push(instruction, span);
    }

    pub fn consume(
        &mut self,
        predicate: impl Fn(&Token<'_>) -> bool,
        expected_tokens: impl Fn() -> &'static [TokenKind],
    ) -> Result<Spanned<Token<'a>>, StaticError<'a>> {
        match self.lexer.next()? {
            Some(token) if predicate(&token) => Ok(token),

            // Handle errors
            Some(token) => {
                // Unexpected token
                let kind = CompileErrorKind::UnexpectedToken {
                    expected: expected_tokens(),
                    found: token.item.as_kind(),
                };
                let err = CompileError::new(kind, token.span);

                Err(err.into())
            }
            None => {
                // Unexpected EOF
                let kind = CompileErrorKind::UnexpectedToken {
                    expected: expected_tokens(),
                    found: TokenKind::Eof,
                };
                let err = CompileError::new(
                    kind,
                    Span::from_len(self.lexer.line(), self.source.len().saturating_sub(1), 0),
                );

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

    fn compile_grouping(
        &mut self,
        lparen: Spanned<Token<'a>>,
        can_assign: bool,
    ) -> Result<(), StaticError<'a>> {
        self.compile_expr()?;

        self.consume(
            |token| matches!(token, Token::RParen),
            || &[TokenKind::RParen],
        )?;

        Ok(())
    }

    fn compile_unary(
        &mut self,
        operator: Spanned<Token<'a>>,
        can_assign: bool,
    ) -> Result<(), StaticError<'a>> {
        self.compile_precedence(Precedence::Unary)?; // operand

        match operator.item {
            Token::Minus => self.emit(Instruction::Negate, operator.span),
            Token::Bang => self.emit(Instruction::Not, operator.span),
            _ => unreachable!("Invalid unary operator."),
        }

        Ok(())
    }

    fn compile_binary(
        &mut self,
        operator: Spanned<Token<'a>>,
        can_assign: bool,
    ) -> Result<(), StaticError<'a>> {
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

        let can_assign = precedence <= Precedence::Assignment;
        self.compile_prefix(token, can_assign)?;

        while let Some(infix) = self
            .lexer
            .next_if(|tok| precedence <= Precedence::from_token(tok))?
        {
            self.compile_infix(infix, can_assign)?;
        }

        if can_assign {
            if let Some(tok) = self.lexer.next_if(|token| matches!(token, Token::Eq))? {
                let kind = CompileErrorKind::InvalidAssignmentTarget;
                let err = CompileError::new(kind, tok.span);
                return Err(err.into());
            }
        }

        Ok(())
    }

    fn compile_prefix(
        &mut self,
        prefix: Spanned<Token<'a>>,
        can_assign: bool,
    ) -> Result<(), StaticError<'a>> {
        match prefix.item {
            Token::Number { value, .. } => self
                .compile_constant(Value::Number(value), prefix.span)
                .map(|_| ()),
            Token::LParen => self.compile_grouping(prefix, can_assign),
            Token::Minus | Token::Bang => self.compile_unary(prefix, can_assign),
            Token::Keyword(Keyword::Nil) => Ok(self.emit(Instruction::Nil, prefix.span)),
            Token::Keyword(Keyword::True) => Ok(self.emit(Instruction::True, prefix.span)),
            Token::Keyword(Keyword::False) => Ok(self.emit(Instruction::False, prefix.span)),
            Token::String { value, .. } => self
                .compile_constant(
                    Value::new_object(Object::String(InternedString::new(value.to_string()))),
                    prefix.span,
                )
                .map(|_| ()),
            Token::Ident(ident) => self.compile_variable(ident, prefix.span, can_assign),
            _ => unreachable!("Invalid prefix operator."),
        }
    }

    fn compile_variable(
        &mut self,
        ident: &str,
        span: Span,
        can_assign: bool,
    ) -> Result<(), StaticError<'a>> {
        self.compile_named_variable(ident, span, can_assign)
    }

    fn compile_named_variable(
        &mut self,
        ident: &str,
        span: Span,
        can_assign: bool,
    ) -> Result<(), StaticError<'a>> {
        let idx = self.compile_ident_constant(ident, span)?;

        if can_assign {
            if let Some(tok) = self.lexer.next_if(|tok| matches!(tok, Token::Eq))? {
                self.compile_expr()?;
                self.emit(Instruction::SetGlobal(idx.item), idx.span);

                return Ok(());
            }
        }

        self.emit(Instruction::GetGlobal(idx.item), idx.span);

        Ok(())
    }

    fn has_prefix_rule(token: &Token<'_>) -> bool {
        matches!(
            token,
            Token::Number { .. }
                | Token::LParen
                | Token::Minus
                | Token::Bang
                | Token::String { .. }
                | Token::Ident(_)
                | Token::Keyword(Keyword::Nil | Keyword::True | Keyword::False)
        )
    }

    const fn prefix_tokens() -> &'static [TokenKind] {
        &[
            TokenKind::Number,
            TokenKind::LParen,
            TokenKind::Minus,
            TokenKind::Bang,
            TokenKind::String,
            TokenKind::Ident,
            TokenKind::Keyword(Keyword::Nil),
            TokenKind::Keyword(Keyword::True),
            TokenKind::Keyword(Keyword::False),
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

    fn compile_infix(
        &mut self,
        infix: Spanned<Token<'a>>,
        can_assign: bool,
    ) -> Result<(), StaticError<'a>> {
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
            | Token::LtEq => self.compile_binary(infix, can_assign),
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

#[derive(Debug, Clone, PartialEq)]
pub struct Local {
    pub ident: Spanned<String>,
    pub depth: usize,
}
