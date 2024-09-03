use crate::bytecode::{ByteCode, Instruction};
use crate::common::{Span, Spanned};
use crate::lexer::{Lexer, PeekableLexer, Token};
use crate::value::Value;

use num_traits::FromPrimitive;

pub struct Compiler<'a> {
    pub source: &'a str,
    pub lexer: PeekableLexer<'a>,
    pub bytecode: ByteCode,
}

impl<'a> Compiler<'a> {
    pub fn from_str(source: &'a str) -> Self {
        Self {
            source,
            lexer: PeekableLexer::new(Lexer::new(source)),
            bytecode: ByteCode::new(),
        }
    }

    pub fn compile(mut self) -> Result<ByteCode, CompileError> {
        self.compile_expr()?;

        self.consume(
            |token| matches!(token, Token::Eof),
            "Expected end of expression.",
        )?;

        self.emit(
            Instruction::Return,
            Span::from_len(0, self.source.len() - 1, 1),
        );

        Ok(self.bytecode)
    }

    fn emit(&mut self, instruction: Instruction, span: Span) {
        self.bytecode.push(instruction, span);
    }

    pub fn consume(
        &mut self,
        predicate: impl Fn(&Token<'_>) -> bool,
        msg: &'static str,
    ) -> Result<Spanned<Token<'a>>, CompileError> {
        match self.lexer.next() {
            Some(Ok(token)) if predicate(&token) => Ok(token),
            Some(Ok(token)) => {
                todo!("Unexpected token {:?}. {}", token, msg);
                Err(CompileError {})
            },
            Some(Err(err)) => {
                todo!("Lexer error: {}", err);
                Err(CompileError {})
            }
            None => {
                todo!("Unexpected EOF.");
                Err(CompileError {})
            },
        }
    }

    pub fn compile_expr(&mut self) -> Result<(), CompileError> {
        self.compile_precedence(Precedence::Assignment)
    }

    fn compile_constant(&mut self, value: Value, span: Span) -> Result<(), CompileError> {
        let idx = self.bytecode.add_constant(value);
        let Ok(idx) = u8::try_from(idx) else {
            todo!("Too many constants.");
            return Err(CompileError {});
        };

        self.emit(Instruction::Constant(idx), span);

        Ok(())
    }

    fn compile_grouping(&mut self, lparen: Spanned<Token<'a>>) -> Result<(), CompileError> {
        self.compile_expr()?;

        self.consume(
            |token| matches!(token, Token::RParen),
            "Expected ')' after expression.",
        )?;

        Ok(())
    }

    fn compile_unary(&mut self, operator: Spanned<Token<'a>>) -> Result<(), CompileError> {
        self.compile_precedence(Precedence::Unary)?; // operand

        match operator.item {
            Token::Minus => self.emit(Instruction::Negate, operator.span),
            _ => unreachable!("Invalid unary operator."),
        }

        Ok(())
    }

    fn compile_binary(&mut self, operator: Spanned<Token<'a>>) -> Result<(), CompileError> {
        let precedence = match operator.item {
            Token::Plus => Precedence::Term,
            Token::Minus => Precedence::Term,
            Token::Star => Precedence::Factor,
            Token::Slash => Precedence::Factor,
            _ => unreachable!("Invalid binary operator."),
        };

        self.compile_precedence(precedence.next().unwrap())?;

        match operator.item {
            Token::Plus => self.emit(Instruction::Add, operator.span),
            Token::Minus => self.emit(Instruction::Subtract, operator.span),
            Token::Star => self.emit(Instruction::Multiply, operator.span),
            Token::Slash => self.emit(Instruction::Divide, operator.span),
            _ => unreachable!("Invalid binary operator."),
        }

        Ok(())
    }

    fn compile_precedence(&mut self, precedence: Precedence) -> Result<(), CompileError> {
        let Some(Ok(token)) = self.lexer.next() else {
            todo!("Invalid prefix operator (EOF).");
            return Err(CompileError {});
        };

        if !self.has_prefix_rule(&token) {
            todo!("Invalid prefix operator {:?}.", &token);
            return Err(CompileError {});
        }

        self.combile_prefix(token)?;

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

    fn combile_prefix(&mut self, prefix: Spanned<Token<'a>>) -> Result<(), CompileError> {
        match prefix.item {
            Token::Number { value, .. } => self.compile_constant(Value::Number(value), prefix.span),
            Token::LParen => self.compile_grouping(prefix),
            Token::Minus => self.compile_unary(prefix),
            _ => unreachable!("Invalid prefix operator."),
        }
    }

    fn has_prefix_rule(&self, token: &Token<'_>) -> bool {
        matches!(token, Token::Number { .. } | Token::LParen | Token::Minus)
    }

    fn compile_infix(&mut self, infix: Spanned<Token<'a>>) -> Result<(), CompileError> {
        match infix.item {
            Token::Plus | Token::Minus | Token::Star | Token::Slash => self.compile_binary(infix),
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
            _ => Self::None,
        }
    }
}

#[derive(Debug)]
pub struct CompileError {}
