use crate::bytecode::ByteCode;
use crate::common::Spanned;
use crate::lexer::{Lexer, PeekableLexer, Token};

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
        self.advance();
        self.compile_expr();

        self.consume(
            |token| matches!(token, Token::Eof),
            "Expected end of expression.",
        )?;

        Ok(self.bytecode)
    }

    pub fn consume(
        &mut self,
        predicate: impl Fn(&Token<'_>) -> bool,
        msg: &'static str,
    ) -> Result<Spanned<Token<'a>>, CompileError> {
        match self.lexer.next() {
            Some(Ok(token)) if predicate(&token) => Ok(token),
            Some(Ok(token)) => Err(CompileError {}),
            Some(Err(err)) => Err(CompileError {}),
            None => Err(CompileError {}),
        }
    }

    pub fn advance(&mut self) -> Option<Token<'a>> {
        todo!()
    }

    pub fn compile_expr(&mut self) {
        todo!()
    }
}

#[derive(Debug)]
pub struct CompileError {}
