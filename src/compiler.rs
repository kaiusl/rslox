use std::rc::Rc;

use crate::bytecode::{ByteCode, Instruction};
use crate::common::{Span, Spanned};
use crate::lexer::{Keyword, Lexer, PeekableLexer, Token, TokenKind};
use crate::value::{InternedString, ObjFunction, Object, Value};

use num_traits::FromPrimitive;

pub mod error;
use error::StaticError;

use self::error::{CompileError, CompileErrorKind, StaticErrors};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunType {
    Function,
    Script,
    Method,
    Initializer,
}

pub struct Compiler<'a> {
    pub source: &'a str,
    pub lexer: PeekableLexer<'a>,
    pub errors: StaticErrors<'a>,

    pub chunk: CompileUnit,
    pub current_class: Option<ClassCompileUnit>,
}

pub struct ClassCompileUnit {
    pub parent: Option<Box<ClassCompileUnit>>,
    pub has_superclass: bool,
}

impl ClassCompileUnit {
    pub fn new(parent: Option<Box<ClassCompileUnit>>) -> Self {
        Self {
            parent,
            has_superclass: false,
        }
    }
}

pub struct CompileUnit {
    pub bytecode: ByteCode,
    pub locals: Vec<Local>,
    pub scope_depth: usize,
    pub fun_type: FunType,
    pub constants: Vec<Value>,
    pub parent: Option<Box<CompileUnit>>,
    pub upvalues: Vec<Upvalue>,
}

pub struct Upvalue {
    pub index: u8,
    pub is_local: bool,
}

impl CompileUnit {
    pub fn new(ty: FunType, ident: Spanned<String>, parent: Option<Box<CompileUnit>>) -> Self {
        //locals[0] is for VM's internal use
        let locals = vec![Local {
            ident,
            depth: 0,
            init: true,
            is_captured: false,
        }];
        Self {
            bytecode: ByteCode::new(),
            locals,
            scope_depth: 0,
            fun_type: ty,
            constants: Vec::new(),
            parent,
            upvalues: Vec::new(),
        }
    }
}

impl<'a> Compiler<'a> {
    pub const INIT_METHOD_NAME: &'static str = "init";

    #[allow(clippy::should_implement_trait)]
    pub fn from_str(source: &'a str) -> Self {
        //let mut locals = Vec::with_capacity(1);
        // I don't think we need it atm, we'll add it later if we do need it
        // locals[0] is for VM's internal use
        // locals.push(Local {
        //     ident: Spanned::new(String::from("this"), Span::from_len(0, 0, 0)),
        //     depth: 0,
        //     init: true,
        // });

        Self {
            source,
            lexer: PeekableLexer::new(Lexer::new(source)),
            errors: StaticErrors::new(source),
            chunk: CompileUnit::new(
                FunType::Script,
                Spanned::new(String::from("this"), Span::from_len(0, 0, 0)),
                None,
            ),
            current_class: None,
        }
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        if let Some(idx) = self.chunk.constants.iter().position(|v| *v == value) {
            return idx;
        }

        self.chunk.constants.push(value);
        self.chunk.constants.len() - 1
    }

    pub fn compile(mut self) -> Result<(ByteCode, Vec<Value>), StaticErrors<'a>> {
        loop {
            match self.lexer.peek() {
                Ok(Some(tok)) if **tok == Token::Eof => break,
                Ok(None) => break,
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

        match self.consume(Token::is_eof, || &[TokenKind::Eof]) {
            Ok(_) => (),
            Err(err) => self.errors.push(err),
        };

        // self.emit(
        //     Instruction::Return,
        //     Span::from_len(0, self.source.len().saturating_sub(1), 0),
        // );

        if !self.errors.is_empty() {
            Err(self.errors)
        } else {
            Ok((self.chunk.bytecode, self.chunk.constants))
        }
    }

    fn compile_declaration(&mut self) -> Result<(), StaticError<'a>> {
        let result = match self.lexer.next_if(|tok| {
            matches!(
                tok,
                Token::Keyword(Keyword::Var | Keyword::Fun | Keyword::Class)
            )
        })? {
            Some(tok) if tok.item == Token::Keyword(Keyword::Var) => self.compile_var_decl(tok),
            Some(tok) if tok.item == Token::Keyword(Keyword::Fun) => self.compile_fun_decl(tok),
            Some(tok) if tok.item == Token::Keyword(Keyword::Class) => self.compile_class_decl(tok),
            None => self.compile_stmt(),
            Some(_) => unreachable!(),
        };

        match result {
            Ok(()) => Ok(()),
            Err(err) => {
                self.synchronize();
                Err(err)
            }
        }
    }

    fn compile_class_decl(&mut self, _class_kw: Spanned<Token<'a>>) -> Result<(), StaticError<'a>> {
        let class_ident = self.consume(Token::is_ident, || &[TokenKind::Ident])?;
        let class_ident = class_ident.map(|ident| ident.clone().try_into_ident().unwrap());
        let span = class_ident.span.clone();
        let const_idx = self.compile_ident_constant(class_ident.map(|ident| ident.to_string()))?;
        self.declare_variable(class_ident.clone())?;

        self.emit(Instruction::Class(const_idx), span.clone());
        self.compile_define_variable(Spanned::new(const_idx, span.clone()));

        self.current_class = Some(ClassCompileUnit::new(
            self.current_class.take().map(Box::new),
        ));

        if let Some(_lt) = self.lexer.next_if(Token::is_lt)? {
            let super_ident = self.consume(Token::is_ident, || &[TokenKind::Ident])?;
            let super_ident = super_ident.map(|ident| ident.clone().try_into_ident().unwrap());
            self.compile_variable(&super_ident, super_ident.span.clone(), false)?;

            if super_ident.item == class_ident.item {
                let kind = CompileErrorKind::Msg("a class can't inherit from itself".into());
                let err = CompileError::new(kind, super_ident.span);
                return Err(err.into());
            }
            self.current_class.as_mut().unwrap().has_superclass = true;

            self.enter_scope();
            let local = Local {
                ident: Spanned::new("super".into(), Span::new(0, 0, 0)),
                depth: self.chunk.scope_depth,
                init: false,
                is_captured: false,
            };
            self.chunk.locals.push(local);
            self.compile_define_variable(Spanned::new(0, Span::new(0, 0, 0)));

            self.compile_named_variable(class_ident.item, span.clone(), false)?;
            self.emit(Instruction::Inherit, super_ident.span);
        }

        self.compile_named_variable(class_ident.item, span.clone(), false)?; // push class to the stack, so that methods can find it

        let _lbrace = self.consume(Token::is_lbrace, || &[TokenKind::LBrace])?;

        while !self
            .lexer
            .is_next(|tok| matches!(tok, Token::RBrace | Token::Eof))?
        {
            self.compile_method()?;
        }

        let _lbrace = self.consume(Token::is_rbrace, || &[TokenKind::RBrace])?;
        self.emit(Instruction::Pop, span.clone()); // pop class

        if self.current_class.as_mut().unwrap().has_superclass {
            self.exit_scope();
        }

        self.current_class = self
            .current_class
            .as_mut()
            .unwrap()
            .parent
            .take()
            .map(|c| *c);

        Ok(())
    }

    fn compile_method(&mut self) -> Result<(), StaticError<'a>> {
        let ident = self.consume(Token::is_ident, || &[TokenKind::Ident])?;
        let ident = ident.map(|ident| ident.clone().try_into_ident().unwrap());
        let span = ident.span.clone();
        let const_idx = self.compile_ident_constant(ident.map(|ident| ident.to_string()))?;

        let fun_type = if ident.item == Self::INIT_METHOD_NAME {
            FunType::Initializer
        } else {
            FunType::Method
        };

        self.compile_function(None, ident.map(|ident| ident.to_string()), fun_type)?;
        self.emit(Instruction::Method(const_idx), span.clone());

        Ok(())
    }

    fn compile_fun_decl(&mut self, fun_kw: Spanned<Token<'a>>) -> Result<(), StaticError<'a>> {
        let (ident, idx, ident_span) = self.compile_variable_name()?;
        if let Some(last) = self.chunk.locals.last_mut() {
            last.init = true;
        }
        self.compile_function(
            Some(fun_kw),
            Spanned::new(ident.to_string(), ident_span.clone()),
            FunType::Function,
        )?;
        self.compile_define_variable(Spanned::new(idx, ident_span));

        Ok(())
    }

    fn compile_function(
        &mut self,
        fun_kw: Option<Spanned<Token<'a>>>,
        name: Spanned<String>,
        fun_type: FunType,
    ) -> Result<(), StaticError<'a>> {
        let unit_name = if matches!(fun_type, FunType::Method | FunType::Initializer) {
            "this".into()
        } else {
            String::new()
        };

        let prev_chunk = std::mem::replace(
            &mut self.chunk,
            CompileUnit::new(
                fun_type,
                Spanned::new(unit_name, Span::from_len(0, 0, 0)),
                None,
            ),
        );
        self.chunk.parent = Some(Box::new(prev_chunk));

        let _lparen = self.consume(Token::is_lparen, || &[TokenKind::LParen])?;

        self.enter_scope();

        let mut arity = 0;
        if !self.lexer.is_next(Token::is_rparen)? {
            loop {
                arity += 1;
                if arity == u8::MAX as usize {
                    // == because local slot 1 is for the function itself
                    let kind = CompileErrorKind::Msg("can't have more then 255 parameters".into());
                    let span = self.lexer.next()?.unwrap().span;
                    let err = CompileError::new(kind, span);
                    return Err(err.into());
                }

                let (_ident, idx, span) = self.compile_variable_name()?;
                //println!("{}: {}", idx, self.constants[idx.item as usize]);
                self.compile_define_variable(Spanned::new(idx, span));

                if self.lexer.next_if(Token::is_comma)?.is_none() {
                    break;
                }
            }
        }

        let _rparen = self.consume(Token::is_rparen, || &[TokenKind::RParen])?;
        let lbrace = self.consume(Token::is_lbrace, || &[TokenKind::LBrace])?;
        self.compile_block()?;

        // emit return nil in case the block does not have explicit return,
        // if it has there instructions simply will not be executed
        if fun_type == FunType::Initializer {
            self.emit(Instruction::GetLocal(0), lbrace.span.clone());
        } else {
            self.emit(Instruction::Nil, lbrace.span.clone());
        }
        self.emit(Instruction::Return, lbrace.span.clone());

        self.exit_scope();
        let parent = *self.chunk.parent.take().unwrap();
        let function_bytecode = std::mem::replace(&mut self.chunk, parent);

        let fun = ObjFunction::new(
            InternedString::new(name.item),
            function_bytecode.bytecode,
            function_bytecode.constants.into(),
            arity,
            function_bytecode.upvalues.len(),
        );
        let fun = Value::Object(Object::Function(Rc::new(fun)));

        let span = fun_kw.map(|s| s.span).unwrap_or_else(|| name.span.clone());
        let idx = self.add_constant(fun);
        let Ok(idx) = u8::try_from(idx) else {
            todo!("Too many constants. Add another op to support more constants.");
        };

        self.emit(Instruction::Closure(idx), span.clone());

        for upvalue in function_bytecode.upvalues {
            self.chunk
                .bytecode
                .push_byte(upvalue.is_local as u8, span.clone());
            self.chunk.bytecode.push_byte(upvalue.index, span.clone());
        }

        Ok(())
    }

    fn compile_var_decl(&mut self, var_kw: Spanned<Token<'a>>) -> Result<(), StaticError<'a>> {
        let (_, const_idx, ident_span) = self.compile_variable_name()?;

        if let Some(_eq) = self.lexer.next_if(Token::is_eq)? {
            self.compile_expr()?;
        } else {
            self.emit(Instruction::Nil, var_kw.span.combine(&ident_span.clone()))
        }

        self.consume(Token::is_semicolon, || &[TokenKind::Semicolon])?;

        self.compile_define_variable(Spanned::new(const_idx, ident_span));

        Ok(())
    }

    fn compile_variable_name(&mut self) -> Result<(&'a str, u8, Span), StaticError<'a>> {
        let ident = self.consume(Token::is_ident, || &[TokenKind::Ident])?;

        let ident_str = ident.map(|ident| ident.clone().try_into_ident().unwrap());

        self.declare_variable(ident_str.clone())?;
        if self.chunk.scope_depth > 0 {
            let local_idx = u8::try_from(self.chunk.locals.len() - 1)
                .expect("self.declare_variable checks that self.chunk.locals.len() <= u8::MAX");

            return Ok((ident_str.item, local_idx, ident_str.span));
        }

        self.compile_ident_constant(ident_str.clone().map_into(|s| s.to_string()))
            .map(|a| (ident_str.item, a, ident_str.span))
    }

    fn compile_define_variable(&mut self, idx: Spanned<u8>) {
        if self.chunk.scope_depth > 0 {
            if let Some(last) = self.chunk.locals.last_mut() {
                last.init = true;
            }
            return;
        }

        self.emit(Instruction::DefineGlobal(idx.item), idx.span);
    }

    fn declare_variable(&mut self, ident: Spanned<&str>) -> Result<(), StaticError<'a>> {
        if self.chunk.scope_depth == 0 {
            return Ok(());
        }

        if self.chunk.locals.len() == u8::MAX as usize {
            let kind = CompileErrorKind::Msg("too many local variables".into());
            let err = CompileError::new(kind, ident.span);
            return Err(err.into());
        }

        for local in self.chunk.locals.iter().rev() {
            if local.depth < self.chunk.scope_depth {
                break;
            }
            if local.ident.item == ident.item {
                let kind = CompileErrorKind::ReassignmentOfVariableInLocalScope {
                    ident: ident.item.to_string(),
                };
                let err = CompileError::new(kind, ident.span);
                return Err(err.into());
            }
        }

        let local = Local {
            ident: ident.map_into(|s| s.to_string()),
            depth: self.chunk.scope_depth,
            init: false,
            is_captured: false,
        };
        self.chunk.locals.push(local);

        Ok(())
    }

    fn compile_ident_constant(&mut self, ident: Spanned<String>) -> Result<u8, StaticError<'a>> {
        let span = ident.span;
        let ident = Value::new_object(Object::String(InternedString::new(ident.item)));
        let idx = self.add_constant(ident);
        let Ok(idx) = u8::try_from(idx) else {
            let kind = CompileErrorKind::Msg("too many constants".into());
            let err = CompileError::new(kind, span);
            return Err(err.into());
        };

        Ok(idx)
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
                Ok(None) => break,
                Err(_) => {
                    // ignore following error before synchronization point
                    let _ = self.lexer.next();
                }
            }
        }
    }

    fn compile_stmt(&mut self) -> Result<(), StaticError<'a>> {
        let Some(tok) = self.lexer.next_if(|tok| {
            matches!(
                tok,
                Token::Keyword(
                    Keyword::Print | Keyword::If | Keyword::While | Keyword::For | Keyword::Return
                ) | Token::LBrace
            )
        })?
        else {
            return self.compile_expr_stmt();
        };

        match tok.item {
            Token::Keyword(Keyword::Print) => self.compile_print_stmt(tok),
            Token::Keyword(Keyword::If) => self.compile_if_stmt(tok),
            Token::Keyword(Keyword::While) => self.compile_while_stmt(tok),
            Token::Keyword(Keyword::For) => self.compile_for_stmt(tok),
            Token::Keyword(Keyword::Return) => self.compile_return_stmt(tok),
            Token::LBrace => self.compile_block_stmt(tok),
            _ => unreachable!(),
        }
    }

    fn compile_return_stmt(
        &mut self,
        return_kw: Spanned<Token<'a>>,
    ) -> Result<(), StaticError<'a>> {
        if self.chunk.fun_type == FunType::Script {
            let kind = CompileErrorKind::Msg("can't return from top-level code".into());
            let err = CompileError::new(kind, return_kw.span);
            return Err(err.into());
        }

        if let Some(semicolon) = self.lexer.next_if(Token::is_semicolon)? {
            if self.chunk.fun_type == FunType::Initializer {
                self.emit(Instruction::GetLocal(0), semicolon.span.clone());
            } else {
                self.emit(Instruction::Nil, semicolon.span.clone());
            }
            self.emit(Instruction::Return, semicolon.span);
        } else {
            if self.chunk.fun_type == FunType::Initializer {
                let kind = CompileErrorKind::Msg("can't return a value from an initializer".into());
                let err = CompileError::new(kind, return_kw.span);
                return Err(err.into());
            }

            self.compile_expr()?;
            let _semicolon = self.consume(Token::is_semicolon, || &[TokenKind::Semicolon])?;
            self.emit(Instruction::Return, return_kw.span);
        }

        Ok(())
    }

    fn compile_for_stmt(&mut self, for_kw: Spanned<Token<'a>>) -> Result<(), StaticError<'a>> {
        self.enter_scope();
        let _lparen = self.consume(Token::is_lparen, || &[TokenKind::LParen])?;
        match self.lexer.peek()? {
            Some(tok) if tok.item == Token::Semicolon => {
                self.consume(Token::is_semicolon, || &[TokenKind::Semicolon])?;
            }
            Some(tok) if tok.item == Token::Keyword(Keyword::Var) => {
                let tok = self
                    .lexer
                    .next()
                    .expect("we just checked that it's ok")
                    .expect("we just checked that it's some");
                self.compile_var_decl(tok)?;
            }
            Some(_) => self.compile_expr_stmt()?,
            None => unreachable!(),
        }

        let mut loop_start = self.chunk.bytecode.code.len();
        let mut exit_jump = None;
        if !self.lexer.is_next(Token::is_semicolon)? {
            self.compile_expr()?;
            let semicolon = self.consume(Token::is_semicolon, || &[TokenKind::Semicolon])?;

            exit_jump =
                Some(self.emit_jump(Instruction::JumpIfFalse(u16::MAX), semicolon.span.clone()));
            self.emit(Instruction::Pop, semicolon.span);
        } else {
            self.consume(Token::is_semicolon, || &[TokenKind::Semicolon])?;
        }

        if !self.lexer.is_next(Token::is_rparen)? {
            let body_jump = self.emit_jump(Instruction::Jump(u16::MAX), for_kw.span.clone());
            let increment_start = self.chunk.bytecode.code.len();
            self.compile_expr()?;
            self.emit(Instruction::Pop, for_kw.span.clone());
            let _rparen = self.consume(Token::is_rparen, || &[TokenKind::RParen])?;

            self.emit_loop(loop_start, for_kw.span.clone())?;
            loop_start = increment_start;
            self.patch_jump(body_jump);
        } else {
            let _rparen = self.consume(Token::is_rparen, || &[TokenKind::RParen])?;
        }

        self.compile_stmt()?;
        self.emit_loop(loop_start, for_kw.span.clone())?;

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump);
            self.emit(Instruction::Pop, for_kw.span.clone());
        }

        self.exit_scope();

        Ok(())
    }

    fn compile_while_stmt(&mut self, while_kw: Spanned<Token<'a>>) -> Result<(), StaticError<'a>> {
        let loop_start = self.chunk.bytecode.code.len();
        let _lparen = self.consume(Token::is_lparen, || &[TokenKind::LParen])?;
        self.compile_expr()?;
        let _rparen = self.consume(Token::is_rparen, || &[TokenKind::RParen])?;

        let exit_jump = self.emit_jump(Instruction::JumpIfFalse(u16::MAX), while_kw.span.clone());
        self.emit(Instruction::Pop, while_kw.span.clone());
        self.compile_stmt()?;
        self.emit_loop(loop_start, while_kw.span.clone())?;

        self.patch_jump(exit_jump);
        self.emit(Instruction::Pop, while_kw.span.clone());

        Ok(())
    }

    fn emit_loop(&mut self, loop_start: usize, span: Span) {
        let offset = self.chunk.bytecode.code.len() - loop_start + 3;
        if offset >= u16::MAX as usize {
            todo!("Too long jump. Add another op to support more jumps.");
        }

        self.emit(Instruction::Loop(offset as u16), span);

        Ok(())
    }

    fn compile_if_stmt(&mut self, if_kw: Spanned<Token<'a>>) -> Result<(), StaticError<'a>> {
        let _lparen = self.consume(Token::is_lparen, || &[TokenKind::LParen])?;
        self.compile_expr()?;
        let _rparen = self.consume(Token::is_rparen, || &[TokenKind::RParen])?;

        let then_jump = self.emit_jump(Instruction::JumpIfFalse(u16::MAX), if_kw.span.clone());
        self.emit(Instruction::Pop, if_kw.span.clone());
        self.compile_stmt()?;

        let else_jump = self.emit_jump(Instruction::Jump(u16::MAX), if_kw.span.clone());

        self.patch_jump(then_jump);
        self.emit(Instruction::Pop, if_kw.span.clone());

        if let Some(_else_kw) = self.lexer.next_if(Token::is_else)? {
            self.compile_stmt()?;
        }
        self.patch_jump(else_jump);

        Ok(())
    }

    fn emit_jump(&mut self, instruction: Instruction, span: Span) -> usize {
        let operand_start = self.chunk.bytecode.code.len() + 1;
        self.emit(instruction, span);

        operand_start
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump =
            self.chunk.bytecode.code.len() - offset - (Instruction::JumpIfFalse(0).byte_len() - 1);

        if jump >= u16::MAX as usize {
            todo!("Too long jump. Add another op to support more jumps.");
        }

        let dst = &mut self.chunk.bytecode.code[offset..offset + 2];
        debug_assert!(dst.len() == 2);
        debug_assert!(dst[0] == 0xff);
        debug_assert!(dst[1] == 0xff);

        dst.copy_from_slice(&(jump as u16).to_le_bytes());
    }

    fn compile_block_stmt(&mut self, _lbrace: Spanned<Token<'a>>) -> Result<(), StaticError<'a>> {
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

        self.consume(Token::is_rbrace, || &[TokenKind::RBrace])?;
        Ok(())
    }

    fn enter_scope(&mut self) {
        self.chunk.scope_depth += 1;
    }

    fn exit_scope(&mut self) {
        while self.chunk.locals.last().map(|l| l.depth) == Some(self.chunk.scope_depth) {
            let last = self.chunk.locals.pop().unwrap();
            if last.is_captured {
                self.emit(Instruction::CloseUpvalue, Span::new(0, 0, 0));
            } else {
                self.emit(Instruction::Pop, Span::new(0, 0, 0));
            }
        }

        self.chunk.scope_depth -= 1;
    }

    fn compile_expr_stmt(&mut self) -> Result<(), StaticError<'a>> {
        self.compile_expr()?;
        let semicolon = self.consume(Token::is_semicolon, || &[TokenKind::Semicolon])?;
        self.emit(Instruction::Pop, semicolon.span);
        Ok(())
    }

    fn compile_print_stmt(&mut self, print: Spanned<Token<'a>>) -> Result<(), StaticError<'a>> {
        self.compile_expr()?;
        self.consume(Token::is_semicolon, || &[TokenKind::Semicolon])?;

        self.emit(Instruction::Print, print.span);
        Ok(())
    }

    fn emit(&mut self, instruction: Instruction, span: Span) {
        self.chunk.bytecode.push(instruction, span);
    }

    pub fn consume(
        &mut self,
        predicate: impl Fn(&Token<'a>) -> bool,
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
        let idx = self.add_constant(value);
        let Ok(idx) = u8::try_from(idx) else {
            todo!("Too many constants. Add another op to support more constants.");
        };

        self.emit(Instruction::Constant(idx), span);

        Ok(idx)
    }

    fn compile_grouping(
        &mut self,
        _lparen: Spanned<Token<'a>>,
        _can_assign: bool,
    ) -> Result<(), StaticError<'a>> {
        self.compile_expr()?;

        self.consume(Token::is_rparen, || &[TokenKind::RParen])?;

        Ok(())
    }

    fn compile_unary(
        &mut self,
        operator: Spanned<Token<'a>>,
        _can_assign: bool,
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
        _can_assign: bool,
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
            if let Some(tok) = self.lexer.next_if(Token::is_eq)? {
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
        #[allow(clippy::unit_arg)]
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
            Token::Keyword(Keyword::This) => self.compile_this(prefix),
            Token::Keyword(Keyword::Super) => self.compile_super(prefix),
            _ => unreachable!("Invalid prefix operator."),
        }
    }

    fn compile_super(&mut self, super_kw: Spanned<Token<'a>>) -> Result<(), StaticError<'a>> {
        if self.current_class.is_none() {
            let kind = CompileErrorKind::Msg("can't use 'super' outside of a class".into());
            let err = CompileError::new(kind, super_kw.span);
            return Err(err.into());
        } else if !self.current_class.as_ref().unwrap().has_superclass {
            let kind =
                CompileErrorKind::Msg("can't use 'super' in a class with no superclass".into());
            let err = CompileError::new(kind, super_kw.span);
            return Err(err.into());
        }

        let _dot = self.consume(Token::is_dot, || &[TokenKind::Dot])?;
        let name = self.consume(Token::is_ident, || &[TokenKind::Ident])?;
        let name = name.map(|name| name.clone().try_into_ident().unwrap());
        let name_const_idx = self.compile_ident_constant(name.map(|name| name.to_string()))?;

        self.compile_named_variable("this", Span::new(0, 0, 0), false)?;
        self.compile_named_variable("super", Span::new(0, 0, 0), false)?;

        self.emit(Instruction::GetSuper(name_const_idx), super_kw.span);

        Ok(())
    }

    fn compile_this(&mut self, this_kw: Spanned<Token<'a>>) -> Result<(), StaticError<'a>> {
        if self.current_class.is_none() {
            let kind = CompileErrorKind::Msg("can't use 'this' outside of a class".into());
            let err = CompileError::new(kind, this_kw.span);
            return Err(err.into());
        }

        self.compile_variable("this", this_kw.span, false)
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
        let (get, set, span) = match Self::resolve_local(&self.chunk, ident, span.clone()) {
            Some(idx) => {
                let idx = idx?;
                (
                    Instruction::GetLocal(idx.item),
                    Instruction::SetLocal(idx.item),
                    idx.span,
                )
            }
            None => match Self::resolve_upvalue(&mut self.chunk, ident, span.clone()) {
                Some(idx) => {
                    let idx = idx?;
                    (
                        Instruction::GetUpvalue(idx.item),
                        Instruction::SetUpvalue(idx.item),
                        idx.span,
                    )
                }
                None => {
                    let idx =
                        self.compile_ident_constant(Spanned::new(ident.to_string(), span.clone()))?;
                    (
                        Instruction::GetGlobal(idx),
                        Instruction::SetGlobal(idx),
                        span,
                    )
                }
            },
        };

        if can_assign {
            if let Some(_tok) = self.lexer.next_if(Token::is_eq)? {
                self.compile_expr()?;
                self.emit(set, span);

                return Ok(());
            }
        }

        self.emit(get, span);

        Ok(())
    }

    fn resolve_upvalue(
        unit: &mut CompileUnit,
        ident: &str,
        span: Span,
    ) -> Option<Result<Spanned<u8>, StaticError<'a>>> {
        let Some(parent) = &mut unit.parent else {
            return None;
        };

        let (idx, is_local) = match Self::resolve_local(parent, ident, span.clone()) {
            Some(idx) => (idx, true),
            None => match Self::resolve_upvalue(parent, ident, span) {
                Some(idx) => (idx, false),
                None => return None,
            },
        };

        match idx {
            Ok(idx) => {
                if is_local {
                    parent.locals[idx.item as usize].is_captured = true;
                }
                Self::add_upvalue(unit, idx, is_local)
            }
            Err(_) => unreachable!(),
        }
    }

    fn add_upvalue(
        unit: &mut CompileUnit,
        idx: Spanned<u8>,
        is_local: bool,
    ) -> Option<Result<Spanned<u8>, StaticError<'a>>> {
        for (i, existing) in unit.upvalues.iter().enumerate() {
            if existing.index == idx.item && existing.is_local == is_local {
                return Some(Ok(Spanned::new(i as u8, idx.span)));
            }
        }

        if unit.upvalues.len() >= u8::MAX as usize {
            let kind = CompileErrorKind::Msg("too many closure variables in function".into());
            let err = CompileError::new(kind, idx.span);
            return Some(Err(err.into()));
        }

        let upvalue = Upvalue {
            index: idx.item,
            is_local,
        };

        unit.upvalues.push(upvalue);
        Some(Ok(Spanned::new((unit.upvalues.len() - 1) as u8, idx.span)))
    }

    fn resolve_local(
        unit: &CompileUnit,
        ident: &str,
        span: Span,
    ) -> Option<Result<Spanned<u8>, StaticError<'a>>> {
        for (i, local) in unit.locals.iter().enumerate().rev() {
            if local.ident.item == ident {
                assert!(i < u8::MAX as usize);
                if !local.init {
                    let kind = CompileErrorKind::UseOfLocalInItsOwnInitializer;
                    let err = CompileError::new(kind, span);
                    return Some(Err(err.into()));
                }
                return Some(Ok(local.ident.map(|_| i as u8)));
            }
        }

        None
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
                | Token::Keyword(
                    Keyword::Nil | Keyword::True | Keyword::False | Keyword::This | Keyword::Super
                )
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
            TokenKind::Keyword(Keyword::This),
            TokenKind::Keyword(Keyword::Super),
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
            Token::Keyword(Keyword::And) => self.compile_and(infix, can_assign),
            Token::Keyword(Keyword::Or) => self.compile_or(infix, can_assign),
            Token::LParen => self.compile_call(infix, can_assign),
            Token::Dot => self.compile_dot(infix, can_assign),
            _ => unreachable!("Invalid infix operator."),
        }
    }

    fn compile_dot(
        &mut self,
        _dot: Spanned<Token<'a>>,
        can_assign: bool,
    ) -> Result<(), StaticError<'a>> {
        let ident = self.consume(Token::is_ident, || &[TokenKind::Ident])?;
        let ident = ident.map(|ident| ident.clone().try_into_ident().unwrap());
        let name_const_idx = self.compile_ident_constant(ident.map(|ident| ident.to_string()))?;

        let maybe_eq = self.lexer.next_if(Token::is_eq)?;
        if can_assign && maybe_eq.is_some() {
            let _eq = maybe_eq.unwrap();
            self.compile_expr()?;
            self.emit(Instruction::SetProperty(name_const_idx), ident.span);
        } else if let Some(lparen) = self.lexer.next_if(Token::is_lparen)? {
            let arg_count = self.compile_arg_list()?;
            self.emit(Instruction::Invoke(name_const_idx, arg_count), lparen.span);
        } else {
            self.emit(Instruction::GetProperty(name_const_idx), ident.span);
        }

        Ok(())
    }

    fn compile_call(
        &mut self,
        lparen: Spanned<Token<'a>>,
        _can_assign: bool,
    ) -> Result<(), StaticError<'a>> {
        let arg_count = self.compile_arg_list()?;
        self.emit(Instruction::Call(arg_count), lparen.span);
        Ok(())
    }

    fn compile_arg_list(&mut self) -> Result<u8, StaticError<'a>> {
        let mut arg_count = 0;
        if !self.lexer.is_next(Token::is_rparen)? {
            loop {
                if arg_count == u8::MAX {
                    let kind = CompileErrorKind::Msg("can't have more than 255 arguments".into());
                    let ident = self.consume(Token::is_ident, || &[TokenKind::Ident])?;
                    let err = CompileError::new(kind, ident.span);
                    return Err(err.into());
                }
                self.compile_expr()?;
                arg_count += 1;
                if self.lexer.next_if(Token::is_comma)?.is_none() {
                    break;
                }
            }
        }

        let _rparen = self.consume(Token::is_rparen, || &[TokenKind::RParen])?;
        Ok(arg_count)
    }

    fn compile_and(
        &mut self,
        and: Spanned<Token<'a>>,
        _can_assign: bool,
    ) -> Result<(), StaticError<'a>> {
        let end_jump = self.emit_jump(Instruction::JumpIfFalse(u16::MAX), and.span.clone());
        self.emit(Instruction::Pop, and.span);
        self.compile_precedence(Precedence::And)?;
        self.patch_jump(end_jump);

        Ok(())
    }

    fn compile_or(
        &mut self,
        or: Spanned<Token<'a>>,
        _can_assign: bool,
    ) -> Result<(), StaticError<'a>> {
        let else_jump = self.emit_jump(Instruction::JumpIfFalse(u16::MAX), or.span.clone());
        let end_jump = self.emit_jump(Instruction::Jump(u16::MAX), or.span.clone());
        self.patch_jump(else_jump);
        self.emit(Instruction::Pop, or.span);
        self.compile_precedence(Precedence::Or)?;
        self.patch_jump(end_jump);

        Ok(())
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
            Token::Keyword(Keyword::And) => Precedence::And,
            Token::Keyword(Keyword::Or) => Precedence::Or,
            Token::LParen | Token::Dot => Precedence::Call,

            _ => Self::None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Local {
    pub ident: Spanned<String>,
    pub depth: usize,
    pub init: bool,
    pub is_captured: bool,
}
