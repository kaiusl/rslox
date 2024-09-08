use std::borrow::Cow;
use std::collections::HashMap;
use std::rc::Rc;
use std::{fmt, mem};

use crate::common::Span;
use crate::disassembler::{Disassembler, DisassemblerError};
use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct ByteCode {
    pub code: Vec<u8>,
    pub spans: HashMap<usize, Span>,
}

impl ByteCode {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            spans: HashMap::new(),
        }
    }

    pub fn push(&mut self, instruction: Instruction, span: Span) {
        self.spans.insert(self.code.len(), span);
        instruction.write_bytes(&mut self.code);
    }

    pub fn push_byte(&mut self, byte: u8, span: Span) {
        self.spans.insert(self.code.len(), span);
        self.code.push(byte);
    }
}

#[derive(Debug, Clone)]
pub struct BytesCursor {
    offset: usize,
    instructions: Rc<[u8]>,
}

impl BytesCursor {
    pub fn new(instructions: Rc<[u8]>) -> Self {
        BytesCursor {
            offset: 0,
            instructions,
        }
    }

    pub fn as_instructions(&self) -> &[u8] {
        &self.instructions
    }

    pub fn jump_forward(&mut self, count: usize) {
        self.offset += count
    }

    pub fn jump_backward(&mut self, count: usize) {
        self.offset -= count
    }

    pub fn u8(&mut self) -> Option<u8> {
        let result = self.instructions.get(self.offset).copied();
        self.offset += 1;
        result
    }

    pub fn peek_u8(&mut self) -> Option<u8> {
        self.instructions.get(self.offset).copied()
    }

    pub fn u16(&mut self) -> Option<u16> {
        const LEN: usize = std::mem::size_of::<u16>();
        self.array::<LEN>().map(u16::from_le_bytes)
    }

    pub fn u32(&mut self) -> Option<u32> {
        const LEN: usize = std::mem::size_of::<u32>();
        self.array::<LEN>().map(u32::from_le_bytes)
    }

    #[inline]
    fn array<const N: usize>(&mut self) -> Option<[u8; N]> {
        let result = self
            .instructions
            .get(self.offset..)?
            .first_chunk::<N>()
            .copied()?;
        self.offset += N;

        Some(result)
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn len(&self) -> usize {
        self.instructions.len()
    }

    pub fn is_empty(&self) -> bool {
        self.offset >= self.instructions.len()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, num_derive::FromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    Return,
    Constant,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Nil,
    True,
    False,
    Not,
    Eq,
    Gt,
    Lt,
    Print,
    Pop,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    GetUpvalue,
    SetUpvalue,
    GetProperty,
    SetProperty,
    JumpIfFalse,
    Jump,
    Loop,
    Call,
    Closure,
    CloseUpvalue,
    Class,
    Method,
    Invoke,
    Inherit,
    GetSuper,
}

impl OpCode {
    pub fn from_u8(byte: u8) -> Self {
        num_traits::FromPrimitive::from_u8(byte).unwrap()
    }

    pub fn try_from_u8(byte: u8) -> Option<Self> {
        num_traits::FromPrimitive::from_u8(byte)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    Return,
    Constant(u8),
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Nil,
    True,
    False,
    Not,
    Eq,
    Gt,
    Lt,
    Print,
    Pop,
    DefineGlobal(u8),
    GetGlobal(u8),
    SetGlobal(u8),
    GetLocal(u8),
    SetLocal(u8),
    GetUpvalue(u8),
    SetUpvalue(u8),
    GetProperty(u8),
    SetProperty(u8),
    JumpIfFalse(u16),
    Jump(u16),
    Loop(u16),
    Call(u8),
    Closure(u8),
    CloseUpvalue,
    Class(u8),
    Method(u8),
    Invoke(u8, u8),
    Inherit,
    GetSuper(u8),
}

impl Instruction {
    pub fn op_code(&self) -> OpCode {
        match self {
            Instruction::Return => OpCode::Return,
            Instruction::Constant(_) => OpCode::Constant,
            Instruction::Negate => OpCode::Negate,
            Instruction::Add => OpCode::Add,
            Instruction::Subtract => OpCode::Subtract,
            Instruction::Multiply => OpCode::Multiply,
            Instruction::Divide => OpCode::Divide,
            Instruction::Nil => OpCode::Nil,
            Instruction::True => OpCode::True,
            Instruction::False => OpCode::False,
            Instruction::Not => OpCode::Not,
            Instruction::Eq => OpCode::Eq,
            Instruction::Gt => OpCode::Gt,
            Instruction::Lt => OpCode::Lt,
            Instruction::Print => OpCode::Print,
            Instruction::Pop => OpCode::Pop,
            Instruction::DefineGlobal(_) => OpCode::DefineGlobal,
            Instruction::GetGlobal(_) => OpCode::GetGlobal,
            Instruction::SetGlobal(_) => OpCode::SetGlobal,
            Instruction::GetLocal(_) => OpCode::GetLocal,
            Instruction::SetLocal(_) => OpCode::SetLocal,
            Instruction::GetUpvalue(_) => OpCode::GetUpvalue,
            Instruction::SetUpvalue(_) => OpCode::SetUpvalue,
            Instruction::GetProperty(_) => OpCode::GetProperty,
            Instruction::SetProperty(_) => OpCode::SetProperty,
            Instruction::JumpIfFalse(_) => OpCode::JumpIfFalse,
            Instruction::Jump(_) => OpCode::Jump,
            Instruction::Loop(_) => OpCode::Loop,
            Instruction::Call(_) => OpCode::Call,
            Instruction::Closure(_) => OpCode::Closure,
            Instruction::CloseUpvalue => OpCode::CloseUpvalue,
            Instruction::Class(_) => OpCode::Class,
            Instruction::Method(_) => OpCode::Method,
            Instruction::Invoke(_, _) => OpCode::Invoke,
            Instruction::Inherit => OpCode::Inherit,
            Instruction::GetSuper(_) => OpCode::GetSuper,
        }
    }

    pub fn write_bytes(&self, dst: &mut Vec<u8>) {
        dst.push(self.op_code() as u8);
        // push operands
        match self {
            Instruction::Return
            | Instruction::Negate
            | Instruction::Add
            | Instruction::Subtract
            | Instruction::Multiply
            | Instruction::Divide
            | Instruction::Nil
            | Instruction::True
            | Instruction::False
            | Instruction::Not
            | Instruction::Eq
            | Instruction::Gt
            | Instruction::Lt
            | Instruction::Print
            | Instruction::Pop
            | Instruction::CloseUpvalue
            | Instruction::Inherit => {}

            Instruction::Constant(idx)
            | Instruction::DefineGlobal(idx)
            | Instruction::GetGlobal(idx)
            | Instruction::SetGlobal(idx)
            | Instruction::GetLocal(idx)
            | Instruction::SetLocal(idx)
            | Instruction::Call(idx)
            | Instruction::GetUpvalue(idx)
            | Instruction::SetUpvalue(idx)
            | Instruction::GetProperty(idx)
            | Instruction::SetProperty(idx)
            | Instruction::Class(idx)
            | Instruction::Method(idx)
            | Instruction::GetSuper(idx) => {
                dst.push(*idx);
            }

            Instruction::Closure(idx) => {
                dst.push(*idx);
            }

            Instruction::JumpIfFalse(offset)
            | Instruction::Jump(offset)
            | Instruction::Loop(offset) => {
                dst.extend_from_slice(&offset.to_le_bytes());
            }

            Instruction::Invoke(idx, arg_count) => {
                dst.push(*idx);
                dst.push(*arg_count);
            }
        }
    }

    pub fn from_bytes(bytes: &mut BytesCursor) -> Result<Self, DisassemblerError> {
        let instr = match bytes.u8().and_then(OpCode::try_from_u8) {
            Some(OpCode::Return) => Instruction::Return,
            Some(OpCode::Constant) => match bytes.u8() {
                Some(idx) => Instruction::Constant(idx),
                None => {
                    return Err(DisassemblerError {
                        message: Cow::Borrowed("Expected constant index"),
                    })
                }
            },
            Some(OpCode::Negate) => Instruction::Negate,
            Some(OpCode::Add) => Instruction::Add,
            Some(OpCode::Subtract) => Instruction::Subtract,
            Some(OpCode::Multiply) => Instruction::Multiply,
            Some(OpCode::Divide) => Instruction::Divide,
            Some(OpCode::Nil) => Instruction::Nil,
            Some(OpCode::True) => Instruction::True,
            Some(OpCode::False) => Instruction::False,
            Some(OpCode::Not) => Instruction::Not,
            Some(OpCode::Eq) => Instruction::Eq,
            Some(OpCode::Gt) => Instruction::Gt,
            Some(OpCode::Lt) => Instruction::Lt,
            Some(OpCode::Print) => Instruction::Print,
            Some(OpCode::Pop) => Instruction::Pop,
            Some(OpCode::DefineGlobal) => match bytes.u8() {
                Some(idx) => Instruction::DefineGlobal(idx),
                None => {
                    return Err(DisassemblerError {
                        message: Cow::Borrowed("Expected global index"),
                    })
                }
            },
            Some(OpCode::GetGlobal) => match bytes.u8() {
                Some(idx) => Instruction::GetGlobal(idx),
                None => {
                    return Err(DisassemblerError {
                        message: Cow::Borrowed("Expected global index"),
                    })
                }
            },
            Some(OpCode::SetGlobal) => match bytes.u8() {
                Some(idx) => Instruction::SetGlobal(idx),
                None => {
                    return Err(DisassemblerError {
                        message: Cow::Borrowed("Expected global index"),
                    })
                }
            },
            Some(OpCode::GetLocal) => match bytes.u8() {
                Some(idx) => Instruction::GetLocal(idx),
                None => {
                    return Err(DisassemblerError {
                        message: Cow::Borrowed("Expected local index"),
                    })
                }
            },
            Some(OpCode::SetLocal) => match bytes.u8() {
                Some(idx) => Instruction::SetLocal(idx),
                None => {
                    return Err(DisassemblerError {
                        message: Cow::Borrowed("Expected local index"),
                    })
                }
            },
            Some(OpCode::GetUpvalue) => match bytes.u8() {
                Some(idx) => Instruction::GetUpvalue(idx),
                None => {
                    return Err(DisassemblerError {
                        message: Cow::Borrowed("Expected upvalue index"),
                    })
                }
            },

            Some(OpCode::SetUpvalue) => match bytes.u8() {
                Some(idx) => Instruction::SetUpvalue(idx),
                None => {
                    return Err(DisassemblerError {
                        message: Cow::Borrowed("Expected upvalue index"),
                    })
                }
            },
            Some(OpCode::JumpIfFalse) => match bytes.u16() {
                Some(offset) => Instruction::JumpIfFalse(offset),
                None => {
                    return Err(DisassemblerError {
                        message: Cow::Borrowed("Expected offset"),
                    })
                }
            },
            Some(OpCode::Jump) => match bytes.u16() {
                Some(offset) => Instruction::Jump(offset),
                None => {
                    return Err(DisassemblerError {
                        message: Cow::Borrowed("Expected offset"),
                    })
                }
            },
            Some(OpCode::Loop) => match bytes.u16() {
                Some(offset) => Instruction::Loop(offset),
                None => {
                    return Err(DisassemblerError {
                        message: Cow::Borrowed("Expected offset"),
                    })
                }
            },
            Some(OpCode::Call) => match bytes.u8() {
                Some(num_args) => Instruction::Call(num_args),
                None => {
                    return Err(DisassemblerError {
                        message: Cow::Borrowed("Expected function index"),
                    })
                }
            },
            Some(OpCode::Closure) => match bytes.u8() {
                Some(idx) => Instruction::Closure(idx),
                None => {
                    return Err(DisassemblerError {
                        message: Cow::Borrowed("Expected function index"),
                    })
                }
            },
            Some(OpCode::Class) => match bytes.u8() {
                Some(idx) => Instruction::Class(idx),
                None => {
                    return Err(DisassemblerError {
                        message: Cow::Borrowed("Expected class index"),
                    })
                }
            },

            Some(OpCode::GetProperty) => match bytes.u8() {
                Some(idx) => Instruction::GetProperty(idx),
                None => {
                    return Err(DisassemblerError {
                        message: Cow::Borrowed("Expected property index"),
                    })
                }
            },
            Some(OpCode::SetProperty) => match bytes.u8() {
                Some(idx) => Instruction::SetProperty(idx),
                None => {
                    return Err(DisassemblerError {
                        message: Cow::Borrowed("Expected property index"),
                    })
                }
            },

            Some(OpCode::Method) => match bytes.u8() {
                Some(idx) => Instruction::Method(idx),
                None => {
                    return Err(DisassemblerError {
                        message: Cow::Borrowed("Expected method index"),
                    })
                }
            },
            Some(OpCode::Invoke) => {
                let idx = match bytes.u8() {
                    Some(idx) => idx,
                    None => {
                        return Err(DisassemblerError {
                            message: Cow::Borrowed("Expected method index"),
                        })
                    }
                };
                let arg_count = match bytes.u8() {
                    Some(arg_count) => arg_count,
                    None => {
                        return Err(DisassemblerError {
                            message: Cow::Borrowed("Expected argument count"),
                        })
                    }
                };
                Instruction::Invoke(idx, arg_count)
            }
            Some(OpCode::GetSuper) => match bytes.u8() {
                Some(idx) => Instruction::GetSuper(idx),
                None => {
                    return Err(DisassemblerError {
                        message: Cow::Borrowed("Expected method index"),
                    })
                }
            },
            Some(OpCode::CloseUpvalue) => Instruction::CloseUpvalue,
            Some(OpCode::Inherit) => Instruction::Inherit,
            None => {
                return Err(DisassemblerError {
                    message: Cow::Borrowed("Unknown opcode"),
                });
            }
        };

        Ok(instr)
    }

    pub fn byte_len(&self) -> usize {
        1 + match self {
            Instruction::Return
            | Instruction::Negate
            | Instruction::Add
            | Instruction::Subtract
            | Instruction::Multiply
            | Instruction::Divide
            | Instruction::Nil
            | Instruction::True
            | Instruction::False
            | Instruction::Not
            | Instruction::Eq
            | Instruction::Gt
            | Instruction::Lt
            | Instruction::Print
            | Instruction::Pop
            | Instruction::CloseUpvalue
            | Instruction::Inherit => 0,

            Instruction::Constant(idx)
            | Instruction::DefineGlobal(idx)
            | Instruction::GetGlobal(idx)
            | Instruction::SetGlobal(idx)
            | Instruction::GetLocal(idx)
            | Instruction::SetLocal(idx)
            | Instruction::Call(idx)
            | Instruction::GetUpvalue(idx)
            | Instruction::SetUpvalue(idx)
            | Instruction::Class(idx)
            | Instruction::GetProperty(idx)
            | Instruction::SetProperty(idx)
            | Instruction::Method(idx)
            | Instruction::GetSuper(idx) => mem::size_of_val(idx),

            Instruction::Closure(idx) => mem::size_of_val(idx),

            Instruction::JumpIfFalse(offset)
            | Instruction::Jump(offset)
            | Instruction::Loop(offset) => mem::size_of_val(offset),

            Instruction::Invoke(idx, arg_count) => {
                mem::size_of_val(idx) + mem::size_of_val(arg_count)
            }
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Return => write!(f, "RETURN"),
            Instruction::Constant(idx) => write!(f, "CONSTANT {}", idx),
            Instruction::Negate => write!(f, "NEGATE"),
            Instruction::Add => write!(f, "ADD"),
            Instruction::Subtract => write!(f, "SUBTRACT"),
            Instruction::Multiply => write!(f, "MULTIPLY"),
            Instruction::Divide => write!(f, "DIVIDE"),
            Instruction::Nil => write!(f, "NIL"),
            Instruction::True => write!(f, "TRUE"),
            Instruction::False => write!(f, "FALSE"),
            Instruction::Not => write!(f, "NOT"),
            Instruction::Eq => write!(f, "EQ"),
            Instruction::Gt => write!(f, "GT"),
            Instruction::Lt => write!(f, "LT"),
            Instruction::Print => write!(f, "PRINT"),
            Instruction::Pop => write!(f, "POP"),
            Instruction::DefineGlobal(idx) => write!(f, "DEFINE_GLOBAL {}", idx),
            Instruction::GetGlobal(idx) => write!(f, "GET_GLOBAL {}", idx),
            Instruction::SetGlobal(idx) => write!(f, "SET_GLOBAL {}", idx),
            Instruction::GetLocal(idx) => write!(f, "GET_LOCAL {}", idx),
            Instruction::SetLocal(idx) => write!(f, "SET_LOCAL {}", idx),
            Instruction::GetUpvalue(idx) => write!(f, "GET_UPVALUE {}", idx),
            Instruction::SetUpvalue(idx) => write!(f, "SET_UPVALUE {}", idx),
            Instruction::JumpIfFalse(offset) => write!(f, "JUMP_IF_FALSE {}", offset),
            Instruction::Jump(offset) => write!(f, "JUMP {}", offset),
            Instruction::Loop(offset) => write!(f, "LOOP {}", offset),
            Instruction::Call(idx) => write!(f, "CALL {}", idx),
            Instruction::Closure(idx) => write!(f, "CLOSURE {}", idx),
            Instruction::CloseUpvalue => write!(f, "CLOSE_UPVALUE"),
            Instruction::Class(idx) => write!(f, "CLASS {}", idx),
            Instruction::GetProperty(idx) => write!(f, "GET_PROPERTY {}", idx),
            Instruction::SetProperty(idx) => write!(f, "SET_PROPERTY {}", idx),
            Instruction::Method(idx) => write!(f, "METHOD {}", idx),
            Instruction::Invoke(idx, arg_count) => write!(f, "INVOKE {} {}", idx, arg_count),
            Instruction::Inherit => write!(f, "INHERIT"),
            Instruction::GetSuper(idx) => write!(f, "GET_SUPER {}", idx),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpFromBytesResult {
    pub op: Result<Instruction, DisassemblerError>,
    pub offset: usize,
}
