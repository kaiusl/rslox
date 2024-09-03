use std::borrow::Cow;
use std::collections::HashMap;
use std::{fmt, mem};

use crate::common::Span;
use crate::disassembler::{Disassembler, DisassemblerError};
use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub struct ByteCode {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    pub spans: HashMap<usize, Span>,
}

impl ByteCode {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
            spans: HashMap::new(),
        }
    }

    pub fn push(&mut self, instruction: Instruction, span: Span) {
        self.spans.insert(self.code.len(), span);
        instruction.write_bytes(&mut self.code);
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn disassemble(&self) -> Disassembler {
        Disassembler::new(self)
    }
}

#[derive(Debug, Clone)]
pub struct BytesCursor {
    offset: usize,
    instructions: Vec<u8>,
}

impl BytesCursor {
    pub fn new(instructions: Vec<u8>) -> Self {
        BytesCursor {
            offset: 0,
            instructions,
        }
    }

    pub fn u8(&mut self) -> Option<u8> {
        let result = self.instructions.get(self.offset).copied();
        self.offset += 1;
        result
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
            | Instruction::Lt => {}
            Instruction::Constant(idx) => {
                dst.push(*idx);
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
            | Instruction::Lt => 0,
            Instruction::Constant(idx) => mem::size_of_val(idx),
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
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpFromBytesResult {
    pub op: Result<Instruction, DisassemblerError>,
    pub offset: usize,
}
