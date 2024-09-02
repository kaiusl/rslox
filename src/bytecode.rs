use std::borrow::Cow;
use std::fmt;

use crate::disassembler::{Disassembler, DisassemblerError};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ByteCode {
    pub code: Vec<u8>,
}

impl ByteCode {
    pub fn disassemble(&self) -> Disassembler<'_> {
        Disassembler {
            bytecode: &self.code,
            offset: 0,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum OpCode {
    Return,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    Return,
}

impl Instruction {
    pub fn op_code(&self) -> OpCode {
        match self {
            Instruction::Return => OpCode::Return,
        }
    }

    pub fn write_bytes(&self, dst: &mut Vec<u8>) {
        dst.push(self.op_code() as u8);
        match self {
            Instruction::Return => {}
        }
    }

    pub fn from_bytes(bytes: &[u8]) -> OpFromBytesResult<'_> {
        match bytes.first() {
            Some(&b) if b == OpCode::Return as u8 => OpFromBytesResult {
                op: Ok(Instruction::Return),
                offset: 1,
                remainder: bytes.get(1..).unwrap_or_default(),
            },
            _ => OpFromBytesResult {
                op: Err(DisassemblerError {
                    message: Cow::Borrowed("Unknown opcode"),
                }),
                offset: 0,
                remainder: bytes,
            },
        }
    }

    pub fn byte_len(&self) -> usize {
        1 + match self {
            Instruction::Return => 0,
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Return => write!(f, "RETURN"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpFromBytesResult<'a> {
    pub op: Result<Instruction, DisassemblerError>,
    pub offset: usize,
    pub remainder: &'a [u8],
}
