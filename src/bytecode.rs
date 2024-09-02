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
        (self.constants.len() - 1)
    }

    pub fn disassemble(&self) -> Disassembler<'_> {
        Disassembler::new(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum OpCode {
    Return,
    Constant,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
    Return,
    Constant(u8),
}

impl Instruction {
    pub fn op_code(&self) -> OpCode {
        match self {
            Instruction::Return => OpCode::Return,
            Instruction::Constant(_) => OpCode::Constant,
        }
    }

    pub fn write_bytes(&self, dst: &mut Vec<u8>) {
        dst.push(self.op_code() as u8);
        // push operands
        match self {
            Instruction::Return => {}
            Instruction::Constant(idx) => {
                dst.push(*idx);
            }
        }
    }

    pub fn from_bytes(bytes: &[u8]) -> OpFromBytesResult<'_> {
        let instr = match bytes.first() {
            Some(&b) if b == OpCode::Return as u8 => Instruction::Return,
            Some(&b) if b == OpCode::Constant as u8 => {
                let idx = bytes[1];
                Instruction::Constant(idx)
            }
            _ => {
                return OpFromBytesResult {
                    op: Err(DisassemblerError {
                        message: Cow::Borrowed("Unknown opcode"),
                    }),
                    offset: 0,
                    remainder: bytes,
                }
            }
        };

        let offset = instr.byte_len();
        OpFromBytesResult {
            op: Ok(instr),
            offset,
            remainder: &bytes[offset..],
        }
    }

    pub fn byte_len(&self) -> usize {
        1 + match self {
            Instruction::Return => 0,
            Instruction::Constant(idx) => mem::size_of_val(idx),
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Return => write!(f, "RETURN"),
            Instruction::Constant(idx) => write!(f, "CONSTANT {}", idx),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpFromBytesResult<'a> {
    pub op: Result<Instruction, DisassemblerError>,
    pub offset: usize,
    pub remainder: &'a [u8],
}
