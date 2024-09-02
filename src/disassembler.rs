use std::borrow::Cow;

use crate::bytecode::Instruction;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Disassembler<'a> {
    pub bytecode: &'a [u8],
    pub offset: usize,
}

impl Disassembler<'_> {
    pub fn new(bytecode: &[u8]) -> Disassembler<'_> {
        Disassembler {
            bytecode,
            offset: 0,
        }
    }

    pub fn print(mut self) {
        for result in self {
            match result {
                Ok((offset, op)) => println!("{:04} {:?}", offset, op),
                Err(err) => println!("Error: '{}'", err.message),
            }
        }
    }
}

impl<'a> Iterator for Disassembler<'a> {
    type Item = Result<(usize, Instruction), DisassemblerError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.bytecode.is_empty() {
            None
        } else {
            let result = Instruction::from_bytes(self.bytecode);
            self.bytecode = result.remainder;
            match result.op {
                Ok(op) => {
                    let ret = Some(Ok((self.offset, op)));
                    self.offset += result.offset;
                    ret
                }
                Err(err) => Some(Err(err)),
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DisassemblerError {
    pub message: Cow<'static, str>,
}

#[cfg(test)]
mod tests {
    use crate::bytecode::OpCode;

    use super::*;

    #[test]
    fn it_disassembles() {
        let mut disassembler = Disassembler {
            bytecode: &[OpCode::Return as u8, OpCode::Return as u8],
            offset: 0,
        };

        disassembler.clone().print();

        assert_eq!(disassembler.next(), Some(Ok((0, Instruction::Return))));
        assert_eq!(disassembler.next(), Some(Ok((1, Instruction::Return))));
        assert_eq!(disassembler.next(), None);
    }
}
