use std::collections::HashMap;

use crate::bytecode::{ByteCode, ByteCursor, OpCode};
use crate::common::Span;
use crate::value::Value;

use self::error::InterpretError;

pub mod error;

pub struct Vm {
    pub constants: Vec<Value>,
    pub spans: HashMap<usize, Span>,
    pub instructions: ByteCursor,
}

impl Vm {
    pub fn new() -> Vm {
        Vm {
            constants: Vec::new(),
            spans: HashMap::new(),
            instructions: ByteCursor::new(Vec::new()),
        }
    }

    pub fn interpret(&mut self, bytecode: ByteCode) -> Result<(), InterpretError> {
        self.constants = bytecode.constants;
        self.spans = bytecode.spans;
        self.instructions = ByteCursor::new(bytecode.code);

        self.run()
    }

    fn run(&mut self) -> Result<(), InterpretError> {
        while let Some(op) = self.instructions.u8().map(OpCode::from_u8) {
            match op {
                OpCode::Return => return Ok(()),
                OpCode::Constant => {
                    let index = self.instructions.u8().unwrap();
                    let value = &self.constants[index as usize];

                    println!("{}", value);
                }
            }
        }

        Ok(())
    }
}
