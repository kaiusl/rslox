use std::collections::HashMap;

use crate::bytecode::{ByteCode, ByteCursor, OpCode};
use crate::common::Span;
use crate::disassembler::{self, Disassembler};
use crate::value::Value;

use self::error::InterpretError;

pub mod error;

type Stack<T> = Vec<T>;

pub struct Vm {
    pub constants: Vec<Value>,
    pub spans: HashMap<usize, Span>,
    pub instructions: ByteCursor,
    pub stack: Stack<Value>,

    #[cfg(feature = "debug_trace")]
    pub disassembler: Option<Disassembler>,
}

impl Vm {
    pub fn new() -> Vm {
        Vm {
            constants: Vec::new(),
            spans: HashMap::new(),
            instructions: ByteCursor::new(Vec::new()),
            stack: Stack::new(),

            #[cfg(feature = "debug_trace")]
            disassembler: None,
        }
    }

    pub fn interpret(&mut self, bytecode: ByteCode) -> Result<(), InterpretError> {
        #[cfg(feature = "debug_trace")]
        {
            self.disassembler = Some(Disassembler::new(&bytecode));
        }
        self.constants = bytecode.constants;
        self.spans = bytecode.spans;
        self.instructions = ByteCursor::new(bytecode.code);

        self.run()
    }

    fn run(&mut self) -> Result<(), InterpretError> {
        while let Some(op) = self.instructions.u8().map(OpCode::from_u8) {
            #[cfg(feature = "debug_trace")]
            {
                println!("\n/ [");
                for slot in self.stack.iter().rev() {
                    println!("/   {slot}");
                }
                println!("/ ]");
                if let Some(disassembler) = &mut self.disassembler {
                    print!("/ ");
                    disassembler.print_next();
                }
            }

            match op {
                OpCode::Return => {
                    let value = self.stack.pop().unwrap();
                    println!("{}", value);
                }
                OpCode::Constant => {
                    let index = self.instructions.u8().unwrap();
                    let value = &self.constants[index as usize];
                    self.stack.push(value.clone());

                    println!("{}", value);
                }
                OpCode::Negate => {
                    let value = self.stack.pop().unwrap();
                    match value {
                        Value::Number(v) => {
                            self.stack.push(Value::Number(-v));
                        }
                    }
                }
            }
        }

        Ok(())
    }
}
