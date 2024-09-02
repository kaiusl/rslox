use std::collections::HashMap;

use crate::bytecode::{ByteCode, BytesCursor, OpCode};
use crate::common::Span;
use crate::disassembler::Disassembler;
use crate::value::Value;

use self::error::InterpretError;

pub mod error;

type Stack<T> = Vec<T>;

pub struct Vm {
    pub constants: Vec<Value>,
    pub spans: HashMap<usize, Span>,
    pub instructions: BytesCursor,
    pub stack: Stack<Value>,

    #[cfg(feature = "debug_trace")]
    pub disassembler: Option<Disassembler>,
}

impl Vm {
    pub fn new(bytecode: ByteCode) -> Vm {
        Vm {
            constants: bytecode.constants,
            spans: bytecode.spans,
            instructions: BytesCursor::new(bytecode.code),
            stack: Stack::new(),

            #[cfg(feature = "debug_trace")]
            disassembler: None,
        }
    }

    pub fn run(&mut self) -> Result<(), InterpretError> {
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
                OpCode::Add => self.binary_arithmetic_op(Self::add),
                OpCode::Subtract => self.binary_arithmetic_op(Self::subtract),
                OpCode::Multiply => self.binary_arithmetic_op(Self::multiply),
                OpCode::Divide => self.binary_arithmetic_op(Self::divide),
            }
        }

        Ok(())
    }

    #[inline]
    fn binary_arithmetic_op(&mut self, op: impl Fn(f64, f64) -> f64) {
        let rhs = self.stack.pop().unwrap();
        let lhs = self.stack.pop().unwrap();
        match (lhs, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => {
                self.stack.push(Value::Number(op(lhs, rhs)));
            }
        }
    }

    #[inline]
    fn add(lhs: f64, rhs: f64) -> f64 {
        lhs + rhs
    }

    #[inline]
    fn subtract(lhs: f64, rhs: f64) -> f64 {
        lhs - rhs
    }

    #[inline]
    fn multiply(lhs: f64, rhs: f64) -> f64 {
        lhs * rhs
    }

    #[inline]
    fn divide(lhs: f64, rhs: f64) -> f64 {
        lhs / rhs
    }
}
