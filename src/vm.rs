use std::collections::HashMap;

use crate::bytecode::{ByteCode, BytesCursor, OpCode};
use crate::common::Span;
use crate::disassembler::Disassembler;
use crate::value::Value;

use self::error::{InterpretError, RuntimeError, RuntimeErrorKind};

pub mod error;

type Stack<T> = Vec<T>;

pub struct Vm<'a> {
    pub src: &'a str,
    pub constants: Vec<Value>,
    pub spans: HashMap<usize, Span>,
    pub instructions: BytesCursor,
    pub stack: Stack<Value>,

    #[cfg(feature = "debug_trace")]
    pub disassembler: Disassembler,
}

impl<'a> Vm<'a> {
    pub fn new(src: &'a str, bytecode: ByteCode) -> Vm<'a> {
        #[cfg(feature = "debug_trace")]
        let disassembler = Disassembler::new(&bytecode);

        Vm {
            src,
            constants: bytecode.constants,
            spans: bytecode.spans,
            instructions: BytesCursor::new(bytecode.code),
            stack: Stack::new(),

            #[cfg(feature = "debug_trace")]
            disassembler,
        }
    }

    pub fn run(&mut self) -> Result<(), InterpretError<'a>> {
        while let Some(op) = self.instructions.u8().map(OpCode::from_u8) {
            #[cfg(feature = "debug_trace")]
            {
                println!("\n/ [");
                for slot in self.stack.iter().rev() {
                    println!("/   {slot}");
                }
                println!("/ ]");
                print!("/ ");
                self.disassembler.print_next();
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
                    let value = self.stack.pop();
                    match value {
                        Some(Value::Number(v)) => {
                            self.stack.push(Value::Number(-v));
                        }
                        Some(val) => {
                            self.stack.push(val);
                            let kind = RuntimeErrorKind::InvalidOperand { expected: "number" };
                            return Err(self.runtime_error(kind, 1));
                        }
                        None => {
                            let kind = RuntimeErrorKind::MissingOperand { expected: "number" };
                            return Err(self.runtime_error(kind, 1));
                        }
                    }
                }
                OpCode::Add => self.binary_arithmetic_op(Self::add)?,
                OpCode::Subtract => self.binary_arithmetic_op(Self::subtract)?,
                OpCode::Multiply => self.binary_arithmetic_op(Self::multiply)?,
                OpCode::Divide => self.binary_arithmetic_op(Self::divide)?,
            }
        }

        Ok(())
    }

    #[inline]
    fn binary_arithmetic_op(
        &mut self,
        op: impl Fn(f64, f64) -> f64,
    ) -> Result<(), InterpretError<'a>> {
        let rhs = self.stack.pop();
        let lhs = self.stack.pop();
        match (lhs, rhs) {
            (Some(Value::Number(lhs)), Some(Value::Number(rhs))) => {
                self.stack.push(Value::Number(op(lhs, rhs)));
            }
            (Some(lhs), Some(rhs)) => {
                self.stack.push(lhs);
                self.stack.push(rhs);
                let kind = RuntimeErrorKind::InvalidOperands { expected: "number" };
                return Err(self.runtime_error(kind, 1));
            }
            (None, Some(rhs)) => {
                self.stack.push(rhs);
                let kind = RuntimeErrorKind::MissingOperand { expected: "number" };
                return Err(self.runtime_error(kind, 1));
            }
            (None, None) => {
                let kind = RuntimeErrorKind::MissingOperand { expected: "number" };
                return Err(self.runtime_error(kind, 1));
            }
            (Some(lhs), None) => unreachable!() // lhs cannot be some is rhs is already none
        }

        Ok(())
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

    fn runtime_error(&self, kind: RuntimeErrorKind, offset: usize) -> InterpretError<'a> {
        let span = self
            .spans
            .get(&(self.instructions.offset() - offset))
            .cloned()
            .unwrap();
        let err = RuntimeError {
            kind,
            span,
            src: self.src.into(),
        };
        err.into()
    }
}
