use std::collections::{HashMap, HashSet};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use crate::bytecode::{ByteCode, BytesCursor, OpCode};
use crate::common::Span;
use crate::disassembler::Disassembler;
use crate::value::{InternedString, Object, Value};

use self::error::{InterpretError, RuntimeError, RuntimeErrorKind};

pub mod error;

type Stack<T> = Vec<T>;

#[derive(Debug)]
pub struct Vm<'a> {
    pub src: &'a str,
    pub constants: Vec<Value>,
    pub spans: HashMap<usize, Span>,
    pub instructions: BytesCursor,
    pub stack: Stack<Value>,
    pub strings: HashSet<InternedString>,
    pub globals: HashMap<InternedString, Value>,

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
            strings: HashSet::new(),
            globals: HashMap::new(),

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
                    let value = self.stack.pop();
                    if let Some(value) = value {
                        println!("RET: {}", value);
                    }
                }
                OpCode::Constant => {
                    let index = self.instructions.u8().unwrap();
                    let value = &self.constants[index as usize];

                    // Push constant to strings table
                    if let Value::Object(obj) = value {
                        match obj.borrow_mut().deref_mut() {
                            Object::String(s) => match self.strings.get(&*s) {
                                None => {
                                    self.strings.insert(s.clone());
                                }
                                // We already have self string, make constant point to it
                                Some(existing) => *s = existing.clone(),
                            },
                            _ => {
                                todo!()
                            }
                        }
                    }
                    self.stack.push(value.clone());
                }
                OpCode::DefineGlobal => {
                    let index = self.instructions.u8().unwrap();
                    let value = &self.constants[index as usize];

                    let name = if let Value::Object(obj) = value {
                        match obj.borrow_mut().deref_mut() {
                            Object::String(s) => {
                                match self.strings.get(&*s) {
                                    None => {
                                        self.strings.insert(s.clone());
                                    }
                                    // We already have self string, make constant point to it
                                    Some(existing) => *s = existing.clone(),
                                }
                                s.clone()
                            }
                            _ => {
                                todo!()
                            }
                        }
                    } else {
                        todo!()
                    };

                    let Some(value) = self.stack.last() else {
                        todo!()
                    };

                    self.globals.insert(name, value.clone());
                    self.stack.pop();
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
                OpCode::Not => {
                    let value = self.stack.pop();
                    match value {
                        Some(value) => {
                            self.stack.push(Value::Bool(value.is_falsey()));
                        }
                        None => {
                            let kind = RuntimeErrorKind::MissingOperand { expected: "any" };
                            return Err(self.runtime_error(kind, 1));
                        }
                    }
                }
                OpCode::Add => self.run_binary_add()?,
                OpCode::Subtract => self.binary_arithmetic_op(Self::subtract_number)?,
                OpCode::Multiply => self.binary_arithmetic_op(Self::multiply_number)?,
                OpCode::Divide => self.binary_arithmetic_op(Self::divide_number)?,

                OpCode::Eq => {
                    let rhs = self.stack.pop();
                    let lhs = self.stack.pop();
                    match (lhs, rhs) {
                        (Some(lhs), Some(rhs)) => {
                            self.stack.push(Value::Bool(lhs == rhs));
                        }
                        (None, Some(rhs)) => {
                            self.stack.push(rhs);
                            let kind = RuntimeErrorKind::MissingOperand { expected: "any" };
                            return Err(self.runtime_error(kind, 1));
                        }
                        (None, None) => {
                            let kind = RuntimeErrorKind::MissingOperand { expected: "any" };
                            return Err(self.runtime_error(kind, 1));
                        }
                        (Some(lhs), None) => unreachable!(), // lhs cannot be some is rhs is already none
                    }
                }

                OpCode::Lt => self.binary_cmp_op(Self::lt_number)?,
                OpCode::Gt => self.binary_cmp_op(Self::gt_number)?,

                OpCode::Nil => self.stack.push(Value::Nil),
                OpCode::True => self.stack.push(Value::Bool(true)),
                OpCode::False => self.stack.push(Value::Bool(false)),
                OpCode::Print => {
                    let value = self.stack.pop();
                    match value {
                        Some(value) => {
                            println!("{}", value);
                        }
                        None => {
                            let kind = RuntimeErrorKind::MissingOperand { expected: "any" };
                            return Err(self.runtime_error(kind, 1));
                        }
                    }
                }

                OpCode::Pop => {
                    self.stack.pop();
                }
            }
        }

        Ok(())
    }

    fn intern_string(&mut self, s: String) -> InternedString {
        match self.strings.get(&s) {
            Some(existing) => existing.clone(),
            None => {
                let new = InternedString::new(s);
                self.strings.insert(new.clone());
                new
            }
        }
    }

    fn run_binary_add(&mut self) -> Result<(), InterpretError<'a>> {
        let op = Self::add_number;
        let rhs = self.stack.pop();
        let lhs = self.stack.pop();
        match (lhs, rhs) {
            (Some(Value::Number(lhs)), Some(Value::Number(rhs))) => {
                self.stack.push(Value::Number(op(lhs, rhs)));
            }
            (Some(Value::Object(lhs)), Some(Value::Object(rhs))) => {
                match (lhs.borrow().deref(), rhs.borrow().deref()) {
                    (Object::String(lhs), Object::String(rhs)) => {
                        let new = lhs.to_string() + rhs;
                        let new = self.intern_string(new);
                        self.stack.push(Value::new_object(Object::String(new)));
                    }
                    _ => {
                        todo!()
                    }
                }
            }
            (Some(lhs), Some(rhs)) => {
                self.stack.push(lhs);
                self.stack.push(rhs);
                let kind = RuntimeErrorKind::InvalidOperands {
                    expected: "two numbers or string",
                };
                return Err(self.runtime_error(kind, 1));
            }
            (None, Some(rhs)) => {
                self.stack.push(rhs);
                let kind = RuntimeErrorKind::MissingOperand {
                    expected: "two numbers or string",
                };
                return Err(self.runtime_error(kind, 1));
            }
            (None, None) => {
                let kind = RuntimeErrorKind::MissingOperand {
                    expected: "two numbers or string",
                };
                return Err(self.runtime_error(kind, 1));
            }
            (Some(lhs), None) => unreachable!(), // lhs cannot be some is rhs is already none
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
            (Some(lhs), None) => unreachable!(), // lhs cannot be some is rhs is already none
        }

        Ok(())
    }

    #[inline]
    fn binary_cmp_op(&mut self, op: impl Fn(f64, f64) -> bool) -> Result<(), InterpretError<'a>> {
        let rhs = self.stack.pop();
        let lhs = self.stack.pop();
        match (lhs, rhs) {
            (Some(Value::Number(lhs)), Some(Value::Number(rhs))) => {
                self.stack.push(Value::Bool(op(lhs, rhs)));
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
            (Some(lhs), None) => unreachable!(), // lhs cannot be some is rhs is already none
        }
        Ok(())
    }

    #[inline]
    fn lt_number(lhs: f64, rhs: f64) -> bool {
        lhs < rhs
    }

    #[inline]
    fn gt_number(lhs: f64, rhs: f64) -> bool {
        lhs > rhs
    }

    #[inline]
    fn add_number(lhs: f64, rhs: f64) -> f64 {
        lhs + rhs
    }

    #[inline]
    fn subtract_number(lhs: f64, rhs: f64) -> f64 {
        lhs - rhs
    }

    #[inline]
    fn multiply_number(lhs: f64, rhs: f64) -> f64 {
        lhs * rhs
    }

    #[inline]
    fn divide_number(lhs: f64, rhs: f64) -> f64 {
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
