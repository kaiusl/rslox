use core::panic;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::io::{self, Write};
use std::ops::{Deref, DerefMut};

use crate::bytecode::{ByteCode, BytesCursor, OpCode};
use crate::common::Span;
use crate::disassembler::Disassembler;
use crate::value::{InternedString, Object, Value};

use self::error::{InterpretError, RuntimeError, RuntimeErrorKind};

pub mod error;

type Stack<T> = Vec<T>;

#[derive(Debug)]
pub struct Vm<'a, OUT = std::io::Stdout, OUTERR = std::io::Stderr> {
    pub src: &'a str,
    pub constants: Vec<Value>,
    pub spans: HashMap<usize, Span>,
    pub instructions: BytesCursor,
    pub stack: Stack<Value>,
    pub strings: HashSet<InternedString>,
    pub globals: HashMap<InternedString, Value>,
    output: OUT,
    outerr: OUTERR,

    #[cfg(feature = "debug_trace")]
    pub disassembler: Disassembler,
}

impl<'a> Vm<'a> {
    pub fn new() -> Self {
        Self::with_output(std::io::stdout(), std::io::stderr())
    }
}

impl<'a, OUT, OUTERR> Vm<'a, OUT, OUTERR> {
    pub fn with_output(output: OUT, outerr: OUTERR) -> Self {
        #[cfg(feature = "debug_trace")]
        let disassembler = Disassembler::new(&ByteCode::new());

        Vm {
            src: "",
            constants: Vec::new(),
            spans: HashMap::new(),
            instructions: BytesCursor::new(Vec::new()),
            stack: Stack::new(),
            strings: HashSet::new(),
            globals: HashMap::new(),
            output,
            outerr,

            #[cfg(feature = "debug_trace")]
            disassembler,
        }
    }

    pub fn compile(&mut self, input: &'a str) -> Result<(), ()>
    where
        for<'b> &'b mut OUTERR: io::Write,
    {
        self.src = input;

        let compiler = crate::compiler::Compiler::from_str(input);
        let bytecode = match compiler.compile() {
            Ok(bytecode) => bytecode,
            Err(err) => {
                let report = miette::Report::new(err.to_owned());
                writeln!(&mut self.outerr, "{report:?}").unwrap();
                return Err(());
            }
        };

        #[cfg(feature = "debug_trace")]
        {
            self.disassembler = Disassembler::new(&bytecode);
        }

        self.constants = bytecode.constants;
        self.spans = bytecode.spans;
        self.instructions = BytesCursor::new(bytecode.code);

        Ok(())
    }

    pub fn run(&mut self)
    where
        for<'b> &'b mut OUT: io::Write,
        for<'b> &'b mut OUTERR: io::Write,
    {
        if let Err(err) = self.run_core() {
            let report = miette::Report::new(err.to_owned());
            writeln!(&mut self.outerr, "{report:?}").unwrap();
        }
    }

    fn run_core(&mut self) -> Result<(), RuntimeError<'a>>
    where
        for<'b> &'b mut OUT: io::Write,
    {
        while let Some(op) = self.instructions.u8().map(OpCode::from_u8) {
            #[cfg(feature = "debug_trace")]
            {
                print!("\n/ [");
                for slot in self.stack.iter().rev() {
                    print!("/   {slot}");
                }
                println!("/ ]");
                print!("/ ");
                self.disassembler.print_next();
            }

            match op {
                OpCode::Return => {
                    let value = self.stack.pop();
                    if let Some(value) = value {
                        writeln!(&mut self.output, "RET: {}", value).unwrap();
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
                        }
                    } else {
                        panic!("tried to define global with non string identifier, it's a bug in VM or compiler")
                    };

                    let Some(value) = self.stack.last() else {
                        panic!("tried to define global with no value on stack, it's a bug in VM or compiler")
                    };

                    self.globals.insert(name, value.clone());
                    self.stack.pop();
                }
                OpCode::GetGlobal => {
                    let index = self.instructions.u8().unwrap();
                    let Some(name) = self.constants[index as usize].try_to_string() else {
                        panic!("tried to get global with non string identifier, it's a bug in VM or compiler")
                    };

                    if let Some(value) = self.globals.get(&name) {
                        self.stack.push(value.clone());
                    } else {
                        let kind = RuntimeErrorKind::UndefinedVariable { name };
                        return Err(self.runtime_error(kind, 2));
                    }
                }
                OpCode::SetGlobal => {
                    let index = self.instructions.u8().unwrap();
                    let Some(name) = self.constants[index as usize].try_to_string() else {
                        panic!("tried to set global with non string identifier, it's a bug in VM or compiler")
                    };
                    match self.globals.entry(name) {
                        Entry::Occupied(mut entry) => {
                            entry.insert(self.stack.last().unwrap().clone());
                        }
                        Entry::Vacant(entry) => {
                            let kind = RuntimeErrorKind::UndefinedVariable {
                                name: entry.into_key(),
                            };
                            return Err(self.runtime_error(kind, 2));
                        }
                    }
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
                            panic!("tried to do negate with no value on stack, it's a bug in VM or compiler")
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
                            panic!("tried to do not with no value on stack, it's a bug in VM or compiler")
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
                        _ => {
                            panic!("tried to do eq binary op with not enough values on stack, it's a bug in VM or compiler")
                        }
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
                            writeln!(&mut self.output, "{}", value).unwrap();
                        }
                        None => {
                            panic!("tried to print with no value on stack, it's a bug in VM or compiler")
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

    fn run_binary_add(&mut self) -> Result<(), RuntimeError<'a>> {
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
            _ => {
                panic!("tried to do binary add with not enough values on stack, it's a bug in VM or compiler")
            }
        }

        Ok(())
    }

    #[inline]
    fn binary_arithmetic_op(
        &mut self,
        op: impl Fn(f64, f64) -> f64,
    ) -> Result<(), RuntimeError<'a>> {
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
            _ => {
                panic!("tried to do arithmetic binary op with not enough values on stack, it's a bug in VM or compiler")
            }
        }

        Ok(())
    }

    #[inline]
    fn binary_cmp_op(&mut self, op: impl Fn(f64, f64) -> bool) -> Result<(), RuntimeError<'a>> {
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
            _ => {
                panic!("tried to do cmp binary op with not enough values on stack, it's a bug in VM or compiler")
            }
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

    fn runtime_error(&self, kind: RuntimeErrorKind, offset: usize) -> RuntimeError<'a> {
        let span = self
            .spans
            .get(&(self.instructions.offset() - offset))
            .cloned()
            .unwrap();
        RuntimeError {
            kind,
            span,
            src: self.src.into(),
        }
    }
}
