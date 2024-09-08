use std::borrow::Cow;
use std::collections::HashMap;
use std::rc::Rc;

use crate::bytecode::{BytesCursor, Instruction};
use crate::common::Span;
use crate::value::Value;

#[derive(Debug, Clone)]
pub struct Disassembler {
    pub constants: Rc<[Value]>,
    pub spans: Rc<HashMap<usize, Span>>,
    pub cursor: BytesCursor,
    pub prev_line: Option<usize>,
}

impl Disassembler {
    pub fn new(
        code: Rc<[u8]>,
        spans: Rc<HashMap<usize, Span>>,
        constants: Rc<[Value]>,
    ) -> Disassembler {
        Disassembler {
            constants,
            spans,
            cursor: BytesCursor::new(code),
            prev_line: None,
        }
    }

    pub fn print(mut self) {
        while !self.cursor.is_empty() {
            self.print_next();
        }
    }

    pub fn print_next(&mut self) {
        let Some(result) = self.next() else { return };

        match result {
            Ok((offset, op)) => {
                let line = self.spans[&offset].line;
                'print_line: {
                    if let Some(prev_line) = self.prev_line {
                        if prev_line == line {
                            print!("   |");
                            break 'print_line;
                        }
                    }

                    print!("{:04}", line);
                }
                self.prev_line = Some(line);

                print!(" {:04} {}", offset, op);
                if let Instruction::Constant(idx) | Instruction::DefineGlobal(idx) = op {
                    print!(" ({})", &self.constants[idx as usize]);
                }

                if let Instruction::Closure(idx) = op {
                    let fun = &self.constants[idx as usize].try_to_function().unwrap();
                    for _ in 0..fun.upvalues_count {
                        let is_local = self.cursor.u8().unwrap();
                        let index = self.cursor.u8().unwrap();
                        print!(
                            "\n{line:04}      |    {} {}",
                            if is_local == 1 { "local" } else { "upvalue" },
                            index
                        );
                    }
                }

                println!();
            }
            Err(err) => println!("Error: '{}'", err.message),
        }
    }
}

impl Iterator for Disassembler {
    type Item = Result<(usize, Instruction), DisassemblerError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cursor.is_empty() {
            return None;
        }
        let offset = self.cursor.offset();
        let result = Instruction::from_bytes(&mut self.cursor);
        match result {
            Ok(op) => Some(Ok((offset, op))),
            Err(err) => Some(Err(err)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DisassemblerError {
    pub message: Cow<'static, str>,
}
