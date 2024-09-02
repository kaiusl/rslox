use self::bytecode::{ByteCode, Instruction};
use self::common::Span;
use self::disassembler::{Disassembler, DisassemblerError};
use self::value::Value;

mod bytecode;
mod common;
mod disassembler;
mod value;
mod vm;

fn main() {
    let span = Span::from_len(0, 0, 1);
    let mut bytecode = ByteCode::new();

    let idx = bytecode.add_constant(Value::Number(1.2));
    bytecode.push(Instruction::Constant(idx as u8), span.clone());

    let idx = bytecode.add_constant(Value::Number(3.4));
    bytecode.push(Instruction::Constant(idx as u8), span.clone());

    bytecode.push(Instruction::Add, span.clone());

    let idx = bytecode.add_constant(Value::Number(5.6));
    bytecode.push(Instruction::Constant(idx as u8), span.clone());

    bytecode.push(Instruction::Divide, span.clone());

    bytecode.push(Instruction::Negate, span.clone());
    bytecode.push(Instruction::Return, span.clone());

    let disassembler = Disassembler::new(&bytecode);
    disassembler.print();

    #[cfg(feature = "debug_trace")]
    {
        println!("DEBUG_TRACE")
    }

    let mut vm = vm::Vm::new();
    vm.interpret(bytecode).unwrap();
}
