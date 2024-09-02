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
    let mut bytecode = ByteCode::new();
    let idx = bytecode.add_constant(Value::Number(1.2));
    bytecode.push(Instruction::Constant(idx as u8), Span::from_len(0, 0, 1));
    bytecode.push(Instruction::Negate, Span::from_len(0, 1, 1));
    bytecode.push(Instruction::Return, Span::from_len(1, 2, 6));

    let disassembler = Disassembler::new(&bytecode);
    disassembler.print();

    #[cfg(feature = "debug_trace")]
    {
        println!("DEBUG_TRACE")
    }

    let mut vm = vm::Vm::new();
    vm.interpret(bytecode).unwrap();
}
