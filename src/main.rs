use self::bytecode::{ByteCode, Instruction};
use self::common::Span;
use self::compiler::Compiler;
use self::disassembler::{Disassembler, DisassemblerError};
use self::value::Value;

mod bytecode;
mod common;
mod compiler;
mod disassembler;
mod lexer;
mod value;
mod vm;

fn main() {
    let input = "return 1 + 2;";
    let compiler = Compiler::from_str(input);

    let bytecode = compiler.compile().unwrap();

    let disassembler = Disassembler::new(&bytecode);
    disassembler.print();

    #[cfg(feature = "debug_trace")]
    {
        println!("DEBUG_TRACE")
    }

    let mut vm = vm::Vm::new(bytecode);
    vm.run().unwrap();
}
