use miette::Result;

use self::compiler::Compiler;
use self::disassembler::Disassembler;

mod bytecode;
mod common;
mod compiler;
mod disassembler;
mod lexer;
mod value;
mod vm;

fn main() -> Result<()> {
    let input = "2*3+4;";
    let compiler = Compiler::from_str(input);

    let bytecode = compiler.compile()?;

    let disassembler = Disassembler::new(&bytecode);
    disassembler.print();

    #[cfg(feature = "debug_trace")]
    {
        println!("DEBUG_TRACE")
    }

    let mut vm = vm::Vm::new(bytecode);
    vm.run()?;

    Ok(())
}
