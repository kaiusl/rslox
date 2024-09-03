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
    let input = r#"print 10 + 10;
    print 1+2;"#;
    let compiler = Compiler::from_str(input);

    let bytecode = compiler.compile()?;

    let disassembler = Disassembler::new(&bytecode);
    disassembler.print();

    #[cfg(feature = "debug_trace")]
    {
        println!("DEBUG_TRACE")
    }

    let mut vm = vm::Vm::new(input, bytecode);
    vm.run()?;

    dbg!(vm);

    Ok(())
}
