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

fn main() {
    let input = r#"print a + 1;"#;
    
    let compiler = Compiler::from_str(input);
    let bytecode = compiler.compile().unwrap();
    let disassembler = Disassembler::new(&bytecode);
    disassembler.print();

    #[cfg(feature = "debug_trace")]
    {
        println!("DEBUG_TRACE")
    }

    let mut out = Vec::<u8>::new();
    let mut outerr = Vec::<u8>::new();
    let mut vm = vm::Vm::with_output(&mut out, &mut outerr);
    vm.compile(input).unwrap();
    vm.run();

    println!("{}", String::from_utf8(out).unwrap());
    println!("{}", String::from_utf8(outerr).unwrap());
}
