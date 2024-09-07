use miette::Result;

use rslox::compiler::Compiler;
use rslox::disassembler::Disassembler;
use rslox::vm::Vm;

fn main() {
    let input = r#"{
  fun isEven(n) {
    if (n == 0) return true;
    return isOdd(n - 1); // expect runtime error: Undefined variable 'isOdd'.
  }

  fun isOdd(n) {
    if (n == 0) return false;
    return isEven(n - 1);
  }

  isEven(4);
}
"#;

    let compiler = Compiler::from_str(input);
    let (bytecode, constants) = compiler.compile().unwrap();
    let disassembler = Disassembler::new(bytecode, constants);
    disassembler.print();

    #[cfg(feature = "debug_trace")]
    {
        println!("DEBUG_TRACE")
    }

    let mut out = Vec::<u8>::new();
    let mut outerr = Vec::<u8>::new();
    let mut vm = Vm::with_output(&mut out, &mut outerr);
    vm.compile(input).unwrap();
    vm.run();

    println!("{}", String::from_utf8(out).unwrap());
    println!("{}", String::from_utf8(outerr).unwrap());
}
