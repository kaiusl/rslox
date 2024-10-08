use std::io::Write;
use std::path::PathBuf;

#[cfg(feature = "debug_disassemble")]
use rslox::compiler::Compiler;
#[cfg(feature = "debug_disassemble")]
use rslox::disassembler::Disassembler;
use rslox::stack::create_stack;
use rslox::vm::Vm;

use clap::Parser;

fn main() {
    let args = Args::parse();
    let mut stack = create_stack();
    match args.file {
        Some(file) => {
            let input = std::fs::read_to_string(file).unwrap();
            #[cfg(feature = "debug_disassemble")]
            {
                let compiler = Compiler::from_str(&input);
                let (bytecode, constants) = compiler.compile().unwrap();
                println!("\n\n<fn main>");
                let disassembler = Disassembler::new(
                    bytecode.code.into(),
                    bytecode.spans.into(),
                    constants.into(),
                );
                disassembler.print();
            }
            let mut vm = Vm::new(&mut stack);
            vm.compile(&input).unwrap();
            vm.run(&input);
        }
        None => {
            let mut vm = Vm::new(&mut stack);
            let mut buffer = String::new();
            const PROMPT: &str = ">> ";
            loop {
                print!("{}", PROMPT);
                std::io::stdout().flush().ok();
                std::io::stdin().read_line(&mut buffer).ok();

                //let input = std::str::from_utf8(&buffer).unwrap();
                let input = buffer.trim_end_matches('\n');
                println!("input: {}", input);
                match input {
                    "@exit" => break,
                    "@stack" => {
                        vm.print_stack();
                        buffer.clear();
                        continue;
                    }
                    "@globals" => {
                        vm.print_globals();
                        buffer.clear();
                        continue;
                    }
                    "@gc" => {
                        vm.run_gc();
                        buffer.clear();
                        continue;
                    }

                    _ => {}
                }
                match vm.compile(input) {
                    Ok(_) => {}
                    Err(_) => {
                        buffer.clear();
                        continue;
                    }
                }
                vm.run(input);

                buffer.clear();
            }
        }
    }
}

/// Simple program to greet a person
#[derive(clap::Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Name of the person to greet
    #[arg(short, long)]
    file: Option<PathBuf>,
}
