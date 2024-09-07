use std::io::{BufRead, Read, Write};
use std::path::PathBuf;

use miette::Result;

use rslox::compiler::Compiler;
use rslox::disassembler::Disassembler;
use rslox::vm::Vm;

use clap::Parser;

fn main() {
    let args = Args::parse();
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
            let mut vm = Vm::new();
            vm.compile(&input).unwrap();
            vm.run(&input);
        }
        None => {
            let mut vm = Vm::new();
            let mut input = String::new();
            const PROMPT: &str = ">> ";
            loop {
                print!("{}", PROMPT);
                std::io::stdout().flush();
                std::io::stdin().read_line(&mut input);

                //let input = std::str::from_utf8(&buffer).unwrap();
                println!("input: {}", input.trim_end_matches('\n'));
                if input == "@exit" {
                    break;
                }
                vm.compile(&input);
                vm.run(&input);

                input.clear();
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
