#![feature(strict_provenance)]

pub mod bytecode;
pub mod common;
pub mod compiler;
pub mod disassembler;
pub mod lexer;
pub mod stack;
pub mod value;
pub mod vm;

pub use vm::Vm;
