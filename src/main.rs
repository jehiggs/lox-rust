#![warn(clippy::pedantic)]

mod chunk;
mod compiler;
mod error;
mod scanner;
mod vm;

use std::env;
use std::fs;
use std::io;
use std::io::Write;

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    let vm = vm::VM::new();
    if args.len() > 2 {
        eprintln!("Too many arguments: {args:?}");
        eprintln!("Usage: rlox [file_path].");
        Err("Too many arguments".into())
    } else if let Some(file) = args.get(1) {
        run_file(vm, file)
    } else {
        repl(vm)
    }
}

fn repl(mut vm: vm::VM) -> Result<(), String> {
    let mut line = String::with_capacity(256);

    loop {
        print!("> ");
        io::stdout().flush().map_err(|err| err.to_string())?;
        io::stdin()
            .read_line(&mut line)
            .map_err(|err| err.to_string())?;
        vm.interpret(&line).map_err(|err| err.to_string())?;
        line.clear();
    }
}

fn run_file(mut vm: vm::VM, file: &str) -> Result<(), String> {
    let content = fs::read_to_string(file).map_err(|err| err.to_string())?;
    vm.interpret(&content).map_err(|err| format!("{err:?}"))
}
