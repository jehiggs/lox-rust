mod chunk;
mod scanner;
mod vm;

use std::env;
use std::fs;
use std::io;

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    let vm = vm::VM::new(chunk::Chunk::new());
    if args.len() > 1 {
        eprintln!("Too many arguments: {:?}", args);
        eprintln!("Usage: rlox [file_path].");
        Err("Too many arguments".into())
    } else if let Some(file) = args.first() {
        run_file(vm, file)
    } else {
        repl(vm)
    }
}

fn repl(mut vm: vm::VM) -> Result<(), String> {
    let mut line = String::new();

    loop {
        print!("> ");
        io::stdin()
            .read_line(&mut line)
            .map_err(|err| err.to_string())?;
        vm.interpret(&line); // TODO handle the result.
    }
}

fn run_file(mut vm: vm::VM, file: &str) -> Result<(), String> {
    let content = fs::read_to_string(file).map_err(|err| err.to_string())?;
    vm.interpret(&content).map_err(|err| format!("{:?}", err))
}
