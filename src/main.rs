use std::env::args;
use std::fs;
use std::io;
use std::io::Write;
use std::process;

#[macro_use]
extern crate num_derive;

mod chunk;
mod common;
mod compiler;
mod debug;
mod object;
mod scanner;
mod value;
mod vm;

fn main() {
    let vm = vm::VM::new();
    let mut args = args();

    if args.len() == 1 {
        repl(vm);
    } else if args.len() == 2 {
        run_file(vm, &args.nth(1).unwrap());
    } else {
        eprintln!("Usage: {} [path]", args.next().unwrap());
        process::exit(64);
    }
}

fn repl(mut vm: vm::VM) {
    let mut line = String::new();
    loop {
        line.clear();
        print!("> ");
        io::stdout().flush().expect("Could not flush stdout");
        let status = io::stdin().read_line(&mut line);
        if status.is_err() || line.is_empty() {
            println!();
            break;
        }

        vm.interpret(&line[..]);
    }
}

fn run_file(mut vm: vm::VM, name: &str) {
    let source = match fs::read_to_string(name) {
        Ok(val) => val,
        Err(msg) => {
            eprintln!("File system error: {}", msg);
            process::exit(74);
        }
    };
    match vm.interpret(&source[..]) {
        vm::InterpretResult::CompileError => process::exit(65),
        vm::InterpretResult::RuntimeError => process::exit(70),
        _ => (),
    }
}
