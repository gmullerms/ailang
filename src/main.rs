mod ast;
mod interpreter;
mod lexer;
mod parser;
mod token;

use std::env;
use std::fs;
use std::process;

fn main() {
    // Spawn with larger stack to support deep recursion in interpreted programs
    let builder = std::thread::Builder::new().stack_size(8 * 1024 * 1024);
    let handler = builder.spawn(run).unwrap();
    if let Err(e) = handler.join() {
        eprintln!("ailang panicked: {:?}", e);
        process::exit(1);
    }
}

fn run() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("ailang v0.1.0");
        eprintln!("usage: ailang <file.ai>");
        eprintln!("       ailang test <file.ai>");
        process::exit(1);
    }

    let (run_tests_only, file_path) = if args[1] == "test" {
        if args.len() < 3 {
            eprintln!("usage: ailang test <file.ai>");
            process::exit(1);
        }
        (true, &args[2])
    } else {
        (false, &args[1])
    };

    let source = match fs::read_to_string(file_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error reading '{}': {}", file_path, e);
            process::exit(1);
        }
    };

    // Lex
    let mut lex = lexer::Lexer::new(&source);
    let tokens = match lex.tokenize() {
        Ok(t) => t,
        Err(e) => {
            eprintln!("{}", e);
            process::exit(1);
        }
    };

    // Parse
    let mut par = parser::Parser::new(tokens);
    let program = match par.parse() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("{}", e);
            process::exit(1);
        }
    };

    if run_tests_only {
        eprintln!("running tests...");
    }

    // Interpret
    let mut interp = interpreter::Interpreter::new();
    match interp.run(program, run_tests_only) {
        Ok(val) => {
            if !matches!(val, interpreter::Value::Void) {
                println!("{}", val);
            }
        }
        Err(e) => {
            eprintln!("{}", e);
            process::exit(1);
        }
    }
}
