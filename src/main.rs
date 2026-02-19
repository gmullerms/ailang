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
    let handler = builder.spawn(run).expect("failed to spawn interpreter thread");
    if let Err(e) = handler.join() {
        eprintln!("ailang panicked: {:?}", e);
        process::exit(1);
    }
}

const VERSION: &str = env!("CARGO_PKG_VERSION");

const BANNER: &str = r#"
     _    ___ _
    / \  |_ _| |    __ _ _ __   __ _
   / _ \  | || |   / _` | '_ \ / _` |
  / ___ \ | || |__| (_| | | | | (_| |
 /_/   \_\___|_____\__,_|_| |_|\__, |
                                |___/
"#;

fn run() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("{}", BANNER);
        eprintln!("  AILang v{} — A programming language for AI agents", VERSION);
        eprintln!("  Not for humans.\n");
        eprintln!("  usage: ailang <file.ai>");
        eprintln!("         ailang test <file.ai>");
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

    eprintln!("AILang v{} | {}", VERSION, file_path);

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

    let fn_count = program.functions.len();
    let test_count = program.tests.len();
    let has_entry = program.entry.is_some();

    eprintln!(
        "  parsed: {} fn | {} test | entry: {}",
        fn_count,
        test_count,
        if has_entry { "yes" } else { "no" }
    );

    if run_tests_only {
        eprintln!("running tests...");
    }

    // Interpret
    let mut interp = interpreter::Interpreter::new();
    let source_path = std::path::Path::new(file_path);
    let source_path = if source_path.is_absolute() {
        source_path.to_path_buf()
    } else {
        std::env::current_dir()
            .unwrap_or_default()
            .join(source_path)
    };
    match interp.run_with_path(program, run_tests_only, Some(&source_path)) {
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
