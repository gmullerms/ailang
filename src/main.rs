mod ast;
mod interpreter;
mod json;
mod lexer;
mod parser;
mod token;

use std::env;
use std::fs;
use std::io::{self, BufRead, Write};
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

    // Parse flags: --verbose / -v
    let mut verbose = false;
    let mut positional: Vec<&String> = Vec::new();
    for arg in &args[1..] {
        match arg.as_str() {
            "--verbose" | "-v" => verbose = true,
            _ => positional.push(arg),
        }
    }

    if positional.is_empty() {
        run_repl(verbose);
        return;
    }

    let (run_tests_only, file_path) = if positional[0] == "test" {
        if positional.len() < 2 {
            eprintln!("usage: ailang test <file.ai>");
            process::exit(1);
        }
        (true, positional[1].as_str())
    } else {
        (false, positional[0].as_str())
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
    interp.verbose = verbose;
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

fn run_repl(verbose: bool) {
    eprintln!("{}", BANNER);
    eprintln!("  AILang v{} — Interactive REPL", VERSION);
    eprintln!("  Type expressions to evaluate. Use #fn to define functions.");
    eprintln!("  Type 'exit' to quit.\n");

    let mut interp = interpreter::Interpreter::new();
    interp.verbose = verbose;
    let mut env = interpreter::Env::new();
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines();

    loop {
        // Print prompt
        eprint!("ai> ");
        io::stderr().flush().ok();

        // Read a line
        let line = match lines.next() {
            Some(Ok(line)) => line,
            Some(Err(e)) => {
                eprintln!("error reading input: {}", e);
                break;
            }
            None => {
                // EOF (Ctrl+D / Ctrl+Z)
                eprintln!();
                break;
            }
        };

        let trimmed = line.trim();

        // Handle empty lines
        if trimmed.is_empty() {
            continue;
        }

        // Handle exit command
        if trimmed == "exit" {
            break;
        }

        // Check if this is a multi-line block (starts with #)
        let source = if trimmed.starts_with('#') {
            // Multi-line mode: collect lines until blank line or new # at column 0
            let mut block = line.clone();
            block.push('\n');

            loop {
                eprint!("..  ");
                io::stderr().flush().ok();

                match lines.next() {
                    Some(Ok(next_line)) => {
                        // Blank line or new sigil at column 0 ends the block
                        if next_line.trim().is_empty() {
                            break;
                        }
                        if next_line.starts_with('#') {
                            // New sigil — this ends current block.
                            // We do NOT include this line in the current block.
                            // For simplicity, just end current block here.
                            // The user will need to type the next block separately.
                            break;
                        }
                        block.push_str(&next_line);
                        block.push('\n');
                    }
                    Some(Err(_)) => break,
                    None => {
                        // EOF during multi-line input
                        eprintln!();
                        break;
                    }
                }
            }

            block
        } else {
            line.clone()
        };

        // Lex
        let mut lex = lexer::Lexer::new(&source);
        let tokens = match lex.tokenize() {
            Ok(t) => t,
            Err(e) => {
                eprintln!("error: {}", e);
                continue;
            }
        };

        // Parse as REPL input
        let mut par = parser::Parser::new(tokens);
        let input = match par.parse_repl() {
            Ok(i) => i,
            Err(e) => {
                eprintln!("error: {}", e);
                continue;
            }
        };

        // Evaluate
        match input {
            parser::ReplInput::Block(program) => {
                match interp.register_program(program, &mut env) {
                    Ok(()) => {}
                    Err(e) => {
                        eprintln!("error: {}", e);
                    }
                }
            }
            parser::ReplInput::Stmt(stmt) => {
                match interp.eval_stmt(&stmt, &mut env) {
                    Ok(val) => {
                        if !matches!(val, interpreter::Value::Void) {
                            println!("{}", val);
                        }
                    }
                    Err(e) => {
                        eprintln!("error: {}", e);
                    }
                }
            }
            parser::ReplInput::Empty => {}
        }
    }
}
