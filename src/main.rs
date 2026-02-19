mod ast;
mod ffi;
mod formatter;
mod interpreter;
mod json;
mod lexer;
mod parser;
mod token;
mod warnings;

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

    // Parse flags: --help / -h, --version, --sandbox, --verbose / -v
    let mut sandboxed = false;
    let mut verbose = false;
    let mut positional: Vec<&String> = Vec::new();
    for arg in &args[1..] {
        match arg.as_str() {
            "--help" | "-h" => {
                print_help();
                return;
            }
            "--version" => {
                println!("ailang {}", VERSION);
                return;
            }
            "--sandbox" => sandboxed = true,
            "--verbose" | "-v" => verbose = true,
            _ => positional.push(arg),
        }
    }

    if positional.is_empty() {
        run_repl(sandboxed, verbose);
        return;
    }

    // Handle `fmt` subcommand: ailang fmt <file.ai>
    if positional[0] == "fmt" {
        if positional.len() < 2 {
            eprintln!("usage: ailang fmt <file.ai>");
            process::exit(1);
        }
        run_fmt(positional[1].as_str());
        return;
    }

    // Handle `inspect` subcommand: ailang inspect <library>
    if positional[0] == "inspect" {
        if positional.len() < 2 {
            eprintln!("usage: ailang inspect <library>");
            process::exit(1);
        }
        run_inspect(positional[1].as_str());
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

    eprintln!("AILang v{} | {}{}", VERSION, file_path, if sandboxed { " [sandbox]" } else { "" });

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
    let extern_count: usize = program.externs.iter().map(|e| e.functions.len()).sum();
    let has_entry = program.entry.is_some();

    eprintln!(
        "  parsed: {} fn | {} extern | {} test | entry: {}",
        fn_count,
        extern_count,
        test_count,
        if has_entry { "yes" } else { "no" }
    );

    // Check for :any type usage and emit warnings to stderr
    let any_warnings = warnings::check_any_warnings(&program);
    for w in &any_warnings {
        eprintln!("{}", w);
    }

    if run_tests_only {
        eprintln!("running tests...");
    }

    // Interpret
    let mut interp = interpreter::Interpreter::new();
    interp.sandboxed = sandboxed;
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

fn print_help() {
    println!("AILang v{}", VERSION);
    println!("A programming language designed for AI agents.");
    println!();
    println!("USAGE:");
    println!("  ailang [FLAGS] <file.ai>           Run a program");
    println!("  ailang [FLAGS] test <file.ai>      Run tests only (#test blocks)");
    println!("  ailang fmt <file.ai>               Format file in canonical form");
    println!("  ailang inspect <library>            List exported symbols from a shared library");
    println!("  ailang                              Launch interactive REPL");
    println!();
    println!("FLAGS:");
    println!("  --sandbox       Restrict file, env, network, and FFI operations");
    println!("  --verbose, -v   Trace execution to stderr");
    println!("  --help, -h      Show this help message");
    println!("  --version       Show version");
    println!();
    println!("EXAMPLES:");
    println!("  ailang examples/hello.ai");
    println!("  ailang test examples/hello.ai");
    println!("  ailang --sandbox examples/hello.ai");
    println!("  ailang -v test examples/hello.ai");
    println!("  ailang fmt examples/hello.ai");
    println!("  ailang inspect mylib.dll");
}

fn run_fmt(file_path: &str) {
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

    // Format
    let formatted = formatter::format_program(&program);

    // Write back to file
    match fs::write(file_path, &formatted) {
        Ok(()) => {
            eprintln!("formatted: {}", file_path);
        }
        Err(e) => {
            eprintln!("error writing '{}': {}", file_path, e);
            process::exit(1);
        }
    }
}

fn run_inspect(lib_name: &str) {
    use goblin::Object;

    // Resolve the library path using the same logic as FFI
    let resolved = match ffi::resolve_library_path(lib_name, Some(&std::env::current_dir().unwrap_or_default())) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("error: {}", e);
            process::exit(1);
        }
    };

    // Read the binary file
    let data = match fs::read(&resolved) {
        Ok(d) => d,
        Err(e) => {
            eprintln!("error reading '{}': {}", resolved.display(), e);
            process::exit(1);
        }
    };

    // Parse and extract exports
    let mut exports: Vec<String> = Vec::new();

    match Object::parse(&data) {
        Ok(Object::PE(pe)) => {
            for export in &pe.exports {
                if let Some(name) = export.name {
                    exports.push(name.to_string());
                }
            }
        }
        Ok(Object::Elf(elf)) => {
            for sym in &elf.dynsyms {
                if sym.is_function() && sym.st_bind() != goblin::elf::sym::STB_LOCAL {
                    if let Some(name) = elf.dynstrtab.get_at(sym.st_name) {
                        if !name.is_empty() {
                            exports.push(name.to_string());
                        }
                    }
                }
            }
        }
        Ok(Object::Mach(mach)) => {
            match mach {
                goblin::mach::Mach::Binary(macho) => {
                    for sym in macho.exports().unwrap_or_default() {
                        let name = sym.name.strip_prefix('_').unwrap_or(&sym.name);
                        exports.push(name.to_string());
                    }
                }
                goblin::mach::Mach::Fat(fat) => {
                    // Use the first architecture
                    if let Ok(arches) = fat.into_iter().collect::<Result<Vec<_>, _>>() {
                        if let Some(first) = arches.first() {
                            match first {
                                goblin::mach::SingleArch::MachO(macho) => {
                                    for sym in macho.exports().unwrap_or_default() {
                                        let name = sym.name.strip_prefix('_').unwrap_or(&sym.name);
                                        exports.push(name.to_string());
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
        }
        Ok(_) => {
            eprintln!("error: '{}' is not a recognized shared library format", resolved.display());
            process::exit(1);
        }
        Err(e) => {
            eprintln!("error parsing '{}': {}", resolved.display(), e);
            process::exit(1);
        }
    }

    exports.sort();

    println!("Exports from {} ({} symbols):", resolved.display(), exports.len());
    println!();
    for name in &exports {
        println!("  {}", name);
    }
}

fn run_repl(sandboxed: bool, verbose: bool) {
    eprintln!("{}", BANNER);
    eprintln!("  AILang v{} — Interactive REPL", VERSION);
    eprintln!("  Type expressions to evaluate. Use #fn to define functions.");
    eprintln!("  Use --sandbox to restrict file/env/network I/O.");
    eprintln!("  Type 'exit' to quit.\n");

    if sandboxed {
        eprintln!("  ** sandbox mode active: file, env, and network I/O are disabled **\n");
    }

    let mut interp = interpreter::Interpreter::new();
    interp.sandboxed = sandboxed;
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
