# AILang

A programming language designed for AI agents to write, read, and maintain code. Not for humans.

AILang optimizes for how LLMs actually process tokens: SSA variables (`v0`, `v1`, `v2`), prefix notation, flat structure, canonical form, and explicit types on every line. See [MANIFESTO.md](MANIFESTO.md) for why this matters.

## Prerequisites

### Rust Toolchain

AILang is implemented in Rust. You need Rust 1.85+ (edition 2024).

**Windows (winget):**
```
winget install Rustlang.Rustup
```

**macOS / Linux:**
```
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

After installing, verify:
```
rustc --version
cargo --version
```

### C/C++ Linker (Windows only)

Rust needs a C linker. If you have Visual Studio installed, make sure the **C++ Build Tools** workload is included. If not:

```
winget install Microsoft.VisualStudio.2022.BuildTools
```

Then open **Visual Studio Installer** and add the **Desktop development with C++** workload.

## Build

```
git clone https://github.com/gmullerms/ailang.git
cd ailang
cargo build
```

The binary is at `target/debug/ailang` (or `target/debug/ailang.exe` on Windows).

For an optimized build (faster execution, smaller stack usage):
```
cargo build --release
```

Release binary: `target/release/ailang`.

## Usage

### Run a program
```
./target/debug/ailang examples/hello.ai
```

### Run tests
```
./target/debug/ailang test examples/hello.ai
```

Both modes execute the `#test` blocks. In test mode, `#entry` is skipped so interactive programs (using `read_line`) can have tests that run cleanly.

### Interactive REPL
```
./target/debug/ailang
```

Launches the interactive REPL. Define functions with `#fn` (multi-line block), evaluate expressions inline. Type `exit` to quit.

### Format a file
```
./target/debug/ailang fmt examples/hello.ai
```

Rewrites the file in canonical form: SSA variable renaming (`v0`, `v1`, ...), 2-space indentation, canonical block ordering (`#use` -> `#const` -> `#fn` -> `#test` -> `#err` -> `#entry`), and normalized spacing. Idempotent — running it twice produces the same output.

### Inspect a shared library
```
./target/debug/ailang inspect mylib.dll
./target/debug/ailang inspect C:/Windows/System32/kernel32.dll
```

Lists all exported function symbols from a shared library (.dll / .so / .dylib). Useful for discovering available functions before writing `#extern` blocks. Uses the same library resolution as `#extern` (platform-aware, searches relative to current directory).

### Flags
```
./target/debug/ailang --sandbox examples/hello.ai    # Restrict file/env/network I/O
./target/debug/ailang --verbose examples/hello.ai     # Trace execution to stderr
./target/debug/ailang -v test examples/hello.ai       # Combine flags with test mode
```

### Example output
```
$ ./target/debug/ailang test examples/hello.ai
running tests...
  PASS: add_basic
  PASS: add_negative
  PASS: square_works
  PASS: abs_positive
  PASS: abs_negative
```

```
$ ./target/debug/ailang examples/hello.ai
[info] greeting: Hello AILang
[info] 10 + 20 = 30
[info] 7 squared = 49
[info] abs(-42) = 42
[info] squares: [1 4 9 16 25]
[info] sum 1..5 = 15
```

## Project Structure

```
ailang/
  src/
    token.rs        -- Token types (sigils, keywords, operators, literals)
    lexer.rs        -- Tokenizer (source text -> token stream)
    parser.rs       -- Recursive descent parser (tokens -> AST)
    ast.rs          -- AST type definitions
    interpreter.rs  -- Tree-walking interpreter with 55+ built-in functions, TCO
    ffi.rs          -- Foreign function interface (libloading-based)
    formatter.rs    -- Canonical formatter (ailang fmt)
    warnings.rs     -- Static analysis warnings (:any type usage)
    json.rs         -- JSON parser/serializer (no dependencies)
    main.rs         -- CLI entry point + REPL
  tests/
    integration.rs  -- Integration tests for all example programs
    ffi_test_lib/   -- Rust cdylib for FFI integration tests
  std/
    math.ai         -- Standard library: math functions
    list.ai         -- Standard library: list utilities
  examples/
    hello.ai            -- Introduction: add, square, abs, map, fold
    01_two_sum.ai       -- LeetCode #1: recursive pair search
    02_fizzbuzz.ai      -- FizzBuzz with map + select chain
    03_palindrome_number.ai  -- chars -> reverse -> join comparison
    04_reverse_integer.ai    -- abs, chars, reverse, join, cast
    05_fibonacci.ai     -- fold with [a b] accumulator
    06_max_subarray.ai  -- Kadane's algorithm via fold
    07_valid_parentheses.ai  -- fold with list-as-stack
    08_merge_sorted_lists.ai -- recursive merge with lazy select
    09_climbing_stairs.ai    -- fold with [prev curr] accumulator
    10_contains_duplicate.ai -- sort then recursive adjacent check
    11_invert_binary_tree.ai -- nested lists as tree, recursive invert
    12_builtins_demo.ai     -- Showcase of find, replace, set, pop, typeof, is
    13_cond_demo.ai         -- cond expression: classify, grade, fizzbuzz
    14_file_io_demo.ai      -- read_file, write_file, env_get
    15_pipeline_demo.ai     -- Pipeline operator |> chaining
    16_error_handling_demo.ai -- error, ?, #err handlers
    17_modules_demo.ai      -- #use imports, selective imports, privacy
    18_json_demo.ai         -- jparse, jstr, jget, jset
    19_http_demo.ai         -- http_get, http_post with error handling
    20_ffi_demo.ai          -- #extern FFI: call native C functions
    connect4.ai             -- Connect 4 game: PvP and PvC with AI
  editors/
    vscode/ailang/          -- VS Code syntax highlighting extension
  SPEC.md           -- Full language specification
  MANIFESTO.md      -- Why AILang is better for LLMs than human languages
  TODO.md           -- Prioritized roadmap (short/medium/long term)
```

## Language Quick Start

### Hello World
```
#fn greet :text name:text
  = call concat "Hello " name

#entry
  v0 :text = call greet "AILang"
  log "info" "{0}" v0
  = 0
```

### Key Syntax Rules

Every function starts with `#fn` at column 0. Body is indented 2 spaces:
```
#fn add :i32 a:i32 b:i32
  = + a b
```

Intermediate values use SSA naming (`v0`, `v1`, `v2`, ...):
```
#fn hypotenuse :f64 a:f64 b:f64
  v0 :f64 = * a a
  v1 :f64 = * b b
  v2 :f64 = + v0 v1
  = call sqrt v2
```

All operations are prefix notation:
```
v0 :i32 = + 3 5        -- add
v1 :bool = > v0 4      -- compare
v2 :i32 = % v0 3       -- modulo
```

Conditionals use `select` (lazy — only evaluates the taken branch):
```
v0 :i32 = select (> x 0) x (neg x)
```

Multi-branch conditionals use `cond` (flat alternative to nested `select`):
```
v0 :text = cond (== x 0) "zero" (> x 0) "pos" "neg"
```

Pipeline operator chains function calls:
```
v0 :text = "  hello world  " |> trim |> upper
v1 :i32 = 5 |> double |> add 3
```

Error handling with `error`, `?` propagation, and `#err` handlers:
```
v0 :f64 = (call safe_divide x y)?
```

Modules import functions from other files:
```
#use "math_helpers"
#use "std/math" {sqrt abs}
```

FFI calls native C functions from shared libraries:
```
#extern "mylib"
  add_numbers :i32 a:i32 b:i32
  compute :f64 x:f64

#entry
  v0 :i32 = call add_numbers 3 4
  = 0
```

Iteration is functional — no loops:
```
v0 :[i32] = map (fn x:i32 => * x x) nums
v1 :i32 = fold nums 0 (fn acc:i32 x:i32 => + acc x)
v2 :[i32] = filter (fn x:i32 => > x 0) nums
v3 :[[any]] = zip [1 2 3] ["a" "b" "c"]
v4 :[i32] = flatmap (fn x:i32 => [x (* x x)]) nums
```

Sub-expressions as arguments need `()` grouping:
```
v0 :i32 = call foo (+ a 1) (call bar x)
```

**Every statement is one line.** No exceptions.

### For AI Code Generators

Read [SPEC.md Section 17](SPEC.md) before generating AILang. The three most common mistakes:

1. **Multi-statement lambdas** — lambdas are `(fn params => SINGLE_EXPR)`, not blocks
2. **Multi-line statements** — `select`, `fold`, `map` args must all be on one line
3. **Recursive calls in binds** — binds execute eagerly; put recursive calls inside `select` branches

## Built-in Functions

### Math
`abs`, `min`, `max`, `sqrt`

### Text
`concat`, `len`, `slice`, `upper`, `lower`, `trim`, `split`, `join`, `chars`, `char_at`, `to_text`, `fmt`, `find`, `replace`

### List
`len`, `get`, `safe_get`, `push`, `set`, `pop`, `head`, `tail`, `range`, `reverse`, `sort`, `append`, `is_empty`, `slice`, `zip`, `flatmap`

### Error
`error`

### Type
`typeof`, `is`

### Map
`mget`, `mset`, `mdel`, `mkeys`, `mvals`, `mhas`

### JSON
`jparse`, `jstr`, `jget`, `jset`

### I/O
`print`, `print_no_nl`, `read_line`, `log`, `read_file`, `write_file`, `env_get`, `http_get`, `http_post`

## Testing

AILang has 274 tests: 246 unit tests (lexer, parser, interpreter, formatter, warnings, FFI) and 28 integration tests.

```
cargo test
```

Unit tests cover arithmetic, comparison, select/cond laziness, builtins (including safe_get, file I/O, env, HTTP, zip, flatmap, JSON), pipeline operator, error handling (?/error/#err), tail-call optimization (10k+ depth recursion), map operations, fold/map/filter, cast, null handling, sandbox mode, verbose tracing, canonical formatting (SSA renaming, block ordering, expression grouping), `:any` type warnings, FFI value marshaling, and parse error detection. Integration tests run each `examples/*.ai` file as a subprocess, including module system, HTTP, formatter, warning, and FFI tests.

CI runs automatically on push and PRs via GitHub Actions (ubuntu + windows).

## Docs

- **[SPEC.md](SPEC.md)** — Full language specification with grammar, types, operations, and code generation guide
- **[MANIFESTO.md](MANIFESTO.md)** — Why human programming languages are wrong for AI, with side-by-side comparisons
- **[TODO.md](TODO.md)** — Prioritized roadmap: FFI, compiler backend, agent primitives

## Editor Support

### VS Code

A syntax highlighting extension for `.ai` files is included at `editors/vscode/ailang/`.

**Install locally:**
1. Copy (or symlink) `editors/vscode/ailang/` into your VS Code extensions directory:
   - **Windows:** `%USERPROFILE%\.vscode\extensions\ailang`
   - **macOS/Linux:** `~/.vscode/extensions/ailang`
2. Reload VS Code.

The extension provides:
- Syntax highlighting for all AILang constructs (sigils, keywords, operators, types, builtins, SSA variables, strings, numbers, comments, lambdas, pipelines)
- Line comment toggling (`--`)
- Bracket matching and auto-closing for `()`, `[]`, `{}`, and `""`
- Auto-indentation after block sigils (`#fn`, `#entry`, `#test`, etc.)

## License

MIT
