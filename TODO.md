# AILang TODO

## Short Term — Polish & Robustness (v0.1.x)

### Testing
- [x] Add Rust unit tests for parser (18 tests: functions, select, literals, lambda, errors, iterators, cast)
- [x] Add Rust unit tests for interpreter (42 tests: arithmetic, comparison, select laziness, builtins, maps, fold/map/filter, cast, null, logic)
- [x] Add integration tests that run each `examples/*.ai` and assert output (13 tests)
- [x] Add negative tests (parse errors, runtime errors, type mismatches)

### Missing Builtins (spec'd, easy to add)
- [x] `find` — find index of substring in text
- [x] `replace` — replace first occurrence in text
- [x] `set` — return new list with element at index replaced
- [x] `pop` — return `(new_list, last_element)` tuple
- [x] `typeof` — return type as text
- [x] `is` — type check, returns bool

### Bug Fixes & Hardening
- [x] Handle `read_line` EOF gracefully (returns error "end of input" on EOF)
- [x] Replace `unwrap()` in `main.rs` thread spawn with `.expect()`
- [x] Improve error messages with function context (`in 'funcname': ...` wrapping)
- [ ] Fix `Expr::Builtin` AST variant — parsed but never constructed; remove or wire up

### Project Quality
- [x] Expand `.gitignore` (added `.vscode/`, `.idea/`, `*.log`, `.DS_Store`, `.env`, editor swap files)
- [ ] Add CI with GitHub Actions (`cargo build`, `cargo test`, run example tests)
- [ ] Add `Cargo.toml` metadata (description, license, repository, authors)
- [x] Add more examples: builtins demo (`12_builtins_demo.ai`), Connect 4 game, binary tree invert

---

## Medium Term — Language Completeness (v0.2)

### Pipeline Operator
- [ ] Implement `|>` pipeline sugar: `val |> fn1 |> fn2` desugars to `call fn2 (call fn1 val)`
- [ ] Update parser to handle `|>` as infix operator
- [ ] Add pipeline examples to demonstrate chaining

### Iteration Extensions
- [ ] `zip` — combine two lists into list of tuples
- [ ] `flatmap` — map then flatten one level

### Module System
- [ ] Implement `#use` module loading (resolve file paths, parse, merge declarations)
- [ ] Support selective imports: `#use math {sqrt abs}`
- [ ] Enforce `_` prefix visibility (private functions not importable)
- [ ] Standard library: `std/math.ai`, `std/text.ai`, `std/list.ai`

### Error Handling
- [ ] Wire up `#err` handler blocks (parsed but not executed)
- [ ] Implement `?` propagation operator (AST exists as `Expr::Propagate`)
- [ ] Retry logic in `#err` blocks

### JSON Operations
- [ ] `jget` — navigate JSON-like nested maps/lists by path
- [ ] `jset` — set value at path in nested structure
- [ ] `jstr` — serialize value to JSON text
- [ ] `jparse` — parse JSON text into AILang values

### FFI — Foreign Function Interface (Phase 1)
- [ ] `#extern` block to declare native functions from shared libraries (.dll / .so / .dylib)
  ```
  #extern "sqlite3"
    sqlite3_open :i32 path:text db:any
    sqlite3_close :i32 db:any
  ```
- [ ] `libffi` or Rust `dlopen` integration to load shared libraries at runtime
- [ ] Type marshaling: AILang types <-> C types (i32, f64, text -> `*const c_char`, pointers as opaque `any`)
- [ ] Return value marshaling: C integers/floats/pointers back to AILang values
- [ ] Platform-aware library resolution (`.dll` on Windows, `.so` on Linux, `.dylib` on macOS)

### I/O & Files
- [ ] `read_file` / `write_file` builtins for file I/O
- [ ] `http_get` / `http_post` for basic HTTP (useful for agent tasks)
- [ ] `env_get` — read environment variables

### Performance
- [ ] Profile interpreter on larger programs (fold over 10k+ elements)
- [ ] Optimize environment cloning (currently full deep clone on every scope)
- [ ] Consider arena allocation for AST nodes
- [ ] Tail call optimization for recursive functions

### Developer Experience
- [ ] REPL mode: `ailang` with no args launches interactive prompt
- [x] Better runtime errors: show call stack / function name chain (added `in 'funcname':` wrapping)
- [ ] `--verbose` flag to trace execution (print each statement as it runs)
- [ ] Syntax highlighting files (VS Code `.tmLanguage`, tree-sitter grammar)

---

## Long Term — Platform & Ecosystem (v1.0+)

### Compiler Backend
- [ ] Design bytecode format (stack-based VM)
- [ ] Implement bytecode compiler (AST -> bytecode)
- [ ] Implement VM interpreter (faster than tree-walking)
- [ ] Optional: WASM compilation target

### Concurrency
- [ ] `async` / `await` for non-blocking operations
- [ ] `par` — parallel execution of independent expressions
- [ ] `chan` / `send` / `recv` — channel-based communication
- [ ] Thread pool or async runtime integration

### Agent Primitives
- [ ] `prompt` — call LLM with text, return text response
- [ ] `prompt_json` — call LLM, parse response as structured data
- [ ] `store_get` / `store_set` / `store_del` — persistent key-value state
- [ ] `observe` — emit metrics/events for monitoring
- [ ] `tool` — upgrade from stub to real tool execution framework

### FFI — Foreign Function Interface (Phase 2)
- [ ] Struct marshaling: map AILang `#type` fields to C struct layouts
- [ ] Callback support: pass AILang lambdas as C function pointers
- [ ] Array/buffer passing: AILang `[i32]` <-> C `int*` with length
- [ ] String encoding control: UTF-8 / UTF-16 / null-terminated
- [ ] Memory management: `alloc` / `free` wrappers for native buffers
- [ ] Prebuilt bindings for common libraries (SQLite, OpenSSL, CUDA, ONNX Runtime)
- [ ] Safety: sandboxed FFI mode that restricts which libraries can be loaded

### LSP & Tooling
- [ ] Language Server Protocol implementation (autocomplete, diagnostics)
- [ ] Formatter: canonical form enforcement (SSA naming, indentation)
- [ ] Linter: detect anti-patterns (Section 17 violations)
- [ ] `ailang fmt` — auto-format AILang source files

### Package Manager
- [ ] Package format and registry design
- [ ] `ailang install <package>` — dependency management
- [ ] Lock file for reproducible builds
- [ ] Publish workflow for sharing AILang libraries

### AI Integration
- [ ] Benchmark: LLM code generation accuracy vs Python/JS/Rust
- [ ] Fine-tune models on AILang corpus
- [ ] AILang-to-Python / Python-to-AILang transpiler
- [ ] Self-hosting: write parts of the AILang toolchain in AILang
