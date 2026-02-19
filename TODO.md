# AILang TODO

> Roadmap informed by review from Grok, ChatGPT, Gemini, and Claude.

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

### New Builtins (from LLM review)
- [x] `safe_get` — like `get` but returns null on out-of-bounds instead of error

### Bug Fixes & Hardening
- [x] Handle `read_line` EOF gracefully (returns error "end of input" on EOF)
- [x] Replace `unwrap()` in `main.rs` thread spawn with `.expect()`
- [x] Improve error messages with function context (`in 'funcname': ...` wrapping)
- [x] Fix `Expr::Builtin` AST variant — removed dead code (never constructed by parser)

### New Language Feature: `cond` (Flat Multi-Branch)
- [x] Implement `cond` expression: flat alternative to nested `select`
  ```
  v0 :text = cond (== x 0) "zero" (> x 0) "pos" "neg"
  ```
- [x] Parser: condition-value pairs, odd final element is default
- [x] Interpreter: evaluate conditions left-to-right, return first matching value (lazy)
- [x] Update SPEC.md with `cond` grammar and semantics
- [x] Add examples demonstrating `cond` vs nested `select` (`13_cond_demo.ai`)

### Project Quality
- [x] Expand `.gitignore` (added `.vscode/`, `.idea/`, `*.log`, `.DS_Store`, `.env`, editor swap files)
- [x] Add CI with GitHub Actions (`cargo build`, `cargo test`, run example tests on ubuntu + windows)
- [x] Add `Cargo.toml` metadata (description, license, repository, authors, keywords, categories)
- [x] Add more examples: builtins demo (`12_builtins_demo.ai`), Connect 4 game, binary tree invert
- [x] Document deterministic execution as a language guarantee in SPEC.md (Section 18)

---

## Medium Term — Language Completeness (v0.2)

### Tail-Call Optimization
- [x] Detect tail-position calls (last expression in function body is `call` or `select`/`cond` branches are calls)
- [x] Reuse stack frame for tail calls via trampoline pattern
- [x] Add tests: deep recursion (10k+ depth), mutual recursion, accumulator pattern (11 tests)
- [x] Document TCO guarantee in SPEC.md

### Pipeline Operator
- [x] Implement `|>` pipeline sugar: `val |> fn1 |> fn2` desugars to `call fn2 (call fn1 val)`
- [x] Parser desugaring (no new AST nodes, no interpreter changes needed)
- [x] Add pipeline examples (`15_pipeline_demo.ai`) and 9 tests (3 parser + 6 interpreter)

### Iteration Extensions
- [x] `zip` — combine two lists into list of tuples (5 tests)
- [x] `flatmap` — map then flatten one level (5 tests)

### Module System
- [x] Implement `#use` module loading (resolve file paths, parse, merge declarations)
- [x] Support selective imports: `#use math {sqrt abs}`
- [x] Enforce `_` prefix visibility (private functions not importable)
- [x] Standard library: `std/math.ai`, `std/list.ai` (6 module integration tests)

### Error Handling
- [x] Wire up `#err` handler blocks with fallback and retry logic
- [x] Implement `?` propagation operator (postfix, unwraps or propagates errors)
- [x] Add `Value::Err(String)` variant and `error` builtin for first-class errors
- [x] Add 13 error handling tests + example (`16_error_handling_demo.ai`)

### JSON Operations
- [x] `jparse` — parse JSON text into AILang values (hand-written recursive descent parser)
- [x] `jstr` — serialize value to JSON text
- [x] `jget` — navigate JSON-like nested maps/lists by path
- [x] `jset` — set value at path in nested structure
- [x] Add 13 JSON tests + example (`18_json_demo.ai`)

### FFI — Foreign Function Interface (Phase 1)
- [x] `#extern` block to declare native functions from shared libraries (.dll / .so / .dylib)
  ```
  #extern "sqlite3"
    sqlite3_open :i32 path:text db:any
    sqlite3_close :i32 db:any
  ```
- [x] `libloading` integration to load shared libraries at runtime (no libffi C dependency)
- [x] Type marshaling: AILang types <-> C types (i32, f64, text -> `*const c_char`, pointers as opaque `any`)
- [x] Return value marshaling: C integers/floats/pointers back to AILang values
- [x] Platform-aware library resolution (`.dll` on Windows, `.so` on Linux, `.dylib` on macOS)

### I/O & Files
- [x] `read_file` / `write_file` builtins for file I/O
- [x] `http_get` / `http_post` for basic HTTP via `ureq` (3 tests + `19_http_demo.ai`)
- [x] `env_get` — read environment variables
- [x] Add 4 I/O tests + example (`14_file_io_demo.ai`)

### Sandboxed Execution Mode
- [x] `--sandbox` flag that restricts I/O (blocks read_file, write_file, env_get, http_get, http_post)
- [ ] Whitelist-based permission model for agent safety
- [ ] Sandboxed mode for FFI (restrict which libraries can be loaded)

### Performance
- [ ] Profile interpreter on larger programs (fold over 10k+ elements)
- [ ] Optimize environment cloning (currently full deep clone on every scope)
- [ ] Consider arena allocation for AST nodes

### Developer Experience
- [x] REPL mode: `ailang` with no args launches interactive prompt (ASCII banner, multi-line blocks, persistent env)
- [x] Better runtime errors: show call stack / function name chain (added `in 'funcname':` wrapping)
- [x] `--verbose` / `-v` flag to trace execution (function calls, binds, tests, modules)
- [x] Syntax highlighting: VS Code extension (`editors/vscode/ailang/`) with tmLanguage grammar
- [x] Restrict overuse of `any` type — emit warnings when `any` is used outside FFI/interop

---

## Long Term — Platform & Ecosystem (v1.0+)

### Compiler Backend
- [ ] Design bytecode format (stack-based VM)
- [ ] Implement bytecode compiler (AST -> bytecode)
- [ ] Implement VM interpreter (faster than tree-walking)
- [ ] Optional: WASM compilation target

### Canonical Formatter
- [x] `ailang fmt` — auto-format AILang source files
- [x] Enforce SSA naming convention (`v0`, `v1`, ... in declaration order)
- [x] Enforce indentation (2-space body indent)
- [x] Enforce canonical operator ordering and spacing

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

### Static Analysis
- [ ] Compile-time recursion safety checks (detect recursive calls not guarded by `select`/`cond`)
- [ ] Structural hashing for memoization / caching of pure functions
- [ ] Dataflow graph export (leverage SSA form for analysis tooling)
- [ ] Linter: detect anti-patterns (Section 17 violations)

### LSP & Tooling
- [ ] Language Server Protocol implementation (autocomplete, diagnostics)

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

---

## LLM Review Summary

Suggestions were collected from **Grok**, **ChatGPT**, **Gemini**, and **Claude** and consolidated above. Key decisions:

| Suggestion | Source | Decision |
|---|---|---|
| Tail-call optimization | ChatGPT, Grok, Gemini | **Accepted** — moved to medium-term, critical for recursion-only language |
| `cond` (flat multi-branch) | Gemini | **Accepted** — short-term, eliminates nested `select` problem |
| `safe_get` builtin | ChatGPT | **Accepted** — quick win |
| Sandboxed execution mode | ChatGPT, Claude | **Accepted** — medium-term, important for agent safety |
| Canonical formatter | Gemini, ChatGPT | **Accepted** — long-term (already planned) |
| Agent primitives priority | ChatGPT | **Noted** — keep in long-term but acknowledge importance |
| Deterministic execution docs | ChatGPT, Claude | **Accepted** — short-term, document as guarantee |
| `any` type restrictions | ChatGPT | **Accepted** — medium-term, warn on overuse |
| Static analysis (recursion, hashing) | ChatGPT, Gemini | **Accepted** — long-term |
| Dataflow graph export | ChatGPT | **Accepted** — long-term |
| Rename SSA to `_n` base-36 | Gemini | **Rejected** — conflicts with `_` private prefix, marginal token gain, hurts readability |
| `\|?` error pipes | Gemini | **Rejected** — more verbose than `?`, existing `#err` blocks suffice |
| Controlled loop primitive | ChatGPT | **Rejected** — contradicts language philosophy (functional iteration only) |
