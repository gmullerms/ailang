# AILang Specification v0.1

## 1. Philosophy

AILang is a programming language designed exclusively for AI agents to write, read, maintain, and execute. It is **not** designed for human readability. Every design decision optimizes for how large language models process tokens.

### Core Principles

1. **Token efficiency** — maximize semantic density per token
2. **Uniform structure** — every construct follows the same shape; zero special cases
3. **Flat over nested** — maximum nesting depth is 1; pipelines replace nesting
4. **Immutable data flow** — SSA (Single Static Assignment); every value bound once
5. **Prefix notation** — all operations are prefix; no operator precedence rules
6. **No forward references** — everything is defined before use; entry point is last
7. **Canonical form** — exactly one way to express any program; no style variations
8. **Explicit types everywhere** — every binding carries its type; no inference tracing
9. **Line independence** — each line is a complete statement; one change = one line
10. **Agent-native** — first-class tool calls, structured data, async, retry

---

## 2. Lexical Structure

### 2.1 Encoding
UTF-8. One statement per line. No semicolons. No significant whitespace beyond block membership.

### 2.2 Sigils (Block Markers)
Blocks begin at column 0 with a sigil. Statements inside blocks are indented by exactly 2 spaces (cosmetic only — parsing uses sigil boundaries).

| Sigil   | Purpose            |
|---------|--------------------|
| `#fn`   | Function           |
| `#type` | Type definition    |
| `#enum` | Enum definition    |
| `#const`| Constant           |
| `#use`  | Import             |
| `#entry`| Program entry      |
| `#test` | Test block         |
| `#err`  | Error handler      |

A block ends when the next sigil appears at column 0, or at end of file.

### 2.3 Value Names
All intermediate values use SSA naming: `v0`, `v1`, `v2`, etc. Reset per block. This eliminates naming decisions entirely — the AI never wastes tokens choosing variable names inside functions.

Parameters use short lowercase names: `a`, `b`, `x`, `n`, `src`, `dst`.

### 2.4 Literals

| Type    | Example                          | Status      |
|---------|----------------------------------|-------------|
| Integer | `42`, `-7`, `0xFF`               | Implemented |
| Float   | `3.14`, `-0.5`                   | Implemented |
| Bool    | `true`, `false`                  | Implemented |
| Text    | `"hello"`, `"line\nbreak"`       | Implemented |
| List    | `[1 2 3]`                        | Implemented |
| Map     | `{"key" 42 "other" 99}`          | Implemented |
| Tuple   | `(1 "hello" true)`               | Implemented |
| Null    | `null`                           | Implemented |
| Bytes   | `0b[FF A0 3C]`                   | Planned     |

Note: list/map/tuple elements are space-separated, not comma-separated. Commas are wasted tokens.

### 2.5 Comments
`--` starts a line comment. Comments are metadata for the AI maintaining the code, not for human explanation.

### 2.6 Grouped Expressions
When a compound expression (operator, `call`, `select`, etc.) is needed as an argument inside another expression, wrap it in parentheses:

```
v0 :i32 = call foo (+ a 1) (call bar x)
v1 :bool = == (call len lst) 0
```

Parentheses here create a **grouped expression** — the inner expression is fully parsed and evaluated before being passed as an argument. This is the standard mechanism for nesting sub-expressions within arguments; without grouping, the parser consumes atoms greedily.

The parser distinguishes grouped expressions from tuples by checking whether the first token after `(` is an expression-starting keyword (operator, `call`, `select`, etc.). If so, it's a grouped expression. Otherwise, it's a tuple literal.

---

## 3. Type System

### 3.1 Primitives

| Type   | Description              |
|--------|--------------------------|
| `i32`  | 32-bit signed integer    |
| `i64`  | 64-bit signed integer    |
| `f32`  | 32-bit float             |
| `f64`  | 64-bit float             |
| `bool` | Boolean                  |
| `text` | UTF-8 string             |
| `byte` | Single byte              |
| `void` | No value                 |
| `any`  | Dynamic type             |

### 3.2 Compound Types

| Syntax          | Description                  |
|-----------------|------------------------------|
| `[T]`           | List of T                    |
| `{K:V}`         | Map from K to V              |
| `(T1 T2 T3)`   | Tuple                        |
| `?T`            | Optional (T or null)         |
| `!T`            | Result (T or error)          |
| `(T1 T2 -> T3)` | Function type               |

### 3.3 Custom Types

```
#type Point
  x :f64
  y :f64

#type HttpResponse
  status :i32
  headers :{text:text}
  body :text
```

### 3.4 Enums

```
#enum Color
  Red
  Green
  Blue

#enum Shape
  Circle :f64
  Rect :f64 :f64
  Point
```

### 3.5 No Implicit Conversions
All conversions are explicit via `cast`:
```
v0 :i32 = 42
v1 :f64 = cast f64 v0
```

---

## 4. Functions

### 4.1 Definition

```
#fn NAME :RETURN_TYPE PARAM1:TYPE1 PARAM2:TYPE2
  BODY
```

### 4.2 Body Statements

| Form                  | Meaning                              |
|-----------------------|--------------------------------------|
| `vN :TYPE = EXPR`     | Bind value (SSA)                     |
| `= EXPR`              | Return                               |
| `> EXPR`              | Emit/yield (for generators)          |

Every value binding includes its type. This is redundant for a compiler but critical for the AI — it can read any line in isolation and know what type `v3` is without tracing backwards.

### 4.3 Example

```
#fn add :i32 a:i32 b:i32
  = + a b

#fn hypotenuse :f64 a:f64 b:f64
  v0 :f64 = * a a
  v1 :f64 = * b b
  v2 :f64 = + v0 v1
  = sqrt v2

#fn greet :text name:text
  = concat "Hello " name
```

### 4.4 Function Calls
Always prefix, always explicit:
```
v0 :i32 = call add 3 5
v1 :text = call greet "World"
```

The `call` keyword is used for invoking both user-defined functions and built-in functions:
```
v0 :i32 = call add 3 5         -- user-defined
v1 :i32 = call len "hello"     -- built-in
v2 :i32 = call abs -7          -- built-in
```

Built-in *operators* (arithmetic, comparison, logic) don't use `call`:
```
v0 :i32 = + 3 5
v1 :bool = > v0 4
```

The distinction: `call` = named function (user-defined or built-in). No `call` = operator.

### 4.5 Lambdas / Closures

Anonymous functions use the syntax `(fn PARAMS => BODY)`:
```
(fn x:i32 => * x x)
(fn a:i32 b:i32 => + a b)
(fn s:text => == (call len s) 0)
```

Lambdas capture the enclosing environment (closures). They are commonly used as arguments to functional iteration:
```
v0 :[i32] = map (fn x:i32 => * x x) nums
v1 :i32 = fold v0 0 (fn acc:i32 x:i32 => + acc x)
v2 :[i32] = filter (fn x:i32 => > x 0) nums
```

---

## 5. Operations

### 5.1 Arithmetic (prefix)
```
+ a b       -- add
- a b       -- subtract
* a b       -- multiply
/ a b       -- divide
% a b       -- modulo
neg a       -- negate
```

### 5.2 Comparison (prefix)
```
== a b      -- equal
!= a b      -- not equal
< a b       -- less than
> a b       -- greater than
<= a b      -- less or equal
>= a b      -- greater or equal
```

### 5.3 Logic (prefix)
```
and a b
or a b
not a
```

### 5.4 Bitwise (prefix)
```
band a b
bor a b
bxor a b
bnot a
shl a n
shr a n
```

### 5.5 Text Operations

| Operation               | Description                          | Status      |
|-------------------------|--------------------------------------|-------------|
| `call concat a b`       | Concatenate two texts                | Implemented |
| `call len a`            | Length (chars)                        | Implemented |
| `call slice a start end` | Substring by index range            | Implemented |
| `call upper a`          | Uppercase                            | Implemented |
| `call lower a`          | Lowercase                            | Implemented |
| `call trim a`           | Strip leading/trailing whitespace    | Implemented |
| `call split a delim`    | Split into list by delimiter         | Implemented |
| `call join lst delim`   | Join list into text with delimiter   | Implemented |
| `call chars a`          | Split text into list of single chars | Implemented |
| `call char_at a idx`    | Get character at index               | Implemented |
| `call to_text a`        | Convert any value to text            | Implemented |
| `call fmt tpl a b`      | Format with positional `{0}` `{1}`   | Implemented |
| `call find a pattern`   | Find index (-1 if not found)         | Planned     |
| `call replace a old new`| Replace first occurrence             | Planned     |

### 5.6 List Operations

| Operation                 | Description                              | Status      |
|---------------------------|------------------------------------------|-------------|
| `call len lst`            | Length                                    | Implemented |
| `call get lst idx`        | Get element (null if out of range)        | Implemented |
| `call push lst val`       | Return new list with element appended     | Implemented |
| `call head lst`           | First element (null if empty)             | Implemented |
| `call tail lst`           | All but first (empty list if len <= 1)    | Implemented |
| `call range start end`    | Generate list `[start..end)`              | Implemented |
| `call reverse lst`        | Reverse a list                            | Implemented |
| `call sort lst`           | Sort list (integers only)                 | Implemented |
| `call append a b`         | Concatenate two lists                     | Implemented |
| `call is_empty lst`       | Check if list/text/null is empty          | Implemented |
| `call slice lst start end`| Sub-list by index range                   | Implemented |
| `call set lst idx val`    | Return new list with element changed      | Planned     |
| `call pop lst`            | Return (new_list, last_element)           | Planned     |

### 5.7 Math Operations

| Operation          | Description                        | Status      |
|--------------------|------------------------------------|-------------|
| `call abs x`       | Absolute value (i32 or f64)        | Implemented |
| `call min a b`     | Minimum of two numbers             | Implemented |
| `call max a b`     | Maximum of two numbers             | Implemented |
| `call sqrt x`      | Square root (returns f64)          | Implemented |

### 5.8 I/O Operations

| Operation            | Description           | Status      |
|----------------------|-----------------------|-------------|
| `call print args...` | Print to stdout       | Implemented |

### 5.9 Map Operations

| Operation          | Description                        | Status      |
|--------------------|------------------------------------|-------------|
| `call mget m key`  | Get value (null if not found)      | Implemented |
| `call mset m key val` | Return new map with key set     | Implemented |
| `call mdel m key`  | Return new map with key removed    | Implemented |
| `call mkeys m`     | List of keys                       | Implemented |
| `call mvals m`     | List of values                     | Implemented |
| `call mhas m key`  | Bool                               | Implemented |

### 5.10 Type Operations

| Operation          | Description                        | Status      |
|--------------------|------------------------------------|-------------|
| `cast TYPE val`    | Explicit type conversion            | Implemented |
| `typeof val`       | Returns type as text                | Planned     |
| `is TYPE val`      | Type check, returns bool            | Planned     |

---

## 6. Control Flow

**No nesting. Ever.** All control flow is flat expressions.

### 6.1 Select (Conditional)
```
select CONDITION THEN_VAL ELSE_VAL
```

Select uses **lazy evaluation** — only the chosen branch is evaluated. This is critical for recursive algorithms where the non-taken branch might diverge.

Example:
```
#fn abs :i32 x:i32
  v0 :bool = < x 0
  v1 :i32 = neg x
  v2 :i32 = select v0 v1 x
  = v2
```

### 6.2 Match (Pattern Matching)
```
match VALUE
  PATTERN1 => EXPR1
  PATTERN2 => EXPR2
  _ => DEFAULT_EXPR
```

Example:
```
#fn describe :text shape:Shape
  v0 :text = match shape
    Circle r => fmt "circle r={0}" r
    Rect w h => fmt "rect {0}x{1}" w h
    Point => "point"
    _ => "unknown"
  = v0
```

Match is the **only** multi-line expression and it uses `=>` to keep each case on one line. It is still flat — no nesting of matches.

### 6.3 Iteration (Functional Only)
No loops. No while. No for. Everything is functional:

| Operation             | Description                            | Status      |
|-----------------------|----------------------------------------|-------------|
| `map FN LIST`         | Apply FN to each element               | Implemented |
| `filter FN LIST`      | Keep elements where FN returns true    | Implemented |
| `fold LIST INIT FN`   | Reduce left with accumulator           | Implemented |
| `each LIST FN`        | Side-effect iteration (returns void)   | Implemented |
| `zip LISTA LISTB`     | Combine into list of tuples            | Planned     |
| `flatmap FN LIST`     | Map then flatten                       | Planned     |

Example:
```
#fn sum_squares :i32 nums:[i32]
  v0 :[i32] = map (fn x:i32 => * x x) nums
  v1 :i32 = fold v0 0 (fn acc:i32 x:i32 => + acc x)
  = v1
```

### 6.4 Pipelines
The `|>` operator threads values through a chain of operations:

```
#fn process :text raw:text
  v0 :text = |> raw (lower) (call trim) (call truncate 100)
  = v0
```

This is sugar for:
```
v0 :text = lower raw
v1 :text = call trim v0
v2 :text = call truncate v1 100
```

The pipeline form is more token-efficient when operations are simple transforms.

**Status:** Planned

---

## 7. Error Handling

### 7.1 Result Type
Functions that can fail return `!T` (Result type):
```
#fn parse_int :!i32 s:text
  ...
```

### 7.2 Try/Unwrap
```
v0 :!i32 = call parse_int "42"
v1 :i32 = unwrap v0 0               -- unwrap with default
v2 :i32 = unwrap v0 panic            -- unwrap or crash
```

**Status:** Implemented (try, unwrap, ok wrap)

### 7.3 Try Block
`try` converts any expression into a `!T`:
```
v0 :!text = try call fetch_url "https://example.com"
```

**Status:** Implemented

### 7.4 Error Propagation
`?` after an expression propagates errors upward (early return):
```
#fn load_config :!Config path:text
  v0 :text = call read_file path ?
  v1 :Config = call parse_json v0 ?
  = ok v1
```

The `?` on a line means: if this returns an error, immediately return that error from the enclosing function.

**Status:** Planned

### 7.5 Error Handler Blocks
```
#fn fetch_data :text url:text
  v0 :text = call http_get url
  = v0

#err fetch_data
  retry 3 1000
  fallback "default_value"
```

Error handlers are separate blocks attached by name. This keeps the happy path clean. The AI can modify error handling without touching business logic.

`retry COUNT DELAY_MS` — retry the function
`fallback VALUE` — return default on failure

**Status:** Parsed but not executed at runtime

---

## 8. Modules and Imports

### 8.1 Import
```
#use io
#use net/http
#use math {sqrt sin cos}
```

Imports are always at the top of a file. Selective imports use `{}` to list names.

### 8.2 Module Declaration
Each file is a module. The filename is the module name. No explicit module declaration needed.

### 8.3 Visibility
All `#fn` and `#type` definitions are public by default. Prefix with `_` to make private:
```
#fn _helper :i32 x:i32
  = + x 1
```

**Status:** `#use` is parsed but module loading is not yet implemented.

---

## 9. Concurrency

### 9.1 Async
```
v0 :async!text = async call fetch url
v1 :text = await v0
```

### 9.2 Parallel Execution
```
v0 :(text text text) = par
  call fetch url1
  call fetch url2
  call fetch url3
```

`par` executes all expressions concurrently and returns a tuple of results. Again: flat, no nesting.

### 9.3 Channels
```
v0 :chan!i32 = chan 10               -- buffered channel, capacity 10
send v0 42                           -- send value
v1 :i32 = recv v0                    -- receive value
```

**Status:** All concurrency features are planned. Keywords are reserved in the lexer.

---

## 10. Agent Primitives

This is what makes AILang unique — first-class support for AI agent operations.

### 10.1 Tool Calls
```
v0 :any = tool "bash" {"command" "ls -la"}
v1 :any = tool "read_file" {"path" "/etc/config"}
v2 :any = tool "http" {"method" "GET" "url" "https://api.example.com"}
```

`tool` is a built-in that invokes external tools by name with a parameter map. This is the primary way agents interact with the outside world.

**Status:** Implemented (stub — logs tool name and params, returns placeholder text)

### 10.2 Structured Data (First-Class JSON)
```
v0 :any = json {"name" "Alice" "age" 30 "tags" ["admin" "user"]}
v1 :?text = jget v0 "name"          -- JSON get
v2 :any = jset v0 "age" 31          -- JSON set (returns new)
v3 :text = jstr v0                   -- serialize to text
v4 :any = jparse "{\"key\":1}"      -- parse from text
```

**Status:** Planned. Map literals `{"key" value}` are implemented; JSON-specific operations are not.

### 10.3 Prompt / LLM Call
```
v0 :text = prompt "Summarize this: {0}" content
v1 :any = prompt_json "Extract fields: {0}" content schema
```

Agent programs can invoke LLM calls as a primitive. `prompt` returns text, `prompt_json` returns structured data matching a schema.

**Status:** Planned. Keyword is reserved in the lexer.

### 10.4 Log and Observe
```
log "info" "Processing item {0}" item_id
log "error" "Failed: {0}" err_msg
observe "metric" "latency_ms" 42
```

**Status:** `log` is implemented (outputs to stderr with `[level] message` format, supports `{N}` positional args). `observe` is planned.

### 10.5 State Store
Agents need persistent state across invocations:
```
v0 :?text = store_get "last_run"
store_set "last_run" "2024-01-15"
store_del "temp_key"
```

**Status:** Planned.

---

## 11. Entry Point

Every program has exactly one `#entry` block. It is always the **last** block in the file (enforced — no forward references).

```
#entry
  v0 :text = call fetch_data "https://api.example.com/data"
  v1 :any = call parse_json v0
  v2 :text = call summarize v1
  log "info" "Result: {0}" v2
  = v2
```

---

## 12. Tests

```
#test add_basic
  v0 :i32 = call add 2 3
  assert == v0 5

#test add_negative
  v0 :i32 = call add -1 1
  assert == v0 0

#test fetch_handles_error
  v0 :!text = try call fetch_data "invalid://url"
  assert is_err v0
```

Tests are flat blocks. `assert EXPR` — if the expression evaluates to `false` or is an error, the test fails. No test frameworks, no ceremony.

Run tests with `ailang test <file.ai>`. Both tests and the `#entry` block execute. Test results are printed to stderr with `PASS:` or `FAIL:` prefixes.

---

## 13. Complete Example Program

```
#use net/http
#use text {split trim}

#type Article
  title :text
  body :text
  word_count :i32

#fn count_words :i32 s:text
  v0 :[text] = call split s " "
  v1 :i32 = call len v0
  = v1

#fn fetch_article :!Article url:text
  v0 :text = call http_get url ?
  v1 :any = jparse v0 ?
  v2 :text = jget v1 "title" ?
  v3 :text = jget v1 "body" ?
  v4 :text = call trim v3
  v5 :i32 = call count_words v4
  v6 :Article = Article v2 v4 v5
  = ok v6

#fn process_urls :![Article] urls:[text]
  v0 :[!Article] = map fetch_article urls
  = v0

#test count_words_basic
  v0 :i32 = call count_words "hello world foo"
  assert == v0 3

#entry
  v0 :[text] = ["https://example.com/1" "https://example.com/2"]
  v1 :![Article] = call process_urls v0
  v2 :[Article] = unwrap v1 []
  each v2 (fn a:Article => log "info" "{0}: {1} words" a.title a.word_count)
  = 0
```

---

## 14. Grammar Summary (BNF-like)

```
program     = block*
block       = fn_block | type_block | enum_block | const_block
            | use_block | entry_block | test_block | err_block

fn_block    = "#fn" NAME ":" TYPE (PARAM ":" TYPE)* NEWLINE stmt*
type_block  = "#type" NAME NEWLINE (INDENT NAME ":" TYPE NEWLINE)*
enum_block  = "#enum" NAME NEWLINE (INDENT NAME (":" TYPE)* NEWLINE)*
const_block = "#const" NAME ":" TYPE "=" LITERAL
use_block   = "#use" PATH ("{" NAME* "}")?
entry_block = "#entry" NEWLINE stmt*
test_block  = "#test" NAME NEWLINE stmt*
err_block   = "#err" NAME NEWLINE err_stmt*

stmt        = bind_stmt | return_stmt | emit_stmt | effect_stmt
bind_stmt   = NAME ":" TYPE "=" expr
return_stmt = "=" expr
emit_stmt   = ">" expr
effect_stmt = EFFECT_FN args            -- log, send, store_set, each, assert

expr        = op_expr | call_expr | select_expr | match_expr
            | iter_expr | cast_expr | try_expr | unwrap_expr
            | tool_expr | log_expr | assert_expr
            | grouped_expr | lambda_expr | literal | NAME

op_expr     = OP expr expr              -- prefix binary
call_expr   = "call" NAME atom*         -- function call
select_expr = "select" expr expr expr   -- conditional (lazy)
match_expr  = "match" atom NEWLINE (INDENT PATTERN "=>" expr NEWLINE)*
iter_expr   = ("map" | "filter") atom atom
            | "fold" atom atom atom
            | "each" atom atom
cast_expr   = "cast" TYPE atom
try_expr    = "try" expr
unwrap_expr = "unwrap" expr expr
tool_expr   = "tool" atom atom
log_expr    = "log" atom atom atom*
assert_expr = "assert" expr

grouped_expr = "(" expr ")"             -- sub-expression grouping
lambda_expr  = "(" "fn" (PARAM ":" TYPE)* "=>" expr ")"
             -- anonymous function / closure

atom        = INT | FLOAT | BOOL | TEXT | NULL | NAME
            | "[" atom* "]"             -- list
            | "{" (atom atom)* "}"      -- map
            | "(" atom* ")"            -- tuple (when not grouped/lambda)
            | NAME "." NAME             -- field access

LITERAL     = INT | FLOAT | BOOL | TEXT | LIST | MAP | TUPLE | NULL
NAME        = [a-z_][a-z0-9_]*
TYPE        = PRIM_TYPE | "[" TYPE "]" | "{" TYPE ":" TYPE "}"
            | "?" TYPE | "!" TYPE | "(" TYPE* "->" TYPE ")" | NAME
```

---

## 15. Why This Design (For The AI)

| Design Choice           | Why It Helps AI                                              |
|-------------------------|--------------------------------------------------------------|
| SSA variables (v0..vN)  | Zero naming decisions; deterministic generation              |
| Prefix notation         | No precedence rules; no ambiguity                            |
| Explicit types on every line | Read any line in isolation; no tracing needed          |
| No nesting              | Cannot lose track of depth                                   |
| One statement per line  | One change = one line diff                                   |
| Sigil-delimited blocks  | Block boundaries are visually obvious tokens                 |
| No forward references   | Generate top-to-bottom; never needs to look ahead            |
| Functional iteration    | No mutable loop counters to track                            |
| Separate error handlers | Modify error handling without touching logic                  |
| Canonical form          | Never wastes tokens deciding between equivalent expressions  |
| Tool calls as primitive | Agent operations are first-class, not library hacks          |
| `call` keyword          | Always distinguishes function calls from operators            |
| Grouped expressions     | Explicit sub-expression boundaries; no precedence needed     |
| Lazy select             | Safe recursion; only evaluates the taken branch              |

---

## 16. Implementation Status

### Interpreter
The current implementation is a **tree-walking interpreter** written in Rust. It directly executes the AST without compilation.

- **Lexer** (`src/lexer.rs`) — tokenizes source into sigils, keywords, operators, literals, identifiers
- **Parser** (`src/parser.rs`) — recursive descent, sigil-driven top-level parsing
- **AST** (`src/ast.rs`) — typed representation of all language constructs
- **Interpreter** (`src/interpreter.rs`) — immutable environments with scope chaining, built-in functions
- **CLI** (`src/main.rs`) — `ailang <file.ai>` to run, `ailang test <file.ai>` to run tests

### What's Implemented (v0.1)
- All block types: `#fn`, `#type`, `#enum`, `#const`, `#use`, `#entry`, `#test`, `#err`
- All prefix operators: arithmetic, comparison, logic, bitwise
- Control flow: `select` (lazy), `match` with patterns (literal, variant, wildcard)
- Iteration: `map`, `filter`, `fold`, `each` with lambdas
- Type system: all primitives parsed, compound types parsed, `cast` executed
- Error handling: `try`, `unwrap`, `ok` wrap
- Agent primitives: `tool` (stub), `log` (stderr output)
- 34 built-in functions (see sections 5.5–5.9)
- Lambdas/closures with environment capture
- Grouped sub-expressions
- Tests with `assert`

### What's Planned (v0.2+)
- Concurrency: `async`/`await`, `par`, channels (`chan`/`send`/`recv`)
- Pipelines: `|>` operator
- Iteration: `zip`, `flatmap`
- Module system: `#use` loading, visibility rules
- Agent primitives: `prompt`/`prompt_json`, `store_get`/`store_set`/`store_del`, `observe`
- JSON operations: `jget`, `jset`, `jstr`, `jparse`
- Additional builtins: `find`, `replace`, `set`, `pop`, `typeof`, `is`
- Error propagation: `?` operator
- Byte literals
- Compiler backend (bytecode or native)

---

## 17. Code Generation Guide: Patterns and Anti-Patterns

This section exists because AI models trained on human languages will instinctively generate AILang as if it were Python or JavaScript with different syntax. **AILang requires a fundamentally different approach.** Read this section before generating any code.

### 17.1 CRITICAL: Lambdas Are Single Expressions

A lambda body is **one expression**, not a block of statements. There are no multi-line lambdas. There are no bind statements inside lambdas.

**WRONG — multi-statement lambda (parse error):**
```
v0 :[i32] = map (fn x:i32 =>
  v1 :i32 = * x x
  v2 :i32 = + v1 1
  v2
) nums
```

**RIGHT — single expression lambda:**
```
v0 :[i32] = map (fn x:i32 => + (* x x) 1) nums
```

**RIGHT — extract complex logic into a named function:**
```
#fn square_plus_one :i32 x:i32
  v0 :i32 = * x x
  v1 :i32 = + v0 1
  = v1

-- then use as reference:
v0 :[i32] = map square_plus_one nums
```

**Rule:** If a lambda needs more than one operation, extract a `#fn` and pass it by name.

### 17.2 CRITICAL: No Nesting — Decompose Into Flat Functions

AILang has a maximum nesting depth of 1. Never put a `map` inside a `map`, a `select` inside a lambda inside a `fold`, or any similar nesting of complex logic.

**WRONG — nested iteration (the "Python translation" mistake):**
```
#fn find_pairs :[i32] nums:[i32] target:i32
  v0 :[i32] = call range 0 (call len nums)
  v1 :[[i32]] = map (fn i:i32 =>
    v2 :[i32] = call range (+ i 1) (call len nums)
    v3 :[[i32]] = map (fn j:i32 =>
      select (== (+ (call get nums i) (call get nums j)) target)
        [i j] [-1 -1]
    ) v2
    v3
  ) v0
  ...
```

**RIGHT — decompose into separate functions:**
```
#fn check_pair :bool nums:[i32] i:i32 j:i32 target:i32
  v0 :i32 = call get nums i
  v1 :i32 = call get nums j
  = == (+ v0 v1) target

#fn search_from :i32 nums:[i32] target:i32 i:i32 j:i32 size:i32
  v0 :bool = >= j size
  v1 :i32 = select v0 (call next_i nums target i size) (call try_pair nums target i j size)
  = v1

#fn try_pair :i32 nums:[i32] target:i32 i:i32 j:i32 size:i32
  v0 :bool = call check_pair nums i j target
  = select v0 i (call search_from nums target i (+ j 1) size)

#fn next_i :i32 nums:[i32] target:i32 i:i32 size:i32
  v0 :i32 = + i 1
  v1 :bool = >= v0 size
  = select v1 -1 (call search_from nums target v0 (+ v0 1) size)
```

**Rule:** When you catch yourself nesting, stop. Create a new `#fn`. Each function should be flat — a sequence of binds followed by a return. Recursion replaces loops; named functions replace nested lambdas.

### 17.3 CRITICAL: Every Statement Is One Line

A statement is a single line. There are no multi-line statements, no line continuations, no expression wrapping across lines. When the parser hits a newline, the current statement ends.

**WRONG — select split across lines (parse error):**
```
v0 :i32 = select v1
  (call foo a b)
  (call bar a b)
```

The parser reads `select v1`, then hits a newline and expects the statement to be complete — but `select` needs 3 arguments.

**RIGHT — everything on one line:**
```
v0 :i32 = select v1 (call foo a b) (call bar a b)
```

**If a line gets too long**, break the logic into smaller steps with intermediate binds:
```
v0 :i32 = call foo a b
v1 :i32 = call bar a b
v2 :bool = > x 0
v3 :i32 = select v2 v0 v1
```

This also applies to `fold`, `map`, and other expressions — all arguments must appear on the same line as the keyword.

**WRONG:**
```
v0 :i32 = fold nums 0
  (fn acc:i32 x:i32 => + acc x)
```

**RIGHT:**
```
v0 :i32 = fold nums 0 (fn acc:i32 x:i32 => + acc x)
```

**Rule:** One line, one statement. If it doesn't fit, decompose into binds or extract a `#fn`.

### 17.4 Statements vs Expressions (Not Interchangeable)

Only `#fn`, `#entry`, and `#test` blocks contain statements. Everywhere else (lambda bodies, `select` branches, `match` arms, function arguments) expects **expressions**.

**Statements** (only inside block bodies):
```
v0 :i32 = + a b          -- bind
= v0                      -- return
> v0                      -- emit
log "info" "msg"          -- effect
assert == v0 5            -- effect
```

**Expressions** (anywhere a value is needed):
```
+ a b                     -- binary op
call foo x y              -- function call
select cond a b           -- conditional
(fn x:i32 => * x x)      -- lambda
42                        -- literal
v0                        -- variable reference
```

**WRONG — bind statement used as select branch:**
```
v0 :i32 = select cond (v1 :i32 = + a b) (v2 :i32 = + a c)
```

**RIGHT — expressions in select branches:**
```
v0 :i32 = select cond (+ a b) (+ a c)
```

### 17.5 CRITICAL: Recursive Calls Must Be Inside Select, Not In Binds

Bind statements (`vN :TYPE = EXPR`) are **eagerly evaluated** — the expression executes unconditionally when the line is reached. `select` branches are **lazily evaluated** — only the taken branch executes.

This distinction is critical for recursion. If a recursive call is in a bind, it runs on every invocation and there is no base case to stop it.

**WRONG — recursive call in a bind (infinite recursion):**
```
#fn countdown :i32 n:i32
  v0 :bool = == n 0
  v1 :i32 = call countdown (- n 1)     -- ALWAYS executes, even when n=0
  = select v0 0 v1                      -- too late, v1 already recursed
```

The bind `v1 = call countdown (- n 1)` runs every time. When `n=0`, it calls `countdown -1`, then `countdown -2`, forever.

**RIGHT — recursive call inside the select branch:**
```
#fn countdown :i32 n:i32
  v0 :bool = == n 0
  = select v0 0 (call countdown (- n 1))   -- only recurses when n != 0
```

The recursive call is inside the `select`'s else-branch, which is lazily evaluated. When `n=0`, `v0` is true, `select` returns `0`, and the else-branch never executes.

**Larger example — search with base case:**

WRONG:
```
#fn _search :[i32] nums:[i32] target:i32 i:i32 j:i32 n:i32
  v0 :bool = >= i (- n 1)
  v1 :bool = call _check nums target i j
  v2 :[i32] = call _search nums target ...    -- ALWAYS runs, ignores base case
  v3 :[i32] = select v1 [i j] v2
  = select v0 [-1 -1] v3
```

RIGHT:
```
#fn _search :[i32] nums:[i32] target:i32 i:i32 j:i32 n:i32
  v0 :bool = >= i (- n 1)
  v1 :bool = select v0 false (call _check nums target i j)
  = select v0 [-1 -1] (select v1 [i j] (call _search nums target ...))
```

**Rule:** Never bind a recursive call to a variable. Always place recursive calls directly inside `select` branches where they will only execute when needed.

### 17.6 Use Lists, Not Tuples, for Indexed Data

The `get` builtin works on **lists**, not tuples. When you need indexed access to a compound accumulator, use a list.

**WRONG — tuple with `get`:**
```
v0 :(i32 i32) = (0 1)
v1 :i32 = call get v0 0       -- ERROR: get requires a list
```

**RIGHT — list with `get`:**
```
v0 :[i32] = [0 1]
v1 :i32 = call get v0 0       -- OK: returns 0
v2 :i32 = call get v0 1       -- OK: returns 1
```

This pattern is especially common in `fold` accumulators:
```
-- fold with multi-value accumulator using a list
v0 :[i32] = fold nums [0 0] (fn acc:[i32] x:i32 =>
  [
    (call max (call get acc 0) x)
    (+ (call get acc 1) x)
  ])
```

### 17.7 Grouped Expressions for Sub-Expression Arguments

When passing a compound expression (operator, call, select) as an argument, wrap it in `()`. Without grouping, the parser consumes atoms greedily and misinterprets boundaries.

**WRONG — bare operator as argument:**
```
v0 :i32 = call foo + a 1 b        -- parses as: call foo (+) a 1 b ??
```

**RIGHT — grouped sub-expression:**
```
v0 :i32 = call foo (+ a 1) b      -- clear: arg1 = (+ a 1), arg2 = b
```

**More examples:**
```
v0 :bool = == (call len lst) 0           -- compare result of call
v1 :i32 = call max (+ x 5) (- y 3)      -- two computed args
v2 :[i32] = call range 0 (call len nums) -- nested call as arg
```

**Rule:** If an argument to `call`, `select`, `map`, etc. is anything other than a literal or variable, wrap it in `()`.

### 17.8 Recursion Replaces Loops

AILang has no `for`, `while`, or loop constructs. Use either functional iteration (`map`/`filter`/`fold`) or recursion with `select` for termination.

**Pattern — iterating with index (use recursion):**
```
#fn find_index :i32 lst:[i32] target:i32 i:i32
  v0 :bool = >= i (call len lst)
  v1 :bool = select v0 false (== (call get lst i) target)
  = select v0 -1 (select v1 i (call find_index lst target (+ i 1)))
```

**Pattern — accumulating a result (use fold):**
```
#fn sum :i32 nums:[i32]
  = fold nums 0 (fn acc:i32 x:i32 => + acc x)
```

**Pattern — transforming each element (use map):**
```
#fn double_all :[i32] nums:[i32]
  = map (fn x:i32 => * x 2) nums
```

**Pattern — conditional recursion (separate functions for branches):**

When `select` branches contain recursive calls, extract each branch into its own `#fn`. This avoids deeply nested grouped expressions and keeps each function readable.

```
-- WRONG — deeply nested select with recursive calls inline:
v0 :i32 = select cond1 (select cond2 (call recurse_a x) (call recurse_b x)) (call recurse_c x)

-- RIGHT — one function per branch:
#fn handle_case :i32 x:i32 cond1:bool cond2:bool
  = select cond1 (call handle_true x cond2) (call recurse_c x)

#fn handle_true :i32 x:i32 cond2:bool
  = select cond2 (call recurse_a x) (call recurse_b x)
```

### 17.9 SSA Naming Convention

Variable names inside blocks are always `v0`, `v1`, `v2`, ... in strictly incrementing order. Never skip numbers. Never reuse numbers. Never use descriptive names for intermediates.

**WRONG:**
```
#fn calc :i32 x:i32
  result :i32 = * x x         -- descriptive name
  total :i32 = + result 1     -- descriptive name
  = total
```

**WRONG:**
```
#fn calc :i32 x:i32
  v0 :i32 = * x x
  v5 :i32 = + v0 1            -- skipped v1-v4
  = v5
```

**RIGHT:**
```
#fn calc :i32 x:i32
  v0 :i32 = * x x
  v1 :i32 = + v0 1
  = v1
```

Parameters use short names: `a`, `b`, `x`, `n`, `lst`, `acc`, `idx`, `nums`, `target`, `size`.

### 17.10 Function Order: Dependencies First, Entry Last

Functions must be defined before they are called. Helper functions come first, higher-level functions come next, tests come after functions, and `#entry` is always last.

```
-- 1. Helpers (leaf functions, no dependencies)
#fn helper_a :i32 x:i32
  = + x 1

-- 2. Functions that use helpers
#fn process :i32 x:i32
  v0 :i32 = call helper_a x
  = * v0 2

-- 3. Tests
#test process_works
  assert == (call process 5) 12

-- 4. Entry (always last)
#entry
  v0 :i32 = call process 10
  log "info" "result: {0}" v0
  = 0
```

### 17.11 Quick Reference: "If I Want X, I Write Y"

| I want...                          | I write...                                                |
|------------------------------------|-----------------------------------------------------------|
| A for loop                         | `map`, `filter`, `fold`, or recursion                     |
| An if/else                         | `select cond then_val else_val`                           |
| A nested loop                      | Multiple `#fn` with recursion                             |
| A multi-statement lambda           | Extract a `#fn`, pass by name                             |
| A variable with a descriptive name | `v0`, `v1`, `v2` (SSA)                                   |
| Indexed access to a pair           | `[a b]` list + `call get lst 0`                           |
| A computed argument                | `(+ a b)` grouped expression                              |
| Early return / break               | `select` to choose between base case and recursion        |
| Mutable accumulator                | `fold` with list accumulator `[val1 val2]`                |
| String building in a loop          | `fold` with text accumulator + `concat`                   |
| Nested conditionals                | Chain of `select` or separate `#fn` per branch            |
| A long expression                  | Break into intermediate `vN` binds, one per line          |
| Multi-line statement               | **Not possible.** One line = one statement. Always.       |
| A recursive call with a base case  | Put recursive `call` inside `select`, never in a bind     |
