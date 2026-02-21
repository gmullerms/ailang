# AILang — Complete Reference for LLMs

> **Purpose:** This document is designed to be pasted into a system prompt, custom instructions, or context window so that an LLM can generate correct AILang code. It is self-contained — no other document is needed.

---

## How to Use This with Other LLMs

### ChatGPT (GPT-4, GPT-4o)
1. Go to **Settings → Personalization → Custom Instructions**
2. In "What would you like ChatGPT to know about you?", paste:
   ```
   I use a programming language called AILang. When I ask you to write code,
   write it in AILang following the reference guide I will provide.
   ```
3. Start a new chat, paste this entire document as the first message, then say:
   "Use this as the AILang reference. All code I ask for should be in AILang."
4. **Alternative:** Use a GPT with this document as its knowledge base.

### Claude (claude.ai)
1. Create a **Project** at claude.ai
2. Add this file as project knowledge (upload `LLM_GUIDE.md`)
3. In the project system prompt, add: "Write all code in AILang following the reference in project knowledge."

### Gemini
1. Paste this document at the start of the conversation
2. Say: "Use this as the AILang reference for all code generation."

### Any LLM (API, local, etc.)
Include this document in the system prompt or as the first user message. The document is ~600 lines — well within context limits for modern models.

---

## 1. What is AILang?

AILang is a programming language designed for AI agents. It is implemented as a tree-walking interpreter in Rust.

**Key properties:**
- SSA variables: `v0`, `v1`, `v2` (never descriptive names inside functions)
- Prefix notation: `+ a b`, `== x 0`, `not flag`
- One statement per line — no exceptions
- No loops — recursion and functional iteration only
- Lazy `select`/`cond` for conditionals
- Tail-call optimization for deep recursion
- `--` for comments (NOT `#`)

---

## 2. Critical Rules (Read First)

These are the rules that LLMs violate most often. Memorize them.

### 2.1 Keywords vs `call` — THE #1 MISTAKE

Some operations are **keywords** (used directly). Others are **builtins** (called with `call`). Mixing them up causes parse errors.

**Keywords — used DIRECTLY, NO `call`:**
```
map FN LIST                    -- apply function to each element
filter FN LIST                 -- keep elements where FN returns true
fold LIST INIT FN              -- reduce with accumulator
each LIST FN                   -- side-effect iteration
flatmap FN LIST                -- map then flatten one level
zip LISTA LISTB                -- combine into pairs
select COND THEN ELSE          -- conditional (lazy)
cond C1 V1 C2 V2 ... DEFAULT  -- multi-way conditional (lazy)
cast TYPE VALUE                -- type conversion
log "level" "template" args    -- log to stderr
assert EXPR                    -- test assertion
error "message"                -- create error value
try EXPR                       -- wrap in result type
unwrap EXPR DEFAULT            -- unwrap result with fallback
```

**Builtins — called with `call`:**
```
call len x              call get lst 0          call push lst val
call head lst           call tail lst           call range 0 10
call abs -5             call max a b            call min a b
call concat "a" "b"     call chr 65             call sleep 100
call random 1 10        call read_key           call print "hi"
... (all 57 builtins listed in Section 6)
```

**WRONG:** `call map (fn x:i32 => * x 2) nums`  — parse error!
**WRONG:** `call fold nums 0 (fn a:i32 b:i32 => + a b)` — parse error!
**WRONG:** `call filter (fn x:i32 => > x 0) nums` — parse error!
**RIGHT:** `map (fn x:i32 => * x 2) nums`
**RIGHT:** `fold nums 0 (fn a:i32 b:i32 => + a b)`
**RIGHT:** `filter (fn x:i32 => > x 0) nums`

### 2.2 Comments use `--`, NOT `#`

`#` is a sigil character for block headers (`#fn`, `#entry`, `#test`, etc.).
`--` starts a line comment.

**WRONG:** `# this is a comment`
**RIGHT:** `-- this is a comment`

### 2.3 One statement per line — ALWAYS

No multi-line statements. No line continuations. When the parser hits a newline, the statement ends.

**WRONG:**
```
v0 :i32 = select v1
  (call foo a) (call bar b)
```
**RIGHT:**
```
v0 :i32 = select v1 (call foo a) (call bar b)
```

If a line is too long, use intermediate binds:
```
v0 :i32 = call foo a
v1 :i32 = call bar b
v2 :i32 = select cond v0 v1
```

### 2.4 Lambdas are single expressions

No multi-statement lambdas. No binds inside lambdas.

**WRONG:**
```
map (fn x:i32 =>
  v0 :i32 = * x x
  + v0 1
) nums
```
**RIGHT:** `map (fn x:i32 => + (* x x) 1) nums`

If complex, extract to a named function:
```
#fn square_plus :i32 x:i32
  v0 :i32 = * x x
  = + v0 1

-- then: map square_plus nums
```

### 2.5 Recursive calls MUST be inside `select`/`cond`, NEVER in binds

Binds are eagerly evaluated. `select`/`cond` branches are lazy.

**WRONG — infinite recursion:**
```
#fn countdown :i32 n:i32
  v0 :i32 = call countdown (- n 1)   -- ALWAYS runs, even when n=0!
  = select (== n 0) 0 v0
```

**RIGHT:**
```
#fn countdown :i32 n:i32
  = select (== n 0) 0 (call countdown (- n 1))
```

### 2.6 No string interpolation

AILang has NO string interpolation. Use `call fmt` with positional placeholders `{0}`, `{1}`, etc., or `call concat` / `+` for text concatenation.

**WRONG:** `v0 :text = "Hello {name}"`
**RIGHT:** `v0 :text = call fmt "Hello {0}" name`
**RIGHT:** `v0 :text = + "Hello " name`

### 2.7 `+` on lists does NOT work

The `+` operator concatenates text, but does NOT concatenate lists. Use `call append` for lists.

**WRONG:** `v0 :[i32] = + [1 2] [3 4]`  — runtime error!
**RIGHT:** `v0 :[i32] = call append [1 2] [3 4]`
**RIGHT:** `v0 :text = + "hello" " world"`  — text concat works

### 2.8 Entry code must be in `#entry` block

Top-level code outside any block is a parse error. Everything runs inside `#fn`, `#test`, or `#entry`.

**WRONG:**
```
v0 :i32 = call add 1 2
call print v0
```

**RIGHT:**
```
#entry
  v0 :i32 = call add 1 2
  call print v0
  = 0
```

### 2.9 `#entry` must return a value

The last statement in `#entry` must be `= EXPR` (typically `= 0`).

### 2.10 SSA variables: `v0`, `v1`, `v2`... in order, never reused

Each variable assigned exactly once. Sequential numbering. Never skip. Never reuse.

**WRONG:** `v0 :i32 = 1` then later `v0 :i32 = 2`
**WRONG:** `v0`, then `v5` (skipped v1-v4)
**RIGHT:** `v0`, `v1`, `v2`, `v3`... in strict order

### 2.11 `flatmap` is ONE WORD (not `flat_map`)

**WRONG:** `flat_map (fn x:i32 => [x (* x x)]) nums`
**RIGHT:** `flatmap (fn x:i32 => [x (* x x)]) nums`

---

## 3. Program Structure

```
-- Comments
#use "module_name"              -- imports (optional)
#use "module" {func1 func2}     -- selective imports

#const NAME :TYPE = LITERAL     -- constants

#fn name :return_type param1:type1 param2:type2
  v0 :type = EXPR               -- bind (SSA)
  v1 :type = EXPR
  call side_effect args          -- effect statement (no bind)
  = EXPR                         -- return (last statement)

#test test_name
  v0 :type = EXPR
  assert EXPR                    -- assertion

#err function_name               -- error handler for a function
  retry 3 1000                   -- retry 3 times, 1s delay
  fallback "default"             -- fallback value

#entry                           -- program entry (always last)
  v0 :type = EXPR
  = 0                            -- exit code
```

**Block order:** `#use` → `#const` → `#fn` → `#test` → `#err` → `#entry`
Functions must be defined before they are called (no forward references).

---

## 4. Type System

### Primitive types
| Type   | Description              | Example        |
|--------|--------------------------|----------------|
| `i32`  | 64-bit signed integer    | `42`, `-7`     |
| `f64`  | 64-bit float             | `3.14`         |
| `bool` | Boolean                  | `true`, `false` |
| `text` | UTF-8 string             | `"hello"`      |
| `any`  | Dynamic type             | (avoid — warns) |
| `void` | No value                 |                |

Note: `i32` is actually 64-bit internally. Use `i32` as the standard integer type.

### Compound types
| Syntax         | Description           | Example                |
|----------------|-----------------------|------------------------|
| `[T]`          | List of T             | `[1 2 3]`             |
| `{K:V}`        | Map                   | `{"a" 1 "b" 2}`       |
| `(T1 T2)`      | Tuple                 | `(1 "hello")`         |
| `?T`           | Optional (T or null)  |                        |
| `!T`           | Result (T or error)   |                        |

### Literals
- Integers: `42`, `-7`, `0xFF`
- Floats: `3.14`, `-0.5`
- Booleans: `true`, `false`
- Text: `"hello"`, `"line\nbreak"`, `"\t"`
- Lists: `[1 2 3]` (space-separated, no commas)
- Maps: `{"key" 42 "other" 99}` (alternating key value, no commas)
- Null: `null`

---

## 5. Expressions Reference

### 5.1 Arithmetic (prefix)
```
+ a b       -- add (also concatenates text)
- a b       -- subtract
* a b       -- multiply
/ a b       -- divide
% a b       -- modulo
neg a       -- negate
```

### 5.2 Comparison (prefix)
```
== a b      != a b      < a b
> a b       <= a b      >= a b
```

### 5.3 Logic (prefix)
```
and a b     or a b      not a
```

### 5.4 Select (lazy conditional)
```
select CONDITION THEN_VALUE ELSE_VALUE
```
Both branches are **lazy** — only the chosen one evaluates. Critical for recursion.

```
v0 :i32 = select (> x 0) x (neg x)        -- absolute value
= select (== n 0) 1 (* n (call fact (- n 1)))  -- factorial
```

### 5.5 Cond (multi-way conditional, lazy)
```
cond COND1 VAL1 COND2 VAL2 ... DEFAULT
```
Odd number of args. Last is default. All lazy.

```
v0 :text = cond (== x 0) "zero" (> x 0) "positive" "negative"
v1 :i32 = cond (== n 1) 100 (== n 2) 300 (== n 3) 500 800
```

### 5.6 Function calls
```
call FUNCTION_NAME arg1 arg2 ...
```
Used for both user-defined functions and builtins.

Sub-expressions as arguments need `()` grouping:
```
v0 :i32 = call foo (+ a 1) (call bar x)
v1 :bool = == (call len lst) 0
```

### 5.7 Iteration keywords (NO `call`)
```
map FN LIST                -- [a b c] → [FN(a) FN(b) FN(c)]
filter FN LIST             -- keep where FN returns true
fold LIST INIT FN          -- reduce: FN(FN(INIT, a), b)...
each LIST FN               -- side effects, returns void
flatmap FN LIST            -- map then flatten one level
zip LISTA LISTB            -- [[a1 b1] [a2 b2] ...]
```

FN can be a lambda or a function name:
```
v0 :[i32] = map (fn x:i32 => * x x) nums       -- lambda
v1 :i32 = fold nums 0 (fn acc:i32 x:i32 => + acc x)
v2 :[i32] = map square nums                      -- named function
v3 :[i32] = filter (fn x:i32 => > x 0) nums
v4 :[i32] = flatmap (fn x:i32 => [x (neg x)]) nums
```

### 5.8 Pipeline operator
```
value |> func1 |> func2 arg
-- desugars to: call func2 (call func1 value) arg
```

### 5.9 Error handling
```
error "message"               -- create error value
v0 :text = (call risky_fn x)?  -- propagate error, unwrap ok
v0 :!i32 = try call parse s   -- wrap in result
v0 :i32 = unwrap v0 0         -- unwrap with default
```

### 5.10 Cast
```
v0 :f64 = cast f64 42         -- int to float
v1 :i32 = cast i32 3.14       -- float to int (truncates)
v2 :text = call to_text 42    -- any to text
```

---

## 6. Complete Builtin Reference

Every builtin is called with `call`. Signature format: `name arg1:type arg2:type → return_type`

### Math
| Builtin | Signature | Example |
|---------|-----------|---------|
| `abs` | `x:i32/f64 → i32/f64` | `call abs -5` → `5` |
| `min` | `a:i32 b:i32 → i32` | `call min 3 7` → `3` |
| `max` | `a:i32 b:i32 → i32` | `call max 3 7` → `7` |
| `sqrt` | `x:f64 → f64` | `call sqrt 9.0` → `3.0` |
| `random` | `min:i32 max:i32 → i32` | `call random 1 10` → random in [1,10] |

### Text
| Builtin | Signature | Example |
|---------|-----------|---------|
| `concat` | `a:text b:text → text` | `call concat "hi" " there"` → `"hi there"` |
| `len` | `x:text/[T] → i32` | `call len "hello"` → `5` |
| `slice` | `x:text/[T] start:i32 end:i32 → text/[T]` | `call slice "hello" 1 3` → `"el"` |
| `upper` | `x:text → text` | `call upper "hi"` → `"HI"` |
| `lower` | `x:text → text` | `call lower "HI"` → `"hi"` |
| `trim` | `x:text → text` | `call trim "  hi  "` → `"hi"` |
| `split` | `x:text delim:text → [text]` | `call split "a,b" ","` → `["a" "b"]` |
| `join` | `lst:[text] delim:text → text` | `call join ["a" "b"] ","` → `"a,b"` |
| `chars` | `x:text → [text]` | `call chars "hi"` → `["h" "i"]` |
| `char_at` | `x:text idx:i32 → text` | `call char_at "hello" 0` → `"h"` |
| `to_text` | `x:any → text` | `call to_text 42` → `"42"` |
| `fmt` | `template:text args... → text` | `call fmt "x={0} y={1}" 3 4` → `"x=3 y=4"` |
| `find` | `haystack:text needle:text → i32` | `call find "hello" "ll"` → `2` (-1 if not found) |
| `replace` | `x:text old:text new:text → text` | `call replace "hello" "l" "r"` → `"herlo"` |
| `chr` | `code:i32 → text` | `call chr 65` → `"A"` |

### List
| Builtin | Signature | Example |
|---------|-----------|---------|
| `len` | `lst:[T] → i32` | `call len [1 2 3]` → `3` |
| `get` | `lst:[T] idx:i32 → T/null` | `call get [10 20] 0` → `10` (null if OOB) |
| `safe_get` | `lst:[T] idx:i32 → T/null` | same as get, also null for negative |
| `set` | `lst:[T] idx:i32 val:T → [T]` | `call set [1 2 3] 1 99` → `[1 99 3]` |
| `push` | `lst:[T] val:T → [T]` | `call push [1 2] 3` → `[1 2 3]` |
| `pop` | `lst:[T] → (list, last)` | `call pop [1 2 3]` → `([1 2], 3)` |
| `head` | `lst:[T] → T/null` | `call head [1 2 3]` → `1` |
| `tail` | `lst:[T] → [T]` | `call tail [1 2 3]` → `[2 3]` |
| `range` | `start:i32 end:i32 → [i32]` | `call range 0 5` → `[0 1 2 3 4]` |
| `reverse` | `lst:[T] → [T]` | `call reverse [1 2 3]` → `[3 2 1]` |
| `sort` | `lst:[i32] → [i32]` | `call sort [3 1 2]` → `[1 2 3]` |
| `append` | `a:[T] b:[T] → [T]` | `call append [1 2] [3 4]` → `[1 2 3 4]` |
| `is_empty` | `x:[T]/text/null → bool` | `call is_empty []` → `true` |
| `slice` | `lst:[T] start:i32 end:i32 → [T]` | `call slice [1 2 3 4] 1 3` → `[2 3]` |

### Map
| Builtin | Signature | Example |
|---------|-----------|---------|
| `mget` | `m:{K:V} key:K → V/null` | `call mget {"a" 1} "a"` → `1` |
| `mset` | `m:{K:V} key:K val:V → {K:V}` | `call mset {} "a" 1` → `{"a" 1}` |
| `mdel` | `m:{K:V} key:K → {K:V}` | `call mdel {"a" 1} "a"` → `{}` |
| `mkeys` | `m:{K:V} → [K]` | `call mkeys {"a" 1 "b" 2}` → `["a" "b"]` |
| `mvals` | `m:{K:V} → [V]` | `call mvals {"a" 1 "b" 2}` → `[1 2]` |
| `mhas` | `m:{K:V} key:K → bool` | `call mhas {"a" 1} "a"` → `true` |

### JSON
| Builtin | Signature | Example |
|---------|-----------|---------|
| `jparse` | `text → any` | `call jparse "{\"a\":1}"` → map |
| `jstr` | `val:any → text` | `call jstr {"a" 1}` → `"{\"a\":1}"` |
| `jget` | `val:any path:text → any` | `call jget obj "name"` → value |
| `jset` | `val:any path:text newval:any → any` | `call jset obj "name" "Alice"` |

### I/O
| Builtin | Signature | Example |
|---------|-----------|---------|
| `print` | `args... → void` | `call print "hello"` (adds newline) |
| `print_no_nl` | `args... → void` | `call print_no_nl "prompt: "` (no newline) |
| `read_line` | `→ text` | `v0 :text = call read_line` |
| `read_key` | `→ text` | `v0 :text = call read_key` (non-blocking, "" if none) |
| `read_file` | `path:text → text` | `call read_file "data.txt"` |
| `write_file` | `path:text content:text → null` | `call write_file "out.txt" data` |
| `env_get` | `name:text → text/null` | `call env_get "HOME"` |
| `http_get` | `url:text → text` | `call http_get "https://..."` |
| `http_post` | `url:text body:text → text` | `call http_post url payload` |
| `sleep` | `ms:i32 → null` | `call sleep 1000` (pause 1 second) |
| `log` | N/A — `log` is a keyword | `log "info" "msg {0}" val` |

### Type
| Builtin | Signature | Example |
|---------|-----------|---------|
| `typeof` | `val:any → text` | `call typeof 42` → `"i32"` |
| `is` | `type:text val:any → bool` | `call is "i32" 42` → `true` |

### Error
| Builtin | Signature | Example |
|---------|-----------|---------|
| `error` | `msg:text → err` | `error "not found"` (also: `call error "msg"`) |

---

## 7. Patterns & Idioms

### Create a list of N zeros (no `repeat` builtin exists)
```
v0 :[i32] = map (fn i:i32 => 0) (call range 0 200)
```

### Recursive search with base case
```
#fn find :i32 lst:[i32] target:i32 i:i32
  v0 :bool = >= i (call len lst)
  v1 :bool = select v0 false (== (call get lst i) target)
  = select v0 -1 (select v1 i (call find lst target (+ i 1)))
```

### Accumulator pattern (tail-recursive)
```
#fn sum :i32 lst:[i32] acc:i32 i:i32
  v0 :bool = >= i (call len lst)
  v1 :i32 = + acc (call get lst i)
  = select v0 acc (call sum lst v1 (+ i 1))
```

### Multi-value fold accumulator
```
v0 :[i32] = fold nums [0 0] (fn acc:[i32] x:i32 =>
  [(call max (call get acc 0) x) (+ (call get acc 1) x)])
-- acc[0] = running max, acc[1] = running sum
```

### Side-effect calls (no binding needed)
```
call print "hello world"           -- standalone effect
call sleep 100                     -- standalone effect
log "info" "value is {0}" v0       -- log is a keyword, no call
```

### ANSI terminal output
```
v0 :text = call chr 27                    -- ESC character
call print_no_nl (+ v0 "[2J")            -- clear screen
call print_no_nl (+ v0 "[H")             -- cursor home
call print_no_nl (+ v0 "[48;5;196m  ")   -- red background cell
call print_no_nl (+ v0 "[0m")            -- reset colors
```

### Non-blocking keyboard input
```
v0 :text = call read_key
-- Returns: "left", "right", "up", "down", "enter", "esc",
--          "space", "backspace", "tab", single char "a"/"q"/etc.
--          "" if no key pressed
```

---

## 8. Complete Examples

### Example 1: Hello World
```
#fn greet :text name:text
  = + "Hello " name

#entry
  v0 :text = call greet "AILang"
  call print v0
  = 0
```

### Example 2: FizzBuzz (map + select)
```
#fn fizzbuzz_one :text n:i32
  v0 :bool = == (% n 15) 0
  v1 :bool = == (% n 3) 0
  v2 :bool = == (% n 5) 0
  v3 :text = call to_text n
  v4 :text = select v2 "Buzz" v3
  v5 :text = select v1 "Fizz" v4
  = select v0 "FizzBuzz" v5

#fn fizzbuzz :[text] n:i32
  v0 :[i32] = call range 1 (+ n 1)
  = map fizzbuzz_one v0

#test fizzbuzz_15
  v0 :[text] = call fizzbuzz 15
  assert == (call get v0 0) "1"
  assert == (call get v0 2) "Fizz"
  assert == (call get v0 4) "Buzz"
  assert == (call get v0 14) "FizzBuzz"

#entry
  v0 :[text] = call fizzbuzz 20
  log "info" "FizzBuzz: {0}" v0
  = 0
```

### Example 3: Two Sum (recursive search)
```
-- LeetCode #1: find indices of two numbers that sum to target

#fn check_pair :bool nums:[i32] i:i32 j:i32 target:i32
  v0 :i32 = call get nums i
  v1 :i32 = call get nums j
  = == (+ v0 v1) target

#fn search :i32 nums:[i32] target:i32 i:i32 j:i32 n:i32
  v0 :bool = >= i (- n 1)
  v1 :bool = >= j n
  v2 :bool = select v0 false (select v1 false (call check_pair nums i j target))
  = cond v0 -1 v1 (call search nums target (+ i 1) (+ i 2) n) v2 i (call search nums target i (+ j 1) n)

#fn two_sum :i32 nums:[i32] target:i32
  = call search nums target 0 1 (call len nums)

#test two_sum_basic
  assert == (call two_sum [2 7 11 15] 9) 0

#entry
  v0 :i32 = call two_sum [2 7 11 15] 9
  log "info" "index: {0}" v0
  = 0
```

### Example 4: Fibonacci (fold with list accumulator)
```
#fn fibonacci :i32 n:i32
  v0 :[i32] = call range 0 (- n 1)
  v1 :[i32] = fold v0 [0 1] (fn acc:[i32] i:i32 =>
    [(call get acc 1) (+ (call get acc 0) (call get acc 1))])
  = call get v1 1

#test fib_10
  assert == (call fibonacci 10) 55

#entry
  v0 :i32 = call fibonacci 10
  log "info" "fib(10) = {0}" v0
  = 0
```

---

## 9. Anti-Pattern Checklist

Before submitting generated AILang code, verify:

- [ ] No `call` before `map`, `filter`, `fold`, `each`, `flatmap`, `zip`
- [ ] No `call` before `select`, `cond`, `cast`, `log`, `assert`, `error`
- [ ] Comments use `--` not `#` or `//`
- [ ] All statements are single lines (no line breaks in expressions)
- [ ] Lambdas are single expressions (no binds inside)
- [ ] Recursive calls are inside `select`/`cond` branches, not in binds
- [ ] Variables are `v0`, `v1`, `v2`... sequential, no gaps, no reuse
- [ ] Entry code is inside `#entry` block
- [ ] `#entry` ends with `= EXPR` (return value)
- [ ] Functions defined before first use (no forward references)
- [ ] Lists concatenated with `call append`, not `+`
- [ ] Text concatenated with `+` or `call concat`
- [ ] Sub-expressions in arguments wrapped in `()`
- [ ] No string interpolation — use `call fmt "{0}" val`
- [ ] `flatmap` not `flat_map`
- [ ] List elements space-separated: `[1 2 3]` not `[1, 2, 3]`
- [ ] No `repeat` builtin — use `map (fn i:i32 => 0) (call range 0 N)`
- [ ] `get` returns `null` on out-of-bounds (not error)
- [ ] `print` adds newline; `print_no_nl` does not

---

## 10. Running AILang Programs

```bash
# Run a program
ailang examples/hello.ai

# Run tests only (skips #entry)
ailang test examples/hello.ai

# Sandbox mode (restricts file/network/env I/O)
ailang --sandbox examples/hello.ai

# Format source file (canonical form)
ailang fmt examples/hello.ai

# REPL
ailang
```

---

## 11. Quick Syntax Card

```
#fn NAME :RET_TYPE PARAM:TYPE ...     -- function definition
  v0 :TYPE = EXPR                     -- bind (SSA)
  call BUILTIN args...                -- builtin call
  call USER_FN args...                -- user function call
  map FN LIST                         -- keyword iteration (NO call)
  fold LIST INIT FN                   -- keyword fold (NO call)
  filter FN LIST                      -- keyword filter (NO call)
  select COND THEN ELSE               -- lazy conditional
  cond C1 V1 C2 V2 DEFAULT            -- multi-way conditional
  log "level" "template {0}" arg      -- log to stderr
  = EXPR                              -- return

#test NAME                            -- test block
  assert EXPR                         -- assertion

#entry                                -- entry point (last block)
  = 0                                 -- exit code
```
