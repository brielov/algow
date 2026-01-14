# Algow

A small, experimental programming language with Hindley-Milner type inference that compiles to JavaScript. This is a learning project — I'm not an academic, just someone trying to understand how compilers and type systems work by building one from scratch.

## Features

### Type System

- **Hindley-Milner type inference** (Algorithm W) with let-polymorphism
- **Primitive types**: `int`, `float`, `string`, `char`, `bool`
- **Algebraic data types** with pattern matching
- **Parameterized types**: `List a`, `Maybe a`, `Either a b`
- **Tuples**: `(int, string, bool)` with positional access (`.0`, `.1`, etc.)
- **Records** with row polymorphism: `{ x: int, y: string | r }`
- **Type classes** for operator overloading: `Eq`, `Ord`, `Num`

### Language Features

- **Pattern matching** with exhaustiveness checking
- **Pattern guards**: `when x if x > 0 -> ...`
- **As-patterns**: `when Cons x rest as whole -> ...`
- **Or-patterns**: `when Nothing | Just Nothing -> ...`
- **Mutual recursion**: `let rec f = ... and g = ...`
- **Modules**: `module Foo ... end` with `use` declarations
- **List literals**: `[1, 2, 3]` desugars to `Cons` chains
- **Order-independent bindings**: top-level `let` can reference later definitions
- **Foreign function interface** for JavaScript interop

### Compiler

- Compiles to JavaScript with **multi-target support**: Node.js, Deno, Browser, Cloudflare Workers
- A-Normal Form intermediate representation
- Basic optimizations (constant folding, dead code elimination)
- Comprehensive standard library (prelude)
- Required `main : unit -> a` entry point (use `IO.getArgs` for command-line arguments)

## Quick Start

```bash
# Install dependencies
bun install

# Type check a file
bun run src/index.ts check integration/basics/main.alg

# Compile and run (with arguments)
bun run src/index.ts run integration/basics/main.alg -- arg1 arg2

# Compile to JavaScript
bun run src/index.ts compile integration/basics/main.alg > out.js

# Compile for different targets
bun run src/index.ts compile --target deno file.alg      # Deno
bun run src/index.ts compile --target browser file.alg   # Browser
bun run src/index.ts compile --target cloudflare file.alg # Cloudflare Workers
```

## Syntax Overview

```
-- Line comments start with --

-- Every program needs a main function
let main = args ->
  IO.printLine "Hello, World!"

-- Type declarations
type Maybe a = Nothing | Just a
type List a = Nil | Cons a (List a)

-- Functions (lambda syntax)
let double = x -> x * 2
let add = x y -> x + y

-- Recursive functions
let rec factorial = n ->
  if n <= 1 then 1 else n * factorial (n - 1)

-- Mutual recursion
let rec isEven = n -> if n == 0 then true else isOdd (n - 1)
and isOdd = n -> if n == 0 then false else isEven (n - 1)

-- Pattern matching
let length = xs ->
  match xs
    when Nil -> 0
    when Cons _ rest -> 1 + length rest
  end

-- Pattern guards
let abs = n ->
  match n
    when x if x < 0 -> 0 - x
    when x -> x
  end

-- Or-patterns
let isNone = opt ->
  match opt
    when Nothing | Just Nothing -> true
    when _ -> false
  end

-- Records
let point = { x = 10, y = 20 }
let moved = { point | x = point.x + 1 }

-- Tuples
let pair = (1, "hello")
let first = pair.0

-- List literals
let nums = [1, 2, 3, 4, 5]

-- Modules
module Math
  let square = x -> x * x
  let cube = x -> x * x * x
end

-- Import from modules
use Math (square, cube)

-- Operators
-- Arithmetic: +, -, *, Int.div, Int.mod
-- Comparison: <, <=, >, >=, ==, !=
-- Boolean: &&, ||
-- Pipe: |> (x |> f is f x)
-- Cons: :: (x :: xs is Cons x xs)
```

## Standard Library

The prelude provides common types and functions:

**Types**: `Maybe a`, `Either a b`, `List a`, `IOError`

**Maybe**: `isJust`, `isNothing`, `map`, `flatMap`, `withDefault`, `toList`

**Either**: `isLeft`, `isRight`, `map`, `mapLeft`, `flatMap`, `withDefault`

**List**: `map`, `filter`, `foldl`, `foldr`, `length`, `reverse`, `append`, `concat`, `head`, `tail`, `take`, `drop`, `zip`, `any`, `all`, `find`, `elem`, `nub`, `sortBy`

**Foreign modules**: `String`, `Char`, `Int`, `Float`, `IO`, `Debug`, `Map`, `Set`, `File`, `Dir`, `Path`

Note: `File` and `Dir` modules are only available on Node.js and Deno targets.

## Compilation Targets

Algow supports multiple JavaScript runtimes:

| Target           | Description        | File/Dir Access |
| ---------------- | ------------------ | --------------- |
| `node` (default) | Node.js / Bun      | Yes             |
| `deno`           | Deno runtime       | Yes             |
| `browser`        | Web browsers       | No              |
| `cloudflare`     | Cloudflare Workers | No              |

```bash
# Compile for Deno
bun run src/index.ts compile --target deno app.alg > app.js
deno run --allow-read app.js

# Compile for browser (outputs ES module)
bun run src/index.ts compile --target browser app.alg > app.js
```

## Integration Tests

See the `integration/` directory for comprehensive examples:

- `basics/` — Literals, operators, strings, booleans, tuples
- `functions/` — Higher-order functions, closures, currying
- `recursion/` — Basic, tail, and mutual recursion
- `pattern-matching/` — All pattern matching features
- `records/` — Record types and row polymorphism
- `data-types/` — Algebraic data types
- `algorithms/` — Sorting, searching, data structures
- `modules/` — Module system and imports

## Architecture

```
Source → Lexer → Parser → Desugar → Resolve → Type Check → Lower → Optimize → Codegen → JavaScript
```

- **Lexer**: Cursor-based tokenization
- **Parser**: Pratt parser (top-down operator precedence)
- **Desugar**: Surface syntax to core AST
- **Resolve**: Name resolution with SCC analysis for mutual recursion
- **Type Check**: Algorithm W with constraint solving
- **Lower**: ANF transformation
- **Optimize**: Constant folding, dead code elimination
- **Codegen**: JavaScript emission

## Resources

- [Write You a Haskell](http://dev.stephendiehl.com/fun/)
- [Algorithm W Step by Step](https://github.com/wh5a/Algorithm-W-Step-By-Step)
- Types and Programming Languages (Pierce)

## Disclaimer

This is not production code. It's messy, incomplete, and probably wrong in places. But that's the point — learning by doing.
