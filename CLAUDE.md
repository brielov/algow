# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Algow is a statically-typed ML-like language that compiles to JavaScript. It implements Hindley-Milner type inference (Algorithm W) with extensions for algebraic data types, pattern matching, row-polymorphic records, and a type class system.

## Commands

```bash
bun install                                        # Install dependencies
bun run src/index.ts check file.alg                # Type check only
bun run src/index.ts compile file.alg              # Compile to JavaScript (node target)
bun run src/index.ts compile --target deno file.alg # Compile for Deno
bun run src/index.ts run file.alg -- arg1 arg2     # Compile and run with args
bun test                                           # Run example tests
bunx oxlint                                        # Run linter
```

## Architecture

**Compilation Pipeline**:

```
Source → Parser → Surface AST → Desugar → Core AST → Resolve → Typed Core → Lower → IR → Optimize → Codegen → JavaScript
```

### `src/surface.ts` — Surface AST

Parser output preserving syntactic sugar: `SExpr`, `SPattern`, `STypeExpr`, `SDecl`. Includes list literals, pipe operators, cons syntax, multi-param lambdas.

### `src/parser.ts` — Pratt Parser

Top-down operator precedence parser producing Surface AST. Collects diagnostics (no exceptions). Key entry: `parse(source)` → `ParseResult`.

### `src/desugar.ts` — Desugaring

Transforms Surface AST to Core AST. Removes all sugar:

- Multi-param lambdas → nested single-param lambdas
- Pipe `x |> f` → `f x`
- Cons `x :: xs` → `Cons x xs`
- List literals → `Cons` chains
- `if/then/else` → pattern match on bool

### `src/core.ts` — Core AST

Minimal canonical representation: `CExpr`, `CPattern`. Names have unique IDs (`Name = { id: number, original: string }`). No syntactic sugar remains.

### `src/resolve.ts` — Name Resolution

Assigns unique `Name.id` values to all bindings. Handles:

- Scope tracking and shadowing
- Constructor resolution from type declarations
- SCC analysis for mutual recursion (order-independent top-level bindings)
- Unbound variable detection

### `src/types.ts` — Type Representations

Internal types for inference: `TVar`, `TCon`, `TApp`, `TFun`, `TTuple`, `TRecord`. Also: `Scheme` (polymorphic types), `Subst` (substitutions), `TypeEnv`, `ConstructorRegistry`.

### `src/checker.ts` — Type Inference (Algorithm W)

Core type checker operating on resolved Core AST. Key concepts:

- **Unification**: `unify(ctx, t1, t2)` finds substitution making types equal
- **Generalization**: Creates polymorphic schemes at let-bindings
- **Row polymorphism**: `{ x: t | ρ }` for extensible records
- **Type classes**: `Eq`, `Ord`, `Num`, `Add` for operator overloading
- **Exhaustiveness checking**: Pattern match coverage analysis

Entry: `checkProgram(program)` → `CheckOutput` with type, substitution, type maps.

### `src/ir.ts` — A-Normal Form IR

All intermediate values bound to names. Types: `IRAtom`, `IRExpr`, `IRBinding`, `IRPattern`. Every node carries inferred type for code generation.

### `src/lower.ts` — Core to IR

Transforms typed Core AST to ANF IR. `normalize()` ensures expressions are atomic by binding complex subexpressions to fresh variables.

### `src/optimize.ts` — IR Optimization

Constant folding, dead code elimination, simple inlining.

### `src/codegen.ts` — JavaScript Backend

Generates JS from IR with multi-target support. Runtime representations:

- Lists: `Nil` → `null`, `Cons h t` → `{ h, t }`
- Other ADTs: `[tag, arg1, arg2, ...]` with numeric tags
- Tuples: Arrays
- Records: Plain objects
- Functions: Native closures

Targets: `node` (default), `deno`, `browser`, `cloudflare`

### `src/runtime/` — Target-Specific Runtimes

Modular runtime system:

- `base.js` — Core utilities (`$eq`, `$ioError`, `$unavailable`, `$foreign`)
- `modules/` — Cross-platform modules (string, char, int, float, debug, map, set, path)
- `targets/` — Target-specific implementations (node, deno, browser, cloudflare)
- `index.ts` — Runtime builder that assembles modules by target

### `src/compile.ts` — Pipeline Orchestration

`compile(sources, options)` chains all phases, collecting diagnostics. Validates `main : List String -> a` entry point. Supports multi-file compilation.

## Language Features

```
let main = args -> IO.printLine "Hello"   -- Required entry point (List String -> a)
type Maybe a = Nothing | Just a           -- ADTs
let double = x -> x * 2                   -- Lambdas (always arrow syntax)
let rec fact = n -> if n <= 1 then 1 else n * fact (n - 1)
let rec even = n -> ... and odd = n -> ... -- Mutual recursion
match xs when Nil -> 0 when Cons x _ -> x end
[1, 2, 3]                                 -- List literal → Cons chains
x |> f |> g                               -- Pipe → g (f x)
{ x = 1, y = 2 }                          -- Records
(1, "hello").0                            -- Tuples with index access
```

## Runtime

Use Bun, not Node.js. Programs require a `main` function that receives command-line arguments as `List String`.
