# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Algow is a complete implementation of Hindley-Milner type inference (Algorithm W) in TypeScript. It includes a lexer, parser, type checker, and interpreter for an ML-like language.

## Commands

```bash
bun install          # Install dependencies
bun run src/index.ts # Run with a file: bun run src/index.ts examples/demo.alg
bun run src/index.ts -e "1 + 2"  # Run inline expression
bun run src/index.ts -t file.alg # Type check only (show inferred type)
bun run src/index.ts -c file.alg # Compile to JavaScript
bun run src/index.ts --emit-ir file.alg # Emit IR (for debugging)
bun test             # Run all tests
bun test src/infer.test.ts       # Run single test file
bunx oxlint          # Run linter

# LSP Server
bun run lsp          # Start LSP server (stdio transport)

# Playground
bun run playground:build  # Bundle for browser
bun run playground:serve  # Start dev server at http://localhost:3000
```

## Architecture

The compiler has two execution modes:

**Interpreter**: Source → Lexer → Parser → Type Checker → Interpreter → **Value**

**Compiler**: Source → Lexer → Parser → Type Checker → IR → JS Backend → **JavaScript**

### `src/lexer.ts` — Tokenization

Cursor-based lexer returning `[kind, start, end]` tuples. No string allocation during lexing—text extracted via `slice()` when needed.

### `src/parser.ts` — Pratt Parser

Top-down operator precedence parser producing AST and collecting diagnostics (no exceptions). Key exports:

- `parse(source)` → `ParseResult` with program and diagnostics
- `programToExpr(program)` → Converts top-level bindings to nested let expressions

### `src/ast.ts` — AST Definitions

All nodes use discriminated unions with `kind` field. Smart constructors (`num()`, `abs()`, `let_()`, etc.) for concise AST construction. Spans are optional to support programmatic AST (prelude).

### `src/infer.ts` — Type Inference (Algorithm W)

The core type checker. Key concepts:

- **Types**: `TVar`, `TCon`, `TFun`, `TApp`, `TRecord`, `TTuple`
- **Substitution**: Maps type variables to resolved types, composed via `composeSubst()`
- **Unification**: `unify(ctx, t1, t2)` finds substitution making types equal
- **Generalization**: `generalize(env, type)` creates polymorphic schemes for let-bindings
- **Row polymorphism**: `{ x: t | ρ }` for extensible records
- **Type classes**: `Eq`, `Ord`, `Add` for operator overloading

Key functions:

- `infer(env, registry, expr)` → Main entry point returning `InferOutput`
- `processDataDecl(decl)` → Converts ADT to constructor schemes + registry
- `checkExhaustiveness(registry, type, patterns)` → Pattern match coverage

### `src/eval.ts` — Tree-walking Interpreter

Evaluates type-checked AST to runtime values (`VNum`, `VStr`, `VBool`, `VClosure`, `VCon`, `VTuple`, `VRecord`). Trusts the type checker—minimal runtime checks.

### `src/ir.ts` — Intermediate Representation

A-Normal Form (ANF) IR where all intermediate values are named. Key types:

- **Atoms**: `IRLit`, `IRVar` (trivial values)
- **Expressions**: `IRAtomExpr`, `IRLet`, `IRLetRec`
- **Bindings**: `IRAppBinding`, `IRBinOpBinding`, `IRIfBinding`, `IRMatchBinding`, `IRLambdaBinding`, etc.
- **Patterns**: `IRPVar`, `IRPWildcard`, `IRPCon`, `IRPLit`, `IRPTuple`, `IRPRecord`

Every IR node carries its inferred type for type-directed code generation.

### `src/lower.ts` — AST to IR Lowering

Transforms typed AST to ANF IR. Key operations:

- `lowerToIR(expr, typeEnv, checkOutput)` → Main entry point
- `normalize(ctx, expr)` → Ensures expression is atomic (binds to fresh variable if needed)
- Preserves type information from the type checker

### `src/backend/` — Code Generation

JavaScript backend that generates optimized JS from IR:

- `runtime.ts` — Runtime helpers (`$apply`, `$con`, `$eq`)
- `js.ts` — IR to JavaScript code generation

Runtime value representations:

- Primitives: JS primitives directly
- Closures: Native JS closures
- Constructors: `{ $tag: "Name", $args: [...] }`
- Tuples: Arrays
- Records: Plain objects

### `src/prelude.ts` — Standard Library

Built-in data types (`Maybe`, `Either`, `List`) and functions (`map`, `filter`, `foldr`, `foldl`) defined as AST nodes.

### `src/symbols.ts` — LSP Support

Symbol table builder for go-to-definition, find references, and rename. Tracks definitions and references with source spans.

### `src/diagnostics.ts` — Error Reporting

Structured diagnostics with severity levels and source positions.

### `src/lsp/` — Language Server Protocol

Transport-agnostic LSP implementation with diagnostics, hover, and go-to-definition:

- `transport.ts` — Transport interface + JSON-RPC message types
- `server.ts` — Core LSP logic (document sync, diagnostics, hover, definition)
- `stdio.ts` — stdio transport for editors (VS Code, Neovim)
- `worker.ts` — Web worker transport for browser (Monaco playground)
- `positions.ts` — Byte offset ↔ LSP Position conversion

### `playground/` — Monaco Editor Playground

Web-based playground with Monaco editor and LSP integration:

- `index.html` — Editor page
- `main.ts` — Monaco setup + LSP client bridge
- `worker.ts` — Bundles LSP server for browser
- `server.ts` — Bun dev server

## Language Syntax

```
-- Line comment
{- Block comment -}

-- Data types
data Maybe a = Nothing | Just a
data List a = Nil | Cons a (List a)

-- Functions (curried, single parameter)
let add x y = x + y
let rec fact n = if n == 0 then 1 else n * fact (n - 1)

-- Pattern matching
match xs with
| Nil => 0
| Cons x rest => 1 + length rest
end

-- Operators: +, -, *, /, <, <=, >, >=, ==, !=
-- Pipe operator: x |> f  (desugars to f x)
-- Cons operator: x :: xs (desugars to Cons x xs)

-- Records and tuples
let point = { x = 1, y = 2 }
let pair = (1, "hello")
point.x  -- field access
```

## Runtime

Use Bun, not Node.js.
