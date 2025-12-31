# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Algow is an experimental Hindley-Milner type inference implementation (Algorithm W) in TypeScript. A learning project exploring how compilers and type systems work.

## Commands

```bash
bun install          # Install dependencies
bun run src/index.ts # Run the type checker
bun test             # Run tests (uses bun:test)
bunx oxlint          # Run linter
```

## Architecture

### `src/ast.ts` - Abstract Syntax Tree

**Expressions:**

- Literals: `Num`, `Bool`, `Str`
- Bindings: `Let` (polymorphic), `LetRec` (recursive)
- Functions: `Abs` (lambda), `App` (application)
- Control: `If`, `BinOp`, `Match`
- Data: `Tuple`

**Patterns** (for pattern matching):

- `PVar`, `PWildcard`, `PCon`, `PLit`

**Type expressions** (for ADT declarations):

- `TyVar`, `TyCon`, `TyApp`, `TyFun`

**Data declarations:**

- `DataDecl`, `ConDecl` — define ADTs like `data List a = Nil | Cons a (List a)`

### `src/infer.ts` - Type Inference Engine

**Internal types:**

- `TVar` (type variable), `TCon` (type constant), `TFun` (function type), `TApp` (type application)

**Key functions:**

- `infer(env, registry, expr)` — Main entry point
- `processDataDecl(decl)` — Converts ADT declarations to constructor schemes
- `checkExhaustiveness(registry, type, patterns)` — Validates pattern match coverage

**Type classes:** `Eq`, `Ord`, `Add` for operator overloading

### `src/index.ts` - Entry Point

Test harness demonstrating type inference with ADT declarations for `List`, `Maybe`, `Either`.

## Runtime

Use Bun, not Node.js.
