# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Algow is an experimental Hindley-Milner type inference implementation (Algorithm W) in TypeScript. This is a learning project exploring how compilers and type systems work.

## Commands

```bash
bun install          # Install dependencies
bun run src/index.ts # Run the type checker
bun test             # Run tests (uses bun:test)
bunx oxlint          # Run linter
```

## Architecture

The type checker has three main components:

### `src/ast.ts` - Abstract Syntax Tree

Defines expression types and smart constructors:

- **Literals**: `Num`, `Bool`, `Str`
- **Bindings**: `Let` (polymorphic), `LetRec` (recursive)
- **Functions**: `Abs` (lambda), `App` (application)
- **Control**: `If`, `BinOp`

### `src/infer.ts` - Type Inference Engine

Implements Algorithm W with:

- **Types**: `TVar` (type variable), `TCon` (type constant), `TFun` (function type)
- **Schemes**: Polymorphic types with quantified variables and constraints
- **Type classes**: `Eq`, `Ord`, `Add` for operator overloading
- **Core functions**: `infer()` is the entry point; internally uses `unify()`, `generalize()`, `instantiate()`

### `src/index.ts` - Entry Point

Simple test harness for building and testing expressions.

## Runtime

Use Bun, not Node.js. Bun auto-loads `.env` files. See `node_modules/bun-types/docs/**.mdx` for Bun API documentation.
