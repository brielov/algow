# algow

A small, experimental type checker written in TypeScript. This is a learning project — I'm not an academic, just someone trying to understand how compilers and type systems work by building one from scratch.

## What's implemented

- Hindley-Milner type inference (Algorithm W)
- Basic types: `number`, `string`, `boolean`
- Functions (lambda abstractions and application)
- Let bindings with polymorphism
- Binary operators (`+`, `-`, `*`, `/`, `<`, `>`, `==`, etc.)
- Type classes for operator overloading (`Eq`, `Ord`, `Add`)
- Parameterized types (`List a`, `Maybe a`, `Either a b`)
- Algebraic data types (ADTs) with `dataDecl`
- Pattern matching with `match` expressions
- Exhaustiveness checking for pattern matches
- Tuples with proper arity: `(number, string, boolean)`
- Record types with field access: `{ x: number, y: string }`
- Row polymorphism: `fn r => r.x` types as `{ x: t | ρ } -> t`
- Pattern matching on records and tuples

## What's next

- Lexer and parser
- Runtime/interpreter for code execution

## Running

```bash
bun install
bun run src/index.ts
```

## Resources that helped

- [Write You a Haskell](http://dev.stephendiehl.com/fun/)
- [Algorithm W Step by Step](https://github.com/wh5a/Algorithm-W-Step-By-Step)
- Types and Programming Languages (Pierce)

## Disclaimer

This is not production code. It's messy, incomplete, and probably wrong in places. But that's the point — learning by doing.
