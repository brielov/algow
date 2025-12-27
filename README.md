# algow

A small, experimental type checker written in TypeScript. This is a learning project — I'm not an academic, just someone trying to understand how compilers and type systems work by building one from scratch.

## What's implemented

- Hindley-Milner type inference (Algorithm W)
- Basic types: `number`, `string`, `boolean`
- Functions (lambda abstractions and application)
- Let bindings with polymorphism
- Binary operators (`+`, `-`, `*`, `/`, `<`, `>`, `==`, etc.)
- Type classes for operator overloading (`Eq`, `Ord`, `Add`)

## What's next

- Algebraic data types (ADTs)
- Pattern matching
- Exhaustiveness checking

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
