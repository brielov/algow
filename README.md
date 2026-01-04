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

### Language Features

#### High Priority

- [x] **Logical operators (`&&`, `||`)** — Short-circuit boolean operators
- [x] **String concatenation (`++`)** — Concatenate strings with the `++` operator
- [x] **Unary negation (`-x`)** — Negate numbers with prefix `-`
- [x] **Let destructuring** — `let (x, y) = (10, 20) in x * y`
- [x] **Tuple indexing** — `tuple.0`, `tuple.1` for positional access
- [x] **List literals** — `[1, 2, 3]` desugaring to `Cons` chains

#### Medium Priority

- [ ] **Mutual recursion** — `let rec f = ... and g = ...` for co-recursive definitions
- [ ] **Pattern guards** — `| n if n > 0 => ...` in match expressions
- [ ] **As-patterns** — `| (Cons x rest) as whole => ...`
- [ ] **Or-patterns** — `| Nothing | Just Nothing => ...`
- [ ] **Optional type annotations** — `let add (x: number) y: number = x + y`
- [ ] **Wildcard in expressions** — `let _ = expr in ...` to ignore values
- [ ] **Record field punning** — `{ x, y }` as shorthand for `{ x = x, y = y }`

#### Lower Priority

- [ ] **Type aliases** — `type Point = { x: number, y: number }`
- [ ] **Char type and literals** — `'a'`, `'b'`, with `string` as `List Char`
- [ ] **Numeric tower** — Distinguish `Int` vs `Float` types

### Compiler Improvements

#### Error Messages

- [ ] **Contextual errors** — Show the expression that caused the type error
- [ ] **Expected vs actual** — "Expected `number`, got `string` in argument to `add`"
- [ ] **Occurs check clarity** — "Infinite type: cannot unify `t0` with `t0 -> t1`"
- [ ] **Suggestions** — "Did you mean `foo` instead of `fop`?"

#### Optimizations

- [ ] **Tail-call optimization** — Essential for idiomatic recursive code
- [ ] **Constant folding** — Evaluate `1 + 2` at compile time
- [ ] **Dead code elimination** — Remove unused bindings
- [ ] **Lambda lifting** — Optimize closure allocation
- [ ] **Inlining** — Inline small functions

#### Tooling

- [ ] **Source maps** — Map generated JS back to Algow source
- [ ] **Better IR dump** — Pretty-print IR with type annotations
- [ ] **Benchmark suite** — Track compilation and runtime performance

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
