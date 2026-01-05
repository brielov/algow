# Chapter 1: Intermediate Representations

Before generating executable code, compilers typically transform the AST into one or more **intermediate representations** (IRs). This chapter explains why IRs exist and what design choices Algow makes.

---

## Why Not Generate Code Directly from AST?

The AST faithfully represents source code structure. But that structure often makes code generation difficult:

### Problem 1: Nested Expressions

Consider this expression:

```
f (g x) (h y)
```

The AST represents this as nested applications:
```
App(
  App(f, App(g, x)),
  App(h, y)
)
```

To generate code, we need to:
1. Evaluate `g x` first
2. Store the result somewhere
3. Evaluate `h y`
4. Store that result
5. Apply `f` to both results

But where do we store intermediate results? The AST doesn't tell us.

### Problem 2: Complex Control Flow

Pattern matching with nested constructors:

```
match list with
| Cons x (Cons y rest) => ...
end
```

Generating efficient code requires careful sequencing of tests. The AST represents patterns structurally, not operationally.

### Problem 3: Optimization Opportunities

Optimizations like constant folding and dead code elimination are easier on a simpler representation. The AST carries lots of syntactic details (spans, sugared operators) that complicate analysis.

---

## What Is an IR?

An **intermediate representation** is a data structure that sits between the AST and target code:

```
Source → AST → IR → Target Code
```

An IR is designed to:
1. Make code generation straightforward
2. Enable analysis and optimization
3. Be independent of both source syntax and target details

Most compilers have multiple IRs, each suited to different purposes.

---

## Common IR Designs

### Three-Address Code

Operations have at most two operands and one result:

```
t1 = g x
t2 = h y
t3 = f t1
t4 = t3 t2
```

Named for instructions like `x = y + z` that use three "addresses" (variables).

### Static Single Assignment (SSA)

Every variable is assigned exactly once:

```
t1 = g x
t2 = h y
t3 = f t1
t4 = t3 t2
```

Makes data flow explicit and enables many optimizations. Used by LLVM and many modern compilers.

### Continuation-Passing Style (CPS)

Control flow is made explicit through continuations:

```
(fn t1 =>
  (fn t2 =>
    f t1 (fn t3 =>
      t3 t2))
  (h y))
(g x)
```

Every call receives a "continuation" that represents "what to do next." Used in functional language compilers.

### A-Normal Form (ANF)

A simpler alternative to CPS. All intermediate values are named, and arguments to operations must be "atomic" (variables or constants):

```
let t1 = g x in
let t2 = h y in
let t3 = f t1 in
t3 t2
```

ANF shares many benefits of CPS but is easier to read and work with.

---

## Algow's Choice: ANF

Algow uses **A-Normal Form** because:

1. **Simplicity**: ANF looks like nested `let` expressions—familiar from the source language

2. **Explicit Evaluation Order**: The sequence of `let` bindings makes evaluation order clear

3. **Easy Code Generation**: Every operation has named inputs (atoms), so generating target code is straightforward

4. **Good for Optimization**: Named intermediates make it easy to track uses and eliminate dead code

5. **Type-Directed**: We can annotate every binding with its type, enabling type-directed code generation

---

## The ANF Invariant

The key property of ANF: **all arguments to complex operations are atomic**.

An **atom** is something that requires no computation:
- A literal: `42`, `"hello"`, `true`
- A variable: `x`, `f`, `result`

A **complex operation** requires computation:
- Function application: `f x`
- Binary operations: `x + y`
- Constructing data: `(a, b)`, `{ x = 1 }`
- Conditionals: `if c then a else b`
- Pattern matching: `match x with ...`

### Example Transformation

Source:
```
f (g x) (h y)
```

ANF:
```
let t0 = g x in
let t1 = h y in
let t2 = f t0 in
t2 t1
```

Every application now has atomic arguments:
- `g x`: `g` and `x` are variables (atoms)
- `h y`: `h` and `y` are variables (atoms)
- `f t0`: `f` and `t0` are variables (atoms)
- `t2 t1`: both are variables (atoms)

---

## IR vs AST

| Aspect | AST | IR (ANF) |
|--------|-----|----------|
| Purpose | Represent source structure | Enable code generation |
| Nesting | Arbitrarily deep | Flat (let bindings) |
| Intermediates | Implicit | Named explicitly |
| Types | Attached during type checking | Carried on every node |
| Evaluation order | Implicit in structure | Explicit in binding order |

---

## The IR Pipeline

Algow's compilation pipeline:

```
Source
  ↓ (parse)
AST
  ↓ (bind)
AST with resolved names
  ↓ (check)
Typed AST
  ↓ (lower)
IR (ANF)
  ↓ (optimize)
Optimized IR
  ↓ (generate)
JavaScript
```

The IR is created by **lowering** the typed AST, then transformed by **optimization passes**, then consumed by **code generation**.

---

## Types in the IR

Every IR node carries its type from the type checker:

```typescript
export type IRVar = {
  readonly kind: "IRVar";
  readonly name: string;
  readonly type: Type;  // ← Type information preserved
};
```

This enables:
1. **Type-directed code generation**: Different types may need different representations
2. **Type checking optimizations**: Verify that transformations preserve types
3. **Runtime optimization**: Know when deep equality is needed vs. primitive comparison

---

## Summary

Intermediate representations bridge the gap between source structure and target code:

1. **AST** represents source syntax—good for analysis, hard for code generation
2. **IR** represents computation—easy to generate code from
3. **ANF** is Algow's IR—all arguments are atomic, evaluation order is explicit
4. **Type preservation** enables type-directed code generation

The next chapter explores ANF in detail—its structure and invariants.
