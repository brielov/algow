# Chapter 1: Types Introduction

Types are one of the most powerful tools for catching bugs before your code runs. This chapter introduces type systems: what they are, why they matter, and what makes Algow's type system special.

---

## What Are Types?

A **type** classifies values by what you can do with them. For example:

- `42` is a **number**: you can add, subtract, multiply, compare it
- `"hello"` is a **string**: you can concatenate, slice, check its length
- `true` is a **boolean**: you can use it in conditions, negate it

Types tell us what operations make sense. Adding two numbers makes sense. Adding a number and a function doesn't.

### Types as Contracts

Think of types as contracts:

```
add : number -> number -> number
```

This says: "Give me two numbers, and I'll give you a number back." It's a promise about behavior.

If you try to call `add "hello" 42`, you're violating the contract. The type checker catches this before you run the program.

---

## Static vs Dynamic Typing

Languages handle types differently:

### Dynamic Typing (JavaScript, Python, Ruby)

Types are checked at runtime. You can write anything, and errors happen when code runs:

```javascript
function add(x, y) {
  return x + y;
}

add(1, 2);       // 3 - works
add("a", "b");   // "ab" - also works (different operation!)
add(1, "hello"); // "1hello" - surprising behavior
```

**Pros**: Flexible, less upfront work
**Cons**: Bugs surface at runtime, often in production

### Static Typing (TypeScript, Java, Rust)

Types are checked at compile time. Errors are caught before running:

```typescript
function add(x: number, y: number): number {
  return x + y;
}

add(1, 2);       // OK
add(1, "hello"); // Error: Argument of type 'string' is not assignable
```

**Pros**: Catches bugs early, better tooling (autocomplete, refactoring)
**Cons**: More upfront work, less flexibility

### Algow: Static Typing with Inference

Algow is statically typed, but you rarely need to write types. The compiler **infers** them:

```
let add x y = x + y
```

The compiler figures out: `add : number -> number -> number`. You get the safety of static types without the verbosity.

---

## Why Type Inference?

Consider writing types for everything:

```typescript
// TypeScript with explicit types
function map<A, B>(f: (x: A) => B, list: List<A>): List<B> {
  // ...
}
```

Now compare:

```
-- Algow with inference
let rec map f list =
  match list with
  | Nil => Nil
  | Cons x rest => Cons (f x) (map f rest)
  end
```

The compiler infers: `map : (a -> b) -> List a -> List b`. Same type, no annotations.

Type inference gives you:
- **Concise code**: Write what you mean, not types for everything
- **Safety**: Still catches all type errors at compile time
- **Documentation**: Inferred types are available in editor hover

---

## The Hindley-Milner Type System

Algow uses the **Hindley-Milner** (HM) type system, invented independently by Roger Hindley (1969) and Robin Milner (1978). It's used in ML, Haskell, OCaml, F#, and Rust (partially).

### Key Properties

1. **Complete Inference**: Every expression has a type, and the compiler can find it without any annotations.

2. **Principal Types**: The inferred type is the most general possible. If `id = fn x => x` has type `a -> a`, that works for `number -> number`, `string -> string`, and any other type.

3. **Decidable**: The type checking algorithm always terminates with a definite answer.

4. **Let Polymorphism**: Values bound with `let` can be used at different types:

```
let id = fn x => x in
(id 42, id "hello")  -- id used as number->number AND string->string
```

### What Makes HM Special

HM finds the **most general type** automatically. When you write:

```
let apply f x = f x
```

The compiler infers: `apply : (a -> b) -> a -> b`. This works for any types `a` and `b`. You don't have to tell the compiler "this is generic"—it figures it out.

---

## Types in Algow

### Primitive Types

```
number    -- 42, 3.14
string    -- "hello"
boolean   -- true, false
```

### Function Types

Functions types use arrows:

```
number -> number              -- function from number to number
number -> number -> number    -- curried function of two numbers
(number -> number) -> number  -- function that takes a function
```

The arrow is right-associative:
- `a -> b -> c` means `a -> (b -> c)`

### Type Variables

Type variables represent unknown or generic types:

```
a -> a               -- same type in, same type out
a -> b               -- any input type, any output type
(a -> b) -> List a -> List b  -- map function
```

### Algebraic Data Types

User-defined types:

```
data Maybe a = Nothing | Just a
data List a = Nil | Cons a (List a)
data Either a b = Left a | Right b
```

These types can be:
- **Parameterized**: `Maybe a`, `List a` work for any type `a`
- **Recursive**: `List a` contains another `List a`
- **Sum types**: Multiple constructors (like `Nothing | Just a`)

### Tuple Types

Fixed-length, heterogeneous collections:

```
(number, string)           -- pair
(number, string, boolean)  -- triple
```

### Record Types

Named fields:

```
{ x : number, y : number }
{ name : string, age : number }
```

With row polymorphism, a function can work on any record with certain fields:

```
let getX r = r.x
-- getX : { x : a | ... } -> a
-- Works on any record with an 'x' field
```

---

## Type Errors

When types don't match, you get an error:

```
let x = 1 + "hello"
```

```
Error: Cannot unify 'number' with 'string'
  in expression: 1 + "hello"
  '+' expects two numbers, but got number and string
```

The error tells you:
- What went wrong (types don't match)
- Where it happened (the expression)
- What was expected vs. found

---

## Type Annotations

Although inference handles most cases, you can add annotations:

```
let double (x : number) : number = x * 2
```

Annotations are useful for:
- **Documentation**: Making types explicit for readers
- **Constraining**: Forcing a specific type when inference would be more general
- **Error localization**: Helping the compiler give better error messages

---

## The Type Checking Process

Type checking happens after parsing and name resolution:

```
Source Code
    ↓
[Lexer + Parser]
    ↓
AST
    ↓
[Binder]
    ↓
AST + Symbol Table
    ↓
[Type Checker] ←── We are here
    ↓
Typed AST + Type Environment
```

The type checker:

1. **Assigns types** to every expression
2. **Unifies** types when they must match
3. **Generalizes** let bindings to polymorphic types
4. **Reports errors** when types conflict

The result is a typed AST where every node knows its type.

---

## Coming Up

The next chapters dive into the implementation:

- **Type Representation**: How we represent types in code
- **Substitution**: Mapping type variables to concrete types
- **Unification**: Making two types equal
- **Algorithm W**: The complete inference algorithm
- **Let Polymorphism**: Why `let` is special
- **Pattern Matching**: Type checking patterns
- **Type Classes**: Overloading operators

Let's start with how we represent types.
