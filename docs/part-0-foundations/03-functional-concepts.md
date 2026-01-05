# Chapter 3: Functional Programming Concepts

The Algow compiler is written in a functional style. This chapter introduces the functional programming concepts you'll encounter. Even if you've never used a functional language, these ideas will make sense—they're just a different way of organizing code.

---

## What Is Functional Programming?

Functional programming is a style where you:

1. Write **pure functions** that don't modify external state
2. Use **immutable data** that never changes after creation
3. Compose small functions into larger ones
4. Prefer **recursion** over loops for repetition

This style leads to code that's easier to test, easier to reason about, and less prone to bugs. Let's look at each concept.

---

## Pure Functions

A **pure function** has two properties:

1. **Same inputs, same output**: Given the same arguments, it always returns the same result
2. **No side effects**: It doesn't modify anything outside itself (no changing global variables, no writing to files, no printing)

### Examples

Pure function:

```typescript
const add = (a: number, b: number): number => a + b;

add(2, 3);  // Always returns 5
add(2, 3);  // Still 5
```

Impure function (modifies external state):

```typescript
let counter = 0;

const increment = (): number => {
  counter += 1;  // Side effect: modifies external variable
  return counter;
};

increment();  // Returns 1
increment();  // Returns 2 (different result, same inputs)
```

Impure function (uses external state):

```typescript
let multiplier = 2;

const scale = (x: number): number => x * multiplier;

scale(5);       // Returns 10
multiplier = 3;
scale(5);       // Returns 15 (different result, same input)
```

### Why Pure Functions?

1. **Testable**: You can test a pure function by checking its output. No setup, no cleanup, no mocks.

2. **Predictable**: The function always does the same thing. No "it works sometimes" bugs.

3. **Parallelizable**: Pure functions can run in parallel because they don't share state.

4. **Composable**: You can combine pure functions freely because they don't interfere with each other.

### Practical Reality

In practice, programs need side effects—they need to read files, display output, and interact with the world. The functional approach is to push side effects to the edges of your program. The core logic is pure; side effects happen only at the boundaries.

In Algow:
- Parsing is pure: source string in, AST out
- Type checking is pure: AST in, types out
- Code generation is pure: IR in, JavaScript string out
- The main function handles I/O (reading files, printing results)

---

## Immutability

**Immutable data** cannot be changed after creation. Instead of modifying data, you create new data.

### Mutable Approach

```typescript
// Mutable: modifying in place
const numbers = [1, 2, 3];
numbers.push(4);  // numbers is now [1, 2, 3, 4]
```

### Immutable Approach

```typescript
// Immutable: creating new data
const numbers = [1, 2, 3];
const moreNumbers = [...numbers, 4];  // [1, 2, 3, 4]
// numbers is still [1, 2, 3]
```

### Why Immutability?

1. **No Spooky Action at a Distance**: When data can't change, code that has a reference to it can trust that reference. There's no risk of another part of the program modifying it unexpectedly.

2. **Easier Debugging**: You can always trace a value back to its creation. It wasn't modified somewhere else.

3. **Safe Sharing**: You can share immutable data freely. Multiple functions can hold references without coordinating.

### In Practice

TypeScript helps enforce immutability with `readonly`:

```typescript
// Arrays
const numbers: readonly number[] = [1, 2, 3];
numbers.push(4);  // Error: 'push' doesn't exist on 'readonly number[]'

// Objects
interface Point {
  readonly x: number;
  readonly y: number;
}

const p: Point = { x: 1, y: 2 };
p.x = 10;  // Error: cannot assign to 'x'
```

In Algow, AST nodes are immutable. When we transform an expression, we create a new node rather than modifying the old one:

```typescript
// Transforming an expression by creating new nodes
const doubleNumbers = (expr: Expr): Expr => {
  if (expr.kind === "Num") {
    // Create a new Num node with doubled value
    return { kind: "Num", value: expr.value * 2 };
  }
  return expr;
};
```

---

## Recursion

**Recursion** is when a function calls itself. It's the functional alternative to loops.

### A Simple Example

Summing an array with a loop:

```typescript
const sum = (numbers: number[]): number => {
  let total = 0;
  for (const n of numbers) {
    total += n;
  }
  return total;
};
```

Summing an array with recursion:

```typescript
const sum = (numbers: number[]): number => {
  // Base case: empty array sums to 0
  if (numbers.length === 0) {
    return 0;
  }
  // Recursive case: first element plus sum of the rest
  const [first, ...rest] = numbers;
  return first + sum(rest);
};
```

### How It Works

```
sum([1, 2, 3])
= 1 + sum([2, 3])
= 1 + (2 + sum([3]))
= 1 + (2 + (3 + sum([])))
= 1 + (2 + (3 + 0))
= 1 + (2 + 3)
= 1 + 5
= 6
```

Each recursive call waits for the next to complete, then adds its contribution.

### Anatomy of Recursion

Every recursive function needs:

1. **Base case(s)**: Conditions where the function returns without recursing. This prevents infinite recursion.

2. **Recursive case(s)**: The function calls itself with a "smaller" problem. Eventually, it reaches a base case.

```typescript
const factorial = (n: number): number => {
  // Base case
  if (n === 0) {
    return 1;
  }
  // Recursive case: n! = n * (n-1)!
  return n * factorial(n - 1);
};
```

### Why Recursion?

1. **Matches Data Structure**: Recursive data structures (trees, lists) are naturally processed with recursive functions. The structure of your code mirrors the structure of your data.

2. **No Mutation Needed**: Loops typically need mutable counters or accumulators. Recursion uses function parameters instead.

3. **Compositional**: Recursive definitions often match mathematical definitions directly.

### Tree Recursion

Our AST is a tree. Processing it naturally uses recursion:

```typescript
// Count the number of nodes in an expression tree
const countNodes = (expr: Expr): number => {
  switch (expr.kind) {
    case "Num":
    case "Bool":
    case "Str":
    case "Var":
      return 1;  // Leaf nodes: count 1
    case "App":
      // Function application: count self + children
      return 1 + countNodes(expr.func) + countNodes(expr.arg);
    case "If":
      // If expression: count self + all branches
      return 1 + countNodes(expr.cond) + countNodes(expr.then) + countNodes(expr.else_);
    // ... handle all cases
  }
};
```

The structure of the code follows the structure of the data.

---

## Higher-Order Functions

A **higher-order function** either:
- Takes a function as an argument, or
- Returns a function as its result

This is possible because functions are **first-class values**—they can be stored in variables, passed as arguments, and returned from functions.

### Functions as Arguments

```typescript
// 'process' takes a function 'f' and applies it to 'x'
const apply = <T, U>(f: (x: T) => U, x: T): U => f(x);

const double = (n: number): number => n * 2;

apply(double, 5);  // Returns 10
```

### Map, Filter, Reduce

The classic higher-order functions for working with collections:

```typescript
const numbers = [1, 2, 3, 4, 5];

// map: apply a function to each element
const doubled = numbers.map(n => n * 2);
// [2, 4, 6, 8, 10]

// filter: keep elements that satisfy a predicate
const evens = numbers.filter(n => n % 2 === 0);
// [2, 4]

// reduce: combine elements into a single value
const sum = numbers.reduce((acc, n) => acc + n, 0);
// 15
```

These replace loops with more declarative code:

```typescript
// Imperative (loop)
const doubleAll = (numbers: number[]): number[] => {
  const result: number[] = [];
  for (const n of numbers) {
    result.push(n * 2);
  }
  return result;
};

// Functional (map)
const doubleAll = (numbers: number[]): number[] => numbers.map(n => n * 2);
```

### Functions Returning Functions

```typescript
// A function that creates adder functions
const makeAdder = (n: number): ((x: number) => number) => {
  return (x: number) => x + n;
};

const add5 = makeAdder(5);
add5(10);  // 15

const add10 = makeAdder(10);
add10(3);  // 13
```

This pattern is used extensively in compilers:

```typescript
// A function that creates a transformer
const makeTransformer = (transform: (n: number) => number) => {
  return (expr: Expr): Expr => {
    if (expr.kind === "Num") {
      return { kind: "Num", value: transform(expr.value) };
    }
    return expr;
  };
};

const doubleNumbers = makeTransformer(n => n * 2);
const negateNumbers = makeTransformer(n => -n);
```

---

## Currying

**Currying** transforms a function that takes multiple arguments into a sequence of functions, each taking one argument.

### Uncurried Version

```typescript
const add = (a: number, b: number): number => a + b;
add(2, 3);  // 5
```

### Curried Version

```typescript
const add = (a: number) => (b: number): number => a + b;
add(2)(3);  // 5

// Partial application
const add2 = add(2);  // A function that adds 2 to its argument
add2(3);  // 5
add2(10); // 12
```

### Why Currying?

Currying makes partial application natural. You can "pre-fill" some arguments to create specialized functions:

```typescript
const multiply = (a: number) => (b: number): number => a * b;

const double = multiply(2);   // Specialized: always multiplies by 2
const triple = multiply(3);   // Specialized: always multiplies by 3

double(5);  // 10
triple(5);  // 15
```

Algow functions are curried by design:

```algow
let add x y = x + y

-- Partial application
let add5 = add 5

add5 10  -- 15
```

The type of `add` is `number -> number -> number`, which means:
- Give it a number, get back a function `number -> number`
- Give that function a number, get back a number

---

## Pattern Matching

**Pattern matching** is a way to destructure data and branch based on its shape. It's more powerful than traditional `if/else` or `switch` statements.

### In Algow

```algow
data Maybe a = Nothing | Just a

let describe x =
  match x with
  | Nothing => "Empty"
  | Just n => "Contains: " ++ toString n
  end
```

### In TypeScript (Simulation)

We simulate pattern matching with switch statements on discriminated unions:

```typescript
type Maybe<T> =
  | { kind: "nothing" }
  | { kind: "just"; value: T };

const describe = (x: Maybe<number>): string => {
  switch (x.kind) {
    case "nothing":
      return "Empty";
    case "just":
      return `Contains: ${x.value}`;
  }
};
```

### Nested Patterns

Pattern matching can go deep:

```algow
data List a = Nil | Cons a (List a)

let second list =
  match list with
  | Nil => Nothing
  | Cons _ Nil => Nothing
  | Cons _ (Cons x _) => Just x
  end
```

This matches:
- Empty list → `Nothing`
- Single-element list → `Nothing`
- List with at least two elements → `Just` the second element

---

## Function Composition

**Function composition** combines functions to create new functions:

```typescript
// compose: run g, then f on the result
const compose = <A, B, C>(
  f: (b: B) => C,
  g: (a: A) => B
): ((a: A) => C) => {
  return (a: A) => f(g(a));
};

const double = (n: number): number => n * 2;
const addOne = (n: number): number => n + 1;

const doubleThenAddOne = compose(addOne, double);
doubleThenAddOne(5);  // double(5) = 10, addOne(10) = 11
```

### Pipelines

The opposite of composition is a pipeline—data flows left to right:

```typescript
const pipe = <A, B, C>(
  f: (a: A) => B,
  g: (b: B) => C
): ((a: A) => C) => {
  return (a: A) => g(f(a));
};

const doubleThenAddOne = pipe(double, addOne);
doubleThenAddOne(5);  // double(5) = 10, addOne(10) = 11
```

Algow has a pipe operator:

```algow
5 |> double |> addOne  -- 11
```

This reads naturally left-to-right: take 5, double it, add one.

---

## Algebraic Data Types

**Algebraic Data Types (ADTs)** are types built from combinations of other types. There are two kinds:

### Product Types (AND)

A product type contains multiple values at once. It's like a struct or object:

```typescript
// A Point has x AND y
type Point = { x: number; y: number };
```

Called "product" because the number of possible values is the product:
- If x can be 10 things and y can be 10 things, Point can be 100 things

### Sum Types (OR)

A sum type is one of several possibilities. It's a discriminated union:

```typescript
// A Shape is a circle OR a rectangle
type Shape =
  | { kind: "circle"; radius: number }
  | { kind: "rectangle"; width: number; height: number };
```

Called "sum" because the number of possible values is the sum:
- If there are 10 possible circles and 10 possible rectangles, there are 20 possible shapes

### In Algow

```algow
-- Product type (tuple or record)
let point = (3, 4)
let person = { name = "Alice", age = 30 }

-- Sum type (data declaration)
data Shape =
  | Circle number
  | Rectangle number number
```

### Why "Algebraic"?

Because they follow algebraic laws! For example:

- `A * 1 = A`: A product with a "unit" type is just A
- `A + 0 = A`: A sum with an "empty" type is just A
- `A * (B + C) = (A * B) + (A * C)`: Distribution works

These laws help us reason about and transform types.

---

## Expression vs Statement

In functional programming, everything is an **expression** that produces a value. This contrasts with **statements** that perform actions but don't produce values.

### Statements (Imperative)

```javascript
// 'if' is a statement; doesn't produce a value
let result;
if (condition) {
  result = valueA;
} else {
  result = valueB;
}
```

### Expressions (Functional)

```typescript
// Ternary is an expression; produces a value
const result = condition ? valueA : valueB;
```

In Algow, everything is an expression:

```algow
-- 'if' is an expression
let result = if x > 0 then "positive" else "non-positive"

-- 'match' is an expression
let description = match x with
  | Nothing => "empty"
  | Just n => "has value"
end
```

This means you can nest expressions freely:

```algow
let max a b = if a > b then a else b
let clamp lo hi x = max lo (min hi x)
```

---

## Putting It Together

Here's how these concepts appear in a typical piece of compiler code:

```typescript
// Pure function: takes AST, returns new AST
// Uses pattern matching (switch on kind)
// Uses recursion for tree traversal
// Immutable: creates new nodes instead of modifying

const transform = (expr: Expr): Expr => {
  switch (expr.kind) {
    case "Num":
      // Leaf node: just return (immutable copy)
      return expr;

    case "BinOp":
      // Recursive case: transform children, create new node
      const left = transform(expr.left);
      const right = transform(expr.right);
      return { kind: "BinOp", op: expr.op, left, right };

    case "If":
      // More recursion for each branch
      const cond = transform(expr.cond);
      const then_ = transform(expr.then);
      const else_ = transform(expr.else_);
      return { kind: "If", cond, then: then_, else_: else_ };

    // ... handle all cases
  }
};
```

This function:
- Is **pure**: no side effects, same input → same output
- Uses **immutability**: creates new nodes instead of modifying
- Uses **recursion**: processes the tree by calling itself
- Uses **pattern matching**: switches on the expression kind
- Demonstrates **exhaustive handling**: every case is covered

---

## Summary

These functional concepts will appear throughout the compiler:

1. **Pure functions**: Most functions take data and return data, with no side effects
2. **Immutability**: AST nodes, types, and environments are never modified
3. **Recursion**: Tree structures are processed recursively
4. **Higher-order functions**: Functions that take or return other functions
5. **Pattern matching**: Switching on discriminated union tags
6. **Algebraic data types**: AST is a sum type; nodes are product types

You don't need to master these concepts before starting. They'll become natural as you see them used throughout the codebase. The next chapter begins the actual compiler implementation, starting with the lexer.
