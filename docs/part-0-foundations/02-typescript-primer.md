# Chapter 2: TypeScript Primer

The Algow compiler is written in TypeScript. Before we dive into the code, let's cover the TypeScript features you'll encounter. If you already know TypeScript well, skim this chapter—but pay attention to the section on discriminated unions, as they're central to our AST design.

---

## Why TypeScript?

TypeScript is JavaScript with types. The compiler checks your types at build time, catching errors before your code runs. This is exactly what we want when building a compiler—we're implementing a type system, so it helps to have one ourselves.

TypeScript also runs anywhere JavaScript runs, making it easy to develop and test. And TypeScript's type system is expressive enough to represent the complex data structures compilers need.

---

## Basic Types

TypeScript has several primitive types:

```typescript
// Numbers (integers and floats are both 'number')
const x: number = 42;
const pi: number = 3.14;

// Strings
const name: string = "Algow";

// Booleans
const isReady: boolean = true;

// Arrays
const numbers: number[] = [1, 2, 3];
const names: string[] = ["Alice", "Bob"];

// Tuples (fixed-length arrays with specific types per position)
const pair: [number, string] = [1, "one"];
```

### Type Inference

You don't always need to write types explicitly. TypeScript can infer them:

```typescript
// TypeScript infers 'number' from the value
const x = 42;

// TypeScript infers 'string[]' from the array contents
const names = ["Alice", "Bob"];

// TypeScript infers the return type from the function body
const double = (n: number) => n * 2;  // Returns number
```

We write explicit types when:
- The type isn't obvious from context
- We want to document the API
- We need TypeScript to check against a specific type

---

## Type Aliases

A **type alias** gives a name to a type:

```typescript
// Simple alias
type Name = string;

// Object type alias
type Point = {
  x: number;
  y: number;
};

// Function type alias
type Transformer = (input: string) => string;
```

Once defined, you use the alias wherever you'd use the type:

```typescript
const origin: Point = { x: 0, y: 0 };

const uppercase: Transformer = (s) => s.toUpperCase();
```

Type aliases are purely a compile-time construct. They disappear when TypeScript compiles to JavaScript.

---

## Interfaces

Interfaces are similar to type aliases for objects, but with some differences:

```typescript
interface Point {
  x: number;
  y: number;
}

const origin: Point = { x: 0, y: 0 };
```

Interfaces can extend other interfaces:

```typescript
interface Point3D extends Point {
  z: number;
}

const p: Point3D = { x: 1, y: 2, z: 3 };
```

For this book, you can treat `type` and `interface` as mostly interchangeable for object types. We use `type` throughout Algow because it works better with union types (covered below).

---

## Union Types

A **union type** represents a value that could be one of several types:

```typescript
// A value that's either a number or a string
type NumOrStr = number | string;

const x: NumOrStr = 42;      // OK
const y: NumOrStr = "hello"; // OK
const z: NumOrStr = true;    // Error: boolean isn't in the union
```

Unions are powerful because they let you express "this or that" relationships. But how do you know which type you have at runtime?

---

## Discriminated Unions (Tagged Unions)

**This is the most important section in this chapter.**

A discriminated union is a pattern where each variant has a **tag** (also called a discriminant) that identifies which variant it is. In TypeScript, we use a literal type for the tag:

```typescript
// Each variant has a 'kind' field with a specific string value
type Shape =
  | { kind: "circle"; radius: number }
  | { kind: "rectangle"; width: number; height: number }
  | { kind: "triangle"; base: number; height: number };
```

Now `Shape` can be a circle, rectangle, or triangle. Each variant has:
1. A `kind` field with a specific literal value
2. Additional fields specific to that variant

### Working with Discriminated Unions

To work with a discriminated union, you check the tag:

```typescript
const area = (shape: Shape): number => {
  // TypeScript narrows the type based on the 'kind' check
  if (shape.kind === "circle") {
    // TypeScript knows shape is { kind: "circle", radius: number }
    return Math.PI * shape.radius * shape.radius;
  } else if (shape.kind === "rectangle") {
    // TypeScript knows shape is { kind: "rectangle", width: number, height: number }
    return shape.width * shape.height;
  } else {
    // TypeScript knows shape is { kind: "triangle", base: number, height: number }
    return (shape.base * shape.height) / 2;
  }
};
```

This is called **type narrowing**. After checking `shape.kind === "circle"`, TypeScript knows `shape` is specifically the circle variant, so you can access `shape.radius`.

### Using Switch Statements

Switch statements work well with discriminated unions:

```typescript
const area = (shape: Shape): number => {
  switch (shape.kind) {
    case "circle":
      return Math.PI * shape.radius * shape.radius;
    case "rectangle":
      return shape.width * shape.height;
    case "triangle":
      return (shape.base * shape.height) / 2;
  }
};
```

### Exhaustiveness Checking

TypeScript can verify you've handled all cases:

```typescript
const area = (shape: Shape): number => {
  switch (shape.kind) {
    case "circle":
      return Math.PI * shape.radius * shape.radius;
    case "rectangle":
      return shape.width * shape.height;
    // Oops, forgot triangle!
    default:
      // TypeScript error: 'triangle' isn't handled
      const _exhaustive: never = shape;
      return _exhaustive;
  }
};
```

The `never` type means "this should never happen." If there's a case you didn't handle, `shape` could still have that type, so it can't be assigned to `never`.

### Why This Matters

Algow's AST is a discriminated union. Every expression type has a `kind` field:

```typescript
type Expr =
  | { kind: "Num"; value: number }
  | { kind: "Str"; value: string }
  | { kind: "Bool"; value: boolean }
  | { kind: "Var"; name: string }
  | { kind: "App"; func: Expr; arg: Expr }
  // ... more variants
```

When we write functions that process expressions, we switch on `expr.kind` and handle each case. TypeScript ensures we don't forget any cases and that we access the right fields for each variant.

---

## Readonly Properties

The `readonly` modifier prevents assignment after initialization:

```typescript
interface Point {
  readonly x: number;
  readonly y: number;
}

const origin: Point = { x: 0, y: 0 };
origin.x = 1;  // Error: cannot assign to 'x' because it is read-only
```

For arrays, `readonly` prevents mutation:

```typescript
const numbers: readonly number[] = [1, 2, 3];
numbers.push(4);     // Error: 'push' doesn't exist on 'readonly number[]'
numbers[0] = 10;     // Error: cannot assign
```

We use `readonly` extensively in Algow. AST nodes, types, and most data structures are immutable. This prevents bugs where you accidentally modify something that other code depends on.

### The `Readonly` Utility Type

You can make all properties of a type readonly:

```typescript
type Point = {
  x: number;
  y: number;
};

type ReadonlyPoint = Readonly<Point>;
// Equivalent to { readonly x: number; readonly y: number }
```

---

## Generics

Generics let you write code that works with multiple types. Think of them as type parameters:

```typescript
// A generic function that works with any type T
const identity = <T>(x: T): T => x;

// When called, T is inferred from the argument
identity(42);      // T is number, returns number
identity("hello"); // T is string, returns string
```

### Generic Types

Types can also be generic:

```typescript
// A generic type for optional values
type Option<T> = { kind: "none" } | { kind: "some"; value: T };

// Using it with specific types
const maybeNumber: Option<number> = { kind: "some", value: 42 };
const maybeString: Option<string> = { kind: "none" };
```

### Multiple Type Parameters

You can have multiple type parameters:

```typescript
// A function that takes two different types
const pair = <A, B>(a: A, b: B): [A, B] => [a, b];

pair(1, "hello");  // Returns [number, string]
```

### Generic Constraints

Sometimes you need to constrain what types are allowed:

```typescript
// T must have a 'length' property
const getLength = <T extends { length: number }>(x: T): number => x.length;

getLength("hello");    // OK: strings have length
getLength([1, 2, 3]);  // OK: arrays have length
getLength(42);         // Error: numbers don't have length
```

---

## Maps and Sets

TypeScript has built-in Map and Set types:

```typescript
// Map: key-value pairs
const ages = new Map<string, number>();
ages.set("Alice", 30);
ages.set("Bob", 25);

ages.get("Alice");  // 30
ages.has("Bob");    // true

// Iterate over entries
for (const [name, age] of ages) {
  console.log(`${name} is ${age}`);
}
```

```typescript
// Set: unique values
const seen = new Set<string>();
seen.add("hello");
seen.add("world");
seen.add("hello");  // No effect, already in set

seen.has("hello");  // true
seen.size;          // 2
```

We use Maps extensively for things like:
- Symbol tables (mapping names to their definitions)
- Type environments (mapping names to their type schemes)
- Substitutions (mapping type variables to types)

---

## Arrow Functions

Arrow functions are concise function expressions:

```typescript
// Traditional function
function add(a: number, b: number): number {
  return a + b;
}

// Arrow function (explicit return)
const add = (a: number, b: number): number => {
  return a + b;
};

// Arrow function (implicit return for single expression)
const add = (a: number, b: number): number => a + b;

// No parentheses needed for single parameter
const double = (n: number): number => n * 2;
// Can also write: const double = n => n * 2 (if type can be inferred)
```

Arrow functions capture `this` from their enclosing scope, but we rarely use `this` in Algow—our code is mostly functional.

---

## Destructuring

Destructuring extracts values from objects and arrays:

```typescript
// Object destructuring
const point = { x: 1, y: 2 };
const { x, y } = point;  // x = 1, y = 2

// With renaming
const { x: px, y: py } = point;  // px = 1, py = 2

// Array destructuring
const pair = [1, "hello"];
const [num, str] = pair;  // num = 1, str = "hello"

// In function parameters
const distance = ({ x, y }: Point): number => Math.sqrt(x * x + y * y);
```

### Rest Patterns

The rest pattern (`...`) collects remaining elements:

```typescript
const [first, ...rest] = [1, 2, 3, 4];
// first = 1, rest = [2, 3, 4]

const { x, ...others } = { x: 1, y: 2, z: 3 };
// x = 1, others = { y: 2, z: 3 }
```

---

## Enums

Enums define a set of named constants:

```typescript
// Numeric enum (each value is automatically numbered)
enum Direction {
  Up,    // 0
  Down,  // 1
  Left,  // 2
  Right  // 3
}

const move = (d: Direction) => {
  if (d === Direction.Up) { /* ... */ }
};

// String enum (explicit values)
enum TokenKind {
  Number = "Number",
  String = "String",
  Plus = "Plus",
}
```

### Const Enums

`const enum` is a compile-time only construct that inlines values:

```typescript
const enum TokenKind {
  Number,
  String,
  Plus,
}

// This:
const kind = TokenKind.Number;

// Compiles to:
const kind = 0;  // Value is inlined
```

We use `const enum` for token kinds and other constants where we want zero runtime overhead.

---

## Null and Undefined

TypeScript distinguishes between `null` and `undefined`:

```typescript
let x: number | null = null;  // Can be number or null
let y: number | undefined;    // Can be number or undefined

// Optional properties use undefined
interface Person {
  name: string;
  age?: number;  // Same as: age: number | undefined
}
```

### Non-Null Assertion

The `!` operator tells TypeScript "trust me, this isn't null":

```typescript
const element = document.getElementById("app");
// element is HTMLElement | null

const el = document.getElementById("app")!;
// el is HTMLElement (we assert it's not null)
```

Use this sparingly. If you're wrong, you'll get a runtime error.

### Optional Chaining

The `?.` operator short-circuits on null/undefined:

```typescript
const name = person?.address?.city;
// If person or address is null/undefined, name is undefined
// Otherwise, name is the city
```

---

## Type Guards

Sometimes you need to check types at runtime:

```typescript
const process = (x: string | number) => {
  if (typeof x === "string") {
    // TypeScript knows x is string here
    return x.toUpperCase();
  } else {
    // TypeScript knows x is number here
    return x * 2;
  }
};
```

For discriminated unions, checking the tag is the type guard:

```typescript
const printExpr = (expr: Expr) => {
  if (expr.kind === "Num") {
    console.log(`Number: ${expr.value}`);
  } else if (expr.kind === "Str") {
    console.log(`String: ${expr.value}`);
  }
  // ... etc
};
```

### Custom Type Guards

You can write functions that act as type guards:

```typescript
const isNumber = (x: unknown): x is number => {
  return typeof x === "number";
};

const process = (x: unknown) => {
  if (isNumber(x)) {
    // TypeScript knows x is number here
    return x * 2;
  }
  return 0;
};
```

The `x is number` syntax is a **type predicate**. It tells TypeScript that if the function returns true, `x` is a number.

---

## Utility Types

TypeScript provides utility types for common transformations:

```typescript
// Partial: all properties become optional
type Partial<T> = { [P in keyof T]?: T[P] };

// Required: all properties become required
type Required<T> = { [P in keyof T]-?: T[P] };

// Pick: select some properties
type Pick<T, K extends keyof T> = { [P in K]: T[P] };

// Omit: remove some properties
type Omit<T, K> = Pick<T, Exclude<keyof T, K>>;

// Record: object with specific key and value types
type Record<K extends string | number | symbol, V> = { [P in K]: V };
```

Examples:

```typescript
interface User {
  name: string;
  email: string;
  age: number;
}

// Only name and email
type UserContact = Pick<User, "name" | "email">;

// All properties optional
type PartialUser = Partial<User>;

// String keys, number values
type Scores = Record<string, number>;
```

---

## Summary

Here are the TypeScript features you'll see most in Algow:

1. **Discriminated unions** for AST nodes, types, and values
2. **Readonly properties** for immutable data structures
3. **Generics** for reusable type-safe code
4. **Maps** for symbol tables, environments, and substitutions
5. **Arrow functions** for concise function definitions
6. **Type narrowing** via switch and if statements

The next chapter covers functional programming concepts. If you're already comfortable with recursion, immutability, and higher-order functions, you can skim it. Otherwise, it provides essential background for understanding how the compiler is structured.
