# Chapter 5: Abstract Syntax Tree

The parser produces an **Abstract Syntax Tree (AST)**—a tree structure representing the program. This chapter explores how we design and represent the AST in TypeScript.

---

## What Is an AST?

An Abstract Syntax Tree represents the structure of a program without the noise of concrete syntax. Consider:

```
let x = 1 + 2 * 3
```

The concrete syntax includes keywords (`let`), operators (`=`, `+`, `*`), and whitespace. The AST captures only the meaning:

```
Let
├── name: "x"
└── value: BinOp(+)
           ├── left: Num(1)
           └── right: BinOp(*)
                      ├── left: Num(2)
                      └── right: Num(3)
```

The AST is:

- **Abstract**: Ignores syntactic details (parentheses, whitespace, keywords)
- **Structured**: Represents hierarchical relationships
- **Typed**: Each node has a specific type with known fields

---

## Design Philosophy

Our AST design follows several principles:

### 1. Discriminated Unions

Every node type has a `kind` field that uniquely identifies it:

```typescript
type Expr =
  | { kind: "Num"; value: number }
  | { kind: "Var"; name: string }
  | { kind: "BinOp"; op: Op; left: Expr; right: Expr }
  // ... more variants
```

This enables type-safe pattern matching in TypeScript.

### 2. Immutability

All fields are `readonly`:

```typescript
export interface Num extends Node {
  readonly kind: "Num";
  readonly value: number;
}
```

Once created, nodes never change. Transformations create new nodes.

### 3. Optional Spans

Spans (source positions) are optional:

```typescript
export interface Node {
  readonly span?: Span;
}
```

This supports programmatically-created AST (like prelude functions) that don't correspond to source text.

### 4. Smart Constructors

Helper functions make AST construction concise:

```typescript
// Instead of:
const expr = { kind: "Num", value: 42, span: { start: 0, end: 2 } };

// We write:
const expr = num(42, span(0, 2));
```

---

## The Expression Type

All expressions share a base interface:

```typescript
export interface Span {
  readonly start: number;
  readonly end: number;
}

export interface Node {
  readonly span?: Span;
}
```

The main `Expr` type is a discriminated union:

```typescript
/**
 * The root expression type.
 * Each variant has a unique `kind` field for type narrowing.
 */
export type Expr =
  | Con           // Constants (Num | Bool | Str)
  | Let           // Let bindings
  | LetRec        // Recursive let bindings
  | Var           // Variable references
  | Abs           // Lambda abstractions
  | App           // Function application
  | Tuple         // Tuples: (a, b, c)
  | Record        // Records: { x = 1, y = 2 }
  | FieldAccess   // Field access: record.field
  | TupleIndex    // Tuple indexing: tuple.0
  | If            // Conditional
  | BinOp         // Binary operators
  | Match;        // Pattern matching
```

---

## Constants (Literals)

Constants are values known at parse time:

```typescript
/**
 * Constants are literal values.
 */
export type Con = Num | Bool | Str;

/**
 * Numeric literal.
 * Examples: 42, 3.14
 */
export interface Num extends Node {
  readonly kind: "Num";
  readonly value: number;
}

/**
 * Boolean literal.
 * Examples: true, false
 */
export interface Bool extends Node {
  readonly kind: "Bool";
  readonly value: boolean;
}

/**
 * String literal.
 * Example: "hello"
 */
export interface Str extends Node {
  readonly kind: "Str";
  readonly value: string;
}
```

Smart constructors:

```typescript
export const num = (value: number, span?: Span): Num =>
  ({ kind: "Num", value, span });

export const bool = (value: boolean, span?: Span): Bool =>
  ({ kind: "Bool", value, span });

export const str = (value: string, span?: Span): Str =>
  ({ kind: "Str", value, span });
```

---

## Variable References

```typescript
/**
 * Variable reference - looks up a name in the environment.
 *
 * Can refer to:
 * - Let-bound values
 * - Function parameters
 * - Constructors (Just, Cons, Nothing)
 * - Built-in functions
 */
export interface Var extends Node {
  readonly kind: "Var";
  readonly name: string;
}
```

Smart constructor:

```typescript
export const var_ = (name: string, span?: Span): Var =>
  ({ kind: "Var", name, span });
```

Note the underscore: `var_` because `var` is a JavaScript keyword.

---

## Lambda Abstractions

```typescript
/**
 * Lambda abstraction - an anonymous function.
 *
 * Syntax: fn param => body
 * Example: fn x => x + 1
 *
 * Only single-parameter functions. Multi-parameter functions
 * are represented as curried chains: fn x => fn y => x + y
 */
export interface Abs extends Node {
  readonly kind: "Abs";
  readonly param: string;         // Parameter name
  readonly paramSpan?: Span;      // Span of parameter name (for LSP)
  readonly paramType?: TypeExpr;  // Optional type annotation
  readonly body: Expr;            // Function body
}
```

Smart constructor:

```typescript
export const abs = (
  param: string,
  body: Expr,
  span?: Span,
  paramSpan?: Span,
  paramType?: TypeExpr,
): Abs => ({ kind: "Abs", param, body, span, paramSpan, paramType });
```

---

## Function Application

```typescript
/**
 * Function application - applies a function to an argument.
 *
 * Syntax: func arg
 * Example: add 1, map f xs
 *
 * Application is left-associative: f x y z = ((f x) y) z
 */
export interface App extends Node {
  readonly kind: "App";
  readonly func: Expr;   // The function being applied
  readonly param: Expr;  // The argument
}
```

Note that we call it `param` (the parameter being provided), not `arg`. This matches the term from lambda calculus.

Multi-argument calls are nested applications:

```
add 1 2
```

Becomes:

```
App
├── func: App
│         ├── func: Var("add")
│         └── param: Num(1)
└── param: Num(2)
```

---

## Let Bindings

```typescript
/**
 * Let binding - introduces a polymorphic variable.
 *
 * Syntax: let name = value in body
 * Example: let id = fn x => x in id 42
 *
 * The binding is generalized (made polymorphic), so:
 *   let id = fn x => x in (id 42, id "hello")
 * works because id has type ∀a. a -> a
 */
export interface Let extends Node {
  readonly kind: "Let";
  readonly name: string;          // Variable name
  readonly nameSpan?: Span;       // For LSP features
  readonly returnType?: TypeExpr; // Optional type annotation
  readonly value: Expr;           // The bound expression
  readonly body: Expr;            // Where the binding is used
}
```

### Recursive Bindings

```typescript
/**
 * A single binding in a recursive let.
 */
export interface RecBinding {
  readonly name: string;
  readonly nameSpan?: Span;
  readonly returnType?: TypeExpr;
  readonly value: Expr;
}

/**
 * Recursive let binding.
 *
 * Syntax: let rec f = ... in body
 *         let rec f = ... and g = ... in body
 *
 * All binding names are in scope within all values,
 * enabling mutual recursion.
 */
export interface LetRec extends Node {
  readonly kind: "LetRec";
  readonly bindings: readonly RecBinding[];
  readonly body: Expr;
}
```

The key difference: in regular `let`, the name isn't in scope while evaluating the value. In `let rec`, it is—enabling recursion.

---

## Compound Data Types

### Tuples

```typescript
/**
 * Tuple - a fixed-length collection of heterogeneous values.
 *
 * Syntax: (a, b, c)
 * Example: (1, "hello", true) has type (number, string, boolean)
 *
 * Unlike lists:
 * - Fixed length known at compile time
 * - Can contain different types
 * - Positional access via tuple.0, tuple.1, etc.
 */
export interface Tuple extends Node {
  readonly kind: "Tuple";
  readonly elements: readonly Expr[];
}
```

### Records

```typescript
/**
 * Record - a collection of named fields.
 *
 * Syntax: { x = 1, y = 2 }
 *
 * Records have structural typing: two records with the same
 * fields are compatible regardless of declaration order.
 */
export interface Record extends Node {
  readonly kind: "Record";
  readonly fields: readonly RecordField[];
}

export interface RecordField extends Node {
  readonly name: string;
  readonly value: Expr;
}
```

### Field and Index Access

```typescript
/**
 * Field access - extracts a field from a record.
 *
 * Syntax: record.field
 * Example: person.name
 */
export interface FieldAccess extends Node {
  readonly kind: "FieldAccess";
  readonly record: Expr;
  readonly field: string;
}

/**
 * Tuple index - extracts an element by position.
 *
 * Syntax: tuple.0, tuple.1
 * Example: pair.0
 */
export interface TupleIndex extends Node {
  readonly kind: "TupleIndex";
  readonly tuple: Expr;
  readonly index: number;
}
```

---

## Control Flow

### Conditionals

```typescript
/**
 * If expression - conditional.
 *
 * Syntax: if cond then thenBranch else elseBranch
 *
 * Both branches must have the same type.
 * This is an expression, not a statement—it returns a value.
 */
export interface If extends Node {
  readonly kind: "If";
  readonly cond: Expr;
  readonly then: Expr;
  readonly else: Expr;
}
```

### Binary Operators

```typescript
/**
 * Binary operator.
 *
 * Syntax: left op right
 * Example: x + y, a == b
 */
export interface BinOp extends Node {
  readonly kind: "BinOp";
  readonly op: Op;
  readonly left: Expr;
  readonly right: Expr;
}

/**
 * All supported operators.
 */
export type Op =
  | "+" | "-" | "/" | "*"     // Arithmetic (+ also works for string concatenation)
  | "<" | "<=" | ">" | ">="   // Comparison
  | "==" | "!=";              // Equality
```

---

## Pattern Matching

Pattern matching is a powerful feature for destructuring data:

```typescript
/**
 * Match expression.
 *
 * Syntax:
 *   match expr with
 *   | pattern1 => body1
 *   | pattern2 => body2
 *   end
 */
export interface Match extends Node {
  readonly kind: "Match";
  readonly expr: Expr;              // The value being matched
  readonly cases: readonly Case[];  // The patterns and bodies
}

export interface Case extends Node {
  readonly pattern: Pattern;  // What to match
  readonly guard?: Expr;      // Optional guard condition
  readonly body: Expr;        // What to return if matched
}
```

### Pattern Types

```typescript
export type Pattern =
  | PVar       // Variable: binds the value
  | PWildcard  // Wildcard: matches anything, binds nothing
  | PCon       // Constructor: matches and destructures
  | PLit       // Literal: matches exact value
  | PRecord    // Record: destructures fields
  | PTuple     // Tuple: destructures by position
  | PAs        // As: binds name to whole value
  | POr;       // Or: matches if any alternative matches
```

#### Variable Pattern

```typescript
/**
 * Matches anything, binds the value to a name.
 * Always matches.
 */
export interface PVar extends Node {
  readonly kind: "PVar";
  readonly name: string;
}
```

#### Wildcard Pattern

```typescript
/**
 * Matches anything, binds nothing.
 * Use when you need to match a position but don't need the value.
 */
export interface PWildcard extends Node {
  readonly kind: "PWildcard";
}
```

#### Constructor Pattern

```typescript
/**
 * Matches and destructures algebraic data types.
 *
 * Examples:
 * - Nil matches empty list
 * - Cons x xs matches non-empty list
 * - Just value matches Some variant
 */
export interface PCon extends Node {
  readonly kind: "PCon";
  readonly name: string;              // Constructor name
  readonly args: readonly Pattern[];  // Arguments to match
  readonly nameSpan?: Span;
}
```

#### Literal Pattern

```typescript
/**
 * Matches a specific constant value.
 *
 * Example: | 0 => "zero"
 */
export interface PLit extends Node {
  readonly kind: "PLit";
  readonly value: number | string | boolean;
}
```

#### Tuple Pattern

```typescript
/**
 * Destructures a tuple by position.
 *
 * Example: (x, y, z) matches a 3-tuple
 */
export interface PTuple extends Node {
  readonly kind: "PTuple";
  readonly elements: readonly Pattern[];
}
```

#### Record Pattern

```typescript
/**
 * Destructures a record by field names.
 *
 * Example: { x, y } matches and binds x and y fields
 */
export interface PRecord extends Node {
  readonly kind: "PRecord";
  readonly fields: readonly PRecordField[];
}

export interface PRecordField extends Node {
  readonly name: string;
  readonly pattern: Pattern;
}
```

#### As Pattern

```typescript
/**
 * Binds the whole value while also destructuring it.
 *
 * Example: Cons x rest as whole
 * Binds: x (head), rest (tail), whole (entire list)
 */
export interface PAs extends Node {
  readonly kind: "PAs";
  readonly pattern: Pattern;  // Inner pattern
  readonly name: string;      // Name for whole value
  readonly nameSpan?: Span;
}
```

#### Or Pattern

```typescript
/**
 * Matches if any alternative matches.
 *
 * Example: Nothing | Just Nothing => "empty-ish"
 *
 * All alternatives must bind the same variables.
 */
export interface POr extends Node {
  readonly kind: "POr";
  readonly alternatives: readonly Pattern[];
}
```

---

## Data Declarations

Data declarations define algebraic data types:

```typescript
/**
 * Data type declaration.
 *
 * Syntax: data TypeName a b = Con1 args | Con2 args
 * Example: data Maybe a = Nothing | Just a
 */
export interface DataDecl {
  readonly name: string;                       // Type name
  readonly params: readonly string[];          // Type parameters
  readonly constructors: readonly ConDecl[];   // Constructors
}

/**
 * Constructor declaration.
 */
export interface ConDecl {
  readonly name: string;                  // Constructor name
  readonly fields: readonly TypeExpr[];   // Field types
}
```

---

## Type Expressions

For type annotations:

```typescript
/**
 * Type expressions for annotations.
 */
export type TypeExpr =
  | TyVar   // a - type variable
  | TyCon   // number, string, List
  | TyApp   // List a - type application
  | TyFun;  // a -> b - function type

export interface TyVar {
  readonly kind: "TyVar";
  readonly name: string;
}

export interface TyCon {
  readonly kind: "TyCon";
  readonly name: string;
}

export interface TyApp {
  readonly kind: "TyApp";
  readonly con: TypeExpr;
  readonly arg: TypeExpr;
}

export interface TyFun {
  readonly kind: "TyFun";
  readonly from: TypeExpr;
  readonly to: TypeExpr;
}
```

---

## Smart Constructors

Smart constructors provide a clean API for building AST nodes:

```typescript
export const num = (value: number, span?: Span): Num =>
  ({ kind: "Num", value, span });

export const bool = (value: boolean, span?: Span): Bool =>
  ({ kind: "Bool", value, span });

export const str = (value: string, span?: Span): Str =>
  ({ kind: "Str", value, span });

export const var_ = (name: string, span?: Span): Var =>
  ({ kind: "Var", name, span });

export const abs = (param: string, body: Expr, span?: Span, paramSpan?: Span, paramType?: TypeExpr): Abs =>
  ({ kind: "Abs", param, body, span, paramSpan, paramType });

export const app = (func: Expr, param: Expr, span?: Span): App =>
  ({ kind: "App", func, param, span });

export const let_ = (name: string, value: Expr, body: Expr, span?: Span, nameSpan?: Span, returnType?: TypeExpr): Let =>
  ({ kind: "Let", name, value, body, span, nameSpan, returnType });

export const letRec = (bindings: readonly RecBinding[], body: Expr, span?: Span): LetRec =>
  ({ kind: "LetRec", bindings, body, span });

export const if_ = (cond: Expr, then_: Expr, else_: Expr, span?: Span): If =>
  ({ kind: "If", cond, then: then_, else: else_, span });

export const binOp = (op: Op, left: Expr, right: Expr, span?: Span): BinOp =>
  ({ kind: "BinOp", op, left, right, span });

export const match = (expr: Expr, cases: readonly Case[], span?: Span): Match =>
  ({ kind: "Match", expr, cases, span });

export const tuple = (elements: readonly Expr[], span?: Span): Tuple =>
  ({ kind: "Tuple", elements, span });

export const record = (fields: readonly RecordField[], span?: Span): Record =>
  ({ kind: "Record", fields, span });
```

---

## Working with the AST

### Pattern Matching on Expressions

```typescript
const countNodes = (expr: Expr): number => {
  switch (expr.kind) {
    case "Num":
    case "Bool":
    case "Str":
    case "Var":
      return 1;  // Leaf nodes

    case "Abs":
      return 1 + countNodes(expr.body);

    case "App":
      return 1 + countNodes(expr.func) + countNodes(expr.param);

    case "Let":
      return 1 + countNodes(expr.value) + countNodes(expr.body);

    case "If":
      return 1 + countNodes(expr.cond) + countNodes(expr.then) + countNodes(expr.else);

    case "BinOp":
      return 1 + countNodes(expr.left) + countNodes(expr.right);

    // ... etc
  }
};
```

TypeScript's type narrowing ensures we access the right fields for each kind.

### Transforming the AST

```typescript
// Double all number literals
const doubleNumbers = (expr: Expr): Expr => {
  switch (expr.kind) {
    case "Num":
      return num(expr.value * 2, expr.span);

    case "App":
      return app(
        doubleNumbers(expr.func),
        doubleNumbers(expr.param),
        expr.span
      );

    // ... transform children recursively

    default:
      return expr;  // Leave unchanged
  }
};
```

We create new nodes rather than modifying existing ones.

---

## Summary

The AST is the central data structure of the compiler:

1. **Discriminated unions** enable type-safe pattern matching
2. **Immutability** prevents accidental modifications
3. **Optional spans** support programmatic AST construction
4. **Smart constructors** provide a clean construction API
5. **Expressions** form a tree representing program structure
6. **Patterns** enable destructuring and binding

The next chapter covers name resolution—connecting variable uses to their definitions.
