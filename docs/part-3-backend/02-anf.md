# Chapter 2: A-Normal Form

A-Normal Form (ANF) is a restricted representation where all intermediate values have names. This chapter explores Algow's IR structure in detail.

---

## The ANF Structure

ANF expressions fall into three categories:

1. **Atoms**: Values requiring no computation
2. **Bindings**: Complex operations producing values
3. **Expressions**: Combinations of atoms and bindings via let

Let's examine each category.

---

## Atoms

An atom is a value you can use directly—no computation needed:

```typescript
export type IRAtom = IRLit | IRVar;

export type IRLit = {
  readonly kind: "IRLit";
  readonly value: number | string | boolean;
  readonly type: Type;
};

export type IRVar = {
  readonly kind: "IRVar";
  readonly name: string;
  readonly type: Type;
};
```

### Literals

Constant values embedded in the IR:

```typescript
irLit(42, tNum)        // number literal
irLit("hello", tStr)   // string literal
irLit(true, tBool)     // boolean literal
```

### Variables

References to named values:

```typescript
irVar("x", tNum)       // reference to variable x
irVar("f", funcType)   // reference to function f
```

The key property: both literals and variables carry their types. This is essential for type-directed code generation.

---

## Expressions

ANF expressions combine atoms and bindings:

```typescript
export type IRExpr = IRAtomExpr | IRLet | IRLetRec;
```

### Atom Expressions

The simplest expression—just an atom in terminal position:

```typescript
export type IRAtomExpr = {
  readonly kind: "IRAtomExpr";
  readonly atom: IRAtom;
  readonly type: Type;
};
```

Example: The expression `42` becomes:

```typescript
irAtomExpr(irLit(42, tNum))
```

### Let Bindings

The workhorse of ANF—binds a name to a computed value:

```typescript
export type IRLet = {
  readonly kind: "IRLet";
  readonly name: string;
  readonly binding: IRBinding;
  readonly body: IRExpr;
  readonly type: Type;
};
```

Example: `let x = 1 + 2 in x` becomes:

```typescript
irLet("x",
  irBinOpBinding("+", irLit(1, tNum), irLit(2, tNum), tNum, tNum),
  irAtomExpr(irVar("x", tNum))
)
```

The type of a `let` expression is the type of its body.

### Recursive Let

For recursive and mutually recursive bindings:

```typescript
export type IRLetRec = {
  readonly kind: "IRLetRec";
  readonly bindings: readonly IRRecBinding[];
  readonly body: IRExpr;
  readonly type: Type;
};

export type IRRecBinding = {
  readonly name: string;
  readonly binding: IRBinding;
};
```

All bindings in a `letrec` are in scope for all the binding values, enabling mutual recursion.

---

## Bindings

Bindings are the "complex operations" that compute values. They appear on the right-hand side of let expressions:

```typescript
export type IRBinding =
  | IRAtomBinding
  | IRAppBinding
  | IRBinOpBinding
  | IRIfBinding
  | IRTupleBinding
  | IRRecordBinding
  | IRFieldAccessBinding
  | IRTupleIndexBinding
  | IRMatchBinding
  | IRLambdaBinding
  | IRClosureBinding;
```

Let's examine each one.

### Atom Binding

Trivial binding for uniformity—just wraps an atom:

```typescript
export type IRAtomBinding = {
  readonly kind: "IRAtomBinding";
  readonly atom: IRAtom;
  readonly type: Type;
};
```

### Function Application

Applies a function to an argument—both must be atoms (the ANF invariant!):

```typescript
export type IRAppBinding = {
  readonly kind: "IRAppBinding";
  readonly func: IRAtom;    // Must be atom
  readonly arg: IRAtom;     // Must be atom
  readonly type: Type;
};
```

Example: `f x` becomes:

```typescript
irAppBinding(irVar("f", funcType), irVar("x", argType), resultType)
```

### Binary Operation

Binary operators with atomic operands:

```typescript
export type IRBinOpBinding = {
  readonly kind: "IRBinOpBinding";
  readonly op: Op;
  readonly left: IRAtom;      // Must be atom
  readonly right: IRAtom;     // Must be atom
  readonly operandType: Type; // For dispatch (e.g., number vs string)
  readonly type: Type;        // Result type
};
```

The `operandType` is crucial for operators like `+` that behave differently on different types (addition vs concatenation).

### Conditional

If-then-else with an atomic condition:

```typescript
export type IRIfBinding = {
  readonly kind: "IRIfBinding";
  readonly cond: IRAtom;         // Must be atom
  readonly thenBranch: IRExpr;   // Full expression
  readonly elseBranch: IRExpr;   // Full expression
  readonly type: Type;
};
```

The condition must be atomic, but the branches are full expressions (they can contain their own let bindings).

### Tuple Construction

Build a tuple from atomic elements:

```typescript
export type IRTupleBinding = {
  readonly kind: "IRTupleBinding";
  readonly elements: readonly IRAtom[];  // All must be atoms
  readonly type: Type;
};
```

### Record Construction

Build a record from atomic field values:

```typescript
export type IRRecordBinding = {
  readonly kind: "IRRecordBinding";
  readonly fields: readonly IRRecordField[];
  readonly type: Type;
};

export type IRRecordField = {
  readonly name: string;
  readonly value: IRAtom;  // Must be atom
};
```

### Field Access

Extract a field from an atomic record value:

```typescript
export type IRFieldAccessBinding = {
  readonly kind: "IRFieldAccessBinding";
  readonly record: IRAtom;  // Must be atom
  readonly field: string;
  readonly type: Type;
};
```

### Tuple Index

Extract an element from an atomic tuple:

```typescript
export type IRTupleIndexBinding = {
  readonly kind: "IRTupleIndexBinding";
  readonly tuple: IRAtom;  // Must be atom
  readonly index: number;
  readonly type: Type;
};
```

### Pattern Matching

Match on an atomic scrutinee:

```typescript
export type IRMatchBinding = {
  readonly kind: "IRMatchBinding";
  readonly scrutinee: IRAtom;  // Must be atom
  readonly cases: readonly IRCase[];
  readonly type: Type;
};

export type IRCase = {
  readonly pattern: IRPattern;
  readonly guard?: IRExpr;
  readonly body: IRExpr;
};
```

### Lambda

Function definition:

```typescript
export type IRLambdaBinding = {
  readonly kind: "IRLambdaBinding";
  readonly param: string;
  readonly paramType: Type;
  readonly body: IRExpr;
  readonly type: Type;
  readonly tailRecursive?: {
    readonly selfName: string;
    readonly params: readonly string[];
  };
};
```

The optional `tailRecursive` field is added by optimization passes to enable tail call optimization.

### Closure

Explicit closure (after closure conversion, if used):

```typescript
export type IRClosureBinding = {
  readonly kind: "IRClosureBinding";
  readonly funcId: string;
  readonly captures: readonly IRAtom[];
  readonly type: Type;
};
```

---

## IR Patterns

Patterns in the IR mirror AST patterns but carry type information:

```typescript
export type IRPattern =
  | IRPVar
  | IRPWildcard
  | IRPCon
  | IRPLit
  | IRPTuple
  | IRPRecord
  | IRPAs
  | IRPOr;
```

Each pattern variant carries its type:

```typescript
export type IRPVar = {
  readonly kind: "IRPVar";
  readonly name: string;
  readonly type: Type;
};

export type IRPCon = {
  readonly kind: "IRPCon";
  readonly name: string;
  readonly args: readonly IRPattern[];
  readonly type: Type;
};
```

---

## Complete Example

Let's trace through a complete example:

Source:

```
let add x y = x + y in add 1 2
```

After lowering to ANF:

```typescript
irLet("add",
  irLambdaBinding("x", tNum,
    irLet("_fn0",
      irLambdaBinding("y", tNum,
        irLet("_t0",
          irBinOpBinding("+", irVar("x", tNum), irVar("y", tNum), tNum, tNum),
          irAtomExpr(irVar("_t0", tNum))
        ),
        tfun(tNum, tNum)
      ),
      irAtomExpr(irVar("_fn0", tfun(tNum, tNum)))
    ),
    tfun(tNum, tfun(tNum, tNum))
  ),
  irLet("_t1",
    irAppBinding(irVar("add", tfun(tNum, tfun(tNum, tNum))), irLit(1, tNum), tfun(tNum, tNum)),
    irLet("_t2",
      irAppBinding(irVar("_t1", tfun(tNum, tNum)), irLit(2, tNum), tNum),
      irAtomExpr(irVar("_t2", tNum))
    )
  )
)
```

Key observations:

1. The curried function becomes nested lambdas
2. The application `add 1 2` becomes two separate applications
3. Every intermediate result has a name (`_t0`, `_t1`, `_t2`)
4. Every node carries its type

---

## Smart Constructors

The IR module provides smart constructors for building IR nodes:

```typescript
// Atoms
export const irLit = (value: number | string | boolean, type: Type): IRLit => ...
export const irVar = (name: string, type: Type): IRVar => ...

// Expressions
export const irAtomExpr = (atom: IRAtom): IRAtomExpr => ...
export const irLet = (name: string, binding: IRBinding, body: IRExpr): IRLet => ...
export const irLetRec = (bindings: readonly IRRecBinding[], body: IRExpr): IRLetRec => ...

// Bindings
export const irAppBinding = (func: IRAtom, arg: IRAtom, type: Type): IRAppBinding => ...
export const irBinOpBinding = (op: Op, left: IRAtom, right: IRAtom, ...) => ...
// ... and so on
```

These ensure consistent construction and derive types where possible.

---

## Summary

ANF provides a clean intermediate representation:

1. **Atoms** are values requiring no computation (literals, variables)
2. **Bindings** are complex operations (applications, operators, etc.)
3. **Expressions** combine these via let bindings
4. **All arguments to complex operations are atoms** (the ANF invariant)
5. **Types are preserved** on every node

The next chapter shows how we transform the AST into this ANF representation through a process called lowering.
