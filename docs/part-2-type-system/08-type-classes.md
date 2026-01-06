# Chapter 8: Type Classes

Type classes provide constrained polymorphism—types that can vary but must support certain operations. Algow uses a simplified type class system to enable operators like `+` and `==` to work on multiple types.

---

## The Problem

Consider the equality operator `==`:

```
42 == 42        -- number comparison
"hi" == "hi"    -- string comparison
true == false   -- boolean comparison
```

All three work, but with different underlying operations. What type should `==` have?

- `number -> number -> boolean`?
- `string -> string -> boolean`?
- `boolean -> boolean -> boolean`?

None of these captures the full story. We need a type that says "any type that supports equality."

---

## Constrained Polymorphism

The solution: **type classes**. Instead of `∀a. a -> a -> boolean`, we write:

```
∀a. Eq a => a -> a -> boolean
```

Read as: "for all types `a` that satisfy `Eq`, this is a function from `a` and `a` to `boolean`."

The `Eq a =>` part is a **constraint**—a promise that the caller must fulfill. Only types with an `Eq` instance can be compared.

---

## Algow's Type Classes

Algow has three built-in type classes:

### Eq (Equality)

Types that support `==` and `!=`:

| Type      | Supports Eq?    |
| --------- | --------------- |
| `number`  | Yes             |
| `string`  | Yes             |
| `boolean` | Yes             |
| `a -> b`  | No              |
| `List a`  | No (simplified) |

### Ord (Ordering)

Types that support `<`, `>`, `<=`, `>=`:

| Type      | Supports Ord? |
| --------- | ------------- |
| `number`  | Yes           |
| `string`  | Yes           |
| `boolean` | No            |
| `a -> b`  | No            |

### Add (Addition/Concatenation)

Types that support `+`:

| Type      | Supports Add?       |
| --------- | ------------------- |
| `number`  | Yes (addition)      |
| `string`  | Yes (concatenation) |
| `boolean` | No                  |
| `a -> b`  | No                  |

---

## How Constraints Work

### The Constraint Type

Constraints record requirements during inference:

```typescript
type Constraint = {
  readonly className: string;  // "Eq", "Ord", or "Add"
  readonly type: Type;         // The type that must satisfy this class
};
```

For example, inferring `x == y` generates:

```
{ className: "Eq", type: <type of x> }
```

### The Instance Table

A simple lookup table records which types support which classes:

```typescript
const instances: Map<string, Set<string>> = new Map([
  ["Eq", new Set(["number", "string", "boolean"])],
  ["Ord", new Set(["number", "string"])],
  ["Add", new Set(["number", "string"])],
]);
```

This is much simpler than Haskell's open-ended system—no user-defined instances or superclass constraints.

---

## Constraint Generation

Constraints are generated during inference when we encounter operators:

```typescript
const inferBinOp = (
  ctx: CheckContext,
  env: TypeEnv,
  registry: ConstructorRegistry,
  expr: BinOp
): InferResult => {
  // Infer left operand
  const [s1, leftType, c1] = inferExpr(ctx, env, registry, expr.left);

  // Infer right operand
  const [s2, rightType, c2] = inferExpr(ctx, applySubstEnv(s1, env), registry, expr.right);

  // Operands must have the same type
  const s3 = unify(ctx, applySubst(s2, leftType), rightType, expr.span);
  const operandType = applySubst(s3, rightType);

  const subst = composeSubst(composeSubst(s1, s2), s3);
  const constraints = [...applySubstConstraints(subst, c1), ...applySubstConstraints(subst, c2)];

  switch (expr.op) {
    // Addition: Add constraint
    case "+": {
      constraints.push({ className: "Add", type: operandType });
      return [subst, operandType, constraints];
    }

    // Other arithmetic: no constraint (must be number)
    case "-":
    case "/":
    case "*": {
      const s4 = unify(ctx, operandType, tNum, expr.span);
      return [composeSubst(subst, s4), tNum, constraints];
    }

    // Comparison: Ord constraint
    case "<":
    case ">":
    case "<=":
    case ">=": {
      constraints.push({ className: "Ord", type: operandType });
      return [subst, tBool, constraints];
    }

    // Equality: Eq constraint
    case "==":
    case "!=": {
      constraints.push({ className: "Eq", type: operandType });
      return [subst, tBool, constraints];
    }
  }
};
```

### Operator Classification

| Operator             | Constraint            | Return Type      |
| -------------------- | --------------------- | ---------------- |
| `+`                  | Add                   | Same as operands |
| `-`, `*`, `/`        | None (must be number) | `number`         |
| `<`, `>`, `<=`, `>=` | Ord                   | `boolean`        |
| `==`, `!=`           | Eq                    | `boolean`        |

Notice: `-`, `*`, `/` require `number` directly (via unification) rather than using a constraint. This is because they only work on numbers—no constraint polymorphism needed. The `+` operator is overloaded for both numbers and strings via the `Add` type class.

---

## Constraint Propagation

Constraints propagate through the inference process, just like substitutions:

```typescript
type InferResult = [Subst, Type, readonly Constraint[]];
```

Each inference function returns:

1. A substitution (what we learned about types)
2. The inferred type
3. Accumulated constraints

### Applying Substitutions to Constraints

When we compose substitutions, we must also update constraints:

```typescript
const applySubstConstraint = (subst: Subst, c: Constraint): Constraint => ({
  className: c.className,
  type: applySubst(subst, c.type),
});

const applySubstConstraints = (
  subst: Subst,
  cs: readonly Constraint[]
): Constraint[] =>
  cs.map((c) => applySubstConstraint(subst, c));
```

If we learn that `t0 = number`, any constraint `{ className: "Eq", type: t0 }` becomes `{ className: "Eq", type: number }`.

---

## Constraint Solving

After inference completes, we verify all constraints:

```typescript
const solveConstraints = (
  ctx: CheckContext,
  constraints: readonly Constraint[]
): void => {
  for (const c of constraints) {
    // Type variables: defer until instantiation
    if (c.type.kind === "TVar") {
      continue;
    }

    // Concrete type: check it has the required instance
    if (c.type.kind === "TCon") {
      const classInstances = instances.get(c.className);
      if (!classInstances?.has(c.type.name)) {
        addError(ctx, `Type '${c.type.name}' does not satisfy ${c.className}`);
      }
    }

    // Function types never satisfy our type classes
    if (c.type.kind === "TFun") {
      addError(ctx, `Function types do not satisfy ${c.className}`);
    }
  }
};
```

### The Three Cases

1. **Type variables**: Skip. The constraint remains unsolved—it will be checked when the polymorphic value is instantiated with a concrete type.

2. **Type constructors**: Look up in the instance table. If found, the constraint is satisfied. If not, report an error.

3. **Function types**: Always fail. Functions can't be compared or added.

---

## Examples

### Valid: Comparing Numbers

```
42 == 42
```

1. Infer `42`: type `number`, no constraints
2. Infer `42`: type `number`, no constraints
3. Generate constraint: `{ className: "Eq", type: number }`
4. Solve: `number` is in `Eq`'s instance set. OK!

### Valid: Generic Equality Function

```
let eq x y = x == y
```

1. `x` gets fresh type `t0`
2. `y` gets fresh type `t1`
3. `x == y` unifies `t0` with `t1`, generates `Eq t0`
4. Result type: `t0 -> t0 -> boolean`
5. Constraint `Eq t0` stays unsolved (deferred)

Type: `∀t0. Eq t0 => t0 -> t0 -> boolean`

When used:

```
eq 1 2      -- t0 = number, Eq number satisfied
eq "a" "b"  -- t0 = string, Eq string satisfied
```

### Invalid: Comparing Functions

```
let f = fn x => x
let g = fn x => x
f == g
```

1. `f` has type `t0 -> t0`
2. `g` has type `t1 -> t1`
3. `==` unifies them and generates `Eq (t0 -> t0)`
4. Solve: Function types don't satisfy `Eq`
5. Error: "Function types do not satisfy Eq"

### Invalid: Ordering Booleans

```
true > false
```

1. Both operands have type `boolean`
2. `>` generates `Ord boolean`
3. Solve: `boolean` is not in `Ord`'s instance set
4. Error: "Type 'boolean' does not satisfy Ord"

---

## Constraints in Schemes

When generalizing a type with unsolved constraints, the constraints become part of the scheme:

```typescript
export type Scheme = {
  readonly vars: readonly string[];        // Quantified variables
  readonly constraints: readonly Constraint[];  // Required constraints
  readonly type: Type;
};
```

Example:

```
let eq x y = x == y
```

Scheme for `eq`:

- vars: `["t0"]`
- constraints: `[{ className: "Eq", type: TVar("t0") }]`
- type: `t0 -> t0 -> boolean`

Written mathematically: `∀t0. Eq t0 => t0 -> t0 -> boolean`

---

## Why This Design?

Algow's type class system is deliberately simple:

### What We Have

- Fixed set of type classes (Eq, Ord, Add)
- Fixed instances (primitives only)
- Single-parameter constraints
- Constraint deferral for polymorphic types

### What We Don't Have

- User-defined type classes
- User-defined instances
- Superclass constraints (Ord implies Eq)
- Multi-parameter type classes
- Functional dependencies

This simplicity keeps the implementation straightforward while still enabling:

- Polymorphic comparison (`==` works on numbers and strings)
- Clear error messages when operations aren't supported
- Type-safe operator overloading

---

## Error Messages

The constraint system produces clear errors:

```
-- Comparing functions
(fn x => x) == (fn x => x)
-- Error: Function types do not satisfy Eq

-- Ordering booleans
true < false
-- Error: Type 'boolean' does not satisfy Ord

-- Adding booleans
true + false
-- Error: Type 'boolean' does not satisfy Add
```

---

## Summary

Type classes enable constrained polymorphism:

1. **Constraints** record requirements on types
2. **Instances** define which types satisfy which classes
3. **Generation** happens when inferring operators
4. **Propagation** through substitutions keeps constraints updated
5. **Solving** verifies concrete types after inference
6. **Deferral** for type variables preserves polymorphism

Algow's three type classes (Eq, Ord, Add) are simple but sufficient for safe operator overloading. More sophisticated systems like Haskell's allow user-defined classes and instances, but the core ideas remain the same.

This concludes Part 2: Type System. The next part covers the backend—how we transform our typed AST into executable code.
