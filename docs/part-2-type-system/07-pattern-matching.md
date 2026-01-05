# Chapter 7: Pattern Matching Types

Pattern matching combines type checking with destructuring. The type checker must verify that patterns are compatible with the matched value and determine types for bound variables.

---

## The Two Jobs

When type checking a match expression, we:

1. **Check patterns** against the scrutinee type
2. **Bind variables** to appropriate types
3. **Check bodies** with bindings in scope
4. **Verify exhaustiveness** (all cases covered)

---

## Inferring Pattern Types

Each pattern kind has its own typing rules:

### Variable Patterns

A variable pattern matches anything and binds the whole value:

```typescript
const inferPattern = (
  ctx: CheckContext,
  pattern: Pattern,
  expectedType: Type
): PatternBindings => {
  if (pattern.kind === "PVar") {
    // Bind variable to expected type
    return new Map([[pattern.name, expectedType]]);
  }
  // ...
};
```

Example:
```
match x with
| y => y + 1
end
```
- `x : number`
- Pattern `y` binds to `number`
- Body `y + 1` type checks with `y : number`

### Wildcard Patterns

Wildcards match anything but don't bind:

```typescript
if (pattern.kind === "PWildcard") {
  return new Map();  // No bindings
}
```

### Literal Patterns

Literals must match the expected type:

```typescript
if (pattern.kind === "PLit") {
  const litType = typeof pattern.value === "number" ? tNum
    : typeof pattern.value === "string" ? tStr
    : tBool;

  unify(ctx, expectedType, litType, pattern.span);
  return new Map();  // No bindings
}
```

Example:
```
match x with
| 0 => "zero"
| _ => "non-zero"
end
```
- `x` must be `number` (because `0` is a number)

### Constructor Patterns

Constructors provide the most interesting case:

```typescript
if (pattern.kind === "PCon") {
  // Look up constructor scheme
  const scheme = env.get(pattern.name);
  const conType = instantiate(scheme);

  // Extract argument types from constructor type
  // Cons : a -> List a -> List a
  // Arguments: [a, List a]
  // Result: List a

  // Unify result with expected type
  unify(ctx, resultType, expectedType);

  // Recursively check argument patterns
  const bindings = new Map();
  for (let i = 0; i < pattern.args.length; i++) {
    const argBindings = inferPattern(ctx, pattern.args[i], argTypes[i]);
    for (const [name, type] of argBindings) {
      bindings.set(name, type);
    }
  }

  return bindings;
}
```

Example:
```
match list with
| Cons x rest => ...
end
```
- `Cons : ∀a. a -> List a -> List a`
- Instantiate: `t0 -> List t0 -> List t0`
- Unify `List t0` with `list`'s type
- Bind `x : t0`, `rest : List t0`

### Tuple Patterns

Tuples destructure by position:

```typescript
if (pattern.kind === "PTuple") {
  // Expected type should be a tuple
  if (expectedType.kind !== "TTuple") {
    // Create fresh tuple type and unify
    const elemTypes = pattern.elements.map(() => freshTypeVar());
    unify(ctx, expectedType, ttuple(elemTypes));
  }

  // Check each element pattern
  const bindings = new Map();
  for (let i = 0; i < pattern.elements.length; i++) {
    const elemBindings = inferPattern(ctx, pattern.elements[i], elemTypes[i]);
    // ... merge bindings
  }
  return bindings;
}
```

### Record Patterns

Records destructure by field name:

```typescript
if (pattern.kind === "PRecord") {
  // Build expected record type from pattern fields
  const fieldTypes = new Map();
  for (const field of pattern.fields) {
    fieldTypes.set(field.name, freshTypeVar());
  }

  // Create open record type (row polymorphism)
  const recordType = trecord([...fieldTypes.entries()], freshTypeVar());
  unify(ctx, expectedType, recordType);

  // Check each field pattern
  const bindings = new Map();
  for (const field of pattern.fields) {
    const fieldBindings = inferPattern(ctx, field.pattern, fieldTypes.get(field.name));
    // ... merge bindings
  }
  return bindings;
}
```

### As-Patterns

As-patterns bind both the whole and parts:

```typescript
if (pattern.kind === "PAs") {
  // Bind the as-name to whole value
  const bindings = new Map([[pattern.name, expectedType]]);

  // Also check inner pattern
  const innerBindings = inferPattern(ctx, pattern.pattern, expectedType);
  for (const [name, type] of innerBindings) {
    bindings.set(name, type);
  }

  return bindings;
}
```

Example:
```
match list with
| Cons x rest as whole => ...
end
```
- `x`, `rest` bound from constructor pattern
- `whole` bound to entire list

### Or-Patterns

All alternatives must bind the same variables with compatible types:

```typescript
if (pattern.kind === "POr") {
  // Check first alternative
  const firstBindings = inferPattern(ctx, pattern.alternatives[0], expectedType);

  // Check remaining alternatives bind same variables
  for (let i = 1; i < pattern.alternatives.length; i++) {
    const altBindings = inferPattern(ctx, pattern.alternatives[i], expectedType);

    // Verify same variables
    for (const [name, type] of firstBindings) {
      const altType = altBindings.get(name);
      if (!altType) {
        addError(ctx, `Variable '${name}' not bound in all alternatives`);
      } else {
        // Unify types across alternatives
        unify(ctx, type, altType);
      }
    }
  }

  return firstBindings;
}
```

---

## Exhaustiveness Checking

The type checker verifies all cases are covered:

```
data Maybe a = Nothing | Just a

match m with
| Nothing => ...
end
-- Warning: non-exhaustive patterns, missing: Just _
```

### The Algorithm

1. **Collect** all constructors for the scrutinee type
2. **Track** which constructors are covered
3. **Report** missing patterns

```typescript
const checkExhaustiveness = (
  registry: ConstructorRegistry,
  scrutineeType: Type,
  cases: Case[]
): string[] => {
  // Get type name from scrutinee
  const typeName = getTypeName(scrutineeType);
  if (!typeName) return [];  // Can't check non-ADT types

  // Get all constructors for this type
  const allConstructors = registry.get(typeName);
  if (!allConstructors) return [];

  // Collect covered constructors
  const covered = new Set<string>();
  for (const case_ of cases) {
    collectCoveredConstructors(case_.pattern, covered);
  }

  // Check for wildcard/variable (covers everything)
  if (hasWildcard(cases)) {
    return [];  // Exhaustive
  }

  // Find missing constructors
  return allConstructors.filter(c => !covered.has(c));
};
```

### Nested Patterns

Exhaustiveness is recursive:

```
match list with
| Nil => ...
| Cons x Nil => ...
end
-- Missing: Cons x (Cons _ _)
```

The algorithm must consider nested constructor patterns.

### Guards Make It Harder

Guards complicate exhaustiveness:

```
match n with
| x if x > 0 => "positive"
| x if x < 0 => "negative"
| _ => "zero"
end
```

The last case is needed—guards might not cover all values.

---

## Type Inference for Match

The complete algorithm:

```typescript
const inferMatch = (
  ctx: CheckContext,
  env: TypeEnv,
  expr: Match
): [Subst, Type] => {
  // Infer scrutinee type
  const [s1, scrutineeType] = infer(ctx, env, expr.expr);

  // Result type (fresh variable, unified across all cases)
  let resultType = freshTypeVar();
  let currentSubst = s1;

  for (const case_ of expr.cases) {
    // Check pattern against scrutinee type
    const patternBindings = inferPattern(
      ctx,
      case_.pattern,
      applySubst(currentSubst, scrutineeType)
    );

    // Extend environment with pattern bindings
    const caseEnv = new Map(applySubstEnv(currentSubst, env));
    for (const [name, type] of patternBindings) {
      caseEnv.set(name, scheme([], type));
    }

    // Check guard if present
    if (case_.guard) {
      const [gs, guardType] = infer(ctx, caseEnv, case_.guard);
      const guardUnify = unify(ctx, guardType, tBool);
      currentSubst = composeSubst(currentSubst, composeSubst(gs, guardUnify));
    }

    // Infer body type
    const [s2, bodyType] = infer(ctx, caseEnv, case_.body);

    // Unify with result type
    const s3 = unify(ctx, applySubst(s2, resultType), bodyType);

    currentSubst = composeSubst(currentSubst, composeSubst(s2, s3));
    resultType = applySubst(s3, resultType);
  }

  // Check exhaustiveness
  const missing = checkExhaustiveness(registry, scrutineeType, expr.cases);
  if (missing.length > 0) {
    addWarning(ctx, `Non-exhaustive patterns, missing: ${missing.join(", ")}`);
  }

  return [currentSubst, resultType];
};
```

---

## Examples

### Simple Constructor Match

```
match maybe with
| Nothing => 0
| Just x => x
end
```

- `maybe : Maybe t0`
- `Nothing` matches, returns `number`
- `Just x` matches, `x : t0`, returns `t0`
- Unify `number` with `t0`: `t0 = number`
- Result type: `number`

### Nested Pattern

```
match pair with
| (x, (y, z)) => x + y + z
end
```

- `pair : (t0, (t1, t2))`
- Pattern binds: `x : t0`, `y : t1`, `z : t2`
- `+` requires numbers: `t0 = t1 = t2 = number`
- Result: `number`

### Polymorphic Match

```
let head list = match list with
  | Nil => Nothing
  | Cons x _ => Just x
end
```

- `list : List t0`
- `Nothing : Maybe t1`
- `Just x : Maybe t0`
- Unify: `t1 = t0`
- Result: `Maybe t0`
- Generalize: `∀a. List a -> Maybe a`

---

## Summary

Pattern matching type checking:

1. **Pattern inference** determines types for bound variables
2. **Constructor patterns** use constructor type schemes
3. **Nested patterns** check recursively
4. **Or-patterns** require consistent bindings
5. **Exhaustiveness** verifies all cases covered
6. **All case bodies** must have the same type

The next chapter covers type classes for operator overloading.
