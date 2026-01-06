# Chapter 3: Pattern Matching at Runtime

Pattern matching inspects values and binds variables to their parts. At runtime, we try each pattern until one matches, then evaluate the corresponding body.

---

## The Match Result

Pattern matching either succeeds with bindings or fails:

```typescript
type MatchResult =
  | { matched: true; bindings: Map<string, Value> }
  | { matched: false };
```

A successful match produces a map from variable names to values—these become available in the case body.

---

## Evaluating Match Expressions

```typescript
const evalMatch = (env: Env, expr: ast.Match): Value => {
  const scrutinee = evaluate(env, expr.expr);

  for (const case_ of expr.cases) {
    const result = matchPattern(case_.pattern, scrutinee);
    if (result.matched) {
      // Extend environment with pattern bindings
      let caseEnv = env;
      for (const [name, value] of result.bindings) {
        caseEnv = extendEnv(caseEnv, name, value);
      }

      // Check guard if present
      if (case_.guard) {
        const guardResult = evaluate(caseEnv, case_.guard) as VBool;
        if (!guardResult.value) {
          continue;  // Guard failed, try next case
        }
      }

      return evaluate(caseEnv, case_.body);
    }
  }

  // Exhaustiveness checking guarantees this is unreachable
  throw new Error("Unreachable: exhaustiveness check failed");
};
```

The algorithm:

1. Evaluate the scrutinee (the value being matched)
2. Try each pattern in order
3. If a pattern matches, extend environment with bindings
4. If there's a guard, check it
5. If guard passes (or no guard), evaluate the body
6. If guard fails, try next case

---

## Pattern Matching Algorithm

The `matchPattern` function recursively matches patterns against values:

```typescript
const matchPattern = (pattern: ast.Pattern, value: Value): MatchResult => {
  switch (pattern.kind) {
    case "PWildcard":
      return { matched: true, bindings: new Map() };

    case "PVar":
      return { matched: true, bindings: new Map([[pattern.name, value]]) };

    case "PLit":
      return matchLiteral(pattern, value);

    case "PCon":
      return matchConstructor(pattern, value);

    case "PTuple":
      return matchTuple(pattern, value);

    case "PRecord":
      return matchRecord(pattern, value);

    case "PAs":
      return matchAs(pattern, value);

    case "POr":
      return matchOr(pattern, value);
  }
};
```

Let's examine each pattern kind.

---

## Wildcard Patterns

The wildcard `_` matches anything without binding:

```typescript
case "PWildcard":
  return { matched: true, bindings: new Map() };
```

Example:

```
match x with
| _ => "matched anything"
end
```

---

## Variable Patterns

A variable matches anything and binds the value:

```typescript
case "PVar":
  return { matched: true, bindings: new Map([[pattern.name, value]]) };
```

Example:

```
match x with
| y => y + 1
end
```

`y` binds to whatever `x` is.

---

## Literal Patterns

Literals match equal values:

```typescript
const matchLiteral = (pattern: ast.PLit, value: Value): MatchResult => {
  if (typeof pattern.value === "number" && value.kind === "VNum") {
    return pattern.value === value.value
      ? { matched: true, bindings: new Map() }
      : { matched: false };
  }
  if (typeof pattern.value === "string" && value.kind === "VStr") {
    return pattern.value === value.value
      ? { matched: true, bindings: new Map() }
      : { matched: false };
  }
  if (typeof pattern.value === "boolean" && value.kind === "VBool") {
    return pattern.value === value.value
      ? { matched: true, bindings: new Map() }
      : { matched: false };
  }
  return { matched: false };
};
```

Example:

```
match n with
| 0 => "zero"
| 1 => "one"
| _ => "many"
end
```

---

## Constructor Patterns

Constructor patterns check the tag and match arguments recursively:

```typescript
const matchConstructor = (pattern: ast.PCon, value: Value): MatchResult => {
  // Must be a constructor with matching name
  if (value.kind !== "VCon" || value.name !== pattern.name) {
    return { matched: false };
  }

  // Arity must match
  if (value.args.length !== pattern.args.length) {
    return { matched: false };
  }

  // Match each argument pattern
  const bindings = new Map<string, Value>();
  for (let i = 0; i < pattern.args.length; i++) {
    const result = matchPattern(pattern.args[i]!, value.args[i]!);
    if (!result.matched) return { matched: false };
    for (const [name, val] of result.bindings) {
      bindings.set(name, val);
    }
  }

  return { matched: true, bindings };
};
```

Example:

```
match list with
| Nil => 0
| Cons x rest => 1 + length rest
end
```

For `Cons 42 Nil`:

1. Check tag: `"Cons" === "Cons"` ✓
2. Check arity: 2 === 2 ✓
3. Match `x` against `42` → binds `x = 42`
4. Match `rest` against `Nil` → binds `rest = Nil`
5. Return bindings: `{ x → 42, rest → Nil }`

---

## Tuple Patterns

Tuple patterns destructure by position:

```typescript
const matchTuple = (pattern: ast.PTuple, value: Value): MatchResult => {
  if (value.kind !== "VTuple") {
    return { matched: false };
  }
  if (value.elements.length !== pattern.elements.length) {
    return { matched: false };
  }

  const bindings = new Map<string, Value>();
  for (let i = 0; i < pattern.elements.length; i++) {
    const result = matchPattern(pattern.elements[i]!, value.elements[i]!);
    if (!result.matched) return { matched: false };
    for (const [name, val] of result.bindings) {
      bindings.set(name, val);
    }
  }

  return { matched: true, bindings };
};
```

Example:

```
match pair with
| (x, y) => x + y
end
```

---

## Record Patterns

Record patterns match by field name:

```typescript
const matchRecord = (pattern: ast.PRecord, value: Value): MatchResult => {
  if (value.kind !== "VRecord") {
    return { matched: false };
  }

  const bindings = new Map<string, Value>();
  for (const field of pattern.fields) {
    const fieldValue = value.fields.get(field.name);
    if (fieldValue === undefined) return { matched: false };

    const result = matchPattern(field.pattern, fieldValue);
    if (!result.matched) return { matched: false };
    for (const [name, val] of result.bindings) {
      bindings.set(name, val);
    }
  }

  return { matched: true, bindings };
};
```

Example:

```
match point with
| { x = a, y = b } => a + b
end
```

Note: The pattern only needs to match some fields—the record can have more.

---

## As-Patterns

As-patterns bind the whole value while also destructuring:

```typescript
const matchAs = (pattern: ast.PAs, value: Value): MatchResult => {
  // Match inner pattern
  const result = matchPattern(pattern.pattern, value);
  if (!result.matched) return { matched: false };

  // Add as-binding
  result.bindings.set(pattern.name, value);
  return result;
};
```

Example:

```
match list with
| Cons x rest as whole => (whole, x, rest)
end
```

`whole` binds to the entire list, `x` and `rest` bind to its parts.

---

## Or-Patterns

Or-patterns try each alternative:

```typescript
const matchOr = (pattern: ast.POr, value: Value): MatchResult => {
  for (const alt of pattern.alternatives) {
    const result = matchPattern(alt, value);
    if (result.matched) return result;
  }
  return { matched: false };
};
```

Example:

```
match maybe with
| Nothing | Just Nothing => "nothing"
| Just (Just x) => x
end
```

The type checker ensures all alternatives bind the same variables.

---

## Guards

Guards add extra conditions to patterns:

```
match n with
| x if x > 0 => "positive"
| x if x < 0 => "negative"
| _ => "zero"
end
```

In `evalMatch`:

```typescript
if (case_.guard) {
  const guardResult = evaluate(caseEnv, case_.guard) as VBool;
  if (!guardResult.value) {
    continue;  // Guard failed, try next case
  }
}
```

The pattern bindings are in scope for the guard expression.

---

## Nested Patterns

Patterns can nest arbitrarily:

```
match list with
| Cons (x, y) (Cons (a, b) rest) => x + y + a + b
| _ => 0
end
```

The matching algorithm recursively descends through the pattern structure.

---

## Example: Complete Match

```
data Tree a = Leaf a | Node (Tree a) (Tree a)

let sum tree = match tree with
  | Leaf n => n
  | Node left right => sum left + sum right
end
```

For `Node (Leaf 1) (Node (Leaf 2) (Leaf 3))`:

1. Match against `Leaf n`: fails (tag is `Node`, not `Leaf`)
2. Match against `Node left right`:
   - Check tag: `"Node" === "Node"` ✓
   - Match `left` against `Leaf 1` → binds `left = Leaf 1`
   - Match `right` against `Node (Leaf 2) (Leaf 3)` → binds `right = Node (Leaf 2) (Leaf 3)`
3. Evaluate body: `sum left + sum right`
   - Recursively evaluate `sum (Leaf 1)` → 1
   - Recursively evaluate `sum (Node (Leaf 2) (Leaf 3))` → 5
   - Result: 6

---

## Performance Considerations

### Sequential Matching

The interpreter tries patterns sequentially—first pattern first. This is simple but not optimal. Production compilers often compile patterns to decision trees for better performance.

### Exhaustiveness

The type checker verifies exhaustiveness, so the interpreter trusts that some pattern will match. If no pattern matches (which shouldn't happen), we throw an error.

### Binding Collection

We build up bindings in a Map as we match. This is straightforward but creates temporary data structures. Optimized implementations might avoid this overhead.

---

## Summary

Pattern matching at runtime:

1. **Evaluate scrutinee** to get the value to match
2. **Try each pattern** in order
3. **Recursively match** subpatterns against subvalues
4. **Collect bindings** for variable patterns
5. **Check guards** if present
6. **Evaluate body** with bindings in scope

The interpreter's pattern matching mirrors the type checker's—both traverse patterns recursively, ensuring consistency between type checking and execution.

This concludes Part 4: Interpreter. The next part shows how all the pieces fit together in the complete compilation and execution pipeline.
