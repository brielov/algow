# Chapter 4: Unification

Unification is the heart of type inference. Given two types that should be equal, unification finds a substitution that makes them identical—or reports an error if they can't match.

---

## What Is Unification?

When we see `f x` where:
- `f : t0 -> t1`
- `x : number`

The parameter type of `f` must match the type of `x`. We need to **unify** `t0` with `number`.

Unification finds the substitution that makes two types equal:

```
unify(t0, number) = { t0 = number }
unify(number, number) = {}  (already equal)
unify(number, string) = ERROR  (can't make equal)
```

---

## The Unification Algorithm

```typescript
const unify = (ctx: CheckContext, t1: Type, t2: Type): Subst => {
  // Same type variable - already equal
  if (t1.kind === "TVar" && t2.kind === "TVar" && t1.name === t2.name) {
    return new Map();
  }

  // t1 is a variable - bind it to t2
  if (t1.kind === "TVar") {
    return bindVar(ctx, t1.name, t2);
  }

  // t2 is a variable - bind it to t1
  if (t2.kind === "TVar") {
    return bindVar(ctx, t2.name, t1);
  }

  // Both are type constructors - must have same name
  if (t1.kind === "TCon" && t2.kind === "TCon") {
    if (t1.name === t2.name) {
      return new Map();  // Already equal
    }
    // Report error
    ctx.diagnostics.push(typeMismatch(...));
    return new Map();
  }

  // Both are functions - unify parameter and return types
  if (t1.kind === "TFun" && t2.kind === "TFun") {
    const s1 = unify(ctx, t1.param, t2.param);
    const s2 = unify(ctx, applySubst(s1, t1.ret), applySubst(s1, t2.ret));
    return composeSubst(s1, s2);
  }

  // Both are type applications - unify constructor and argument
  if (t1.kind === "TApp" && t2.kind === "TApp") {
    const s1 = unify(ctx, t1.con, t2.con);
    const s2 = unify(ctx, applySubst(s1, t1.arg), applySubst(s1, t2.arg));
    return composeSubst(s1, s2);
  }

  // Both are tuples - unify element-wise
  if (t1.kind === "TTuple" && t2.kind === "TTuple") {
    if (t1.elements.length !== t2.elements.length) {
      ctx.diagnostics.push(...);  // Arity mismatch
      return new Map();
    }
    let subst: Subst = new Map();
    for (let i = 0; i < t1.elements.length; i++) {
      const s = unify(ctx,
        applySubst(subst, t1.elements[i]),
        applySubst(subst, t2.elements[i])
      );
      subst = composeSubst(subst, s);
    }
    return subst;
  }

  // Both are records - use specialized record unification
  if (t1.kind === "TRecord" && t2.kind === "TRecord") {
    return unifyRecords(ctx, t1, t2);
  }

  // Types are incompatible
  ctx.diagnostics.push(typeMismatch(...));
  return new Map();
};
```

---

## Binding Variables

When we unify a variable with a type, we create a substitution:

```typescript
const bindVar = (ctx: CheckContext, name: string, type: Type): Subst => {
  // Binding to itself is a no-op
  if (type.kind === "TVar" && type.name === name) {
    return new Map();
  }

  // Occurs check: prevent infinite types
  if (freeTypeVars(type).has(name)) {
    addError(ctx, `Infinite type: ${name} appears in ${typeToString(type)}`);
    return new Map();
  }

  return new Map([[name, type]]);
};
```

### The Occurs Check

Consider unifying `t0` with `List t0`. This would mean:
- `t0 = List t0`
- `t0 = List (List t0)`
- `t0 = List (List (List t0))`
- ... forever

This is an infinite type—it never resolves. The **occurs check** prevents this:

```typescript
if (freeTypeVars(type).has(name)) {
  addError(ctx, `Infinite type: ${name} appears in ${typeToString(type)}`);
  return new Map();
}
```

If the variable we're binding appears in the target type, we reject it.

### Finding Free Variables

The occurs check needs to collect all type variables in a type:

```typescript
const freeTypeVars = (type: Type): Set<string> => {
  switch (type.kind) {
    case "TVar":
      return new Set([type.name]);
    case "TCon":
      return new Set();
    case "TFun":
      return freeTypeVars(type.param).union(freeTypeVars(type.ret));
    case "TApp":
      return freeTypeVars(type.con).union(freeTypeVars(type.arg));
    case "TTuple":
      return type.elements.reduce(
        (acc, t) => acc.union(freeTypeVars(t)),
        new Set<string>()
      );
    case "TRecord": {
      let vars = new Set<string>();
      for (const t of type.fields.values()) {
        vars = vars.union(freeTypeVars(t));
      }
      if (type.row) {
        vars = vars.union(freeTypeVars(type.row));
      }
      return vars;
    }
  }
};
```

---

## Unifying Functions

Function unification is recursive:

```typescript
if (t1.kind === "TFun" && t2.kind === "TFun") {
  // First unify parameters
  const s1 = unify(ctx, t1.param, t2.param);

  // Apply s1 before unifying return types
  const s2 = unify(ctx,
    applySubst(s1, t1.ret),
    applySubst(s1, t2.ret)
  );

  // Combine substitutions
  return composeSubst(s1, s2);
}
```

### Example

Unify `t0 -> t1` with `number -> string`:

1. Unify `t0` with `number`: `s1 = { t0 = number }`
2. Apply `s1` to return types (no change here)
3. Unify `t1` with `string`: `s2 = { t1 = string }`
4. Compose: `{ t0 = number, t1 = string }`

### Why Apply Between Steps?

Consider unifying `t0 -> t0` with `number -> t1`:

1. Unify `t0` with `number`: `s1 = { t0 = number }`
2. **Apply s1** to return types: `t0` becomes `number`
3. Unify `number` with `t1`: `s2 = { t1 = number }`
4. Result: `{ t0 = number, t1 = number }`

If we didn't apply `s1`, we'd miss that `t0` and `t1` must both be `number`.

---

## Unifying Type Applications

Similar to functions:

```typescript
if (t1.kind === "TApp" && t2.kind === "TApp") {
  const s1 = unify(ctx, t1.con, t2.con);
  const s2 = unify(ctx,
    applySubst(s1, t1.arg),
    applySubst(s1, t2.arg)
  );
  return composeSubst(s1, s2);
}
```

### Example

Unify `List t0` with `List number`:

1. Unify `List` with `List`: equal, `s1 = {}`
2. Unify `t0` with `number`: `s2 = { t0 = number }`
3. Result: `{ t0 = number }`

Unify `List number` with `Maybe number`:

1. Unify `List` with `Maybe`: ERROR (different type constructors)

---

## Unifying Tuples

Tuples must have the same arity:

```typescript
if (t1.kind === "TTuple" && t2.kind === "TTuple") {
  if (t1.elements.length !== t2.elements.length) {
    addError(ctx, `Tuple arity mismatch: ${t1.elements.length} vs ${t2.elements.length}`);
    return new Map();
  }

  let subst: Subst = new Map();
  for (let i = 0; i < t1.elements.length; i++) {
    const s = unify(ctx,
      applySubst(subst, t1.elements[i]),
      applySubst(subst, t2.elements[i])
    );
    subst = composeSubst(subst, s);
  }
  return subst;
}
```

### Example

Unify `(t0, t1)` with `(number, string)`:

1. Unify `t0` with `number`: `s = { t0 = number }`
2. Unify `t1` with `string`: `s = { t0 = number, t1 = string }`

Unify `(number, string)` with `(number, string, boolean)`:

- ERROR: arity mismatch (2 vs 3)

---

## Unifying Records (Row Polymorphism)

Record unification is the most complex case because of **row polymorphism**. Records can be **open** (may have more fields) or **closed** (exact fields).

### The Full Implementation

```typescript
const unifyRecords = (ctx: CheckContext, t1: TRecord, t2: TRecord): Subst => {
  const fields1 = new Set(t1.fields.keys());
  const fields2 = new Set(t2.fields.keys());

  // Categorize fields into three groups
  const commonFields = fields1.intersection(fields2);  // Both have
  const onlyIn1 = fields1.difference(fields2);          // Only t1 has
  const onlyIn2 = fields2.difference(fields1);          // Only t2 has

  // Step 1: Unify the types of common fields
  let currentSubst: Subst = new Map();
  for (const fieldName of commonFields) {
    const fieldType1 = applySubst(currentSubst, t1.fields.get(fieldName)!);
    const fieldType2 = applySubst(currentSubst, t2.fields.get(fieldName)!);
    const s = unify(ctx, fieldType1, fieldType2);
    currentSubst = composeSubst(currentSubst, s);
  }

  // Get row variables after applying current substitutions
  const row1 = t1.row ? applySubst(currentSubst, t1.row) : null;
  const row2 = t2.row ? applySubst(currentSubst, t2.row) : null;

  // Prepare extra fields with substitutions applied
  const extraFields1: [string, Type][] = [...onlyIn1].map((f) => [
    f,
    applySubst(currentSubst, t1.fields.get(f)!),
  ]);
  const extraFields2: [string, Type][] = [...onlyIn2].map((f) => [
    f,
    applySubst(currentSubst, t2.fields.get(f)!),
  ]);

  // Case 1: No extra fields on either side
  if (onlyIn1.size === 0 && onlyIn2.size === 0) {
    if (row1 && row2) {
      // Both open - unify row variables
      const s = unify(ctx, row1, row2);
      return composeSubst(currentSubst, s);
    }
    if (row1 && !row2) {
      // t1 open, t2 closed - t1's row must be empty
      const s = unify(ctx, row1, trecord([]));
      return composeSubst(currentSubst, s);
    }
    if (!row1 && row2) {
      // t1 closed, t2 open - t2's row must be empty
      const s = unify(ctx, row2, trecord([]));
      return composeSubst(currentSubst, s);
    }
    // Both closed with matching fields - already done
    return currentSubst;
  }

  // Case 2: t1 has extra fields, t2 doesn't
  if (onlyIn1.size > 0 && onlyIn2.size === 0) {
    if (!row2) {
      addError(ctx, `Record field mismatch: missing fields { ${[...onlyIn1].join(", ")} }`);
      return currentSubst;
    }
    // t2's row must equal t1's extra fields (plus t1's row if any)
    const extraRecord = trecord(extraFields1, row1);
    const s = unify(ctx, row2, extraRecord);
    return composeSubst(currentSubst, s);
  }

  // Case 3: t2 has extra fields, t1 doesn't
  if (onlyIn2.size > 0 && onlyIn1.size === 0) {
    if (!row1) {
      addError(ctx, `Record field mismatch: missing fields { ${[...onlyIn2].join(", ")} }`);
      return currentSubst;
    }
    // t1's row must equal t2's extra fields (plus t2's row if any)
    const extraRecord = trecord(extraFields2, row2);
    const s = unify(ctx, row1, extraRecord);
    return composeSubst(currentSubst, s);
  }

  // Case 4: Both have unique extra fields - both must be open
  if (!row1) {
    addError(ctx, `Record field mismatch: missing fields { ${[...onlyIn2].join(", ")} }`);
    return currentSubst;
  }
  if (!row2) {
    addError(ctx, `Record field mismatch: missing fields { ${[...onlyIn1].join(", ")} }`);
    return currentSubst;
  }

  // Both records contribute unique fields - create a shared tail
  const freshRow = freshTypeVar();
  // row1 = { t2's extra fields | freshRow }
  const s1 = unify(ctx, row1, trecord(extraFields2, freshRow));
  currentSubst = composeSubst(currentSubst, s1);
  // row2 = { t1's extra fields | freshRow }
  const s2 = unify(ctx, applySubst(currentSubst, row2), trecord(extraFields1, freshRow));
  return composeSubst(currentSubst, s2);
};
```

### Case Analysis

Let's walk through each case with examples:

#### Case 1: Same Fields (No Extras)

```
{ x: t0, y: t1 }  ↔  { x: number, y: string }
```

Both records have exactly fields `x` and `y`. Just unify the field types:
- `t0 = number`
- `t1 = string`

With row variables:

```
{ x: number | ρ1 }  ↔  { x: number | ρ2 }
```

After unifying `x` types, unify `ρ1` with `ρ2`.

#### Case 2: One Record Has Extra Fields

```
{ x: number, y: string }  ↔  { x: number | ρ }
```

t1 has `y`, t2 doesn't. Since t2 is open (has row `ρ`), its row absorbs the extra field:
- `ρ = { y: string }`

If t2 were closed, this would be an error.

#### Case 3: Other Record Has Extra Fields

Symmetric to Case 2.

#### Case 4: Both Have Unique Extra Fields

```
{ x: number | ρ1 }  ↔  { y: string | ρ2 }
```

t1 has `x`, t2 has `y`. Both must be open for this to work:
- Create fresh row variable `ρ3`
- `ρ1 = { y: string | ρ3 }`
- `ρ2 = { x: number | ρ3 }`

This means the unified record has both `x` and `y`, plus whatever `ρ3` adds.

### Why Row Polymorphism Matters

Consider this function:

```
let getX r = r.x
```

What type should `getX` have?

Without row polymorphism, we might require an exact record:
- `getX : { x: a } -> a`

But that's too restrictive—we can't pass `{ x = 1, y = 2 }`.

With row polymorphism:
- `getX : { x: a | ρ } -> a`

Now any record with at least field `x` works:
- `getX { x = 1 }` ✓
- `getX { x = 1, y = 2 }` ✓
- `getX { x = "hi", z = true }` ✓

---

## Error Reporting

When unification fails, we report clear errors:

```typescript
// Type mismatch
ctx.diagnostics.push({
  start: span.start,
  end: span.end,
  message: `Type mismatch: expected '${typeToString(t1)}', found '${typeToString(t2)}'`,
  severity: "error",
  expected: typeToString(t1),
  actual: typeToString(t2),
});
```

Good error messages include:
- What types were expected vs. found
- Where in the source code the error occurred
- Why the types don't match (when possible)

---

## Unification in Action

Let's trace through a real example:

```
let f = fn x => x + 1 in f "hello"
```

### Step 1: Infer `fn x => x + 1`

1. `x : t0` (fresh variable)
2. Infer `x + 1`:
   - `x : t0`
   - `1 : number`
   - `+` requires `Add` constraint on operand type
   - Unify `t0` with `number`: `{ t0 = number }`
3. Result: `fn x => x + 1 : number -> number`

### Step 2: Infer `f "hello"`

1. `f : number -> number`
2. `"hello" : string`
3. Application requires: unify `number -> number` with `string -> t1`
4. Unify `number` with `string`:
   - **ERROR**: `Type mismatch: expected 'number', found 'string'`

The error points to the application, explaining that `f` expects a number but got a string.

---

## Unification Properties

The unification algorithm has important properties:

### 1. Most General Unifier

When unification succeeds, it returns the **most general** substitution. Given:

```
t0 -> t1  ↔  a -> a
```

The result is `{ t0 = a, t1 = a }`, not `{ t0 = number, t1 = number }`. We don't over-constrain.

### 2. Termination

The occurs check ensures we never create infinite types, guaranteeing termination.

### 3. Symmetry

`unify(t1, t2)` produces the same logical result as `unify(t2, t1)` (the substitutions may differ in representation but are equivalent).

---

## Common Unification Errors

### Infinite Type

```
let rec f = f in f
```

When inferring `f`, we try to unify `t0` (f's type) with itself applied. This creates `t0 = t0 -> t1`, which fails the occurs check.

### Arity Mismatch

```
let f (x, y) = x + y in f (1, 2, 3)
```

The function expects a 2-tuple but receives a 3-tuple.

### Missing Field

```
let f r = r.x in f { y = 1 }
```

The function requires field `x`, but the argument only has `y`.

### Type Constructor Mismatch

```
let f (Just x) = x in f (Left 1)
```

Pattern expects `Maybe`, but argument is `Either`.

---

## Summary

Unification finds substitutions that make types equal:

1. **Variables** get bound to other types (with occurs check)
2. **Constructors** must match exactly
3. **Compound types** unify recursively (functions, tuples, applications)
4. **Records** use row polymorphism for flexible field matching
5. **Errors** are reported when types can't match

The key insight is that unification propagates information—when we learn that `t0 = number`, we immediately apply that knowledge to all subsequent unifications. This cascading effect is what makes type inference work.

The next chapter shows how unification fits into the complete inference algorithm.
