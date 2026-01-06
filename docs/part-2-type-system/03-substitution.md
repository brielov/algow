# Chapter 3: Substitution

During type inference, we discover relationships between types. A **substitution** records these discoveries: "type variable `t0` should be `number`", "`t1` should be `t0`", etc.

---

## What Is a Substitution?

A substitution is a mapping from type variable names to types:

```typescript
type Subst = Map<string, Type>;
```

Example substitutions:

```
{ t0 = number }           -- t0 is number
{ t0 = number, t1 = string }  -- t0 is number, t1 is string
{ t0 = t1 }               -- t0 and t1 are the same
```

When we apply a substitution to a type, we replace type variables with their mapped values.

---

## Applying Substitutions

The core operation: replace type variables with their values.

```typescript
const applySubst = (subst: Subst, type: Type): Type => {
  switch (type.kind) {
    // Type constructors have no variables to substitute
    case "TCon":
      return type;

    // Type variable: look up in substitution
    case "TVar":
      return subst.get(type.name) ?? type;  // Return as-is if not found

    // Function type: substitute in both parts
    case "TFun":
      return tfun(
        applySubst(subst, type.param),
        applySubst(subst, type.ret)
      );

    // Type application: substitute in both parts
    case "TApp":
      return tapp(
        applySubst(subst, type.con),
        applySubst(subst, type.arg)
      );

    // Record: substitute in all field types and row
    case "TRecord": {
      const newFields = new Map<string, Type>();
      for (const [name, fieldType] of type.fields) {
        newFields.set(name, applySubst(subst, fieldType));
      }
      const newRow = type.row ? applySubst(subst, type.row) : null;
      return trecord([...newFields.entries()], newRow);
    }

    // Tuple: substitute in all elements
    case "TTuple":
      return ttuple(type.elements.map(t => applySubst(subst, t)));
  }
};
```

### Example

Given substitution `{ t0 = number, t1 = string }`:

```
applySubst to (t0 -> t1)
  = number -> string

applySubst to (List t0)
  = List number

applySubst to { x: t0, y: t1 }
  = { x: number, y: string }
```

The substitution is applied recursively through the entire type tree.

---

## Why Substitutions?

During inference, we learn type constraints incrementally:

1. We see `let x = 1 in ...`
   - Learn: `x : number`

2. We see `fn f => f x`
   - `f` has unknown type `t0`
   - Application `f x` means `f : t1 -> t2` for some `t1`, `t2`
   - `x : number`, so `t1 = number`
   - Learn: `{ t1 = number }`

3. We continue and learn more...
   - Maybe `t2 = string`
   - Add to substitution: `{ t1 = number, t2 = string }`

Each step adds to our knowledge. We apply the accumulated substitution to propagate this knowledge everywhere.

---

## Composing Substitutions

Often we learn multiple facts in sequence. We need to combine substitutions.

Given two substitutions:

- `s1 = { t0 = number }`
- `s2 = { t1 = t0 }`

We want a combined substitution that captures both facts. The key insight: `s2` refers to `t0`, which `s1` tells us is `number`. So the combined substitution should give `t1 = number`.

```typescript
const composeSubst = (s1: Subst, s2: Subst): Subst => {
  const result: Subst = new Map();

  // Apply s2 to all types in s1's mappings
  for (const [name, type] of s1) {
    result.set(name, applySubst(s2, type));
  }

  // Add s2's mappings that aren't already in result
  for (const [name, type] of s2) {
    if (!result.has(name)) {
      result.set(name, type);
    }
  }

  return result;
};
```

### Example

```
s1 = { t0 = t1 -> boolean }
s2 = { t1 = number }

composeSubst(s1, s2):
  1. Apply s2 to s1's values:
     t0 = applySubst(s2, t1 -> boolean)
        = number -> boolean
  2. Add s2's mappings not in result:
     t1 = number

Result: { t0 = number -> boolean, t1 = number }
```

Now if we have type `t0`, applying this substitution gives `number -> boolean`. The transitive knowledge is captured.

### Order Matters

Composition isn't commutative. The second substitution is applied to the values of the first:

```
composeSubst({a = b}, {b = number})
  = {a = number, b = number}  -- b in 'a = b' becomes number

composeSubst({b = number}, {a = b})
  = {b = number, a = b}  -- No change to 'a = b'
```

In the first case, we learn `a = number`. In the second, we don't. The order depends on which facts we learned first.

---

## Applying Substitutions to Schemes

Type schemes have quantified variables. We must NOT substitute those:

```typescript
const applySubstScheme = (subst: Subst, scheme: Scheme): Scheme => {
  // Filter out the quantified variables from the substitution
  const filtered: Subst = new Map();
  for (const [name, type] of subst) {
    if (!scheme.vars.includes(name)) {
      filtered.set(name, type);
    }
  }
  return {
    vars: scheme.vars,
    constraints: scheme.constraints,
    type: applySubst(filtered, scheme.type)
  };
};
```

### Why?

Consider scheme `∀a. a -> t0` (a polymorphic function where the return type is unknown).

If our substitution is `{ a = number, t0 = string }`:

- We should NOT replace `a`—it's bound by the `∀`
- We SHOULD replace `t0`—it's a free variable

Result: `∀a. a -> string`

If we replaced `a`, we'd lose the polymorphism:

- Wrong: `∀a. number -> string` (makes no sense—`a` isn't used)

---

## Applying to Environments

Type environments map names to schemes. We apply substitutions to the entire environment:

```typescript
const applySubstEnv = (subst: Subst, env: TypeEnv): TypeEnv => {
  const result: TypeEnv = new Map();
  for (const [name, scheme] of env) {
    result.set(name, applySubstScheme(subst, scheme));
  }
  return result;
};
```

This propagates knowledge throughout the context. If we learn `t0 = number`, all schemes in the environment that mention `t0` get updated.

---

## The Empty Substitution

The identity for composition:

```typescript
const emptySubst: Subst = new Map();
```

Properties:

- `applySubst(emptySubst, type) = type`
- `composeSubst(emptySubst, s) = s`
- `composeSubst(s, emptySubst) = s`

We return the empty substitution when:

- Two types are already equal (no new information)
- An error occurred (we can't make types equal)

---

## Substitution in Practice

Here's how substitutions flow through inference:

```typescript
const infer = (env: TypeEnv, expr: Expr): [Subst, Type] => {
  // ... inference logic

  // Example: inferring function application
  if (expr.kind === "App") {
    // Infer function type
    const [s1, funcType] = infer(env, expr.func);

    // Apply s1 to environment before inferring argument
    const [s2, argType] = infer(applySubstEnv(s1, env), expr.param);

    // Create fresh type variable for result
    const resultType = freshTypeVar();

    // Apply s2 to function type, then unify with argType -> resultType
    const s3 = unify(
      applySubst(s2, funcType),
      tfun(argType, resultType)
    );

    // Compose all substitutions
    const finalSubst = composeSubst(composeSubst(s1, s2), s3);

    // Apply to result type
    return [finalSubst, applySubst(s3, resultType)];
  }
};
```

The pattern:

1. Infer sub-expressions, getting substitutions
2. Apply earlier substitutions before inferring later expressions
3. Compose substitutions as we go
4. Apply final substitution to the result type

---

## Summary

Substitutions are the mechanism for propagating type information:

1. **Creation**: Unification creates substitutions
2. **Application**: Replace type variables with their values
3. **Composition**: Combine substitutions while maintaining transitivity
4. **Environment updates**: Propagate knowledge through the context

The next chapter covers unification—how we create substitutions by making two types equal.
