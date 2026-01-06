# Chapter 6: Let Polymorphism

Let polymorphism is what makes Hindley-Milner type systems powerful. It allows functions to be used at multiple types, enabling code reuse without losing type safety.

---

## The Problem Without Polymorphism

Consider the identity function:

```
fn x => x
```

What type should it have?

- If we call it with a number, it's `number -> number`
- If we call it with a string, it's `string -> string`

Without polymorphism, we'd have to choose one:

```
-- If id : number -> number
id 42       -- OK
id "hello"  -- ERROR!
```

We'd need separate functions for each type—defeating the purpose of abstraction.

---

## Polymorphic Types

A polymorphic type works for **any** type. The identity function has type:

```
∀a. a -> a
```

Read as: "for all types `a`, this is a function from `a` to `a`."

The `∀a` (forall a) says `a` is a **type parameter**—it can be instantiated to any concrete type.

---

## Let Polymorphism

The key insight: **let bindings enable polymorphism**.

```
let id = fn x => x in (id 42, id "hello")
```

This works because:

1. `id` is generalized to `∀a. a -> a`
2. First use instantiates to `t0 -> t0`, unifies with `number -> number`
3. Second use instantiates to `t1 -> t1`, unifies with `string -> string`

Each use gets **fresh type variables**, allowing different instantiations.

---

## Why Not Lambda Polymorphism?

Consider:

```
fn f => (f 42, f "hello")
```

This doesn't work. Why?

The parameter `f` has type `t0` (a single type variable). When we see `f 42`:

- We unify `t0` with `number -> t1`
- Now `f : number -> t1`

When we then see `f "hello"`:

- We try to apply `f : number -> t1` to `string`
- ERROR: `number` doesn't match `string`

Lambda parameters are **monomorphic**—they have one type within the function body.

### Why This Difference?

The difference is **when generalization happens**:

- **Let**: Value is inferred, then generalized, then added to environment
- **Lambda**: Parameter is added to environment immediately (no generalization)

Generalization can only happen at certain points—specifically, when we have complete information about the value's type.

---

## The Value Restriction

Not everything can be generalized. Consider:

```
let r = ref None in (r := Some 42; !r)
```

If we generalized `r` to `∀a. ref (option a)`, we could:

- Store an `int` with `r := Some 42`
- Read it as a `string` with `(!r : option string)`

This would be unsound—type safety would be violated.

The **value restriction** says: only **values** can be generalized. A value is:

- A lambda: `fn x => ...`
- A literal: `42`, `"hello"`
- A constructor application: `Just x`
- A variable (already polymorphic)

Function applications and other expressions are not generalized.

In Algow, we're simpler—we don't have mutable references, so this isn't a concern. But it explains why some languages restrict polymorphism.

---

## How Generalization Works

```typescript
const generalize = (env: TypeEnv, type: Type): Scheme => {
  // Variables free in the type
  const typeVars = freeTypeVars(type);

  // Variables free in the environment
  const envVars = freeTypeVarsEnv(env);

  // Quantify variables in type but NOT in environment
  const toQuantify = [...typeVars].filter(v => !envVars.has(v));

  return scheme(toQuantify, type);
};
```

### Example 1: Simple Function

```
let id = fn x => x
```

After inferring `fn x => x`:

- Type: `t0 -> t0`
- Environment: `{}` (empty)
- Free in type: `{t0}`
- Free in env: `{}`
- Quantify: `{t0}`
- Scheme: `∀t0. t0 -> t0`

### Example 2: Nested Let

```
let f x =
  let g y = x + y in g
```

After inferring `fn y => x + y`:

- Type: `number -> number` (because `+` requires numbers)
- Environment: `{x: number, f: ...}`
- Free in type: `{}`
- Free in env: `{}`
- Quantify: `{}`
- Scheme: `number -> number` (monomorphic)

The type is already concrete—nothing to generalize.

### Example 3: Polymorphism Preserved

```
let compose f g = fn x => f (g x)
```

After inferring:

- Type: `(t1 -> t2) -> (t0 -> t1) -> t0 -> t2`
- Environment: `{}`
- Quantify all: `{t0, t1, t2}`
- Scheme: `∀t0 t1 t2. (t1 -> t2) -> (t0 -> t1) -> t0 -> t2`

---

## Instantiation in Detail

When using a polymorphic value, we instantiate with fresh variables:

```typescript
const instantiate = (scheme: Scheme): Type => {
  // Map each quantified variable to a fresh one
  const freshVars = new Map<string, Type>();
  for (const v of scheme.vars) {
    freshVars.set(v, freshTypeVar());
  }

  // Replace in the type
  return replaceVars(scheme.type, freshVars);
};
```

### Example

Using `id : ∀a. a -> a` twice:

```
let id = fn x => x in
(id 42, id "hello")
```

First `id`:

- Instantiate: `t5 -> t5`
- Apply to `42`: unify `t5` with `number`
- Result: `number`

Second `id`:

- Instantiate: `t6 -> t6` (fresh variables!)
- Apply to `"hello"`: unify `t6` with `string`
- Result: `string`

Each use is independent because instantiation creates fresh variables.

---

## Monomorphism vs Polymorphism

| Context                  | Polymorphic? | Why                                |
| ------------------------ | ------------ | ---------------------------------- |
| `let x = v in body`      | Yes          | `x` is generalized                 |
| `fn x => body`           | No           | `x` is a parameter                 |
| `match e with x => body` | No           | `x` is pattern-bound               |
| `let rec f = v in body`  | Yes          | `f` is generalized after inference |

---

## Practical Implications

### Code Reuse

Polymorphism enables generic functions:

```
let map f list = match list with
  | Nil => Nil
  | Cons x rest => Cons (f x) (map f rest)
end
```

Type: `∀a b. (a -> b) -> List a -> List b`

Works for any element types!

### Type Safety

Despite flexibility, types are checked:

```
let id = fn x => x in
id 42 + id "hello"  -- ERROR!
```

Even though `id` is polymorphic:

- `id 42` has type `number`
- `id "hello"` has type `string`
- `+` requires both operands to have the same numeric type
- Error: can't add number and string

---

## Recursive Polymorphism

Recursive functions need special handling:

```
let rec length list = match list with
  | Nil => 0
  | Cons _ rest => 1 + length rest
end
```

Process:

1. Create fresh type `t0` for `length`
2. Add `length : t0` to environment (monomorphic)
3. Infer body with `length` in scope
4. Unify inferred type with `t0`
5. Generalize the final type

Result: `length : ∀a. List a -> number`

---

## Summary

Let polymorphism is the key to expressive type inference:

1. **Let bindings** enable polymorphism through generalization
2. **Lambda parameters** remain monomorphic
3. **Generalization** quantifies free type variables
4. **Instantiation** creates fresh variables for each use
5. **Type safety** is preserved despite flexibility

The next chapter covers type checking for pattern matching.
