# Chapter 2: Closures

A **closure** is a function bundled with the environment in which it was created. This chapter explains how closures work and why they're essential.

---

## The Problem: Free Variables

Consider this function:

```
let x = 10 in
let add = fn y => x + y in
add 5
```

When we evaluate `add 5`:

1. Look up `add` → get a function `fn y => x + y`
2. Apply it to `5`
3. Evaluate `x + y` with `y = 5`
4. But where is `x`?

The variable `x` is **free** in the function body—it's not a parameter. Its value comes from the enclosing scope where the function was defined.

---

## The Solution: Capture the Environment

When we create a function, we capture the current environment:

```typescript
case "Abs":
  return vclosure(expr.param, expr.body, env);
```

The closure stores:

- `param`: The parameter name (`"y"`)
- `body`: The function body (`x + y`)
- `env`: The environment at creation time (`{ x → 10 }`)

---

## Closure Representation

```typescript
export type VClosure = {
  readonly kind: "VClosure";
  readonly param: string;
  readonly body: ast.Expr;
  readonly env: Env;
};
```

The `env` field is crucial—it holds the values of all variables that were in scope when the function was created.

---

## Applying Closures

When we apply a closure:

```typescript
const apply = (func: Value, arg: Value): Value => {
  if (func.kind === "VClosure") {
    const newEnv = extendEnv(func.env, func.param, arg);
    return evaluate(newEnv, func.body);
  }
  // ... constructor case
};
```

Key steps:

1. Start with the closure's captured environment (`func.env`)
2. Extend it with the argument binding (`param → arg`)
3. Evaluate the body in this extended environment

We use the **closure's** environment, not the **caller's** environment. This is what makes closures work.

---

## Example: Step by Step

```
let x = 10 in
let add = fn y => x + y in
add 5
```

### Step 1: Evaluate `let x = 10`

- Value: `VNum(10)`
- Extend env: `{ x → 10 }`

### Step 2: Evaluate `let add = fn y => x + y`

- Create closure with current env:
  ```
  VClosure {
    param: "y",
    body: (x + y),
    env: { x → 10 }
  }
  ```
- Extend env: `{ x → 10, add → <closure> }`

### Step 3: Evaluate `add 5`

- Look up `add` → get the closure
- Evaluate argument: `VNum(5)`
- Apply:
  1. Start with closure's env: `{ x → 10 }`
  2. Extend with param: `{ x → 10, y → 5 }`
  3. Evaluate `x + y`:
     - Look up `x` → `10`
     - Look up `y` → `5`
     - Result: `VNum(15)`

The closure "remembers" that `x = 10` even though we're now in a different context.

---

## Why Closures Matter

### Higher-Order Functions

```
let makeAdder n = fn x => n + x in
let add5 = makeAdder 5 in
let add10 = makeAdder 10 in
(add5 3, add10 3)
```

`add5` captures `n = 5`; `add10` captures `n = 10`. Same function body, different captured values.

### Callbacks

```
let list = Cons 1 (Cons 2 (Cons 3 Nil)) in
let multiplier = 2 in
map (fn x => x * multiplier) list
```

The anonymous function captures `multiplier = 2`.

### Partial Application

```
let add x y = x + y in
let add10 = add 10 in
add10 5
```

`add 10` returns a closure capturing `x = 10`.

---

## Lexical vs Dynamic Scoping

Algow uses **lexical scoping**: a function's free variables are resolved where the function is **defined**, not where it's **called**.

Consider:

```
let x = 1 in
let f = fn y => x + y in
let x = 100 in
f 5
```

With **lexical scoping** (Algow): `f 5` returns `6` (x = 1 from definition site)

With **dynamic scoping**: `f 5` would return `105` (x = 100 from call site)

Lexical scoping is more predictable—you can understand a function's behavior by looking at where it's defined, not all the places it might be called.

---

## Nested Functions

Closures can nest arbitrarily:

```
let a = 1 in
let f = fn x =>
  let b = 2 in
  fn y =>
    let c = 3 in
    fn z => a + b + c + x + y + z
in
let g = f 10 in
let h = g 20 in
h 30
```

Each function captures its enclosing environment:

- Outer lambda captures `{ a → 1 }`
- Middle lambda captures `{ a → 1, x → 10, b → 2 }`
- Inner lambda captures `{ a → 1, x → 10, b → 2, y → 20, c → 3 }`

---

## Closures and Recursion

Simple recursion works with `letrec`:

```
let rec fact n =
  if n == 0 then 1
  else n * fact (n - 1)
in fact 5
```

The `fact` closure captures a reference to itself, enabling recursion.

### Mutual Recursion

```
let rec isEven n = if n == 0 then true else isOdd (n - 1)
and isOdd n = if n == 0 then false else isEven (n - 1)
in isEven 10
```

Both `isEven` and `isOdd` capture references to each other.

---

## How Letrec Works

Recursive bindings need special handling. The textbook approach:

```typescript
case "LetRec": {
  // Step 1: Create ref cells for ALL bindings
  const refs = new Map<string, VRef>();
  let newEnv = env;

  for (const binding of expr.bindings) {
    const ref = vref();  // { kind: "VRef", value: null }
    refs.set(binding.name, ref);
    newEnv = extendEnv(newEnv, binding.name, ref);
  }

  // Step 2: Evaluate all values with all refs in scope
  for (const binding of expr.bindings) {
    const value = evaluate(newEnv, binding.value);
    refs.get(binding.name)!.value = value;  // Fill the ref
  }

  return evaluate(newEnv, expr.body);
}
```

The trick:

1. Create mutable reference cells for all bindings
2. Add all refs to the environment (values still null)
3. Evaluate each binding—closures capture refs, not values
4. Fill in the refs with actual values
5. When closures look up the recursive name, they dereference the ref

### Why Refs Work

When we create the `fact` closure:

- Environment contains `{ fact → VRef(null) }`
- Closure captures this environment

When we fill the ref:

- `refs.get("fact").value = <the closure>`
- Now `VRef.value` points to the closure

When the closure runs and looks up `fact`:

1. Get `VRef` from captured environment
2. Dereference to get the actual closure
3. Apply recursively

---

## Dereferencing

Variable lookup handles refs:

```typescript
case "Var": {
  const value = env.get(expr.name)!;
  if (value.kind === "VRef") {
    if (value.value === null) {
      throw new RuntimeError(`Uninitialized recursive binding: ${expr.name}`);
    }
    return value.value;
  }
  return value;
}
```

If the value is a ref, we dereference it. An uninitialized ref (value is null) means the code tried to use a recursive binding before it was defined—a runtime error.

---

## Summary

Closures are functions with captured environments:

1. **Creation**: Capture the current environment
2. **Application**: Use captured environment + argument
3. **Free variables**: Resolved from captured environment
4. **Lexical scoping**: Variables resolved at definition site
5. **Recursion**: Uses mutable refs to tie the knot

The next chapter covers pattern matching at runtime.
