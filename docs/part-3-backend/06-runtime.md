# Chapter 6: The Runtime

The runtime provides helper functions that generated JavaScript code depends on. This chapter explains each helper and why it's needed.

---

## Value Representations

Before examining the helpers, let's understand how Algow values become JavaScript values:

| Algow Type | JavaScript Representation |
|------------|--------------------------|
| `number` | JavaScript `number` |
| `string` | JavaScript `string` |
| `boolean` | JavaScript `boolean` |
| `a -> b` | JavaScript function (closure) |
| `(a, b, c)` | JavaScript array `[a, b, c]` |
| `{ x: a, y: b }` | JavaScript object `{ x: a, y: b }` |
| `Just x` | `{ $tag: "Just", $args: [x] }` |
| `Nil` | `{ $tag: "Nil", $args: [] }` |

Most types map naturally. Constructors need a special tagged format for pattern matching.

---

## The Complete Runtime

Here's the full runtime that gets prepended to generated code:

```javascript
// Algow Runtime
"use strict";

/**
 * Apply a function to an argument.
 * Handles both closures and partial constructor application.
 */
const $apply = (fn, arg) => {
  if (typeof fn === "function") {
    return fn(arg);
  }
  // Constructor - partial application
  return { $tag: fn.$tag, $args: [...fn.$args, arg] };
};

/**
 * Create a constructor value.
 */
const $con = (tag, ...args) => ({ $tag: tag, $args: args });

/**
 * Deep structural equality for values.
 */
const $eq = (a, b) => {
  // Same reference or primitive equality
  if (a === b) return true;

  // Different types
  if (typeof a !== typeof b) return false;
  if (typeof a !== "object" || a === null) return false;

  // Constructor equality
  if ("$tag" in a && "$tag" in b) {
    if (a.$tag !== b.$tag) return false;
    if (a.$args.length !== b.$args.length) return false;
    return a.$args.every((x, i) => $eq(x, b.$args[i]));
  }

  // Array (tuple) equality
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    return a.every((x, i) => $eq(x, b[i]));
  }

  // Object (record) equality
  const keysA = Object.keys(a);
  const keysB = Object.keys(b);
  if (keysA.length !== keysB.length) return false;
  return keysA.every(k => k in b && $eq(a[k], b[k]));
};
```

Let's examine each helper in detail.

---

## $apply: Function Application

```javascript
const $apply = (fn, arg) => {
  if (typeof fn === "function") {
    return fn(arg);
  }
  // Constructor - partial application
  return { $tag: fn.$tag, $args: [...fn.$args, arg] };
};
```

### Why Not Just Call Functions Directly?

In Algow, constructors are values—you can pass them around like functions:

```
let apply f x = f x in
apply Just 42
```

Here `Just` is passed to `apply` like any function. But `Just` isn't a JavaScript function—it's a constructor that becomes `{ $tag: "Just", $args: [] }`.

When we apply an argument to a constructor, we need to add the argument to `$args`. That's what `$apply` does.

### How It Works

1. **If `fn` is a function**: Just call it normally
2. **If `fn` is a constructor**: Add the argument to the args list

Example execution:

```javascript
const Just = $con("Just");  // { $tag: "Just", $args: [] }
$apply(Just, 42);           // { $tag: "Just", $args: [42] }
```

### Multi-Argument Constructors

For constructors with multiple arguments, partial application chains:

```javascript
const Cons = $con("Cons");          // { $tag: "Cons", $args: [] }
const Cons_1 = $apply(Cons, 1);     // { $tag: "Cons", $args: [1] }
const list = $apply(Cons_1, Nil);   // { $tag: "Cons", $args: [1, Nil] }
```

---

## $con: Constructor Creation

```javascript
const $con = (tag, ...args) => ({ $tag: tag, $args: args });
```

This is the simplest helper—it creates a tagged object representing a constructor value.

### Usage in Generated Code

When the code generator sees a constructor reference, it emits:

```javascript
// For: Nil
$con("Nil")  // { $tag: "Nil", $args: [] }

// After full application: Just 42
$con("Just", 42)  // { $tag: "Just", $args: [42] }
```

Actually, the code generator uses `$con` for unapplied constructors and builds up applications with `$apply`.

### Why Tagged Objects?

We need to:
1. Distinguish between different constructors (via `$tag`)
2. Store constructor arguments (via `$args`)
3. Enable pattern matching to inspect the structure

The `{ $tag, $args }` format makes all of this straightforward.

---

## $eq: Structural Equality

```javascript
const $eq = (a, b) => {
  // Same reference or primitive equality
  if (a === b) return true;

  // Different types
  if (typeof a !== typeof b) return false;
  if (typeof a !== "object" || a === null) return false;

  // Constructor equality
  if ("$tag" in a && "$tag" in b) {
    if (a.$tag !== b.$tag) return false;
    if (a.$args.length !== b.$args.length) return false;
    return a.$args.every((x, i) => $eq(x, b.$args[i]));
  }

  // Array (tuple) equality
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    return a.every((x, i) => $eq(x, b[i]));
  }

  // Object (record) equality
  const keysA = Object.keys(a);
  const keysB = Object.keys(b);
  if (keysA.length !== keysB.length) return false;
  return keysA.every(k => k in b && $eq(a[k], b[k]));
};
```

### Why Not Just Use `===`?

JavaScript's `===` checks reference equality for objects:

```javascript
[1, 2] === [1, 2]  // false - different arrays
{ x: 1 } === { x: 1 }  // false - different objects
```

But Algow's `==` checks structural equality:

```
(1, 2) == (1, 2)  -- true
{ x = 1 } == { x = 1 }  -- true
Just 42 == Just 42  -- true
```

### How It Works

The algorithm handles each case:

1. **Primitives and references**: Use `===` directly
2. **Constructors**: Same tag? Same number of args? All args equal?
3. **Tuples (arrays)**: Same length? All elements equal?
4. **Records (objects)**: Same keys? All values equal?

Recursion handles nested structures—a tuple containing constructors containing records all works correctly.

### When Is $eq Used?

The code generator uses `$eq` when comparing complex types:

```typescript
case "==":
  if (isComplexType(binding.operandType)) {
    return `$eq(${left}, ${right})`;
  }
  return `(${left} === ${right})`;
```

For primitives (`number`, `string`, `boolean`), we use `===` directly—it's faster and correct.

---

## Why These Helpers?

### Minimal Runtime

Algow's runtime is intentionally small:
- **3 functions** totaling about 30 lines
- **No external dependencies**
- **Embeddable** in any JavaScript environment

### Native JavaScript Closures

We don't need explicit closure conversion because JavaScript handles closures natively:

```javascript
const makeAdder = (x) => (y) => x + y;
const add5 = makeAdder(5);  // Captures x=5
add5(3);  // Returns 8
```

No `$env` parameter or explicit capture list needed—JavaScript's closure mechanism does the work.

### Performance

By using native JavaScript features:
- Function calls are fast (JIT-optimized)
- Closures are fast (JIT-optimized)
- Object/array creation is fast

The runtime helpers add minimal overhead.

---

## Example: Complete Generated Code

For this Algow program:

```
data List a = Nil | Cons a (List a)

let length list =
  match list with
  | Nil => 0
  | Cons _ rest => 1 + length rest
  end
in length (Cons 1 (Cons 2 (Cons 3 Nil)))
```

The generated JavaScript:

```javascript
// Algow Runtime
"use strict";

const $apply = (fn, arg) => {
  if (typeof fn === "function") {
    return fn(arg);
  }
  return { $tag: fn.$tag, $args: [...fn.$args, arg] };
};

const $con = (tag, ...args) => ({ $tag: tag, $args: args });

const $eq = (a, b) => {
  if (a === b) return true;
  if (typeof a !== typeof b) return false;
  if (typeof a !== "object" || a === null) return false;
  // ... rest of $eq
};

// Generated code
let length;
length = (list) => {
  const _t0 = ((_s) => {
    if (_s.$tag === "Nil") {
      return 0;
    } else if (_s.$tag === "Cons") {
      const rest = _s.$args[1];
      const _t1 = $apply(length, rest);
      return (1 + _t1);
    }
  })(list);
  return _t0;
};

const _t2 = $con("Cons");
const _t3 = $apply(_t2, 1);
const _t4 = $con("Cons");
const _t5 = $apply(_t4, 2);
const _t6 = $con("Cons");
const _t7 = $apply(_t6, 3);
const _t8 = $con("Nil");
const _t9 = $apply(_t7, _t8);
const _t10 = $apply(_t5, _t9);
const _t11 = $apply(_t3, _t10);
const _t12 = $apply(length, _t11);
const $result = _t12;
console.log($result);  // Outputs: 3
```

The runtime helpers enable this JavaScript to faithfully execute the Algow program.

---

## Summary

The Algow runtime is minimal but essential:

1. **$apply**: Handles function calls and constructor application uniformly
2. **$con**: Creates tagged constructor values
3. **$eq**: Provides structural equality for complex types

Design choices that keep the runtime small:
- Native JavaScript closures (no explicit environment)
- Native JavaScript functions (no trampolining for most cases)
- Direct primitive operations (no wrapping)

This concludes Part 3: Backend. The next part covers the interpreter—an alternative execution path that evaluates AST directly.
