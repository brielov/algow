# Chapter 4: Optimizations

Optimization passes transform the IR to improve performance while preserving semantics. Algow includes three key optimizations: constant folding, dead code elimination, and tail call optimization.

---

## The Optimization Pass Interface

Each pass follows a simple interface:

```typescript
export type OptPass = {
  readonly name: string;
  run(expr: ir.IRExpr): ir.IRExpr;
};
```

A pass takes an IR expression and returns a (potentially) improved version. Passes can be composed:

```typescript
export const optimize = (
  expr: ir.IRExpr,
  passes: readonly OptPass[] = defaultPasses,
): ir.IRExpr => {
  let result = expr;
  for (const pass of passes) {
    result = pass.run(result);
  }
  return result;
};
```

---

## Constant Folding

**Constant folding** evaluates operations on literals at compile time.

### Before and After

```
let x = 2 + 3 in x * 2
```

After constant folding:

```
let x = 5 in x * 2
```

The addition is computed at compile time, not runtime.

### The Algorithm

```typescript
export const constantFolding: OptPass = {
  name: "constant-folding",
  run(expr) {
    return transformBindings(expr, (binding) => {
      if (binding.kind === "IRBinOpBinding") {
        const { op, left, right, type } = binding;

        // Both operands must be literals
        if (left.kind === "IRLit" && right.kind === "IRLit") {
          const result = evalBinOp(op, left.value, right.value);
          if (result !== undefined) {
            return ir.irAtomBinding(ir.irLit(result, type));
          }
        }
      }
      return binding;
    });
  },
};
```

### Evaluating Binary Operations

The `evalBinOp` function computes results for literal operands:

```typescript
const evalBinOp = (
  op: Op,
  left: number | string | boolean,
  right: number | string | boolean,
): number | string | boolean | undefined => {
  // Arithmetic operations (numbers only)
  if (typeof left === "number" && typeof right === "number") {
    switch (op) {
      case "+":
        return left + right;
      case "-":
        return left - right;
      case "*":
        return left * right;
      case "/":
        return right !== 0 ? left / right : undefined;
      case "<":
        return left < right;
      case ">":
        return left > right;
      case "<=":
        return left <= right;
      case ">=":
        return left >= right;
      case "==":
        return left === right;
      case "!=":
        return left !== right;
    }
  }

  // String operations
  if (typeof left === "string" && typeof right === "string") {
    switch (op) {
      case "+":
        return left + right;  // + is overloaded for string concatenation
      case "==":
        return left === right;
      // ... comparisons
    }
  }

  // Boolean equality
  if (typeof left === "boolean" && typeof right === "boolean") {
    switch (op) {
      case "==":
        return left === right;
      case "!=":
        return left !== right;
    }
  }

  return undefined;  // Can't fold
};
```

Note: Division by zero returns `undefined` to avoid runtime errors at compile time.

---

## Dead Code Elimination

**Dead code elimination** (DCE) removes bindings that are never used.

### Before and After

```
let x = 1 in
let y = 2 in
let z = x + 3 in
z
```

After DCE (if `y` is unused):

```
let x = 1 in
let z = x + 3 in
z
```

The binding `y = 2` is removed because `y` is never referenced.

### The Algorithm

DCE works in two phases:

1. **Collect uses**: Count how many times each variable is referenced
2. **Remove unused**: Delete bindings with zero references

```typescript
export const deadCodeElimination: OptPass = {
  name: "dead-code-elimination",
  run(expr) {
    // Collect all variable uses
    const uses = new Map<string, number>();
    collectUses(expr, uses);

    // Remove unused bindings
    return removeUnused(expr, uses);
  },
};
```

### Collecting Uses

Walk the IR and count variable references:

```typescript
const collectUses = (expr: ir.IRExpr, uses: Map<string, number>): void => {
  switch (expr.kind) {
    case "IRAtomExpr":
      if (expr.atom.kind === "IRVar") {
        uses.set(expr.atom.name, (uses.get(expr.atom.name) ?? 0) + 1);
      }
      break;

    case "IRLet":
      collectUsesBinding(expr.binding, uses);
      collectUses(expr.body, uses);
      break;

    case "IRLetRec":
      for (const b of expr.bindings) {
        collectUsesBinding(b.binding, uses);
      }
      collectUses(expr.body, uses);
      break;
  }
};
```

### Removing Unused Bindings

Check each binding and remove if unused:

```typescript
const removeUnused = (expr: ir.IRExpr, uses: Map<string, number>): ir.IRExpr => {
  switch (expr.kind) {
    case "IRAtomExpr":
      return expr;

    case "IRLet": {
      const newBody = removeUnused(expr.body, uses);

      // Check if this binding is used
      const usageCount = uses.get(expr.name) ?? 0;

      // If unused and pure, skip this let
      if (usageCount === 0 && isPure(expr.binding)) {
        return newBody;
      }

      // Otherwise keep the binding
      const newBinding = removeUnusedBinding(expr.binding, uses);
      return ir.irLet(expr.name, newBinding, newBody);
    }

    case "IRLetRec": {
      const newBody = removeUnused(expr.body, uses);

      // Filter out unused bindings
      const usedBindings = expr.bindings.filter((b) => {
        const usageCount = uses.get(b.name) ?? 0;
        return usageCount > 0 || !isPure(b.binding);
      });

      if (usedBindings.length === 0) {
        return newBody;
      }

      return ir.irLetRec(usedBindings.map(b => ({
        name: b.name,
        binding: removeUnusedBinding(b.binding, uses),
      })), newBody);
    }
  }
};
```

### Purity

We only remove **pure** bindings—those without side effects. In Algow, everything is pure:

```typescript
const isPure = (_binding: ir.IRBinding): boolean => {
  return true;  // All our bindings are pure
};
```

In languages with effects (I/O, mutation), impure bindings must be kept even if unused.

---

## Tail Call Optimization

**Tail call optimization** (TCO) transforms tail-recursive functions into loops. This prevents stack overflow on deep recursion.

### What Is a Tail Call?

A **tail call** is a function call in **tail position**—the call's result is immediately returned, with no further computation.

```
-- Tail recursive (tail call)
let rec fact n acc =
  if n == 0 then acc
  else fact (n - 1) (n * acc)

-- NOT tail recursive (multiplication after call)
let rec fact n =
  if n == 0 then 1
  else n * fact (n - 1)
```

In the first version, `fact (n - 1) (n * acc)` is in tail position. In the second, `n * fact (n - 1)` is not—multiplication happens after the recursive call.

### Why TCO Matters

Without TCO, each recursive call adds a stack frame:

```
fact 10000 1
  → fact 9999 10000
    → fact 9998 99990000
      → ... (10000 stack frames)
        → STACK OVERFLOW!
```

With TCO, we reuse the same stack frame:

```
fact 10000 1
  → [reuse frame] n=9999, acc=10000
  → [reuse frame] n=9998, acc=99990000
  → ... (1 stack frame)
  → result
```

### Detecting Tail Recursion

The algorithm checks each `letrec` binding:

```typescript
const isTailRecursive = (name: string, binding: ir.IRBinding): { params: string[] } | null => {
  if (binding.kind !== "IRLambdaBinding") {
    return null;
  }

  const params = collectLambdaParams(binding);
  const innerBody = getInnermostBody(binding);

  // Check if the body only has tail calls
  const result = checkTailCallsInBody(innerBody, name, params.length);

  if (result) {
    return { params };
  }

  return null;
};
```

### Checking for Tail Calls

A tail call pattern looks like:

```
let _t1 = n - 1 in
let _t2 = n * acc in
let _t3 = fact _t1 in
let _t4 = _t3 _t2 in
_t4
```

We trace backwards from the result to find the application chain:

```typescript
const extractTailCall = (
  expr: ir.IRExpr,
  funcName: string,
  paramCount: number,
): ir.IRAtom[] | null => {
  // Collect all let bindings
  const bindings: Array<{ name: string; binding: ir.IRBinding }> = [];
  let current: ir.IRExpr = expr;

  while (current.kind === "IRLet") {
    bindings.push({ name: current.name, binding: current.binding });
    current = current.body;
  }

  // Final expression should be a variable
  if (current.kind !== "IRAtomExpr" || current.atom.kind !== "IRVar") {
    return null;
  }

  const resultVar = current.atom.name;

  // Work backwards to find the application chain
  const args: ir.IRAtom[] = [];
  let targetVar = resultVar;

  for (let i = bindings.length - 1; i >= 0; i--) {
    const { name, binding } = bindings[i]!;

    if (name === targetVar && binding.kind === "IRAppBinding") {
      args.unshift(binding.arg);

      if (binding.func.kind === "IRVar") {
        if (binding.func.name === funcName) {
          if (args.length === paramCount) {
            return args;  // Found tail call!
          }
          return null;
        }
        targetVar = binding.func.name;
      } else {
        return null;
      }
    }
  }

  return null;
};
```

### Marking Tail-Recursive Functions

When detected, we add a marker to the lambda binding:

```typescript
const markTailRecursive = (
  binding: ir.IRLambdaBinding,
  selfName: string,
  params: readonly string[],
): ir.IRLambdaBinding => ({
  ...binding,
  tailRecursive: { selfName, params },
});
```

The code generator uses this marker to generate a loop instead of recursion.

### The Full TCO Pass

```typescript
export const tailCallOptimization: OptPass = {
  name: "tail-call-optimization",
  run(expr) {
    return transformTCO(expr);
  },
};

const transformTCO = (expr: ir.IRExpr): ir.IRExpr => {
  switch (expr.kind) {
    case "IRAtomExpr":
      return expr;

    case "IRLet": {
      const newBinding = transformBindingTCO(expr.binding);
      const newBody = transformTCO(expr.body);
      return ir.irLet(expr.name, newBinding, newBody);
    }

    case "IRLetRec": {
      // Check each binding for tail recursion
      const newBindings = expr.bindings.map((b) => {
        const tcoInfo = isTailRecursive(b.name, b.binding);
        if (tcoInfo && b.binding.kind === "IRLambdaBinding") {
          const marked = markTailRecursive(b.binding, b.name, tcoInfo.params);
          return { name: b.name, binding: transformBindingTCO(marked) };
        }
        return { name: b.name, binding: transformBindingTCO(b.binding) };
      });
      const newBody = transformTCO(expr.body);
      return ir.irLetRec(newBindings, newBody);
    }
  }
};
```

---

## The Default Pipeline

Algow runs these passes in order:

```typescript
const defaultPasses: readonly OptPass[] = [
  constantFolding,
  deadCodeElimination,
  tailCallOptimization,
];
```

Order matters:

1. **Constant folding** first—may create dead code
2. **DCE** second—removes code made dead by folding
3. **TCO** last—marks functions for loop generation

---

## Example: Full Optimization

Source:

```
let rec fact n acc =
  if n == 0 then acc
  else fact (n - 1) (n * acc)
in fact 5 1
```

After lowering and optimization:

1. **Constant folding**: No literals to fold in function body
2. **DCE**: All bindings used
3. **TCO**: `fact` marked as tail-recursive

The lambda binding now has `tailRecursive: { selfName: "fact", params: ["n", "acc"] }`.

The code generator will produce a loop instead of recursive calls.

---

## Summary

Optimization passes transform IR while preserving semantics:

1. **Constant folding**: Evaluate literal operations at compile time
2. **Dead code elimination**: Remove unused pure bindings
3. **Tail call optimization**: Mark tail-recursive functions for loop generation

Each pass has a simple interface and can be composed. The next chapter shows how optimized IR becomes JavaScript code.
