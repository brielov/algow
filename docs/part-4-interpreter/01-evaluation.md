# Chapter 1: Tree-Walking Evaluation

The interpreter directly executes AST nodes without generating intermediate code. This **tree-walking** approach is simpler than compilation but generally slower. It's useful for rapid development and as a reference implementation.

---

## Runtime Values

Before we can evaluate expressions, we need to define what values look like at runtime:

```typescript
export type Value = VNum | VStr | VBool | VClosure | VCon | VTuple | VRecord | VRef;
```

Let's examine each kind.

### Primitive Values

Numbers, strings, and booleans wrap their JavaScript counterparts:

```typescript
export type VNum = {
  readonly kind: "VNum";
  readonly value: number;
};

export type VStr = {
  readonly kind: "VStr";
  readonly value: string;
};

export type VBool = {
  readonly kind: "VBool";
  readonly value: boolean;
};
```

### Closures

Functions capture their environment:

```typescript
export type VClosure = {
  readonly kind: "VClosure";
  readonly param: string;
  readonly body: ast.Expr;
  readonly env: Env;
};
```

A closure stores:
- **param**: The parameter name
- **body**: The function body (AST node)
- **env**: The captured environment (for free variables)

### Constructors

Data constructors hold their arguments:

```typescript
export type VCon = {
  readonly kind: "VCon";
  readonly name: string;
  readonly args: readonly Value[];
};
```

`Just 42` becomes `{ kind: "VCon", name: "Just", args: [VNum(42)] }`.

### Compound Values

Tuples and records:

```typescript
export type VTuple = {
  readonly kind: "VTuple";
  readonly elements: readonly Value[];
};

export type VRecord = {
  readonly kind: "VRecord";
  readonly fields: ReadonlyMap<string, Value>;
};
```

### Reference Cells

For recursive bindings:

```typescript
export type VRef = {
  readonly kind: "VRef";
  value: Value | null;  // Mutable!
};
```

This is the only mutable value—used to implement `letrec`.

---

## Value Constructors

Smart constructors create values:

```typescript
export const vnum = (value: number): Value => ({ kind: "VNum", value });
export const vstr = (value: string): Value => ({ kind: "VStr", value });
export const vbool = (value: boolean): Value => ({ kind: "VBool", value });
export const vclosure = (param: string, body: ast.Expr, env: Env): Value => ({
  kind: "VClosure",
  param,
  body,
  env,
});
export const vcon = (name: string, args: readonly Value[] = []): Value => ({
  kind: "VCon",
  name,
  args,
});
export const vtuple = (elements: readonly Value[]): Value => ({ kind: "VTuple", elements });
export const vrecord = (fields: ReadonlyMap<string, Value>): Value => ({ kind: "VRecord", fields });
export const vref = (): VRef => ({ kind: "VRef", value: null });
```

---

## The Environment

The environment maps names to values:

```typescript
export type Env = ReadonlyMap<string, Value>;

const emptyEnv: Env = new Map();

const extendEnv = (env: Env, name: string, value: Value): Env => {
  const newEnv = new Map(env);
  newEnv.set(name, value);
  return newEnv;
};
```

When we evaluate a variable, we look it up in the environment. When we enter a `let` or function, we extend the environment.

---

## The Evaluate Function

The core of the interpreter dispatches on expression kind:

```typescript
export const evaluate = (env: Env, expr: ast.Expr): Value => {
  switch (expr.kind) {
    case "Num":
      return vnum(expr.value);

    case "Str":
      return vstr(expr.value);

    case "Bool":
      return vbool(expr.value);

    case "Var": {
      const value = env.get(expr.name)!;
      // Dereference if it's a ref cell (from letrec)
      if (value.kind === "VRef") {
        if (value.value === null) {
          throw new RuntimeError(`Uninitialized recursive binding: ${expr.name}`);
        }
        return value.value;
      }
      return value;
    }

    case "Abs":
      return vclosure(expr.param, expr.body, env);

    case "App": {
      const func = evaluate(env, expr.func);
      const arg = evaluate(env, expr.param);
      return apply(func, arg);
    }

    case "Let": {
      const value = evaluate(env, expr.value);
      const newEnv = extendEnv(env, expr.name, value);
      return evaluate(newEnv, expr.body);
    }

    case "LetRec":
      return evalLetRec(env, expr);

    case "If": {
      const cond = evaluate(env, expr.cond) as VBool;
      return cond.value ? evaluate(env, expr.then) : evaluate(env, expr.else);
    }

    case "BinOp":
      return evalBinOp(env, expr);

    case "Tuple": {
      const elements = expr.elements.map((e) => evaluate(env, e));
      return elements.length === 1 ? elements[0]! : vtuple(elements);
    }

    case "Record": {
      const fields = new Map<string, Value>();
      for (const field of expr.fields) {
        fields.set(field.name, evaluate(env, field.value));
      }
      return vrecord(fields);
    }

    case "FieldAccess": {
      const record = evaluate(env, expr.record) as VRecord;
      return record.fields.get(expr.field)!;
    }

    case "TupleIndex": {
      const tuple = evaluate(env, expr.tuple) as VTuple;
      return tuple.elements[expr.index]!;
    }

    case "Match":
      return evalMatch(env, expr);
  }
};
```

Let's walk through each case.

---

## Evaluating Literals

Literals are straightforward—wrap the value:

```typescript
case "Num":
  return vnum(expr.value);

case "Str":
  return vstr(expr.value);

case "Bool":
  return vbool(expr.value);
```

---

## Evaluating Variables

Look up the name in the environment:

```typescript
case "Var": {
  const value = env.get(expr.name)!;
  // Dereference if it's a ref cell (from letrec)
  if (value.kind === "VRef") {
    if (value.value === null) {
      throw new RuntimeError(`Uninitialized recursive binding: ${expr.name}`);
    }
    return value.value;
  }
  return value;
}
```

The type checker ensures the variable exists. If it's a `VRef` (from `letrec`), we dereference it.

---

## Evaluating Lambdas

Create a closure capturing the current environment:

```typescript
case "Abs":
  return vclosure(expr.param, expr.body, env);
```

The closure stores:
- The parameter name
- The body (unevaluated)
- The current environment (for free variables)

---

## Evaluating Application

Evaluate both function and argument, then apply:

```typescript
case "App": {
  const func = evaluate(env, expr.func);
  const arg = evaluate(env, expr.param);
  return apply(func, arg);
}
```

The `apply` function handles both closures and constructors:

```typescript
const apply = (func: Value, arg: Value): Value => {
  if (func.kind === "VClosure") {
    const newEnv = extendEnv(func.env, func.param, arg);
    return evaluate(newEnv, func.body);
  }
  // Must be a constructor (partial application)
  return vcon((func as VCon).name, [...(func as VCon).args, arg]);
};
```

For closures: extend the closure's captured environment with the argument binding, then evaluate the body.

For constructors: add the argument to the constructor's argument list.

---

## Evaluating Let

Evaluate the value, extend the environment, evaluate the body:

```typescript
case "Let": {
  const value = evaluate(env, expr.value);
  const newEnv = extendEnv(env, expr.name, value);
  return evaluate(newEnv, expr.body);
}
```

---

## Evaluating Conditionals

Evaluate the condition, then evaluate the appropriate branch:

```typescript
case "If": {
  const cond = evaluate(env, expr.cond) as VBool;
  return cond.value ? evaluate(env, expr.then) : evaluate(env, expr.else);
}
```

Note: We only evaluate one branch—not both. This is essential for termination (consider `if true then 1 else infinite_loop`).

---

## Evaluating Binary Operations

```typescript
const evalBinOp = (env: Env, expr: ast.BinOp): Value => {
  const left = evaluate(env, expr.left);
  const right = evaluate(env, expr.right);

  switch (expr.op) {
    // Arithmetic (type checker ensures numbers)
    case "-":
      return vnum((left as VNum).value - (right as VNum).value);
    case "*":
      return vnum((left as VNum).value * (right as VNum).value);
    case "/": {
      const divisor = (right as VNum).value;
      if (divisor === 0) throw new RuntimeError("Division by zero");
      return vnum((left as VNum).value / divisor);
    }

    // Addition (numbers or strings)
    case "+":
      if (left.kind === "VNum") {
        return vnum(left.value + (right as VNum).value);
      }
      return vstr((left as VStr).value + (right as VStr).value);

    // Comparisons
    case "<":
      if (left.kind === "VNum") return vbool(left.value < (right as VNum).value);
      return vbool((left as VStr).value < (right as VStr).value);
    // ... other comparisons

    // Equality
    case "==":
      return vbool(valuesEqual(left, right));
    case "!=":
      return vbool(!valuesEqual(left, right));

    // String concatenation
    case "++":
      return vstr((left as VStr).value + (right as VStr).value);
  }
};
```

The type checker ensures operands have compatible types. We dispatch on the left operand's type for polymorphic operators like `+`.

---

## Evaluating Tuples and Records

Build compound values:

```typescript
case "Tuple": {
  const elements = expr.elements.map((e) => evaluate(env, e));
  return elements.length === 1 ? elements[0]! : vtuple(elements);
}

case "Record": {
  const fields = new Map<string, Value>();
  for (const field of expr.fields) {
    fields.set(field.name, evaluate(env, field.value));
  }
  return vrecord(fields);
}
```

Single-element tuples unwrap to their element (no parentheses needed).

---

## Evaluating Field/Index Access

Extract from compound values:

```typescript
case "FieldAccess": {
  const record = evaluate(env, expr.record) as VRecord;
  return record.fields.get(expr.field)!;
}

case "TupleIndex": {
  const tuple = evaluate(env, expr.tuple) as VTuple;
  return tuple.elements[expr.index]!;
}
```

---

## Value Equality

Structural equality for the `==` operator:

```typescript
const valuesEqual = (a: Value, b: Value): boolean => {
  if (a.kind !== b.kind) return false;

  switch (a.kind) {
    case "VNum":
      return a.value === (b as VNum).value;
    case "VStr":
      return a.value === (b as VStr).value;
    case "VBool":
      return a.value === (b as VBool).value;
    case "VCon": {
      const bCon = b as VCon;
      if (a.name !== bCon.name || a.args.length !== bCon.args.length) return false;
      return a.args.every((arg, i) => valuesEqual(arg, bCon.args[i]!));
    }
    case "VTuple": {
      const bTuple = b as VTuple;
      if (a.elements.length !== bTuple.elements.length) return false;
      return a.elements.every((elem, i) => valuesEqual(elem, bTuple.elements[i]!));
    }
    case "VRecord": {
      const bRecord = b as VRecord;
      if (a.fields.size !== bRecord.fields.size) return false;
      for (const [key, val] of a.fields) {
        const bVal = bRecord.fields.get(key);
        if (bVal === undefined || !valuesEqual(val, bVal)) return false;
      }
      return true;
    }
    case "VClosure":
    case "VRef":
      return false;  // Functions are not comparable
  }
};
```

---

## Value Display

For showing results to users:

```typescript
export const valueToString = (value: Value): string => {
  switch (value.kind) {
    case "VNum":
      return String(value.value);
    case "VStr":
      return `"${value.value}"`;
    case "VBool":
      return String(value.value);
    case "VClosure":
      return "<function>";
    case "VCon":
      if (value.args.length === 0) return value.name;
      return `(${value.name} ${value.args.map(valueToString).join(" ")})`;
    case "VTuple":
      return `(${value.elements.map(valueToString).join(", ")})`;
    case "VRecord": {
      const fields = [...value.fields.entries()]
        .map(([k, v]) => `${k}: ${valueToString(v)}`)
        .join(", ");
      return `{ ${fields} }`;
    }
    case "VRef":
      return value.value ? valueToString(value.value) : "<uninitialized>";
  }
};
```

---

## Summary

Tree-walking evaluation:

1. **Dispatch on expression kind** to select evaluation logic
2. **Look up variables** in the environment
3. **Create closures** that capture the environment
4. **Extend environment** for let bindings and function calls
5. **Build runtime values** for each expression type

The next chapter covers closures in depth—how functions capture and use their environment.
