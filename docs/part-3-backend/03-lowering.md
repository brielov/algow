# Chapter 3: Lowering to ANF

**Lowering** transforms the typed AST into A-Normal Form IR. The key operation: ensuring all arguments to complex operations are atomic by introducing fresh variable names.

---

## The Normalization Process

The central operation in lowering is **normalization**: taking an expression and producing an atom, introducing let bindings as needed.

If the expression is already atomic (a literal or variable), we return it directly. Otherwise, we lower it, bind the result to a fresh variable, and return that variable.

```typescript
type NormalizeResult = {
  bindings: Array<{ name: string; binding: IRBinding }>;
  atom: IRAtom;
};
```

The result contains:
1. **Bindings** to prepend (the work needed to compute the value)
2. **Atom** representing the result

---

## The Normalize Function

```typescript
const normalize = (ctx: LowerContext, expr: ast.Expr): NormalizeResult => {
  // Check if expression is already atomic
  switch (expr.kind) {
    case "Num":
      return { bindings: [], atom: ir.irLit(expr.value, tNum) };
    case "Str":
      return { bindings: [], atom: ir.irLit(expr.value, tStr) };
    case "Bool":
      return { bindings: [], atom: ir.irLit(expr.value, tBool) };
    case "Var": {
      const type = lookupType(ctx, expr.name);
      return { bindings: [], atom: ir.irVar(expr.name, type) };
    }
  }

  // Complex expression - lower it and bind to fresh variable
  const lowered = lowerExpr(ctx, expr);

  // If the lowered expression is just an atom, return it directly
  if (lowered.kind === "IRAtomExpr") {
    return { bindings: [], atom: lowered.atom };
  }

  // Extract bindings and create a fresh variable for the result
  const name = freshVar(ctx);
  const type = lowered.type;
  extendEnv(ctx, name, type);

  const { bindings, finalBinding } = extractBindings(lowered);

  return {
    bindings: [...bindings, { name, binding: finalBinding }],
    atom: ir.irVar(name, type),
  };
};
```

### Example: Normalizing a Complex Expression

Consider normalizing `1 + 2`:

1. `1 + 2` is not atomic (it's a `BinOp`)
2. Lower it to get a `BinOpBinding`
3. Generate fresh name `_t0`
4. Return `{ bindings: [{ name: "_t0", binding: <1 + 2> }], atom: irVar("_t0") }`

Now `1 + 2` can be used as an argument—via the atom `_t0`.

---

## The Lowering Context

Lowering needs some context:

```typescript
type LowerContext = {
  varCounter: number;           // For generating fresh names
  typeEnv: TypeEnv;            // Maps names to type schemes
  subst: ReadonlyMap<string, Type>;  // From type checker
};
```

Key operations:

```typescript
// Generate a fresh variable name
const freshVar = (ctx: LowerContext, prefix = "_t"): string => {
  return `${prefix}${ctx.varCounter++}`;
};

// Extend environment with a new binding
const extendEnv = (ctx: LowerContext, name: string, type: Type): void => {
  ctx.typeEnv.set(name, { vars: [], constraints: [], type });
};

// Look up a variable's type
const lookupType = (ctx: LowerContext, name: string): Type => {
  const scheme = ctx.typeEnv.get(name);
  if (!scheme) {
    throw new Error(`Unknown variable during lowering: ${name}`);
  }
  return applySubst(ctx.subst, scheme.type);
};
```

---

## Lowering Each Expression Type

### Literals

Literals are already atomic:

```typescript
case "Num":
  return ir.irAtomExpr(ir.irLit(expr.value, tNum));

case "Str":
  return ir.irAtomExpr(ir.irLit(expr.value, tStr));

case "Bool":
  return ir.irAtomExpr(ir.irLit(expr.value, tBool));
```

### Variables

Variables are already atomic:

```typescript
case "Var": {
  const type = lookupType(ctx, expr.name);
  return ir.irAtomExpr(ir.irVar(expr.name, type));
}
```

### Let Bindings

Lower the value, extract bindings, then lower the body:

```typescript
const lowerLet = (ctx: LowerContext, expr: ast.Let): ir.IRExpr => {
  const valueIR = lowerExpr(ctx, expr.value);

  // Extract bindings from the value
  const { bindings, finalBinding } = extractBindings(valueIR);

  // Add the binding to the environment
  extendEnv(ctx, expr.name, finalBinding.type);

  // Lower the body
  const bodyIR = lowerExpr(ctx, expr.body);

  // Create the let for the main binding
  const letExpr = ir.irLet(expr.name, finalBinding, bodyIR);

  // Wrap with any preceding bindings
  return wrapWithBindings(bindings, letExpr);
};
```

### Recursive Bindings

Recursive bindings need all names in scope before lowering any values:

```typescript
const lowerLetRec = (ctx: LowerContext, expr: ast.LetRec): ir.IRExpr => {
  // Step 1: Add placeholder types for ALL bindings
  for (const binding of expr.bindings) {
    const placeholderType: Type = { kind: "TVar", name: `_rec_${binding.name}` };
    extendEnv(ctx, binding.name, placeholderType);
  }

  // Step 2: Lower all values (all names are in scope)
  const irBindings: ir.IRRecBinding[] = [];
  for (const binding of expr.bindings) {
    const valueIR = lowerExpr(ctx, binding.value);
    const { bindings: preBindings, finalBinding } = extractBindings(valueIR);
    irBindings.push(ir.irRecBinding(binding.name, finalBinding));

    // Update environment with actual type
    extendEnv(ctx, binding.name, finalBinding.type);
  }

  // Step 3: Lower the body
  const bodyIR = lowerExpr(ctx, expr.body);

  // Step 4: Create the letrec
  return ir.irLetRec(irBindings, bodyIR);
};
```

### Function Application

This is where normalization shines—ensure both function and argument are atoms:

```typescript
const lowerApp = (ctx: LowerContext, expr: ast.App): ir.IRExpr => {
  // Normalize function and argument to atoms
  const funcResult = normalize(ctx, expr.func);
  const argResult = normalize(ctx, expr.param);

  // Get the return type from the function type
  const funcType = funcResult.atom.type;
  const returnType = getReturnType(funcType);

  // Create the application binding
  const binding = ir.irAppBinding(funcResult.atom, argResult.atom, returnType);

  // Generate a name for the result
  const name = freshVar(ctx);
  extendEnv(ctx, name, returnType);

  const result = ir.irLet(name, binding, ir.irAtomExpr(ir.irVar(name, returnType)));

  // Wrap with all the bindings from normalization
  return wrapWithBindings([...funcResult.bindings, ...argResult.bindings], result);
};
```

### Example: Lowering `f (g x)`

1. Normalize `f`: already a variable → `{ bindings: [], atom: irVar("f") }`
2. Normalize `g x`:
   - Not atomic, so lower it
   - Normalize `g`: `{ bindings: [], atom: irVar("g") }`
   - Normalize `x`: `{ bindings: [], atom: irVar("x") }`
   - Create app binding: `_t0 = g x`
   - Return `{ bindings: [_t0 = g x], atom: irVar("_t0") }`
3. Create app binding: `_t1 = f _t0`
4. Wrap with bindings:
   ```
   let _t0 = g x in
   let _t1 = f _t0 in
   _t1
   ```

Both applications now have atomic arguments.

### Binary Operations

Similar to application—normalize both operands:

```typescript
const lowerBinOp = (ctx: LowerContext, expr: ast.BinOp): ir.IRExpr => {
  // Normalize operands to atoms
  const leftResult = normalize(ctx, expr.left);
  const rightResult = normalize(ctx, expr.right);

  // Determine result type based on operator
  const operandType = leftResult.atom.type;
  let resultType: Type;
  switch (expr.op) {
    case "+":
    case "-":
    case "*":
    case "/":
      resultType = tNum;
      break;
    case "<":
    case "<=":
    case ">":
    case ">=":
    case "==":
    case "!=":
      resultType = tBool;
      break;
    case "++":
      resultType = tStr;
      break;
  }

  // Create the binary operation binding
  const binding = ir.irBinOpBinding(
    expr.op,
    leftResult.atom,
    rightResult.atom,
    operandType,
    resultType,
  );

  // Generate a name for the result
  const name = freshVar(ctx);
  extendEnv(ctx, name, resultType);

  const result = ir.irLet(name, binding, ir.irAtomExpr(ir.irVar(name, resultType)));

  return wrapWithBindings([...leftResult.bindings, ...rightResult.bindings], result);
};
```

### Lambdas

Lambdas create new scopes:

```typescript
const lowerAbs = (ctx: LowerContext, expr: ast.Abs): ir.IRExpr => {
  // Create a type for the parameter
  const paramType: Type = { kind: "TVar", name: `_param${ctx.varCounter++}` };

  // Extend environment with parameter
  const savedEnv = new Map(ctx.typeEnv);
  extendEnv(ctx, expr.param, paramType);

  // Lower the body
  const bodyIR = lowerExpr(ctx, expr.body);

  // Restore environment
  ctx.typeEnv = savedEnv;

  // Create the lambda binding
  const funcType: Type = { kind: "TFun", param: paramType, ret: bodyIR.type };
  const binding = ir.irLambdaBinding(expr.param, paramType, bodyIR, funcType);

  // Wrap in a let to name the lambda
  const name = freshVar(ctx, "_fn");
  extendEnv(ctx, name, funcType);

  return ir.irLet(name, binding, ir.irAtomExpr(ir.irVar(name, funcType)));
};
```

### Conditionals

Condition must be atomic; branches are full expressions:

```typescript
const lowerIf = (ctx: LowerContext, expr: ast.If): ir.IRExpr => {
  // Normalize condition to atom
  const condResult = normalize(ctx, expr.cond);

  // Lower both branches
  const thenIR = lowerExpr(ctx, expr.then);
  const elseIR = lowerExpr(ctx, expr.else);

  // Both branches should have the same type
  const resultType = thenIR.type;

  // Create the if binding
  const binding = ir.irIfBinding(condResult.atom, thenIR, elseIR, resultType);

  // Generate a name for the result
  const name = freshVar(ctx);
  extendEnv(ctx, name, resultType);

  const result = ir.irLet(name, binding, ir.irAtomExpr(ir.irVar(name, resultType)));

  return wrapWithBindings(condResult.bindings, result);
};
```

### Pattern Matching

Normalize the scrutinee, then lower each case:

```typescript
const lowerMatch = (ctx: LowerContext, expr: ast.Match): ir.IRExpr => {
  // Normalize scrutinee to atom
  const scrutineeResult = normalize(ctx, expr.expr);

  // Lower each case
  const irCases: ir.IRCase[] = [];
  let resultType: Type | null = null;

  for (const case_ of expr.cases) {
    // Lower the pattern
    const irPattern = lowerPattern(case_.pattern, scrutineeResult.atom.type);

    // Extend environment with pattern bindings
    const savedEnv = new Map(ctx.typeEnv);
    extendPatternBindings(ctx, case_.pattern, scrutineeResult.atom.type);

    // Lower guard and body
    const guardIR = case_.guard ? lowerExpr(ctx, case_.guard) : undefined;
    const bodyIR = lowerExpr(ctx, case_.body);
    resultType = bodyIR.type;

    // Restore environment
    ctx.typeEnv = savedEnv;

    irCases.push(ir.irCase(irPattern, bodyIR, guardIR));
  }

  // Create the match binding
  const binding = ir.irMatchBinding(scrutineeResult.atom, irCases, resultType!);

  // Generate a name for the result
  const name = freshVar(ctx);
  return wrapWithBindings(scrutineeResult.bindings,
    ir.irLet(name, binding, ir.irAtomExpr(ir.irVar(name, resultType!)))
  );
};
```

---

## Pattern Lowering

Patterns are lowered recursively, preserving structure but adding types:

```typescript
const lowerPattern = (pattern: ast.Pattern, type: Type): ir.IRPattern => {
  switch (pattern.kind) {
    case "PVar":
      return ir.irPVar(pattern.name, type);

    case "PWildcard":
      return ir.irPWildcard(type);

    case "PLit": {
      let litType: Type;
      if (typeof pattern.value === "number") litType = tNum;
      else if (typeof pattern.value === "string") litType = tStr;
      else litType = tBool;
      return ir.irPLit(pattern.value, litType);
    }

    case "PCon": {
      const args = pattern.args.map((arg, i) => {
        const argType: Type = { kind: "TVar", name: `_arg${i}` };
        return lowerPattern(arg, argType);
      });
      return ir.irPCon(pattern.name, args, type);
    }

    case "PTuple": {
      const elementTypes = type.kind === "TTuple" ? type.elements : [];
      const elements = pattern.elements.map((elem, i) => {
        const elemType = elementTypes[i] ?? { kind: "TVar", name: `_elem${i}` };
        return lowerPattern(elem, elemType);
      });
      return ir.irPTuple(elements, type);
    }

    // ... other pattern kinds
  }
};
```

---

## Wrapping With Bindings

After normalization, we have a list of bindings to prepend. This helper builds the nested let structure:

```typescript
const wrapWithBindings = (
  bindings: Array<{ name: string; binding: ir.IRBinding }>,
  body: ir.IRExpr,
): ir.IRExpr => {
  let result = body;
  for (let i = bindings.length - 1; i >= 0; i--) {
    const { name, binding } = bindings[i]!;
    result = ir.irLet(name, binding, result);
  }
  return result;
};
```

Building from the inside out ensures the final structure is correct.

---

## The Entry Point

The main entry point combines context creation and expression lowering:

```typescript
export const lowerToIR = (
  expr: ast.Expr,
  typeEnv: TypeEnv,
  checkOutput: CheckOutput,
): ir.IRExpr => {
  const ctx = createContext(typeEnv, checkOutput);
  return lowerExpr(ctx, expr);
};
```

---

## Summary

Lowering transforms AST to ANF through normalization:

1. **Atomic expressions** (literals, variables) pass through unchanged
2. **Complex expressions** are bound to fresh variables
3. **Normalization** ensures all arguments are atomic
4. **Type information** flows from the type checker to the IR
5. **Patterns** are lowered with type annotations

The result is ANF IR ready for optimization and code generation. The next chapter covers optimization passes.
