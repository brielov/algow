# Chapter 5: Code Generation

Code generation transforms IR into executable JavaScript. This chapter walks through the JavaScript backend, showing how each IR construct maps to JavaScript code.

---

## The Code Generation Context

Code generation maintains a simple context:

```typescript
type CodeGenContext = {
  indent: number;              // Current indentation level
  lines: string[];             // Generated code lines
  constructors: Set<string>;   // Constructor names (for identification)
};

const createContext = (constructorNames: readonly string[]): CodeGenContext => ({
  indent: 0,
  lines: [],
  constructors: new Set(constructorNames),
});
```

We track constructor names to distinguish them from regular variables—constructors need special handling.

---

## Generating Expressions

The main entry point dispatches on expression kind:

```typescript
const genExpr = (ctx: CodeGenContext, expr: ir.IRExpr): string => {
  switch (expr.kind) {
    case "IRAtomExpr":
      return genAtom(ctx, expr.atom);

    case "IRLet": {
      const binding = genBinding(ctx, expr.binding);
      const jsName = toJsId(expr.name);
      emit(ctx, `const ${jsName} = ${binding};`);
      return genExpr(ctx, expr.body);
    }

    case "IRLetRec": {
      // For recursive bindings, declare with let first
      const hasLambda = expr.bindings.some((b) => b.binding.kind === "IRLambdaBinding");

      if (hasLambda) {
        // Declare all variables first
        for (const { name } of expr.bindings) {
          emit(ctx, `let ${toJsId(name)};`);
        }
        // Then assign all values
        for (const { name, binding } of expr.bindings) {
          const bindingCode = genBinding(ctx, binding);
          emit(ctx, `${toJsId(name)} = ${bindingCode};`);
        }
      } else {
        // No lambdas - can use const directly
        for (const { name, binding } of expr.bindings) {
          const bindingCode = genBinding(ctx, binding);
          emit(ctx, `const ${toJsId(name)} = ${bindingCode};`);
        }
      }
      return genExpr(ctx, expr.body);
    }
  }
};
```

### Why Two-Phase Initialization for Lambdas?

In JavaScript, a function can reference variables declared later in the same scope:

```javascript
let f;
let g;
f = (x) => g(x);     // f references g
g = (x) => x + 1;    // g defined after f
```

This works because `g` is declared (not yet assigned) when `f` is created. The two-phase approach enables mutual recursion.

---

## Generating Atoms

Atoms are the simplest—literals and variables:

```typescript
const genAtom = (ctx: CodeGenContext, atom: ir.IRAtom): string => {
  switch (atom.kind) {
    case "IRLit":
      if (typeof atom.value === "string") {
        return JSON.stringify(atom.value);
      }
      return String(atom.value);

    case "IRVar": {
      // Check if this is a constructor
      if (ctx.constructors.has(atom.name)) {
        return `$con("${atom.name}")`;
      }
      return toJsId(atom.name);
    }
  }
};
```

Constructors are wrapped in `$con()` to create the proper tagged structure.

---

## Generating Bindings

Each binding type has its own generation strategy:

### Atom Binding

Trivial—just generate the atom:

```typescript
case "IRAtomBinding":
  return genAtom(ctx, binding.atom);
```

### Function Application

Uses the runtime `$apply` helper:

```typescript
case "IRAppBinding": {
  const func = genAtom(ctx, binding.func);
  const arg = genAtom(ctx, binding.arg);
  return `$apply(${func}, ${arg})`;
}
```

The `$apply` helper handles both regular functions and partial constructor application.

### Binary Operations

Most operators map directly; equality needs special handling:

```typescript
const genBinOp = (ctx: CodeGenContext, binding: ir.IRBinOpBinding): string => {
  const left = genAtom(ctx, binding.left);
  const right = genAtom(ctx, binding.right);

  switch (binding.op) {
    case "+":
    case "-":
    case "*":
    case "/":
    case "<":
    case ">":
    case "<=":
    case ">=":
      return `(${left} ${binding.op} ${right})`;

    case "==":
      // Use deep equality for complex types
      if (isComplexType(binding.operandType)) {
        return `$eq(${left}, ${right})`;
      }
      return `(${left} === ${right})`;

    case "!=":
      if (isComplexType(binding.operandType)) {
        return `!$eq(${left}, ${right})`;
      }
      return `(${left} !== ${right})`;

    case "++":
      // String concatenation uses + in JavaScript
      return `(${left} + ${right})`;
  }
};
```

The `isComplexType` check determines when we need structural equality:

```typescript
const isComplexType = (type: Type): boolean => {
  switch (type.kind) {
    case "TCon":
      return !["number", "string", "boolean"].includes(type.name);
    case "TVar":
      return true;  // Unknown type - use deep equality
    case "TFun":
    case "TApp":
    case "TRecord":
    case "TTuple":
      return true;
  }
};
```

### Conditionals

Conditionals become ternary expressions with IIFEs for complex branches:

```typescript
const genIf = (ctx: CodeGenContext, binding: ir.IRIfBinding): string => {
  const cond = genAtom(ctx, binding.cond);

  ctx.indent++;
  const savedLines = ctx.lines;

  // Generate then branch
  const thenLines: string[] = [];
  ctx.lines = thenLines;
  const thenResult = genExpr(ctx, binding.thenBranch);
  const thenCode = thenLines.length > 0
    ? `(() => {\n${thenLines.join("\n")}\nreturn ${thenResult};\n})()`
    : thenResult;

  // Generate else branch
  const elseLines: string[] = [];
  ctx.lines = elseLines;
  const elseResult = genExpr(ctx, binding.elseBranch);
  const elseCode = elseLines.length > 0
    ? `(() => {\n${elseLines.join("\n")}\nreturn ${elseResult};\n})()`
    : elseResult;

  ctx.lines = savedLines;
  ctx.indent--;

  return `(${cond} ? ${thenCode} : ${elseCode})`;
};
```

Simple branches become simple ternaries; complex branches get wrapped in IIFEs.

### Tuples and Records

Tuples become arrays; records become objects:

```typescript
case "IRTupleBinding": {
  const elements = binding.elements.map((e) => genAtom(ctx, e));
  return `[${elements.join(", ")}]`;
}

case "IRRecordBinding": {
  const fields = binding.fields.map((f) => `${f.name}: ${genAtom(ctx, f.value)}`);
  return `{ ${fields.join(", ")} }`;
}
```

Field and index access are straightforward:

```typescript
case "IRFieldAccessBinding": {
  const record = genAtom(ctx, binding.record);
  return `${record}.${binding.field}`;
}

case "IRTupleIndexBinding": {
  const tuple = genAtom(ctx, binding.tuple);
  return `${tuple}[${binding.index}]`;
}
```

### Lambdas

Regular lambdas become arrow functions:

```typescript
const genLambda = (ctx: CodeGenContext, binding: ir.IRLambdaBinding): string => {
  // Check for tail-recursive optimization
  if (binding.tailRecursive) {
    return genTailRecursiveLambda(ctx, binding);
  }

  const param = toJsId(binding.param);

  // Generate body
  ctx.indent++;
  const bodyLines: string[] = [];
  const savedLines = ctx.lines;
  ctx.lines = bodyLines;

  const bodyResult = genExpr(ctx, binding.body);

  ctx.lines = savedLines;
  ctx.indent--;

  if (bodyLines.length === 0) {
    // Simple lambda - single expression
    return `(${param}) => ${bodyResult}`;
  }

  // Complex lambda with bindings
  const body = bodyLines.join("\n");
  return `(${param}) => {\n${body}\nreturn ${bodyResult};\n}`;
};
```

---

## Tail Call Optimization in Code Generation

When a lambda is marked as tail-recursive, we generate a loop:

```typescript
const genTailRecursiveLambda = (ctx: CodeGenContext, binding: ir.IRLambdaBinding): string => {
  const tco = binding.tailRecursive!;
  const params = tco.params.map(toJsId);

  // Get the innermost body (unwrap nested lambdas)
  let innerBody = binding.body;
  while (innerBody.kind === "IRLet" && innerBody.binding.kind === "IRLambdaBinding") {
    innerBody = innerBody.binding.body;
  }

  // Generate the loop body with TCO-aware expression generator
  ctx.indent++;
  const bodyLines: string[] = [];
  ctx.lines = bodyLines;

  const bodyResult = genExprTCO(ctx, innerBody, tco.selfName, params);

  ctx.lines = [];
  ctx.indent--;

  // Build curried function with while loop in innermost
  if (params.length === 1) {
    return `(${params[0]}) => {\n  while (true) {\n${bodyLines.join("\n")}\n  return ${bodyResult};\n  }\n}`;
  }

  // Multi-param: (p1) => (p2) => ... => { while (true) { ... } }
  let result = "";
  for (let i = 0; i < params.length - 1; i++) {
    result += `(${params[i]}) => `;
  }
  result += `(${params[params.length - 1]}) => {\n  while (true) {\n${bodyLines.join("\n")}\n  return ${bodyResult};\n  }\n}`;

  return result;
};
```

### TCO Expression Generation

The TCO-aware generator converts tail calls into `continue`:

```typescript
const genExprTCO = (
  ctx: CodeGenContext,
  expr: ir.IRExpr,
  selfName: string,
  params: string[],
): string => {
  switch (expr.kind) {
    case "IRAtomExpr":
      return genAtom(ctx, expr.atom);

    case "IRLet": {
      // Check if this is a tail call to self
      const tailCallArgs = extractTailCallArgs(expr, selfName, params.length);
      if (tailCallArgs) {
        // Emit argument computations
        emitNonAppBindings(ctx, expr, selfName);

        // Generate parameter reassignment + continue
        const assignments = params
          .map((p, i) => `${p} = ${genAtom(ctx, tailCallArgs[i]!)};`)
          .join(" ");
        emit(ctx, `${assignments} continue;`);
        return "undefined";  // Never reached
      }

      // Regular let binding
      const binding = genBindingTCO(ctx, expr.binding, selfName, params);
      emit(ctx, `const ${toJsId(expr.name)} = ${binding};`);
      return genExprTCO(ctx, expr.body, selfName, params);
    }

    // ...
  }
};
```

### Example: Factorial with TCO

Source:
```
let rec fact n acc =
  if n == 0 then acc
  else fact (n - 1) (n * acc)
```

Generated JavaScript:
```javascript
const fact = (n) => (acc) => {
  while (true) {
    if (n === 0) {
      return acc;
    } else {
      const _t0 = n - 1;
      const _t1 = n * acc;
      n = _t0; acc = _t1; continue;
    }
  }
};
```

The recursive call becomes parameter reassignment and `continue`.

---

## Pattern Matching

Pattern matching generates a chain of if-else checks:

```typescript
const genMatch = (ctx: CodeGenContext, binding: ir.IRMatchBinding): string => {
  const scrutinee = genAtom(ctx, binding.scrutinee);
  const scrutineeVar = "_s";

  const lines: string[] = [];
  lines.push(`((${scrutineeVar}) => {`);

  for (let i = 0; i < binding.cases.length; i++) {
    const case_ = binding.cases[i]!;
    const { condition, bindings } = genPatternMatch(scrutineeVar, case_.pattern);

    const prefix = i === 0 ? "if" : "} else if";
    lines.push(`  ${prefix} (${condition}) {`);

    // Emit pattern bindings
    for (const [name, expr] of bindings) {
      lines.push(`    const ${toJsId(name)} = ${expr};`);
    }

    // Generate body
    const bodyResult = genExpr(ctx, case_.body);
    lines.push(`    return ${bodyResult};`);
  }

  lines.push("  }");
  lines.push(`})(${scrutinee})`);

  return lines.join("\n");
};
```

### Pattern Condition Generation

Each pattern generates a condition and bindings:

```typescript
const genPatternMatch = (
  scrutinee: string,
  pattern: ir.IRPattern,
): { condition: string; bindings: Array<[string, string]> } => {
  switch (pattern.kind) {
    case "IRPVar":
      return { condition: "true", bindings: [[pattern.name, scrutinee]] };

    case "IRPWildcard":
      return { condition: "true", bindings: [] };

    case "IRPLit": {
      const value = typeof pattern.value === "string"
        ? JSON.stringify(pattern.value)
        : String(pattern.value);
      return { condition: `${scrutinee} === ${value}`, bindings: [] };
    }

    case "IRPCon": {
      const conditions: string[] = [`${scrutinee}.$tag === "${pattern.name}"`];
      const bindings: Array<[string, string]> = [];

      for (let i = 0; i < pattern.args.length; i++) {
        const argScrutinee = `${scrutinee}.$args[${i}]`;
        const result = genPatternMatch(argScrutinee, pattern.args[i]!);
        if (result.condition !== "true") {
          conditions.push(result.condition);
        }
        bindings.push(...result.bindings);
      }

      return { condition: conditions.join(" && "), bindings };
    }

    case "IRPTuple": {
      const conditions: string[] = [];
      const bindings: Array<[string, string]> = [];

      for (let i = 0; i < pattern.elements.length; i++) {
        const elemScrutinee = `${scrutinee}[${i}]`;
        const result = genPatternMatch(elemScrutinee, pattern.elements[i]!);
        if (result.condition !== "true") {
          conditions.push(result.condition);
        }
        bindings.push(...result.bindings);
      }

      return { condition: conditions.join(" && ") || "true", bindings };
    }

    // ... other patterns
  }
};
```

---

## The Final Output

The `generateJS` function assembles everything:

```typescript
export const generateJS = (
  irExpr: ir.IRExpr,
  constructorNames: readonly string[],
): CodeGenOutput => {
  const ctx = createContext(constructorNames);

  // Generate the main expression
  const result = genExpr(ctx, irExpr);

  // Combine runtime + generated code
  const code = [
    RUNTIME,
    "// Generated code",
    ...ctx.lines,
    `const $result = ${result};`,
    "console.log($result);",
  ].join("\n");

  return { code, warnings: [] };
};
```

The runtime is prepended, followed by generated bindings, then the final result.

---

## Summary

JavaScript code generation maps IR constructs to JavaScript:

1. **Atoms** become literals or variable references
2. **Let bindings** become `const` declarations
3. **Letrec** uses two-phase initialization for mutual recursion
4. **Applications** use the `$apply` helper
5. **Lambdas** become arrow functions
6. **Tail-recursive lambdas** become while loops
7. **Pattern matching** becomes if-else chains
8. **Constructors** use tagged objects via `$con`

The next chapter covers the runtime helpers that make this all work.
