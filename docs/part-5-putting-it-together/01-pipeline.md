# Chapter 1: The Compilation Pipeline

This chapter shows how all the pieces connect. From source code to execution, we trace the complete flow through parsing, type checking, and either interpretation or compilation.

---

## Two Execution Modes

Algow supports two execution paths:

**Interpreter** (default):
```
Source → Parse → Bind → Check → Evaluate → Value
```

**Compiler** (`-c` flag):
```
Source → Parse → Bind → Check → Lower → Optimize → Generate → JavaScript
```

Both paths share the frontend (parse, bind, check). They diverge at execution—direct evaluation vs. code generation.

---

## The Entry Point

The main function handles command-line arguments:

```typescript
const main = async (): Promise<void> => {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    console.error("Usage: algow <file.alg>");
    console.error("       algow -e <expression>");
    console.error("       algow -t <file.alg>       (type check only)");
    console.error("       algow -c <file.alg>       (compile to JS)");
    console.error("       algow --emit-ir <file.alg> (emit IR)");
    process.exit(1);
  }

  try {
    if (args[0] === "-e" && args[1]) {
      run(args[1], "<stdin>");
    } else if (args[0] === "-t" && args[1]) {
      const source = await Bun.file(args[1]).text();
      typeCheck(source, args[1]);
    } else if (args[0] === "-c" && args[1]) {
      const source = await Bun.file(args[1]).text();
      compile(source, args[1]);
    } else if (args[0] === "--emit-ir" && args[1]) {
      const source = await Bun.file(args[1]).text();
      emitIR(source, args[1]);
    } else {
      const filename = args[0]!;
      const source = await Bun.file(filename).text();
      run(source, filename);
    }
  } catch (err) {
    console.error((err as Error).message);
    process.exit(1);
  }
};
```

---

## The Interpreter Pipeline

The `run` function executes via the interpreter:

```typescript
const run = (source: string, filename: string): void => {
  const diagnostics: Diagnostic[] = [];

  // Step 1: Parse
  const parseResult = parse(source);
  diagnostics.push(...parseResult.diagnostics);

  const expr = programToExpr(parseResult.program);
  if (!expr) {
    console.error(`${filename}: No expression to evaluate`);
    process.exit(1);
  }

  // Step 2: Inject prelude
  const wrappedExpr = wrapWithPrelude(expr);

  // Step 3: Process data declarations
  const prelude = processDeclarations(preludeDeclarations);
  const { typeEnv, registry, constructorNames } = processDeclarations(
    parseResult.program.declarations,
    prelude,
  );

  // Step 4: Bind names
  const bindResult = bindWithConstructors(constructorNames, wrappedExpr);
  diagnostics.push(...bindResult.diagnostics);

  // Step 5: Type check
  const checkResult = check(typeEnv, registry, wrappedExpr, bindResult.symbols);
  diagnostics.push(...checkResult.diagnostics);

  // Report any errors
  if (diagnostics.length > 0) {
    printDiagnostics(diagnostics, source, filename);
    process.exit(1);
  }

  // Step 6: Evaluate
  const evalEnv = createConstructorEnv(constructorNames);
  const result = evaluate(evalEnv, wrappedExpr);
  console.log(result);
};
```

### Step by Step

1. **Parse**: Transform source text to AST
2. **Inject prelude**: Add standard library bindings
3. **Process declarations**: Extract constructor types and build registry
4. **Bind names**: Resolve variable references
5. **Type check**: Infer types and check constraints
6. **Evaluate**: Execute the AST directly

---

## The Compiler Pipeline

The `compile` function generates JavaScript:

```typescript
const compile = (source: string, filename: string): void => {
  // Steps 1-5 are identical to run()
  const parseResult = parse(source);
  const expr = programToExpr(parseResult.program);

  if (parseResult.diagnostics.length > 0) {
    printDiagnostics(parseResult.diagnostics, source, filename);
    process.exit(1);
  }

  if (!expr) {
    console.error(`${filename}: No expression to compile`);
    process.exit(1);
  }

  const wrappedExpr = wrapWithPrelude(expr);
  const prelude = processDeclarations(preludeDeclarations);
  const { typeEnv, registry, constructorNames } = processDeclarations(
    parseResult.program.declarations,
    prelude,
  );

  const bindResult = bindWithConstructors(constructorNames, wrappedExpr);
  const checkResult = check(typeEnv, registry, wrappedExpr, bindResult.symbols);

  const allDiagnostics = [...bindResult.diagnostics, ...checkResult.diagnostics];
  if (allDiagnostics.length > 0) {
    printDiagnostics(allDiagnostics, source, filename);
    process.exit(1);
  }

  // Step 6: Lower to IR
  let ir = lowerToIR(wrappedExpr, typeEnv, checkResult);

  // Step 7: Optimize
  ir = optimize(ir);

  // Step 8: Generate JavaScript
  const output = generateJS(ir, constructorNames);

  for (const warning of output.warnings) {
    console.warn(`Warning: ${warning}`);
  }

  console.log(output.code);
};
```

### Additional Steps

6. **Lower to IR**: Transform AST to A-Normal Form
7. **Optimize**: Apply constant folding, DCE, TCO
8. **Generate JavaScript**: Emit executable JS code

---

## Processing Declarations

Data declarations define types and constructors:

```typescript
const prelude = processDeclarations(preludeDeclarations);
const { typeEnv, registry, constructorNames } = processDeclarations(
  parseResult.program.declarations,
  prelude,
);
```

`processDeclarations` returns:
- **typeEnv**: Type schemes for constructors
- **registry**: Maps type names to their constructors (for exhaustiveness)
- **constructorNames**: List of constructor names (for code generation)

The prelude's declarations are processed first, then user declarations extend them.

---

## Error Handling

Errors are collected as diagnostics, then reported at the end:

```typescript
const diagnostics: Diagnostic[] = [];

// Collect from each phase
diagnostics.push(...parseResult.diagnostics);
diagnostics.push(...bindResult.diagnostics);
diagnostics.push(...checkResult.diagnostics);

// Report all at once
if (diagnostics.length > 0) {
  printDiagnostics(diagnostics, source, filename);
  process.exit(1);
}
```

This enables better error messages—we can report all errors in a file, not just the first one.

---

## Diagnostic Display

The `printDiagnostics` function formats errors nicely:

```typescript
const printDiagnostics = (
  diagnostics: readonly Diagnostic[],
  source: string,
  filename: string,
): void => {
  for (const diag of diagnostics) {
    const { line, col } = offsetToLineCol(source, diag.start);
    const lineContent = getLine(source, line);

    // Header
    const severityLabel = diag.severity === "error" ? "error" : "warning";
    console.error(`${severityLabel}: ${diag.message}`);
    console.error(`  --> ${filename}:${line}:${col}`);

    // Source line with underline
    console.error(`${line} | ${lineContent}`);
    const underline = " ".repeat(col - 1) + "^".repeat(diag.end - diag.start);
    console.error(`   | ${underline}`);

    // Expected/actual for type mismatches
    if (diag.expected || diag.actual) {
      console.error(`   = expected: ${diag.expected}`);
      console.error(`   =   actual: ${diag.actual}`);
    }

    // Suggestions for unbound variables
    if (diag.suggestions?.length > 0) {
      console.error(`   = did you mean: ${diag.suggestions.join(", ")}?`);
    }
  }
};
```

Example output:

```
error[type-mismatch]: Type mismatch
  --> example.alg:3:5
   |
 3 | x + "hello"
   |     ^^^^^^^
   |
   = expected: number
   =   actual: string
```

---

## Type Check Only

The `-t` flag shows the inferred type without executing:

```typescript
const typeCheck = (source: string, filename: string): void => {
  // Parse, bind, check (same as run)
  // ...

  if (allDiagnostics.length > 0) {
    printDiagnostics(allDiagnostics, source, filename);
    process.exit(1);
  }

  // Print the type
  console.log(typeToString(checkResult.type));
};
```

Useful for exploring types:

```bash
$ algow -t -e "fn x => x"
t0 -> t0

$ algow -t -e "map (fn x => x + 1)"
List number -> List number
```

---

## Emit IR

The `--emit-ir` flag shows the intermediate representation:

```typescript
const emitIR = (source: string, filename: string): void => {
  // Parse, bind, check (same as run)
  // ...

  // Lower to IR
  const ir = lowerToIR(wrappedExpr, typeEnv, checkResult);
  console.log(JSON.stringify(ir, null, 2));
};
```

Useful for debugging the lowering and optimization passes.

---

## Complete Data Flow

```
┌──────────────┐
│ Source Code  │
└──────┬───────┘
       │ parse()
       ▼
┌──────────────┐
│    AST       │
└──────┬───────┘
       │ wrapWithPrelude()
       ▼
┌──────────────┐
│  AST + Lib   │
└──────┬───────┘
       │ bindWithConstructors()
       ▼
┌──────────────┐
│ Bound AST +  │
│ Symbol Table │
└──────┬───────┘
       │ check()
       ▼
┌──────────────┐
│  Typed AST   │
└──────┬───────┘
       │
       ├─────────────────┐
       │                 │
       ▼                 ▼
┌──────────────┐   ┌──────────────┐
│  evaluate()  │   │ lowerToIR()  │
└──────┬───────┘   └──────┬───────┘
       │                  │ optimize()
       ▼                  ▼
┌──────────────┐   ┌──────────────┐
│    Value     │   │  Optimized   │
└──────────────┘   │     IR       │
                   └──────┬───────┘
                          │ generateJS()
                          ▼
                   ┌──────────────┐
                   │  JavaScript  │
                   └──────────────┘
```

---

## Summary

The compilation pipeline:

1. **Parse**: Source → AST
2. **Prelude**: Inject standard library
3. **Declarations**: Process data types
4. **Bind**: Resolve names
5. **Check**: Infer and verify types
6. **Execute** (two paths):
   - Interpret: Evaluate AST directly
   - Compile: Lower → Optimize → Generate JS

All phases collect diagnostics for unified error reporting. The next chapter covers the prelude—the standard library available in every program.
