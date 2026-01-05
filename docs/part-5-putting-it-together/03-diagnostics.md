# Chapter 3: The Diagnostic System

Good error messages transform a frustrating debugging experience into a learning opportunity. This chapter covers how Algow collects, structures, and displays diagnostics.

---

## Design Philosophy

Algow's diagnostic system follows these principles:

1. **No exceptions for recoverable errors**: Parse and type errors don't throw—they're collected as diagnostics
2. **Multiple errors per run**: Report all errors found, not just the first one
3. **Rich context**: Include expected/actual types, suggestions, and notes
4. **Source positions**: Every diagnostic points to exact source locations

---

## The Diagnostic Type

```typescript
export type DiagnosticSeverity = "error" | "warning" | "info";

export type DiagnosticKind =
  | "type-mismatch"
  | "unbound-variable"
  | "arity-mismatch"
  | "infinite-type"
  | "missing-field"
  | "non-exhaustive"
  | "syntax-error";

export type Diagnostic = {
  readonly start: number;        // Byte offset of start
  readonly end: number;          // Byte offset of end
  readonly message: string;      // Human-readable message
  readonly severity: DiagnosticSeverity;
  readonly kind?: DiagnosticKind;
  readonly expected?: string;    // For type mismatches
  readonly actual?: string;      // For type mismatches
  readonly suggestions?: readonly string[];  // For unbound variables
  readonly notes?: readonly string[];        // Additional context
};
```

Key design decisions:

- **Byte offsets, not line/column**: Faster to compute, LSP converts as needed
- **Optional kind field**: Enables categorized error handling
- **Structured context**: `expected`/`actual` separate from `message`

---

## Severity Levels

### Error

Fatal issues that prevent execution:

```typescript
export const error = (start: number, end: number, message: string): Diagnostic => ({
  start,
  end,
  message,
  severity: "error",
});
```

Examples:
- Type mismatches
- Unbound variables
- Syntax errors
- Non-exhaustive patterns

### Warning

Issues that don't prevent execution but indicate problems:

```typescript
export const warning = (start: number, end: number, message: string): Diagnostic => ({
  start,
  end,
  message,
  severity: "warning",
});
```

Examples:
- Unused variables
- Redundant patterns (future)

### Info

Informational messages:

```typescript
export const info = (start: number, end: number, message: string): Diagnostic => ({
  start,
  end,
  message,
  severity: "info",
});
```

Examples:
- Style suggestions (future)

---

## Specialized Diagnostic Constructors

### Type Mismatch

The most common error—when types don't unify:

```typescript
export const typeMismatch = (
  start: number,
  end: number,
  expected: string,
  actual: string,
  context?: string,
): Diagnostic => ({
  start,
  end,
  message: context ? `Type mismatch in ${context}` : "Type mismatch",
  severity: "error",
  kind: "type-mismatch",
  expected,
  actual,
});
```

Example output:

```
error: Type mismatch in function application
  --> example.alg:5:10
   |
 5 | add "hello"
   |     ^^^^^^^
   |
   = expected: number
   =   actual: string
```

The `expected` and `actual` fields are separate from the message, enabling:
- Structured display (as shown above)
- LSP integration
- Custom formatters

### Unbound Variable

When a name isn't in scope:

```typescript
export const unboundVariable = (
  start: number,
  end: number,
  name: string,
  suggestions?: readonly string[],
): Diagnostic => ({
  start,
  end,
  message: `Unknown variable: ${name}`,
  severity: "error",
  kind: "unbound-variable",
  suggestions,
});
```

Example output with suggestions:

```
error: Unknown variable: lenght
  --> example.alg:3:5
   |
 3 | lenght list
   | ^^^^^^
   |
   = did you mean: length?
```

---

## The Suggestion System

When a variable isn't found, we suggest similar names using Levenshtein distance.

### Levenshtein Distance

Levenshtein distance counts the minimum edits (insertions, deletions, substitutions) to transform one string into another.

```typescript
export const levenshteinDistance = (a: string, b: string): number => {
  if (a.length === 0) return b.length;
  if (b.length === 0) return a.length;

  // Create a 2D matrix for dynamic programming
  const matrix: number[][] = [];

  // Initialize first column: cost of deleting from b
  for (let i = 0; i <= b.length; i++) {
    matrix[i] = [i];
  }

  // Initialize first row: cost of inserting into empty string
  for (let j = 0; j <= a.length; j++) {
    matrix[0]![j] = j;
  }

  // Fill in the rest of the matrix
  for (let i = 1; i <= b.length; i++) {
    for (let j = 1; j <= a.length; j++) {
      const cost = a[j - 1] === b[i - 1] ? 0 : 1;
      matrix[i]![j] = Math.min(
        matrix[i - 1]![j]! + 1,      // Delete from b
        matrix[i]![j - 1]! + 1,      // Insert into a
        matrix[i - 1]![j - 1]! + cost,  // Substitute
      );
    }
  }

  return matrix[b.length]![a.length]!;
};
```

Examples:

| String A | String B | Distance |
|----------|----------|----------|
| `length` | `lenght` | 1 (swap h/t) |
| `map`    | `mop`    | 1 (substitute) |
| `filter` | `flter`  | 1 (delete i) |
| `hello`  | `world`  | 4 |

### Finding Similar Names

```typescript
export const findSimilarNames = (
  name: string,
  candidates: Iterable<string>,
  maxDistance = 2,
  maxSuggestions = 3,
): string[] => {
  const scored: Array<[string, number]> = [];

  for (const candidate of candidates) {
    if (candidate === name) continue;  // Skip exact match

    // Case-insensitive comparison
    const dist = levenshteinDistance(
      name.toLowerCase(),
      candidate.toLowerCase()
    );

    if (dist <= maxDistance) {
      scored.push([candidate, dist]);
    }
  }

  // Sort by distance, take best matches
  return scored
    .sort((a, b) => a[1] - b[1])
    .slice(0, maxSuggestions)
    .map(([s]) => s);
};
```

The algorithm:

1. **Compare with each candidate**: Calculate edit distance
2. **Filter by threshold**: Only keep candidates within 2 edits
3. **Sort by distance**: Best matches first
4. **Limit results**: At most 3 suggestions

### Why Case-Insensitive?

Users often make capitalization mistakes:

```
-- User wrote:
Map fn list

-- Suggests:
= did you mean: map?
```

Case-insensitive matching catches these errors while still showing the correct capitalization.

---

## Diagnostic Collection Pattern

Throughout the compiler, diagnostics are collected rather than thrown:

```typescript
// In the parser
const parse = (source: string): ParseResult => {
  const diagnostics: Diagnostic[] = [];

  // ... parsing logic ...

  if (unexpectedToken) {
    diagnostics.push(error(
      token.start,
      token.end,
      `Unexpected token: ${tokenText}`
    ));
    // Continue parsing for error recovery
  }

  return { program, diagnostics };
};

// In the type checker
const check = (expr: Expr): CheckResult => {
  const diagnostics: Diagnostic[] = [];

  // ... type checking logic ...

  if (!unifyResult.success) {
    diagnostics.push(typeMismatch(
      expr.span?.start ?? 0,
      expr.span?.end ?? 0,
      typeToString(expected),
      typeToString(actual),
      "function application"
    ));
    // Continue with error type to find more errors
  }

  return { type, diagnostics };
};
```

Benefits of this approach:

1. **Multiple errors**: Users see all errors at once
2. **Error recovery**: Parsing/checking continues past errors
3. **Composable**: Each phase returns its diagnostics
4. **Testable**: Diagnostics can be inspected in tests

---

## Displaying Diagnostics

The `printDiagnostics` function formats errors nicely:

```typescript
const printDiagnostics = (
  diagnostics: readonly Diagnostic[],
  source: string,
  filename: string,
): void => {
  for (const diag of diagnostics) {
    // Convert byte offset to line/column
    const { line, col } = offsetToLineCol(source, diag.start);
    const lineContent = getLine(source, line);

    // Header with severity and message
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

### Example Output

```
error: Type mismatch in if expression
  --> example.alg:5:15
   |
 5 | if true then 1 else "hello"
   |               ^^^^^^^^^^^^^
   |
   = expected: number
   =   actual: string

error: Unknown variable: lenght
  --> example.alg:8:1
   |
 8 | lenght list
   | ^^^^^^
   |
   = did you mean: length?

error: Non-exhaustive patterns
  --> example.alg:12:1
   |
12 | match maybe with
   | ^^^^^^^^^^^^^^^^^
   |
   = missing: Just _
```

---

## Converting Offsets to Positions

Source positions are stored as byte offsets (faster to work with), but displayed as line/column:

```typescript
const offsetToLineCol = (source: string, offset: number): { line: number; col: number } => {
  let line = 1;
  let lineStart = 0;

  for (let i = 0; i < offset && i < source.length; i++) {
    if (source[i] === "\n") {
      line++;
      lineStart = i + 1;
    }
  }

  return { line, col: offset - lineStart + 1 };
};

const getLine = (source: string, lineNum: number): string => {
  const lines = source.split("\n");
  return lines[lineNum - 1] ?? "";
};
```

---

## Integration with Phases

Each compiler phase produces diagnostics:

### Lexer

```typescript
// Invalid character
diagnostics.push(error(pos, pos + 1, `Invalid character: ${char}`));
```

### Parser

```typescript
// Missing expression
diagnostics.push(error(start, end, "Expected expression"));

// Unclosed delimiter
diagnostics.push(error(start, end, "Unclosed parenthesis"));
```

### Binder

```typescript
// Unbound variable
const suggestions = findSimilarNames(name, scope.keys());
diagnostics.push(unboundVariable(span.start, span.end, name, suggestions));
```

### Type Checker

```typescript
// Type mismatch
diagnostics.push(typeMismatch(
  span.start, span.end,
  typeToString(expected),
  typeToString(actual),
  "pattern"
));

// Infinite type
diagnostics.push(error(
  span.start, span.end,
  `Cannot construct infinite type: ${v} = ${typeToString(t)}`
));

// Non-exhaustive match
diagnostics.push({
  start: matchSpan.start,
  end: matchSpan.end,
  message: "Non-exhaustive patterns",
  severity: "error",
  kind: "non-exhaustive",
  notes: [`missing: ${missingPatterns.join(", ")}`],
});
```

---

## LSP Integration

The LSP server converts internal diagnostics to LSP format:

```typescript
const convertDiagnostic = (source: string, diag: Diagnostic): LspDiagnostic => {
  const severity: DiagnosticSeverity =
    diag.severity === "error" ? 1
    : diag.severity === "warning" ? 2
    : 3;

  return {
    range: spanToRange(source, { start: diag.start, end: diag.end }),
    message: diag.message,
    severity,
    source: "algow",
  };
};
```

The LSP specification uses numeric severity codes:
- 1 = Error
- 2 = Warning
- 3 = Information
- 4 = Hint

---

## Summary

The diagnostic system provides:

1. **Structured diagnostics** with severity, kind, and rich context
2. **Multiple errors per run** through collection rather than throwing
3. **Helpful suggestions** using Levenshtein distance
4. **Pretty printing** with source context and underlines
5. **LSP compatibility** through simple conversion

The next chapter covers the Language Server Protocol integration.
