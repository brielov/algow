# Chapter 6: Name Resolution

Before type checking, we need to know what each name refers to. The **binder** walks the AST and connects every variable use to its definition. This process is called **name resolution** or **binding**.

---

## Why Name Resolution?

Consider this code:

```
let x = 1 in
let y = x + 1 in
x + y
```

When we see `x` in `x + 1`, which `x` do we mean? The one defined on line 1. But how does the type checker know that?

The binder creates a **symbol table** that maps each variable reference to its definition. Later stages can look up "where was this variable defined?" without re-analyzing scope.

---

## What the Binder Produces

The binder produces a `SymbolTable`:

```typescript
/**
 * A definition is where a name is introduced.
 */
export type Definition = {
  readonly name: string;
  readonly span: Span;  // Where in source it's defined
  readonly kind: DefinitionKind;
};

export type DefinitionKind =
  | "variable"     // let binding
  | "parameter"    // lambda parameter or pattern binding
  | "constructor"  // data constructor (Just, Nothing)
  | "type";        // type name from data declaration

/**
 * A reference is a usage of a name.
 */
export type Reference = {
  readonly name: string;
  readonly span: Span;  // Where in source it's used
  readonly definition: Definition | null;  // null if unbound
};

/**
 * The symbol table stores all definitions and references.
 */
export type SymbolTable = {
  readonly definitions: readonly Definition[];
  readonly references: readonly Reference[];
};
```

For each name in the program:

- **Definitions** record where names are introduced
- **References** record where names are used, linked to their definitions

---

## Scoping Rules

Names have **scope**—the region where they're visible:

### Let Bindings

In `let x = v in body`, `x` is in scope only for `body`, not for `v`:

```
let x = x + 1 in ...
        ^
        Error! x is not yet defined here
```

```typescript
const bindLet = (ctx: BindContext, expr: Let): void => {
  // First bind the value (name NOT yet in scope)
  bindExpr(ctx, expr.value);

  // Add definition (now name is in scope)
  addDefinition(ctx, expr.name, expr.nameSpan, "variable");

  // Bind body (name is in scope)
  bindExpr(ctx, expr.body);

  // Pop scope (name no longer in scope after body)
  popScope(ctx, expr.name);
};
```

### Recursive Let

In `let rec f = v in body`, `f` IS in scope for `v`—enabling recursion:

```
let rec fact = fn n => if n == 0 then 1 else n * fact (n - 1) in ...
                                                 ^^^^
                                                 OK! fact is in scope
```

```typescript
const bindLetRec = (ctx: BindContext, expr: LetRec): void => {
  // Add ALL definitions FIRST (before binding any values)
  for (const binding of expr.bindings) {
    addDefinition(ctx, binding.name, binding.nameSpan, "variable");
  }

  // Now bind all values (all names are in scope)
  for (const binding of expr.bindings) {
    bindExpr(ctx, binding.value);
  }

  // Bind body
  bindExpr(ctx, expr.body);

  // Pop all scopes
  for (const binding of expr.bindings) {
    popScope(ctx, binding.name);
  }
};
```

This also enables mutual recursion:

```
let rec isEven n = if n == 0 then true else isOdd (n - 1)
and isOdd n = if n == 0 then false else isEven (n - 1)
in ...
```

Both `isEven` and `isOdd` are in scope for both definitions.

### Lambda Parameters

Parameters are in scope for the function body:

```
fn x => x + 1
        ^
        x is in scope here
```

```typescript
const bindAbs = (ctx: BindContext, expr: Abs): void => {
  // Add parameter definition
  addDefinition(ctx, expr.param, expr.paramSpan, "parameter");

  // Bind body (parameter is in scope)
  bindExpr(ctx, expr.body);

  // Pop scope
  popScope(ctx, expr.param);
};
```

### Pattern Bindings

Patterns in `match` bind variables for their case body:

```
match xs with
| Cons x rest => x + length rest
                 ^        ^^^^
                 Both in scope for this arm only
| Nil => 0
end
```

```typescript
const bindMatch = (ctx: BindContext, expr: Match): void => {
  // Bind the scrutinee (the value being matched)
  bindExpr(ctx, expr.expr);

  // Bind each case
  for (const case_ of expr.cases) {
    // Collect pattern bindings
    const bindings = bindPattern(ctx, case_.pattern);

    // Bind case body with pattern bindings in scope
    bindExpr(ctx, case_.body);

    // Pop all pattern bindings
    for (const name of bindings) {
      popScope(ctx, name);
    }
  }
};
```

---

## Shadowing

An inner scope can redefine a name from an outer scope:

```
let x = 1 in
let x = 2 in   -- Inner x shadows outer x
x              -- Refers to inner x (value: 2)
```

We implement this with a **scope stack** per name:

```typescript
type BindContext = {
  /** Maps name to stack of definitions */
  readonly scope: Map<string, Definition[]>;
  // ...
};

const addDefinition = (ctx: BindContext, name: string, span: Span, kind: DefinitionKind): Definition => {
  const def: Definition = { name, span, kind };

  // Push onto scope stack for this name
  const stack = ctx.scope.get(name);
  if (stack) {
    stack.push(def);  // Add to existing stack
  } else {
    ctx.scope.set(name, [def]);  // Create new stack
  }

  return def;
};

const popScope = (ctx: BindContext, name: string): void => {
  const stack = ctx.scope.get(name);
  if (stack && stack.length > 0) {
    stack.pop();  // Remove most recent definition
  }
};
```

When we look up a name, we take the top of the stack (most recent definition):

```typescript
const addReference = (ctx: BindContext, name: string, span: Span): Reference => {
  const stack = ctx.scope.get(name);
  const definition = stack && stack.length > 0
    ? stack[stack.length - 1]!  // Most recent definition
    : null;  // Not found

  const ref: Reference = { name, span, definition };
  ctx.references.push(ref);

  // Report error if unbound
  if (!definition) {
    const suggestions = findSimilarNames(name, ctx.scope.keys());
    ctx.diagnostics.push(unboundVariable(span.start, span.end, name, suggestions));
  }

  return ref;
};
```

---

## Pattern Binding

Patterns can bind multiple variables:

```typescript
const bindPattern = (ctx: BindContext, pattern: Pattern): string[] => {
  switch (pattern.kind) {
    // Variable pattern: binds the name
    case "PVar": {
      addDefinition(ctx, pattern.name, pattern.span, "parameter");
      return [pattern.name];  // Return bound name
    }

    // Wildcard: matches anything, binds nothing
    case "PWildcard":
      return [];

    // Constructor pattern: reference to constructor + nested patterns
    case "PCon": {
      // The constructor name is a REFERENCE (not a definition)
      addReference(ctx, pattern.name, pattern.nameSpan);

      // Bind nested argument patterns
      const bindings: string[] = [];
      for (const arg of pattern.args) {
        bindings.push(...bindPattern(ctx, arg));
      }
      return bindings;
    }

    // Literal: no bindings
    case "PLit":
      return [];

    // Tuple: bind each element pattern
    case "PTuple": {
      const bindings: string[] = [];
      for (const element of pattern.elements) {
        bindings.push(...bindPattern(ctx, element));
      }
      return bindings;
    }

    // Record: bind each field pattern
    case "PRecord": {
      const bindings: string[] = [];
      for (const field of pattern.fields) {
        bindings.push(...bindPattern(ctx, field.pattern));
      }
      return bindings;
    }

    // As-pattern: bind the as-name AND the inner pattern
    case "PAs": {
      addDefinition(ctx, pattern.name, pattern.nameSpan, "parameter");
      const innerBindings = bindPattern(ctx, pattern.pattern);
      return [pattern.name, ...innerBindings];
    }

    // Or-pattern: all alternatives bind the same variables
    case "POr": {
      // Just bind the first alternative (type checker validates they match)
      if (pattern.alternatives.length > 0) {
        return bindPattern(ctx, pattern.alternatives[0]!);
      }
      return [];
    }
  }
};
```

Example:

```
match pair with
| (x, (y, z)) as whole => ...
```

Binds: `x`, `y`, `z`, `whole` (four variables)

---

## Constructor References

Constructor names (like `Just`, `Cons`, `Nothing`) are references, not definitions. Their definitions come from `data` declarations:

```
data Maybe a = Nothing | Just a
               ^^^^^^^   ^^^^
               These are constructor definitions
```

The binder pre-populates constructors in scope:

```typescript
export const bindWithConstructors = (
  constructorNames: readonly string[],
  expr: Expr,
): BindResult => {
  const ctx = createContext();

  // Add constructors to scope before binding
  for (const name of constructorNames) {
    addDefinition(ctx, name, { start: 0, end: 0 }, "constructor");
  }

  bindExpr(ctx, expr);
  return { symbols: finalize(ctx), diagnostics: ctx.diagnostics };
};
```

---

## Error Messages with Suggestions

When a name isn't found, we suggest similar names:

```
let x = 1 in
y + 1
^
Error: Unbound variable 'y'. Did you mean 'x'?
```

The suggestions use Levenshtein distance (edit distance):

```typescript
const addReference = (ctx: BindContext, name: string, span: Span): Reference => {
  const stack = ctx.scope.get(name);
  const definition = stack && stack.length > 0 ? stack[stack.length - 1] : null;

  if (!definition) {
    // Find similar names for suggestions
    const suggestions = findSimilarNames(name, ctx.scope.keys());
    ctx.diagnostics.push(
      unboundVariable(span.start, span.end, name, suggestions)
    );
  }

  // ...
};
```

---

## The Complete Binding Algorithm

```typescript
const bindExpr = (ctx: BindContext, expr: Expr): void => {
  switch (expr.kind) {
    // Literals: no bindings
    case "Num":
    case "Bool":
    case "Str":
      break;

    // Variable: add reference
    case "Var":
      addReference(ctx, expr.name, expr.span);
      break;

    // Let: value first (no name), then name, then body
    case "Let":
      bindExpr(ctx, expr.value);
      addDefinition(ctx, expr.name, expr.nameSpan, "variable");
      bindExpr(ctx, expr.body);
      popScope(ctx, expr.name);
      break;

    // Let rec: add all names first, then all values, then body
    case "LetRec":
      for (const b of expr.bindings) {
        addDefinition(ctx, b.name, b.nameSpan, "variable");
      }
      for (const b of expr.bindings) {
        bindExpr(ctx, b.value);
      }
      bindExpr(ctx, expr.body);
      for (const b of expr.bindings) {
        popScope(ctx, b.name);
      }
      break;

    // Lambda: parameter, then body
    case "Abs":
      addDefinition(ctx, expr.param, expr.paramSpan, "parameter");
      bindExpr(ctx, expr.body);
      popScope(ctx, expr.param);
      break;

    // Application: bind both sides
    case "App":
      bindExpr(ctx, expr.func);
      bindExpr(ctx, expr.param);
      break;

    // Conditional: bind all branches
    case "If":
      bindExpr(ctx, expr.cond);
      bindExpr(ctx, expr.then);
      bindExpr(ctx, expr.else);
      break;

    // Binary operator: bind both operands
    case "BinOp":
      bindExpr(ctx, expr.left);
      bindExpr(ctx, expr.right);
      break;

    // Tuple: bind all elements
    case "Tuple":
      for (const element of expr.elements) {
        bindExpr(ctx, element);
      }
      break;

    // Record: bind all field values
    case "Record":
      for (const field of expr.fields) {
        bindExpr(ctx, field.value);
      }
      break;

    // Field access: bind the record
    case "FieldAccess":
      bindExpr(ctx, expr.record);
      break;

    // Match: bind scrutinee, then each case
    case "Match":
      bindExpr(ctx, expr.expr);
      for (const case_ of expr.cases) {
        const bindings = bindPattern(ctx, case_.pattern);
        bindExpr(ctx, case_.body);
        for (const name of bindings) {
          popScope(ctx, name);
        }
      }
      break;
  }
};
```

---

## LSP Features

The symbol table enables IDE features:

### Go to Definition

```typescript
export const findReferenceAt = (table: SymbolTable, position: number): Reference | null => {
  for (const ref of table.references) {
    if (position >= ref.span.start && position < ref.span.end) {
      return ref;
    }
  }
  return null;
};

// When user clicks "Go to definition" on a variable:
const ref = findReferenceAt(symbolTable, cursorPosition);
if (ref && ref.definition) {
  // Jump to ref.definition.span
}
```

### Find All References

```typescript
export const findReferences = (table: SymbolTable, definition: Definition): Reference[] => {
  return table.references.filter(ref => ref.definition === definition);
};
```

### Rename

To rename a variable:

1. Find the definition at cursor
2. Find all references to that definition
3. Replace text at all those spans

---

## Summary

The binder connects names to definitions:

1. **Scope rules** determine when names are visible
2. **Shadowing** allows inner scopes to redefine names
3. **Scope stacks** track which definition is current
4. **Pattern binding** extracts variables from patterns
5. **Symbol tables** enable LSP features

After binding, we know what every name refers to. The type checker uses this to assign types to definitions and propagate them to references.

This completes Part 1: Frontend. We've transformed source text into a structured AST with resolved names. Next, we'll dive into the type system—the heart of the compiler.
