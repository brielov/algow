# Algow Compiler Refactoring Plan

This document outlines the architectural issues in the Algow compiler and provides a detailed plan to bring the codebase to production quality. The goal is to transform this from a working prototype into a canonical, textbook implementation of a compiler pipeline.

---

## Success Criteria

### The Standard

After this refactor, the codebase must look like it was developed by the combined efforts of:

- The MIT CSAIL programming languages group
- The Berkeley PLSE lab
- Rob Pike (simplicity, clarity, no cleverness)
- Linus Torvalds (uncompromising quality, zero tolerance for hacks)
- Simon Peyton Jones (principled compiler architecture)

This is not hyperbole. This is the bar. Anything less is failure.

### Non-Negotiable Principles

**1. No Shortcuts. Ever.**

If the right solution takes 3 days and a hack takes 30 minutes, we take 3 days. Shortcuts compound. One hack justifies another. Before you know it, you have a codebase held together by duct tape and prayers.

**2. No Hacks. Ever.**

A hack is any code that:

- Works around a problem instead of fixing it
- Has a comment like "TODO: fix this properly later"
- Makes you feel slightly ashamed when you write it
- Requires explaining "why it's done this weird way"

If you find yourself writing a hack, STOP. Go back. Fix the underlying problem.

**3. No Workarounds. Ever.**

A workaround is a hack's cousin. It's code that exists because something else is broken. Instead of writing a workaround, fix the thing that's broken.

Example of a workaround we're removing:

```typescript
// 300 lines of offsetSpan, offsetExpr, offsetPattern...
// These exist because spans don't know their file.
// Workaround: manually adjust every span for multi-file compilation.
// Fix: spans know their file. No adjustment needed.
```

**4. Follow Canonical Patterns**

We invent nothing. Every problem we face has been solved before by smarter people. Our job is to:

- Identify the established solution
- Implement it correctly
- Document why we chose it

Examples:

- Type inference → Algorithm W (Damas-Milner). Not our invention.
- ANF transformation → Standard technique from literature. Not our invention.
- Pattern exhaustiveness → Standard algorithm. Not our invention.
- Tail call optimization → Standard transformation. Not our invention.

If you're tempted to invent a "clever" solution, resist. Find the canonical approach.

**5. The Type System Is The Specification**

If the type says `span: Span`, then span is always present. Period.

If the type says `span?: Span`, then span might be undefined, and every consumer must handle that case.

We do not:

- Use `!` to assert something is non-null when the type says it could be
- Use `as` to cast away type errors
- Ignore TypeScript errors with `@ts-ignore`

If the type is wrong, fix the type. If the implementation doesn't match the type, fix the implementation.

**6. One Way To Do Things**

There should be one obvious way to:

- Look up a type for a node
- Get the span of any AST element
- Find the definition of a name
- Report an error with location

Not two ways. Not "it depends." One way.

### Quality Checklist

Before considering any phase complete, verify:

- [ ] **No optional types where values are always present.** If something is always there, the type reflects that.

- [ ] **No type assertions (`as`, `!`).** If you need to assert, the types are wrong. Fix the types.

- [ ] **No `any` types.** Everything has a precise type.

- [ ] **No comments explaining workarounds.** If code needs a comment explaining why it's weird, it shouldn't be weird.

- [ ] **No TODO comments.** Either do it now or create an issue. TODOs in code are lies we tell ourselves.

- [ ] **No dead code.** If it's not used, delete it. Version control remembers.

- [ ] **No duplicated logic.** If two places do the same thing, extract it. DRY without being clever.

- [ ] **Single responsibility.** Each function does one thing. Each file has one purpose. If you can't name it clearly, it's doing too much.

- [ ] **Consistent naming.** Same concepts use same names everywhere. A "span" is always a "span", not sometimes "location" or "range" or "position".

- [ ] **Tests pass.** Not "mostly pass" or "pass with known failures." Pass.

### What "Done" Looks Like

A phase is done when:

1. All checklist items for that phase are complete
2. All quality checklist items pass
3. The test suite passes with no skipped tests
4. A cold read of the changed code reveals no surprises
5. You would be proud to show this code in a job interview
6. You would be comfortable if Simon Peyton Jones reviewed it

### What "Done" Does NOT Look Like

- "It works but I'll clean it up later" → Not done
- "The types are a bit loose but it's fine" → Not done
- "There's a small hack but it's just temporary" → Not done
- "I had to add a workaround for edge case X" → Not done
- "The tests pass if you ignore these two" → Not done

### The Refusal Principle

It is better to:

- Leave a feature unimplemented than implement it with hacks
- Have a smaller codebase that's correct than a larger one with workarounds
- Take longer to do it right than ship something broken quickly
- Say "I don't know how to do this properly yet" than pretend a hack is acceptable

If the right way isn't clear, STOP. Research. Ask. Think. Do not proceed with a hack while you "figure out the right way later." Later never comes.

---

## Table of Contents

1. [Core Problems](#core-problems)
2. [Detailed Issue Analysis](#detailed-issue-analysis)
3. [Refactoring Phases](#refactoring-phases)
4. [Implementation Checklist](#implementation-checklist)
5. [File-by-File Changes](#file-by-file-changes)

---

## Core Problems

### The Fundamental Issue: Spans Are Optional

Throughout the codebase, spans are defined as optional:

```typescript
// Current (bad)
type SExpr = {
  kind: "SVar";
  name: string;
  span?: Span; // Why optional?
};
```

**There is no valid reason for spans to be optional.** Every token in the source has a location. Every AST node is constructed from tokens. Therefore, every node has a span. Period.

The only scenario where a span might be "missing" is for compiler-generated nodes (e.g., during desugaring). But even then, these nodes should inherit or synthesize spans from their source nodes—not have `undefined` spans.

**Making spans optional has cascading consequences:**

- Every consumer must handle `undefined` with `?.` or `!`
- LSP features become unreliable ("sometimes we have position info, sometimes not")
- Debugging becomes harder ("where did this node come from?")
- The type system can't help catch missing span bugs

### The Second Fundamental Issue: Names Are Not Nodes

Names are represented inconsistently:

```typescript
// Surface AST: name is a string, span is separate
type SDeclLet = {
  name: string;
  nameSpan?: Span; // Separate from name!
  // ...
};

// Core AST: name has id but no span at all
type Name = {
  id: number;
  original: string;
  // No span!
};
```

A name IS a node. It has:

- A string value (what it says)
- A location (where it appears)
- An identity (unique ID for the binding it refers to)

These three things should be bundled together, always.

---

## Detailed Issue Analysis

### 1. Name/NameSpan Separation (Critical)

**Current State:**

In `src/surface.ts`, bindings have `name: string` and `nameSpan?: Span` as siblings:

```typescript
export type SDeclLet = {
  readonly kind: "SDeclLet";
  readonly name: string;
  readonly nameSpan?: Span;
  readonly value: SExpr;
  readonly span?: Span;
};

export type SLet = {
  readonly kind: "SLet";
  readonly name: string;
  readonly nameSpan?: Span;
  readonly value: SExpr;
  readonly body: SExpr;
  readonly span?: Span;
};
```

In `src/core.ts`, the `Name` type has an ID but no span:

```typescript
export type Name = {
  readonly id: number;
  readonly original: string;
};
```

**Why This Is Wrong:**

1. **Separation of concerns violation**: The name string and its location are intrinsically linked. Separating them means you can have a name without knowing where it came from.

2. **Inconsistent tracking**: Some nodes have `nameSpan`, others don't. The parser has to remember to set both `name` and `nameSpan` separately.

3. **Lost information in Core AST**: When we desugar Surface → Core, we lose span information because `Name` doesn't have a span field.

4. **LSP workarounds**: To support "go to definition" and "hover", we had to create separate tracking structures (`typeMap`, `exprTypeMap`) indexed by position because names don't carry their own positions.

**What Should Exist:**

```typescript
// A Name is a node with all three properties
export type Name = {
  readonly id: number; // Unique identifier (assigned during resolution)
  readonly text: string; // The actual name string
  readonly span: Span; // Where this name appears in source
};

// Before resolution, we have UnresolvedName (no id yet)
export type UnresolvedName = {
  readonly text: string;
  readonly span: Span;
};
```

### 2. Optional Spans Everywhere (Critical)

**Current State:**

Every AST node type has `span?: Span`:

```typescript
// src/surface.ts
export type SVar = {
  readonly kind: "SVar";
  readonly name: string;
  readonly span?: Span;
};

export type SApp = {
  readonly kind: "SApp";
  readonly func: SExpr;
  readonly arg: SExpr;
  readonly span?: Span;
};
// ... every single node type
```

**Why This Is Wrong:**

1. **Parsing always produces spans**: The parser reads tokens which have positions. When it constructs nodes, it knows exactly where they start and end. There's no reason for the span to be missing.

2. **Null checks everywhere**: Code that uses spans must constantly check for `undefined`:

   ```typescript
   if (expr.span) {
     const range = spanToRange(lineIndex, expr.span);
   }
   // What do we do if there's no span? Crash? Return null? Guess?
   ```

3. **Type system failure**: TypeScript can't help us catch "oops, I forgot to set the span" bugs because `undefined` is always valid.

**What Should Exist:**

```typescript
// Span is ALWAYS present
export type SVar = {
  readonly kind: "SVar";
  readonly name: UnresolvedName; // Name has span built-in
  readonly span: Span; // Node's overall span (required)
};
```

For compiler-generated nodes (during desugaring), use a synthetic span that points to the source construct:

```typescript
// When desugaring `if c then a else b` into a match:
// The generated match node gets the span of the original if-expression
const matchSpan = ifExpr.span; // Always available, always valid
```

### 3. Span Offsetting for Multi-File Compilation (High)

**Current State:**

`src/compile.ts` contains 300+ lines of span offsetting functions:

```typescript
const offsetSpan = (span: Span | undefined, offset: number): Span | undefined => {
  if (!span) return undefined;
  return { start: span.start + offset, end: span.end + offset };
};

const offsetType = (type: SType, offset: number): SType => {
  /* 30 lines */
};
const offsetPattern = (pattern: SPattern, offset: number): SPattern => {
  /* 50 lines */
};
const offsetCase = (c: SCase, offset: number): SCase => {
  /* ... */
};
const offsetDoStmt = (stmt: SDoStmt, offset: number): SDoStmt => {
  /* ... */
};
const offsetExpr = (expr: SExpr, offset: number): SExpr => {
  /* 120 lines */
};
const offsetDecl = (decl: SDecl, offset: number): SDecl => {
  /* 50 lines */
};
const offsetProgram = (program: SProgram, offset: number): SProgram => {
  /* ... */
};
```

**Why This Is Wrong:**

1. **Manual traversal for every node type**: Every time we add a new AST node kind, we must remember to add offset handling. Easy to forget, hard to test.

2. **Repetitive boilerplate**: These functions do almost identical things—recursively walk the tree and add an offset to spans.

3. **Indicates broken abstraction**: If we need to manually adjust positions for multi-file compilation, our position model is wrong.

**What Should Exist:**

Two options:

**Option A: File-relative spans with file ID**

```typescript
type Span = {
  readonly fileId: number; // Which file
  readonly start: number; // Offset within that file
  readonly end: number;
};
```

No offsetting needed—each file's spans are independent.

**Option B: Generic AST traversal**

```typescript
// One function to rule them all
const mapSpans = <T extends ASTNode>(node: T, f: (span: Span) => Span): T => {
  // Generic traversal that handles all node types
};

// Usage
const adjusted = mapSpans(program, (span) => ({
  ...span,
  start: span.start + offset,
  end: span.end + offset,
}));
```

Option A is cleaner and eliminates the problem at its source.

### 4. Type Information Scattered Across Multiple Maps (High)

**Current State:**

Type information lives in multiple places:

```typescript
// src/checker.ts
export type CheckOutput = {
  typeEnv: TypeEnv; // Map<string, Scheme> - name to type
  typeMap: Map<number, Scheme>; // Map<nameId, Scheme>
  exprTypeMap: Map<number, Type>; // Map<globalOffset, Type>
  tupleProjections: Map<number, Type>; // Special case for .0, .1 access
  constructorRegistry: ConstructorRegistry;
  // ...
};
```

**Why This Is Wrong:**

1. **No single source of truth**: To find "what is the type at position X?", you must:
   - Check if it's a name reference → look in `typeMap` by name ID
   - Check if it's an expression → look in `exprTypeMap` by position
   - Check if it's a tuple projection → look in `tupleProjections`
   - Fall back to inferring from context

2. **Position-based indexing is fragile**: `exprTypeMap` is keyed by global byte offset. This breaks if:
   - File order changes
   - We want incremental compilation
   - We forget to offset positions correctly

3. **LSP code is complex**: `src/lsp/analysis.ts:236-270` has to check multiple sources with fallbacks.

**What Should Exist:**

Every AST node should have a unique ID assigned during parsing:

```typescript
type NodeId = number;

type SExpr = {
  readonly nodeId: NodeId; // Unique across entire compilation
  readonly kind: "SVar";
  readonly name: UnresolvedName;
  readonly span: Span;
};
```

Type information is stored in one map:

```typescript
type TypeMap = Map<NodeId, Type>;
```

Simple. One lookup. No fallbacks.

### 5. LSP Symbol Table Bolt-On Design (High)

**Current State:**

The resolver optionally builds a symbol table:

```typescript
// src/resolve.ts
export const resolveProgram = (
  program: CProgram,
  env: Env = new Map(),
  constructorTypes: ConstructorTypes = new Set(),
  knownTypeNames: Set<string> = new Set(),
  fileRegistry?: FileRegistry,  // Optional! Only for LSP
): ResolveResult => {
  // ...
  // Symbol table only built if fileRegistry provided
  const symbolTable = fileRegistry ? createSymbolTableBuilder() : null;
```

**Why This Is Wrong:**

1. **Two code paths**: The resolver behaves differently with/without LSP mode. This means bugs might only appear in one mode.

2. **Symbol information is duplicated**: The resolver tracks bindings in `Env`, then separately in `SymbolTable`. They can get out of sync.

3. **Type enrichment is a hack**: After type checking, we call `enrichWithTypes()` to add type info to the symbol table. This is a post-hoc patch, not integrated design.

**What Should Exist:**

The resolver ALWAYS builds a symbol table. For CLI compilation, we just don't use it. The cost is negligible (it's just storing information we already compute), and it guarantees consistency.

```typescript
export const resolveProgram = (
  program: CProgram,
  fileRegistry: FileRegistry, // Required, not optional
): ResolveResult => {
  const symbolTable = createSymbolTable();
  // Always track symbols
};
```

### 6. Core AST Loses Span Information (High)

**Current State:**

The Core AST (`src/core.ts`) has no span information:

```typescript
export type CExpr =
  | CVar
  | CLit
  | CApp
  | CAbs
  | CLet
  | CLetRec
  | CMatch
  | CCon
  | CTuple
  | CRecord
  | CRecordUpdate
  | CField;

// None of these have spans!
export type CVar = {
  readonly kind: "CVar";
  readonly name: Name; // Name has no span either
};
```

**Why This Is Wrong:**

1. **Error messages in type checker reference positions**: When we report "type mismatch at line 5", we need to know where the expression is. Without spans in Core, we lose this.

2. **Desugaring erases origin**: When `if c then a else b` becomes a match expression, we lose the ability to point back to the `if` keyword.

3. **Forces position-based hacks**: The checker uses `exprTypeMap` keyed by global offset because nodes don't have their own identity/position.

**What Should Exist:**

Core AST should preserve spans:

```typescript
export type CExpr =
  | { kind: "CVar"; name: Name; span: Span }
  | { kind: "CLit"; value: Literal; span: Span }
  | { kind: "CApp"; func: CExpr; arg: CExpr; span: Span };
// ...
```

Alternatively, use node IDs that map back to spans:

```typescript
type NodeId = number;
type CExpr = { kind: "CVar"; nodeId: NodeId; name: Name };
// ...
// Spans stored in: Map<NodeId, Span>
```

### 7. Checker File Is Too Large (Medium)

**Current State:**

`src/checker.ts` is 1817 lines containing:

- Type context and environment management
- Unification algorithm
- Type inference for all expression types
- Pattern type inference
- Exhaustiveness checking
- SCC analysis for recursive bindings
- Type class constraint handling

**Why This Is Wrong:**

Single Responsibility Principle. A 1800-line file is hard to:

- Navigate
- Test in isolation
- Understand as a whole
- Modify safely

**What Should Exist:**

Split into focused modules:

```
src/checker/
├── index.ts          # Main API, ties everything together
├── context.ts        # CheckContext, TypeEnv management
├── unify.ts          # Unification algorithm
├── infer.ts          # Expression type inference
├── patterns.ts       # Pattern type inference
├── exhaustiveness.ts # Exhaustiveness checking
├── constraints.ts    # Type class constraint solving
└── scc.ts            # Strongly connected components analysis
```

### 8. Optimize.ts Is Too Large (Medium)

**Current State:**

`src/optimize.ts` is 2655 lines containing:

- Alpha renaming
- Constant folding
- Beta reduction
- Eta reduction
- Dead code elimination
- Tail call optimization
- Case-of-known-constructor
- Case-of-case transformation
- Let floating (inward and outward)
- Function inlining

**Why This Is Wrong:**

Same as above. Plus:

- Each optimization has its own environment type (`ExtendedEnv`, `InlineEnv`, `FunctionEnv`, `ConstEnv`)
- No clear pass interface
- Difficult to enable/disable individual optimizations
- Hard to add new optimizations

**What Should Exist:**

```
src/optimize/
├── index.ts           # Optimization pipeline orchestration
├── pass.ts            # Pass interface definition
├── traverse.ts        # Common AST traversal utilities
├── fold.ts            # Constant folding
├── beta.ts            # Beta reduction
├── eta.ts             # Eta reduction
├── dce.ts             # Dead code elimination
├── tco.ts             # Tail call optimization
├── case-of-known.ts   # Case-of-known-constructor
├── case-of-case.ts    # Case-of-case transformation
├── float.ts           # Let floating
└── inline.ts          # Function inlining
```

### 9. Code Generation Async IIFEs (Medium)

**Current State:**

Every match expression generates an async IIFE:

```typescript
// src/codegen.ts:418-534
const genMatch = (ctx: CodeGenContext, match: IR.IRMatch): string => {
  const lines: string[] = [];
  lines.push("(async () => {");
  // ... generate match body ...
  lines.push("})()");
  return "await " + lines.join("\n");
};
```

Generated code:

```javascript
const result = await (async () => {
  if (x === null) {
    return 0;
  } else {
    return x.h;
  }
})();
```

**Why This Is Wrong:**

1. **Bloated output**: Every match becomes a function call
2. **Performance overhead**: Creating and awaiting async functions has cost
3. **Harder to debug**: Stack traces show anonymous functions

**What Should Exist:**

Track whether we're in an expression or statement context:

```javascript
// In statement context, generate directly:
let result;
if (x === null) {
  result = 0;
} else {
  result = x.h;
}

// In expression context, use ternary when possible:
const result = x === null ? 0 : x.h;

// Only use IIFE when truly necessary (complex multi-branch in expression position)
```

### 10. Token Type Uses Tuples (Low)

**Current State:**

```typescript
// src/lexer.ts:45
export type Token = [TokenKind, number, number];
```

**Why This Is Wrong:**

```typescript
// Hard to read
const kind = token[0];
const start = token[1];
const end = token[2];

// vs

const { kind, start, end } = token;
```

**What Should Exist:**

```typescript
export type Token = {
  readonly kind: TokenKind;
  readonly start: number;
  readonly end: number;
};
```

---

## Refactoring Phases

### Phase 1: Non-Optional Spans

**Goal:** Make all spans required throughout the AST.

**Rationale:** This is foundational. Until spans are reliable, we can't build reliable LSP features or error messages.

**Changes:**

1. Change `span?: Span` to `span: Span` in all Surface AST types
2. Update parser to always provide spans (it already does, we just need to make types honest)
3. Add `span: Span` to Core AST types
4. Update desugaring to propagate/synthesize spans
5. Remove all `span?.` null checks

### Phase 2: Unified Name Type

**Goal:** Names are first-class nodes with text, span, and (after resolution) ID.

**Rationale:** This eliminates the name/nameSpan split and carries location through the pipeline.

**Changes:**

1. Define `UnresolvedName = { text: string; span: Span }`
2. Define `Name = { id: number; text: string; span: Span }`
3. Replace `name: string` + `nameSpan?: Span` with `name: UnresolvedName` in Surface AST
4. Replace `Name` in Core AST to include span
5. Update parser to create `UnresolvedName` nodes
6. Update desugarer and resolver accordingly
7. Remove all `nameSpan` fields

### Phase 3: File-Aware Spans

**Goal:** Spans know which file they belong to, eliminating manual offset adjustment.

**Rationale:** Multi-file compilation should be transparent, not require 300 lines of offsetting.

**Changes:**

1. Add `FileRegistry` that assigns IDs to files
2. Change `Span = { fileId: number; start: number; end: number }`
3. Remove all `offset*` functions from `compile.ts`
4. Update span-to-position conversion to use file registry

### Phase 4: Node IDs and Unified Type Map

**Goal:** Every AST node has a unique ID; type information is stored in one map.

**Rationale:** Simplifies type lookup and enables incremental compilation.

**Changes:**

1. Add `nodeId: number` to all AST node types
2. Create ID generator used during parsing
3. Replace `typeEnv`, `typeMap`, `exprTypeMap`, `tupleProjections` with single `TypeMap<NodeId, Type>`
4. Update checker to store types by node ID
5. Update LSP to use node ID for type lookup

### Phase 5: Integrated Symbol Table

**Goal:** Symbol table is always built, not optional.

**Rationale:** Eliminates dual code paths and ensures consistency.

**Changes:**

1. Make `fileRegistry` required parameter in resolver
2. Always build symbol table during resolution
3. Store type information directly in symbol table during checking
4. Remove `enrichWithTypes` post-processing

### Phase 6: Module Splitting

**Goal:** Break large files into focused modules.

**Rationale:** Improves maintainability, testability, and comprehension.

**Changes:**

1. Split `checker.ts` into `src/checker/` directory
2. Split `optimize.ts` into `src/optimize/` directory
3. Define clear interfaces between modules

### Phase 7: Code Generation Improvements

**Goal:** Generate cleaner JavaScript without unnecessary async IIFEs.

**Rationale:** Better output quality and runtime performance.

**Changes:**

1. Track expression vs statement context in codegen
2. Generate direct statements when in statement context
3. Use ternary operators for simple matches
4. Only use IIFE when necessary

---

## Implementation Checklist

### Phase 1: Non-Optional Spans

- [x] **1.1** Update `Span` definition to be always required
- [x] **1.2** Update all Surface AST types (`SExpr`, `SPattern`, `SType`, `SDecl`) to have `span: Span`
- [x] **1.3** Update parser to always set span (remove any `undefined` assignments)
- [x] **1.4** Add span field to all Core AST types (`CExpr`, `CPattern`, `CDecl`)
- [x] **1.5** Update desugarer to propagate spans from Surface to Core
- [x] **1.6** Update desugarer synthetic nodes to use source span
- [x] **1.7** Remove all `?.` optional chaining on span access throughout codebase
- [x] **1.8** Remove all `span && ...` conditional checks
- [x] **1.9** Update tests to provide spans in test AST construction
- [x] **1.10** Run full test suite and fix any issues

### Phase 2: Unified Name Type

- [x] **2.1** Define `UnresolvedName` type: `{ text: string; span: Span }`
- [x] **2.2** Define `ResolvedName` type: `{ id: number; text: string; span: Span }`
- [x] **2.3** Update `SVar` to use `name: UnresolvedName` instead of `name: string`
- [x] **2.4** Update `SLet` to use `name: UnresolvedName`, remove `nameSpan`
- [x] **2.5** Update `SLetRec` bindings to use `name: UnresolvedName`, remove `nameSpan`
- [x] **2.6** Update `SDeclLet` to use `name: UnresolvedName`, remove `nameSpan`
- [x] **2.7** Update `SDeclLetRec` bindings to use `name: UnresolvedName`, remove `nameSpan`
- [x] **2.8** Update `SDeclForeign` to use `name: UnresolvedName`, remove `nameSpan`
- [x] **2.9** Update `SAbs` params to use `UnresolvedName`
- [x] **2.10** Update `SPVar` to use `name: UnresolvedName`
- [x] **2.11** Update `SPAs` to use `name: UnresolvedName`
- [x] **2.12** Update `Name` in Core AST to include `span: Span`
- [x] **2.13** Update parser to create `UnresolvedName` nodes
- [x] **2.14** Update desugarer to convert `UnresolvedName` to `Name` (with placeholder id=-1)
- [x] **2.15** Update resolver to assign IDs and preserve spans in `Name`
- [x] **2.16** Update all code that accesses `name` string to use `name.text`
- [x] **2.17** Remove all references to `nameSpan` field
- [x] **2.18** Update formatter to use `name.text`
- [x] **2.19** Update LSP analysis to use `name.span` directly
- [x] **2.20** Run full test suite and fix any issues

### Phase 3: File-Aware Spans

- [x] **3.1** Update `Span` type to include `fileId: number`
- [x] **3.2** Move `FileRegistry` from LSP to core (it's needed by all spans now)
- [x] **3.3** Update parser to accept file ID and embed in all spans
- [x] **3.4** Update `compile.ts` to create file registry before parsing
- [x] **3.5** Remove `offsetSpan` function
- [x] **3.6** Remove `offsetType` function
- [x] **3.7** Remove `offsetPattern` function
- [x] **3.8** Remove `offsetCase` function
- [x] **3.9** Remove `offsetDoStmt` function
- [x] **3.10** Remove `offsetExpr` function
- [x] **3.11** Remove `offsetDecl` function
- [x] **3.12** Remove `offsetProgram` function
- [x] **3.13** Update `spanToRange` to use file registry for content lookup
- [x] **3.14** Update diagnostic printing to resolve file from span
- [x] **3.15** Update LSP position conversion to use file-aware spans
- [x] **3.16** Run full test suite and fix any issues

### Phase 4: Node IDs and Unified Type Map ✅

- [x] **4.1** Create `NodeId` type alias: `type NodeId = number`
- [x] **4.2** Create `NodeIdGenerator` with `next(): NodeId` method
- [x] **4.3** Add `nodeId: NodeId` to all Surface AST types
- [x] **4.4** Add `nodeId: NodeId` to all Core AST types
- [x] **4.5** Update parser to assign node IDs during parsing
- [x] **4.6** Update desugarer to assign new node IDs to synthetic nodes
- [x] **4.7** Create unified `nodeTypeMap = Map<NodeId, Type>` in checker
- [x] **4.8** Update checker to store types by node ID
- [x] **4.9** ~~Remove `typeMap`~~ (kept for backward compatibility with lower.ts)
- [x] **4.10** Remove `exprTypeMap: Map<number, Type>` from CheckOutput
- [x] **4.11** ~~Remove `tupleProjections`~~ (still used for deferred tuple projections)
- [x] **4.12** ~~Update LSP `getHoverAtPosition`~~ (uses symbolTable, not typeMap)
- [x] **4.13** Update lower.ts type lookup to use node IDs
- [x] **4.14** Run full test suite and fix any issues

### Phase 5: Integrated Symbol Table ✅

- [x] **5.1** Make `fileRegistry` required in `resolveProgram` signature
- [x] **5.2** Always create symbol table in resolver (remove conditional)
- [x] **5.3** Update `ResolveResult` to always include `symbolTableBuilder` (mutable)
- [x] **5.4** Move type storage into symbol table during checking
- [x] **5.5** Remove `enrichWithTypes` function (replaced by `freezeSymbolTableWithSubst`)
- [x] **5.6** Update `compileForLSP` to use integrated symbol table
- [x] **5.7** Update regular `compile` to pass file registry
- [x] **5.8** Ensure symbol table is populated even in CLI mode
- [x] **5.9** Run full test suite and fix any issues

### Phase 6: Module Splitting

- [x] **6.1** Create `src/checker/` directory
- [x] **6.2** Extract `CheckContext` type and functions to `src/checker/context.ts`
- [x] **6.3** Extract unification to `src/checker/unify.ts`
- [x] **6.4** Extract expression inference to `src/checker/infer.ts`
- [x] **6.5** Extract pattern inference to `src/checker/patterns.ts`
- [x] **6.6** Extract exhaustiveness checking to `src/checker/exhaustiveness.ts`
- [x] **6.7** Extract SCC analysis to `src/checker/scc.ts`
- [x] **6.8** Create `src/checker/index.ts` that re-exports public API
- [x] **6.9** Create `src/optimize/` directory
- [x] **6.10** ~~Define `Pass` interface in `src/optimize/pass.ts`~~ Created `src/optimize/types.ts` with shared types instead
- [x] **6.11** Extract constant folding to `src/optimize/fold.ts` (includes beta reduction, case-of-known, eta reduction)
- [x] **6.12** ~~Extract beta reduction to `src/optimize/beta.ts`~~ Combined in fold.ts
- [x] **6.13** Extract dead code elimination to `src/optimize/dce.ts`
- [x] **6.14** Extract tail call optimization to `src/optimize/tco.ts`
- [x] **6.15** ~~Extract case-of-known to `src/optimize/case-of-known.ts`~~ Combined in fold.ts
- [x] **6.16** Extract case-of-case to `src/optimize/case-of-case.ts`
- [x] **6.17** Extract let floating to `src/optimize/float.ts`
- [x] **6.18** Extract function inlining to `src/optimize/inline.ts`
- [x] **6.19** ~~Create common traversal utilities in `src/optimize/traverse.ts`~~ Created `src/optimize/types.ts` and `src/optimize/alpha.ts`
- [x] **6.20** Create `src/optimize/index.ts` that orchestrates passes
- [x] **6.21** Update imports throughout codebase
- [x] **6.22** Run full test suite and fix any issues

### Phase 7: Code Generation Improvements

- [x] **7.1** Add `context: "expr" | "stmt"` to `CodeGenContext`
- [ ] ~~**7.2** Update `genExpr` to track context~~ (not needed with IIFE approach)
- [ ] ~~**7.3** Implement direct statement generation for matches in statement context~~ (would require major changes)
- [x] **7.4** Implement ternary generation for simple two-branch matches (True/False and literal patterns)
- [x] **7.5** Only generate async IIFE when necessary (detect if body requires await)
- [ ] ~~**7.6** Consider making pure functions non-async~~ (requires tracking async-ness through call graph)
- [ ] **7.7** Add output quality tests comparing generated code (optional)
- [x] **7.8** Run full test suite and fix any issues

### Cleanup Tasks

- [ ] **C.1** Change `Token` from tuple to object type
- [ ] **C.2** Remove unnecessary type assertions in parser
- [ ] **C.3** Add stricter TypeScript settings (noUncheckedIndexedAccess, etc.)
- [ ] **C.4** Review and remove dead code
- [ ] **C.5** Ensure consistent naming conventions
- [ ] **C.6** Add missing JSDoc comments to public APIs

---

## File-by-File Changes

### `src/surface.ts`

**Current:**

- All nodes have `span?: Span`
- Names are strings with separate `nameSpan` fields

**After Phase 1:**

- All nodes have `span: Span`

**After Phase 2:**

- Names use `UnresolvedName` type with embedded span
- `nameSpan` fields removed

**After Phase 3:**

- `Span` includes `fileId`

**After Phase 4:**

- All nodes have `nodeId: NodeId`

### `src/core.ts`

**Current:**

- `Name` has `id` and `original` but no span
- No spans on any nodes

**After Phase 1:**

- All nodes have `span: Span`

**After Phase 2:**

- `Name` includes `span: Span`

**After Phase 4:**

- All nodes have `nodeId: NodeId`

### `src/parser.ts`

**Current:**

- Creates nodes with optional spans
- Creates `name: string` fields

**After Phase 1:**

- Always sets span (types now require it)

**After Phase 2:**

- Creates `UnresolvedName` nodes for all names

**After Phase 3:**

- Accepts `fileId` parameter, embeds in all spans

**After Phase 4:**

- Uses `NodeIdGenerator` to assign IDs

### `src/desugar.ts`

**Current:**

- Creates Core nodes without spans

**After Phase 1:**

- Propagates spans from Surface to Core
- Synthesizes spans for compiler-generated nodes

**After Phase 2:**

- Converts `UnresolvedName` to `Name` (with span)

### `src/resolve.ts`

**Current:**

- Optional `fileRegistry` parameter
- Conditional symbol table creation

**After Phase 5:**

- Required `fileRegistry` parameter
- Always builds symbol table

### `src/checker.ts`

**Current:**

- 1817 lines, multiple concerns
- Multiple type maps

**After Phase 4:**

- Single `TypeMap<NodeId, Type>`

**After Phase 6:**

- Split into `src/checker/` directory with focused modules

### `src/compile.ts`

**Current:**

- 300+ lines of span offsetting functions

**After Phase 3:**

- All offset functions removed (spans are file-aware)

### `src/optimize.ts`

**Current:**

- 2655 lines, all optimizations in one file

**After Phase 6:**

- Split into `src/optimize/` directory
- Each optimization in its own file
- Common traversal utilities shared

### `src/codegen.ts`

**Current:**

- Generates async IIFEs for all matches

**After Phase 7:**

- Context-aware code generation
- Direct statements when possible
- Ternaries for simple matches
- IIFEs only when necessary

### `src/lexer.ts`

**Current:**

- `Token` is a tuple `[TokenKind, number, number]`

**After Cleanup:**

- `Token` is an object with named fields

### `src/lsp/analysis.ts`

**Current:**

- Complex type lookup with multiple fallbacks

**After Phase 4:**

- Simple lookup by node ID

**After Phase 5:**

- Uses integrated symbol table

---

## Guiding Principles

1. **Types don't lie.** If something can be `undefined`, the type says `| undefined`. If something is always present, the type doesn't allow `undefined`.

2. **Spans are sacred.** Every piece of source code has a location. That location should be preserved through all transformations.

3. **Names are nodes.** A name isn't just a string—it's a piece of source code with a location and (after resolution) an identity.

4. **One source of truth.** Type information lives in one place. Symbol information lives in one place. No duplication, no synchronization bugs.

5. **Small, focused modules.** Each file has one job. If you can't describe the file's purpose in one sentence, split it.

6. **No special modes.** The compiler works the same way whether invoked from CLI or LSP. Different consumers, same pipeline.
