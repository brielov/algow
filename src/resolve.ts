/**
 * Name Resolution (Section 7)
 *
 * Assigns unique identifiers to all bindings and resolves variable references.
 * After this phase, all CVar nodes have valid Name.id values.
 *
 * When LSP tracking is enabled, also collects:
 * - Symbol definitions (where bindings are declared)
 * - Symbol references (where bindings are used)
 * - Scope snapshots (for autocomplete)
 */

import type { Diagnostic } from "./diagnostics";
import { error as diagError } from "./diagnostics";
import * as C from "./core";
import { freshName, resetNameCounter, type Name } from "./core";
import type { Span } from "./surface";
import type { SymbolTable, SymbolTableBuilder, SymbolKind, ScopeSnapshot } from "./lsp/symbols";
import {
  createSymbolTableBuilder,
  addDefinition,
  addReference,
  addScopeSnapshot,
  freezeSymbolTable,
} from "./lsp/symbols";
import type { FileRegistry } from "./files";

// =============================================================================
// Resolution Context
// =============================================================================

type Env = Map<string, Name>;

/** LSP tracking state (optional) */
type LSPTracking = {
  readonly builder: SymbolTableBuilder;
  readonly fileRegistry: FileRegistry;
};

type ResolveContext = {
  readonly env: Env;
  readonly diagnostics: Diagnostic[];
  // Constructors are always in scope (from type declarations)
  readonly constructors: Set<string>;
  // Track defined type names to detect duplicates
  readonly types: Set<string>;
  // Constructor aliases: alias name → qualified name (e.g., "Ok" → "Result.Ok")
  readonly constructorAliases: Map<string, string>;
  // Constructor name → nameId (for LSP references)
  readonly constructorIds: Map<string, number>;
  // LSP tracking (null when not needed)
  readonly lsp: LSPTracking | null;
};

const createContext = (
  constructors: Set<string> = new Set(),
  types: Set<string> = new Set(),
  lsp: LSPTracking | null = null,
): ResolveContext => ({
  env: new Map(),
  diagnostics: [],
  constructors,
  types,
  constructorAliases: new Map(),
  constructorIds: new Map(),
  lsp,
});

const syntheticSpan: Span = { fileId: 0, start: 0, end: 0 };

const extendEnv = (
  ctx: ResolveContext,
  original: string,
  kind: SymbolKind = "variable",
  span?: Span,
): [ResolveContext, Name] => {
  const name = freshName(original, span ?? syntheticSpan);
  const newEnv = new Map(ctx.env);
  newEnv.set(original, name);

  // Track definition for LSP
  if (ctx.lsp && span) {
    addDefinition(ctx.lsp.builder, {
      nameId: name.id,
      name: original,
      kind,
      location: span,
    });
  }

  return [{ ...ctx, env: newEnv }, name];
};

const extendEnvMany = (
  ctx: ResolveContext,
  originals: readonly string[],
  kind: SymbolKind = "variable",
  spans?: readonly (Span | undefined)[],
): [ResolveContext, Name[]] => {
  const newEnv = new Map(ctx.env);
  const names: Name[] = [];
  for (let i = 0; i < originals.length; i++) {
    const original = originals[i]!;
    const span = spans?.[i];
    const name = freshName(original, span ?? syntheticSpan);
    newEnv.set(original, name);
    names.push(name);

    // Track definition for LSP
    if (ctx.lsp && span) {
      addDefinition(ctx.lsp.builder, {
        nameId: name.id,
        name: original,
        kind,
        location: span,
      });
    }
  }
  return [{ ...ctx, env: newEnv }, names];
};

const lookup = (ctx: ResolveContext, original: string, span?: C.CExpr["span"]): Name | null => {
  const name = ctx.env.get(original);
  if (name) {
    // Track reference for LSP
    if (ctx.lsp && span) {
      addReference(ctx.lsp.builder, {
        targetId: name.id,
        location: span,
      });
    }
    return name;
  }

  // Check if it's a constructor
  if (ctx.constructors.has(original)) {
    // Constructors don't need unique IDs, they're identified by name
    return { id: -1, text: original, span: span ?? syntheticSpan };
  }

  // Unbound variable error
  ctx.diagnostics.push(
    diagError(span?.start ?? 0, span?.end ?? 0, `Unbound variable: ${original}`),
  );
  return null;
};

/** Record a scope snapshot at a given position (for autocomplete) */
const _recordScope = (ctx: ResolveContext, offset: number): void => {
  if (!ctx.lsp) return;

  const snapshot: ScopeSnapshot = {
    offset,
    bindings: new Map(Array.from(ctx.env.entries()).map(([name, nameObj]) => [name, nameObj.id])),
    constructors: new Set(ctx.constructors),
  };
  addScopeSnapshot(ctx.lsp.builder, snapshot);
};

// =============================================================================
// Expression Resolution
// =============================================================================

const resolveExpr = (ctx: ResolveContext, expr: C.CExpr): C.CExpr => {
  switch (expr.kind) {
    case "CVar": {
      // For qualified names (Module.member), use memberSpan for reference tracking
      // This allows hovering over just the member part to show its type
      const lookupSpan = expr.memberSpan ?? expr.span;
      const resolved = lookup(ctx, expr.name.text, lookupSpan);
      if (resolved) {
        return C.cvar(expr.nodeId, resolved, expr.span, expr.moduleSpan, expr.memberSpan);
      }
      // Keep the unresolved name for error recovery
      return expr;
    }

    case "CLit":
      return expr;

    case "CApp":
      return C.capp(
        expr.nodeId,
        resolveExpr(ctx, expr.func),
        resolveExpr(ctx, expr.arg),
        expr.span,
      );

    case "CAbs": {
      const [newCtx, param] = extendEnv(ctx, expr.param.text, "parameter", expr.paramSpan);
      return C.cabs(expr.nodeId, param, resolveExpr(newCtx, expr.body), expr.span, expr.paramSpan);
    }

    case "CLet": {
      // Resolve value in current context
      const resolvedValue = resolveExpr(ctx, expr.value);
      // Extend context with binding (span embedded in name for LSP tracking)
      const [newCtx, name] = extendEnv(ctx, expr.name.text, "variable", expr.name.span);
      return C.clet(expr.nodeId, name, resolvedValue, resolveExpr(newCtx, expr.body), expr.span);
    }

    case "CLetRec": {
      // For recursive let, add all names first (enables mutual recursion)
      const originals = expr.bindings.map((b) => b.name.text);
      const spans = expr.bindings.map((b) => b.name.span);
      const [newCtx, names] = extendEnvMany(ctx, originals, "variable", spans);

      // Resolve all values in the extended context
      const resolvedBindings = expr.bindings.map((b, i) => ({
        name: names[i]!,
        value: resolveExpr(newCtx, b.value),
      }));

      return C.cletrec(expr.nodeId, resolvedBindings, resolveExpr(newCtx, expr.body), expr.span);
    }

    case "CMatch":
      return C.cmatch(
        expr.nodeId,
        resolveExpr(ctx, expr.scrutinee),
        expr.cases.map((c) => resolveCase(ctx, c)),
        expr.span,
      );

    case "CCon": {
      // Check if this constructor is an alias (from use declarations)
      const aliasedName = ctx.constructorAliases.get(expr.name);
      if (aliasedName) {
        return C.ccon(expr.nodeId, aliasedName, expr.span);
      }
      // Constructors are global, no resolution needed
      return expr;
    }

    case "CTuple":
      return C.ctuple(
        expr.nodeId,
        expr.elements.map((e) => resolveExpr(ctx, e)),
        expr.span,
      );

    case "CRecord":
      return C.crecord(
        expr.nodeId,
        expr.fields.map((f) => ({ name: f.name, value: resolveExpr(ctx, f.value) })),
        expr.span,
      );

    case "CRecordUpdate":
      return C.crecordUpdate(
        expr.nodeId,
        resolveExpr(ctx, expr.record),
        expr.fields.map((f) => ({ name: f.name, value: resolveExpr(ctx, f.value) })),
        expr.span,
      );

    case "CField":
      return C.cfield(expr.nodeId, resolveExpr(ctx, expr.record), expr.field, expr.span);

    case "CForeign":
      // Foreign calls don't need resolution
      return expr;

    case "CBinOp":
      return C.cbinop(
        expr.nodeId,
        expr.op,
        resolveExpr(ctx, expr.left),
        resolveExpr(ctx, expr.right),
        expr.span,
      );
  }
};

// =============================================================================
// Pattern Resolution
// =============================================================================

type PatternBinding = {
  name: Name;
  span: Span;
};

type PatternResult = {
  pattern: C.CPattern;
  bindings: PatternBinding[];
};

const resolvePattern = (ctx: ResolveContext, pattern: C.CPattern): PatternResult => {
  switch (pattern.kind) {
    case "CPWild":
      return { pattern, bindings: [] };

    case "CPVar": {
      const name = freshName(pattern.name.text, pattern.name.span);
      return {
        pattern: C.cpvar(pattern.nodeId, name, pattern.span),
        bindings: [{ name, span: pattern.span }],
      };
    }

    case "CPLit":
      return { pattern, bindings: [] };

    case "CPCon": {
      const results = pattern.args.map((p) => resolvePattern(ctx, p));
      // Check if this constructor is an alias
      const aliasedName = ctx.constructorAliases.get(pattern.name);
      const resolvedName = aliasedName ?? pattern.name;

      // Track constructor reference for LSP
      const conId = ctx.constructorIds.get(resolvedName);
      if (ctx.lsp && conId !== undefined) {
        addReference(ctx.lsp.builder, {
          targetId: conId,
          location: pattern.span,
        });
      }

      return {
        pattern: C.cpcon(
          pattern.nodeId,
          resolvedName,
          results.map((r) => r.pattern),
          pattern.span,
        ),
        bindings: results.flatMap((r) => r.bindings),
      };
    }

    case "CPTuple": {
      const results = pattern.elements.map((p) => resolvePattern(ctx, p));
      return {
        pattern: C.cptuple(
          pattern.nodeId,
          results.map((r) => r.pattern),
          pattern.span,
        ),
        bindings: results.flatMap((r) => r.bindings),
      };
    }

    case "CPRecord": {
      const resolvedFields: { name: string; pattern: C.CPattern }[] = [];
      const bindings: PatternBinding[] = [];

      for (const f of pattern.fields) {
        const result = resolvePattern(ctx, f.pattern);
        resolvedFields.push({ name: f.name, pattern: result.pattern });
        bindings.push(...result.bindings);
      }

      return {
        pattern: C.cprecord(pattern.nodeId, resolvedFields, pattern.span),
        bindings,
      };
    }

    case "CPAs": {
      const name = freshName(pattern.name.text, pattern.name.span);
      const inner = resolvePattern(ctx, pattern.pattern);
      return {
        pattern: C.cpas(pattern.nodeId, name, inner.pattern, pattern.span),
        bindings: [{ name, span: pattern.span }, ...inner.bindings],
      };
    }

    case "CPOr": {
      // For or-patterns, both branches must bind the same variables
      const left = resolvePattern(ctx, pattern.left);
      const right = resolvePattern(ctx, pattern.right);
      // Use left's bindings (should be same as right's)
      return {
        pattern: C.cpor(pattern.nodeId, left.pattern, right.pattern, pattern.span),
        bindings: left.bindings,
      };
    }
  }
};

// =============================================================================
// Case Resolution
// =============================================================================

const resolveCase = (ctx: ResolveContext, c: C.CCase): C.CCase => {
  const { pattern, bindings } = resolvePattern(ctx, c.pattern);

  // Extend context with pattern bindings
  const newEnv = new Map(ctx.env);
  for (const binding of bindings) {
    newEnv.set(binding.name.text, binding.name);

    // Track definition for LSP
    if (ctx.lsp) {
      addDefinition(ctx.lsp.builder, {
        nameId: binding.name.id,
        name: binding.name.text,
        kind: "pattern-binding",
        location: binding.span,
      });
    }
  }
  const newCtx = { ...ctx, env: newEnv };

  return {
    pattern,
    guard: c.guard ? resolveExpr(newCtx, c.guard) : null,
    body: resolveExpr(newCtx, c.body),
  };
};

// =============================================================================
// Program Resolution
// =============================================================================

export type ResolveResult = {
  program: C.CProgram;
  diagnostics: readonly Diagnostic[];
  /** Symbol table for LSP features (only present when fileRegistry is provided) */
  symbolTable: SymbolTable | null;
};

export const resolveProgram = (
  program: C.CProgram,
  preludeEnv: Map<string, Name> = new Map(),
  preludeConstructors: Set<string> = new Set(),
  preludeTypes: Set<string> = new Set(),
  fileRegistry: FileRegistry | null = null,
): ResolveResult => {
  resetNameCounter();

  // Set up LSP tracking if file registry is provided
  const lsp: LSPTracking | null = fileRegistry
    ? { builder: createSymbolTableBuilder(), fileRegistry }
    : null;

  let ctx = createContext(preludeConstructors, preludeTypes, lsp);

  // Add prelude bindings to initial environment
  ctx = { ...ctx, env: new Map([...ctx.env, ...preludeEnv]) };

  // ==========================================================================
  // First pass: collect all type declarations and binding names
  // This allows bindings to reference each other regardless of declaration order
  // ==========================================================================

  const bindingNames = new Map<string, Name>();

  for (const decl of program.decls) {
    switch (decl.kind) {
      case "CDeclType": {
        // Register constructors and types
        if (ctx.types.has(decl.name)) {
          ctx.diagnostics.push(
            diagError(decl.span.start, decl.span.end, `Duplicate type definition: ${decl.name}`),
          );
        }
        const newConstructors = new Set(ctx.constructors);
        const newConstructorIds = new Map(ctx.constructorIds);
        for (let tag = 0; tag < decl.constructors.length; tag++) {
          const con = decl.constructors[tag]!;
          if (ctx.constructors.has(con.name)) {
            ctx.diagnostics.push(
              diagError(decl.span.start, decl.span.end, `Duplicate constructor: ${con.name}`),
            );
          }
          newConstructors.add(con.name);

          // Create nameId for constructor and track for LSP
          const conName = freshName(con.name, con.span);
          newConstructorIds.set(con.name, conName.id);
          if (ctx.lsp) {
            addDefinition(ctx.lsp.builder, {
              nameId: conName.id,
              name: con.name,
              kind: "constructor",
              location: con.span,
            });
            // Also add to constructors map for completions
            ctx.lsp.builder.constructors.set(con.name, {
              name: con.name,
              qualifiedName: con.name,
              typeName: decl.name,
              tag,
              arity: con.fields.length,
              location: con.span,
            });
          }
        }
        const newTypes = new Set(ctx.types);
        newTypes.add(decl.name);
        ctx = {
          ...ctx,
          constructors: newConstructors,
          constructorIds: newConstructorIds,
          types: newTypes,
        };
        break;
      }

      case "CDeclLet": {
        const name = freshName(decl.name.text, decl.name.span);
        bindingNames.set(decl.name.text, name);
        // Track definition for LSP (name now includes span)
        if (ctx.lsp) {
          addDefinition(ctx.lsp.builder, {
            nameId: name.id,
            name: decl.name.text,
            kind: "function",
            location: decl.name.span,
          });
        }
        break;
      }

      case "CDeclLetRec":
        for (const b of decl.bindings) {
          const name = freshName(b.name.text, b.name.span);
          bindingNames.set(b.name.text, name);
          // Track definition for LSP (name now includes span)
          if (ctx.lsp) {
            addDefinition(ctx.lsp.builder, {
              nameId: name.id,
              name: b.name.text,
              kind: "function",
              location: b.name.span,
            });
          }
        }
        break;

      case "CDeclForeign": {
        const name = freshName(decl.name.text, decl.name.span);
        bindingNames.set(decl.name.text, name);
        // Track definition for LSP (name now includes span)
        if (ctx.lsp) {
          addDefinition(ctx.lsp.builder, {
            nameId: name.id,
            name: decl.name.text,
            kind: "foreign",
            location: decl.name.span,
          });
        }
        break;
      }
    }
  }

  // Add all binding names to environment
  const fullEnv = new Map(ctx.env);
  for (const [original, name] of bindingNames) {
    fullEnv.set(original, name);
  }
  ctx = { ...ctx, env: fullEnv };

  // ==========================================================================
  // Second pass: resolve all declarations with full environment
  // ==========================================================================

  const resolvedDecls: C.CDecl[] = [];

  for (const decl of program.decls) {
    switch (decl.kind) {
      case "CDeclType":
        resolvedDecls.push(decl);
        break;

      case "CDeclLet": {
        const name = bindingNames.get(decl.name.text)!;
        const resolvedValue = resolveExpr(ctx, decl.value);

        // Track constructor aliases for use declarations
        if (decl.value.kind === "CCon") {
          const newConstructors = new Set(ctx.constructors);
          newConstructors.add(decl.name.text);
          const newAliases = new Map(ctx.constructorAliases);
          newAliases.set(decl.name.text, decl.value.name);
          ctx = { ...ctx, constructors: newConstructors, constructorAliases: newAliases };
        }

        resolvedDecls.push(C.cdecllet(decl.nodeId, name, resolvedValue, decl.span));
        break;
      }

      case "CDeclLetRec": {
        const resolvedBindings = decl.bindings.map((b) => ({
          name: bindingNames.get(b.name.text)!,
          value: resolveExpr(ctx, b.value),
        }));
        resolvedDecls.push(C.cdeclletrec(decl.nodeId, resolvedBindings, decl.span));
        break;
      }

      case "CDeclForeign": {
        const name = bindingNames.get(decl.name.text)!;
        resolvedDecls.push(
          C.cdeclforeign(
            decl.nodeId,
            name,
            decl.module,
            decl.jsName,
            decl.type,
            decl.isAsync,
            decl.span,
          ),
        );
        break;
      }
    }
  }

  const resolvedExpr = program.expr ? resolveExpr(ctx, program.expr) : null;

  // Build symbol table if LSP tracking was enabled
  const symbolTable = ctx.lsp ? freezeSymbolTable(ctx.lsp.builder) : null;

  return {
    program: { decls: resolvedDecls, expr: resolvedExpr },
    diagnostics: ctx.diagnostics,
    symbolTable,
  };
};

// =============================================================================
// Utilities for Prelude
// =============================================================================

/**
 * Create prelude environment from a list of known names.
 * Used to provide built-in functions (map, filter, etc.) to user code.
 */
export const createPreludeEnv = (names: readonly string[]): Map<string, Name> => {
  const env = new Map<string, Name>();
  for (const name of names) {
    env.set(name, freshName(name, syntheticSpan));
  }
  return env;
};

/**
 * Create constructor set from type declarations.
 */
export const createConstructorSet = (decls: readonly C.CDecl[]): Set<string> => {
  const constructors = new Set<string>();
  for (const decl of decls) {
    if (decl.kind === "CDeclType") {
      for (const con of decl.constructors) {
        constructors.add(con.name);
      }
    }
  }
  return constructors;
};
