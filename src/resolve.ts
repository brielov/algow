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
import type { SymbolTable, SymbolTableBuilder, SymbolKind, ScopeSnapshot } from "./lsp/symbols";
import {
  createSymbolTableBuilder,
  addDefinition,
  addReference,
  addScopeSnapshot,
  freezeSymbolTable,
} from "./lsp/symbols";
import type { FileRegistry } from "./lsp/workspace";
import { spanToLocation } from "./lsp/workspace";

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
  lsp,
});

const extendEnv = (
  ctx: ResolveContext,
  original: string,
  kind: SymbolKind = "variable",
  span?: C.CExpr["span"],
): [ResolveContext, Name] => {
  const name = freshName(original);
  const newEnv = new Map(ctx.env);
  newEnv.set(original, name);

  // Track definition for LSP
  if (ctx.lsp && span) {
    const location = spanToLocation(ctx.lsp.fileRegistry, span);
    if (location) {
      addDefinition(ctx.lsp.builder, {
        nameId: name.id,
        name: original,
        kind,
        location,
      });
    }
  }

  return [{ ...ctx, env: newEnv }, name];
};

const extendEnvMany = (
  ctx: ResolveContext,
  originals: readonly string[],
  kind: SymbolKind = "variable",
  spans?: readonly (C.CExpr["span"] | undefined)[],
): [ResolveContext, Name[]] => {
  const newEnv = new Map(ctx.env);
  const names: Name[] = [];
  for (let i = 0; i < originals.length; i++) {
    const original = originals[i]!;
    const name = freshName(original);
    newEnv.set(original, name);
    names.push(name);

    // Track definition for LSP
    const span = spans?.[i];
    if (ctx.lsp && span) {
      const location = spanToLocation(ctx.lsp.fileRegistry, span);
      if (location) {
        addDefinition(ctx.lsp.builder, {
          nameId: name.id,
          name: original,
          kind,
          location,
        });
      }
    }
  }
  return [{ ...ctx, env: newEnv }, names];
};

const lookup = (ctx: ResolveContext, original: string, span?: C.CExpr["span"]): Name | null => {
  const name = ctx.env.get(original);
  if (name) {
    // Track reference for LSP
    if (ctx.lsp && span) {
      const location = spanToLocation(ctx.lsp.fileRegistry, span);
      if (location) {
        addReference(ctx.lsp.builder, {
          targetId: name.id,
          location,
        });
      }
    }
    return name;
  }

  // Check if it's a constructor
  if (ctx.constructors.has(original)) {
    // Constructors don't need unique IDs, they're identified by name
    return { id: -1, original };
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
      const resolved = lookup(ctx, expr.name.original, expr.span);
      if (resolved) {
        return C.cvar(resolved, expr.span);
      }
      // Keep the unresolved name for error recovery
      return expr;
    }

    case "CLit":
      return expr;

    case "CApp":
      return C.capp(resolveExpr(ctx, expr.func), resolveExpr(ctx, expr.arg), expr.span);

    case "CAbs": {
      const [newCtx, param] = extendEnv(ctx, expr.param.original);
      return C.cabs(param, resolveExpr(newCtx, expr.body), expr.span);
    }

    case "CLet": {
      // Resolve value in current context
      const resolvedValue = resolveExpr(ctx, expr.value);
      // Extend context with binding
      const [newCtx, name] = extendEnv(ctx, expr.name.original);
      return C.clet(name, resolvedValue, resolveExpr(newCtx, expr.body), expr.span);
    }

    case "CLetRec": {
      // For recursive let, add all names first (enables mutual recursion)
      const originals = expr.bindings.map((b) => b.name.original);
      const [newCtx, names] = extendEnvMany(ctx, originals);

      // Resolve all values in the extended context
      const resolvedBindings = expr.bindings.map((b, i) => ({
        name: names[i]!,
        value: resolveExpr(newCtx, b.value),
      }));

      return C.cletrec(resolvedBindings, resolveExpr(newCtx, expr.body), expr.span);
    }

    case "CMatch":
      return C.cmatch(
        resolveExpr(ctx, expr.scrutinee),
        expr.cases.map((c) => resolveCase(ctx, c)),
        expr.span,
      );

    case "CCon": {
      // Check if this constructor is an alias (from use declarations)
      const aliasedName = ctx.constructorAliases.get(expr.name);
      if (aliasedName) {
        return C.ccon(aliasedName, expr.span);
      }
      // Constructors are global, no resolution needed
      return expr;
    }

    case "CTuple":
      return C.ctuple(
        expr.elements.map((e) => resolveExpr(ctx, e)),
        expr.span,
      );

    case "CRecord":
      return C.crecord(
        expr.fields.map((f) => ({ name: f.name, value: resolveExpr(ctx, f.value) })),
        expr.span,
      );

    case "CRecordUpdate":
      return C.crecordUpdate(
        resolveExpr(ctx, expr.record),
        expr.fields.map((f) => ({ name: f.name, value: resolveExpr(ctx, f.value) })),
        expr.span,
      );

    case "CField":
      return C.cfield(resolveExpr(ctx, expr.record), expr.field, expr.span);

    case "CForeign":
      // Foreign calls don't need resolution
      return expr;

    case "CBinOp":
      return C.cbinop(
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

type PatternResult = {
  pattern: C.CPattern;
  bindings: Name[];
};

const resolvePattern = (
  pattern: C.CPattern,
  constructorAliases: Map<string, string>,
): PatternResult => {
  switch (pattern.kind) {
    case "CPWild":
      return { pattern, bindings: [] };

    case "CPVar": {
      const name = freshName(pattern.name.original);
      return {
        pattern: C.cpvar(name, pattern.span),
        bindings: [name],
      };
    }

    case "CPLit":
      return { pattern, bindings: [] };

    case "CPCon": {
      const results = pattern.args.map((p) => resolvePattern(p, constructorAliases));
      // Check if this constructor is an alias
      const aliasedName = constructorAliases.get(pattern.name);
      return {
        pattern: C.cpcon(
          aliasedName ?? pattern.name,
          results.map((r) => r.pattern),
          pattern.span,
        ),
        bindings: results.flatMap((r) => r.bindings),
      };
    }

    case "CPTuple": {
      const results = pattern.elements.map((p) => resolvePattern(p, constructorAliases));
      return {
        pattern: C.cptuple(
          results.map((r) => r.pattern),
          pattern.span,
        ),
        bindings: results.flatMap((r) => r.bindings),
      };
    }

    case "CPRecord": {
      const resolvedFields: { name: string; pattern: C.CPattern }[] = [];
      const bindings: Name[] = [];

      for (const f of pattern.fields) {
        const result = resolvePattern(f.pattern, constructorAliases);
        resolvedFields.push({ name: f.name, pattern: result.pattern });
        bindings.push(...result.bindings);
      }

      return {
        pattern: C.cprecord(resolvedFields, pattern.span),
        bindings,
      };
    }

    case "CPAs": {
      const name = freshName(pattern.name.original);
      const inner = resolvePattern(pattern.pattern, constructorAliases);
      return {
        pattern: C.cpas(name, inner.pattern, pattern.span),
        bindings: [name, ...inner.bindings],
      };
    }

    case "CPOr": {
      // For or-patterns, both branches must bind the same variables
      const left = resolvePattern(pattern.left, constructorAliases);
      const right = resolvePattern(pattern.right, constructorAliases);
      // Use left's bindings (should be same as right's)
      return {
        pattern: C.cpor(left.pattern, right.pattern, pattern.span),
        bindings: left.bindings,
      };
    }
  }
};

// =============================================================================
// Case Resolution
// =============================================================================

const resolveCase = (ctx: ResolveContext, c: C.CCase): C.CCase => {
  const { pattern, bindings } = resolvePattern(c.pattern, ctx.constructorAliases);

  // Extend context with pattern bindings
  const newEnv = new Map(ctx.env);
  for (const name of bindings) {
    newEnv.set(name.original, name);
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
            diagError(
              decl.span?.start ?? 0,
              decl.span?.end ?? 0,
              `Duplicate type definition: ${decl.name}`,
            ),
          );
        }
        const newConstructors = new Set(ctx.constructors);
        for (const con of decl.constructors) {
          if (ctx.constructors.has(con.name)) {
            ctx.diagnostics.push(
              diagError(
                decl.span?.start ?? 0,
                decl.span?.end ?? 0,
                `Duplicate constructor: ${con.name}`,
              ),
            );
          }
          newConstructors.add(con.name);
        }
        const newTypes = new Set(ctx.types);
        newTypes.add(decl.name);
        ctx = { ...ctx, constructors: newConstructors, types: newTypes };
        break;
      }

      case "CDeclLet": {
        const name = freshName(decl.name.original);
        bindingNames.set(decl.name.original, name);
        // Track definition for LSP (use nameSpan for precise hover, fallback to full span)
        const letDefSpan = decl.nameSpan ?? decl.span;
        if (ctx.lsp && letDefSpan) {
          const location = spanToLocation(ctx.lsp.fileRegistry, letDefSpan);
          if (location) {
            addDefinition(ctx.lsp.builder, {
              nameId: name.id,
              name: decl.name.original,
              kind: "function",
              location,
            });
          }
        }
        break;
      }

      case "CDeclLetRec":
        for (const b of decl.bindings) {
          const name = freshName(b.name.original);
          bindingNames.set(b.name.original, name);
          // Track definition for LSP (use nameSpan for precise hover)
          const recDefSpan = b.nameSpan ?? b.value.span;
          if (ctx.lsp && recDefSpan) {
            const location = spanToLocation(ctx.lsp.fileRegistry, recDefSpan);
            if (location) {
              addDefinition(ctx.lsp.builder, {
                nameId: name.id,
                name: b.name.original,
                kind: "function",
                location,
              });
            }
          }
        }
        break;

      case "CDeclForeign": {
        const name = freshName(decl.name.original);
        bindingNames.set(decl.name.original, name);
        // Track definition for LSP (use nameSpan for precise hover, fallback to full span)
        const foreignDefSpan = decl.nameSpan ?? decl.span;
        if (ctx.lsp && foreignDefSpan) {
          const location = spanToLocation(ctx.lsp.fileRegistry, foreignDefSpan);
          if (location) {
            addDefinition(ctx.lsp.builder, {
              nameId: name.id,
              name: decl.name.original,
              kind: "foreign",
              location,
            });
          }
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
        const name = bindingNames.get(decl.name.original)!;
        const resolvedValue = resolveExpr(ctx, decl.value);

        // Track constructor aliases for use declarations
        if (decl.value.kind === "CCon") {
          const newConstructors = new Set(ctx.constructors);
          newConstructors.add(decl.name.original);
          const newAliases = new Map(ctx.constructorAliases);
          newAliases.set(decl.name.original, decl.value.name);
          ctx = { ...ctx, constructors: newConstructors, constructorAliases: newAliases };
        }

        resolvedDecls.push(C.cdecllet(name, resolvedValue, decl.span));
        break;
      }

      case "CDeclLetRec": {
        const resolvedBindings = decl.bindings.map((b) => ({
          name: bindingNames.get(b.name.original)!,
          value: resolveExpr(ctx, b.value),
        }));
        resolvedDecls.push(C.cdeclletrec(resolvedBindings, decl.span));
        break;
      }

      case "CDeclForeign": {
        const name = bindingNames.get(decl.name.original)!;
        resolvedDecls.push(
          C.cdeclforeign(name, decl.module, decl.jsName, decl.type, decl.isAsync, decl.span),
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
    env.set(name, freshName(name));
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
