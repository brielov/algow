/**
 * Name Resolution (Section 7)
 *
 * Assigns unique identifiers to all bindings and resolves variable references.
 * After this phase, all CVar nodes have valid Name.id values.
 */

import type { Diagnostic } from "./diagnostics";
import { error as diagError } from "./diagnostics";
import * as C from "./core";
import { freshName, resetNameCounter, type Name } from "./core";

// =============================================================================
// Resolution Context
// =============================================================================

type Env = Map<string, Name>;

type ResolveContext = {
  readonly env: Env;
  readonly diagnostics: Diagnostic[];
  // Constructors are always in scope (from type declarations)
  readonly constructors: Set<string>;
};

const createContext = (constructors: Set<string> = new Set()): ResolveContext => ({
  env: new Map(),
  diagnostics: [],
  constructors,
});

const extendEnv = (ctx: ResolveContext, original: string): [ResolveContext, Name] => {
  const name = freshName(original);
  const newEnv = new Map(ctx.env);
  newEnv.set(original, name);
  return [{ ...ctx, env: newEnv }, name];
};

const extendEnvMany = (
  ctx: ResolveContext,
  originals: readonly string[],
): [ResolveContext, Name[]] => {
  const newEnv = new Map(ctx.env);
  const names: Name[] = [];
  for (const original of originals) {
    const name = freshName(original);
    newEnv.set(original, name);
    names.push(name);
  }
  return [{ ...ctx, env: newEnv }, names];
};

const lookup = (ctx: ResolveContext, original: string, span?: C.CExpr["span"]): Name | null => {
  const name = ctx.env.get(original);
  if (name) return name;

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

    case "CCon":
      // Constructors are global, no resolution needed
      return expr;

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

const resolvePattern = (_ctx: ResolveContext, pattern: C.CPattern): PatternResult => {
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
      const results = pattern.args.map((p) => resolvePattern(_ctx, p));
      return {
        pattern: C.cpcon(
          pattern.name,
          results.map((r) => r.pattern),
          pattern.span,
        ),
        bindings: results.flatMap((r) => r.bindings),
      };
    }

    case "CPTuple": {
      const results = pattern.elements.map((p) => resolvePattern(_ctx, p));
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
        const result = resolvePattern(_ctx, f.pattern);
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
      const inner = resolvePattern(_ctx, pattern.pattern);
      return {
        pattern: C.cpas(name, inner.pattern, pattern.span),
        bindings: [name, ...inner.bindings],
      };
    }

    case "CPOr": {
      // For or-patterns, both branches must bind the same variables
      const left = resolvePattern(_ctx, pattern.left);
      const right = resolvePattern(_ctx, pattern.right);
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
  const { pattern, bindings } = resolvePattern(ctx, c.pattern);

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
// Declaration Resolution
// =============================================================================

const resolveDecl = (ctx: ResolveContext, decl: C.CDecl): [ResolveContext, C.CDecl] => {
  switch (decl.kind) {
    case "CDeclType": {
      // Add constructors to the set
      const newConstructors = new Set(ctx.constructors);
      for (const con of decl.constructors) {
        newConstructors.add(con.name);
      }
      return [{ ...ctx, constructors: newConstructors }, decl];
    }

    case "CDeclLet": {
      const resolvedValue = resolveExpr(ctx, decl.value);
      const [newCtx, name] = extendEnv(ctx, decl.name.original);
      return [newCtx, C.cdecllet(name, resolvedValue, decl.span)];
    }

    case "CDeclLetRec": {
      // Add all names first for mutual recursion
      const originals = decl.bindings.map((b) => b.name.original);
      const [newCtx, names] = extendEnvMany(ctx, originals);

      const resolvedBindings = decl.bindings.map((b, i) => ({
        name: names[i]!,
        value: resolveExpr(newCtx, b.value),
      }));

      return [newCtx, C.cdeclletrec(resolvedBindings, decl.span)];
    }

    case "CDeclForeign": {
      const [newCtx, name] = extendEnv(ctx, decl.name.original);
      return [newCtx, C.cdeclforeign(name, decl.module, decl.jsName, decl.type, decl.span)];
    }
  }
};

// =============================================================================
// Program Resolution
// =============================================================================

export type ResolveResult = {
  program: C.CProgram;
  diagnostics: readonly Diagnostic[];
};

export const resolveProgram = (
  program: C.CProgram,
  preludeEnv: Map<string, Name> = new Map(),
  preludeConstructors: Set<string> = new Set(),
): ResolveResult => {
  resetNameCounter();

  let ctx = createContext(preludeConstructors);

  // Add prelude bindings to initial environment
  ctx = { ...ctx, env: new Map([...ctx.env, ...preludeEnv]) };

  const resolvedDecls: C.CDecl[] = [];

  for (const decl of program.decls) {
    const [newCtx, resolvedDecl] = resolveDecl(ctx, decl);
    ctx = newCtx;
    resolvedDecls.push(resolvedDecl);
  }

  const resolvedExpr = program.expr ? resolveExpr(ctx, program.expr) : null;

  return {
    program: { decls: resolvedDecls, expr: resolvedExpr },
    diagnostics: ctx.diagnostics,
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
