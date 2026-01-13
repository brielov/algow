/**
 * Exhaustiveness Checking for Pattern Matching
 *
 * Checks if a set of patterns exhaustively covers all possible values of a type.
 * Recursively verifies constructor argument patterns are also exhaustive.
 */

import type * as C from "../core";
import type { Type, TypeEnv } from "../types";
import type { CheckContext } from "./context";

// Type guard helpers
const isCPTuple = (p: C.CPattern): p is C.CPTuple => p.kind === "CPTuple";
const isDefined = <T>(x: T | undefined | null): x is T => x != null;

/** Extract argument types from a constructor type (curried function type) */
const getConstructorArgTypes = (conType: Type): Type[] => {
  const argTypes: Type[] = [];
  let current = conType;
  while (current.kind === "TFun") {
    argTypes.push(current.param);
    current = current.ret;
  }
  return argTypes;
};

// =============================================================================
// Exhaustiveness Checking
// =============================================================================

/**
 * Check if a set of patterns exhaustively covers all possible values of a type.
 * Returns a list of missing pattern descriptions if not exhaustive.
 */
export const checkExhaustiveness = (
  ctx: CheckContext,
  type: Type,
  patterns: readonly C.CPattern[],
  env: TypeEnv = new Map(),
): string[] => {
  // If any pattern is a wildcard or variable at the top level, it's exhaustive
  if (patterns.some((p) => p.kind === "CPWild" || p.kind === "CPVar")) {
    return [];
  }

  // Check based on the type
  const resolvedType = resolveType(ctx, type);

  switch (resolvedType.kind) {
    case "TVar":
      // Unknown type - assume exhaustive if we have at least one pattern
      return patterns.length > 0 ? [] : ["_"];

    case "TCon": {
      // Check for bool type specially
      if (resolvedType.name === "bool") {
        return checkBoolExhaustiveness(patterns);
      }
      // For other primitive types (int, float, string, char), literals are never exhaustive
      if (["int", "float", "string", "char"].includes(resolvedType.name)) {
        return ["_"];
      }
      // Check ADT exhaustiveness (no type args for TCon)
      return checkAdtExhaustiveness(ctx, resolvedType.name, patterns, env, resolvedType);
    }

    case "TApp": {
      // Get the base type name for ADT checking
      const baseName = getBaseTypeName(resolvedType);
      if (baseName) {
        return checkAdtExhaustiveness(ctx, baseName, patterns, env, resolvedType);
      }
      return [];
    }

    case "TTuple": {
      return checkTupleExhaustiveness(ctx, resolvedType.elements, patterns, env);
    }

    case "TFun":
      // Function types - can't really pattern match on them
      return [];

    case "TRecord":
      // Record patterns - check if all patterns cover all possibilities
      return checkRecordExhaustiveness(resolvedType, patterns);

    default:
      return [];
  }
};

/** Resolve type aliases */
const resolveType = (ctx: CheckContext, type: Type): Type => {
  if (type.kind === "TCon") {
    const alias = ctx.aliasRegistry.get(type.name);
    if (alias && alias.params.length === 0) {
      return resolveType(ctx, alias.type);
    }
  }
  return type;
};

/** Get the base type constructor name from a type application */
const getBaseTypeName = (type: Type): string | null => {
  if (type.kind === "TCon") return type.name;
  if (type.kind === "TApp") return getBaseTypeName(type.con);
  return null;
};

/** Check exhaustiveness for bool type */
const checkBoolExhaustiveness = (patterns: readonly C.CPattern[]): string[] => {
  let hasTrue = false;
  let hasFalse = false;

  for (const p of patterns) {
    if (p.kind === "CPWild" || p.kind === "CPVar") return [];
    if (p.kind === "CPLit" && p.value.kind === "bool") {
      if (p.value.value) hasTrue = true;
      else hasFalse = true;
    }
  }

  const missing: string[] = [];
  if (!hasTrue) missing.push("true");
  if (!hasFalse) missing.push("false");
  return missing;
};

/** Extract type arguments from a type application (e.g., Box bool -> [bool]) */
const getTypeArgs = (type: Type): Type[] => {
  const args: Type[] = [];
  let current = type;
  while (current.kind === "TApp") {
    args.unshift(current.arg);
    current = current.con;
  }
  return args;
};

/** Substitute type parameters in constructor arg types with actual type args */
const instantiateConstructorArgs = (
  conScheme: { vars: readonly string[]; type: Type },
  typeArgs: Type[],
): Type[] => {
  // Build substitution from type parameters to actual type args BEFORE instantiation
  const subst = new Map<string, Type>();
  for (let i = 0; i < conScheme.vars.length && i < typeArgs.length; i++) {
    subst.set(conScheme.vars[i]!, typeArgs[i]!);
  }

  // Apply substitution to the original type (not instantiated) to get concrete arg types
  const substitutedType = applyTypeSubst(conScheme.type, subst);
  return getConstructorArgTypes(substitutedType);
};

/** Apply a type variable substitution */
const applyTypeSubst = (type: Type, subst: Map<string, Type>): Type => {
  switch (type.kind) {
    case "TVar": {
      const replacement = subst.get(type.name);
      return replacement ?? type;
    }
    case "TCon":
      return type;
    case "TApp":
      return {
        kind: "TApp",
        con: applyTypeSubst(type.con, subst),
        arg: applyTypeSubst(type.arg, subst),
      };
    case "TFun":
      return {
        kind: "TFun",
        param: applyTypeSubst(type.param, subst),
        ret: applyTypeSubst(type.ret, subst),
      };
    case "TTuple":
      return { kind: "TTuple", elements: type.elements.map((e) => applyTypeSubst(e, subst)) };
    case "TRecord":
      return type; // Records are more complex, skip for now
    default:
      return type;
  }
};

/** Check exhaustiveness for ADT types */
const checkAdtExhaustiveness = (
  ctx: CheckContext,
  typeName: string,
  patterns: readonly C.CPattern[],
  env: TypeEnv,
  scrutineeType?: Type,
): string[] => {
  const constructors = ctx.registry.get(typeName);
  if (!constructors) {
    // Unknown type - assume exhaustive
    return [];
  }

  // Extract type arguments from the scrutinee type (e.g., Box bool -> [bool])
  const typeArgs = scrutineeType ? getTypeArgs(scrutineeType) : [];

  // Track which constructors are covered with their patterns
  const covered = new Map<string, C.CPCon[]>();
  for (const con of constructors) {
    covered.set(con, []);
  }

  // Helper to process a single pattern
  const processPattern = (p: C.CPattern): boolean => {
    if (p.kind === "CPWild" || p.kind === "CPVar") {
      // Wildcard covers all constructors
      return true;
    }
    if (p.kind === "CPCon") {
      const existing = covered.get(p.name);
      if (existing) {
        existing.push(p);
      }
    }
    if (p.kind === "CPAs") {
      // As-pattern: check the inner pattern
      return processPattern(p.pattern);
    }
    if (p.kind === "CPOr") {
      // Flatten or-patterns (recursively collect all alternatives)
      const alts = flattenOrPattern(p);
      for (const alt of alts) {
        if (processPattern(alt)) return true;
      }
    }
    return false;
  };

  // Collect patterns for each constructor
  for (const p of patterns) {
    if (processPattern(p)) {
      return []; // Exhaustive due to wildcard/variable
    }
  }

  // Check each constructor and its arguments
  const missing: string[] = [];
  for (const [conName, conPatterns] of covered) {
    if (conPatterns.length === 0) {
      missing.push(conName);
      continue;
    }

    // Look up constructor type to get argument types
    const conScheme = env.get(conName);
    if (!conScheme) continue;

    // Get argument types with actual type arguments substituted
    const argTypes = instantiateConstructorArgs(conScheme, typeArgs);

    // If constructor has no arguments, it's covered
    if (argTypes.length === 0) continue;

    // Recursively check each argument position
    for (let i = 0; i < argTypes.length; i++) {
      const argPatterns = conPatterns.map((cp) => cp.args[i]).filter(isDefined);

      const argMissing = checkExhaustiveness(ctx, argTypes[i]!, argPatterns, env);
      if (argMissing.length > 0) {
        // Build a description of the missing pattern
        const args = argTypes.map((_, j) => (j === i ? argMissing[0] : "_"));
        missing.push(`${conName}${args.length > 0 ? ` ${args.join(" ")}` : ""}`);
        break; // Only report first missing for this constructor
      }
    }
  }

  return missing;
};

/** Check exhaustiveness for tuple types */
const checkTupleExhaustiveness = (
  ctx: CheckContext,
  elementTypes: readonly Type[],
  patterns: readonly C.CPattern[],
  env: TypeEnv,
): string[] => {
  // If any pattern is a wildcard or variable, the tuple match is exhaustive
  if (patterns.some((p) => p.kind === "CPWild" || p.kind === "CPVar")) {
    return [];
  }

  // Collect tuple patterns
  const tuplePatterns = patterns.filter(isCPTuple);

  if (tuplePatterns.length === 0) {
    // No tuple patterns and no wildcards - not exhaustive
    return [`(${elementTypes.map(() => "_").join(", ")})`];
  }

  // For each position, check if it's covered
  for (let i = 0; i < elementTypes.length; i++) {
    const positionPatterns = tuplePatterns.map((tp) => tp.elements[i]).filter(isDefined);
    const missing = checkExhaustiveness(ctx, elementTypes[i]!, positionPatterns, env);
    if (missing.length > 0) {
      // Position not exhaustive
      const parts = elementTypes.map((_, j) => (j === i ? missing[0] : "_"));
      return [`(${parts.join(", ")})`];
    }
  }

  return [];
};

/** Check exhaustiveness for record types */
const checkRecordExhaustiveness = (
  _type: Type & { kind: "TRecord" },
  patterns: readonly C.CPattern[],
): string[] => {
  // Record patterns are typically not exhaustive unless there's a wildcard
  if (patterns.some((p) => p.kind === "CPWild" || p.kind === "CPVar")) {
    return [];
  }
  // Records with row variables are open, so we can't enumerate all possibilities
  return [];
};

/** Flatten an or-pattern into a list of alternative patterns */
const flattenOrPattern = (p: C.CPOr): C.CPattern[] => {
  const result: C.CPattern[] = [];
  const flatten = (pat: C.CPattern): void => {
    if (pat.kind === "CPOr") {
      flatten(pat.left);
      flatten(pat.right);
    } else {
      result.push(pat);
    }
  };
  flatten(p);
  return result;
};

/** Add a warning for non-exhaustive patterns */
export const addExhaustivenessWarning = (
  ctx: CheckContext,
  missing: string[],
  span?: C.CExpr["span"],
): void => {
  const missingStr = missing.slice(0, 3).join(", ") + (missing.length > 3 ? ", ..." : "");
  const message = `Non-exhaustive patterns. Missing: ${missingStr}`;
  const start = span?.start ?? 0;
  const end = span?.end ?? 0;
  ctx.diagnostics.push({
    start,
    end,
    message,
    severity: "warning",
    kind: "non-exhaustive",
  });
};
