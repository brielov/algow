/**
 * Exhaustiveness Checking for Pattern Matching
 *
 * Checks if a set of patterns exhaustively covers all possible values of a type.
 */

import type * as C from "../core";
import type { Type } from "../types";
import type { CheckContext } from "./context";

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
      // Check ADT exhaustiveness
      return checkAdtExhaustiveness(ctx, resolvedType.name, patterns);
    }

    case "TApp": {
      // Get the base type name for ADT checking
      const baseName = getBaseTypeName(resolvedType);
      if (baseName) {
        return checkAdtExhaustiveness(ctx, baseName, patterns);
      }
      return [];
    }

    case "TTuple": {
      return checkTupleExhaustiveness(ctx, resolvedType.elements, patterns);
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

/** Check exhaustiveness for ADT types */
const checkAdtExhaustiveness = (
  ctx: CheckContext,
  typeName: string,
  patterns: readonly C.CPattern[],
): string[] => {
  const constructors = ctx.registry.get(typeName);
  if (!constructors) {
    // Unknown type - assume exhaustive
    return [];
  }

  // Track which constructors are covered
  const covered = new Map<string, C.CPattern[]>();
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

  // Check each constructor
  const missing: string[] = [];
  for (const [conName, conPatterns] of covered) {
    if (conPatterns.length === 0) {
      missing.push(conName);
    }
    // TODO: For constructors with arguments, we could recursively check
    // if all argument patterns are exhaustive, but this is complex
  }

  return missing;
};

/** Check exhaustiveness for tuple types */
const checkTupleExhaustiveness = (
  ctx: CheckContext,
  elementTypes: readonly Type[],
  patterns: readonly C.CPattern[],
): string[] => {
  // If any pattern is a wildcard or variable, the tuple match is exhaustive
  if (patterns.some((p) => p.kind === "CPWild" || p.kind === "CPVar")) {
    return [];
  }

  // Collect tuple patterns
  const tuplePatterns = patterns.filter((p) => p.kind === "CPTuple") as C.CPTuple[];

  if (tuplePatterns.length === 0) {
    // No tuple patterns and no wildcards - not exhaustive
    return [`(${elementTypes.map(() => "_").join(", ")})`];
  }

  // For each position, check if it's covered
  // This is a simplified check - full exhaustiveness for tuples is complex
  for (let i = 0; i < elementTypes.length; i++) {
    const positionPatterns = tuplePatterns
      .map((tp) => tp.elements[i])
      .filter(Boolean) as C.CPattern[];
    const missing = checkExhaustiveness(ctx, elementTypes[i]!, positionPatterns);
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
