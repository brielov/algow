/**
 * Diagnostic types shared across lexer, parser, and type inference.
 *
 * Diagnostics provide error reporting without throwing exceptions,
 * enabling error recovery and future LSP integration.
 */

export type DiagnosticSeverity = "error" | "warning" | "info";

export type DiagnosticKind =
  | "type-mismatch"
  | "unbound-variable"
  | "arity-mismatch"
  | "infinite-type"
  | "missing-field"
  | "non-exhaustive"
  | "syntax-error"
  | "duplicate-definition"
  | "internal-error";

export type Diagnostic = {
  readonly start: number;
  readonly end: number;
  readonly message: string;
  readonly severity: DiagnosticSeverity;
  readonly kind?: DiagnosticKind;
  readonly file?: string;
  readonly expected?: string;
  readonly actual?: string;
  readonly suggestions?: readonly string[];
  readonly notes?: readonly string[];
};

/**
 * Create an error diagnostic.
 */
export const error = (start: number, end: number, message: string): Diagnostic => ({
  start,
  end,
  message,
  severity: "error",
});

/**
 * Create a warning diagnostic.
 */
export const warning = (start: number, end: number, message: string): Diagnostic => ({
  start,
  end,
  message,
  severity: "warning",
});

/**
 * Create an info diagnostic.
 */
export const info = (start: number, end: number, message: string): Diagnostic => ({
  start,
  end,
  message,
  severity: "info",
});

/**
 * Create a type mismatch error with expected/actual info.
 */
export const typeMismatch = (
  start: number,
  end: number,
  expected: string,
  actual: string,
  context?: string,
): Diagnostic => ({
  start,
  end,
  message: context
    ? `Type mismatch in ${context}: expected '${expected}', got '${actual}'`
    : `Type mismatch: expected '${expected}', got '${actual}'`,
  severity: "error",
  kind: "type-mismatch",
  expected,
  actual,
});

/**
 * Create an unbound variable error with suggestions.
 */
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

/**
 * Create an internal compiler error diagnostic.
 * These indicate bugs in the compiler itself, not user errors.
 */
export const internalError = (
  start: number,
  end: number,
  context: string,
  details: string,
): Diagnostic => ({
  start,
  end,
  message: `Internal compiler error in ${context}: ${details}. Please report this bug at https://github.com/anthropics/algow/issues`,
  severity: "error",
  kind: "internal-error",
});

/**
 * Compute Levenshtein distance between two strings.
 */
export const levenshteinDistance = (a: string, b: string): number => {
  if (a.length === 0) return b.length;
  if (b.length === 0) return a.length;

  const matrix: number[][] = [];

  for (let i = 0; i <= b.length; i++) {
    matrix[i] = [i];
  }
  for (let j = 0; j <= a.length; j++) {
    matrix[0]![j] = j;
  }

  for (let i = 1; i <= b.length; i++) {
    for (let j = 1; j <= a.length; j++) {
      const cost = a[j - 1] === b[i - 1] ? 0 : 1;
      matrix[i]![j] = Math.min(
        matrix[i - 1]![j]! + 1,
        matrix[i]![j - 1]! + 1,
        matrix[i - 1]![j - 1]! + cost,
      );
    }
  }

  return matrix[b.length]![a.length]!;
};

/**
 * Find similar names within edit distance threshold.
 */
export const findSimilarNames = (
  name: string,
  candidates: Iterable<string>,
  maxDistance = 2,
  maxSuggestions = 3,
): string[] => {
  const scored: Array<[string, number]> = [];

  for (const candidate of candidates) {
    if (candidate === name) continue;
    const dist = levenshteinDistance(name.toLowerCase(), candidate.toLowerCase());
    if (dist <= maxDistance) {
      scored.push([candidate, dist]);
    }
  }

  return scored
    .sort((a, b) => a[1] - b[1])
    .slice(0, maxSuggestions)
    .map(([s]) => s);
};
