/**
 * Diagnostic types shared across lexer, parser, and type inference.
 *
 * Diagnostics provide error reporting without throwing exceptions,
 * enabling error recovery and future LSP integration.
 */

export type DiagnosticSeverity = "error" | "warning" | "info";

export type Diagnostic = {
  readonly start: number;
  readonly end: number;
  readonly message: string;
  readonly severity: DiagnosticSeverity;
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
