/**
 * Algow Compiler - Core Compilation API
 *
 * Pure compilation function that transforms source files into JavaScript.
 * No file I/O - that's handled by the CLI.
 */

import { parse } from "./parser";
import { desugarProgram } from "./desugar";
import { resolveProgram } from "./resolve";
import { checkProgram, typeToString } from "./checker";
import { lowerProgram } from "./lower";
import { optimize } from "./optimize";
import { generateJS } from "./codegen";
import type { Diagnostic } from "./diagnostics";
import type { SDecl, SProgram } from "./surface";
import type { Type } from "./types";

// =============================================================================
// Types
// =============================================================================

export type SourceFile = {
  readonly path: string;
  readonly content: string;
};

export type CompileOptions = {
  /** Only type check, don't generate code */
  readonly typeCheckOnly?: boolean;
  /** Enable optimizations (default: true) */
  readonly optimize?: boolean;
};

export type CompileResult = {
  /** Generated JavaScript code (if not typeCheckOnly) */
  readonly code?: string;
  /** Inferred type of the main expression */
  readonly type?: Type;
  /** Type as a string */
  readonly typeString?: string;
  /** All diagnostics from all phases */
  readonly diagnostics: readonly Diagnostic[];
  /** Whether compilation succeeded (no errors) */
  readonly success: boolean;
};

// =============================================================================
// Internal Types
// =============================================================================

type ParsedFile = {
  path: string;
  source: string;
  program: SProgram;
};

// =============================================================================
// Compilation Pipeline
// =============================================================================

/**
 * Compile source files to JavaScript.
 *
 * @param sources - Array of source files (path + content)
 * @param options - Compilation options
 * @returns Compilation result with code, type, and diagnostics
 */
export const compile = (
  sources: readonly SourceFile[],
  options: CompileOptions = {},
): CompileResult => {
  const { typeCheckOnly = false, optimize: shouldOptimize = true } = options;
  const allDiagnostics: Diagnostic[] = [];

  // 1. Parse all sources
  const parsedFiles: ParsedFile[] = [];
  let entryIndex = -1;

  for (let i = 0; i < sources.length; i++) {
    const { path, content } = sources[i]!;
    const parseResult = parse(content);

    allDiagnostics.push(...parseResult.diagnostics);
    parsedFiles.push({ path, source: content, program: parseResult.program });

    // Track entry point (file with tail expression)
    if (parseResult.program.expr !== null) {
      if (entryIndex !== -1) {
        allDiagnostics.push({
          severity: "error",
          message: `Multiple entry points: ${sources[entryIndex]!.path} and ${path}`,
          start: 0,
          end: 0,
        });
      } else {
        entryIndex = i;
      }
    }
  }

  // Check for parse errors
  if (allDiagnostics.some((d) => d.severity === "error")) {
    return { diagnostics: allDiagnostics, success: false };
  }

  // 2. Combine all programs
  const combinedProgram = combinePrograms(parsedFiles, entryIndex);

  // 3. Desugar
  const coreProgram = desugarProgram(combinedProgram);

  // 4. Resolve names
  const resolveResult = resolveProgram(coreProgram);
  allDiagnostics.push(...resolveResult.diagnostics);

  if (allDiagnostics.some((d) => d.severity === "error")) {
    return { diagnostics: allDiagnostics, success: false };
  }

  // 5. Type check
  const checkResult = checkProgram(resolveResult.program);
  allDiagnostics.push(...checkResult.diagnostics);

  if (allDiagnostics.some((d) => d.severity === "error")) {
    return { diagnostics: allDiagnostics, success: false };
  }

  // Type check only - return early
  if (typeCheckOnly) {
    return {
      type: checkResult.type ?? undefined,
      typeString: checkResult.type ? typeToString(checkResult.type) : undefined,
      diagnostics: allDiagnostics,
      success: true,
    };
  }

  // 6. Lower to IR
  const lowerResult = lowerProgram(resolveResult.program, checkResult);

  // 7. Optimize (optional)
  const irProgram = shouldOptimize ? optimize(lowerResult.program) : lowerResult.program;

  // 8. Generate JavaScript
  const output = generateJS(irProgram);

  return {
    code: output.code,
    type: checkResult.type ?? undefined,
    typeString: checkResult.type ? typeToString(checkResult.type) : undefined,
    diagnostics: allDiagnostics,
    success: true,
  };
};

// =============================================================================
// Helpers
// =============================================================================

/**
 * Combine parsed files into a single program.
 * All declarations are merged; entry file provides the tail expression.
 */
const combinePrograms = (files: readonly ParsedFile[], entryIndex: number): SProgram => {
  const allDecls: SDecl[] = [];

  // Collect all declarations (order doesn't matter for types)
  for (let i = 0; i < files.length; i++) {
    if (i !== entryIndex) {
      allDecls.push(...files[i]!.program.decls);
    }
  }

  // Entry file declarations last (for now - will change when lets become order-independent)
  if (entryIndex >= 0) {
    allDecls.push(...files[entryIndex]!.program.decls);
    return {
      decls: allDecls,
      expr: files[entryIndex]!.program.expr,
    };
  }

  return { decls: allDecls, expr: null };
};
