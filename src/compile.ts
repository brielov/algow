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
import { generateJS, type Target, DEFAULT_TARGET } from "./codegen";
import type { Diagnostic } from "./diagnostics";
import type { SDecl, SProgram } from "./surface";
import { type Type, type Scheme, applySubst } from "./types";

// Re-export Target type and helpers
export type { Target } from "./codegen";
export { TARGETS, DEFAULT_TARGET, isValidTarget } from "./codegen";

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
  /** Target platform for code generation (default: "node") */
  readonly target?: Target;
};

export type CompileResult = {
  /** Generated JavaScript code (if not typeCheckOnly) */
  readonly code?: string;
  /** Inferred type of main function */
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
 * Check if a type is a valid main signature.
 * Main must be a function. If the parameter type is fully resolved,
 * it must be List String. If it's a type variable (polymorphic), we accept it.
 */
const isValidMainSignature = (type: Type): boolean => {
  if (type.kind !== "TFun") return false;

  // Check param is List String or a type variable
  const param = type.param;

  // Type variable is ok (polymorphic main)
  if (param.kind === "TVar") return true;

  // If not TApp, reject (we already handled TVar above)
  if (param.kind !== "TApp") return false;

  // Must be List String (type system uses lowercase "string")
  if (param.con.kind !== "TCon" || param.con.name !== "List") return false;

  // List argument can be String/string or a type variable
  if (param.arg.kind === "TVar") return true;
  if (param.arg.kind !== "TCon") return false;

  // Accept both "String" and "string" (internal representation)
  return param.arg.name === "String" || param.arg.name === "string";
};

/**
 * Check if a declaration defines `main`.
 */
const hasMainDecl = (decl: SDecl): boolean => {
  if (decl.kind === "SDeclLet" && decl.name === "main") return true;
  if (decl.kind === "SDeclLetRec") {
    return decl.bindings.some((b) => b.name === "main");
  }
  return false;
};

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
  const {
    typeCheckOnly = false,
    optimize: shouldOptimize = true,
    target = DEFAULT_TARGET,
  } = options;
  const allDiagnostics: Diagnostic[] = [];

  // 1. Parse all sources and track main declarations
  const parsedFiles: ParsedFile[] = [];
  const mainFiles: string[] = [];

  for (const { path, content } of sources) {
    const parseResult = parse(content);

    allDiagnostics.push(...parseResult.diagnostics);
    parsedFiles.push({ path, source: content, program: parseResult.program });

    // Track files that declare main
    if (parseResult.program.decls.some(hasMainDecl)) {
      mainFiles.push(path);
    }

    // Warn if file has tail expression (deprecated)
    if (parseResult.program.expr !== null) {
      allDiagnostics.push({
        severity: "warning",
        message: `Tail expression in ${path} will be ignored. Programs must define a 'main' function.`,
        start: 0,
        end: 0,
      });
    }
  }

  // Check for parse errors
  if (allDiagnostics.some((d) => d.severity === "error")) {
    return { diagnostics: allDiagnostics, success: false };
  }

  // Check for multiple main declarations
  if (mainFiles.length > 1) {
    allDiagnostics.push({
      severity: "error",
      message: `Multiple 'main' declarations found in: ${mainFiles.join(", ")}`,
      start: 0,
      end: 0,
    });
    return { diagnostics: allDiagnostics, success: false };
  }

  // 2. Combine all programs (no entry index needed anymore)
  const combinedProgram = combinePrograms(parsedFiles);

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

  // 6. Validate main function exists and has correct signature
  // TypeEnv keys are in format "{id}:{original}", so we need to find the one ending with ":main"
  let mainScheme: Scheme | undefined;
  for (const [key, scheme] of checkResult.typeEnv) {
    if (key.endsWith(":main")) {
      mainScheme = scheme;
      break;
    }
  }
  if (!mainScheme) {
    allDiagnostics.push({
      severity: "error",
      message: "No 'main' function found. Every program must define: main : List String -> a",
      start: 0,
      end: 0,
    });
    return { diagnostics: allDiagnostics, success: false };
  }

  const mainType = applySubst(checkResult.subst, mainScheme.type);
  if (!isValidMainSignature(mainType)) {
    allDiagnostics.push({
      severity: "error",
      message: `'main' must have signature 'List String -> a', but got: ${typeToString(mainType)}`,
      start: 0,
      end: 0,
    });
    return { diagnostics: allDiagnostics, success: false };
  }

  // Type check only - return early
  if (typeCheckOnly) {
    return {
      type: mainType,
      typeString: typeToString(mainType),
      diagnostics: allDiagnostics,
      success: true,
    };
  }

  // 7. Lower to IR
  const lowerResult = lowerProgram(resolveResult.program, checkResult);

  // 8. Optimize (optional)
  const irProgram = shouldOptimize ? optimize(lowerResult.program) : lowerResult.program;

  // 9. Generate JavaScript
  const output = generateJS(irProgram, { target });

  return {
    code: output.code,
    type: mainType,
    typeString: typeToString(mainType),
    diagnostics: allDiagnostics,
    success: true,
  };
};

// =============================================================================
// Helpers
// =============================================================================

/**
 * Combine parsed files into a single program.
 * All declarations are merged; entry point is the main function.
 */
const combinePrograms = (files: readonly ParsedFile[]): SProgram => {
  const allDecls: SDecl[] = [];

  for (const file of files) {
    allDecls.push(...file.program.decls);
  }

  // No tail expression - entry point is main function
  return { decls: allDecls, expr: null };
};
