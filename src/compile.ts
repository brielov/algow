/**
 * Algow Compiler - Core Compilation API
 *
 * Pure compilation function that transforms source files into JavaScript.
 * No file I/O - that's handled by the CLI.
 */

import { minify as terserMinify } from "terser";
import { checkProgram, typeToString, type CheckOutput } from "./checker";
import { DEFAULT_TARGET, generateJS, type Target } from "./codegen";
import { desugarProgram } from "./desugar";
import type { Diagnostic } from "./diagnostics";
import { lowerProgram } from "./lower";
import type { SymbolTable } from "./lsp/symbols";
import { enrichWithTypes } from "./lsp/symbols";
import type { FileRegistry } from "./lsp/workspace";
import { createFileRegistryBuilder, freezeFileRegistry, registerFile } from "./lsp/workspace";
import { optimize } from "./optimize";
import { parse } from "./parser";
import { resolveProgram } from "./resolve";
import type { SCase, SDecl, SDoStmt, SExpr, SPattern, SProgram, Span, SType } from "./surface";
import { applySubst, type Scheme, type Type } from "./types";

// Re-export Target type and helpers
export { DEFAULT_TARGET, isValidTarget, TARGETS } from "./codegen";
export type { Target } from "./codegen";

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
  /** Minify the output JavaScript with terser */
  readonly minify?: boolean;
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
export const compile = async (
  sources: readonly SourceFile[],
  options: CompileOptions = {},
): Promise<CompileResult> => {
  const {
    typeCheckOnly = false,
    optimize: shouldOptimize = true,
    target = DEFAULT_TARGET,
    minify = false,
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

  // 10. Minify (optional)
  let code = output.code;
  if (minify) {
    const result = await terserMinify(code, {
      compress: { dead_code: true, unused: true },
      mangle: true,
    });
    if (result.code) {
      code = result.code;
    }
  }

  return {
    code,
    type: mainType,
    typeString: typeToString(mainType),
    diagnostics: allDiagnostics,
    success: true,
  };
};

// =============================================================================
// Span Offsetting (for multi-file LSP compilation)
// =============================================================================

/** Offset a span by a given amount */
const offsetSpan = (span: Span | undefined, offset: number): Span | undefined => {
  if (!span) return undefined;
  return { start: span.start + offset, end: span.end + offset };
};

/** Offset all spans in a type */
const offsetType = (type: SType, offset: number): SType => {
  switch (type.kind) {
    case "STVar":
      return { ...type, span: offsetSpan(type.span, offset) };
    case "STCon":
      return { ...type, span: offsetSpan(type.span, offset) };
    case "STApp":
      return {
        ...type,
        func: offsetType(type.func, offset),
        arg: offsetType(type.arg, offset),
        span: offsetSpan(type.span, offset),
      };
    case "STFun":
      return {
        ...type,
        param: offsetType(type.param, offset),
        result: offsetType(type.result, offset),
        span: offsetSpan(type.span, offset),
      };
    case "STTuple":
      return {
        ...type,
        elements: type.elements.map((e) => offsetType(e, offset)),
        span: offsetSpan(type.span, offset),
      };
    case "STRecord":
      return {
        ...type,
        fields: type.fields.map((f) => ({ ...f, type: offsetType(f.type, offset) })),
        span: offsetSpan(type.span, offset),
      };
  }
};

/** Offset all spans in a pattern */
const offsetPattern = (pattern: SPattern, offset: number): SPattern => {
  switch (pattern.kind) {
    case "SPWild":
      return { ...pattern, span: offsetSpan(pattern.span, offset) };
    case "SPVar":
      return { ...pattern, span: offsetSpan(pattern.span, offset) };
    case "SPLit":
      return { ...pattern, span: offsetSpan(pattern.span, offset) };
    case "SPCon":
      return {
        ...pattern,
        args: pattern.args.map((p) => offsetPattern(p, offset)),
        span: offsetSpan(pattern.span, offset),
      };
    case "SPTuple":
      return {
        ...pattern,
        elements: pattern.elements.map((p) => offsetPattern(p, offset)),
        span: offsetSpan(pattern.span, offset),
      };
    case "SPRecord":
      return {
        ...pattern,
        fields: pattern.fields.map((f) => ({ ...f, pattern: offsetPattern(f.pattern, offset) })),
        span: offsetSpan(pattern.span, offset),
      };
    case "SPAs":
      return {
        ...pattern,
        pattern: offsetPattern(pattern.pattern, offset),
        span: offsetSpan(pattern.span, offset),
      };
    case "SPOr":
      return {
        ...pattern,
        left: offsetPattern(pattern.left, offset),
        right: offsetPattern(pattern.right, offset),
        span: offsetSpan(pattern.span, offset),
      };
    case "SPCons":
      return {
        ...pattern,
        head: offsetPattern(pattern.head, offset),
        tail: offsetPattern(pattern.tail, offset),
        span: offsetSpan(pattern.span, offset),
      };
    case "SPList":
      return {
        ...pattern,
        elements: pattern.elements.map((p) => offsetPattern(p, offset)),
        span: offsetSpan(pattern.span, offset),
      };
  }
};

/** Offset all spans in a case */
const offsetCase = (c: SCase, offset: number): SCase => ({
  pattern: offsetPattern(c.pattern, offset),
  guard: c.guard ? offsetExpr(c.guard, offset) : null,
  body: offsetExpr(c.body, offset),
});

/** Offset all spans in a do statement */
const offsetDoStmt = (stmt: SDoStmt, offset: number): SDoStmt => {
  switch (stmt.kind) {
    case "DoBindPattern":
      return {
        ...stmt,
        pattern: offsetPattern(stmt.pattern, offset),
        expr: offsetExpr(stmt.expr, offset),
      };
    case "DoLet":
      return {
        ...stmt,
        pattern: offsetPattern(stmt.pattern, offset),
        expr: offsetExpr(stmt.expr, offset),
      };
    case "DoExpr":
      return { ...stmt, expr: offsetExpr(stmt.expr, offset) };
  }
};

/** Offset all spans in an expression */
const offsetExpr = (expr: SExpr, offset: number): SExpr => {
  switch (expr.kind) {
    case "SVar":
      return { ...expr, span: offsetSpan(expr.span, offset) };
    case "SLit":
      return { ...expr, span: offsetSpan(expr.span, offset) };
    case "SApp":
      return {
        ...expr,
        func: offsetExpr(expr.func, offset),
        arg: offsetExpr(expr.arg, offset),
        span: offsetSpan(expr.span, offset),
      };
    case "SAbs":
      return {
        ...expr,
        body: offsetExpr(expr.body, offset),
        span: offsetSpan(expr.span, offset),
      };
    case "SLet":
      return {
        ...expr,
        value: offsetExpr(expr.value, offset),
        body: offsetExpr(expr.body, offset),
        span: offsetSpan(expr.span, offset),
      };
    case "SLetRec":
      return {
        ...expr,
        bindings: expr.bindings.map((b) => ({ ...b, value: offsetExpr(b.value, offset) })),
        body: offsetExpr(expr.body, offset),
        span: offsetSpan(expr.span, offset),
      };
    case "SIf":
      return {
        ...expr,
        cond: offsetExpr(expr.cond, offset),
        thenBranch: offsetExpr(expr.thenBranch, offset),
        elseBranch: offsetExpr(expr.elseBranch, offset),
        span: offsetSpan(expr.span, offset),
      };
    case "SMatch":
      return {
        ...expr,
        scrutinee: offsetExpr(expr.scrutinee, offset),
        cases: expr.cases.map((c) => offsetCase(c, offset)),
        span: offsetSpan(expr.span, offset),
      };
    case "SCon":
      return { ...expr, span: offsetSpan(expr.span, offset) };
    case "STuple":
      return {
        ...expr,
        elements: expr.elements.map((e) => offsetExpr(e, offset)),
        span: offsetSpan(expr.span, offset),
      };
    case "SRecord":
      return {
        ...expr,
        fields: expr.fields.map((f) => ({ ...f, value: offsetExpr(f.value, offset) })),
        span: offsetSpan(expr.span, offset),
      };
    case "SRecordUpdate":
      return {
        ...expr,
        record: offsetExpr(expr.record, offset),
        fields: expr.fields.map((f) => ({ ...f, value: offsetExpr(f.value, offset) })),
        span: offsetSpan(expr.span, offset),
      };
    case "SField":
      return {
        ...expr,
        record: offsetExpr(expr.record, offset),
        span: offsetSpan(expr.span, offset),
      };
    case "SList":
      return {
        ...expr,
        elements: expr.elements.map((e) => offsetExpr(e, offset)),
        span: offsetSpan(expr.span, offset),
      };
    case "SPipe":
      return {
        ...expr,
        left: offsetExpr(expr.left, offset),
        right: offsetExpr(expr.right, offset),
        span: offsetSpan(expr.span, offset),
      };
    case "SCons":
      return {
        ...expr,
        head: offsetExpr(expr.head, offset),
        tail: offsetExpr(expr.tail, offset),
        span: offsetSpan(expr.span, offset),
      };
    case "SBinOp":
      return {
        ...expr,
        left: offsetExpr(expr.left, offset),
        right: offsetExpr(expr.right, offset),
        span: offsetSpan(expr.span, offset),
      };
    case "SDo":
      return {
        ...expr,
        stmts: expr.stmts.map((s) => offsetDoStmt(s, offset)),
        span: offsetSpan(expr.span, offset),
      };
    case "SAnnot":
      return {
        ...expr,
        expr: offsetExpr(expr.expr, offset),
        type: offsetType(expr.type, offset),
        span: offsetSpan(expr.span, offset),
      };
  }
};

/** Offset all spans in a declaration */
const offsetDecl = (decl: SDecl, offset: number): SDecl => {
  switch (decl.kind) {
    case "SDeclType":
      return {
        ...decl,
        constructors: decl.constructors.map((c) => ({
          ...c,
          fields: c.fields.map((f) => offsetType(f, offset)),
        })),
        span: offsetSpan(decl.span, offset),
      };
    case "SDeclTypeAlias":
      return {
        ...decl,
        type: offsetType(decl.type, offset),
        span: offsetSpan(decl.span, offset),
      };
    case "SDeclLet":
      return {
        ...decl,
        value: offsetExpr(decl.value, offset),
        span: offsetSpan(decl.span, offset),
      };
    case "SDeclLetRec":
      return {
        ...decl,
        bindings: decl.bindings.map((b) => ({ ...b, value: offsetExpr(b.value, offset) })),
        span: offsetSpan(decl.span, offset),
      };
    case "SDeclForeign":
      return {
        ...decl,
        type: offsetType(decl.type, offset),
        span: offsetSpan(decl.span, offset),
      };
    case "SDeclModule":
      return {
        ...decl,
        decls: decl.decls.map((d) => offsetDecl(d, offset)),
        span: offsetSpan(decl.span, offset),
      };
    case "SDeclUse":
      return { ...decl, span: offsetSpan(decl.span, offset) };
  }
};

/** Offset all spans in a program */
const offsetProgram = (program: SProgram, offset: number): SProgram => ({
  decls: program.decls.map((d) => offsetDecl(d, offset)),
  expr: program.expr ? offsetExpr(program.expr, offset) : null,
});

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

// =============================================================================
// LSP Compilation
// =============================================================================

/** Result of LSP-focused compilation */
export type LSPCompileResult = {
  /** Whether compilation succeeded (no errors) */
  readonly success: boolean;
  /** All diagnostics from all phases */
  readonly diagnostics: readonly Diagnostic[];
  /** Symbol table for LSP features */
  readonly symbolTable: SymbolTable | null;
  /** Type checker output (for type information) */
  readonly checkOutput: CheckOutput | null;
  /** File registry for position mapping */
  readonly fileRegistry: FileRegistry;
};

/**
 * Compile source files for LSP analysis.
 *
 * Unlike the regular compile function, this:
 * - Tracks symbol definitions and references
 * - Returns the symbol table for LSP features
 * - Does not generate code
 * - Does not require a main function
 *
 * @param sources - Array of source files (path + content)
 * @returns Compilation result with symbol table and diagnostics
 */
export const compileForLSP = (sources: readonly SourceFile[]): LSPCompileResult => {
  const allDiagnostics: Diagnostic[] = [];

  // Build file registry (tracks file identity through compilation)
  // Also track global offsets to adjust spans for multi-file compilation
  const registryBuilder = createFileRegistryBuilder();
  const parsedFiles: ParsedFile[] = [];

  for (const { path, content } of sources) {
    // Get the current global offset BEFORE registering (this is where this file starts)
    const globalOffset = registryBuilder.currentOffset;

    registerFile(registryBuilder, path, content);
    const parseResult = parse(content);
    allDiagnostics.push(...parseResult.diagnostics);

    // Offset all spans in the parsed program to be global
    // This ensures symbol locations are correctly mapped to files
    const offsettedProgram = offsetProgram(parseResult.program, globalOffset);

    parsedFiles.push({ path, source: content, program: offsettedProgram });
  }

  const fileRegistry = freezeFileRegistry(registryBuilder);

  // Check for parse errors - continue anyway for partial results
  const hasParseErrors = allDiagnostics.some((d) => d.severity === "error");

  // Combine all programs
  const combinedProgram = combinePrograms(parsedFiles);

  // Desugar
  const coreProgram = desugarProgram(combinedProgram);

  // Resolve names (with LSP tracking enabled)
  const resolveResult = resolveProgram(coreProgram, new Map(), new Set(), new Set(), fileRegistry);
  allDiagnostics.push(...resolveResult.diagnostics);

  // Check for resolve errors - continue anyway for partial results
  const _hasResolveErrors = allDiagnostics.some((d) => d.severity === "error");

  if (hasParseErrors) {
    // Can't proceed past parsing
    return {
      success: false,
      diagnostics: allDiagnostics,
      symbolTable: resolveResult.symbolTable,
      checkOutput: null,
      fileRegistry,
    };
  }

  // Type check
  const checkResult = checkProgram(resolveResult.program);
  allDiagnostics.push(...checkResult.diagnostics);

  // Enrich symbol table with type information (apply final substitution to resolve type vars)
  const symbolTable = resolveResult.symbolTable
    ? enrichWithTypes(
        resolveResult.symbolTable,
        checkResult.typeMap,
        checkResult.typeEnv,
        checkResult.subst,
      )
    : null;

  return {
    success: !allDiagnostics.some((d) => d.severity === "error"),
    diagnostics: allDiagnostics,
    symbolTable,
    checkOutput: checkResult,
    fileRegistry,
  };
};
