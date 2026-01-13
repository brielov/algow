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
import {
  createFileRegistryBuilder,
  freezeFileRegistry,
  type FileRegistry,
  registerFile,
} from "./files";
import { lowerProgram } from "./lower";
import type { SymbolTable } from "./lsp/symbols";
import { freezeSymbolTableWithSubst } from "./lsp/symbols";
import { optimize } from "./optimize";
import { parse } from "./parser";
import { resolveProgram } from "./resolve";
import type { NodeIdGenerator, SDecl, SProgram } from "./surface";
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
  nodeIds: NodeIdGenerator;
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
  if (decl.kind === "SDeclLet" && decl.name.text === "main") return true;
  if (decl.kind === "SDeclLetRec") {
    return decl.bindings.some((b) => b.name.text === "main");
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
  // Thread nodeIds across files so each file continues from previous counter
  const registryBuilder = createFileRegistryBuilder();
  const parsedFiles: ParsedFile[] = [];
  const mainFiles: string[] = [];
  let sharedNodeIds: NodeIdGenerator | undefined;

  for (const { path, content } of sources) {
    const fileId = registerFile(registryBuilder, path, content);
    const parseResult = parse(content, fileId, sharedNodeIds);
    sharedNodeIds = parseResult.nodeIds; // Continue from where parser left off

    allDiagnostics.push(...parseResult.diagnostics);
    parsedFiles.push({
      path,
      source: content,
      program: parseResult.program,
      nodeIds: parseResult.nodeIds,
    });

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

  // Freeze file registry
  const fileRegistry = freezeFileRegistry(registryBuilder);

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

  // 3. Desugar (continue with shared nodeIds)
  const coreProgram = desugarProgram(combinedProgram, sharedNodeIds);

  // 4. Resolve names (continue with shared nodeIds)
  const resolveResult = resolveProgram(coreProgram, fileRegistry, sharedNodeIds);
  allDiagnostics.push(...resolveResult.diagnostics);

  if (allDiagnostics.some((d) => d.severity === "error")) {
    return { diagnostics: allDiagnostics, success: false };
  }

  // 5. Type check (types are recorded in symbol table builder)
  const checkResult = checkProgram(resolveResult.program, resolveResult.symbolTableBuilder);
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
  allDiagnostics.push(...lowerResult.diagnostics);

  if (allDiagnostics.some((d) => d.severity === "error")) {
    return { diagnostics: allDiagnostics, success: false };
  }

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

/**
 * Collect module exports from parsed declarations.
 * Returns a map from module name to the set of exported names.
 */
const collectModules = (decls: readonly SDecl[]): ModuleInfo => {
  const modules = new Map<string, Set<string>>();

  for (const decl of decls) {
    if (decl.kind === "SDeclModule") {
      const members = new Set<string>();
      for (const inner of decl.decls) {
        switch (inner.kind) {
          case "SDeclLet":
            members.add(inner.name.text);
            break;
          case "SDeclLetRec":
            for (const b of inner.bindings) {
              members.add(b.name.text);
            }
            break;
          case "SDeclForeign":
            members.add(inner.name.text);
            break;
          case "SDeclType":
            for (const ctor of inner.constructors) {
              members.add(ctor.name);
            }
            break;
        }
      }
      modules.set(decl.name, members);
    }
  }

  return modules;
};

// =============================================================================
// LSP Compilation
// =============================================================================

/** Module information for LSP completions */
export type ModuleInfo = ReadonlyMap<string, ReadonlySet<string>>;

/** Result of LSP-focused compilation */
export type LSPCompileResult = {
  /** Whether compilation succeeded (no errors) */
  readonly success: boolean;
  /** All diagnostics from all phases */
  readonly diagnostics: readonly Diagnostic[];
  /** Symbol table for LSP features */
  readonly symbolTable: SymbolTable;
  /** Type checker output (for type information) */
  readonly checkOutput: CheckOutput | null;
  /** File registry for position mapping */
  readonly fileRegistry: FileRegistry;
  /** Module names and their exports (for completions) */
  readonly modules: ModuleInfo;
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
  // File-aware spans: each span includes fileId, no offset adjustment needed
  // Thread nodeIds across files so each file continues from previous counter
  const registryBuilder = createFileRegistryBuilder();
  const parsedFiles: ParsedFile[] = [];
  let sharedNodeIds: NodeIdGenerator | undefined;

  for (const { path, content } of sources) {
    const fileId = registerFile(registryBuilder, path, content);
    const parseResult = parse(content, fileId, sharedNodeIds);
    sharedNodeIds = parseResult.nodeIds;
    allDiagnostics.push(...parseResult.diagnostics);
    parsedFiles.push({
      path,
      source: content,
      program: parseResult.program,
      nodeIds: parseResult.nodeIds,
    });
  }

  const fileRegistry = freezeFileRegistry(registryBuilder);

  // Check for parse errors - continue anyway for partial results
  const hasParseErrors = allDiagnostics.some((d) => d.severity === "error");

  // Combine all programs
  const combinedProgram = combinePrograms(parsedFiles);

  // Collect module information before desugaring (for LSP completions)
  const modules = collectModules(combinedProgram.decls);

  // Desugar (continue with shared nodeIds)
  const coreProgram = desugarProgram(combinedProgram, sharedNodeIds);

  // Resolve names (continue with shared nodeIds)
  const resolveResult = resolveProgram(coreProgram, fileRegistry, sharedNodeIds);
  allDiagnostics.push(...resolveResult.diagnostics);

  if (hasParseErrors) {
    // Can't proceed past parsing - freeze symbol table without type info
    return {
      success: false,
      diagnostics: allDiagnostics,
      symbolTable: freezeSymbolTableWithSubst(resolveResult.symbolTableBuilder, new Map()),
      checkOutput: null,
      fileRegistry,
      modules,
    };
  }

  // Type check (types are recorded in symbol table builder)
  const checkResult = checkProgram(resolveResult.program, resolveResult.symbolTableBuilder);
  allDiagnostics.push(...checkResult.diagnostics);

  // Freeze symbol table with final substitution to resolve type variables
  const symbolTable = freezeSymbolTableWithSubst(
    resolveResult.symbolTableBuilder,
    checkResult.subst,
  );

  return {
    success: !allDiagnostics.some((d) => d.severity === "error"),
    diagnostics: allDiagnostics,
    symbolTable,
    checkOutput: checkResult,
    fileRegistry,
    modules,
  };
};
