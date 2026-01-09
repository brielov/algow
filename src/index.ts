import * as path from "node:path";
import { bindWithConstructors } from "./binder";
import {
  check,
  processDeclarations,
  processModules,
  processUseStatements,
  typeToString,
} from "./checker";
import { createConstructorEnv, evaluate, valueToString, type Env } from "./eval";
import { generateJS } from "./backend/js";
import { generateGo } from "./backend/go";
import { lowerToIR } from "./lower";
import { offsetToLineCol } from "./lsp/positions";
import { optimize } from "./optimize";
import {
  type Diagnostic,
  parse,
  programToExpr,
  type Program,
  type TopLevelBinding,
} from "./parser";
import { modules as preludeModules } from "./prelude";
import * as ast from "./ast";

// =============================================================================
// MULTI-FILE SUPPORT TYPES
// =============================================================================

/** A parsed file with its source and metadata */
type FileProgram = {
  readonly filename: string;
  readonly program: Program;
  readonly source: string;
  readonly diagnostics: Diagnostic[];
};

/** A merged program from multiple files */
type MergedProgram = {
  readonly modules: ast.ModuleDecl[];
  readonly uses: ast.UseDecl[];
  readonly declarations: ast.TypeDecl[];
  readonly bindings: TopLevelBinding[];
  readonly expr: ast.Expr | null;
};

// ANSI color codes
const RED = "\x1b[31m";
const YELLOW = "\x1b[33m";
const CYAN = "\x1b[36m";
const GRAY = "\x1b[90m";
const RESET = "\x1b[0m";

// =============================================================================
// MULTI-FILE SUPPORT FUNCTIONS
// =============================================================================

/** Discover all .alg files in a directory recursively */
const discoverFiles = async (dir: string): Promise<string[]> => {
  const glob = new Bun.Glob("**/*.alg");
  const files: string[] = [];
  for await (const file of glob.scan({ cwd: dir, absolute: true })) {
    files.push(file);
  }
  return files.sort();
};

/** Load and parse a single file */
const loadFile = async (filename: string): Promise<FileProgram> => {
  const source = await Bun.file(filename).text();
  const parseResult = parse(source);
  return {
    filename,
    program: parseResult.program,
    source,
    diagnostics: [...parseResult.diagnostics],
  };
};

/** Detect duplicate module definitions across files */
const detectDuplicateModules = (files: readonly FileProgram[]): Diagnostic[] => {
  const moduleLocations = new Map<string, { file: string; span: ast.Span }[]>();

  for (const f of files) {
    for (const mod of f.program.modules) {
      const key = mod.name;
      if (!moduleLocations.has(key)) {
        moduleLocations.set(key, []);
      }
      moduleLocations.get(key)!.push({
        file: f.filename,
        span: mod.nameSpan ?? mod.span ?? { start: 0, end: 0 },
      });
    }
  }

  const diagnostics: Diagnostic[] = [];
  for (const [name, locations] of moduleLocations) {
    if (locations.length > 1) {
      for (const loc of locations) {
        const others = locations
          .filter((l) => l.file !== loc.file)
          .map((l) => path.basename(l.file))
          .join(", ");
        diagnostics.push({
          start: loc.span.start,
          end: loc.span.end,
          message: `Duplicate module '${name}' (also defined in: ${others})`,
          severity: "error",
          kind: "duplicate-definition",
          file: loc.file,
        });
      }
    }
  }
  return diagnostics;
};

/** Detect duplicate top-level binding definitions across files */
const detectDuplicateBindings = (files: readonly FileProgram[]): Diagnostic[] => {
  const bindingLocations = new Map<string, { file: string; span: ast.Span }[]>();

  for (const f of files) {
    for (const binding of f.program.bindings) {
      const key = binding.name;
      if (!bindingLocations.has(key)) {
        bindingLocations.set(key, []);
      }
      bindingLocations.get(key)!.push({
        file: f.filename,
        span: binding.nameSpan,
      });
    }
  }

  const diagnostics: Diagnostic[] = [];
  for (const [name, locations] of bindingLocations) {
    if (locations.length > 1) {
      for (const loc of locations) {
        const others = locations
          .filter((l) => l.file !== loc.file)
          .map((l) => path.basename(l.file))
          .join(", ");
        diagnostics.push({
          start: loc.span.start,
          end: loc.span.end,
          message: `Duplicate binding '${name}' (also defined in: ${others})`,
          severity: "error",
          kind: "duplicate-definition",
          file: loc.file,
        });
      }
    }
  }
  return diagnostics;
};

/** Detect duplicate type declarations across files */
const detectDuplicateDeclarations = (files: readonly FileProgram[]): Diagnostic[] => {
  const declLocations = new Map<string, { file: string; span: ast.Span }[]>();

  for (const f of files) {
    for (const decl of f.program.declarations) {
      const key = decl.name;
      if (!declLocations.has(key)) {
        declLocations.set(key, []);
      }
      declLocations.get(key)!.push({
        file: f.filename,
        span: decl.span ?? { start: 0, end: 0 },
      });
    }
  }

  const diagnostics: Diagnostic[] = [];
  for (const [name, locations] of declLocations) {
    if (locations.length > 1) {
      for (const loc of locations) {
        const others = locations
          .filter((l) => l.file !== loc.file)
          .map((l) => path.basename(l.file))
          .join(", ");
        diagnostics.push({
          start: loc.span.start,
          end: loc.span.end,
          message: `Duplicate type '${name}' (also defined in: ${others})`,
          severity: "error",
          kind: "duplicate-definition",
          file: loc.file,
        });
      }
    }
  }
  return diagnostics;
};

/** Merge multiple file programs into a single program */
const mergePrograms = (
  files: readonly FileProgram[],
  mainFile: FileProgram | undefined,
): MergedProgram => {
  const modules: ast.ModuleDecl[] = [];
  const uses: ast.UseDecl[] = [];
  const declarations: ast.TypeDecl[] = [];
  const bindings: TopLevelBinding[] = [];

  // Add non-main file bindings first, then main file bindings last.
  // This ensures main.alg bindings are evaluated after their dependencies.
  for (const f of files) {
    modules.push(...f.program.modules);
    uses.push(...f.program.uses);
    declarations.push(...f.program.declarations);
    if (f !== mainFile) {
      bindings.push(...f.program.bindings);
    }
  }
  if (mainFile) {
    bindings.push(...mainFile.program.bindings);
  }

  return {
    modules,
    uses,
    declarations,
    bindings,
    expr: mainFile?.program.expr ?? null,
  };
};

/** Get source map for multi-file diagnostics */
const getSourceMap = (files: readonly FileProgram[]): Map<string, string> => {
  const map = new Map<string, string>();
  for (const f of files) {
    map.set(f.filename, f.source);
  }
  return map;
};

/** Print diagnostics with multi-file support */
const printMultiFileDiagnostics = (
  diagnostics: readonly Diagnostic[],
  sourceMap: Map<string, string>,
  defaultFilename: string,
  defaultSource: string,
): void => {
  for (const diag of diagnostics) {
    const filename = diag.file ?? defaultFilename;
    const source = sourceMap.get(filename) ?? defaultSource;

    const { line, col } = offsetToLineCol(source, diag.start);
    const endPos = offsetToLineCol(source, diag.end);
    const lineContent = source.split("\n")[line - 1] ?? "";
    const lineNumWidth = String(line).length;
    const padding = " ".repeat(lineNumWidth + 1);

    const severityColor =
      diag.severity === "error" ? RED : diag.severity === "warning" ? YELLOW : CYAN;
    const severityLabel =
      diag.severity === "error" ? "error" : diag.severity === "warning" ? "warning" : "note";
    const kindLabel = diag.kind ? `[${diag.kind}]` : "";
    const prefix = `${severityColor}${severityLabel}${kindLabel}${RESET}`;

    console.error(`${prefix}: ${diag.message}`);
    console.error(`${GRAY}  --> ${filename}:${line}:${col}${RESET}`);
    console.error(`${GRAY}${padding}|${RESET}`);
    console.error(`${GRAY}${line} |${RESET} ${lineContent}`);

    const underlineStart = col - 1;
    const underlineLen =
      line === endPos.line ? Math.max(1, endPos.col - col) : lineContent.length - underlineStart;
    const underline = " ".repeat(underlineStart) + "^".repeat(Math.max(1, underlineLen));
    console.error(`${GRAY}${padding}|${RESET} ${RED}${underline}${RESET}`);

    if (diag.notes && diag.notes.length > 0) {
      console.error(`${GRAY}${padding}|${RESET}`);
      for (const note of diag.notes) {
        console.error(`${GRAY}${padding}=${RESET} note: ${note}`);
      }
    }

    console.error();
  }
};

// =============================================================================
// SINGLE-FILE SUPPORT FUNCTIONS
// =============================================================================

/** Get the line content at a given line number */
const getLine = (source: string, lineNum: number): string => {
  const lines = source.split("\n");
  return lines[lineNum - 1] ?? "";
};

/** Print diagnostics with source context */
const printDiagnostics = (
  diagnostics: readonly Diagnostic[],
  source: string,
  filename: string,
): void => {
  for (const diag of diagnostics) {
    const { line, col } = offsetToLineCol(source, diag.start);
    const endPos = offsetToLineCol(source, diag.end);
    const lineContent = getLine(source, line);
    const lineNumWidth = String(line).length;
    const padding = " ".repeat(lineNumWidth + 1);

    // Error/warning prefix with optional kind
    const severityColor =
      diag.severity === "error" ? RED : diag.severity === "warning" ? YELLOW : CYAN;
    const severityLabel =
      diag.severity === "error" ? "error" : diag.severity === "warning" ? "warning" : "note";
    const kindLabel = diag.kind ? `[${diag.kind}]` : "";
    const prefix = `${severityColor}${severityLabel}${kindLabel}${RESET}`;

    // Header
    console.error(`${prefix}: ${diag.message}`);
    console.error(`${GRAY}  --> ${filename}:${line}:${col}${RESET}`);
    console.error(`${GRAY}${padding}|${RESET}`);

    // Source line
    console.error(`${GRAY}${line} |${RESET} ${lineContent}`);

    // Underline
    const underlineStart = col - 1;
    const underlineLen =
      line === endPos.line ? Math.max(1, endPos.col - col) : lineContent.length - underlineStart;
    const underline = " ".repeat(underlineStart) + "^".repeat(Math.max(1, underlineLen));
    console.error(`${GRAY}${padding}|${RESET} ${RED}${underline}${RESET}`);

    // Expected/actual for type mismatches
    if (diag.expected || diag.actual) {
      console.error(`${GRAY}${padding}|${RESET}`);
      if (diag.expected) {
        console.error(`${GRAY}${padding}=${RESET} expected: ${CYAN}${diag.expected}${RESET}`);
      }
      if (diag.actual) {
        console.error(`${GRAY}${padding}=${RESET}   actual: ${RED}${diag.actual}${RESET}`);
      }
    }

    // Suggestions for unbound variables
    if (diag.suggestions && diag.suggestions.length > 0) {
      console.error(`${GRAY}${padding}|${RESET}`);
      const suggestionText =
        diag.suggestions.length === 1
          ? `did you mean: ${CYAN}${diag.suggestions[0]}${RESET}?`
          : `did you mean one of: ${diag.suggestions.map((s) => `${CYAN}${s}${RESET}`).join(", ")}?`;
      console.error(`${GRAY}${padding}=${RESET} ${suggestionText}`);
    }

    // Additional notes
    if (diag.notes && diag.notes.length > 0) {
      console.error(`${GRAY}${padding}|${RESET}`);
      for (const note of diag.notes) {
        console.error(`${GRAY}${padding}=${RESET} note: ${note}`);
      }
    }

    console.error();
  }
};

/**
 * Implicit use statements for essential prelude modules.
 * Only core data types and combinators are auto-imported.
 * Other modules (Int, Float, String, etc.) require explicit import or qualified access.
 */
const AUTO_IMPORT_MODULES = new Set(["Maybe", "Either", "List", "Core"]);

const preludeUses: ast.UseDecl[] = preludeModules
  .filter((mod) => AUTO_IMPORT_MODULES.has(mod.name))
  .map((mod) => ast.useDecl(mod.name, ast.importAll()));

/** Process a program with prelude */
const processProgram = (parseResult: ReturnType<typeof parse>) => {
  // Combine prelude + user modules
  const allModules = [...preludeModules, ...parseResult.program.modules];
  const allUses = [...preludeUses, ...parseResult.program.uses];

  // Process all modules first
  const moduleEnv = processModules(allModules);

  // Process prelude use statements first (no collision checking)
  const {
    localEnv: preludeEnv,
    localRegistry: preludeRegistry,
    constructorNames: preludeConstructors,
    aliases: preludeAliases,
    foreignFunctions: preludeForeign,
  } = processUseStatements(preludeUses, moduleEnv);

  // Process user use statements with collision checking against prelude
  const {
    localEnv: userEnv,
    localRegistry: userRegistry,
    constructorNames: userConstructors,
    aliases: userAliases,
    foreignFunctions: userForeign,
    diagnostics: useDiagnostics,
  } = processUseStatements(parseResult.program.uses, moduleEnv, preludeEnv);

  // Merge prelude and user environments
  const localEnv = new Map(preludeEnv);
  for (const [k, v] of userEnv) localEnv.set(k, v);

  const localRegistry = new Map(preludeRegistry);
  for (const [k, v] of userRegistry) localRegistry.set(k, v);

  // Collect constructors from imported modules (for unqualified access)
  const importedConstructors = [...preludeConstructors, ...userConstructors];

  // Collect constructors from ALL modules (for qualified access in wrapped bindings)
  // This is needed because programToExpr() wraps all module bindings, and those
  // bindings may reference constructors via internal use statements
  const allModuleConstructors: string[] = [];
  for (const [, info] of moduleEnv) {
    for (const name of info.constructorNames) {
      if (!allModuleConstructors.includes(name)) {
        allModuleConstructors.push(name);
      }
    }
  }

  const aliases = new Map(preludeAliases);
  for (const [k, v] of userAliases) aliases.set(k, v);

  const foreignFunctions = new Map(preludeForeign);
  for (const [k, v] of userForeign) foreignFunctions.set(k, v);

  // Process top-level type declarations
  const {
    typeEnv: declEnv,
    registry: declRegistry,
    aliasRegistry: declAliases,
    constructorNames: declConstructors,
  } = processDeclarations(parseResult.program.declarations);

  // Merge type declarations into the environment
  const typeEnv = new Map(localEnv);
  for (const [k, v] of declEnv) typeEnv.set(k, v);

  const registry = new Map(localRegistry);
  for (const [k, v] of declRegistry) registry.set(k, v);

  // Combine all constructor names: imported + all modules + top-level declarations
  const allConstructorNames = [
    ...new Set([...importedConstructors, ...allModuleConstructors, ...declConstructors]),
  ];

  return {
    typeEnv,
    registry,
    aliasRegistry: declAliases,
    constructorNames: allConstructorNames,
    allModules,
    allUses,
    moduleEnv,
    aliases,
    foreignFunctions,
    useDiagnostics,
  };
};

const run = (source: string, filename: string): void => {
  const diagnostics: Diagnostic[] = [];
  const parseResult = parse(source);
  diagnostics.push(...parseResult.diagnostics);

  const {
    typeEnv,
    registry,
    aliasRegistry,
    constructorNames,
    allModules,
    allUses,
    moduleEnv,
    aliases,
    useDiagnostics,
  } = processProgram(parseResult);
  diagnostics.push(...useDiagnostics);

  const expr = programToExpr(parseResult.program, allModules, allUses);
  if (!expr) {
    console.error(`${filename}: No expression to evaluate`);
    process.exit(1);
  }

  // Bind and type check
  const bindResult = bindWithConstructors(constructorNames, expr);
  diagnostics.push(...bindResult.diagnostics);
  const checkResult = check(
    typeEnv,
    registry,
    expr,
    bindResult.symbols,
    moduleEnv,
    aliases,
    aliasRegistry,
  );
  diagnostics.push(...checkResult.diagnostics);

  if (diagnostics.length > 0) {
    printDiagnostics(diagnostics, source, filename);
    process.exit(1);
  }

  // Evaluate with constructor environment
  const evalEnv: Env = createConstructorEnv(constructorNames);
  const result = evaluate(evalEnv, expr);
  console.log(valueToString(result));
};

const typeCheck = (source: string, filename: string): void => {
  const parseResult = parse(source);

  if (parseResult.diagnostics.length > 0) {
    printDiagnostics(parseResult.diagnostics, source, filename);
    process.exit(1);
  }

  const {
    typeEnv,
    registry,
    aliasRegistry,
    constructorNames,
    allModules,
    allUses,
    moduleEnv,
    aliases,
    useDiagnostics,
  } = processProgram(parseResult);

  if (useDiagnostics.length > 0) {
    printDiagnostics(useDiagnostics, source, filename);
    process.exit(1);
  }

  const expr = programToExpr(parseResult.program, allModules, allUses);
  if (!expr) {
    console.error(`${filename}: No expression to type check`);
    process.exit(1);
  }

  // Bind and type check
  const bindResult = bindWithConstructors(constructorNames, expr);
  const checkResult = check(
    typeEnv,
    registry,
    expr,
    bindResult.symbols,
    moduleEnv,
    aliases,
    aliasRegistry,
  );

  const allDiagnostics = [...bindResult.diagnostics, ...checkResult.diagnostics];
  if (allDiagnostics.length > 0) {
    printDiagnostics(allDiagnostics, source, filename);
    process.exit(1);
  }

  console.log(typeToString(checkResult.type));
};

const emitIR = (source: string, filename: string): void => {
  const parseResult = parse(source);

  if (parseResult.diagnostics.length > 0) {
    printDiagnostics(parseResult.diagnostics, source, filename);
    process.exit(1);
  }

  const {
    typeEnv,
    registry,
    aliasRegistry,
    constructorNames,
    allModules,
    allUses,
    moduleEnv,
    aliases,
    foreignFunctions,
    useDiagnostics,
  } = processProgram(parseResult);

  if (useDiagnostics.length > 0) {
    printDiagnostics(useDiagnostics, source, filename);
    process.exit(1);
  }

  const expr = programToExpr(parseResult.program, allModules, allUses);
  if (!expr) {
    console.error(`${filename}: No expression to lower`);
    process.exit(1);
  }

  // Bind and type check
  const bindResult = bindWithConstructors(constructorNames, expr);
  const checkResult = check(
    typeEnv,
    registry,
    expr,
    bindResult.symbols,
    moduleEnv,
    aliases,
    aliasRegistry,
  );

  const allDiagnostics = [...bindResult.diagnostics, ...checkResult.diagnostics];
  if (allDiagnostics.length > 0) {
    printDiagnostics(allDiagnostics, source, filename);
    process.exit(1);
  }

  // Lower to IR
  const ir = lowerToIR(expr, typeEnv, checkResult, foreignFunctions, moduleEnv);
  console.log(JSON.stringify(ir, null, 2));
};

const compile = (source: string, filename: string): void => {
  const parseResult = parse(source);

  if (parseResult.diagnostics.length > 0) {
    printDiagnostics(parseResult.diagnostics, source, filename);
    process.exit(1);
  }

  const {
    typeEnv,
    registry,
    aliasRegistry,
    constructorNames,
    allModules,
    allUses,
    moduleEnv,
    aliases,
    foreignFunctions,
    useDiagnostics,
  } = processProgram(parseResult);

  if (useDiagnostics.length > 0) {
    printDiagnostics(useDiagnostics, source, filename);
    process.exit(1);
  }

  const expr = programToExpr(parseResult.program, allModules, allUses);
  if (!expr) {
    console.error(`${filename}: No expression to compile`);
    process.exit(1);
  }

  // Bind and type check
  const bindResult = bindWithConstructors(constructorNames, expr);
  const checkResult = check(
    typeEnv,
    registry,
    expr,
    bindResult.symbols,
    moduleEnv,
    aliases,
    aliasRegistry,
  );

  const allDiagnostics = [...bindResult.diagnostics, ...checkResult.diagnostics];
  if (allDiagnostics.length > 0) {
    printDiagnostics(allDiagnostics, source, filename);
    process.exit(1);
  }

  // Lower to IR, optimize, and generate JS
  let ir = lowerToIR(expr, typeEnv, checkResult, foreignFunctions, moduleEnv);
  ir = optimize(ir);
  const output = generateJS(ir, constructorNames);

  // Print any warnings
  for (const warning of output.warnings) {
    console.warn(`Warning: ${warning}`);
  }

  console.log(output.code);
};

const compileToGo = (source: string, filename: string): void => {
  const parseResult = parse(source);

  if (parseResult.diagnostics.length > 0) {
    printDiagnostics(parseResult.diagnostics, source, filename);
    process.exit(1);
  }

  const {
    typeEnv,
    registry,
    aliasRegistry,
    constructorNames,
    allModules,
    allUses,
    moduleEnv,
    aliases,
    foreignFunctions,
    useDiagnostics,
  } = processProgram(parseResult);

  if (useDiagnostics.length > 0) {
    printDiagnostics(useDiagnostics, source, filename);
    process.exit(1);
  }

  const expr = programToExpr(parseResult.program, allModules, allUses);
  if (!expr) {
    console.error(`${filename}: No expression to compile`);
    process.exit(1);
  }

  // Bind and type check
  const bindResult = bindWithConstructors(constructorNames, expr);
  const checkResult = check(
    typeEnv,
    registry,
    expr,
    bindResult.symbols,
    moduleEnv,
    aliases,
    aliasRegistry,
  );

  const allDiagnostics = [...bindResult.diagnostics, ...checkResult.diagnostics];
  if (allDiagnostics.length > 0) {
    printDiagnostics(allDiagnostics, source, filename);
    process.exit(1);
  }

  // Lower to IR, optimize, and generate Go
  let ir = lowerToIR(expr, typeEnv, checkResult, foreignFunctions, moduleEnv);
  ir = optimize(ir);
  const output = generateGo(ir, constructorNames);

  // Print any warnings
  for (const warning of output.warnings) {
    console.warn(`Warning: ${warning}`);
  }

  console.log(output.code);
};

// =============================================================================
// DIRECTORY MODE FUNCTIONS
// =============================================================================

/** Process a merged program (multi-file version of processProgram) */
const processMergedProgram = (merged: MergedProgram) => {
  // Combine prelude + user modules
  const allModules = [...preludeModules, ...merged.modules];
  const allUses = [...preludeUses, ...merged.uses];

  // Process all modules first
  const moduleEnv = processModules(allModules);

  // Process prelude use statements first (no collision checking)
  const {
    localEnv: preludeEnv,
    localRegistry: preludeRegistry,
    constructorNames: preludeConstructors,
    aliases: preludeAliases,
    foreignFunctions: preludeForeign,
  } = processUseStatements(preludeUses, moduleEnv);

  // Process user use statements with collision checking against prelude
  const {
    localEnv: userEnv,
    localRegistry: userRegistry,
    constructorNames: userConstructors,
    aliases: userAliases,
    foreignFunctions: userForeign,
    diagnostics: useDiagnostics,
  } = processUseStatements(merged.uses, moduleEnv, preludeEnv);

  // Merge prelude and user environments
  const localEnv = new Map(preludeEnv);
  for (const [k, v] of userEnv) localEnv.set(k, v);

  const localRegistry = new Map(preludeRegistry);
  for (const [k, v] of userRegistry) localRegistry.set(k, v);

  // Collect constructors from imported modules
  const importedConstructors = [...preludeConstructors, ...userConstructors];

  // Collect constructors from ALL modules
  const allModuleConstructors: string[] = [];
  for (const [, info] of moduleEnv) {
    for (const name of info.constructorNames) {
      if (!allModuleConstructors.includes(name)) {
        allModuleConstructors.push(name);
      }
    }
  }

  const aliases = new Map(preludeAliases);
  for (const [k, v] of userAliases) aliases.set(k, v);

  const foreignFunctions = new Map(preludeForeign);
  for (const [k, v] of userForeign) foreignFunctions.set(k, v);

  // Process top-level type declarations
  const {
    typeEnv: declEnv,
    registry: declRegistry,
    aliasRegistry: declAliases,
    constructorNames: declConstructors,
  } = processDeclarations(merged.declarations);

  // Merge type declarations into the environment
  const typeEnv = new Map(localEnv);
  for (const [k, v] of declEnv) typeEnv.set(k, v);

  const registry = new Map(localRegistry);
  for (const [k, v] of declRegistry) registry.set(k, v);

  // Combine all constructor names
  const allConstructorNames = [
    ...new Set([...importedConstructors, ...allModuleConstructors, ...declConstructors]),
  ];

  return {
    typeEnv,
    registry,
    aliasRegistry: declAliases,
    constructorNames: allConstructorNames,
    allModules,
    allUses,
    moduleEnv,
    aliases,
    foreignFunctions,
    useDiagnostics,
  };
};

/** Check if path is a directory */
const isDirectory = async (filepath: string): Promise<boolean> => {
  try {
    const stat = await Bun.file(filepath).exists();
    if (stat) return false; // It's a file
    // Try to scan it as a directory
    const glob = new Bun.Glob("*");
    for await (const _ of glob.scan({ cwd: filepath })) {
      return true; // Has at least one entry, it's a directory
    }
    return true; // Empty directory
  } catch {
    return false;
  }
};

/** Run multi-file program from directory */
const runDirectory = async (dir: string): Promise<void> => {
  // 1. Discover files
  const filePaths = await discoverFiles(dir);
  if (filePaths.length === 0) {
    console.error(`No .alg files found in '${dir}'`);
    process.exit(1);
  }

  // 2. Load and parse all files
  const files = await Promise.all(filePaths.map(loadFile));
  const sourceMap = getSourceMap(files);

  // 3. Collect parse diagnostics
  const diagnostics: Diagnostic[] = [];
  for (const f of files) {
    for (const d of f.diagnostics) {
      diagnostics.push({ ...d, file: f.filename });
    }
  }

  // 4. Detect conflicts
  diagnostics.push(
    ...detectDuplicateModules(files),
    ...detectDuplicateBindings(files),
    ...detectDuplicateDeclarations(files),
  );

  // 5. Find entry point (main.alg)
  const mainFile = files.find((f) => path.basename(f.filename) === "main.alg");
  if (!mainFile) {
    console.error(`No main.alg found in '${dir}'`);
    console.error(
      "hint: Create a main.alg file with an expression to run, or use -t for type-check only",
    );
    process.exit(1);
  }

  // 6. Check for expressions in non-main files
  for (const f of files) {
    if (f !== mainFile && f.program.expr) {
      diagnostics.push({
        start: 0,
        end: 0,
        message: "Only main.alg can have a trailing expression",
        severity: "error",
        file: f.filename,
        notes: ["Move this expression to main.alg or wrap it in a let binding"],
      });
    }
  }

  if (diagnostics.some((d) => d.severity === "error")) {
    printMultiFileDiagnostics(diagnostics, sourceMap, mainFile.filename, mainFile.source);
    process.exit(1);
  }

  // 7. Merge programs
  const merged = mergePrograms(files, mainFile);

  // 8. Process merged program
  const {
    typeEnv,
    registry,
    aliasRegistry,
    constructorNames,
    allModules,
    allUses,
    moduleEnv,
    aliases,
    useDiagnostics,
  } = processMergedProgram(merged);

  for (const d of useDiagnostics) {
    diagnostics.push(d);
  }

  // 9. Convert to expression
  const program: Program = {
    modules: merged.modules,
    uses: merged.uses,
    declarations: merged.declarations,
    bindings: merged.bindings,
    expr: merged.expr,
  };
  const expr = programToExpr(program, allModules, allUses);
  if (!expr) {
    console.error("No expression to evaluate");
    process.exit(1);
  }

  // 10. Bind and type check
  const bindResult = bindWithConstructors(constructorNames, expr);
  for (const d of bindResult.diagnostics) {
    diagnostics.push(d);
  }
  const checkResult = check(
    typeEnv,
    registry,
    expr,
    bindResult.symbols,
    moduleEnv,
    aliases,
    aliasRegistry,
  );
  for (const d of checkResult.diagnostics) {
    diagnostics.push(d);
  }

  if (diagnostics.some((d) => d.severity === "error")) {
    printMultiFileDiagnostics(diagnostics, sourceMap, mainFile.filename, mainFile.source);
    process.exit(1);
  }

  // 11. Evaluate
  const evalEnv: Env = createConstructorEnv(constructorNames);
  const result = evaluate(evalEnv, expr);
  console.log(valueToString(result));
};

/** Type-check multi-file program from directory */
const typeCheckDirectory = async (dir: string): Promise<void> => {
  // 1. Discover files
  const filePaths = await discoverFiles(dir);
  if (filePaths.length === 0) {
    console.error(`No .alg files found in '${dir}'`);
    process.exit(1);
  }

  // 2. Load and parse all files
  const files = await Promise.all(filePaths.map(loadFile));
  const sourceMap = getSourceMap(files);

  // 3. Collect parse diagnostics
  const diagnostics: Diagnostic[] = [];
  for (const f of files) {
    for (const d of f.diagnostics) {
      diagnostics.push({ ...d, file: f.filename });
    }
  }

  // 4. Detect conflicts
  diagnostics.push(
    ...detectDuplicateModules(files),
    ...detectDuplicateBindings(files),
    ...detectDuplicateDeclarations(files),
  );

  // 5. Find entry point (main.alg) - optional for type-check
  const mainFile = files.find((f) => path.basename(f.filename) === "main.alg");

  // For type-check mode, we don't require main.alg
  // Just merge all files and type-check

  if (diagnostics.some((d) => d.severity === "error")) {
    const defaultFile = mainFile ?? files[0]!;
    printMultiFileDiagnostics(diagnostics, sourceMap, defaultFile.filename, defaultFile.source);
    process.exit(1);
  }

  // 6. Merge programs
  const merged = mergePrograms(files, mainFile);

  // 7. Process merged program
  const {
    typeEnv,
    registry,
    aliasRegistry,
    constructorNames,
    allModules,
    allUses,
    moduleEnv,
    aliases,
    useDiagnostics,
  } = processMergedProgram(merged);

  for (const d of useDiagnostics) {
    diagnostics.push(d);
  }

  // 8. Convert to expression
  const program: Program = {
    modules: merged.modules,
    uses: merged.uses,
    declarations: merged.declarations,
    bindings: merged.bindings,
    expr: merged.expr,
  };
  const expr = programToExpr(program, allModules, allUses);
  if (!expr) {
    console.log("(no expression)");
    return;
  }

  // 9. Bind and type check
  const bindResult = bindWithConstructors(constructorNames, expr);
  for (const d of bindResult.diagnostics) {
    diagnostics.push(d);
  }
  const checkResult = check(
    typeEnv,
    registry,
    expr,
    bindResult.symbols,
    moduleEnv,
    aliases,
    aliasRegistry,
  );
  for (const d of checkResult.diagnostics) {
    diagnostics.push(d);
  }

  if (diagnostics.some((d) => d.severity === "error")) {
    const defaultFile = mainFile ?? files[0]!;
    printMultiFileDiagnostics(diagnostics, sourceMap, defaultFile.filename, defaultFile.source);
    process.exit(1);
  }

  console.log(typeToString(checkResult.type));
};

/** Compile multi-file program from directory to JS */
const compileDirectory = async (dir: string): Promise<void> => {
  // 1. Discover files
  const filePaths = await discoverFiles(dir);
  if (filePaths.length === 0) {
    console.error(`No .alg files found in '${dir}'`);
    process.exit(1);
  }

  // 2. Load and parse all files
  const files = await Promise.all(filePaths.map(loadFile));
  const sourceMap = getSourceMap(files);

  // 3. Collect parse diagnostics
  const diagnostics: Diagnostic[] = [];
  for (const f of files) {
    for (const d of f.diagnostics) {
      diagnostics.push({ ...d, file: f.filename });
    }
  }

  // 4. Detect conflicts
  diagnostics.push(
    ...detectDuplicateModules(files),
    ...detectDuplicateBindings(files),
    ...detectDuplicateDeclarations(files),
  );

  // 5. Find entry point (main.alg)
  const mainFile = files.find((f) => path.basename(f.filename) === "main.alg");
  if (!mainFile) {
    console.error(`No main.alg found in '${dir}'`);
    console.error("hint: Create a main.alg file with an expression to compile");
    process.exit(1);
  }

  if (diagnostics.some((d) => d.severity === "error")) {
    printMultiFileDiagnostics(diagnostics, sourceMap, mainFile.filename, mainFile.source);
    process.exit(1);
  }

  // 6. Merge programs
  const merged = mergePrograms(files, mainFile);

  // 7. Process merged program
  const {
    typeEnv,
    registry,
    aliasRegistry,
    constructorNames,
    allModules,
    allUses,
    moduleEnv,
    aliases,
    foreignFunctions,
    useDiagnostics,
  } = processMergedProgram(merged);

  for (const d of useDiagnostics) {
    diagnostics.push(d);
  }

  // 8. Convert to expression
  const program: Program = {
    modules: merged.modules,
    uses: merged.uses,
    declarations: merged.declarations,
    bindings: merged.bindings,
    expr: merged.expr,
  };
  const expr = programToExpr(program, allModules, allUses);
  if (!expr) {
    console.error("No expression to compile");
    process.exit(1);
  }

  // 9. Bind and type check
  const bindResult = bindWithConstructors(constructorNames, expr);
  for (const d of bindResult.diagnostics) {
    diagnostics.push(d);
  }
  const checkResult = check(
    typeEnv,
    registry,
    expr,
    bindResult.symbols,
    moduleEnv,
    aliases,
    aliasRegistry,
  );
  for (const d of checkResult.diagnostics) {
    diagnostics.push(d);
  }

  if (diagnostics.some((d) => d.severity === "error")) {
    printMultiFileDiagnostics(diagnostics, sourceMap, mainFile.filename, mainFile.source);
    process.exit(1);
  }

  // 10. Lower to IR, optimize, and generate JS
  let ir = lowerToIR(expr, typeEnv, checkResult, foreignFunctions, moduleEnv);
  ir = optimize(ir);
  const output = generateJS(ir, constructorNames);

  for (const warning of output.warnings) {
    console.warn(`Warning: ${warning}`);
  }

  console.log(output.code);
};

// =============================================================================
// MAIN ENTRY POINT
// =============================================================================

const main = async (): Promise<void> => {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    console.error("Usage: algow <file.alg>          (run file)");
    console.error("       algow <directory>         (run multi-file project)");
    console.error("       algow -e <expression>     (run inline expression)");
    console.error("       algow -t <file|dir>       (type check only)");
    console.error("       algow -c <file|dir>       (compile to JS)");
    console.error("       algow --go <file.alg>     (compile to Go)");
    console.error("       algow --emit-ir <file.alg> (emit IR)");
    process.exit(1);
  }

  try {
    if (args[0] === "-e" && args[1]) {
      run(args[1], "<stdin>");
    } else if (args[0] === "-t" && args[1]) {
      const target = args[1];
      if (await isDirectory(target)) {
        await typeCheckDirectory(target);
      } else {
        const source = await Bun.file(target).text();
        typeCheck(source, target);
      }
    } else if (args[0] === "-c" && args[1]) {
      const target = args[1];
      if (await isDirectory(target)) {
        await compileDirectory(target);
      } else {
        const source = await Bun.file(target).text();
        compile(source, target);
      }
    } else if (args[0] === "--go" && args[1]) {
      const source = await Bun.file(args[1]).text();
      compileToGo(source, args[1]);
    } else if (args[0] === "--emit-ir" && args[1]) {
      const source = await Bun.file(args[1]).text();
      emitIR(source, args[1]);
    } else {
      const target = args[0]!;
      if (await isDirectory(target)) {
        await runDirectory(target);
      } else {
        const source = await Bun.file(target).text();
        run(source, target);
      }
    }
  } catch (err) {
    console.error((err as Error).message);
    process.exit(1);
  }
};

void main();
