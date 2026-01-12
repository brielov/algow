/**
 * Algow Compiler - CLI Entry Point
 *
 * Commands:
 *   algow compile <files...>  - Compile to JavaScript
 *   algow check <files...>    - Type check only
 *   algow lsp                 - Start LSP server
 */

import { Command } from "commander";
import { Glob } from "bun";
import { parse } from "./parser";
import { desugarProgram } from "./desugar";
import { resolveProgram } from "./resolve";
import { checkProgram, typeToString } from "./checker";
import { lowerProgram } from "./lower";
import { optimize } from "./optimize";
import { generateJS } from "./codegen";
import type { Diagnostic } from "./diagnostics";
import type { SDecl, SProgram } from "./surface";

// =============================================================================
// Position Utilities
// =============================================================================

/** Convert byte offset to line/column (1-indexed) */
const offsetToLineCol = (source: string, offset: number): { line: number; col: number } => {
  let line = 1;
  let col = 1;
  for (let i = 0; i < offset && i < source.length; i++) {
    if (source[i] === "\n") {
      line++;
      col = 1;
    } else {
      col++;
    }
  }
  return { line, col };
};

// =============================================================================
// ANSI Colors
// =============================================================================

const RED = "\x1b[31m";
const YELLOW = "\x1b[33m";
const CYAN = "\x1b[36m";
const GRAY = "\x1b[90m";
const RESET = "\x1b[0m";

// =============================================================================
// Diagnostic Printing
// =============================================================================

const printDiagnostics = (
  diagnostics: readonly Diagnostic[],
  source: string,
  filename: string,
): void => {
  for (const diag of diagnostics) {
    const { line, col } = offsetToLineCol(source, diag.start);
    const endPos = offsetToLineCol(source, diag.end);
    const lines = source.split("\n");
    const lineContent = lines[line - 1] ?? "";
    const lineNumWidth = String(line).length;
    const padding = " ".repeat(lineNumWidth + 1);

    const severityColor =
      diag.severity === "error" ? RED : diag.severity === "warning" ? YELLOW : CYAN;
    const severityLabel =
      diag.severity === "error" ? "error" : diag.severity === "warning" ? "warning" : "note";

    console.error(`${severityColor}${severityLabel}${RESET}: ${diag.message}`);
    console.error(`${GRAY}  --> ${filename}:${line}:${col}${RESET}`);
    console.error(`${GRAY}${padding}|${RESET}`);
    console.error(`${GRAY}${line} |${RESET} ${lineContent}`);

    const underlineStart = col - 1;
    const underlineLen =
      line === endPos.line ? Math.max(1, endPos.col - col) : lineContent.length - underlineStart;
    const underline = " ".repeat(underlineStart) + "^".repeat(Math.max(1, underlineLen));
    console.error(`${GRAY}${padding}|${RESET} ${RED}${underline}${RESET}`);
    console.error();
  }
};

// =============================================================================
// File Resolution
// =============================================================================

type SourceFile = {
  path: string;
  source: string;
  program: SProgram;
};

/** Resolve file patterns to actual file paths. */
const resolveFiles = async (patterns: string[]): Promise<string[]> => {
  const files: string[] = [];

  for (const pattern of patterns) {
    if (pattern.includes("*")) {
      // Glob pattern
      const glob = new Glob(pattern);
      for await (const file of glob.scan({ cwd: process.cwd(), absolute: true })) {
        files.push(file);
      }
    } else {
      // Single file - resolve to absolute path
      const path = await import("path");
      files.push(path.resolve(process.cwd(), pattern));
    }
  }

  return [...new Set(files)]; // Deduplicate
};

/**
 * Get the path to the prelude file bundled with the compiler.
 */
const getPreludePath = (): string => {
  // Resolve relative to this file's location
  const url = new URL("../lib/prelude.alg", import.meta.url);
  return url.pathname;
};

// =============================================================================
// Multi-file Compilation
// =============================================================================

/**
 * Parse all source files and validate structure.
 * Returns parsed files with exactly one having a tail expression.
 */
const parseFiles = async (
  filePaths: string[],
): Promise<{ files: SourceFile[]; entryIndex: number; diagnostics: Diagnostic[] }> => {
  const files: SourceFile[] = [];
  const diagnostics: Diagnostic[] = [];
  let entryIndex = -1;

  for (let i = 0; i < filePaths.length; i++) {
    const path = filePaths[i]!;
    const source = await Bun.file(path).text();
    const parseResult = parse(source);

    // Track diagnostics with file context
    for (const d of parseResult.diagnostics) {
      diagnostics.push(d);
      if (d.severity === "error") {
        printDiagnostics([d], source, path);
      }
    }

    files.push({ path, source, program: parseResult.program });

    // Check for tail expression (entry point)
    if (parseResult.program.expr !== null) {
      if (entryIndex !== -1) {
        console.error(`${RED}error${RESET}: Multiple files have tail expressions`);
        console.error(`${GRAY}  --> ${filePaths[entryIndex]}${RESET}`);
        console.error(`${GRAY}  --> ${path}${RESET}`);
        console.error();
        process.exit(1);
      }
      entryIndex = i;
    }
  }

  return { files, entryIndex, diagnostics };
};

/**
 * Combine all parsed files into a single program.
 * Entry file (with tail expression) comes last.
 */
const combinePrograms = (files: SourceFile[], entryIndex: number): SProgram => {
  const allDecls: SDecl[] = [];

  // Add all declarations, entry file last
  for (let i = 0; i < files.length; i++) {
    if (i !== entryIndex) {
      allDecls.push(...files[i]!.program.decls);
    }
  }

  // Entry file declarations and expression last
  if (entryIndex >= 0) {
    allDecls.push(...files[entryIndex]!.program.decls);
    return {
      decls: allDecls,
      expr: files[entryIndex]!.program.expr,
    };
  }

  return { decls: allDecls, expr: null };
};

// =============================================================================
// Commands
// =============================================================================

const compile = async (patterns: string[]): Promise<void> => {
  // Resolve file patterns
  const userFiles = await resolveFiles(patterns);

  if (userFiles.length === 0) {
    console.error(`${RED}error${RESET}: No files matched the given patterns`);
    process.exit(1);
  }

  // Add prelude first
  const preludePath = getPreludePath();
  const allFiles = [preludePath, ...userFiles];

  // Parse all files
  const { files, entryIndex, diagnostics } = await parseFiles(allFiles);

  if (diagnostics.some((d) => d.severity === "error")) {
    process.exit(1);
  }

  // Entry file must exist (and not be prelude)
  if (entryIndex <= 0) {
    console.error(`${RED}error${RESET}: No entry point found`);
    console.error(`${GRAY}hint${RESET}: One file must have a tail expression`);
    process.exit(1);
  }

  // Combine into single program
  const program = combinePrograms(files, entryIndex);

  // Desugar
  const coreProgram = desugarProgram(program);

  // Resolve names
  const resolveResult = resolveProgram(coreProgram);
  if (resolveResult.diagnostics.length > 0) {
    for (const d of resolveResult.diagnostics) {
      // Find which file this diagnostic belongs to
      // For now, print without file context
      const severityColor = d.severity === "error" ? RED : YELLOW;
      const severityLabel = d.severity === "error" ? "error" : "warning";
      console.error(`${severityColor}${severityLabel}${RESET}: ${d.message}`);
    }
    if (resolveResult.diagnostics.some((d) => d.severity === "error")) {
      process.exit(1);
    }
  }

  // Type check
  const checkResult = checkProgram(resolveResult.program);
  if (checkResult.diagnostics.length > 0) {
    for (const d of checkResult.diagnostics) {
      const severityColor = d.severity === "error" ? RED : YELLOW;
      const severityLabel = d.severity === "error" ? "error" : "warning";
      console.error(`${severityColor}${severityLabel}${RESET}: ${d.message}`);
    }
    if (checkResult.diagnostics.some((d) => d.severity === "error")) {
      process.exit(1);
    }
  }

  // Lower to IR
  const lowerResult = lowerProgram(resolveResult.program, checkResult);

  // Optimize
  const optimizedProgram = optimize(lowerResult.program);

  // Generate JavaScript
  const output = generateJS(optimizedProgram);

  console.log(output.code);
};

const check = async (patterns: string[]): Promise<void> => {
  // Resolve file patterns
  const userFiles = await resolveFiles(patterns);

  if (userFiles.length === 0) {
    console.error(`${RED}error${RESET}: No files matched the given patterns`);
    process.exit(1);
  }

  // Add prelude first
  const preludePath = getPreludePath();
  const allFiles = [preludePath, ...userFiles];

  // Parse all files
  const { files, entryIndex, diagnostics } = await parseFiles(allFiles);

  if (diagnostics.some((d) => d.severity === "error")) {
    process.exit(1);
  }

  // Combine into single program
  const program = combinePrograms(files, entryIndex);

  // Desugar
  const coreProgram = desugarProgram(program);

  // Resolve names
  const resolveResult = resolveProgram(coreProgram);
  if (resolveResult.diagnostics.length > 0) {
    for (const d of resolveResult.diagnostics) {
      const severityColor = d.severity === "error" ? RED : YELLOW;
      const severityLabel = d.severity === "error" ? "error" : "warning";
      console.error(`${severityColor}${severityLabel}${RESET}: ${d.message}`);
    }
    if (resolveResult.diagnostics.some((d) => d.severity === "error")) {
      process.exit(1);
    }
  }

  // Type check
  const checkResult = checkProgram(resolveResult.program);
  if (checkResult.diagnostics.length > 0) {
    for (const d of checkResult.diagnostics) {
      const severityColor = d.severity === "error" ? RED : YELLOW;
      const severityLabel = d.severity === "error" ? "error" : "warning";
      console.error(`${severityColor}${severityLabel}${RESET}: ${d.message}`);
    }
    if (checkResult.diagnostics.some((d) => d.severity === "error")) {
      process.exit(1);
    }
  }

  // Print inferred type
  if (checkResult.type) {
    console.log(typeToString(checkResult.type));
  } else {
    console.log("(no expression)");
  }
};

// =============================================================================
// CLI Setup
// =============================================================================

const program = new Command();

program.name("algow").description("The Algow programming language compiler").version("0.1.0");

program
  .command("compile")
  .description("Compile source files to JavaScript")
  .argument("<files...>", "Source files or glob patterns")
  .action(compile);

program
  .command("check")
  .description("Type check source files")
  .argument("<files...>", "Source files or glob patterns")
  .action(check);

// Default command for backwards compatibility
program
  .argument("[file]", "Source file to compile")
  .option("-c, --compile", "Compile to JavaScript (default)")
  .option("-t, --typecheck", "Type check only")
  .action(async (file, options) => {
    if (!file) {
      program.help();
      return;
    }

    if (options.typecheck) {
      await check([file]);
    } else {
      await compile([file]);
    }
  });

program.parse();
