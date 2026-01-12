/**
 * Algow Compiler - CLI Entry Point
 *
 * Thin CLI that handles file I/O and delegates to the compile function.
 *
 * Commands:
 *   algow compile <files...>  - Compile to JavaScript
 *   algow check <files...>    - Type check only
 */

import { Command } from "commander";
import { Glob } from "bun";
import { compile, type SourceFile } from "./compile";
import { format } from "./format";
import type { Diagnostic } from "./diagnostics";

// =============================================================================
// ANSI Colors
// =============================================================================

const RED = "\x1b[31m";
const YELLOW = "\x1b[33m";
const CYAN = "\x1b[36m";
const GRAY = "\x1b[90m";
const RESET = "\x1b[0m";

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
// Diagnostic Printing
// =============================================================================

const printDiagnostic = (diag: Diagnostic, source?: string, filename?: string): void => {
  const severityColor =
    diag.severity === "error" ? RED : diag.severity === "warning" ? YELLOW : CYAN;
  const severityLabel =
    diag.severity === "error" ? "error" : diag.severity === "warning" ? "warning" : "note";

  console.error(`${severityColor}${severityLabel}${RESET}: ${diag.message}`);

  if (source && filename && diag.start > 0) {
    const { line, col } = offsetToLineCol(source, diag.start);
    const endPos = offsetToLineCol(source, diag.end);
    const lines = source.split("\n");
    const lineContent = lines[line - 1] ?? "";
    const lineNumWidth = String(line).length;
    const padding = " ".repeat(lineNumWidth + 1);

    console.error(`${GRAY}  --> ${filename}:${line}:${col}${RESET}`);
    console.error(`${GRAY}${padding}|${RESET}`);
    console.error(`${GRAY}${line} |${RESET} ${lineContent}`);

    const underlineStart = col - 1;
    const underlineLen =
      line === endPos.line ? Math.max(1, endPos.col - col) : lineContent.length - underlineStart;
    const underline = " ".repeat(underlineStart) + "^".repeat(Math.max(1, underlineLen));
    console.error(`${GRAY}${padding}|${RESET} ${RED}${underline}${RESET}`);
  }

  console.error();
};

const printDiagnostics = (
  diagnostics: readonly Diagnostic[],
  _sources: readonly SourceFile[],
): void => {
  for (const diag of diagnostics) {
    // For now, print without file context since we combined sources
    // TODO: Track source locations through compilation
    printDiagnostic(diag);
  }
};

// =============================================================================
// File Resolution
// =============================================================================

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

/** Get the path to the prelude file bundled with the compiler. */
const getPreludePath = (): string => {
  const url = new URL("../lib/prelude.alg", import.meta.url);
  return url.pathname;
};

/** Read source files from disk. */
const readSources = async (filePaths: string[]): Promise<SourceFile[]> => {
  const sources: SourceFile[] = [];

  for (const path of filePaths) {
    const content = await Bun.file(path).text();
    sources.push({ path, content });
  }

  return sources;
};

// =============================================================================
// Commands
// =============================================================================

/** Load source files from patterns, including prelude. */
const loadSources = async (patterns: string[]): Promise<SourceFile[]> => {
  const userFiles = await resolveFiles(patterns);

  if (userFiles.length === 0) {
    console.error(`${RED}error${RESET}: No files matched the given patterns`);
    process.exit(1);
  }

  const preludePath = getPreludePath();
  return readSources([preludePath, ...userFiles]);
};

/** Run compilation and handle diagnostics/exit. */
const run = async (patterns: string[], options: { typeCheckOnly: boolean }): Promise<void> => {
  const sources = await loadSources(patterns);
  const result = compile(sources, options);

  if (result.diagnostics.length > 0) {
    printDiagnostics(result.diagnostics, sources);
  }

  if (!result.success) {
    process.exit(1);
  }

  if (options.typeCheckOnly) {
    console.log(result.typeString ?? "(no expression)");
  } else if (result.code) {
    console.log(result.code);
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
  .action((files) => run(files, { typeCheckOnly: false }));

program
  .command("check")
  .description("Type check source files")
  .argument("<files...>", "Source files or glob patterns")
  .action((files) => run(files, { typeCheckOnly: true }));

program
  .command("format")
  .description("Format source files")
  .argument("<files...>", "Source files or glob patterns")
  .option("-w, --write", "Write formatted output back to files")
  .option("--check", "Check if files are formatted (exit 1 if not)")
  .action(async (files: string[], options: { write?: boolean; check?: boolean }) => {
    const filePaths = await resolveFiles(files);

    if (filePaths.length === 0) {
      console.error(`${RED}error${RESET}: No files matched the given patterns`);
      process.exit(1);
    }

    let hasChanges = false;
    let hasErrors = false;

    for (const filePath of filePaths) {
      const content = await Bun.file(filePath).text();
      const result = format(content);

      if (result.diagnostics.length > 0) {
        console.error(`${RED}error${RESET}: ${filePath}`);
        for (const diag of result.diagnostics) {
          printDiagnostic(diag, content, filePath);
        }
        hasErrors = true;
        continue;
      }

      const isChanged = result.formatted !== content;

      if (options.check) {
        if (isChanged) {
          console.log(`${filePath}`);
          hasChanges = true;
        }
      } else if (options.write) {
        if (isChanged) {
          await Bun.write(filePath, result.formatted);
          console.log(`${GRAY}formatted${RESET} ${filePath}`);
        }
      } else {
        // Print to stdout
        process.stdout.write(result.formatted);
      }
    }

    if (hasErrors) {
      process.exit(1);
    }

    if (options.check && hasChanges) {
      console.error(`\n${RED}error${RESET}: Some files need formatting`);
      process.exit(1);
    }
  });

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
    await run([file], { typeCheckOnly: !!options.typecheck });
  });

program.parse();
