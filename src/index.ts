/**
 * Algow Compiler - CLI Entry Point
 *
 * Thin CLI that handles file I/O and delegates to the compile function.
 *
 * Commands:
 *   algow compile <files...>  - Compile to JavaScript
 *   algow run <files...>      - Compile and execute
 *   algow check <files...>    - Type check only
 */

import { Glob } from "bun";
import { Command } from "commander";
import {
  DEFAULT_TARGET,
  TARGETS,
  compile,
  isValidTarget,
  type SourceFile,
  type Target,
} from "./compile";
import type { Diagnostic } from "./diagnostics";
import { format } from "./format";
import { prelude } from "./prelude";

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
    printDiagnostic(diag);
  }
};

// =============================================================================
// Input Resolution
// =============================================================================

/** Check if a path is a directory. */
const isDirectory = async (path: string): Promise<boolean> => {
  try {
    const fs = await import("fs/promises");
    const stat = await fs.stat(path);
    return stat.isDirectory();
  } catch {
    return false;
  }
};

/** Resolve input patterns to source files (files, directories, globs, or - for stdin). */
const resolveInputs = async (patterns: string[]): Promise<SourceFile[]> => {
  const pathMod = await import("path");
  const sources: SourceFile[] = [];
  const seenPaths = new Set<string>();

  for (const pattern of patterns) {
    // Stdin
    if (pattern === "-") {
      const content = await Bun.stdin.text();
      sources.push({ path: "<stdin>", content });
      continue;
    }

    const resolved = pathMod.resolve(process.cwd(), pattern);

    // Directory - scan recursively for .alg files
    if (await isDirectory(resolved)) {
      const glob = new Glob("**/*.alg");
      for await (const file of glob.scan({ cwd: resolved, absolute: true })) {
        if (!seenPaths.has(file)) {
          seenPaths.add(file);
          const content = await Bun.file(file).text();
          sources.push({ path: file, content });
        }
      }
      continue;
    }

    // Glob pattern
    if (pattern.includes("*")) {
      const glob = new Glob(pattern);
      for await (const file of glob.scan({ cwd: process.cwd(), absolute: true })) {
        if (!seenPaths.has(file)) {
          seenPaths.add(file);
          const content = await Bun.file(file).text();
          sources.push({ path: file, content });
        }
      }
      continue;
    }

    // Single file
    if (!seenPaths.has(resolved)) {
      seenPaths.add(resolved);
      const content = await Bun.file(resolved).text();
      sources.push({ path: resolved, content });
    }
  }

  return sources;
};

// =============================================================================
// Commands
// =============================================================================

/** Load source files from patterns, including prelude. */
const loadSources = async (patterns: string[]): Promise<SourceFile[]> => {
  const userSources = await resolveInputs(patterns);

  if (userSources.length === 0) {
    console.error(`${RED}error${RESET}: No files matched the given patterns`);
    process.exit(1);
  }

  return [prelude, ...userSources];
};

/** Write output to file or stdout. */
const writeOutput = async (content: string, output?: string): Promise<void> => {
  if (output) {
    await Bun.write(output, content);
  } else {
    process.stdout.write(content);
  }
};

// =============================================================================
// CLI Setup
// =============================================================================

const program = new Command();

program
  .name("algow")
  .description("The Algow programming language compiler")
  .version("0.1.0")
  .enablePositionalOptions();

program
  .command("compile")
  .description("Compile source files to JavaScript")
  .argument("<input...>", "Source files, directories, globs, or - for stdin")
  .option(
    `-t, --target <target>`,
    `Target platform: ${TARGETS.join(", ")} (default: ${DEFAULT_TARGET})`,
  )
  .option("-o, --output <file>", "Write output to file instead of stdout")
  .option("-m, --minify", "Minify the output JavaScript with terser")
  .action(
    async (input: string[], options: { target?: string; output?: string; minify?: boolean }) => {
      const target = options.target;
      if (target && !isValidTarget(target)) {
        console.error(
          `${RED}error${RESET}: Invalid target "${target}". Valid targets: ${TARGETS.join(", ")}`,
        );
        process.exit(1);
      }

      const sources = await loadSources(input);
      const result = await compile(sources, {
        target: (target as Target) ?? DEFAULT_TARGET,
        minify: options.minify,
      });

      if (result.diagnostics.length > 0) {
        printDiagnostics(result.diagnostics, sources);
      }

      if (!result.success || !result.code) {
        process.exit(1);
      }

      await writeOutput(result.code, options.output);
    },
  );

program
  .command("check")
  .description("Type check source files")
  .argument("<input...>", "Source files, directories, globs, or - for stdin")
  .action(async (input: string[]) => {
    const sources = await loadSources(input);
    const result = await compile(sources, { typeCheckOnly: true });

    if (result.diagnostics.length > 0) {
      printDiagnostics(result.diagnostics, sources);
    }

    if (!result.success) {
      process.exit(1);
    }

    console.log(result.typeString ?? "(no expression)");
  });

program
  .command("run")
  .description("Compile and execute source files (use -- to pass args to program)")
  .argument("<input...>", "Source files, directories, globs, or - for stdin")
  .option(
    `-t, --target <target>`,
    `Target platform: ${TARGETS.join(", ")} (default: ${DEFAULT_TARGET})`,
  )
  .option("-m, --minify", "Minify the output JavaScript with terser")
  .action(async (_input: string[], options: { target?: string; minify?: boolean }) => {
    const target = options.target;
    if (target && !isValidTarget(target)) {
      console.error(
        `${RED}error${RESET}: Invalid target "${target}". Valid targets: ${TARGETS.join(", ")}`,
      );
      process.exit(1);
    }

    // Parse process.argv directly to handle -- correctly
    // Commander strips --, so we need to find it ourselves
    const argv = process.argv.slice(2); // Skip node and script
    const runIndex = argv.indexOf("run");
    const argsAfterRun = argv.slice(runIndex + 1);

    // Find -- in the original args
    const dashDashIndex = argsAfterRun.indexOf("--");
    let inputPatterns: string[];
    let programArgs: string[];

    if (dashDashIndex !== -1) {
      // Filter out options like -t/--target from input patterns
      const beforeDash = argsAfterRun.slice(0, dashDashIndex);
      inputPatterns = [];
      for (let i = 0; i < beforeDash.length; i++) {
        const arg = beforeDash[i]!;
        if (arg === "-t" || arg === "--target" || arg === "-m" || arg === "--minify") {
          if (arg === "-t" || arg === "--target") i++; // Skip the value too
        } else if (!arg.startsWith("-")) {
          inputPatterns.push(arg);
        }
      }
      programArgs = argsAfterRun.slice(dashDashIndex + 1);
    } else {
      inputPatterns = _input;
      programArgs = [];
    }

    if (inputPatterns.length === 0) {
      console.error(`${RED}error${RESET}: No input specified`);
      process.exit(1);
    }

    const sources = await loadSources(inputPatterns);
    const result = await compile(sources, {
      target: (target as Target) ?? DEFAULT_TARGET,
      minify: options.minify,
    });

    if (result.diagnostics.length > 0) {
      printDiagnostics(result.diagnostics, sources);
    }

    if (!result.success || !result.code) {
      process.exit(1);
    }

    // Execute with bun
    const proc = Bun.spawn(["bun", "-", ...programArgs], {
      stdin: new Response(result.code),
      stdout: "inherit",
      stderr: "inherit",
    });

    const exitCode = await proc.exited;
    process.exit(exitCode);
  });

program
  .command("format")
  .description("Format source files")
  .argument("<input...>", "Source files, directories, globs, or - for stdin")
  .option("-o, --output <file>", "Write output to file instead of stdout (single input only)")
  .option("-w, --write", "Write formatted output back to files")
  .option("--check", "Check if files are formatted (exit 1 if not)")
  .action(
    async (input: string[], options: { output?: string; write?: boolean; check?: boolean }) => {
      // Handle stdin
      if (input.length === 1 && input[0] === "-") {
        const content = await Bun.stdin.text();
        const result = format(content);

        if (result.diagnostics.length > 0) {
          for (const diag of result.diagnostics) {
            printDiagnostic(diag, content, "<stdin>");
          }
          process.exit(1);
        }

        await writeOutput(result.formatted, options.output);
        return;
      }

      // Resolve file patterns
      const sources = await resolveInputs(input);

      if (sources.length === 0) {
        console.error(`${RED}error${RESET}: No files matched the given patterns`);
        process.exit(1);
      }

      // -o/--output only works with single file
      if (options.output && sources.length > 1) {
        console.error(`${RED}error${RESET}: --output can only be used with a single input file`);
        process.exit(1);
      }

      let hasChanges = false;
      let hasErrors = false;

      for (const source of sources) {
        const result = format(source.content);

        if (result.diagnostics.length > 0) {
          console.error(`${RED}error${RESET}: ${source.path}`);
          for (const diag of result.diagnostics) {
            printDiagnostic(diag, source.content, source.path);
          }
          hasErrors = true;
          continue;
        }

        const isChanged = result.formatted !== source.content;

        if (options.check) {
          if (isChanged) {
            console.log(`${source.path}`);
            hasChanges = true;
          }
        } else if (options.write) {
          if (isChanged) {
            await Bun.write(source.path, result.formatted);
            console.log(`${GRAY}formatted${RESET} ${source.path}`);
          }
        } else if (options.output) {
          await writeOutput(result.formatted, options.output);
        } else {
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
    },
  );

program
  .command("lsp")
  .description("Start the Language Server Protocol server")
  .action(async () => {
    const { startLspServer } = await import("./lsp");
    startLspServer();
  });

program.parse();
