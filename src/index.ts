import { bindWithConstructors } from "./binder";
import { check, processDeclarations, typeToString } from "./checker";
import { createConstructorEnv, evaluate, type Env } from "./eval";
import { offsetToLineCol } from "./lsp/positions";
import { type Diagnostic, parse, programToExpr } from "./parser";
import { declarations as preludeDeclarations } from "./prelude";

// ANSI color codes
const RED = "\x1b[31m";
const YELLOW = "\x1b[33m";
const CYAN = "\x1b[36m";
const GRAY = "\x1b[90m";
const RESET = "\x1b[0m";

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

    // Error/warning prefix
    const prefix =
      diag.severity === "error"
        ? `${RED}error${RESET}`
        : diag.severity === "warning"
          ? `${YELLOW}warning${RESET}`
          : `${CYAN}note${RESET}`;

    // Header
    console.error(`${prefix}: ${diag.message}`);
    console.error(`${GRAY}  --> ${filename}:${line}:${col}${RESET}`);
    console.error(`${GRAY}${" ".repeat(lineNumWidth + 1)}|${RESET}`);

    // Source line
    console.error(`${GRAY}${line} |${RESET} ${lineContent}`);

    // Underline
    const underlineStart = col - 1;
    const underlineLen =
      line === endPos.line ? Math.max(1, endPos.col - col) : lineContent.length - underlineStart;
    const underline = " ".repeat(underlineStart) + "^".repeat(Math.max(1, underlineLen));
    console.error(`${GRAY}${" ".repeat(lineNumWidth + 1)}|${RESET} ${RED}${underline}${RESET}`);
    console.error();
  }
};

const run = (source: string, filename: string): void => {
  const diagnostics: Diagnostic[] = [];
  const parseResult = parse(source);
  diagnostics.push(...parseResult.diagnostics);

  const expr = programToExpr(parseResult.program);
  if (!expr) {
    console.error(`${filename}: No expression to evaluate`);
    process.exit(1);
  }

  // Process prelude + user data declarations
  const prelude = processDeclarations(preludeDeclarations);
  const { typeEnv, registry, constructorNames } = processDeclarations(
    parseResult.program.declarations,
    prelude,
  );

  // Bind and type check
  const bindResult = bindWithConstructors(constructorNames, expr);
  diagnostics.push(...bindResult.diagnostics);
  const checkResult = check(typeEnv, registry, expr, bindResult.symbols);
  diagnostics.push(...checkResult.diagnostics);

  if (diagnostics.length > 0) {
    printDiagnostics(diagnostics, source, filename);
    process.exit(1);
  }

  // Evaluate with constructor environment
  const evalEnv: Env = createConstructorEnv(constructorNames);
  const result = evaluate(evalEnv, expr);
  console.log(result);
};

const typeCheck = (source: string, filename: string): void => {
  const parseResult = parse(source);
  const expr = programToExpr(parseResult.program);

  if (parseResult.diagnostics.length > 0) {
    printDiagnostics(parseResult.diagnostics, source, filename);
    process.exit(1);
  }

  if (!expr) {
    console.error(`${filename}: No expression to type check`);
    process.exit(1);
  }

  // Process prelude + user data declarations
  const prelude = processDeclarations(preludeDeclarations);
  const { typeEnv, registry, constructorNames } = processDeclarations(
    parseResult.program.declarations,
    prelude,
  );

  // Bind and type check
  const bindResult = bindWithConstructors(constructorNames, expr);
  const checkResult = check(typeEnv, registry, expr, bindResult.symbols);

  const allDiagnostics = [...bindResult.diagnostics, ...checkResult.diagnostics];
  if (allDiagnostics.length > 0) {
    printDiagnostics(allDiagnostics, source, filename);
    process.exit(1);
  }

  console.log(typeToString(checkResult.type));
};

const main = async (): Promise<void> => {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    console.error("Usage: algow <file.alg>");
    console.error("       algow -e <expression>");
    console.error("       algow -t <file.alg>    (type check only)");
    process.exit(1);
  }

  try {
    if (args[0] === "-e" && args[1]) {
      run(args[1], "<stdin>");
    } else if (args[0] === "-t" && args[1]) {
      const source = await Bun.file(args[1]).text();
      typeCheck(source, args[1]);
    } else {
      const filename = args[0]!;
      const source = await Bun.file(filename).text();
      run(source, filename);
    }
  } catch (err) {
    console.error((err as Error).message);
    process.exit(1);
  }
};

void main();
