import { createConstructorEnv, evaluate, type Env } from "./eval";
import {
  type ConstructorRegistry,
  infer,
  mergeEnvs,
  mergeRegistries,
  processDataDecl,
  typeToString,
  type TypeEnv,
} from "./infer";
import { type Diagnostic, parse, programToExpr } from "./parser";
import { declarations as preludeDeclarations } from "./prelude";

// ANSI color codes
const RED = "\x1b[31m";
const YELLOW = "\x1b[33m";
const CYAN = "\x1b[36m";
const GRAY = "\x1b[90m";
const RESET = "\x1b[0m";

/** Convert byte offset to line and column */
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

  const { declarations } = parseResult.program;
  const expr = programToExpr(parseResult.program);

  if (!expr) {
    console.error(`${filename}: No expression to evaluate`);
    process.exit(1);
  }

  // Start with prelude data declarations (Maybe, Either, List)
  let typeEnv: TypeEnv = new Map();
  let registry: ConstructorRegistry = new Map();
  const allConstructors: string[] = [];

  for (const decl of preludeDeclarations) {
    const [newEnv, newReg] = processDataDecl(decl);
    typeEnv = mergeEnvs(typeEnv, newEnv);
    registry = mergeRegistries(registry, newReg);
    for (const con of decl.constructors) {
      allConstructors.push(con.name);
    }
  }

  // Process user data declarations
  for (const decl of declarations) {
    const [newEnv, newReg] = processDataDecl(decl);
    typeEnv = mergeEnvs(typeEnv, newEnv);
    registry = mergeRegistries(registry, newReg);
    for (const con of decl.constructors) {
      allConstructors.push(con.name);
    }
  }

  // Type check
  const inferResult = infer(typeEnv, registry, expr);
  diagnostics.push(...inferResult.diagnostics);

  if (diagnostics.length > 0) {
    printDiagnostics(diagnostics, source, filename);
    process.exit(1);
  }

  // Evaluate with constructor environment
  const evalEnv: Env = createConstructorEnv(allConstructors);
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

  let typeEnv: TypeEnv = new Map();
  let registry: ConstructorRegistry = new Map();

  for (const decl of preludeDeclarations) {
    const [newEnv, newReg] = processDataDecl(decl);
    typeEnv = mergeEnvs(typeEnv, newEnv);
    registry = mergeRegistries(registry, newReg);
  }

  for (const decl of parseResult.program.declarations) {
    const [newEnv, newReg] = processDataDecl(decl);
    typeEnv = mergeEnvs(typeEnv, newEnv);
    registry = mergeRegistries(registry, newReg);
  }

  const inferResult = infer(typeEnv, registry, expr);

  if (inferResult.diagnostics.length > 0) {
    printDiagnostics(inferResult.diagnostics, source, filename);
    process.exit(1);
  }

  console.log(typeToString(inferResult.type));
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
