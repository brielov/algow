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
import { type Diagnostic, parse, programToExpr } from "./parser";
import { modules as preludeModules } from "./prelude";
import * as ast from "./ast";

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

/** Implicit use statements for prelude modules (import everything) */
const preludeUses: ast.UseDecl[] = preludeModules.map((mod) =>
  ast.useDecl(mod.name, ast.importAll()),
);

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
  const checkResult = check(typeEnv, registry, expr, bindResult.symbols, moduleEnv, aliases);
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
  const checkResult = check(typeEnv, registry, expr, bindResult.symbols, moduleEnv, aliases);

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
  const checkResult = check(typeEnv, registry, expr, bindResult.symbols, moduleEnv, aliases);

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
  const checkResult = check(typeEnv, registry, expr, bindResult.symbols, moduleEnv, aliases);

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
  const checkResult = check(typeEnv, registry, expr, bindResult.symbols, moduleEnv, aliases);

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

const main = async (): Promise<void> => {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    console.error("Usage: algow <file.alg>");
    console.error("       algow -e <expression>");
    console.error("       algow -t <file.alg>       (type check only)");
    console.error("       algow -c <file.alg>       (compile to JS)");
    console.error("       algow --go <file.alg>     (compile to Go)");
    console.error("       algow --emit-ir <file.alg> (emit IR)");
    process.exit(1);
  }

  try {
    if (args[0] === "-e" && args[1]) {
      run(args[1], "<stdin>");
    } else if (args[0] === "-t" && args[1]) {
      const source = await Bun.file(args[1]).text();
      typeCheck(source, args[1]);
    } else if (args[0] === "-c" && args[1]) {
      const source = await Bun.file(args[1]).text();
      compile(source, args[1]);
    } else if (args[0] === "--go" && args[1]) {
      const source = await Bun.file(args[1]).text();
      compileToGo(source, args[1]);
    } else if (args[0] === "--emit-ir" && args[1]) {
      const source = await Bun.file(args[1]).text();
      emitIR(source, args[1]);
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
