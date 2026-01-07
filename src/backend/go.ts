/**
 * Go Code Generator
 *
 * Generates Go code from IR.
 * Uses explicit closures since Go doesn't support currying natively.
 *
 * Generated code uses:
 * - interface{} (any) for all values
 * - Closure struct for functions with captured variables
 * - PureFunc struct for functions without captures
 * - Con struct for constructors
 * - []any for tuples
 * - map[string]any for records
 */

import type * as ir from "../ir";
import type { Type } from "../checker";
import { GO_RUNTIME } from "./go_runtime";

/** Helper for exhaustive switch checking */
const assertNever = (x: never): never => {
  throw new Error(`Unexpected value: ${JSON.stringify(x)}`);
};

// =============================================================================
// CODE GENERATION CONTEXT
// =============================================================================

type CodeGenContext = {
  /** Current indentation level */
  indent: number;
  /** Generated code lines */
  lines: string[];
  /** Set of constructor names */
  constructors: Set<string>;
  /** Lifted functions from closure conversion */
  functions: readonly ir.IRFunction[];
};

const createContext = (
  constructorNames: readonly string[],
  functions: readonly ir.IRFunction[],
): CodeGenContext => ({
  indent: 0,
  lines: [],
  constructors: new Set(constructorNames),
  functions,
});

/** Add a line of code with current indentation */
const emit = (ctx: CodeGenContext, code: string): void => {
  const indentation = "\t".repeat(ctx.indent);
  ctx.lines.push(indentation + code);
};

/** Generate a valid Go identifier */
const toGoId = (name: string): string => {
  // Handle environment access patterns like $env[0]
  if (name.startsWith("$env[")) {
    return name; // Keep as-is, will be handled specially
  }
  // Replace invalid characters and ensure it starts with letter/underscore
  let id = name.replace(/[^a-zA-Z0-9_]/g, "_");
  if (/^[0-9]/.test(id)) {
    id = "_" + id;
  }
  // Avoid Go keywords
  const goKeywords = new Set([
    "break",
    "case",
    "chan",
    "const",
    "continue",
    "default",
    "defer",
    "else",
    "fallthrough",
    "for",
    "func",
    "go",
    "goto",
    "if",
    "import",
    "interface",
    "map",
    "package",
    "range",
    "return",
    "select",
    "struct",
    "switch",
    "type",
    "var",
  ]);
  if (goKeywords.has(id)) {
    id = id + "_";
  }
  return id;
};

// =============================================================================
// EXPRESSION GENERATION
// =============================================================================

/**
 * Generate Go code for an IR expression.
 * Returns the Go expression as a string.
 */
const genExpr = (ctx: CodeGenContext, expr: ir.IRExpr): string => {
  switch (expr.kind) {
    case "IRAtomExpr":
      return genAtom(ctx, expr.atom);

    case "IRLet": {
      const binding = genBinding(ctx, expr.binding);
      const goName = toGoId(expr.name);
      emit(ctx, `${goName} := ${binding}`);
      return genExpr(ctx, expr.body);
    }

    case "IRLetRec": {
      // Declare all variables first
      for (const { name } of expr.bindings) {
        emit(ctx, `var ${toGoId(name)} Value`);
      }
      // Then assign values
      for (const { name, binding } of expr.bindings) {
        const bindingCode = genBinding(ctx, binding);
        emit(ctx, `${toGoId(name)} = ${bindingCode}`);
      }
      return genExpr(ctx, expr.body);
    }

    default:
      return assertNever(expr);
  }
};

/**
 * Generate Go for an atomic value.
 */
const genAtom = (ctx: CodeGenContext, atom: ir.IRAtom): string => {
  switch (atom.kind) {
    case "IRLit":
      if (typeof atom.value === "string") {
        // Strings are already Value-compatible
        return JSON.stringify(atom.value);
      }
      if (typeof atom.value === "boolean") {
        return atom.value ? "true" : "false";
      }
      // Numbers - wrap in Value() to make them interface{}
      // Use float64 for all numbers to match JS semantics
      return `Value(float64(${atom.value}))`;

    case "IRVar": {
      const name = atom.name;
      // Handle environment access
      if (name.startsWith("$env[")) {
        return name.replace("$env", "env");
      }
      // Check if this is a constructor
      if (ctx.constructors.has(name)) {
        return `NewCon("${name}")`;
      }
      return toGoId(name);
    }

    default:
      return assertNever(atom);
  }
};

/**
 * Generate Go for a binding.
 */
const genBinding = (ctx: CodeGenContext, binding: ir.IRBinding): string => {
  switch (binding.kind) {
    case "IRAtomBinding":
      return genAtom(ctx, binding.atom);

    case "IRAppBinding": {
      const func = genAtom(ctx, binding.func);
      const arg = genAtom(ctx, binding.arg);
      return `Apply(${func}, ${arg})`;
    }

    case "IRBinOpBinding":
      return genBinOp(ctx, binding);

    case "IRIfBinding":
      return genIf(ctx, binding);

    case "IRTupleBinding": {
      const elements = binding.elements.map((e) => genAtom(ctx, e));
      return `[]Value{${elements.join(", ")}}`;
    }

    case "IRRecordBinding": {
      const fields = binding.fields.map((f) => `"${f.name}": ${genAtom(ctx, f.value)}`);
      return `map[string]Value{${fields.join(", ")}}`;
    }

    case "IRFieldAccessBinding": {
      const record = genAtom(ctx, binding.record);
      return `${record}.(map[string]Value)["${binding.field}"]`;
    }

    case "IRTupleIndexBinding": {
      const tuple = genAtom(ctx, binding.tuple);
      return `${tuple}.([]Value)[${binding.index}]`;
    }

    case "IRMatchBinding":
      return genMatch(ctx, binding);

    case "IRLambdaBinding":
      return genLambda(ctx, binding);

    case "IRClosureBinding": {
      const captures = binding.captures.map((c) => genAtom(ctx, c));
      return `Closure{Fn: ${binding.funcId}, Env: []Value{${captures.join(", ")}}}`;
    }

    default:
      return assertNever(binding);
  }
};

/**
 * Generate Go for binary operations.
 */
const genBinOp = (_ctx: CodeGenContext, binding: ir.IRBinOpBinding): string => {
  const left = genAtom(_ctx, binding.left);
  const right = genAtom(_ctx, binding.right);

  // Check operand type for string operations
  const isString = isStringType(binding.operandType);

  // Helper to extract typed value from interface{}
  // Use float64 for all numbers to match JS semantics
  const asNum = (v: string) => `${v}.(float64)`;
  const asStr = (v: string) => `${v}.(string)`;

  switch (binding.op) {
    case "+":
      if (isString) {
        return `Value(${asStr(left)} + ${asStr(right)})`;
      }
      return `Value(${asNum(left)} + ${asNum(right)})`;

    case "-":
      return `Value(${asNum(left)} - ${asNum(right)})`;

    case "*":
      return `Value(${asNum(left)} * ${asNum(right)})`;

    case "/":
      return `Value(${asNum(left)} / ${asNum(right)})`;

    case "<":
      return `Value(${asNum(left)} < ${asNum(right)})`;

    case ">":
      return `Value(${asNum(left)} > ${asNum(right)})`;

    case "<=":
      return `Value(${asNum(left)} <= ${asNum(right)})`;

    case ">=":
      return `Value(${asNum(left)} >= ${asNum(right)})`;

    case "==":
      if (isComplexType(binding.operandType)) {
        return `Eq(${left}, ${right})`;
      }
      return `Value(${left} == ${right})`;

    case "!=":
      if (isComplexType(binding.operandType)) {
        return `!Eq(${left}, ${right})`;
      }
      return `Value(${left} != ${right})`;
  }
};

/** Check if type is string */
const isStringType = (type: Type): boolean => {
  return type.kind === "TCon" && type.name === "string";
};

/** Check if type requires deep equality */
const isComplexType = (type: Type): boolean => {
  switch (type.kind) {
    case "TCon":
      return !["number", "string", "boolean"].includes(type.name);
    case "TVar":
      return true;
    case "TFun":
    case "TApp":
    case "TRecord":
    case "TTuple":
      return true;
  }
};

/**
 * Generate Go for conditional.
 */
const genIf = (ctx: CodeGenContext, binding: ir.IRIfBinding): string => {
  const cond = genAtom(ctx, binding.cond);

  // Use immediately invoked function for complex branches
  const lines: string[] = [];
  lines.push(`func() Value {`);
  lines.push(`\tif ${cond}.(bool) {`);

  // Generate then branch
  ctx.indent += 2;
  const savedLines = ctx.lines;
  ctx.lines = [];
  const thenResult = genExpr(ctx, binding.thenBranch);
  const thenLines = ctx.lines;
  ctx.lines = savedLines;
  ctx.indent -= 2;

  for (const line of thenLines) {
    lines.push("\t\t" + line);
  }
  lines.push(`\t\treturn ${thenResult}`);
  lines.push(`\t} else {`);

  // Generate else branch
  ctx.indent += 2;
  ctx.lines = [];
  const elseResult = genExpr(ctx, binding.elseBranch);
  const elseLines = ctx.lines;
  ctx.lines = savedLines;
  ctx.indent -= 2;

  for (const line of elseLines) {
    lines.push("\t\t" + line);
  }
  lines.push(`\t\treturn ${elseResult}`);
  lines.push(`\t}`);
  lines.push(`}()`);

  return lines.join("\n" + "\t".repeat(ctx.indent));
};

/**
 * Generate Go for lambda.
 */
const genLambda = (ctx: CodeGenContext, binding: ir.IRLambdaBinding): string => {
  const param = toGoId(binding.param);

  // Generate body
  ctx.indent++;
  const savedLines = ctx.lines;
  ctx.lines = [];
  const bodyResult = genExpr(ctx, binding.body);
  const bodyLines = ctx.lines;
  ctx.lines = savedLines;
  ctx.indent--;

  const lines: string[] = [];
  lines.push(`PureFunc{Fn: func(${param} Value) Value {`);

  for (const line of bodyLines) {
    lines.push("\t" + line);
  }
  lines.push(`\treturn ${bodyResult}`);
  lines.push(`}}`);

  return lines.join("\n" + "\t".repeat(ctx.indent));
};

/**
 * Generate Go for pattern matching.
 */
const genMatch = (ctx: CodeGenContext, binding: ir.IRMatchBinding): string => {
  const scrutinee = genAtom(ctx, binding.scrutinee);

  const lines: string[] = [];
  lines.push(`func() Value {`);
  lines.push(`\t_s := ${scrutinee}`);

  // Check if all cases are constructor patterns - use switch
  const allConstructors = binding.cases.every((c) => c.pattern.kind === "IRPCon");
  const hasGuards = binding.cases.some((c) => c.guard !== undefined);

  if (allConstructors && !hasGuards) {
    lines.push(`\tswitch _s.(Con).Tag {`);

    for (const case_ of binding.cases) {
      const pattern = case_.pattern as ir.IRPCon;
      lines.push(`\tcase "${pattern.name}":`);

      // Generate bindings for constructor arguments
      const bindings = genPatternBindings("_s.(Con).Args", pattern);
      for (const [name, expr] of bindings) {
        lines.push(`\t\t${toGoId(name)} := ${expr}`);
      }

      // Generate body
      ctx.indent += 2;
      const savedLines = ctx.lines;
      ctx.lines = [];
      const bodyResult = genExpr(ctx, case_.body);
      const bodyLines = ctx.lines;
      ctx.lines = savedLines;
      ctx.indent -= 2;

      for (const line of bodyLines) {
        lines.push("\t\t" + line);
      }
      lines.push(`\t\treturn ${bodyResult}`);
    }

    lines.push(`\t}`);
  } else {
    // Fall back to if/else chain
    for (let i = 0; i < binding.cases.length; i++) {
      const case_ = binding.cases[i]!;
      const { condition, bindings } = genPatternMatch("_s", case_.pattern);

      const prefix = i === 0 ? "if" : "} else if";
      lines.push(`\t${prefix} ${condition} {`);

      // Emit bindings
      for (const [name, expr] of bindings) {
        lines.push(`\t\t${toGoId(name)} := ${expr}`);
      }

      // Generate guard if present
      if (case_.guard) {
        ctx.indent += 2;
        const savedLines = ctx.lines;
        ctx.lines = [];
        const guardResult = genExpr(ctx, case_.guard);
        const guardLines = ctx.lines;
        ctx.lines = savedLines;
        ctx.indent -= 2;

        for (const line of guardLines) {
          lines.push("\t\t" + line);
        }
        lines.push(`\t\tif ${guardResult}.(bool) {`);
      }

      // Generate body
      ctx.indent += 2;
      const savedLines = ctx.lines;
      ctx.lines = [];
      const bodyResult = genExpr(ctx, case_.body);
      const bodyLines = ctx.lines;
      ctx.lines = savedLines;
      ctx.indent -= 2;

      for (const line of bodyLines) {
        lines.push("\t\t" + (case_.guard ? "\t" : "") + line);
      }
      lines.push(`\t\t${case_.guard ? "\t" : ""}return ${bodyResult}`);

      if (case_.guard) {
        lines.push(`\t\t}`);
      }
    }
    lines.push(`\t}`);
  }

  lines.push(`\tpanic("Pattern match failed")`);
  lines.push(`}()`);

  return lines.join("\n" + "\t".repeat(ctx.indent));
};

/**
 * Generate bindings for constructor pattern arguments.
 */
const genPatternBindings = (argsExpr: string, pattern: ir.IRPCon): Array<[string, string]> => {
  const bindings: Array<[string, string]> = [];

  for (let i = 0; i < pattern.args.length; i++) {
    const arg = pattern.args[i]!;
    const argExpr = `${argsExpr}[${i}]`;

    if (arg.kind === "IRPVar") {
      bindings.push([arg.name, argExpr]);
    } else if (arg.kind === "IRPCon") {
      // Nested constructor - recurse
      const nestedBindings = genPatternBindings(`${argExpr}.(Con).Args`, arg);
      bindings.push(...nestedBindings);
    }
    // Other patterns handled by condition check
  }

  return bindings;
};

/**
 * Generate pattern match condition and bindings.
 */
const genPatternMatch = (
  scrutinee: string,
  pattern: ir.IRPattern,
): { condition: string; bindings: Array<[string, string]> } => {
  switch (pattern.kind) {
    case "IRPVar":
      return { condition: "true", bindings: [[pattern.name, scrutinee]] };

    case "IRPWildcard":
      return { condition: "true", bindings: [] };

    case "IRPLit": {
      const value =
        typeof pattern.value === "string"
          ? JSON.stringify(pattern.value)
          : typeof pattern.value === "boolean"
            ? pattern.value.toString()
            : `float64(${pattern.value})`;
      return { condition: `${scrutinee} == ${value}`, bindings: [] };
    }

    case "IRPCon": {
      const conditions: string[] = [`${scrutinee}.(Con).Tag == "${pattern.name}"`];
      const bindings: Array<[string, string]> = [];

      for (let i = 0; i < pattern.args.length; i++) {
        const argScrutinee = `${scrutinee}.(Con).Args[${i}]`;
        const result = genPatternMatch(argScrutinee, pattern.args[i]!);
        if (result.condition !== "true") {
          conditions.push(result.condition);
        }
        bindings.push(...result.bindings);
      }

      return { condition: conditions.join(" && "), bindings };
    }

    case "IRPTuple": {
      const conditions: string[] = [];
      const bindings: Array<[string, string]> = [];

      for (let i = 0; i < pattern.elements.length; i++) {
        const elemScrutinee = `${scrutinee}.([]Value)[${i}]`;
        const result = genPatternMatch(elemScrutinee, pattern.elements[i]!);
        if (result.condition !== "true") {
          conditions.push(result.condition);
        }
        bindings.push(...result.bindings);
      }

      return { condition: conditions.join(" && ") || "true", bindings };
    }

    case "IRPRecord": {
      const conditions: string[] = [];
      const bindings: Array<[string, string]> = [];

      for (const field of pattern.fields) {
        const fieldScrutinee = `${scrutinee}.(map[string]Value)["${field.name}"]`;
        const result = genPatternMatch(fieldScrutinee, field.pattern);
        if (result.condition !== "true") {
          conditions.push(result.condition);
        }
        bindings.push(...result.bindings);
      }

      return { condition: conditions.join(" && ") || "true", bindings };
    }

    case "IRPAs": {
      const result = genPatternMatch(scrutinee, pattern.pattern);
      result.bindings.push([pattern.name, scrutinee]);
      return result;
    }

    case "IRPOr": {
      const conditions: string[] = [];
      let bindings: Array<[string, string]> = [];

      for (let i = 0; i < pattern.alternatives.length; i++) {
        const result = genPatternMatch(scrutinee, pattern.alternatives[i]!);
        conditions.push(`(${result.condition})`);
        if (i === 0) {
          bindings = result.bindings;
        }
      }

      return { condition: conditions.join(" || "), bindings };
    }
  }
};

/**
 * Generate lifted functions.
 */
const genFunctions = (ctx: CodeGenContext): string[] => {
  const lines: string[] = [];

  for (const fn of ctx.functions) {
    const param = toGoId(fn.param);
    const envParam = toGoId(fn.envParam);

    lines.push(`var ${fn.id} = func(${envParam} []Value, ${param} Value) Value {`);

    // Generate body
    ctx.indent++;
    const savedLines = ctx.lines;
    ctx.lines = [];
    const bodyResult = genExpr(ctx, fn.body);
    const bodyLines = ctx.lines;
    ctx.lines = savedLines;
    ctx.indent--;

    for (const line of bodyLines) {
      lines.push("\t" + line);
    }
    lines.push(`\treturn ${bodyResult}`);
    lines.push(`}`);
    lines.push(``);
  }

  return lines;
};

// =============================================================================
// MAIN ENTRY POINT
// =============================================================================

/**
 * Result of code generation.
 */
export type GoCodeGenOutput = {
  readonly code: string;
  readonly warnings: readonly string[];
};

/**
 * Generate Go code from IR.
 *
 * @param irExpr The IR expression to generate code for
 * @param constructorNames Names of data constructors
 * @returns The generated Go code
 */
export const generateGo = (
  irExpr: ir.IRExpr,
  constructorNames: readonly string[],
): GoCodeGenOutput => {
  // For Go, we should use IRProgram from closure conversion
  // But for now, handle direct IR without closure conversion
  const ctx = createContext(constructorNames, []);

  // Generate the main expression
  const result = genExpr(ctx, irExpr);

  // Combine runtime + generated code
  const code = [
    GO_RUNTIME,
    "func main() {",
    ...ctx.lines.map((l) => "\t" + l),
    `\t_result := ${result}`,
    `\tfmt.Println(_result)`,
    "}",
  ].join("\n");

  return { code, warnings: [] };
};

/**
 * Generate Go code from IRProgram (after closure conversion).
 *
 * @param program The IR program to generate code for
 * @param constructorNames Names of data constructors
 * @returns The generated Go code
 */
export const generateGoFromProgram = (
  program: ir.IRProgram,
  constructorNames: readonly string[],
): GoCodeGenOutput => {
  const ctx = createContext(constructorNames, program.functions);

  // Generate lifted functions
  const funcLines = genFunctions(ctx);

  // Generate main expression
  const result = genExpr(ctx, program.main);

  // Combine runtime + functions + main
  const code = [
    GO_RUNTIME,
    ...funcLines,
    "func main() {",
    ...ctx.lines.map((l) => "\t" + l),
    `\t_result := ${result}`,
    `\tfmt.Println(_result)`,
    "}",
  ].join("\n");

  return { code, warnings: [] };
};
