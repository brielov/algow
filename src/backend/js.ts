/**
 * JavaScript Code Generator
 *
 * Generates JavaScript code from the IR.
 * Uses native JavaScript closures for simplicity and performance.
 *
 * Generated code uses:
 * - Native JS closures (no explicit environment)
 * - Runtime helpers: $apply, $con, $eq
 * - Arrays for tuples
 * - Objects for records
 * - Tagged objects for constructors
 */

import type * as ir from "../ir";
import type { Type } from "../checker";
import { RUNTIME } from "./runtime";

// =============================================================================
// CODE GENERATION CONTEXT
// =============================================================================

type CodeGenContext = {
  /** Current indentation level */
  indent: number;
  /** Generated code lines */
  lines: string[];
  /** Set of constructor names (for identifying constructors vs variables) */
  constructors: Set<string>;
};

const createContext = (constructorNames: readonly string[]): CodeGenContext => ({
  indent: 0,
  lines: [],
  constructors: new Set(constructorNames),
});

/** Add a line of code with current indentation */
const emit = (ctx: CodeGenContext, code: string): void => {
  const indentation = "  ".repeat(ctx.indent);
  ctx.lines.push(indentation + code);
};

/** Generate a valid JavaScript identifier from an IR variable name */
const toJsId = (name: string): string => {
  // Replace any invalid characters
  return name.replace(/[^a-zA-Z0-9_$]/g, "_");
};

// =============================================================================
// EXPRESSION GENERATION
// =============================================================================

/**
 * Generate JavaScript code for an IR expression.
 * Returns the generated JavaScript expression as a string.
 */
const genExpr = (ctx: CodeGenContext, expr: ir.IRExpr): string => {
  switch (expr.kind) {
    case "IRAtomExpr":
      return genAtom(ctx, expr.atom);

    case "IRLet": {
      const binding = genBinding(ctx, expr.binding);
      const jsName = toJsId(expr.name);
      emit(ctx, `const ${jsName} = ${binding};`);
      return genExpr(ctx, expr.body);
    }

    case "IRLetRec": {
      // For mutually recursive bindings:
      // 1. Declare ALL variables with let first
      // 2. Assign ALL values (closures can reference all declared vars)

      // Check if any binding is a lambda (needs two-phase init)
      const hasLambda = expr.bindings.some((b) => b.binding.kind === "IRLambdaBinding");

      if (hasLambda) {
        // Declare all variables first
        for (const { name } of expr.bindings) {
          emit(ctx, `let ${toJsId(name)};`);
        }
        // Then assign all values
        for (const { name, binding } of expr.bindings) {
          const bindingCode = genBinding(ctx, binding);
          emit(ctx, `${toJsId(name)} = ${bindingCode};`);
        }
      } else {
        // No lambdas, can use const directly
        for (const { name, binding } of expr.bindings) {
          const bindingCode = genBinding(ctx, binding);
          emit(ctx, `const ${toJsId(name)} = ${bindingCode};`);
        }
      }
      return genExpr(ctx, expr.body);
    }
  }
};

/**
 * Generate JavaScript for an atomic value.
 */
const genAtom = (ctx: CodeGenContext, atom: ir.IRAtom): string => {
  switch (atom.kind) {
    case "IRLit":
      if (typeof atom.value === "string") {
        return JSON.stringify(atom.value);
      }
      return String(atom.value);

    case "IRVar": {
      // Check if this is a constructor
      if (ctx.constructors.has(atom.name)) {
        return `$con("${atom.name}")`;
      }
      return toJsId(atom.name);
    }
  }
};

/**
 * Generate JavaScript for a binding (RHS of let).
 */
const genBinding = (ctx: CodeGenContext, binding: ir.IRBinding): string => {
  switch (binding.kind) {
    case "IRAtomBinding":
      return genAtom(ctx, binding.atom);

    case "IRAppBinding": {
      const func = genAtom(ctx, binding.func);
      const arg = genAtom(ctx, binding.arg);
      return `$apply(${func}, ${arg})`;
    }

    case "IRBinOpBinding":
      return genBinOp(ctx, binding);

    case "IRIfBinding":
      return genIf(ctx, binding);

    case "IRTupleBinding": {
      const elements = binding.elements.map((e) => genAtom(ctx, e));
      return `[${elements.join(", ")}]`;
    }

    case "IRRecordBinding": {
      const fields = binding.fields.map((f) => `${f.name}: ${genAtom(ctx, f.value)}`);
      return `{ ${fields.join(", ")} }`;
    }

    case "IRFieldAccessBinding": {
      const record = genAtom(ctx, binding.record);
      return `${record}.${binding.field}`;
    }

    case "IRTupleIndexBinding": {
      const tuple = genAtom(ctx, binding.tuple);
      return `${tuple}[${binding.index}]`;
    }

    case "IRMatchBinding":
      return genMatch(ctx, binding);

    case "IRLambdaBinding":
      return genLambda(ctx, binding);

    case "IRClosureBinding": {
      // For explicit closures (after closure conversion)
      const captures = binding.captures.map((c) => genAtom(ctx, c));
      return `{ $fn: ${binding.funcId}, $env: [${captures.join(", ")}] }`;
    }
  }
};

/**
 * Generate JavaScript for binary operations.
 */
const genBinOp = (ctx: CodeGenContext, binding: ir.IRBinOpBinding): string => {
  const left = genAtom(ctx, binding.left);
  const right = genAtom(ctx, binding.right);

  switch (binding.op) {
    case "+":
    case "-":
    case "*":
    case "/":
    case "<":
    case ">":
    case "<=":
    case ">=":
      return `(${left} ${binding.op} ${right})`;

    case "==":
      // Use deep equality for complex types
      if (isComplexType(binding.operandType)) {
        return `$eq(${left}, ${right})`;
      }
      return `(${left} === ${right})`;

    case "!=":
      if (isComplexType(binding.operandType)) {
        return `!$eq(${left}, ${right})`;
      }
      return `(${left} !== ${right})`;

    case "++":
      // String concatenation uses + in JavaScript
      return `(${left} + ${right})`;
  }
};

/**
 * Check if a type requires deep equality comparison.
 */
const isComplexType = (type: Type): boolean => {
  switch (type.kind) {
    case "TCon":
      // Primitives use ===
      return !["number", "string", "boolean"].includes(type.name);
    case "TVar":
      // Unknown type - use deep equality to be safe
      return true;
    case "TFun":
    case "TApp":
    case "TRecord":
    case "TTuple":
      return true;
  }
};

/**
 * Generate JavaScript for conditional expression.
 */
const genIf = (ctx: CodeGenContext, binding: ir.IRIfBinding): string => {
  const cond = genAtom(ctx, binding.cond);

  // Use IIFE for complex branches
  ctx.indent++;
  const thenLines: string[] = [];
  const savedLines = ctx.lines;

  ctx.lines = thenLines;
  const thenResult = genExpr(ctx, binding.thenBranch);
  const thenCode =
    thenLines.length > 0
      ? `(() => {\n${thenLines.join("\n")}\n${"  ".repeat(ctx.indent - 1)}return ${thenResult};\n${"  ".repeat(ctx.indent - 1)}})()`
      : thenResult;

  const elseLines: string[] = [];
  ctx.lines = elseLines;
  const elseResult = genExpr(ctx, binding.elseBranch);
  const elseCode =
    elseLines.length > 0
      ? `(() => {\n${elseLines.join("\n")}\n${"  ".repeat(ctx.indent - 1)}return ${elseResult};\n${"  ".repeat(ctx.indent - 1)}})()`
      : elseResult;

  ctx.lines = savedLines;
  ctx.indent--;

  return `(${cond} ? ${thenCode} : ${elseCode})`;
};

/**
 * Generate JavaScript for lambda expression.
 */
const genLambda = (ctx: CodeGenContext, binding: ir.IRLambdaBinding): string => {
  const param = toJsId(binding.param);

  // Generate body
  ctx.indent++;
  const bodyLines: string[] = [];
  const savedLines = ctx.lines;
  ctx.lines = bodyLines;

  const bodyResult = genExpr(ctx, binding.body);

  ctx.lines = savedLines;
  ctx.indent--;

  if (bodyLines.length === 0) {
    // Simple lambda - single expression
    return `(${param}) => ${bodyResult}`;
  }

  // Complex lambda with bindings
  const indentation = "  ".repeat(ctx.indent + 1);
  const body = bodyLines.map((l) => indentation + l.trimStart()).join("\n");
  return `(${param}) => {\n${body}\n${"  ".repeat(ctx.indent)}return ${bodyResult};\n${"  ".repeat(ctx.indent)}}`;
};

/**
 * Generate JavaScript for pattern matching.
 */
const genMatch = (ctx: CodeGenContext, binding: ir.IRMatchBinding): string => {
  const scrutinee = genAtom(ctx, binding.scrutinee);
  const scrutineeVar = "_s";

  // Check if any case has a guard
  const hasGuards = binding.cases.some((c) => c.guard !== undefined);

  const lines: string[] = [];
  lines.push(`((${scrutineeVar}) => {`);

  for (let i = 0; i < binding.cases.length; i++) {
    const case_ = binding.cases[i]!;
    const { condition, bindings } = genPatternMatch(scrutineeVar, case_.pattern);

    // Use sequential ifs when guards are present (guard failure falls through)
    // Use else-if chain when no guards (more efficient, no fall-through needed)
    const prefix = hasGuards || i === 0 ? "if" : "} else if";
    const suffix = hasGuards && i > 0 ? "}" : "";

    if (suffix) lines.push(`  ${suffix}`);
    lines.push(`  ${prefix} (${condition}) {`);

    // Emit bindings
    for (const [name, expr] of bindings) {
      lines.push(`    const ${toJsId(name)} = ${expr};`);
    }

    // Generate body
    ctx.indent += 2;
    const bodyLines: string[] = [];
    const savedLines = ctx.lines;
    ctx.lines = bodyLines;

    // Generate guard if present
    let guardResult: string | undefined;
    if (case_.guard) {
      guardResult = genExpr(ctx, case_.guard);
    }

    const bodyResult = genExpr(ctx, case_.body);

    ctx.lines = savedLines;
    ctx.indent -= 2;

    for (const line of bodyLines) {
      lines.push("    " + line.trimStart());
    }

    // With guard: wrap return in guard check
    if (guardResult) {
      lines.push(`    if (${guardResult}) return ${bodyResult};`);
    } else {
      lines.push(`    return ${bodyResult};`);
    }
  }

  lines.push("  }");
  lines.push(`})(${scrutinee})`);

  return lines.join("\n" + "  ".repeat(ctx.indent));
};

/**
 * Generate pattern matching condition and bindings.
 * Returns the condition expression and a list of [name, expression] bindings.
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
        typeof pattern.value === "string" ? JSON.stringify(pattern.value) : String(pattern.value);
      return { condition: `${scrutinee} === ${value}`, bindings: [] };
    }

    case "IRPCon": {
      const conditions: string[] = [`${scrutinee}.$tag === "${pattern.name}"`];
      const bindings: Array<[string, string]> = [];

      for (let i = 0; i < pattern.args.length; i++) {
        const argScrutinee = `${scrutinee}.$args[${i}]`;
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
        const elemScrutinee = `${scrutinee}[${i}]`;
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
        const fieldScrutinee = `${scrutinee}.${field.name}`;
        const result = genPatternMatch(fieldScrutinee, field.pattern);
        if (result.condition !== "true") {
          conditions.push(result.condition);
        }
        bindings.push(...result.bindings);
      }

      return { condition: conditions.join(" && ") || "true", bindings };
    }

    case "IRPAs": {
      // Match inner pattern and add as-binding
      const result = genPatternMatch(scrutinee, pattern.pattern);
      result.bindings.push([pattern.name, scrutinee]);
      return result;
    }

    case "IRPOr": {
      // Or-pattern: try each alternative until one matches
      // All alternatives bind the same variables, so we use the first one's bindings
      const conditions: string[] = [];
      let bindings: Array<[string, string]> = [];

      for (let i = 0; i < pattern.alternatives.length; i++) {
        const result = genPatternMatch(scrutinee, pattern.alternatives[i]!);
        conditions.push(`(${result.condition})`);
        // Use bindings from first alternative (all should be the same)
        if (i === 0) {
          bindings = result.bindings;
        }
      }

      return { condition: conditions.join(" || "), bindings };
    }
  }
};

// =============================================================================
// MAIN ENTRY POINT
// =============================================================================

/**
 * Result of code generation.
 */
export type CodeGenOutput = {
  readonly code: string;
  readonly warnings: readonly string[];
};

/**
 * Generate JavaScript code from IR.
 *
 * @param ir The IR expression to generate code for
 * @param constructorNames Names of data constructors (to distinguish from variables)
 * @returns The generated JavaScript code
 */
export const generateJS = (
  irExpr: ir.IRExpr,
  constructorNames: readonly string[],
): CodeGenOutput => {
  const ctx = createContext(constructorNames);

  // Generate the main expression
  const result = genExpr(ctx, irExpr);

  // Combine runtime + generated code
  const code = [
    RUNTIME,
    "// Generated code",
    ...ctx.lines,
    `const $result = ${result};`,
    "console.log($result);",
  ].join("\n");

  return { code, warnings: [] };
};
