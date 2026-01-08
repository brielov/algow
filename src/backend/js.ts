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

/** Helper for exhaustive switch checking - TypeScript will error if called with non-never */
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

    default:
      return assertNever(expr);
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

    case "IRForeignVar":
      // Foreign function: look up in the $foreign runtime object
      return `$foreign["${atom.module}"]["${atom.name}"]`;

    default:
      return assertNever(atom);
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
      // Use direct call when we know it's a function (not a constructor)
      // Constructors need $apply for partial application support
      if (binding.func.kind === "IRVar" && ctx.constructors.has(binding.func.name)) {
        return `$apply(${func}, ${arg})`;
      }
      // Direct call for known functions
      return `${func}(${arg})`;
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

    default:
      return assertNever(binding);
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
  }
};

/**
 * Check if a type requires deep equality comparison.
 */
const isComplexType = (type: Type): boolean => {
  switch (type.kind) {
    case "TCon":
      // Primitives use ===
      return !["Int", "Float", "string", "char", "boolean"].includes(type.name);
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
  // Check for tail-recursive optimization
  if (binding.tailRecursive) {
    return genTailRecursiveLambda(ctx, binding);
  }

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
 * Generate JavaScript for a tail-recursive lambda using a while loop.
 */
const genTailRecursiveLambda = (ctx: CodeGenContext, binding: ir.IRLambdaBinding): string => {
  const tco = binding.tailRecursive!;
  const params = tco.params.map(toJsId);
  const selfName = tco.selfName;

  // Collect the innermost body (unwrap nested lambdas)
  let innerBody = binding.body;
  while (innerBody.kind === "IRLet" && innerBody.binding.kind === "IRLambdaBinding") {
    innerBody = innerBody.binding.body;
  }

  // Generate the loop body
  ctx.indent++;
  const bodyLines: string[] = [];
  const savedLines = ctx.lines;
  ctx.lines = bodyLines;

  const bodyResult = genExprTCO(ctx, innerBody, selfName, params);

  ctx.lines = savedLines;
  ctx.indent--;

  // Build curried function that captures params and runs loop
  if (params.length === 1) {
    // Single param - simpler structure
    const indentation = "  ".repeat(ctx.indent + 1);
    const loopIndent = "  ".repeat(ctx.indent + 2);
    const body = bodyLines.map((l) => loopIndent + l.trimStart()).join("\n");

    return `(${params[0]}) => {\n${indentation}while (true) {\n${body}\n${loopIndent}return ${bodyResult};\n${indentation}}\n${"  ".repeat(ctx.indent)}}`;
  }

  // Multi-param - generate curried function with loop in innermost
  // (p1) => (p2) => ... => { while (true) { ... } }
  let result = "";
  for (let i = 0; i < params.length - 1; i++) {
    result += `(${params[i]}) => `;
  }

  const lastParam = params[params.length - 1]!;
  const indentation = "  ".repeat(ctx.indent + 1);
  const loopIndent = "  ".repeat(ctx.indent + 2);
  const body = bodyLines.map((l) => loopIndent + l.trimStart()).join("\n");

  result += `(${lastParam}) => {\n${indentation}while (true) {\n${body}\n${loopIndent}return ${bodyResult};\n${indentation}}\n${"  ".repeat(ctx.indent)}}`;

  return result;
};

/**
 * Generate expression with TCO - tail calls become continue statements.
 */
const genExprTCO = (
  ctx: CodeGenContext,
  expr: ir.IRExpr,
  selfName: string,
  params: string[],
): string => {
  switch (expr.kind) {
    case "IRAtomExpr":
      return genAtom(ctx, expr.atom);

    case "IRLet": {
      // Check if this is a tail call to self
      const tailCallArgs = extractTailCallArgs(expr, selfName, params.length);
      if (tailCallArgs) {
        // First, emit all non-application bindings (argument computations)
        emitNonAppBindings(ctx, expr, selfName);

        // Generate parameter reassignment + continue
        const assignments = params
          .map((p, i) => `${p} = ${genAtom(ctx, tailCallArgs[i]!)};`)
          .join(" ");
        emit(ctx, `${assignments} continue;`);
        return "undefined"; // Never reached
      }

      // Regular let binding
      const binding = genBindingTCO(ctx, expr.binding, selfName, params);
      const jsName = toJsId(expr.name);
      emit(ctx, `const ${jsName} = ${binding};`);
      return genExprTCO(ctx, expr.body, selfName, params);
    }

    case "IRLetRec": {
      // Generate letrec normally but recurse with TCO
      const hasLambda = expr.bindings.some((b) => b.binding.kind === "IRLambdaBinding");

      if (hasLambda) {
        for (const { name } of expr.bindings) {
          emit(ctx, `let ${toJsId(name)};`);
        }
        for (const { name, binding } of expr.bindings) {
          const bindingCode = genBinding(ctx, binding);
          emit(ctx, `${toJsId(name)} = ${bindingCode};`);
        }
      } else {
        for (const { name, binding } of expr.bindings) {
          const bindingCode = genBinding(ctx, binding);
          emit(ctx, `const ${toJsId(name)} = ${bindingCode};`);
        }
      }
      return genExprTCO(ctx, expr.body, selfName, params);
    }
  }
};

/**
 * Emit all non-application bindings from a tail call expression.
 * These are the computations needed for the tail call arguments.
 */
const emitNonAppBindings = (ctx: CodeGenContext, expr: ir.IRExpr, selfName: string): void => {
  let current: ir.IRExpr = expr;

  while (current.kind === "IRLet") {
    const { name, binding } = current;

    // Skip application bindings (they're part of the tail call chain)
    if (binding.kind !== "IRAppBinding") {
      const bindingCode = genBinding(ctx, binding);
      emit(ctx, `const ${toJsId(name)} = ${bindingCode};`);
    } else if (binding.func.kind === "IRVar" && binding.func.name !== selfName) {
      // Also skip partial applications to the function being called
      // (intermediate results like _t7 = fact _t5)
      // But emit applications to other functions
      const isPartialApp = isPartOfTailCallChain(expr, name, selfName);
      if (!isPartialApp) {
        const bindingCode = genBinding(ctx, binding);
        emit(ctx, `const ${toJsId(name)} = ${bindingCode};`);
      }
    }

    current = current.body;
  }
};

/**
 * Check if a binding is part of the tail call application chain.
 */
const isPartOfTailCallChain = (expr: ir.IRExpr, bindingName: string, selfName: string): boolean => {
  // Collect all bindings
  const bindings: Array<{ name: string; binding: ir.IRBinding }> = [];
  let current: ir.IRExpr = expr;

  while (current.kind === "IRLet") {
    bindings.push({ name: current.name, binding: current.binding });
    current = current.body;
  }

  if (current.kind !== "IRAtomExpr" || current.atom.kind !== "IRVar") {
    return false;
  }

  // Walk back from result to find if bindingName is in the chain
  let targetVar = current.atom.name;

  for (let i = bindings.length - 1; i >= 0; i--) {
    const { name, binding } = bindings[i]!;

    if (name === targetVar && binding.kind === "IRAppBinding") {
      if (name === bindingName) {
        return true; // This binding is part of the chain
      }
      if (binding.func.kind === "IRVar") {
        if (binding.func.name === selfName) {
          return false; // Reached the start, bindingName wasn't in chain
        }
        targetVar = binding.func.name;
      } else {
        return false;
      }
    }
  }

  return false;
};

/**
 * Extract tail call arguments if expr ends with a tail call to selfName.
 * Handles interleaved argument computations.
 */
const extractTailCallArgs = (
  expr: ir.IRExpr,
  selfName: string,
  paramCount: number,
): ir.IRAtom[] | null => {
  // Collect all let bindings first
  const bindings: Array<{ name: string; binding: ir.IRBinding }> = [];
  let current: ir.IRExpr = expr;

  while (current.kind === "IRLet") {
    bindings.push({ name: current.name, binding: current.binding });
    current = current.body;
  }

  // Final expression should be a variable
  if (current.kind !== "IRAtomExpr" || current.atom.kind !== "IRVar") {
    return null;
  }

  const resultVar = current.atom.name;

  // Work backwards from the result to find the application chain
  const args: ir.IRAtom[] = [];
  let targetVar = resultVar;

  for (let i = bindings.length - 1; i >= 0; i--) {
    const { name, binding } = bindings[i]!;

    if (name === targetVar && binding.kind === "IRAppBinding") {
      args.unshift(binding.arg);

      if (binding.func.kind === "IRVar") {
        if (binding.func.name === selfName) {
          if (args.length === paramCount) {
            return args;
          }
          return null;
        }
        targetVar = binding.func.name;
      } else {
        return null;
      }
    }
  }

  return null;
};

/**
 * Generate binding with TCO support for if/match in tail position.
 */
const genBindingTCO = (
  ctx: CodeGenContext,
  binding: ir.IRBinding,
  selfName: string,
  params: string[],
): string => {
  switch (binding.kind) {
    case "IRIfBinding":
      return genIfTCO(ctx, binding, selfName, params);

    case "IRMatchBinding":
      return genMatchTCO(ctx, binding, selfName, params);

    default:
      return genBinding(ctx, binding);
  }
};

/**
 * Generate if expression with TCO in branches.
 */
const genIfTCO = (
  ctx: CodeGenContext,
  binding: ir.IRIfBinding,
  selfName: string,
  params: string[],
): string => {
  const cond = genAtom(ctx, binding.cond);

  ctx.indent++;
  const thenLines: string[] = [];
  const savedLines = ctx.lines;

  ctx.lines = thenLines;
  const thenResult = genExprTCO(ctx, binding.thenBranch, selfName, params);
  const thenHasContinue = thenLines.some((l) => l.includes("continue;"));
  const thenCode = thenHasContinue
    ? `(() => {\n${thenLines.join("\n")}\n${"  ".repeat(ctx.indent - 1)}return ${thenResult};\n${"  ".repeat(ctx.indent - 1)}})()`
    : thenLines.length > 0
      ? `(() => {\n${thenLines.join("\n")}\n${"  ".repeat(ctx.indent - 1)}return ${thenResult};\n${"  ".repeat(ctx.indent - 1)}})()`
      : thenResult;

  const elseLines: string[] = [];
  ctx.lines = elseLines;
  const elseResult = genExprTCO(ctx, binding.elseBranch, selfName, params);
  const elseHasContinue = elseLines.some((l) => l.includes("continue;"));
  const elseCode = elseHasContinue
    ? `(() => {\n${elseLines.join("\n")}\n${"  ".repeat(ctx.indent - 1)}return ${elseResult};\n${"  ".repeat(ctx.indent - 1)}})()`
    : elseLines.length > 0
      ? `(() => {\n${elseLines.join("\n")}\n${"  ".repeat(ctx.indent - 1)}return ${elseResult};\n${"  ".repeat(ctx.indent - 1)}})()`
      : elseResult;

  ctx.lines = savedLines;
  ctx.indent--;

  // If either branch has a continue, we need to use if/else statements
  if (thenHasContinue || elseHasContinue) {
    // Emit as if/else with direct continue in branches
    emit(ctx, `if (${cond}) {`);
    for (const line of thenLines) {
      emit(ctx, "  " + line.trimStart());
    }
    if (!thenHasContinue) {
      emit(ctx, `  return ${thenResult};`);
    }
    emit(ctx, `} else {`);
    for (const line of elseLines) {
      emit(ctx, "  " + line.trimStart());
    }
    if (!elseHasContinue) {
      emit(ctx, `  return ${elseResult};`);
    }
    emit(ctx, `}`);
    return "undefined"; // Control transferred via if/else
  }

  return `(${cond} ? ${thenCode} : ${elseCode})`;
};

/**
 * Generate match expression with TCO in case bodies.
 * When a case body contains 'continue', we cannot use an IIFE wrapper
 * since continue cannot jump out of an IIFE. In that case, we emit
 * inline if-else statements directly.
 */
const genMatchTCO = (
  ctx: CodeGenContext,
  binding: ir.IRMatchBinding,
  selfName: string,
  params: string[],
): string => {
  const scrutinee = genAtom(ctx, binding.scrutinee);
  const scrutineeVar = "_s";

  const hasGuards = binding.cases.some((c) => c.guard !== undefined);

  // First pass: check if any case body contains a continue statement
  // If so, we can't use an IIFE wrapper
  const caseResults: Array<{
    case_: ir.IRCase;
    bodyLines: string[];
    bodyResult: string;
    hasContinue: boolean;
    guardResult?: string;
  }> = [];

  for (const case_ of binding.cases) {
    ctx.indent += 2;
    const bodyLines: string[] = [];
    const savedLines = ctx.lines;
    ctx.lines = bodyLines;

    let guardResult: string | undefined;
    if (case_.guard) {
      guardResult = genExpr(ctx, case_.guard);
    }

    const bodyResult = genExprTCO(ctx, case_.body, selfName, params);
    const hasContinue = bodyLines.some((l) => l.includes("continue;"));

    ctx.lines = savedLines;
    ctx.indent -= 2;

    caseResults.push({ case_, bodyLines, bodyResult, hasContinue, guardResult });
  }

  const anyContinue = caseResults.some((r) => r.hasContinue);

  const lines: string[] = [];

  if (anyContinue) {
    // Emit inline if-else statements (no IIFE wrapper)
    // Assign scrutinee to a local variable to avoid re-evaluation
    // Use 'let' since we may assign the result back to this variable
    emit(ctx, `let ${scrutineeVar} = ${scrutinee};`);

    for (let i = 0; i < caseResults.length; i++) {
      const { case_, bodyLines, bodyResult, hasContinue, guardResult } = caseResults[i]!;
      const { condition, bindings } = genPatternMatch(scrutineeVar, case_.pattern);

      const prefix = hasGuards || i === 0 ? "if" : "} else if";
      const suffix = hasGuards && i > 0 ? "}" : "";

      if (suffix) emit(ctx, suffix);
      emit(ctx, `${prefix} (${condition}) {`);

      ctx.indent++;
      for (const [name, expr] of bindings) {
        emit(ctx, `const ${toJsId(name)} = ${expr};`);
      }

      for (const line of bodyLines) {
        emit(ctx, line.trimStart());
      }

      if (guardResult) {
        if (hasContinue) {
          emit(ctx, `if (${guardResult}) { /* continue handled above */ }`);
        } else {
          emit(ctx, `if (${guardResult}) { ${scrutineeVar} = ${bodyResult}; }`);
        }
      } else {
        if (!hasContinue) {
          emit(ctx, `${scrutineeVar} = ${bodyResult};`);
        }
      }
      ctx.indent--;
    }

    emit(ctx, "}");
    return scrutineeVar;
  }

  // No continue statements - use IIFE wrapper (original approach)
  lines.push(`((${scrutineeVar}) => {`);

  for (let i = 0; i < caseResults.length; i++) {
    const { case_, bodyLines, bodyResult, guardResult } = caseResults[i]!;
    const { condition, bindings } = genPatternMatch(scrutineeVar, case_.pattern);

    const prefix = hasGuards || i === 0 ? "if" : "} else if";
    const suffix = hasGuards && i > 0 ? "}" : "";

    if (suffix) lines.push(`  ${suffix}`);
    lines.push(`  ${prefix} (${condition}) {`);

    for (const [name, expr] of bindings) {
      lines.push(`    const ${toJsId(name)} = ${expr};`);
    }

    for (const line of bodyLines) {
      lines.push("    " + line.trimStart());
    }

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
 * Check if pattern matching can use switch-based dispatch.
 * Requires: all patterns are constructor patterns with unique tags, no guards.
 */
const canUseSwitchDispatch = (cases: readonly ir.IRCase[]): boolean => {
  if (cases.length === 0) return false;

  const tags = new Set<string>();
  for (const case_ of cases) {
    if (case_.pattern.kind !== "IRPCon") {
      return false; // Non-constructor pattern
    }
    if (tags.has(case_.pattern.name)) {
      return false; // Duplicate tag - need if/else for disambiguation
    }
    tags.add(case_.pattern.name);
  }
  return true;
};

/**
 * Generate nested pattern conditions (excluding the outer tag check).
 * Used by switch-based dispatch where the tag is already matched.
 */
const genNestedPatternConditions = (
  scrutineeVar: string,
  pattern: ir.IRPCon,
): { conditions: string[]; bindings: Array<[string, string]> } => {
  const conditions: string[] = [];
  const bindings: Array<[string, string]> = [];

  for (let i = 0; i < pattern.args.length; i++) {
    const argScrutinee = `${scrutineeVar}.$args[${i}]`;
    const result = genPatternMatch(argScrutinee, pattern.args[i]!);
    if (result.condition !== "true") {
      conditions.push(result.condition);
    }
    bindings.push(...result.bindings);
  }

  return { conditions, bindings };
};

/**
 * Generate switch-based pattern matching for constructor patterns.
 * O(1) dispatch on the constructor tag.
 */
const genMatchSwitch = (
  ctx: CodeGenContext,
  scrutineeVar: string,
  scrutinee: string,
  cases: readonly ir.IRCase[],
): string => {
  const lines: string[] = [];
  lines.push(`((${scrutineeVar}) => {`);
  lines.push(`  switch (${scrutineeVar}.$tag) {`);

  for (const case_ of cases) {
    const pattern = case_.pattern as ir.IRPCon;
    lines.push(`    case "${pattern.name}": {`);

    // Generate bindings for constructor arguments
    const { conditions, bindings } = genNestedPatternConditions(scrutineeVar, pattern);

    // Emit bindings
    for (const [name, expr] of bindings) {
      lines.push(`      const ${toJsId(name)} = ${expr};`);
    }

    // Generate body
    ctx.indent += 3;
    const bodyLines: string[] = [];
    const savedLines = ctx.lines;
    ctx.lines = bodyLines;
    const bodyResult = genExpr(ctx, case_.body);
    ctx.lines = savedLines;
    ctx.indent -= 3;

    for (const line of bodyLines) {
      lines.push("      " + line.trimStart());
    }

    // If nested patterns have conditions, wrap return in a check
    if (conditions.length > 0) {
      lines.push(`      if (${conditions.join(" && ")}) return ${bodyResult};`);
      lines.push(`      break;`);
    } else {
      lines.push(`      return ${bodyResult};`);
    }

    lines.push(`    }`);
  }

  lines.push(`  }`);
  lines.push(`})(${scrutinee})`);

  return lines.join("\n" + "  ".repeat(ctx.indent));
};

/**
 * Generate JavaScript for pattern matching.
 * Uses switch-based dispatch when possible for O(1) constructor matching.
 */
const genMatch = (ctx: CodeGenContext, binding: ir.IRMatchBinding): string => {
  const scrutinee = genAtom(ctx, binding.scrutinee);
  const scrutineeVar = "_s";

  // Check if any case has a guard
  const hasGuards = binding.cases.some((c) => c.guard !== undefined);

  // Use switch-based dispatch for constructor patterns with unique tags
  if (!hasGuards && canUseSwitchDispatch(binding.cases)) {
    return genMatchSwitch(ctx, scrutineeVar, scrutinee, binding.cases);
  }

  // Fall back to if/else chain for guards or mixed patterns
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
