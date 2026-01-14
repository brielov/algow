/**
 * Code Generation (Section 12)
 *
 * Generates JavaScript from ANF IR.
 *
 * Value representations (from SPEC.md):
 * - Lists: Nil → null, Cons h t → { h, t }
 * - Other ADTs: [tag, arg1, arg2, ...] with numeric tags
 * - Tuples: JavaScript arrays
 * - Records: Plain JavaScript objects
 * - Functions: Native JavaScript closures
 */

import type * as IR from "./ir";
import type { Name } from "./core";
import { getRuntime, type Target, DEFAULT_TARGET } from "./runtime";

// Re-export Target type for external use
export type { Target } from "./runtime";
export { TARGETS, DEFAULT_TARGET, isValidTarget } from "./runtime";

// =============================================================================
// Code Generation Context
// =============================================================================

type TagMap = Map<string, number>;

/** Context determines how code should be generated */
type GenContext = "expr" | "stmt";

type CodeGenContext = {
  /** Current indentation level */
  indent: number;
  /** Generated code lines */
  lines: string[];
  /** Tag assignments for constructors (type -> constructor -> tag) */
  tagMap: Map<string, TagMap>;
  /** Set of already declared variable names */
  declaredNames: Set<string>;
  /** Counter for fresh scrutinee variables */
  scrutineeCounter: number;
  /** Whether we're generating in expression or statement context */
  genContext: GenContext;
};

const createContext = (): CodeGenContext => ({
  indent: 0,
  lines: [],
  tagMap: new Map(),
  declaredNames: new Set(),
  scrutineeCounter: 0,
  genContext: "stmt",
});

const emit = (ctx: CodeGenContext, code: string): void => {
  const indentation = "  ".repeat(ctx.indent);
  ctx.lines.push(indentation + code);
};

const freshScrutinee = (ctx: CodeGenContext): string => {
  return `_s${ctx.scrutineeCounter++}`;
};

// =============================================================================
// JavaScript Reserved Words
// =============================================================================

const JS_RESERVED = new Set([
  "break",
  "case",
  "catch",
  "class",
  "const",
  "continue",
  "debugger",
  "default",
  "delete",
  "do",
  "else",
  "export",
  "extends",
  "finally",
  "for",
  "function",
  "if",
  "import",
  "in",
  "instanceof",
  "let",
  "new",
  "return",
  "static",
  "super",
  "switch",
  "this",
  "throw",
  "try",
  "typeof",
  "var",
  "void",
  "while",
  "with",
  "yield",
  "implements",
  "interface",
  "package",
  "private",
  "protected",
  "public",
  "arguments",
  "eval",
  "null",
  "true",
  "false",
  "enum",
  "await",
]);

/** Convert a Name to a valid JavaScript identifier */
const nameToJs = (name: Name): string => {
  // Use both id and original to create unique name
  const base = name.text.replace(/[^a-zA-Z0-9_$]/g, "_");
  // For negative IDs (temporaries from lowering), use absolute value
  const suffix = name.id < 0 ? `$${-name.id}` : `$${name.id}`;
  const result = `${base}${suffix}`;
  return JS_RESERVED.has(result) ? `$${result}` : result;
};

/** Convert a constructor name (which may contain dots) to a valid JS identifier */
const conNameToJs = (name: string): string => {
  const result = name.replace(/[^a-zA-Z0-9_$]/g, "_");
  return JS_RESERVED.has(result) ? `$${result}` : result;
};

/** Get the tag for a constructor */
const getConstructorTag = (ctx: CodeGenContext, typeName: string, conName: string): number => {
  const typeMap = ctx.tagMap.get(typeName);
  if (typeMap) {
    const tag = typeMap.get(conName);
    if (tag !== undefined) return tag;
  }
  // Unknown constructor - return 0
  return 0;
};

// =============================================================================
// Atom Generation
// =============================================================================

const genAtom = (_ctx: CodeGenContext, atom: IR.Atom): string => {
  switch (atom.kind) {
    case "ALit": {
      const lit = atom.value;
      switch (lit.kind) {
        case "int":
        case "float":
          return String(lit.value);
        case "string":
        case "char":
          return JSON.stringify(lit.value);
        case "bool":
          return lit.value ? "true" : "false";
      }
    }

    case "AVar":
      return nameToJs(atom.name);

    case "ACon": {
      // Constructor as a value (nullary or partially applied)
      if (atom.name === "Nil") {
        return "null";
      }
      if (atom.name === "Cons") {
        // Cons as a function: (h) => (t) => ({ h, t })
        return "((h) => (t) => ({ h, t }))";
      }
      // Other constructors are declared as variables during type declaration
      // processing, so just reference them by sanitized name
      return conNameToJs(atom.name);
    }
  }
};

// =============================================================================
// Binding Generation
// =============================================================================

const genBinding = (ctx: CodeGenContext, binding: IR.IRBinding): string => {
  switch (binding.kind) {
    case "IRBAtom":
      return genAtom(ctx, binding.atom);

    case "IRBApp": {
      // Special case: Cons(h) → (t) => ({ h: h, t })
      if (binding.func.kind === "ACon" && binding.func.name === "Cons") {
        const h = genAtom(ctx, binding.arg);
        return `(t) => ({ h: ${h}, t })`;
      }
      // Constructor applications don't need await
      if (binding.func.kind === "ACon") {
        const func = genAtom(ctx, binding.func);
        const arg = genAtom(ctx, binding.arg);
        return `${func}(${arg})`;
      }
      const func = genAtom(ctx, binding.func);
      const arg = genAtom(ctx, binding.arg);
      // Await function calls (all user functions are async)
      return `await ${func}(${arg})`;
    }

    case "IRBBinOp": {
      const left = genAtom(ctx, binding.left);
      const right = genAtom(ctx, binding.right);
      // Handle equality specially for structural comparison
      if (binding.op === "==") {
        return `$eq(${left}, ${right})`;
      }
      if (binding.op === "!=") {
        return `!$eq(${left}, ${right})`;
      }
      // Integer division
      if (binding.op === "/") {
        return `Math.trunc(${left} / ${right})`;
      }
      // For other operators, generate direct JS operators
      return `(${left} ${binding.op} ${right})`;
    }

    case "IRBTuple": {
      const elements = binding.elements.map((e) => genAtom(ctx, e));
      return `[${elements.join(", ")}]`;
    }

    case "IRBRecord": {
      const fields = binding.fields.map((f) => `${f.name}: ${genAtom(ctx, f.value)}`);
      return `{ ${fields.join(", ")} }`;
    }

    case "IRBRecordUpdate": {
      const base = genAtom(ctx, binding.record);
      const fields = binding.fields.map((f) => `${f.name}: ${genAtom(ctx, f.value)}`);
      return `{ ...${base}, ${fields.join(", ")} }`;
    }

    case "IRBField": {
      const record = genAtom(ctx, binding.record);
      // Use bracket notation for numeric fields (tuple access)
      if (/^\d+$/.test(binding.field)) {
        return `${record}[${binding.field}]`;
      }
      return `${record}.${binding.field}`;
    }

    case "IRBLambda":
      return genLambda(ctx, binding);

    case "IRBForeign": {
      // Foreign function reference
      if (binding.args.length === 0) {
        return `$foreign["${binding.module}"]["${binding.name}"]`;
      }
      // Foreign function call with args
      let result = `$foreign["${binding.module}"]["${binding.name}"]`;
      for (const arg of binding.args) {
        result = `${result}(${genAtom(ctx, arg)})`;
      }
      // Add await for async foreign functions
      if (binding.isAsync) {
        result = `await ${result}`;
      }
      return result;
    }

    case "IRBMatch": {
      // Match expression as binding - create an IIFE with the match
      const matchExpr: IR.IRMatch = {
        kind: "IRMatch",
        scrutinee: binding.scrutinee,
        cases: binding.cases,
        type: binding.type,
      };
      return genMatch(ctx, matchExpr);
    }
  }
};

const genLambda = (ctx: CodeGenContext, binding: IR.IRBLambda): string => {
  const param = nameToJs(binding.param);

  // Check for tail recursion
  if (binding.tailRecursive) {
    return genTailRecursiveLambda(ctx, binding);
  }

  // Check if body requires await
  const needsAwait = exprRequiresAwait(binding.body);
  const asyncPrefix = needsAwait ? "async " : "";

  // Generate body using statement generation (includes return)
  ctx.indent++;
  const bodyLines: string[] = [];
  const savedLines = ctx.lines;
  const savedDeclared = new Set(ctx.declaredNames);
  ctx.lines = bodyLines;

  genExprAsStatements(ctx, binding.body);

  ctx.lines = savedLines;
  ctx.declaredNames = savedDeclared;
  ctx.indent--;

  // For simple expressions (single return), use arrow shorthand
  if (bodyLines.length === 1) {
    const line = bodyLines[0]!.trim();
    // Check if it's just "return <expr>;"
    const returnMatch = line.match(/^return (.+);$/);
    if (returnMatch) {
      const result = returnMatch[1]!;
      // Wrap object literals in parentheses to avoid ambiguity with function body
      const safeResult = result.startsWith("{") ? `(${result})` : result;
      return `${asyncPrefix}(${param}) => ${safeResult}`;
    }
  }

  const indentation = "  ".repeat(ctx.indent + 1);
  const body = bodyLines.map((l) => indentation + l.trimStart()).join("\n");
  return `${asyncPrefix}(${param}) => {\n${body}\n${"  ".repeat(ctx.indent)}}`;
};

const genTailRecursiveLambda = (ctx: CodeGenContext, binding: IR.IRBLambda): string => {
  const tco = binding.tailRecursive!;
  const params = tco.params.map(nameToJs);

  // Find innermost body
  let innerBody = binding.body;
  while (innerBody.kind === "IRLet" && innerBody.binding.kind === "IRBLambda") {
    innerBody = innerBody.binding.body;
  }

  // Check if body requires await
  const needsAwait = exprRequiresAwait(innerBody);
  const asyncPrefix = needsAwait ? "async " : "";

  // Generate loop body using statement generation
  ctx.indent += 2; // Account for function + while nesting
  const bodyLines: string[] = [];
  const savedLines = ctx.lines;
  const savedDeclared = new Set(ctx.declaredNames);
  ctx.lines = bodyLines;

  genExprAsStatements(ctx, innerBody);

  ctx.lines = savedLines;
  ctx.declaredNames = savedDeclared;
  ctx.indent -= 2;

  // Build curried function with while loop
  if (params.length === 1) {
    const indentation = "  ".repeat(ctx.indent + 1);
    const loopIndent = "  ".repeat(ctx.indent + 2);
    const body = bodyLines.map((l) => loopIndent + l.trimStart()).join("\n");
    return `${asyncPrefix}(${params[0]}) => {\n${indentation}while (true) {\n${body}\n${indentation}}\n${"  ".repeat(ctx.indent)}}`;
  }

  // Multi-param: curried function
  let result = "";
  for (let i = 0; i < params.length - 1; i++) {
    result += `${asyncPrefix}(${params[i]}) => `;
  }

  const lastParam = params[params.length - 1]!;
  const indentation = "  ".repeat(ctx.indent + 1);
  const loopIndent = "  ".repeat(ctx.indent + 2);
  const body = bodyLines.map((l) => loopIndent + l.trimStart()).join("\n");

  result += `${asyncPrefix}(${lastParam}) => {\n${indentation}while (true) {\n${body}\n${indentation}}\n${"  ".repeat(ctx.indent)}}`;

  return result;
};

// =============================================================================
// Expression Generation
// =============================================================================

const genExpr = (ctx: CodeGenContext, expr: IR.IRExpr): string => {
  switch (expr.kind) {
    case "IRAtom":
      return genAtom(ctx, expr.atom);

    case "IRLet": {
      // Optimization: if body is just returning this variable, skip the intermediate binding
      if (
        expr.body.kind === "IRAtom" &&
        expr.body.atom.kind === "AVar" &&
        expr.body.atom.name.id === expr.name.id
      ) {
        return genBinding(ctx, expr.binding);
      }

      const binding = genBinding(ctx, expr.binding);
      const jsName = nameToJs(expr.name);

      if (!ctx.declaredNames.has(jsName)) {
        emit(ctx, `const ${jsName} = ${binding};`);
        ctx.declaredNames.add(jsName);
      } else {
        emit(ctx, `${jsName} = ${binding};`);
      }
      return genExpr(ctx, expr.body);
    }

    case "IRLetRec": {
      // Declare all variables first
      for (const { name } of expr.bindings) {
        const jsName = nameToJs(name);
        if (!ctx.declaredNames.has(jsName)) {
          emit(ctx, `let ${jsName};`);
          ctx.declaredNames.add(jsName);
        }
      }
      // Then assign all values
      for (const { name, binding } of expr.bindings) {
        const bindingCode = genBinding(ctx, binding);
        emit(ctx, `${nameToJs(name)} = ${bindingCode};`);
      }
      return genExpr(ctx, expr.body);
    }

    case "IRMatch":
      return genMatch(ctx, expr);
  }
};

// =============================================================================
// Statement Generation (for return position)
// =============================================================================

/**
 * Generate expression as statements ending with return.
 * Used in function bodies where we can emit if-else directly.
 */
const genExprAsStatements = (ctx: CodeGenContext, expr: IR.IRExpr): void => {
  switch (expr.kind) {
    case "IRAtom":
      emit(ctx, `return ${genAtom(ctx, expr.atom)};`);
      break;

    case "IRLet": {
      // Optimization: if body is just returning this variable, skip the intermediate binding
      if (
        expr.body.kind === "IRAtom" &&
        expr.body.atom.kind === "AVar" &&
        expr.body.atom.name.id === expr.name.id
      ) {
        emit(ctx, `return ${genBinding(ctx, expr.binding)};`);
        return;
      }

      const binding = genBinding(ctx, expr.binding);
      const jsName = nameToJs(expr.name);

      if (!ctx.declaredNames.has(jsName)) {
        emit(ctx, `const ${jsName} = ${binding};`);
        ctx.declaredNames.add(jsName);
      } else {
        emit(ctx, `${jsName} = ${binding};`);
      }
      genExprAsStatements(ctx, expr.body);
      break;
    }

    case "IRLetRec": {
      // Declare all variables first
      for (const { name } of expr.bindings) {
        const jsName = nameToJs(name);
        if (!ctx.declaredNames.has(jsName)) {
          emit(ctx, `let ${jsName};`);
          ctx.declaredNames.add(jsName);
        }
      }
      // Then assign all values
      for (const { name, binding } of expr.bindings) {
        const bindingCode = genBinding(ctx, binding);
        emit(ctx, `${nameToJs(name)} = ${bindingCode};`);
      }
      genExprAsStatements(ctx, expr.body);
      break;
    }

    case "IRMatch":
      genMatchAsStatements(ctx, expr);
      break;
  }
};

/**
 * Generate match as direct if-else statements with returns.
 * No IIFE wrapper needed.
 */
const genMatchAsStatements = (ctx: CodeGenContext, match: IR.IRMatch): void => {
  const scrutineeVar =
    match.scrutinee.kind === "AVar"
      ? nameToJs(match.scrutinee.name)
      : (() => {
          // Need to bind scrutinee to a variable first
          const v = freshScrutinee(ctx);
          emit(ctx, `const ${v} = ${genAtom(ctx, match.scrutinee)};`);
          return v;
        })();

  // Check if any case has a guard
  const hasGuards = match.cases.some((c) => c.guard !== null);

  for (let i = 0; i < match.cases.length; i++) {
    const case_ = match.cases[i]!;
    const { condition, bindings } = genPatternMatch(ctx, scrutineeVar, case_.pattern);
    const isFirst = i === 0;
    const isLast = i === match.cases.length - 1;

    // Generate guard code first
    let guardCode: string | null = null;
    const guardLines: string[] = [];
    if (case_.guard) {
      const savedLines = ctx.lines;
      ctx.lines = guardLines;
      guardCode = genExpr(ctx, case_.guard);
      ctx.lines = savedLines;
    }

    // Determine if statement prefix
    let prefix: string;
    let combineGuard = false;
    if (hasGuards) {
      // With guards: use separate if blocks
      if (guardCode && guardLines.length === 0 && bindings.length === 0) {
        if (condition === "true") {
          prefix = `if (${guardCode})`;
        } else {
          prefix = `if ((${condition}) && (${guardCode}))`;
        }
        combineGuard = true;
      } else {
        prefix = `if (${condition})`;
      }
    } else {
      // No guards: use if/else-if chain
      if (isFirst) {
        prefix = `if (${condition})`;
      } else if (isLast && condition === "true") {
        prefix = "} else";
      } else {
        prefix = `} else if (${condition})`;
      }
    }
    emit(ctx, `${prefix} {`);

    // Emit bindings
    ctx.indent++;
    for (const [name, expr] of bindings) {
      emit(ctx, `const ${name} = ${expr};`);
    }

    // Emit guard computation lines if any
    for (const line of guardLines) {
      emit(ctx, line.trimStart());
    }

    // Generate body as statements
    if (guardCode && !combineGuard) {
      // Guard wasn't combined - need nested if
      emit(ctx, `if (${guardCode}) {`);
      ctx.indent++;
      genExprAsStatements(ctx, case_.body);
      ctx.indent--;
      emit(ctx, "}");
    } else {
      genExprAsStatements(ctx, case_.body);
    }

    ctx.indent--;

    // Close the if block
    if (hasGuards) {
      emit(ctx, "}");
    }
  }

  if (!hasGuards) {
    emit(ctx, "}");
  }
};

// =============================================================================
// Async Detection
// =============================================================================

/** Check if an expression requires await (contains function calls or async foreign) */
const exprRequiresAwait = (expr: IR.IRExpr): boolean => {
  switch (expr.kind) {
    case "IRAtom":
      return false;
    case "IRLet":
      return bindingRequiresAwait(expr.binding) || exprRequiresAwait(expr.body);
    case "IRLetRec":
      return (
        expr.bindings.some((b) => bindingRequiresAwait(b.binding)) || exprRequiresAwait(expr.body)
      );
    case "IRMatch":
      return expr.cases.some(
        (c) => (c.guard && exprRequiresAwait(c.guard)) || exprRequiresAwait(c.body),
      );
  }
};

const bindingRequiresAwait = (binding: IR.IRBinding): boolean => {
  switch (binding.kind) {
    case "IRBAtom":
    case "IRBBinOp":
    case "IRBTuple":
    case "IRBRecord":
    case "IRBRecordUpdate":
    case "IRBField":
      return false;
    case "IRBApp":
      // Function applications require await (all user functions are async)
      // Exception: constructor applications don't require await
      return binding.func.kind !== "ACon";
    case "IRBForeign":
      return binding.isAsync;
    case "IRBLambda":
      // Lambda body may require await, but the lambda itself doesn't
      return false;
    case "IRBMatch":
      return binding.cases.some(
        (c) => (c.guard && exprRequiresAwait(c.guard)) || exprRequiresAwait(c.body),
      );
  }
};

// =============================================================================
// Match Optimization Detection
// =============================================================================

type TernaryMatch = {
  kind: "ternary";
  condition: string;
  thenExpr: IR.IRExpr;
  elseExpr: IR.IRExpr;
};

type SimpleMatch = {
  kind: "simple";
};

type MatchOptimization = TernaryMatch | SimpleMatch | null;

/** Check if a match can be optimized to a ternary expression */
const canOptimizeToTernary = (ctx: CodeGenContext, match: IR.IRMatch): MatchOptimization => {
  // Must have exactly 2 cases with no guards
  if (match.cases.length !== 2) return null;
  if (match.cases.some((c) => c.guard !== null)) return null;

  const [case1, case2] = match.cases;
  if (!case1 || !case2) return null;

  // Check for boolean patterns: True/False
  if (
    case1.pattern.kind === "IRPCon" &&
    case2.pattern.kind === "IRPCon" &&
    case1.pattern.args.length === 0 &&
    case2.pattern.args.length === 0
  ) {
    const name1 = case1.pattern.name;
    const name2 = case2.pattern.name;

    if ((name1 === "True" && name2 === "False") || (name1 === "False" && name2 === "True")) {
      const scrutinee = genAtom(ctx, match.scrutinee);
      const isTrueFirst = name1 === "True";
      return {
        kind: "ternary",
        condition: scrutinee,
        thenExpr: isTrueFirst ? case1.body : case2.body,
        elseExpr: isTrueFirst ? case2.body : case1.body,
      };
    }
  }

  // Check for literal patterns
  if (case1.pattern.kind === "IRPLit" && case2.pattern.kind === "IRPWild") {
    const lit = case1.pattern.value;
    const scrutinee = genAtom(ctx, match.scrutinee);
    let litValue: string;
    switch (lit.kind) {
      case "int":
      case "float":
        litValue = String(lit.value);
        break;
      case "string":
      case "char":
        litValue = JSON.stringify(lit.value);
        break;
      case "bool":
        litValue = lit.value ? "true" : "false";
        break;
    }
    return {
      kind: "ternary",
      condition: `${scrutinee} === ${litValue}`,
      thenExpr: case1.body,
      elseExpr: case2.body,
    };
  }

  return null;
};

// =============================================================================
// Match Generation
// =============================================================================

const genMatch = (ctx: CodeGenContext, match: IR.IRMatch): string => {
  // Try ternary optimization first
  const ternaryOpt = canOptimizeToTernary(ctx, match);
  if (ternaryOpt && ternaryOpt.kind === "ternary") {
    return genTernaryMatch(ctx, ternaryOpt);
  }

  // Check if any case body requires await
  const needsAwait = match.cases.some(
    (c) => (c.guard && exprRequiresAwait(c.guard)) || exprRequiresAwait(c.body),
  );

  // Generate optimized match
  return genMatchFull(ctx, match, needsAwait);
};

/** Generate a ternary expression for simple two-branch matches */
const genTernaryMatch = (ctx: CodeGenContext, opt: TernaryMatch): string => {
  // Generate then and else expressions
  const savedContext = ctx.genContext;
  ctx.genContext = "expr";

  const thenLines: string[] = [];
  const savedLines = ctx.lines;
  ctx.lines = thenLines;
  const thenResult = genExpr(ctx, opt.thenExpr);
  ctx.lines = savedLines;

  const elseLines: string[] = [];
  ctx.lines = elseLines;
  const elseResult = genExpr(ctx, opt.elseExpr);
  ctx.lines = savedLines;

  ctx.genContext = savedContext;

  // If either branch has intermediate lines, fall back to IIFE
  if (thenLines.length > 0 || elseLines.length > 0) {
    // Check if await needed
    const needsAwait = exprRequiresAwait(opt.thenExpr) || exprRequiresAwait(opt.elseExpr);
    const asyncPrefix = needsAwait ? "async " : "";
    const awaitPrefix = needsAwait ? "await " : "";

    const lines: string[] = [];
    lines.push(`(${asyncPrefix}() => {`);
    lines.push(`  if (${opt.condition}) {`);
    for (const line of thenLines) {
      lines.push("    " + line.trimStart());
    }
    lines.push(`    return ${thenResult};`);
    lines.push("  } else {");
    for (const line of elseLines) {
      lines.push("    " + line.trimStart());
    }
    lines.push(`    return ${elseResult};`);
    lines.push("  }");
    lines.push("})()");
    return awaitPrefix + lines.join("\n" + "  ".repeat(ctx.indent));
  }

  // Simple ternary
  return `(${opt.condition} ? ${thenResult} : ${elseResult})`;
};

/** Generate full match expression */
const genMatchFull = (ctx: CodeGenContext, match: IR.IRMatch, needsAwait: boolean): string => {
  // If scrutinee is a simple variable, use it directly without IIFE wrapping
  const isSimpleVar = match.scrutinee.kind === "AVar";
  const scrutineeVar = isSimpleVar ? nameToJs(match.scrutinee.name) : freshScrutinee(ctx);
  const scrutineeExpr = isSimpleVar ? null : genAtom(ctx, match.scrutinee);

  // Check if any case has a guard - if so, we need separate if blocks (not else-if)
  // to allow fallthrough when guards fail
  const hasGuards = match.cases.some((c) => c.guard !== null);

  const lines: string[] = [];
  // Only make IIFEs async when needed
  const asyncPrefix = needsAwait ? "async " : "";
  if (isSimpleVar) {
    lines.push(`(${asyncPrefix}() => {`);
  } else {
    lines.push(`(${asyncPrefix}(${scrutineeVar}) => {`);
  }

  for (let i = 0; i < match.cases.length; i++) {
    const case_ = match.cases[i]!;
    const { condition, bindings } = genPatternMatch(ctx, scrutineeVar, case_.pattern);
    const isLast = i === match.cases.length - 1;

    // Generate guard code first to determine combined condition
    let guardCode: string | null = null;
    const guardLines: string[] = [];
    if (case_.guard) {
      ctx.indent += 2;
      const savedLines = ctx.lines;
      ctx.lines = guardLines;
      guardCode = genExpr(ctx, case_.guard);
      ctx.lines = savedLines;
      ctx.indent -= 2;
    }

    // When there are guards, use separate if blocks to allow fallthrough
    // When no guards, use if/else-if chain for efficiency
    let prefix: string;
    let combineGuard = false;
    if (hasGuards) {
      // With guards: use separate if blocks (no else) to allow fallthrough
      // Only combine guard with condition if:
      // 1. No intermediate guard computations needed
      // 2. No bindings needed (bindings must be available before guard check)
      if (guardCode && guardLines.length === 0 && bindings.length === 0) {
        // Simple guard with no bindings - combine with pattern condition
        if (condition === "true") {
          prefix = `if (${guardCode})`;
        } else {
          prefix = `if ((${condition}) && (${guardCode}))`;
        }
        combineGuard = true;
      } else {
        prefix = `if (${condition})`;
      }
    } else {
      // No guards: use if/else-if chain
      if (i === 0) {
        prefix = `if (${condition})`;
      } else if (isLast) {
        prefix = "} else";
      } else {
        prefix = `} else if (${condition})`;
      }
    }
    lines.push(`  ${prefix} {`);

    // Emit bindings
    for (const [name, expr] of bindings) {
      lines.push(`    const ${name} = ${expr};`);
    }

    // Emit guard computation lines if any
    for (const line of guardLines) {
      lines.push("    " + line.trimStart());
    }

    // Generate body using statement generation (includes return)
    ctx.indent += 2;
    const bodyLines: string[] = [];
    const savedLines = ctx.lines;
    const savedDeclared = new Set(ctx.declaredNames);
    ctx.lines = bodyLines;

    if (guardCode && !combineGuard) {
      // Guard wasn't combined - need nested if for guard check
      emit(ctx, `if (${guardCode}) {`);
      ctx.indent++;
      genExprAsStatements(ctx, case_.body);
      ctx.indent--;
      emit(ctx, "}");
    } else {
      genExprAsStatements(ctx, case_.body);
    }

    ctx.lines = savedLines;
    ctx.declaredNames = savedDeclared;
    ctx.indent -= 2;

    for (const line of bodyLines) {
      lines.push("    " + line.trimStart());
    }

    // Close the if block
    if (hasGuards) {
      lines.push("  }");
    }
  }

  if (!hasGuards) {
    lines.push("  }");
  }
  // Close IIFE
  if (isSimpleVar) {
    lines.push("})()");
  } else {
    lines.push(`})(${scrutineeExpr})`);
  }

  // Only await if the IIFE is async
  const awaitPrefix = needsAwait ? "await " : "";
  return awaitPrefix + lines.join("\n" + "  ".repeat(ctx.indent));
};

// =============================================================================
// Pattern Matching
// =============================================================================

type PatternResult = {
  condition: string;
  bindings: Array<[string, string]>;
};

const genPatternMatch = (
  ctx: CodeGenContext,
  scrutinee: string,
  pattern: IR.IRPattern,
): PatternResult => {
  switch (pattern.kind) {
    case "IRPWild":
      return { condition: "true", bindings: [] };

    case "IRPVar": {
      const jsName = nameToJs(pattern.name);
      return { condition: "true", bindings: [[jsName, scrutinee]] };
    }

    case "IRPLit": {
      const lit = pattern.value;
      let value: string;
      switch (lit.kind) {
        case "int":
        case "float":
          value = String(lit.value);
          break;
        case "string":
        case "char":
          value = JSON.stringify(lit.value);
          break;
        case "bool":
          value = lit.value ? "true" : "false";
          break;
      }
      return { condition: `${scrutinee} === ${value}`, bindings: [] };
    }

    case "IRPCon": {
      if (pattern.name === "Nil") {
        // Nil → null check
        return { condition: `${scrutinee} === null`, bindings: [] };
      }

      if (pattern.name === "Cons") {
        // Cons → object with h/t fields
        const conditions: string[] = [`${scrutinee} !== null`];
        const bindings: Array<[string, string]> = [];

        if (pattern.args.length >= 1) {
          const headResult = genPatternMatch(ctx, `${scrutinee}.h`, pattern.args[0]!);
          if (headResult.condition !== "true") {
            conditions.push(headResult.condition);
          }
          bindings.push(...headResult.bindings);
        }

        if (pattern.args.length >= 2) {
          const tailResult = genPatternMatch(ctx, `${scrutinee}.t`, pattern.args[1]!);
          if (tailResult.condition !== "true") {
            conditions.push(tailResult.condition);
          }
          bindings.push(...tailResult.bindings);
        }

        return { condition: conditions.join(" && "), bindings };
      }

      // Other constructors → array [tag, ...args]
      // Get type name from the pattern's type (strip type applications)
      let typeName = "Unknown";
      let t = pattern.type;
      while (t.kind === "TApp") t = t.con;
      if (t.kind === "TCon") typeName = t.name;

      const tag = getConstructorTag(ctx, typeName, pattern.name);
      const conditions: string[] = [`Array.isArray(${scrutinee})`, `${scrutinee}[0] === ${tag}`];
      const bindings: Array<[string, string]> = [];

      for (let i = 0; i < pattern.args.length; i++) {
        const argResult = genPatternMatch(ctx, `${scrutinee}[${i + 1}]`, pattern.args[i]!);
        if (argResult.condition !== "true") {
          conditions.push(argResult.condition);
        }
        bindings.push(...argResult.bindings);
      }

      return { condition: conditions.join(" && "), bindings };
    }

    case "IRPTuple": {
      const conditions: string[] = [];
      const bindings: Array<[string, string]> = [];

      for (let i = 0; i < pattern.elements.length; i++) {
        const elemResult = genPatternMatch(ctx, `${scrutinee}[${i}]`, pattern.elements[i]!);
        if (elemResult.condition !== "true") {
          conditions.push(elemResult.condition);
        }
        bindings.push(...elemResult.bindings);
      }

      return { condition: conditions.join(" && ") || "true", bindings };
    }

    case "IRPRecord": {
      const conditions: string[] = [];
      const bindings: Array<[string, string]> = [];

      for (const field of pattern.fields) {
        const fieldResult = genPatternMatch(ctx, `${scrutinee}.${field.name}`, field.pattern);
        if (fieldResult.condition !== "true") {
          conditions.push(fieldResult.condition);
        }
        bindings.push(...fieldResult.bindings);
      }

      return { condition: conditions.join(" && ") || "true", bindings };
    }

    case "IRPAs": {
      // As-pattern: bind the whole value AND match the inner pattern
      const jsName = nameToJs(pattern.name);
      const innerResult = genPatternMatch(ctx, scrutinee, pattern.pattern);
      // Add binding for the whole value, then include inner bindings
      return {
        condition: innerResult.condition,
        bindings: [[jsName, scrutinee], ...innerResult.bindings],
      };
    }
  }
};

// =============================================================================
// Declaration Generation
// =============================================================================

const genDecl = (ctx: CodeGenContext, decl: IR.IRDecl): void => {
  switch (decl.kind) {
    case "IRDeclType": {
      // Register constructor tags
      const tagMap = new Map<string, number>();
      for (const con of decl.constructors) {
        tagMap.set(con.name, con.tag);
      }
      ctx.tagMap.set(decl.name, tagMap);

      // Generate constructor functions for non-List types
      if (decl.name !== "List") {
        for (const con of decl.constructors) {
          const jsConName = conNameToJs(con.name);
          // Skip if already declared (avoid duplicates from multi-file compilation)
          if (ctx.declaredNames.has(jsConName)) {
            continue;
          }
          if (con.arity === 0) {
            // Nullary constructor
            emit(ctx, `const ${jsConName} = [${con.tag}];`);
          } else {
            // Constructor with args - generate curried function
            const params = Array.from({ length: con.arity }, (_, i) => `_${i}`);
            let body = `[${con.tag}`;
            for (const p of params) {
              body += `, ${p}`;
            }
            body += "]";

            let func = body;
            for (let i = con.arity - 1; i >= 0; i--) {
              func = `(${params[i]}) => ${func}`;
            }
            emit(ctx, `const ${jsConName} = ${func};`);
          }
          ctx.declaredNames.add(jsConName);
        }
      }
      break;
    }

    case "IRDeclLet": {
      const jsName = nameToJs(decl.name);
      const binding = genBinding(ctx, decl.binding);
      emit(ctx, `const ${jsName} = ${binding};`);
      ctx.declaredNames.add(jsName);
      break;
    }

    case "IRDeclLetRec": {
      // Declare all first
      for (const { name } of decl.bindings) {
        const jsName = nameToJs(name);
        if (!ctx.declaredNames.has(jsName)) {
          emit(ctx, `let ${jsName};`);
          ctx.declaredNames.add(jsName);
        }
      }
      // Then assign
      for (const { name, binding } of decl.bindings) {
        const bindingCode = genBinding(ctx, binding);
        emit(ctx, `${nameToJs(name)} = ${bindingCode};`);
      }
      break;
    }
  }
};

// =============================================================================
// Program Generation
// =============================================================================

export type CodeGenOptions = {
  /** Target platform (default: "node") */
  readonly target?: Target;
};

export type CodeGenOutput = {
  readonly code: string;
};

/**
 * Find the main function's JS name from declarations.
 */
const findMainName = (decls: readonly IR.IRDecl[]): string | null => {
  for (const decl of decls) {
    if (decl.kind === "IRDeclLet" && decl.name.text === "main") {
      return nameToJs(decl.name);
    }
    if (decl.kind === "IRDeclLetRec") {
      for (const b of decl.bindings) {
        if (b.name.text === "main") {
          return nameToJs(b.name);
        }
      }
    }
  }
  return null;
};

/**
 * Generate JavaScript from IR program.
 */
export const generateJS = (program: IR.IRProgram, options: CodeGenOptions = {}): CodeGenOutput => {
  const { target = DEFAULT_TARGET } = options;
  const ctx = createContext();

  // Generate declarations
  for (const decl of program.decls) {
    genDecl(ctx, decl);
  }

  // Find the main function
  const mainName = findMainName(program.decls);

  // Combine runtime + generated code wrapped in IIFE for encapsulation
  const runtime = getRuntime(target);

  // Build IIFE body
  const body = [
    runtime,
    "// Generated code",
    ...ctx.lines,
    // Call main with unit (null)
    mainName ? `await ${mainName}(null);` : "",
  ]
    .filter(Boolean)
    .join("\n");

  // Wrap in async IIFE
  const code = `(async () => {\n"use strict";\n${body}\n})();\n`;

  return { code };
};

/**
 * Generate JavaScript from a single IR expression (for testing).
 */
export const generateExprJS = (
  expr: IR.IRExpr,
  typeDecls: IR.IRDeclType[] = [],
  options: CodeGenOptions = {},
): CodeGenOutput => {
  const { target = DEFAULT_TARGET } = options;
  const ctx = createContext();

  // Process type declarations for tag assignments
  for (const decl of typeDecls) {
    const tagMap = new Map<string, number>();
    for (const con of decl.constructors) {
      tagMap.set(con.name, con.tag);
    }
    ctx.tagMap.set(decl.name, tagMap);
  }

  const result = genExpr(ctx, expr);

  const runtime = getRuntime(target);
  const body = [
    runtime,
    "// Generated code",
    ...ctx.lines,
    `const $result = ${result};`,
    "console.log($result);",
  ].join("\n");

  // Wrap in async IIFE
  const code = `(async () => {\n"use strict";\n${body}\n})();\n`;

  return { code };
};
