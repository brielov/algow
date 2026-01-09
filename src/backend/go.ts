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
import { type Type, freeTypeVars } from "../checker";
import { GO_RUNTIME } from "./go_runtime";

// =============================================================================
// TYPE ANALYSIS
// =============================================================================

/** Check if a type is monomorphic (has no free type variables) */
const isMonomorphic = (type: Type): boolean => freeTypeVars(type).size === 0;

/**
 * Convert a type to its Go representation.
 * Returns null for polymorphic types or complex types that should stay boxed.
 *
 * We only generate typed code for primitives:
 * - number -> float64
 * - string -> string
 * - boolean -> bool
 *
 * Functions, records, tuples, and ADTs stay as Value because:
 * - Functions can be passed to polymorphic higher-order functions that use Apply()
 * - Records/tuples interact with pattern matching which expects Value
 * - ADTs require complex boxing/unboxing at boundaries
 */
const typeToGo = (type: Type): string | null => {
  if (!isMonomorphic(type)) return null;

  switch (type.kind) {
    case "TCon":
      switch (type.name) {
        case "int":
          return "int64";
        case "float":
          return "float64";
        case "string":
          return "string";
        case "boolean":
          return "bool";
        default:
          // User-defined type (ADT) - keep as Value
          return null;
      }

    // Functions, tuples, records, TApp (ADTs) stay as Value
    case "TFun":
    case "TTuple":
    case "TRecord":
    case "TApp":
    case "TVar":
      return null;
  }
};

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
  /** Map of constructor names to integer tags */
  constructorTags: Map<string, number>;
  /** Lifted functions from closure conversion */
  functions: readonly ir.IRFunction[];
  /** Variables with known typed Go representation (not boxed as Value) */
  typedVars: Map<string, string>;
};

const createContext = (
  constructorNames: readonly string[],
  functions: readonly ir.IRFunction[],
): CodeGenContext => {
  // Assign integer tags to constructors
  const constructorTags = new Map<string, number>();
  constructorNames.forEach((name, i) => constructorTags.set(name, i));
  return {
    indent: 0,
    lines: [],
    constructorTags,
    functions,
    typedVars: new Map(),
  };
};

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
 * Check if an atom has a typed Go representation.
 */
const getAtomGoType = (ctx: CodeGenContext, atom: ir.IRAtom): string | null => {
  if (atom.kind === "IRLit") {
    return typeToGo(atom.type);
  }
  if (atom.kind === "IRVar") {
    // Check if this is a typed variable
    return ctx.typedVars.get(atom.name) ?? null;
  }
  return null;
};

/**
 * Generate a typed atom value (raw value without Value wrapper).
 */
const genTypedAtom = (ctx: CodeGenContext, atom: ir.IRAtom): string => {
  switch (atom.kind) {
    case "IRLit":
      if (typeof atom.value === "string") {
        return JSON.stringify(atom.value);
      }
      if (typeof atom.value === "boolean") {
        return atom.value ? "true" : "false";
      }
      // Numbers as raw float64
      return `float64(${atom.value})`;

    case "IRVar": {
      const name = atom.name;
      if (name.startsWith("$env[")) {
        return name.replace("$env", "env");
      }
      return toGoId(name);
    }

    case "IRForeignVar":
      // Foreign function: access from Foreign registry
      return `Foreign["${atom.module}"]["${atom.name}"]`;

    default:
      return assertNever(atom);
  }
};

/**
 * Generate Go code for an IR expression.
 * Returns the Go expression as a string.
 */
const genExpr = (ctx: CodeGenContext, expr: ir.IRExpr): string => {
  switch (expr.kind) {
    case "IRAtomExpr": {
      const goType = getAtomGoType(ctx, expr.atom);
      if (goType) {
        // Typed atom - but may need to box for return
        return genTypedAtom(ctx, expr.atom);
      }
      return genAtom(ctx, expr.atom);
    }

    case "IRLet": {
      const goName = toGoId(expr.name);
      const bindingType = expr.binding.type;
      const goType = typeToGo(bindingType);

      if (goType) {
        // Use typed variable declaration
        const binding = genTypedBinding(ctx, expr.binding);
        emit(ctx, `var ${goName} ${goType} = ${binding}`);
        ctx.typedVars.set(expr.name, goType);
      } else {
        // Fall back to boxed Value
        const binding = genBinding(ctx, expr.binding);
        emit(ctx, `var ${goName} Value = ${binding}`);
      }
      return genExpr(ctx, expr.body);
    }

    case "IRLetRec": {
      // For letrec, check if all bindings are typeable
      // For simplicity, fall back to Value for recursive bindings
      for (const { name } of expr.bindings) {
        emit(ctx, `var ${toGoId(name)} Value`);
      }
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
      // Check if this is a constructor - use integer tag
      const tag = ctx.constructorTags.get(name);
      if (tag !== undefined) {
        return `NewCon(${tag})`;
      }
      return toGoId(name);
    }

    case "IRForeignVar":
      // Foreign function: access from Foreign registry
      return `Foreign["${atom.module}"]["${atom.name}"]`;

    default:
      return assertNever(atom);
  }
};

/**
 * Generate Go for a binding (boxed as Value).
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
      return genBinOp(ctx, binding, false);

    case "IRIfBinding":
      return genIf(ctx, binding);

    case "IRTupleBinding": {
      const elements = binding.elements.map((e) => genAtom(ctx, e));
      return `Value([]Value{${elements.join(", ")}})`;
    }

    case "IRRecordBinding": {
      const fields = binding.fields.map((f) => `"${f.name}": ${genAtom(ctx, f.value)}`);
      return `Value(map[string]Value{${fields.join(", ")}})`;
    }

    case "IRRecordUpdateBinding": {
      const base = genAtom(ctx, binding.base);
      const updates = binding.fields.map((f) => `"${f.name}": ${genAtom(ctx, f.value)}`);
      // Use runtime helper to merge records
      return `$recordUpdate(${base}, map[string]Value{${updates.join(", ")}})`;
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
 * Generate Go for a typed binding (raw value without Value wrapper).
 */
const genTypedBinding = (ctx: CodeGenContext, binding: ir.IRBinding): string => {
  switch (binding.kind) {
    case "IRAtomBinding":
      return genTypedAtom(ctx, binding.atom);

    case "IRBinOpBinding":
      return genBinOp(ctx, binding, true);

    case "IRAppBinding": {
      // Check if we can do a direct typed call
      const funcGoType = getAtomGoType(ctx, binding.func);
      if (funcGoType && funcGoType.startsWith("func(")) {
        // Direct function call
        const func = genTypedAtom(ctx, binding.func);
        const argGoType = getAtomGoType(ctx, binding.arg);
        const arg = argGoType ? genTypedAtom(ctx, binding.arg) : genAtom(ctx, binding.arg);
        return `${func}(${arg})`;
      }
      // Fall through to default
      break;
    }

    case "IRLambdaBinding": {
      // Generate typed lambda directly
      return genLambda(ctx, binding);
    }

    default:
      break;
  }

  // Default: box, then type assert to extract typed value
  const goType = typeToGo(binding.type);
  const boxed = genBinding(ctx, binding);
  if (goType) {
    return `${boxed}.(${goType})`;
  }
  return boxed;
};

/**
 * Generate Go for binary operations.
 * @param typed If true, return raw typed value; if false, wrap in Value()
 */
const genBinOp = (ctx: CodeGenContext, binding: ir.IRBinOpBinding, typed: boolean): string => {
  // Check which operands are typed
  const leftGoType = getAtomGoType(ctx, binding.left);
  const rightGoType = getAtomGoType(ctx, binding.right);

  // Generate operand code - use typed if available, otherwise boxed
  const left = leftGoType ? genTypedAtom(ctx, binding.left) : genAtom(ctx, binding.left);
  const right = rightGoType ? genTypedAtom(ctx, binding.right) : genAtom(ctx, binding.right);

  // Check operand type for string operations
  const isString = isStringType(binding.operandType);

  // Helper to wrap result if needed
  const wrap = (expr: string) => (typed ? expr : `Value(${expr})`);

  // Type assertion helpers - only needed when operand is boxed
  const asNum = (v: string, isTyped: boolean) => (isTyped ? v : `${v}.(float64)`);
  const asStr = (v: string, isTyped: boolean) => (isTyped ? v : `${v}.(string)`);

  const leftTyped = leftGoType !== null;
  const rightTyped = rightGoType !== null;

  switch (binding.op) {
    case "+":
      if (isString) {
        return wrap(`${asStr(left, leftTyped)} + ${asStr(right, rightTyped)}`);
      }
      return wrap(`${asNum(left, leftTyped)} + ${asNum(right, rightTyped)}`);

    case "-":
      return wrap(`${asNum(left, leftTyped)} - ${asNum(right, rightTyped)}`);

    case "*":
      return wrap(`${asNum(left, leftTyped)} * ${asNum(right, rightTyped)}`);

    case "/":
      return wrap(`${asNum(left, leftTyped)} / ${asNum(right, rightTyped)}`);

    case "<":
      return wrap(`${asNum(left, leftTyped)} < ${asNum(right, rightTyped)}`);

    case ">":
      return wrap(`${asNum(left, leftTyped)} > ${asNum(right, rightTyped)}`);

    case "<=":
      return wrap(`${asNum(left, leftTyped)} <= ${asNum(right, rightTyped)}`);

    case ">=":
      return wrap(`${asNum(left, leftTyped)} >= ${asNum(right, rightTyped)}`);

    case "==":
      if (isComplexType(binding.operandType)) {
        return typed ? `Eq(${left}, ${right}).(bool)` : `Eq(${left}, ${right})`;
      }
      return wrap(`${left} == ${right}`);

    case "!=":
      if (isComplexType(binding.operandType)) {
        return typed ? `!Eq(${left}, ${right}).(bool)` : `Value(!Eq(${left}, ${right}).(bool))`;
      }
      return wrap(`${left} != ${right}`);
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
      return !["int", "float", "string", "boolean"].includes(type.name);
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
  // Check if condition is typed
  const condGoType = getAtomGoType(ctx, binding.cond);
  const cond = condGoType ? genTypedAtom(ctx, binding.cond) : genAtom(ctx, binding.cond);
  const condExpr = condGoType === "bool" ? cond : `${cond}.(bool)`;

  // Use immediately invoked function for complex branches
  const lines: string[] = [];
  lines.push(`func() Value {`);
  lines.push(`\tif ${condExpr} {`);

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

  // Check if this lambda can be typed
  // Use typeToGo on the function type - this will return null since we don't generate typed functions
  const funcGoType = typeToGo(binding.type);
  const isTypedLambda = funcGoType !== null;

  const paramGoType = isTypedLambda ? typeToGo(binding.paramType) : null;
  let retGoType: string | null = null;
  if (isTypedLambda && binding.type.kind === "TFun") {
    retGoType = typeToGo(binding.type.ret);
  }

  // If typed, track the param as a typed variable
  if (isTypedLambda) {
    ctx.typedVars.set(binding.param, paramGoType!);
  }

  // Generate body
  ctx.indent++;
  const savedLines = ctx.lines;
  ctx.lines = [];
  const bodyResult = genExpr(ctx, binding.body);
  const bodyLines = ctx.lines;
  ctx.lines = savedLines;
  ctx.indent--;

  // Clean up typed var tracking
  if (isTypedLambda) {
    ctx.typedVars.delete(binding.param);
  }

  const lines: string[] = [];
  if (isTypedLambda) {
    // Generate typed lambda - use raw func, not PureFunc wrapper
    lines.push(`func(${param} ${paramGoType}) ${retGoType} {`);
  } else {
    lines.push(`PureFunc{Fn: func(${param} Value) Value {`);
  }

  for (const line of bodyLines) {
    lines.push("\t" + line);
  }
  lines.push(`\treturn ${bodyResult}`);

  if (isTypedLambda) {
    lines.push(`}`);
  } else {
    lines.push(`}}`);
  }

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

  // Check if all cases are simple constructor patterns - use switch
  // Simple means: no nested constructor patterns and unique tags
  const isSimpleConstructorPattern = (p: ir.IRPattern): boolean => {
    if (p.kind !== "IRPCon") return false;
    // Check that all args are simple (var, wildcard, literal, or tuple of simples)
    return p.args.every(
      (arg) => arg.kind === "IRPVar" || arg.kind === "IRPWildcard" || arg.kind === "IRPLit",
    );
  };
  const allSimpleConstructors = binding.cases.every((c) => isSimpleConstructorPattern(c.pattern));
  const tags = binding.cases
    .filter((c) => c.pattern.kind === "IRPCon")
    .map((c) => (c.pattern as ir.IRPCon).name);
  const hasUniqueTags = new Set(tags).size === tags.length;
  const hasGuards = binding.cases.some((c) => c.guard !== undefined);

  if (allSimpleConstructors && hasUniqueTags && !hasGuards) {
    lines.push(`\tswitch _s.(Con).Tag {`);

    for (const case_ of binding.cases) {
      const pattern = case_.pattern as ir.IRPCon;
      const tag = ctx.constructorTags.get(pattern.name) ?? 0;
      lines.push(`\tcase ${tag}: // ${pattern.name}`);

      // Generate bindings for constructor arguments
      const bindings = genPatternBindings("_s.(Con).Args", pattern);
      for (const [name, expr] of bindings) {
        const goName = toGoId(name);
        lines.push(`\t\t${goName} := ${expr}`);
        lines.push(`\t\t_ = ${goName}`); // Suppress unused variable error
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
      const { condition, bindings } = genPatternMatch(ctx, "_s", case_.pattern);

      const prefix = i === 0 ? "if" : "} else if";
      lines.push(`\t${prefix} ${condition} {`);

      // Emit bindings
      for (const [name, expr] of bindings) {
        const goName = toGoId(name);
        lines.push(`\t\t${goName} := ${expr}`);
        lines.push(`\t\t_ = ${goName}`); // Suppress unused variable error
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
        // Check if guard result is a typed variable
        const guardGoType = typeToGo(case_.guard.type);
        const guardCondition = guardGoType === "bool" ? guardResult : `${guardResult}.(bool)`;
        lines.push(`\t\tif ${guardCondition} {`);
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
  ctx: CodeGenContext,
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
      const tag = ctx.constructorTags.get(pattern.name) ?? 0;
      const conditions: string[] = [`${scrutinee}.(Con).Tag == ${tag}`];
      const bindings: Array<[string, string]> = [];

      for (let i = 0; i < pattern.args.length; i++) {
        const argScrutinee = `${scrutinee}.(Con).Args[${i}]`;
        const result = genPatternMatch(ctx, argScrutinee, pattern.args[i]!);
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
        const result = genPatternMatch(ctx, elemScrutinee, pattern.elements[i]!);
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
        const result = genPatternMatch(ctx, fieldScrutinee, field.pattern);
        if (result.condition !== "true") {
          conditions.push(result.condition);
        }
        bindings.push(...result.bindings);
      }

      return { condition: conditions.join(" && ") || "true", bindings };
    }

    case "IRPAs": {
      const result = genPatternMatch(ctx, scrutinee, pattern.pattern);
      result.bindings.push([pattern.name, scrutinee]);
      return result;
    }

    case "IRPOr": {
      const conditions: string[] = [];
      let bindings: Array<[string, string]> = [];

      for (let i = 0; i < pattern.alternatives.length; i++) {
        const result = genPatternMatch(ctx, scrutinee, pattern.alternatives[i]!);
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
