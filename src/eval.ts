/**
 * Tree-walking interpreter for the language.
 *
 * Evaluates AST expressions to runtime values.
 */

import type * as ast from "./ast";

// =============================================================================
// Runtime Values
// =============================================================================

export type Value = VNum | VStr | VBool | VClosure | VCon | VTuple | VRecord | VRef;

/** Number value */
export type VNum = {
  readonly kind: "VNum";
  readonly value: number;
};

/** String value */
export type VStr = {
  readonly kind: "VStr";
  readonly value: string;
};

/** Boolean value */
export type VBool = {
  readonly kind: "VBool";
  readonly value: boolean;
};

/** Closure - function with captured environment */
export type VClosure = {
  readonly kind: "VClosure";
  readonly param: string;
  readonly body: ast.Expr;
  readonly env: Env;
};

/** Data constructor application (e.g., Just 42, Cons 1 Nil) */
export type VCon = {
  readonly kind: "VCon";
  readonly name: string;
  readonly args: readonly Value[];
};

/** Tuple value */
export type VTuple = {
  readonly kind: "VTuple";
  readonly elements: readonly Value[];
};

/** Record value */
export type VRecord = {
  readonly kind: "VRecord";
  readonly fields: ReadonlyMap<string, Value>;
};

/** Mutable reference cell for recursive bindings (textbook letrec pattern) */
export type VRef = {
  readonly kind: "VRef";
  value: Value | null; // Intentionally mutable - filled after closure creation
};

// =============================================================================
// Value Constructors
// =============================================================================

export const vnum = (value: number): Value => ({ kind: "VNum", value });
export const vstr = (value: string): Value => ({ kind: "VStr", value });
export const vbool = (value: boolean): Value => ({ kind: "VBool", value });
export const vclosure = (param: string, body: ast.Expr, env: Env): Value => ({
  kind: "VClosure",
  param,
  body,
  env,
});
export const vcon = (name: string, args: readonly Value[] = []): Value => ({
  kind: "VCon",
  name,
  args,
});
export const vtuple = (elements: readonly Value[]): Value => ({ kind: "VTuple", elements });
export const vrecord = (fields: ReadonlyMap<string, Value>): Value => ({ kind: "VRecord", fields });
export const vref = (): VRef => ({ kind: "VRef", value: null });

// =============================================================================
// Environment
// =============================================================================

export type Env = ReadonlyMap<string, Value>;

const emptyEnv: Env = new Map();

const extendEnv = (env: Env, name: string, value: Value): Env => {
  const newEnv = new Map(env);
  newEnv.set(name, value);
  return newEnv;
};

// =============================================================================
// Evaluation
// =============================================================================

/**
 * Runtime error for things the type checker can't catch.
 */
export class RuntimeError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "RuntimeError";
  }
}

/**
 * Evaluate an expression to a value.
 */
export const evaluate = (env: Env, expr: ast.Expr): Value => {
  switch (expr.kind) {
    case "Num":
      return vnum(expr.value);

    case "Str":
      return vstr(expr.value);

    case "Bool":
      return vbool(expr.value);

    case "Var": {
      const value = env.get(expr.name)!;
      // Dereference if it's a ref cell (from letrec)
      if (value.kind === "VRef") {
        if (value.value === null) {
          throw new RuntimeError(`Uninitialized recursive binding: ${expr.name}`);
        }
        return value.value;
      }
      return value;
    }

    case "Abs":
      return vclosure(expr.param, expr.body, env);

    case "App": {
      const func = evaluate(env, expr.func);
      const arg = evaluate(env, expr.param);
      return apply(func, arg);
    }

    case "Let": {
      const value = evaluate(env, expr.value);
      const newEnv = extendEnv(env, expr.name, value);
      return evaluate(newEnv, expr.body);
    }

    case "LetRec": {
      // Textbook letrec: create ref cell, evaluate with ref in scope, fill ref
      const ref = vref();
      const newEnv = extendEnv(env, expr.name, ref);
      const value = evaluate(newEnv, expr.value);
      ref.value = value;
      return evaluate(newEnv, expr.body);
    }

    case "If": {
      const cond = evaluate(env, expr.cond) as VBool;
      return cond.value ? evaluate(env, expr.then) : evaluate(env, expr.else);
    }

    case "BinOp":
      return evalBinOp(env, expr);

    case "Tuple": {
      const elements = expr.elements.map((e) => evaluate(env, e));
      return elements.length === 1 ? elements[0]! : vtuple(elements);
    }

    case "Record": {
      const fields = new Map<string, Value>();
      for (const field of expr.fields) {
        fields.set(field.name, evaluate(env, field.value));
      }
      return vrecord(fields);
    }

    case "FieldAccess": {
      const record = evaluate(env, expr.record) as VRecord;
      return record.fields.get(expr.field)!;
    }

    case "TupleIndex": {
      const tuple = evaluate(env, expr.tuple) as VTuple;
      return tuple.elements[expr.index]!;
    }

    case "Match":
      return evalMatch(env, expr);
  }
};

/**
 * Apply a function to an argument.
 * Type checker ensures func is either a closure or constructor.
 */
const apply = (func: Value, arg: Value): Value => {
  if (func.kind === "VClosure") {
    const newEnv = extendEnv(func.env, func.param, arg);
    return evaluate(newEnv, func.body);
  }
  // Must be a constructor (partial application)
  return vcon((func as VCon).name, [...(func as VCon).args, arg]);
};

/**
 * Evaluate a binary operation.
 * Assumes the program has been type-checked, so types are correct.
 */
const evalBinOp = (env: Env, expr: ast.BinOp): Value => {
  const left = evaluate(env, expr.left);
  const right = evaluate(env, expr.right);

  switch (expr.op) {
    // Arithmetic (type checker ensures numbers)
    case "-":
      return vnum((left as VNum).value - (right as VNum).value);
    case "*":
      return vnum((left as VNum).value * (right as VNum).value);
    case "/": {
      const divisor = (right as VNum).value;
      if (divisor === 0) throw new RuntimeError("Division by zero");
      return vnum((left as VNum).value / divisor);
    }

    // Addition (numbers or strings - dispatch on left operand)
    case "+":
      if (left.kind === "VNum") {
        return vnum(left.value + (right as VNum).value);
      }
      return vstr((left as VStr).value + (right as VStr).value);

    // Comparisons (numbers or strings - dispatch on left operand)
    case "<":
      if (left.kind === "VNum") return vbool(left.value < (right as VNum).value);
      return vbool((left as VStr).value < (right as VStr).value);
    case ">":
      if (left.kind === "VNum") return vbool(left.value > (right as VNum).value);
      return vbool((left as VStr).value > (right as VStr).value);
    case "<=":
      if (left.kind === "VNum") return vbool(left.value <= (right as VNum).value);
      return vbool((left as VStr).value <= (right as VStr).value);
    case ">=":
      if (left.kind === "VNum") return vbool(left.value >= (right as VNum).value);
      return vbool((left as VStr).value >= (right as VStr).value);

    // Equality
    case "==":
      return vbool(valuesEqual(left, right));
    case "!=":
      return vbool(!valuesEqual(left, right));

    // String concatenation
    case "++":
      return vstr((left as VStr).value + (right as VStr).value);
  }
};

/**
 * Check structural equality of values.
 */
const valuesEqual = (a: Value, b: Value): boolean => {
  if (a.kind !== b.kind) return false;

  switch (a.kind) {
    case "VNum":
      return a.value === (b as VNum).value;
    case "VStr":
      return a.value === (b as VStr).value;
    case "VBool":
      return a.value === (b as VBool).value;
    case "VCon": {
      const bCon = b as VCon;
      if (a.name !== bCon.name || a.args.length !== bCon.args.length) return false;
      return a.args.every((arg, i) => valuesEqual(arg, bCon.args[i]!));
    }
    case "VTuple": {
      const bTuple = b as VTuple;
      if (a.elements.length !== bTuple.elements.length) return false;
      return a.elements.every((elem, i) => valuesEqual(elem, bTuple.elements[i]!));
    }
    case "VRecord": {
      const bRecord = b as VRecord;
      if (a.fields.size !== bRecord.fields.size) return false;
      for (const [key, val] of a.fields) {
        const bVal = bRecord.fields.get(key);
        if (bVal === undefined || !valuesEqual(val, bVal)) return false;
      }
      return true;
    }
    case "VClosure":
    case "VRef":
      // Functions and refs are not comparable
      return false;
  }
};

// =============================================================================
// Pattern Matching
// =============================================================================

type MatchResult = { matched: true; bindings: Map<string, Value> } | { matched: false };

/**
 * Match a value against a pattern.
 */
const matchPattern = (pattern: ast.Pattern, value: Value): MatchResult => {
  switch (pattern.kind) {
    case "PWildcard":
      return { matched: true, bindings: new Map() };

    case "PVar":
      return { matched: true, bindings: new Map([[pattern.name, value]]) };

    case "PLit": {
      if (typeof pattern.value === "number" && value.kind === "VNum") {
        return pattern.value === value.value
          ? { matched: true, bindings: new Map() }
          : { matched: false };
      }
      if (typeof pattern.value === "string" && value.kind === "VStr") {
        return pattern.value === value.value
          ? { matched: true, bindings: new Map() }
          : { matched: false };
      }
      if (typeof pattern.value === "boolean" && value.kind === "VBool") {
        return pattern.value === value.value
          ? { matched: true, bindings: new Map() }
          : { matched: false };
      }
      return { matched: false };
    }

    case "PCon": {
      if (value.kind !== "VCon" || value.name !== pattern.name) {
        return { matched: false };
      }
      if (value.args.length !== pattern.args.length) {
        return { matched: false };
      }

      const bindings = new Map<string, Value>();
      for (let i = 0; i < pattern.args.length; i++) {
        const result = matchPattern(pattern.args[i]!, value.args[i]!);
        if (!result.matched) return { matched: false };
        for (const [name, val] of result.bindings) {
          bindings.set(name, val);
        }
      }
      return { matched: true, bindings };
    }

    case "PTuple": {
      if (value.kind !== "VTuple" || value.elements.length !== pattern.elements.length) {
        return { matched: false };
      }

      const bindings = new Map<string, Value>();
      for (let i = 0; i < pattern.elements.length; i++) {
        const result = matchPattern(pattern.elements[i]!, value.elements[i]!);
        if (!result.matched) return { matched: false };
        for (const [name, val] of result.bindings) {
          bindings.set(name, val);
        }
      }
      return { matched: true, bindings };
    }

    case "PRecord": {
      if (value.kind !== "VRecord") {
        return { matched: false };
      }

      const bindings = new Map<string, Value>();
      for (const field of pattern.fields) {
        const fieldValue = value.fields.get(field.name);
        if (fieldValue === undefined) return { matched: false };
        const result = matchPattern(field.pattern, fieldValue);
        if (!result.matched) return { matched: false };
        for (const [name, val] of result.bindings) {
          bindings.set(name, val);
        }
      }
      return { matched: true, bindings };
    }

    case "PAs": {
      // Match inner pattern, then add as-binding
      const result = matchPattern(pattern.pattern, value);
      if (!result.matched) return { matched: false };
      result.bindings.set(pattern.name, value);
      return result;
    }
  }
};

/**
 * Evaluate a match expression.
 * Exhaustiveness checking ensures at least one pattern matches.
 */
const evalMatch = (env: Env, expr: ast.Match): Value => {
  const scrutinee = evaluate(env, expr.expr);

  for (const case_ of expr.cases) {
    const result = matchPattern(case_.pattern, scrutinee);
    if (result.matched) {
      let caseEnv = env;
      for (const [name, value] of result.bindings) {
        caseEnv = extendEnv(caseEnv, name, value);
      }

      // Check guard if present
      if (case_.guard) {
        const guardResult = evaluate(caseEnv, case_.guard) as VBool;
        if (!guardResult.value) {
          continue; // Guard failed, try next case
        }
      }

      return evaluate(caseEnv, case_.body);
    }
  }

  // Exhaustiveness checking guarantees this is unreachable
  throw new Error("Unreachable: exhaustiveness check failed");
};

// =============================================================================
// Value Display
// =============================================================================

/**
 * Convert a value to a readable string.
 */
export const valueToString = (value: Value): string => {
  switch (value.kind) {
    case "VNum":
      return String(value.value);
    case "VStr":
      return `"${value.value}"`;
    case "VBool":
      return String(value.value);
    case "VClosure":
      return "<function>";
    case "VCon":
      if (value.args.length === 0) return value.name;
      return `(${value.name} ${value.args.map(valueToString).join(" ")})`;
    case "VTuple":
      return `(${value.elements.map(valueToString).join(", ")})`;
    case "VRecord": {
      const fields = [...value.fields.entries()]
        .map(([k, v]) => `${k}: ${valueToString(v)}`)
        .join(", ");
      return `{ ${fields} }`;
    }
    case "VRef":
      return value.value ? valueToString(value.value) : "<uninitialized>";
  }
};

// =============================================================================
// Prelude Integration
// =============================================================================

/**
 * Create an environment with data constructors from processed declarations.
 * Constructors like Just, Nothing, Cons, Nil are represented as VCon with no args.
 */
export const createConstructorEnv = (constructorNames: readonly string[]): Env => {
  const env = new Map<string, Value>();
  for (const name of constructorNames) {
    env.set(name, vcon(name));
  }
  return env;
};

/**
 * Evaluate a prelude function and add it to the environment.
 */
export const evalPreludeFunction = (env: Env, name: string, expr: ast.Expr): Env => {
  const value = evaluate(env, expr);
  return extendEnv(env, name, value);
};

// Re-export empty env for convenience
export { emptyEnv };
