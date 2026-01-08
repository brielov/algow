/**
 * Tree-walking interpreter for the language.
 *
 * Evaluates AST expressions to runtime values.
 */

import type * as ast from "./ast";

/** Helper for exhaustive switch checking - TypeScript will error if called with non-never */
const assertNever = (x: never): never => {
  throw new Error(`Unexpected value: ${JSON.stringify(x)}`);
};

// =============================================================================
// Runtime Values
// =============================================================================

export type Value =
  | VInt
  | VFloat
  | VStr
  | VChar
  | VBool
  | VClosure
  | VForeign
  | VCon
  | VTuple
  | VRecord
  | VRef;

/** Foreign function - a native JS function */
export type VForeign = {
  readonly kind: "VForeign";
  readonly fn: (arg: Value) => Value;
};

/** Integer value */
export type VInt = {
  readonly kind: "VInt";
  readonly value: number;
};

/** Floating-point value */
export type VFloat = {
  readonly kind: "VFloat";
  readonly value: number;
};

/** String value */
export type VStr = {
  readonly kind: "VStr";
  readonly value: string;
};

/** Character value */
export type VChar = {
  readonly kind: "VChar";
  readonly value: string; // Single character
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

export const vint = (value: number): Value => ({ kind: "VInt", value });
export const vfloat = (value: number): Value => ({ kind: "VFloat", value });
export const vstr = (value: string): Value => ({ kind: "VStr", value });
export const vchar = (value: string): Value => ({ kind: "VChar", value });
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
export const vforeign = (fn: (arg: Value) => Value): VForeign => ({ kind: "VForeign", fn });

// =============================================================================
// Foreign Function Registry
// =============================================================================

/** Helper to create curried foreign functions */
const curry2 =
  (f: (a: Value, b: Value) => Value) =>
  (a: Value): Value =>
    vforeign((b) => f(a, b));

const curry3 =
  (f: (a: Value, b: Value, c: Value) => Value) =>
  (a: Value): Value =>
    vforeign((b) => vforeign((c) => f(a, b, c)));

/**
 * Helper to convert Node.js errors to IOError values.
 */
const toIOError = (err: NodeJS.ErrnoException, path: string): Value => {
  const code = err.code;
  if (code === "ENOENT") return vcon("FileNotFound", [vstr(path)]);
  if (code === "EACCES" || code === "EPERM") return vcon("PermissionDenied", [vstr(path)]);
  if (code === "EISDIR") return vcon("IsDirectory", [vstr(path)]);
  if (code === "EEXIST") return vcon("AlreadyExists", [vstr(path)]);
  return vcon("UnknownError", [vstr(err.message ?? "Unknown error")]);
};

/**
 * Foreign function registry for the interpreter.
 * Maps module names to function names to implementations.
 */
const foreignRegistry: Record<string, Record<string, Value>> = {
  String: {
    length: vforeign((s) => vint((s as VStr).value.length)),
    concat: vforeign(curry2((a, b) => vstr((a as VStr).value + (b as VStr).value))),
    substring: vforeign(
      curry3((start, end, s) =>
        vstr((s as VStr).value.substring((start as VInt).value, (end as VInt).value)),
      ),
    ),
    charAt: vforeign(
      curry2((i, s) => {
        const str = (s as VStr).value;
        const idx = (i as VInt).value;
        if (idx < 0 || idx >= str.length) return vcon("Nothing");
        return vcon("Just", [vchar(str[idx]!)]);
      }),
    ),
    toList: vforeign((s) => {
      const str = (s as VStr).value;
      let result: Value = vcon("Nil");
      for (let i = str.length - 1; i >= 0; i--) {
        result = vcon("Cons", [vchar(str[i]!), result]);
      }
      return result;
    }),
    fromList: vforeign((list) => {
      let result = "";
      let current = list as VCon;
      while (current.name === "Cons") {
        result += (current.args[0] as VChar).value;
        current = current.args[1] as VCon;
      }
      return vstr(result);
    }),
    eq: vforeign(curry2((a, b) => vbool((a as VStr).value === (b as VStr).value))),
    lt: vforeign(curry2((a, b) => vbool((a as VStr).value < (b as VStr).value))),
    split: vforeign(
      curry2((delimiter, s) => {
        const parts = (s as VStr).value.split((delimiter as VStr).value);
        let result: Value = vcon("Nil");
        for (let i = parts.length - 1; i >= 0; i--) {
          result = vcon("Cons", [vstr(parts[i]!), result]);
        }
        return result;
      }),
    ),
    join: vforeign(
      curry2((separator, list) => {
        const parts: string[] = [];
        let current = list as VCon;
        while (current.name === "Cons") {
          parts.push((current.args[0] as VStr).value);
          current = current.args[1] as VCon;
        }
        return vstr(parts.join((separator as VStr).value));
      }),
    ),
    trim: vforeign((s) => vstr((s as VStr).value.trim())),
    toUpper: vforeign((s) => vstr((s as VStr).value.toUpperCase())),
    toLower: vforeign((s) => vstr((s as VStr).value.toLowerCase())),
    contains: vforeign(
      curry2((needle, haystack) =>
        vbool((haystack as VStr).value.includes((needle as VStr).value)),
      ),
    ),
    startsWith: vforeign(
      curry2((prefix, s) => vbool((s as VStr).value.startsWith((prefix as VStr).value))),
    ),
    endsWith: vforeign(
      curry2((suffix, s) => vbool((s as VStr).value.endsWith((suffix as VStr).value))),
    ),
    replace: vforeign(
      curry3((search, replacement, s) =>
        vstr((s as VStr).value.replaceAll((search as VStr).value, (replacement as VStr).value)),
      ),
    ),
  },
  Char: {
    toInt: vforeign((c) => vint((c as VChar).value.charCodeAt(0))),
    fromInt: vforeign((n) => {
      const code = (n as VInt).value;
      if (code < 0 || code > 0x10ffff) return vcon("Nothing");
      return vcon("Just", [vchar(String.fromCodePoint(code))]);
    }),
    toString: vforeign((c) => vstr((c as VChar).value)),
    eq: vforeign(curry2((a, b) => vbool((a as VChar).value === (b as VChar).value))),
    lt: vforeign(curry2((a, b) => vbool((a as VChar).value < (b as VChar).value))),
    isDigit: vforeign((c) => {
      const ch = (c as VChar).value;
      return vbool(ch >= "0" && ch <= "9");
    }),
    isAlpha: vforeign((c) => {
      const ch = (c as VChar).value;
      return vbool((ch >= "a" && ch <= "z") || (ch >= "A" && ch <= "Z"));
    }),
    isAlphaNum: vforeign((c) => {
      const ch = (c as VChar).value;
      return vbool(
        (ch >= "a" && ch <= "z") || (ch >= "A" && ch <= "Z") || (ch >= "0" && ch <= "9"),
      );
    }),
    isSpace: vforeign((c) => {
      const ch = (c as VChar).value;
      return vbool(ch === " " || ch === "\t" || ch === "\n" || ch === "\r");
    }),
    isUpper: vforeign((c) => {
      const ch = (c as VChar).value;
      return vbool(ch >= "A" && ch <= "Z");
    }),
    isLower: vforeign((c) => {
      const ch = (c as VChar).value;
      return vbool(ch >= "a" && ch <= "z");
    }),
    toUpper: vforeign((c) => vchar((c as VChar).value.toUpperCase())),
    toLower: vforeign((c) => vchar((c as VChar).value.toLowerCase())),
  },
  Int: {
    add: vforeign(curry2((a, b) => vint((a as VInt).value + (b as VInt).value))),
    sub: vforeign(curry2((a, b) => vint((a as VInt).value - (b as VInt).value))),
    mul: vforeign(curry2((a, b) => vint((a as VInt).value * (b as VInt).value))),
    div: vforeign(curry2((a, b) => vint(Math.trunc((a as VInt).value / (b as VInt).value)))),
    mod: vforeign(curry2((a, b) => vint((a as VInt).value % (b as VInt).value))),
    neg: vforeign((a) => vint(-(a as VInt).value)),
    abs: vforeign((a) => vint(Math.abs((a as VInt).value))),
    eq: vforeign(curry2((a, b) => vbool((a as VInt).value === (b as VInt).value))),
    lt: vforeign(curry2((a, b) => vbool((a as VInt).value < (b as VInt).value))),
    le: vforeign(curry2((a, b) => vbool((a as VInt).value <= (b as VInt).value))),
    gt: vforeign(curry2((a, b) => vbool((a as VInt).value > (b as VInt).value))),
    ge: vforeign(curry2((a, b) => vbool((a as VInt).value >= (b as VInt).value))),
    toFloat: vforeign((a) => vfloat((a as VInt).value)),
    toString: vforeign((a) => vstr(String((a as VInt).value))),
    fromString: vforeign((s) => {
      const n = parseInt((s as VStr).value, 10);
      if (Number.isNaN(n)) return vcon("Nothing");
      return vcon("Just", [vint(n)]);
    }),
  },
  Float: {
    add: vforeign(curry2((a, b) => vfloat((a as VFloat).value + (b as VFloat).value))),
    sub: vforeign(curry2((a, b) => vfloat((a as VFloat).value - (b as VFloat).value))),
    mul: vforeign(curry2((a, b) => vfloat((a as VFloat).value * (b as VFloat).value))),
    div: vforeign(curry2((a, b) => vfloat((a as VFloat).value / (b as VFloat).value))),
    neg: vforeign((a) => vfloat(-(a as VFloat).value)),
    abs: vforeign((a) => vfloat(Math.abs((a as VFloat).value))),
    eq: vforeign(curry2((a, b) => vbool((a as VFloat).value === (b as VFloat).value))),
    lt: vforeign(curry2((a, b) => vbool((a as VFloat).value < (b as VFloat).value))),
    le: vforeign(curry2((a, b) => vbool((a as VFloat).value <= (b as VFloat).value))),
    gt: vforeign(curry2((a, b) => vbool((a as VFloat).value > (b as VFloat).value))),
    ge: vforeign(curry2((a, b) => vbool((a as VFloat).value >= (b as VFloat).value))),
    floor: vforeign((a) => vint(Math.floor((a as VFloat).value))),
    ceil: vforeign((a) => vint(Math.ceil((a as VFloat).value))),
    round: vforeign((a) => vint(Math.round((a as VFloat).value))),
    sqrt: vforeign((a) => vfloat(Math.sqrt((a as VFloat).value))),
    pow: vforeign(curry2((a, b) => vfloat(Math.pow((a as VFloat).value, (b as VFloat).value)))),
    sin: vforeign((a) => vfloat(Math.sin((a as VFloat).value))),
    cos: vforeign((a) => vfloat(Math.cos((a as VFloat).value))),
    tan: vforeign((a) => vfloat(Math.tan((a as VFloat).value))),
    log: vforeign((a) => vfloat(Math.log((a as VFloat).value))),
    exp: vforeign((a) => vfloat(Math.exp((a as VFloat).value))),
    toString: vforeign((a) => vstr(String((a as VFloat).value))),
    fromString: vforeign((s) => {
      const n = parseFloat((s as VStr).value);
      if (Number.isNaN(n)) return vcon("Nothing");
      return vcon("Just", [vfloat(n)]);
    }),
  },
  IO: {
    print: vforeign((s) => {
      process.stdout.write((s as VStr).value);
      return vcon("Unit");
    }),
    printLine: vforeign((s) => {
      console.log((s as VStr).value);
      return vcon("Unit");
    }),
    readLine: vforeign((_) => {
      try {
        const fs = require("fs");
        const buf = Buffer.alloc(1024);
        const n = fs.readSync(0, buf, 0, buf.length);
        return vcon("Right", [vstr(buf.toString("utf8", 0, n).replace(/\r?\n$/, ""))]);
      } catch (err) {
        return vcon("Left", [toIOError(err as NodeJS.ErrnoException, "<stdin>")]);
      }
    }),
    readFile: vforeign((path) => {
      const pathStr = (path as VStr).value;
      try {
        const fs = require("fs");
        return vcon("Right", [vstr(fs.readFileSync(pathStr, "utf8"))]);
      } catch (err) {
        return vcon("Left", [toIOError(err as NodeJS.ErrnoException, pathStr)]);
      }
    }),
    writeFile: vforeign(
      curry2((path, content) => {
        const pathStr = (path as VStr).value;
        try {
          const fs = require("fs");
          fs.writeFileSync(pathStr, (content as VStr).value, "utf8");
          return vcon("Right", [vcon("Unit")]);
        } catch (err) {
          return vcon("Left", [toIOError(err as NodeJS.ErrnoException, pathStr)]);
        }
      }),
    ),
    appendFile: vforeign(
      curry2((path, content) => {
        const pathStr = (path as VStr).value;
        try {
          const fs = require("fs");
          fs.appendFileSync(pathStr, (content as VStr).value, "utf8");
          return vcon("Right", [vcon("Unit")]);
        } catch (err) {
          return vcon("Left", [toIOError(err as NodeJS.ErrnoException, pathStr)]);
        }
      }),
    ),
    fileExists: vforeign((path) => {
      const fs = require("fs");
      return vbool(fs.existsSync((path as VStr).value));
    }),
    deleteFile: vforeign((path) => {
      const pathStr = (path as VStr).value;
      try {
        const fs = require("fs");
        fs.unlinkSync(pathStr);
        return vcon("Right", [vcon("Unit")]);
      } catch (err) {
        return vcon("Left", [toIOError(err as NodeJS.ErrnoException, pathStr)]);
      }
    }),
    args: vforeign((_) => {
      const args = process.argv.slice(2);
      let result: Value = vcon("Nil");
      for (let i = args.length - 1; i >= 0; i--) {
        result = vcon("Cons", [vstr(args[i]!), result]);
      }
      return result;
    }),
    exit: vforeign((code) => {
      process.exit((code as VInt).value);
    }),
    getEnv: vforeign((name) => {
      const value = process.env[(name as VStr).value];
      if (value === undefined) return vcon("Nothing");
      return vcon("Just", [vstr(value)]);
    }),
  },
  Debug: {
    log: vforeign((x) => {
      console.log(valueToString(x));
      return x;
    }),
    trace: vforeign(
      curry2((label, x) => {
        console.log(`${(label as VStr).value}:`, valueToString(x));
        return x;
      }),
    ),
    panic: vforeign((msg) => {
      throw new RuntimeError((msg as VStr).value);
    }),
  },
};

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
    case "Int":
      return vint(expr.value);

    case "Float":
      return vfloat(expr.value);

    case "Str":
      return vstr(expr.value);

    case "Bool":
      return vbool(expr.value);

    case "Char":
      return vchar(expr.value);

    case "Var": {
      const value = env.get(expr.name);
      if (!value) {
        throw new RuntimeError(`Undefined variable: ${expr.name}`);
      }
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
      // Textbook letrec: create ref cells for ALL bindings first
      const refs = new Map<string, VRef>();
      let newEnv = env;

      for (const binding of expr.bindings) {
        const ref = vref();
        refs.set(binding.name, ref);
        newEnv = extendEnv(newEnv, binding.name, ref);
      }

      // Evaluate all values with all refs in scope
      for (const binding of expr.bindings) {
        const value = evaluate(newEnv, binding.value);
        refs.get(binding.name)!.value = value;
      }

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
      const value = record.fields.get(expr.field);
      if (value === undefined) {
        throw new RuntimeError(`Record has no field '${expr.field}'`);
      }
      return value;
    }

    case "TupleIndex": {
      const tuple = evaluate(env, expr.tuple) as VTuple;
      const value = tuple.elements[expr.index];
      if (value === undefined) {
        throw new RuntimeError(`Tuple index ${expr.index} out of bounds`);
      }
      return value;
    }

    case "QualifiedVar": {
      // First check if this is a foreign module function
      const foreignModule = foreignRegistry[expr.moduleName];
      if (foreignModule) {
        const foreignFn = foreignModule[expr.member];
        if (foreignFn) {
          return foreignFn;
        }
      }
      // Otherwise check if the member was imported into the environment
      const value = env.get(expr.member);
      if (!value) {
        throw new RuntimeError(`Unknown qualified access: ${expr.moduleName}.${expr.member}`);
      }
      // Dereference if it's a ref cell (from letrec)
      if (value.kind === "VRef") {
        if (value.value === null) {
          throw new RuntimeError(`Uninitialized recursive binding: ${expr.member}`);
        }
        return value.value;
      }
      return value;
    }

    case "Match":
      return evalMatch(env, expr);

    default:
      return assertNever(expr);
  }
};

/**
 * Apply a function to an argument.
 * Type checker ensures func is either a closure, foreign function, or constructor.
 */
const apply = (func: Value, arg: Value): Value => {
  if (func.kind === "VClosure") {
    const newEnv = extendEnv(func.env, func.param, arg);
    return evaluate(newEnv, func.body);
  }
  if (func.kind === "VForeign") {
    return func.fn(arg);
  }
  // Must be a constructor (partial application)
  return vcon((func as VCon).name, [...(func as VCon).args, arg]);
};

/** Helper to get numeric value and determine if result should be float */
const numericOp = (left: Value, right: Value, op: (a: number, b: number) => number): Value => {
  const l = (left as VInt | VFloat).value;
  const r = (right as VInt | VFloat).value;
  const result = op(l, r);
  // Result is Float if either operand is Float
  if (left.kind === "VFloat" || right.kind === "VFloat") {
    return vfloat(result);
  }
  return vint(result);
};

const evalBinOp = (env: Env, expr: ast.BinOp): Value => {
  const left = evaluate(env, expr.left);
  const right = evaluate(env, expr.right);

  switch (expr.op) {
    // Arithmetic (type checker ensures numeric types)
    case "-":
      return numericOp(left, right, (a, b) => a - b);
    case "*":
      return numericOp(left, right, (a, b) => a * b);
    case "/": {
      const divisor = (right as VInt | VFloat).value;
      if (divisor === 0) throw new RuntimeError("Division by zero");
      return numericOp(left, right, (a, b) => a / b);
    }

    // Addition (numbers or strings - dispatch on left operand)
    case "+":
      if (left.kind === "VInt" || left.kind === "VFloat") {
        return numericOp(left, right, (a, b) => a + b);
      }
      return vstr((left as VStr).value + (right as VStr).value);

    // Comparisons (numbers, strings, or chars - dispatch on left operand)
    case "<":
      if (left.kind === "VInt" || left.kind === "VFloat")
        return vbool((left as VInt | VFloat).value < (right as VInt | VFloat).value);
      if (left.kind === "VChar") return vbool(left.value < (right as VChar).value);
      return vbool((left as VStr).value < (right as VStr).value);
    case ">":
      if (left.kind === "VInt" || left.kind === "VFloat")
        return vbool((left as VInt | VFloat).value > (right as VInt | VFloat).value);
      if (left.kind === "VChar") return vbool(left.value > (right as VChar).value);
      return vbool((left as VStr).value > (right as VStr).value);
    case "<=":
      if (left.kind === "VInt" || left.kind === "VFloat")
        return vbool((left as VInt | VFloat).value <= (right as VInt | VFloat).value);
      if (left.kind === "VChar") return vbool(left.value <= (right as VChar).value);
      return vbool((left as VStr).value <= (right as VStr).value);
    case ">=":
      if (left.kind === "VInt" || left.kind === "VFloat")
        return vbool((left as VInt | VFloat).value >= (right as VInt | VFloat).value);
      if (left.kind === "VChar") return vbool(left.value >= (right as VChar).value);
      return vbool((left as VStr).value >= (right as VStr).value);

    // Equality
    case "==":
      return vbool(valuesEqual(left, right));
    case "!=":
      return vbool(!valuesEqual(left, right));
  }
};

/**
 * Check structural equality of values.
 */
const valuesEqual = (a: Value, b: Value): boolean => {
  // Allow Int == Float comparisons (comparing numeric values)
  if ((a.kind === "VInt" || a.kind === "VFloat") && (b.kind === "VInt" || b.kind === "VFloat")) {
    return (a as VInt | VFloat).value === (b as VInt | VFloat).value;
  }
  if (a.kind !== b.kind) return false;

  switch (a.kind) {
    case "VInt":
      return a.value === (b as VInt).value;
    case "VFloat":
      return a.value === (b as VFloat).value;
    case "VStr":
      return a.value === (b as VStr).value;
    case "VChar":
      return a.value === (b as VChar).value;
    case "VBool":
      return a.value === (b as VBool).value;
    case "VCon": {
      const bCon = b as VCon;
      if (a.name !== bCon.name || a.args.length !== bCon.args.length) return false;
      for (let i = 0; i < a.args.length; i++) {
        if (!valuesEqual(a.args[i]!, bCon.args[i]!)) return false;
      }
      return true;
    }
    case "VTuple": {
      const bTuple = b as VTuple;
      if (a.elements.length !== bTuple.elements.length) return false;
      for (let i = 0; i < a.elements.length; i++) {
        if (!valuesEqual(a.elements[i]!, bTuple.elements[i]!)) return false;
      }
      return true;
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
    case "VForeign":
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
      if (typeof pattern.value === "number" && (value.kind === "VInt" || value.kind === "VFloat")) {
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

    case "PChar": {
      if (value.kind !== "VChar") return { matched: false };
      return pattern.value === value.value
        ? { matched: true, bindings: new Map() }
        : { matched: false };
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

    case "QualifiedPCon": {
      // Qualified constructor pattern (Module.Constructor)
      // Match against the constructor name only (module already resolved by type checker)
      if (value.kind !== "VCon" || value.name !== pattern.constructor) {
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

    case "POr": {
      // Try each alternative until one matches
      for (const alt of pattern.alternatives) {
        const result = matchPattern(alt, value);
        if (result.matched) return result;
      }
      return { matched: false };
    }

    default:
      return assertNever(pattern);
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
    case "VInt":
      return String(value.value);
    case "VFloat":
      return String(value.value);
    case "VStr":
      return `"${value.value}"`;
    case "VChar":
      return `'${value.value}'`;
    case "VBool":
      return String(value.value);
    case "VClosure":
    case "VForeign":
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

// Re-export empty env for convenience
export { emptyEnv };
