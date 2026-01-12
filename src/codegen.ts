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

// =============================================================================
// Runtime
// =============================================================================

const RUNTIME = `// Algow Runtime
"use strict";

// Deep structural equality
const $eq = (a, b) => {
  if (a === b) return true;
  if (typeof a !== typeof b) return false;
  if (typeof a !== "object" || a === null || b === null) return false;

  // List equality (objects with h/t fields)
  if ("h" in a && "t" in a && "h" in b && "t" in b) {
    return $eq(a.h, b.h) && $eq(a.t, b.t);
  }

  // ADT equality (arrays with numeric tag at index 0)
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    return a.every((x, i) => $eq(x, b[i]));
  }

  // Record equality
  const keysA = Object.keys(a);
  const keysB = Object.keys(b);
  if (keysA.length !== keysB.length) return false;
  return keysA.every(k => k in b && $eq(a[k], b[k]));
};

// Foreign function registry
const $foreign = {
  String: {
    length: (s) => s.length,
    concat: (a) => (b) => a + b,
    substring: (start) => (end) => (s) => s.substring(start, end),
    charAt: (i) => (s) => i < 0 || i >= s.length ? null : [0, s[i]],
    head: (s) => s.length === 0 ? null : [0, s[0]],
    tail: (s) => s.length === 0 ? "" : s.slice(1),
    drop: (n) => (s) => s.slice(n),
    take: (n) => (s) => s.slice(0, n),
    isEmpty: (s) => s.length === 0,
    toList: (s) => {
      let result = null;
      for (let i = s.length - 1; i >= 0; i--) result = { h: s[i], t: result };
      return result;
    },
    fromList: (list) => {
      let result = "";
      let current = list;
      while (current !== null) { result += current.h; current = current.t; }
      return result;
    },
    eq: (a) => (b) => a === b,
    lt: (a) => (b) => a < b,
    split: (delimiter) => (s) => {
      const parts = s.split(delimiter);
      let result = null;
      for (let i = parts.length - 1; i >= 0; i--) result = { h: parts[i], t: result };
      return result;
    },
    join: (separator) => (list) => {
      const parts = [];
      let current = list;
      while (current !== null) { parts.push(current.h); current = current.t; }
      return parts.join(separator);
    },
    trim: (s) => s.trim(),
    toUpper: (s) => s.toUpperCase(),
    toLower: (s) => s.toLowerCase(),
    contains: (needle) => (haystack) => haystack.includes(needle),
    startsWith: (prefix) => (s) => s.startsWith(prefix),
    endsWith: (suffix) => (s) => s.endsWith(suffix),
    replace: (search) => (replacement) => (s) => s.replaceAll(search, replacement),
  },
  Char: {
    toInt: (c) => c.charCodeAt(0),
    fromInt: (n) => n < 0 || n > 0x10FFFF ? null : [0, String.fromCodePoint(n)],
    toString: (c) => c,
    eq: (a) => (b) => a === b,
    lt: (a) => (b) => a < b,
    isDigit: (c) => c >= "0" && c <= "9",
    isAlpha: (c) => (c >= "a" && c <= "z") || (c >= "A" && c <= "Z"),
    isAlphaNum: (c) => (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || (c >= "0" && c <= "9"),
    isSpace: (c) => c === " " || c === "\\t" || c === "\\n" || c === "\\r",
    isUpper: (c) => c >= "A" && c <= "Z",
    isLower: (c) => c >= "a" && c <= "z",
    toUpper: (c) => c.toUpperCase(),
    toLower: (c) => c.toLowerCase(),
    isIdentStart: (c) => (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || c === "_",
    isIdentChar: (c) => (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || (c >= "0" && c <= "9") || c === "_",
  },
  Int: {
    add: (a) => (b) => a + b,
    sub: (a) => (b) => a - b,
    mul: (a) => (b) => a * b,
    div: (a) => (b) => Math.trunc(a / b),
    mod: (a) => (b) => a % b,
    neg: (a) => -a,
    abs: (a) => Math.abs(a),
    eq: (a) => (b) => a === b,
    lt: (a) => (b) => a < b,
    le: (a) => (b) => a <= b,
    gt: (a) => (b) => a > b,
    ge: (a) => (b) => a >= b,
    toFloat: (a) => a,
    toString: (a) => String(a),
    fromString: (s) => { const n = parseInt(s, 10); return Number.isNaN(n) ? null : [0, n]; },
  },
  Float: {
    add: (a) => (b) => a + b,
    sub: (a) => (b) => a - b,
    mul: (a) => (b) => a * b,
    div: (a) => (b) => a / b,
    neg: (a) => -a,
    abs: (a) => Math.abs(a),
    eq: (a) => (b) => a === b,
    lt: (a) => (b) => a < b,
    le: (a) => (b) => a <= b,
    gt: (a) => (b) => a > b,
    ge: (a) => (b) => a >= b,
    floor: (a) => Math.floor(a),
    ceil: (a) => Math.ceil(a),
    round: (a) => Math.round(a),
    sqrt: (a) => Math.sqrt(a),
    pow: (a) => (b) => Math.pow(a, b),
    sin: (a) => Math.sin(a),
    cos: (a) => Math.cos(a),
    tan: (a) => Math.tan(a),
    log: (a) => Math.log(a),
    exp: (a) => Math.exp(a),
    toString: (a) => String(a),
    fromString: (s) => { const n = parseFloat(s); return Number.isNaN(n) ? null : [0, n]; },
  },
  IO: {
    print: (s) => { process.stdout.write(s); return null; },
    printLine: (s) => { console.log(s); return null; },
    readFile: (path) => {
      try {
        const fs = require("fs");
        return [1, fs.readFileSync(path, "utf8")];
      } catch (err) {
        return [0, [0, path]];
      }
    },
    writeFile: (path) => (content) => {
      try {
        const fs = require("fs");
        fs.writeFileSync(path, content, "utf8");
        return [1, null];
      } catch (err) {
        return [0, [0, path]];
      }
    },
    appendFile: (path) => (content) => {
      try {
        const fs = require("fs");
        fs.appendFileSync(path, content, "utf8");
        return [1, null];
      } catch (err) {
        return [0, [0, path]];
      }
    },
    fileExists: (path) => { const fs = require("fs"); return fs.existsSync(path); },
    isDirectory: (path) => {
      try { const fs = require("fs"); return fs.statSync(path).isDirectory(); }
      catch (err) { return false; }
    },
    readDir: (path) => {
      try {
        const fs = require("fs");
        const files = fs.readdirSync(path);
        let result = null;
        for (let i = files.length - 1; i >= 0; i--) result = { h: files[i], t: result };
        return [1, result];
      } catch (err) {
        return [0, [0, path]];
      }
    },
    args: (_) => {
      const args = process.argv.slice(2);
      let result = null;
      for (let i = args.length - 1; i >= 0; i--) result = { h: args[i], t: result };
      return result;
    },
    exit: (code) => { process.exit(code); },
    getEnv: (name) => {
      const value = process.env[name];
      return value === undefined ? null : [0, value];
    },
  },
  Debug: {
    log: (x) => { console.log(x); return x; },
    trace: (label) => (x) => { console.log(label + ":", x); return x; },
    panic: (msg) => { throw new Error(msg); },
  },
  Map: {
    empty: new Map(),
    singleton: (k) => (v) => new Map([[k, v]]),
    insert: (k) => (v) => (m) => new Map(m).set(k, v),
    lookup: (k) => (m) => m.has(k) ? [0, m.get(k)] : null,
    delete: (k) => (m) => { const m2 = new Map(m); m2.delete(k); return m2; },
    member: (k) => (m) => m.has(k),
    size: (m) => m.size,
    keys: (m) => {
      let result = null;
      for (const k of [...m.keys()].reverse()) result = { h: k, t: result };
      return result;
    },
    values: (m) => {
      let result = null;
      for (const v of [...m.values()].reverse()) result = { h: v, t: result };
      return result;
    },
    toList: (m) => {
      let result = null;
      for (const [k, v] of [...m.entries()].reverse()) result = { h: [k, v], t: result };
      return result;
    },
    fromList: (list) => {
      const m = new Map();
      let current = list;
      while (current !== null) { const [k, v] = current.h; m.set(k, v); current = current.t; }
      return m;
    },
  },
  Set: {
    empty: new Set(),
    singleton: (v) => new Set([v]),
    insert: (v) => (s) => new Set(s).add(v),
    delete: (v) => (s) => { const s2 = new Set(s); s2.delete(v); return s2; },
    member: (v) => (s) => s.has(v),
    size: (s) => s.size,
    toList: (s) => {
      let result = null;
      for (const v of [...s].reverse()) result = { h: v, t: result };
      return result;
    },
    fromList: (list) => {
      const s = new Set();
      let current = list;
      while (current !== null) { s.add(current.h); current = current.t; }
      return s;
    },
    union: (s1) => (s2) => s1.union(s2),
    intersect: (s1) => (s2) => s1.intersection(s2),
    difference: (s1) => (s2) => s1.difference(s2),
  },
  Operators: {
    "+": (a) => (b) => a + b,
    "-": (a) => (b) => a - b,
    "*": (a) => (b) => a * b,
    "/": (a) => (b) => Math.trunc(a / b),
    "==": (a) => (b) => $eq(a, b),
    "!=": (a) => (b) => !$eq(a, b),
    "<": (a) => (b) => a < b,
    "<=": (a) => (b) => a <= b,
    ">": (a) => (b) => a > b,
    ">=": (a) => (b) => a >= b,
  },
};

`;

// =============================================================================
// Code Generation Context
// =============================================================================

type TagMap = Map<string, number>;

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
};

const createContext = (): CodeGenContext => ({
  indent: 0,
  lines: [],
  tagMap: new Map(),
  declaredNames: new Set(),
  scrutineeCounter: 0,
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
  const base = name.original.replace(/[^a-zA-Z0-9_$]/g, "_");
  // For negative IDs (temporaries from lowering), use absolute value
  const suffix = name.id < 0 ? `$${-name.id}` : `$${name.id}`;
  const result = `${base}${suffix}`;
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

const genAtom = (ctx: CodeGenContext, atom: IR.Atom): string => {
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
      // processing, so just reference them by name
      return atom.name;
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
      const func = genAtom(ctx, binding.func);
      const arg = genAtom(ctx, binding.arg);
      // Direct function call
      return `${func}(${arg})`;
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

  // Generate body
  ctx.indent++;
  const bodyLines: string[] = [];
  const savedLines = ctx.lines;
  const savedDeclared = new Set(ctx.declaredNames);
  ctx.lines = bodyLines;

  const bodyResult = genExpr(ctx, binding.body);

  ctx.lines = savedLines;
  ctx.declaredNames = savedDeclared;
  ctx.indent--;

  if (bodyLines.length === 0) {
    return `(${param}) => ${bodyResult}`;
  }

  const indentation = "  ".repeat(ctx.indent + 1);
  const body = bodyLines.map((l) => indentation + l.trimStart()).join("\n");
  return `(${param}) => {\n${body}\n${"  ".repeat(ctx.indent)}  return ${bodyResult};\n${"  ".repeat(ctx.indent)}}`;
};

const genTailRecursiveLambda = (ctx: CodeGenContext, binding: IR.IRBLambda): string => {
  const tco = binding.tailRecursive!;
  const params = tco.params.map(nameToJs);

  // Find innermost body
  let innerBody = binding.body;
  while (innerBody.kind === "IRLet" && innerBody.binding.kind === "IRBLambda") {
    innerBody = innerBody.binding.body;
  }

  // Generate loop body
  ctx.indent++;
  const bodyLines: string[] = [];
  const savedLines = ctx.lines;
  const savedDeclared = new Set(ctx.declaredNames);
  ctx.lines = bodyLines;

  const bodyResult = genExpr(ctx, innerBody);

  ctx.lines = savedLines;
  ctx.declaredNames = savedDeclared;
  ctx.indent--;

  // Build curried function with while loop
  if (params.length === 1) {
    const indentation = "  ".repeat(ctx.indent + 1);
    const loopIndent = "  ".repeat(ctx.indent + 2);
    const body = bodyLines.map((l) => loopIndent + l.trimStart()).join("\n");
    return `(${params[0]}) => {\n${indentation}while (true) {\n${body}\n${loopIndent}return ${bodyResult};\n${indentation}}\n${"  ".repeat(ctx.indent)}}`;
  }

  // Multi-param: curried function
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
// Match Generation
// =============================================================================

const genMatch = (ctx: CodeGenContext, match: IR.IRMatch): string => {
  // If scrutinee is a simple variable, use it directly without IIFE wrapping
  const isSimpleVar = match.scrutinee.kind === "AVar";
  const scrutineeVar = isSimpleVar ? nameToJs(match.scrutinee.name) : freshScrutinee(ctx);
  const scrutineeExpr = isSimpleVar ? null : genAtom(ctx, match.scrutinee);

  const lines: string[] = [];
  if (isSimpleVar) {
    lines.push("(() => {");
  } else {
    lines.push(`((${scrutineeVar}) => {`);
  }

  for (let i = 0; i < match.cases.length; i++) {
    const case_ = match.cases[i]!;
    const { condition, bindings } = genPatternMatch(ctx, scrutineeVar, case_.pattern);
    const isLast = i === match.cases.length - 1;

    // Use plain 'else' for last case (type checker ensures exhaustiveness)
    let prefix: string;
    if (i === 0) {
      prefix = `if (${condition})`;
    } else if (isLast) {
      prefix = "} else";
    } else {
      prefix = `} else if (${condition})`;
    }
    lines.push(`  ${prefix} {`);

    // Emit bindings
    for (const [name, expr] of bindings) {
      lines.push(`    const ${name} = ${expr};`);
    }

    // Generate guard if present
    let guardCode: string | null = null;
    if (case_.guard) {
      ctx.indent += 2;
      const guardLines: string[] = [];
      const savedLines = ctx.lines;
      ctx.lines = guardLines;
      guardCode = genExpr(ctx, case_.guard);
      ctx.lines = savedLines;
      ctx.indent -= 2;
      for (const line of guardLines) {
        lines.push("    " + line.trimStart());
      }
    }

    // Generate body
    ctx.indent += 2;
    const bodyLines: string[] = [];
    const savedLines = ctx.lines;
    const savedDeclared = new Set(ctx.declaredNames);
    ctx.lines = bodyLines;
    const bodyResult = genExpr(ctx, case_.body);
    ctx.lines = savedLines;
    ctx.declaredNames = savedDeclared;
    ctx.indent -= 2;

    for (const line of bodyLines) {
      lines.push("    " + line.trimStart());
    }

    if (guardCode) {
      lines.push(`    if (${guardCode}) return ${bodyResult};`);
    } else {
      lines.push(`    return ${bodyResult};`);
    }
  }

  lines.push("  }");
  if (isSimpleVar) {
    lines.push("})()");
  } else {
    lines.push(`})(${scrutineeExpr})`);
  }

  return lines.join("\n" + "  ".repeat(ctx.indent));
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
          if (con.arity === 0) {
            // Nullary constructor
            emit(ctx, `const ${con.name} = [${con.tag}];`);
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
            emit(ctx, `const ${con.name} = ${func};`);
          }
          ctx.declaredNames.add(con.name);
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

export type CodeGenOutput = {
  readonly code: string;
};

/**
 * Generate JavaScript from IR program.
 */
export const generateJS = (program: IR.IRProgram): CodeGenOutput => {
  const ctx = createContext();

  // Generate declarations
  for (const decl of program.decls) {
    genDecl(ctx, decl);
  }

  // Generate main expression
  let mainCode = "";
  if (program.main) {
    mainCode = genExpr(ctx, program.main);
  }

  // Combine runtime + generated code
  const code = [
    RUNTIME,
    "// Generated code",
    ...ctx.lines,
    program.main ? `const $result = ${mainCode};` : "",
    program.main ? "console.log($result);" : "",
  ]
    .filter(Boolean)
    .join("\n");

  return { code };
};

/**
 * Generate JavaScript from a single IR expression (for testing).
 */
export const generateExprJS = (expr: IR.IRExpr, typeDecls: IR.IRDeclType[] = []): CodeGenOutput => {
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

  const code = [
    RUNTIME,
    "// Generated code",
    ...ctx.lines,
    `const $result = ${result};`,
    "console.log($result);",
  ].join("\n");

  return { code };
};
