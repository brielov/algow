// =============================================================================
// ALGOW RUNTIME
// =============================================================================
//
// This file provides the runtime support for compiled Algow programs.
// It defines the core value representations and helper functions.
//
// Value representations:
// - Primitives: JavaScript primitives (number, string, boolean)
// - Closures: Native JavaScript closures
// - Constructors: { $tag: "Name", $args: [...] }
// - Tuples: JavaScript arrays
// - Records: Plain JavaScript objects
//
// =============================================================================

"use strict";

// =============================================================================
// CORE RUNTIME FUNCTIONS
// =============================================================================

/**
 * Apply a function to an argument.
 * Handles both closures and partial constructor application.
 */
const $apply = (fn, arg) => {
  if (typeof fn === "function") {
    return fn(arg);
  }
  // Constructor - partial application
  return { $tag: fn.$tag, $args: [...fn.$args, arg] };
};

/**
 * Create a constructor value.
 */
const $con = (tag, ...args) => ({ $tag: tag, $args: args });

/**
 * Deep structural equality for values.
 */
const $eq = (a, b) => {
  // Same reference or primitive equality
  if (a === b) return true;

  // Different types
  if (typeof a !== typeof b) return false;
  if (typeof a !== "object" || a === null) return false;

  // Constructor equality
  if ("$tag" in a && "$tag" in b) {
    if (a.$tag !== b.$tag) return false;
    if (a.$args.length !== b.$args.length) return false;
    return a.$args.every((x, i) => $eq(x, b.$args[i]));
  }

  // Array (tuple) equality
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length !== b.length) return false;
    return a.every((x, i) => $eq(x, b[i]));
  }

  // Object (record) equality
  const keysA = Object.keys(a);
  const keysB = Object.keys(b);
  if (keysA.length !== keysB.length) return false;
  return keysA.every((k) => k in b && $eq(a[k], b[k]));
};

// =============================================================================
// FOREIGN FUNCTION REGISTRY
// =============================================================================

/**
 * Foreign function implementations organized by module.
 */
const $foreign = {
  // ==========================================================================
  // String Module
  // ==========================================================================
  String: {
    length: (s) => s.length,
    concat: (a) => (b) => a + b,
    substring: (start) => (end) => (s) => s.substring(start, end),
    charAt: (i) => (s) => {
      if (i < 0 || i >= s.length) return $con("Nothing");
      return $con("Just", s[i]);
    },
    head: (s) => {
      if (s.length === 0) return $con("Nothing");
      return $con("Just", s[0]);
    },
    tail: (s) => (s.length === 0 ? "" : s.slice(1)),
    drop: (n) => (s) => s.slice(n),
    take: (n) => (s) => s.slice(0, n),
    isEmpty: (s) => s.length === 0,
    toList: (s) => {
      let result = $con("Nil");
      for (let i = s.length - 1; i >= 0; i--) {
        result = $con("Cons", s[i], result);
      }
      return result;
    },
    fromList: (list) => {
      let result = "";
      let current = list;
      while (current.$tag === "Cons") {
        result += current.$args[0];
        current = current.$args[1];
      }
      return result;
    },
    eq: (a) => (b) => a === b,
    lt: (a) => (b) => a < b,
    split: (delimiter) => (s) => {
      const parts = s.split(delimiter);
      let result = $con("Nil");
      for (let i = parts.length - 1; i >= 0; i--) {
        result = $con("Cons", parts[i], result);
      }
      return result;
    },
    join: (separator) => (list) => {
      const parts = [];
      let current = list;
      while (current.$tag === "Cons") {
        parts.push(current.$args[0]);
        current = current.$args[1];
      }
      return parts.join(separator);
    },
    trim: (s) => s.trim(),
    toUpper: (s) => s.toUpperCase(),
    toLower: (s) => s.toLowerCase(),
    contains: (needle) => (haystack) => haystack.includes(needle),
    startsWith: (prefix) => (s) => s.startsWith(prefix),
    endsWith: (suffix) => (s) => s.endsWith(suffix),
    replace: (search) => (replacement) => (s) => s.replaceAll(search, replacement),
    replicate: (n) => (s) => s.repeat(n),
  },

  // ==========================================================================
  // Char Module
  // ==========================================================================
  Char: {
    toInt: (c) => c.charCodeAt(0),
    fromInt: (n) => {
      if (n < 0 || n > 0x10ffff) return $con("Nothing");
      return $con("Just", String.fromCodePoint(n));
    },
    toString: (c) => c,
    eq: (a) => (b) => a === b,
    lt: (a) => (b) => a < b,
    isDigit: (c) => c >= "0" && c <= "9",
    isAlpha: (c) => (c >= "a" && c <= "z") || (c >= "A" && c <= "Z"),
    isAlphaNum: (c) => (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || (c >= "0" && c <= "9"),
    isSpace: (c) => c === " " || c === "\t" || c === "\n" || c === "\r",
    isUpper: (c) => c >= "A" && c <= "Z",
    isLower: (c) => c >= "a" && c <= "z",
    toUpper: (c) => c.toUpperCase(),
    toLower: (c) => c.toLowerCase(),
    isIdentStart: (c) => (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || c === "_",
    isIdentChar: (c) =>
      (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || (c >= "0" && c <= "9") || c === "_",
  },

  // ==========================================================================
  // Int Module
  // ==========================================================================
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
    fromString: (s) => {
      const n = parseInt(s, 10);
      if (Number.isNaN(n)) return $con("Nothing");
      return $con("Just", n);
    },
  },

  // ==========================================================================
  // Float Module
  // ==========================================================================
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
    fromString: (s) => {
      const n = parseFloat(s);
      if (Number.isNaN(n)) return $con("Nothing");
      return $con("Just", n);
    },
  },

  // ==========================================================================
  // IO Module
  // ==========================================================================
  IO: {
    _toIOError: (err, path) => {
      const code = err.code;
      if (code === "ENOENT") return $con("FileNotFound", path);
      if (code === "EACCES" || code === "EPERM") return $con("PermissionDenied", path);
      if (code === "EISDIR") return $con("IsDirectory", path);
      if (code === "EEXIST") return $con("AlreadyExists", path);
      return $con("UnknownError", err.message || String(err));
    },

    print: (s) => {
      process.stdout.write(s);
      return $con("Unit");
    },
    printLine: (s) => {
      console.log(s);
      return $con("Unit");
    },
    readLine: (_) => {
      try {
        const fs = require("fs");
        const buf = Buffer.alloc(1024);
        const n = fs.readSync(0, buf, 0, buf.length);
        return $con("Right", buf.toString("utf8", 0, n).replace(/\r?\n$/, ""));
      } catch (err) {
        return $con("Left", $foreign.IO._toIOError(err, "<stdin>"));
      }
    },
    readFile: (path) => {
      try {
        const fs = require("fs");
        return $con("Right", fs.readFileSync(path, "utf8"));
      } catch (err) {
        return $con("Left", $foreign.IO._toIOError(err, path));
      }
    },
    writeFile: (path) => (content) => {
      try {
        const fs = require("fs");
        fs.writeFileSync(path, content, "utf8");
        return $con("Right", $con("Unit"));
      } catch (err) {
        return $con("Left", $foreign.IO._toIOError(err, path));
      }
    },
    appendFile: (path) => (content) => {
      try {
        const fs = require("fs");
        fs.appendFileSync(path, content, "utf8");
        return $con("Right", $con("Unit"));
      } catch (err) {
        return $con("Left", $foreign.IO._toIOError(err, path));
      }
    },
    fileExists: (path) => {
      const fs = require("fs");
      return fs.existsSync(path);
    },
    isDirectory: (path) => {
      try {
        const fs = require("fs");
        return fs.statSync(path).isDirectory();
      } catch {
        return false;
      }
    },
    readDir: (path) => {
      try {
        const fs = require("fs");
        const files = fs.readdirSync(path);
        let result = $con("Nil");
        for (let i = files.length - 1; i >= 0; i--) {
          result = $con("Cons", files[i], result);
        }
        return $con("Right", result);
      } catch (err) {
        return $con("Left", $foreign.IO._toIOError(err, path));
      }
    },
    deleteFile: (path) => {
      try {
        const fs = require("fs");
        fs.unlinkSync(path);
        return $con("Right", $con("Unit"));
      } catch (err) {
        return $con("Left", $foreign.IO._toIOError(err, path));
      }
    },
    args: (_) => {
      const args = process.argv.slice(2);
      let result = $con("Nil");
      for (let i = args.length - 1; i >= 0; i--) {
        result = $con("Cons", args[i], result);
      }
      return result;
    },
    exit: (code) => {
      process.exit(code);
    },
    getEnv: (name) => {
      const value = process.env[name];
      if (value === undefined) return $con("Nothing");
      return $con("Just", value);
    },
  },

  // ==========================================================================
  // Debug Module
  // ==========================================================================
  Debug: {
    log: (x) => {
      console.log(x);
      return x;
    },
    trace: (label) => (x) => {
      console.log(label + ":", x);
      return x;
    },
    panic: (msg) => {
      throw new Error(msg);
    },
  },

  // ==========================================================================
  // Map Module (String keys only)
  // ==========================================================================
  Map: {
    empty: new Map(),
    singleton: (k) => (v) => new Map([[k, v]]),
    insert: (k) => (v) => (m) => new Map(m).set(k, v),
    lookup: (k) => (m) => {
      if (m.has(k)) return $con("Just", m.get(k));
      return $con("Nothing");
    },
    delete: (k) => (m) => {
      const m2 = new Map(m);
      m2.delete(k);
      return m2;
    },
    member: (k) => (m) => m.has(k),
    size: (m) => m.size,
    keys: (m) => {
      let result = $con("Nil");
      for (const k of [...m.keys()].reverse()) {
        result = $con("Cons", k, result);
      }
      return result;
    },
    values: (m) => {
      let result = $con("Nil");
      for (const v of [...m.values()].reverse()) {
        result = $con("Cons", v, result);
      }
      return result;
    },
    toList: (m) => {
      let result = $con("Nil");
      for (const [k, v] of [...m.entries()].reverse()) {
        result = $con("Cons", [k, v], result);
      }
      return result;
    },
    fromList: (list) => {
      const m = new Map();
      let current = list;
      while (current.$tag === "Cons") {
        const [k, v] = current.$args[0];
        m.set(k, v);
        current = current.$args[1];
      }
      return m;
    },
  },

  // ==========================================================================
  // Set Module (String values only)
  // ==========================================================================
  Set: {
    empty: new Set(),
    singleton: (v) => new Set([v]),
    insert: (v) => (s) => new Set(s).add(v),
    delete: (v) => (s) => {
      const s2 = new Set(s);
      s2.delete(v);
      return s2;
    },
    member: (v) => (s) => s.has(v),
    size: (s) => s.size,
    toList: (s) => {
      let result = $con("Nil");
      for (const v of [...s].reverse()) {
        result = $con("Cons", v, result);
      }
      return result;
    },
    fromList: (list) => {
      const s = new Set();
      let current = list;
      while (current.$tag === "Cons") {
        s.add(current.$args[0]);
        current = current.$args[1];
      }
      return s;
    },
    union: (s1) => (s2) => s1.union(s2),
    intersect: (s1) => (s2) => s1.intersection(s2),
    difference: (s1) => (s2) => s1.difference(s2),
  },
};

// =============================================================================
// EXPORTS (for Node.js module usage)
// =============================================================================

if (typeof module !== "undefined") {
  module.exports = { $apply, $con, $eq, $foreign };
}
