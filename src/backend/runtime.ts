/**
 * JavaScript Runtime for Algow
 *
 * Defines the runtime representation of values and helper functions
 * needed by generated JavaScript code.
 *
 * Value representations:
 * - Primitives: JavaScript primitives (number, string, boolean)
 * - Closures: Native JavaScript closures (no explicit env needed)
 * - Constructors: { $tag: "Name", $args: [...] }
 * - Tuples: JavaScript arrays
 * - Records: Plain JavaScript objects
 */

export const RUNTIME = `// Algow Runtime
"use strict";

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
  return keysA.every(k => k in b && $eq(a[k], b[k]));
};

/**
 * Foreign function registry.
 * Modules provide their foreign function implementations here.
 * Structure: { ModuleName: { functionName: implementation } }
 */
const $foreign = {
  // ==========================================================================
  // String Module
  // ==========================================================================
  String: {
    // length : String -> Int
    length: (s) => s.length,

    // concat : String -> String -> String
    concat: (a) => (b) => a + b,

    // substring : Int -> Int -> String -> String
    substring: (start) => (end) => (s) => s.substring(start, end),

    // charAt : Int -> String -> Maybe Char
    charAt: (i) => (s) => {
      if (i < 0 || i >= s.length) return $con("Nothing");
      return $con("Just", s[i]);
    },

    // head : String -> Maybe Char
    head: (s) => {
      if (s.length === 0) return $con("Nothing");
      return $con("Just", s[0]);
    },

    // tail : String -> String
    tail: (s) => (s.length === 0 ? "" : s.slice(1)),

    // isEmpty : String -> Bool
    isEmpty: (s) => s.length === 0,

    // toList : String -> List Char
    toList: (s) => {
      let result = $con("Nil");
      for (let i = s.length - 1; i >= 0; i--) {
        result = $con("Cons", s[i], result);
      }
      return result;
    },

    // fromList : List Char -> String
    fromList: (list) => {
      let result = "";
      let current = list;
      while (current.$tag === "Cons") {
        result += current.$args[0];
        current = current.$args[1];
      }
      return result;
    },

    // eq : String -> String -> Bool
    eq: (a) => (b) => a === b,

    // lt : String -> String -> Bool
    lt: (a) => (b) => a < b,

    // split : String -> String -> List String
    split: (delimiter) => (s) => {
      const parts = s.split(delimiter);
      let result = $con("Nil");
      for (let i = parts.length - 1; i >= 0; i--) {
        result = $con("Cons", parts[i], result);
      }
      return result;
    },

    // join : String -> List String -> String
    join: (separator) => (list) => {
      const parts = [];
      let current = list;
      while (current.$tag === "Cons") {
        parts.push(current.$args[0]);
        current = current.$args[1];
      }
      return parts.join(separator);
    },

    // trim : String -> String
    trim: (s) => s.trim(),

    // toUpper : String -> String
    toUpper: (s) => s.toUpperCase(),

    // toLower : String -> String
    toLower: (s) => s.toLowerCase(),

    // contains : String -> String -> Bool
    contains: (needle) => (haystack) => haystack.includes(needle),

    // startsWith : String -> String -> Bool
    startsWith: (prefix) => (s) => s.startsWith(prefix),

    // endsWith : String -> String -> Bool
    endsWith: (suffix) => (s) => s.endsWith(suffix),

    // replace : String -> String -> String -> String
    replace: (search) => (replacement) => (s) => s.replaceAll(search, replacement),
  },

  // ==========================================================================
  // Char Module
  // ==========================================================================
  Char: {
    // toInt : Char -> Int
    toInt: (c) => c.charCodeAt(0),

    // fromInt : Int -> Maybe Char
    fromInt: (n) => {
      if (n < 0 || n > 0x10FFFF) return $con("Nothing");
      return $con("Just", String.fromCodePoint(n));
    },

    // toString : Char -> String
    toString: (c) => c,

    // eq : Char -> Char -> Bool
    eq: (a) => (b) => a === b,

    // lt : Char -> Char -> Bool
    lt: (a) => (b) => a < b,

    // isDigit : Char -> Bool
    isDigit: (c) => c >= "0" && c <= "9",

    // isAlpha : Char -> Bool
    isAlpha: (c) => (c >= "a" && c <= "z") || (c >= "A" && c <= "Z"),

    // isAlphaNum : Char -> Bool
    isAlphaNum: (c) => (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || (c >= "0" && c <= "9"),

    // isSpace : Char -> Bool
    isSpace: (c) => c === " " || c === "\\t" || c === "\\n" || c === "\\r",

    // isUpper : Char -> Bool
    isUpper: (c) => c >= "A" && c <= "Z",

    // isLower : Char -> Bool
    isLower: (c) => c >= "a" && c <= "z",

    // toUpper : Char -> Char
    toUpper: (c) => c.toUpperCase(),

    // toLower : Char -> Char
    toLower: (c) => c.toLowerCase(),

    // isIdentStart : Char -> Bool (can start identifier: alpha or underscore)
    isIdentStart: (c) => (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || c === "_",

    // isIdentChar : Char -> Bool (can continue identifier: alphanumeric or underscore)
    isIdentChar: (c) =>
      (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || (c >= "0" && c <= "9") || c === "_",
  },

  // ==========================================================================
  // Int Module
  // ==========================================================================
  Int: {
    // add : Int -> Int -> Int
    add: (a) => (b) => a + b,

    // sub : Int -> Int -> Int
    sub: (a) => (b) => a - b,

    // mul : Int -> Int -> Int
    mul: (a) => (b) => a * b,

    // div : Int -> Int -> Int (truncating division)
    div: (a) => (b) => Math.trunc(a / b),

    // mod : Int -> Int -> Int
    mod: (a) => (b) => a % b,

    // neg : Int -> Int
    neg: (a) => -a,

    // abs : Int -> Int
    abs: (a) => Math.abs(a),

    // eq : Int -> Int -> Bool
    eq: (a) => (b) => a === b,

    // lt : Int -> Int -> Bool
    lt: (a) => (b) => a < b,

    // le : Int -> Int -> Bool
    le: (a) => (b) => a <= b,

    // gt : Int -> Int -> Bool
    gt: (a) => (b) => a > b,

    // ge : Int -> Int -> Bool
    ge: (a) => (b) => a >= b,

    // toFloat : Int -> Float
    toFloat: (a) => a,

    // toString : Int -> String
    toString: (a) => String(a),

    // fromString : String -> Maybe Int
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
    // add : Float -> Float -> Float
    add: (a) => (b) => a + b,

    // sub : Float -> Float -> Float
    sub: (a) => (b) => a - b,

    // mul : Float -> Float -> Float
    mul: (a) => (b) => a * b,

    // div : Float -> Float -> Float
    div: (a) => (b) => a / b,

    // neg : Float -> Float
    neg: (a) => -a,

    // abs : Float -> Float
    abs: (a) => Math.abs(a),

    // eq : Float -> Float -> Bool
    eq: (a) => (b) => a === b,

    // lt : Float -> Float -> Bool
    lt: (a) => (b) => a < b,

    // le : Float -> Float -> Bool
    le: (a) => (b) => a <= b,

    // gt : Float -> Float -> Bool
    gt: (a) => (b) => a > b,

    // ge : Float -> Float -> Bool
    ge: (a) => (b) => a >= b,

    // floor : Float -> Int
    floor: (a) => Math.floor(a),

    // ceil : Float -> Int
    ceil: (a) => Math.ceil(a),

    // round : Float -> Int
    round: (a) => Math.round(a),

    // sqrt : Float -> Float
    sqrt: (a) => Math.sqrt(a),

    // pow : Float -> Float -> Float
    pow: (a) => (b) => Math.pow(a, b),

    // sin : Float -> Float
    sin: (a) => Math.sin(a),

    // cos : Float -> Float
    cos: (a) => Math.cos(a),

    // tan : Float -> Float
    tan: (a) => Math.tan(a),

    // log : Float -> Float
    log: (a) => Math.log(a),

    // exp : Float -> Float
    exp: (a) => Math.exp(a),

    // toString : Float -> String
    toString: (a) => String(a),

    // fromString : String -> Maybe Float
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
    // Helper to convert Node.js errors to IOError
    _toIOError: (err, path) => {
      const code = err.code;
      if (code === "ENOENT") return $con("FileNotFound", path);
      if (code === "EACCES" || code === "EPERM") return $con("PermissionDenied", path);
      if (code === "EISDIR") return $con("IsDirectory", path);
      if (code === "EEXIST") return $con("AlreadyExists", path);
      return $con("UnknownError", err.message || String(err));
    },

    // print : String -> Unit
    print: (s) => { process.stdout.write(s); return $con("Unit"); },

    // printLine : String -> Unit
    printLine: (s) => { console.log(s); return $con("Unit"); },

    // readLine : Unit -> Either IOError String
    readLine: (_) => {
      try {
        const fs = require("fs");
        const buf = Buffer.alloc(1024);
        const n = fs.readSync(0, buf, 0, buf.length);
        return $con("Right", buf.toString("utf8", 0, n).replace(/\\r?\\n$/, ""));
      } catch (err) {
        return $con("Left", $foreign.IO._toIOError(err, "<stdin>"));
      }
    },

    // readFile : String -> Either IOError String
    readFile: (path) => {
      try {
        const fs = require("fs");
        return $con("Right", fs.readFileSync(path, "utf8"));
      } catch (err) {
        return $con("Left", $foreign.IO._toIOError(err, path));
      }
    },

    // writeFile : String -> String -> Either IOError Unit
    writeFile: (path) => (content) => {
      try {
        const fs = require("fs");
        fs.writeFileSync(path, content, "utf8");
        return $con("Right", $con("Unit"));
      } catch (err) {
        return $con("Left", $foreign.IO._toIOError(err, path));
      }
    },

    // appendFile : String -> String -> Either IOError Unit
    appendFile: (path) => (content) => {
      try {
        const fs = require("fs");
        fs.appendFileSync(path, content, "utf8");
        return $con("Right", $con("Unit"));
      } catch (err) {
        return $con("Left", $foreign.IO._toIOError(err, path));
      }
    },

    // fileExists : String -> Bool
    fileExists: (path) => {
      const fs = require("fs");
      return fs.existsSync(path);
    },

    // deleteFile : String -> Either IOError Unit
    deleteFile: (path) => {
      try {
        const fs = require("fs");
        fs.unlinkSync(path);
        return $con("Right", $con("Unit"));
      } catch (err) {
        return $con("Left", $foreign.IO._toIOError(err, path));
      }
    },

    // args : Unit -> List String
    args: (_) => {
      const args = process.argv.slice(2);
      let result = $con("Nil");
      for (let i = args.length - 1; i >= 0; i--) {
        result = $con("Cons", args[i], result);
      }
      return result;
    },

    // exit : Int -> Unit
    exit: (code) => { process.exit(code); },

    // getEnv : String -> Maybe String
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
    // log : a -> a (prints value, returns it)
    log: (x) => { console.log(x); return x; },

    // trace : String -> a -> a (prints label + value, returns value)
    trace: (label) => (x) => { console.log(label + ":", x); return x; },

    // panic : String -> a (crashes with message)
    panic: (msg) => { throw new Error(msg); },
  },

  // ==========================================================================
  // Map Module (String keys only)
  // ==========================================================================
  Map: {
    // empty : Map v
    empty: new Map(),

    // singleton : String -> v -> Map v
    singleton: (k) => (v) => new Map([[k, v]]),

    // insert : String -> v -> Map v -> Map v
    insert: (k) => (v) => (m) => new Map(m).set(k, v),

    // lookup : String -> Map v -> Maybe v
    lookup: (k) => (m) => {
      if (m.has(k)) return $con("Just", m.get(k));
      return $con("Nothing");
    },

    // delete : String -> Map v -> Map v
    delete: (k) => (m) => { const m2 = new Map(m); m2.delete(k); return m2; },

    // member : String -> Map v -> Bool
    member: (k) => (m) => m.has(k),

    // size : Map v -> Int
    size: (m) => m.size,

    // keys : Map v -> List String
    keys: (m) => {
      let result = $con("Nil");
      for (const k of [...m.keys()].reverse()) {
        result = $con("Cons", k, result);
      }
      return result;
    },

    // values : Map v -> List v
    values: (m) => {
      let result = $con("Nil");
      for (const v of [...m.values()].reverse()) {
        result = $con("Cons", v, result);
      }
      return result;
    },

    // toList : Map v -> List (String, v)
    toList: (m) => {
      let result = $con("Nil");
      for (const [k, v] of [...m.entries()].reverse()) {
        result = $con("Cons", [k, v], result);
      }
      return result;
    },

    // fromList : List (String, v) -> Map v
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
    // empty : Set
    empty: new Set(),

    // singleton : String -> Set
    singleton: (v) => new Set([v]),

    // insert : String -> Set -> Set
    insert: (v) => (s) => new Set(s).add(v),

    // delete : String -> Set -> Set
    delete: (v) => (s) => { const s2 = new Set(s); s2.delete(v); return s2; },

    // member : String -> Set -> Bool
    member: (v) => (s) => s.has(v),

    // size : Set -> Int
    size: (s) => s.size,

    // toList : Set -> List String
    toList: (s) => {
      let result = $con("Nil");
      for (const v of [...s].reverse()) {
        result = $con("Cons", v, result);
      }
      return result;
    },

    // fromList : List String -> Set
    fromList: (list) => {
      const s = new Set();
      let current = list;
      while (current.$tag === "Cons") {
        s.add(current.$args[0]);
        current = current.$args[1];
      }
      return s;
    },

    // union : Set -> Set -> Set
    union: (s1) => (s2) => s1.union(s2),

    // intersect : Set -> Set -> Set
    intersect: (s1) => (s2) => s1.intersection(s2),

    // difference : Set -> Set -> Set
    difference: (s1) => (s2) => s1.difference(s2),
  },
};

`;
