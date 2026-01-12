// Algow Runtime
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
  return keysA.every((k) => k in b && $eq(a[k], b[k]));
};

// Map errno codes to IOError constructors
// IOError = FileNotFound(0) | PermissionDenied(1) | IsDirectory(2) | NotDirectory(3) | AlreadyExists(4) | NotEmpty(5) | UnknownError(6)
const $ioError = (err, path) => {
  const code = err.code;
  if (code === "ENOENT") return [0, path]; // FileNotFound
  if (code === "EACCES" || code === "EPERM") return [1, path]; // PermissionDenied
  if (code === "EISDIR") return [2, path]; // IsDirectory
  if (code === "ENOTDIR") return [3, path]; // NotDirectory
  if (code === "EEXIST") return [4, path]; // AlreadyExists
  if (code === "ENOTEMPTY") return [5, path]; // NotEmpty
  return [6, err.message]; // UnknownError
};

// Foreign function registry
const $foreign = {
  String: {
    length: (s) => s.length,
    concat: (a) => (b) => a + b,
    substring: (start) => (end) => (s) => s.substring(start, end),
    charAt: (i) => (s) => (i < 0 || i >= s.length ? null : [0, s[i]]),
    head: (s) => (s.length === 0 ? null : [0, s[0]]),
    tail: (s) => (s.length === 0 ? "" : s.slice(1)),
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
      while (current !== null) {
        result += current.h;
        current = current.t;
      }
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
      while (current !== null) {
        parts.push(current.h);
        current = current.t;
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
  },
  Char: {
    toInt: (c) => c.charCodeAt(0),
    fromInt: (n) => (n < 0 || n > 0x10ffff ? null : [0, String.fromCodePoint(n)]),
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
      return Number.isNaN(n) ? null : [0, n];
    },
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
    fromString: (s) => {
      const n = parseFloat(s);
      return Number.isNaN(n) ? null : [0, n];
    },
  },
  IO: {
    print: (s) => {
      process.stdout.write(s);
      return null;
    },
    printLine: (s) => {
      console.log(s);
      return null;
    },
    args: (_) => {
      const args = process.argv.slice(2);
      let result = null;
      for (let i = args.length - 1; i >= 0; i--) result = { h: args[i], t: result };
      return result;
    },
    exit: (code) => {
      process.exit(code);
    },
    getEnv: (name) => {
      const value = process.env[name];
      return value === undefined ? null : [0, value];
    },
  },
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
  Map: {
    empty: new Map(),
    singleton: (k) => (v) => new Map([[k, v]]),
    insert: (k) => (v) => (m) => new Map(m).set(k, v),
    lookup: (k) => (m) => (m.has(k) ? [0, m.get(k)] : null),
    delete: (k) => (m) => {
      const m2 = new Map(m);
      m2.delete(k);
      return m2;
    },
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
      while (current !== null) {
        const [k, v] = current.h;
        m.set(k, v);
        current = current.t;
      }
      return m;
    },
  },
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
      let result = null;
      for (const v of [...s].reverse()) result = { h: v, t: result };
      return result;
    },
    fromList: (list) => {
      const s = new Set();
      let current = list;
      while (current !== null) {
        s.add(current.h);
        current = current.t;
      }
      return s;
    },
    union: (s1) => (s2) => s1.union(s2),
    intersect: (s1) => (s2) => s1.intersection(s2),
    difference: (s1) => (s2) => s1.difference(s2),
  },
  File: {
    read: (path) => {
      try {
        const fs = require("fs");
        return [1, fs.readFileSync(path, "utf8")]; // Right content
      } catch (err) {
        return [0, $ioError(err, path)]; // Left IOError
      }
    },
    write: (path) => (content) => {
      try {
        const fs = require("fs");
        fs.writeFileSync(path, content, "utf8");
        return [1, null]; // Right unit
      } catch (err) {
        return [0, $ioError(err, path)]; // Left IOError
      }
    },
    append: (path) => (content) => {
      try {
        const fs = require("fs");
        fs.appendFileSync(path, content, "utf8");
        return [1, null];
      } catch (err) {
        return [0, $ioError(err, path)];
      }
    },
    remove: (path) => {
      try {
        const fs = require("fs");
        fs.unlinkSync(path);
        return [1, null];
      } catch (err) {
        return [0, $ioError(err, path)];
      }
    },
    copy: (src) => (dest) => {
      try {
        const fs = require("fs");
        fs.copyFileSync(src, dest);
        return [1, null];
      } catch (err) {
        return [0, $ioError(err, src)];
      }
    },
    rename: (src) => (dest) => {
      try {
        const fs = require("fs");
        fs.renameSync(src, dest);
        return [1, null];
      } catch (err) {
        return [0, $ioError(err, src)];
      }
    },
    touch: (path) => {
      try {
        const fs = require("fs");
        const now = new Date();
        try {
          fs.utimesSync(path, now, now);
        } catch {
          fs.closeSync(fs.openSync(path, "w"));
        }
        return [1, null];
      } catch (err) {
        return [0, $ioError(err, path)];
      }
    },
    exists: (path) => {
      try {
        const fs = require("fs");
        return fs.statSync(path).isFile();
      } catch {
        return false;
      }
    },
    size: (path) => {
      try {
        const fs = require("fs");
        return [1, fs.statSync(path).size];
      } catch (err) {
        return [0, $ioError(err, path)];
      }
    },
  },
  Dir: {
    create: (path) => {
      try {
        const fs = require("fs");
        fs.mkdirSync(path, { recursive: true });
        return [1, null];
      } catch (err) {
        return [0, $ioError(err, path)];
      }
    },
    remove: (path) => {
      try {
        const fs = require("fs");
        fs.rmSync(path, { recursive: true, force: true });
        return [1, null];
      } catch (err) {
        return [0, $ioError(err, path)];
      }
    },
    list: (path) => {
      try {
        const fs = require("fs");
        const files = fs.readdirSync(path);
        let result = null;
        for (let i = files.length - 1; i >= 0; i--) result = { h: files[i], t: result };
        return [1, result];
      } catch (err) {
        return [0, $ioError(err, path)];
      }
    },
    exists: (path) => {
      try {
        const fs = require("fs");
        return fs.statSync(path).isDirectory();
      } catch {
        return false;
      }
    },
  },
  Path: {
    join: (a) => (b) => {
      const path = require("path");
      return path.join(a, b);
    },
    dirname: (p) => {
      const path = require("path");
      return path.dirname(p);
    },
    basename: (p) => {
      const path = require("path");
      return path.basename(p);
    },
    extension: (p) => {
      const path = require("path");
      const ext = path.extname(p);
      return ext ? [0, ext] : null; // Maybe: Some = [0, val], None = null
    },
    isAbsolute: (p) => {
      const path = require("path");
      return path.isAbsolute(p);
    },
  },
};
